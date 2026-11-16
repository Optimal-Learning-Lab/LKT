# ROOT EXPLICIT_CANDIDATE_LIBRARY FRESH_OUTPUT ORIGINAL_AUTO13_R
# Candidate qualification only: do not discover alternate installations.
a <- commandArgs(TRUE);stopifnot(length(a)==4L,!dir.exists(a[3]))
root <- normalizePath(a[1],winslash='/',mustWork=TRUE)
lib <- normalizePath(a[2],winslash='/',mustWork=TRUE);output <- a[3]
.libPaths(c(lib,file.path(root,'.codex-r-lib'),.libPaths()))
suppressPackageStartupMessages(library(LKT,lib.loc=lib))
stopifnot(normalizePath(find.package('LKT'),winslash='/')==paste0(lib,'/LKT'))
hash <- function(p)digest::digest(file=p,algo='sha256')
stopifnot(hash(file.path(root,'inst/research-source-manifest.json'))==
  hash(system.file('research-source-manifest.json',package='LKT')))
f <- function(n)getFromNamespace(paste0('lkt_pdp_source_',n),'LKT')
model <- f('model');state <- f('state');step <- f('step');idle <- f('idle');recall <- f('recall')
dir.create(output,recursive=TRUE)
write_table <- function(x,n)write.csv(x,file.path(output,paste0(n,'.csv')),row.names=FALSE)
checks <- list();check <- function(n,v) {checks[[n]] <<- isTRUE(v);if(!isTRUE(v))stop('Failed: ',n)}
near <- function(x,y)max(abs(x-y))<1e-11
ternary <- function(nr,nc) {u<-matrix(runif(nr*nc),nr,nc);ifelse(u>.8,ifelse(u>.9,-1,1),0)}

# Parse, but never source, the legacy file. Evaluate only the original numeric
# literals and its two learning loops in an isolated reference environment.
original_hash <- hash(a[4]);ast <- parse(a[4])
assignment <- function(name) {
  found <- Filter(function(x)is.call(x) && identical(x[[1]],as.name('<<-')) &&
    identical(x[[2]],as.name(name)),as.list(ast))
  stopifnot(length(found)==1L);found[[1]][[3]]
}
literal <- function(x) {
  stopifnot(is.call(x),identical(x[[1]],as.name('c')),
    all(vapply(as.list(x)[-1],is.numeric,logical(1))))
  as.numeric(unlist(as.list(x)[-1]))
}
base <- literal(assignment('basepars'));order <- literal(assignment('worder'))
net <- assignment('net');stopifnot(identical(net[[1]],as.name('function')))
loops <- Filter(function(x)is.call(x) && identical(x[[1]],as.name('for')),as.list(net[[3]])[-1])
stopifnot(length(loops)==4L,identical(loops[[1]][[3]],as.name('worder')),
  identical(loops[[2]][[3]],quote(1:dur)))
source_model <- function(decays)model(decays,base[1],base[2],base[3],base[4]*1000,base[6],base[7])
reference <- function(m,words,ordering,interference) {
  n <- ncol(words);e <- new.env(parent=baseenv())
  vars <- list(tu=n,numwords=nrow(words),words=words,prm=seq_len(nrow(words)),worder=ordering,
    wordct=rep(0,nrow(words)),srecalls=matrix(NA,nrow(words),length(ordering)+1L),
    linkSTP=matrix(0,n,n),targetLTP=matrix(0,n,n),linkLTP=matrix(0,n,n),LTPe=matrix(m$base_rate,n,n),
    e=m$stp_rate,bltpe=m$base_rate,ltperc=m$recovery,ltpedc=m$fatigue,
    ltpgr=m$consolidation,s=m$heat,decays=m$decays,intf=interference,dur=nrow(interference))
  list2env(vars,e)
  # Bind every legacy <<- destination here so none escapes into .GlobalEnv.
  for(name in c('out','tout','msum','di','deltaSTP','deltaLTP'))assign(name,NULL,e)
  e$sig <- eval(quote(function(temp)2/(1+exp(-temp/s))-1),e)
  eval(loops[[1]],new.env(parent=e))
  train <- mget(c('linkSTP','targetLTP','linkLTP','LTPe'),e)
  train_recall <- e$srecalls
  eval(loops[[2]],new.env(parent=e))
  list(training=train,retention=mget(c('linkSTP','targetLTP','linkLTP','LTPe'),e),recall=train_recall)
}
compare <- function(m,words,ordering,intf,label) {
  old <- reference(m,words,ordering,intf);z <- state(m);pre <- old$recall*NA;count <- integer(nrow(words))
  for(j in ordering) {
    count[j]<-count[j]+1L;pre[j,count[j]]<-recall(m,z,words[j,])$strength
    z<-step(m,z,words[j,])$state
  }
  keys <- c('short','target','long','rate')
  train_error <- max(vapply(seq_along(keys),function(i)max(abs(z[[keys[i]]]-old$training[[i]])),numeric(1)))
  for(j in seq_len(nrow(intf)))z<-step(m,z,intf[j,],'retention')$state
  retention_error <- max(vapply(seq_along(keys),function(i)max(abs(z[[keys[i]]]-old$retention[[i]])),numeric(1)))
  recall_error <- max(abs(pre-old$recall),na.rm=TRUE)
  check(paste0(label,'_literal_source_parity'),max(train_error,retention_error,recall_error)<1e-11)
  data.frame(case=label,training_max_error=train_error,retention_max_error=retention_error,recall_max_error=recall_error)
}
set.seed(2004);small_words<-ternary(12,8);small_intf<-ternary(30,8)
small_model<-source_model(matrix(pmin(rexp(64,1/base[5]),.8),8,8))
parity <- compare(small_model,small_words,rep(1:12,2),small_intf,'small')
# Explicit historical RNG convention, since R changed sample() in 3.6. This
# qualifies equations and schedule, not a fitted replication of historical data.
suppressWarnings(RNGversion('3.5.0'));set.seed(49);words<-ternary(104,200)
set.seed(512789);permutation<-sample(1:104,104)
decays<-matrix(pmin(rexp(40000,1/base[5]),.8),200,200)
intf<-ternary(225*7,200);m200<-source_model(decays)
parity<-rbind(parity,compare(m200,words[permutation,],order,intf,'original_200_unit_schedule'))
write_table(parity,'literal-source-parity');cat('PASS literal original loops, including 200-unit schedule.\n')
RNGversion(as.character(getRversion()))

# Single active connection: expose reversal, recovery and consolidation directly.
m<-source_model(matrix(base[5],2,2));x<-c(1,1);z<-state(m);trace<-list()
for(i in 1:100) {
  q<-step(m,z,x);nextz<-q$state
  trace[[i]]<-data.frame(practice=i,rate_before=z$rate[1,2],rate_after=nextz$rate[1,2],
    direction=q$direction[1,2],target_change=q$target_change[1,2],target=nextz$target[1,2],
    expressed=nextz$long[1,2],strength=recall(m,nextz,x)$strength)
  z<-nextz
}
trace<-do.call(rbind,trace);write_table(trace,'negative-rate-trace')
first<-step(m,state(m),x)$state
recovery<-do.call(rbind,lapply(c(0:100,200,400,1000),function(t) {
  zz<-idle(m,first,t);data.frame(ticks=t,rate=zz$rate[1,2],target=zz$target[1,2],expressed=zz$long[1,2])
}))
write_table(recovery,'recovery')
check('first_rate_negative',near(first$rate[1],-.0030583527967651))
check('negative_rate_reverses_update',trace$direction[2]>0 && trace$target_change[2]<0)
check('recovery_from_negative_monotone',all(diff(recovery$rate)>0))
check('recovery_exact',near(recovery$rate,m$base_rate+(first$rate[1]-m$base_rate)*(1-m$recovery)^recovery$ticks))
check('idle_target_unchanged',near(recovery$target,rep(m$base_rate,nrow(recovery))))

# Four target presentations; gap is the number of intervening zero-input
# training ticks. Delay is 100 retention blocks (4000 source time units), always
# measured after the final target. Consolidation remains active during rest.
run <- function(mm,x,gap) {
  zz<-state(mm)
  for(i in 1:4) {if(i>1)zz<-idle(mm,zz,gap);zz<-step(mm,zz,x)$state}
  zz
}
conditions<-list(original=m,no_fatigue=modifyList(m,list(fatigue=0)),
  fatigue_only=modifyList(m,list(stp_rate=0,consolidation=1)),
  neither=modifyList(m,list(stp_rate=0,consolidation=1,fatigue=0)),
  fatigue_no_recovery=modifyList(m,list(stp_rate=0,consolidation=1,recovery=0)))
spacing<-list()
for(condition in names(conditions))for(gap in c(0,1,13,97)) {
  mm<-conditions[[condition]];zz<-run(mm,x,gap)
  delayed<-idle(mm,zz,100,'retention')
  spacing[[length(spacing)+1L]]<-data.frame(condition=condition,gap=gap,
    immediate_strength=recall(mm,zz,x)$strength,delayed_strength=recall(mm,delayed,x)$strength,
    target=zz$target[1,2],minimum_rate=min(zz$rate))
}
spacing<-do.call(rbind,spacing);write_table(spacing,'spacing-controls')
pick <- function(c,g)spacing$delayed_strength[spacing$condition==c & spacing$gap==g]
check('original_delayed_spacing',pick('original',97)>pick('original',1))
check('fatigue_only_delayed_spacing',pick('fatigue_only',97)>pick('fatigue_only',1))
check('neither_spacing_absent',near(pick('neither',97),pick('neither',1)))
check('fatigue_without_recovery_spacing_absent',near(pick('fatigue_no_recovery',97),pick('fatigue_no_recovery',1)))

# Source parameters, sparse +/-1 patterns, independent per-link capped
# exponential STP rates. All 20 seeds, all three gaps, identical random
# interference per seed. Reduced 40-unit network is an explicit controlled size.
network<-list();network_states<-list()
for(seed in 1:20) {
  set.seed(seed);patterns<-ternary(101,40);xx<-patterns[1,];noise<-patterns[-1,,drop=FALSE]
  mm<-source_model(matrix(pmin(rexp(1600,1/base[5]),.8),40,40))
  for(gap in c(1,13,97)) {
    trained<-run(mm,xx,gap);rested<-idle(mm,trained,100,'retention');disturbed<-trained
    neg<-0L;updates<-0L;max_abs_rate<-max(abs(trained$rate))
    for(i in 1:100) {
      q<-step(mm,disturbed,noise[i,],'retention')
      active<-abs(q$direction)>0
      neg<-neg+sum(disturbed$rate[active]<0);updates<-updates+sum(active)
      disturbed<-q$state;max_abs_rate<-max(max_abs_rate,abs(disturbed$rate))
    }
    for(condition in c('immediate','idle','interference')) {
      zz<-switch(condition,immediate=trained,idle=rested,interference=disturbed)
      rr<-recall(mm,zz,xx)
      network[[length(network)+1L]]<-data.frame(seed=seed,gap=gap,condition=condition,
        strength=rr$strength,probability=rr$probability,negative_rate_fraction=mean(zz$rate<0),
        negative_update_fraction=if(updates)neg/updates else 0,max_abs_rate=max_abs_rate,
        target_change_from_training=sqrt(sum((zz$target-trained$target)^2)))
    }
    network_states[[paste(seed,gap,sep='_')]]<-list(model=mm,input=xx,trained=trained,idle=rested,interference=disturbed)
  }
}
network<-do.call(rbind,network);write_table(network,'network-all-seeds')
means<-aggregate(cbind(strength,probability,negative_rate_fraction,negative_update_fraction)~gap+condition,network,mean)
write_table(means,'network-means');saveRDS(network_states,file.path(output,'network-states.rds'))
pairs<-merge(subset(network,condition=='idle'),subset(network,condition=='interference'),by=c('seed','gap'),suffixes=c('_idle','_interference'))
pairs$interference_loss<-pairs$strength_idle-pairs$strength_interference
write_table(pairs,'interference-pairs')
check('idle_target_stable_all_seeds',all(subset(network,condition=='idle')$target_change_from_training==0))
check('interference_changes_target_all_seeds',all(subset(network,condition=='interference')$target_change_from_training>0))
# Report directions without asserting every random seed has the desired effect.
contrasts<-merge(subset(network,gap==1),subset(network,gap==97),by=c('seed','condition'),suffixes=c('_massed','_spaced'))
contrasts$spacing_gain<-contrasts$strength_spaced-contrasts$strength_massed
write_table(contrasts,'network-spacing-pairs')

# Matched conflicting association: same cue, opposite target. This tests loss
# causally, with elapsed time and pre-interference state held fixed.
trained<-run(m,x,97);rested<-idle(m,trained,100,'retention');conflict<-trained
for(i in 1:100)conflict<-step(m,conflict,c(1,-1),'retention')$state
conflict_table<-data.frame(condition=c('idle','conflicting_learning'),
  original_strength=c(recall(m,rested,x)$strength,recall(m,conflict,x)$strength),
  conflicting_strength=c(recall(m,rested,c(1,-1))$strength,recall(m,conflict,c(1,-1))$strength))
write_table(conflict_table,'conflicting-association')
check('conflicting_learning_reduces_old_association',conflict_table$original_strength[2]<conflict_table$original_strength[1])
check('conflicting_learning_acquires_new_association',conflict_table$conflicting_strength[2]>conflict_table$conflicting_strength[1])
check('source_file_unchanged',hash(a[4])==original_hash)
write_table(data.frame(check=names(checks),passed=unlist(checks)),'checks')
protocol<-list(original_file=normalizePath(a[4],winslash='/'),original_sha256=original_hash,
  base_parameters=base,parameters_fitted=FALSE,GRU_modified=FALSE,
  source_parity_rng='RNGversion 3.5.0; original seeds 49 and 512789',
  network_seeds=1:20,network_units=40,target_presentations=4,gaps_zero_input_ticks=c(1,13,97),
  retention_blocks=100,time_units_per_retention_block=40,library=lib,
  manifest_sha256=hash(file.path(root,'inst/research-source-manifest.json')),
  script_sha256=hash(file.path(root,'scripts/run-pdp-source.R')))
saveRDS(protocol,file.path(output,'protocol.rds'))
jsonlite::write_json(protocol,file.path(output,'protocol.json'),pretty=TRUE,auto_unbox=TRUE,digits=17)

plot_results<-function() {
  old<-par(mfrow=c(2,2),mar=c(4.2,4.4,2.8,1));on.exit(par(old))
  plot(trace$practice[1:20],trace$rate_before[1:20],type='b',pch=16,col='#176a9c',
    xlab='Consecutive practice',ylab='LTP learning rate before practice',main='A. Signed rates reverse updates');abline(h=0,lty=2)
  plot(recovery$ticks,recovery$rate,type='l',lwd=2,col='#176a9c',xlab='Zero-input training ticks',
    ylab='LTP learning rate',main='B. Recovery from a negative rate');abline(h=c(0,m$base_rate),lty=2,col='gray50')
  pal<-c('#176a9c','#b35b22','#6d5098','#555555','#339966')
  plot(NA,xlim=c(0,97),ylim=range(spacing$delayed_strength),xlab='Intervening zero-input ticks',
    ylab='Delayed partial-cue strength',main='C. Four-practice causal controls')
  for(j in seq_along(conditions)) {d<-subset(spacing,condition==names(conditions)[j]);lines(d$gap,d$delayed_strength,col=pal[j],lty=j,type='b',pch=j)}
  legend('right',legend=c('Original','No fatigue','Fatigue only','Neither','Fatigue, no recovery'),col=pal,lty=1:5,pch=1:5,bty='n',cex=.7)
  d<-subset(means,condition!='immediate');lim<-range(c(0,d$strength))
  plot(NA,xlim=c(1,97),ylim=lim,xlab='Intervening zero-input ticks',ylab='Mean partial-cue strength (20 seeds)',main='D. Matched retention and interference')
  for(j in 1:2) {v<-subset(d,condition==c('idle','interference')[j]);v<-v[order(v$gap),];lines(v$gap,v$strength,type='b',col=pal[j],pch=16,lwd=2)}
  legend('topleft',legend=c('Idle retention','Random interference'),col=pal[1:2],pch=16,lty=1,bty='n',cex=.8)
}
png(file.path(output,'mechanisms.png'),width=1600,height=1100,res=150);plot_results();dev.off()
pdf(file.path(output,'mechanisms.pdf'),width=11,height=7.6);plot_results();dev.off()
svg(file.path(output,'mechanisms.svg'),width=11,height=7.6);plot_results();dev.off()
jsonlite::write_json(list(complete=TRUE,checks=checks,parameters_fitted=FALSE,GRU_modified=FALSE),
  file.path(output,'verification.json'),pretty=TRUE,auto_unbox=TRUE)
print(parity);print(spacing);print(means)
cat('Random interference losses: ',sum(pairs$interference_loss>0),'/',nrow(pairs),'\n')
print(aggregate(spacing_gain~condition,contrasts,function(x)c(mean=mean(x),positive=sum(x>0),negative=sum(x<0))))
cat('PASS ',length(checks),' source-matched diagnostics.\n')
