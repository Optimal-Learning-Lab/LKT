# ROOT CANDIDATE_LIBRARY FRESH_OUTPUT
# The mechanism lives only in the maintained package; this script orchestrates
# controlled experiments and writes a complete, unselected diagnostic record.
a <- commandArgs(TRUE);stopifnot(length(a)==3L,!dir.exists(a[3L]))
root <- normalizePath(a[1L],winslash='/',mustWork=TRUE)
candidate_library <- normalizePath(a[2L],winslash='/',mustWork=TRUE);output <- a[3L]
.libPaths(c(candidate_library,file.path(root,'.codex-r-lib'),.libPaths()))
suppressPackageStartupMessages(library(LKT,lib.loc=candidate_library))
stopifnot(normalizePath(find.package('LKT'),winslash='/')==paste0(candidate_library,'/LKT'))
hash <- function(p)digest::digest(file=p,algo='sha256')
manifest <- file.path(root,'inst/research-source-manifest.json')
stopifnot(hash(manifest)==hash(system.file('research-source-manifest.json',package='LKT')))
f <- function(n)getFromNamespace(paste0('lkt_pdp_controlled_',n),'LKT')
model <- f('model');state <- f('state');advance <- f('advance');practice <- f('practice');predict <- f('predict')
dir.create(output,recursive=TRUE)
write_table <- function(x,n)write.csv(x,file.path(output,paste0(n,'.csv')),row.names=FALSE)
parameters <- list(stp_learning_rate=.3,ltp_learning_rate=.1,fatigue_sensitivity=8,
  recovery_half_life_seconds=300,stp_decay_rates_per_second=matrix(.038/10,1L,1L))
conditions <- list(full=parameters,no_STP=modifyList(parameters,list(stp_learning_rate=0)),
  no_fatigue=modifyList(parameters,list(fatigue_sensitivity=0)),
  neither=modifyList(parameters,list(stp_learning_rate=0,fatigue_sensitivity=0)),
  no_STP_no_recovery=modifyList(parameters,list(stp_learning_rate=0,recovery_half_life_seconds=Inf)))
gaps <- c(massed=20,spaced=300)
files <- c('R/lkt-pdp-controlled.R','tests/pdp-controlled.R','scripts/run-pdp-controlled.R','docs/pdp-controlled-model.md')
source_hashes <- setNames(vapply(file.path(root,files),hash,character(1)),files)
protocol <- list(schema=1L,parameters=parameters,conditions=conditions,gaps_seconds=gaps,
  practices=4L,retention_seconds=c(0,60,300,3600,86400),source_hashes=source_hashes,
  package_version=as.character(packageVersion('LKT')),manifest_sha256=hash(manifest),
  candidate_library=candidate_library,parameters_fitted=FALSE,GRU_modified=FALSE,
  paper='Pavlik (2004), A PDP Model of Spacing Effects in Memory',
  interpretation='Controlled reconstruction; not the original 200-unit network or an empirical fit.')
saveRDS(protocol,file.path(output,'protocol.rds'))
writeLines(capture.output(dput(protocol)),file.path(output,'protocol.txt'))
run <- function(m,gap,target=1) {
  z <- state(m);events <- vector('list',4L);states <- list(z)
  for(i in 1:4) {
    q <- practice(m,z,1,target,(i-1)*gap);z <- q$state;states[[i+1L]] <- z
    events[[i]] <- data.frame(practice=i,time_seconds=z$time_seconds,prediction_before=q$prediction,
      error=q$error,STP_increment=q$short_change[1],LTP_increment=q$long_change[1],
      STP=z$short[1],LTP=z$long[1],rate_before=q$before$available_rate[1],
      rate_after=z$available_rate[1],prediction_after=q$post_prediction)
  }
  list(state=z,states=states,events=do.call(rbind,events))
}
runs <- list();events <- retention <- list()
for(condition in names(conditions))for(schedule in names(gaps)) {
  m <- do.call(model,conditions[[condition]]);r <- run(m,gaps[[schedule]])
  id <- paste(condition,schedule,sep='__');runs[[id]] <- list(model=m,run=r)
  events[[id]] <- cbind(condition=condition,schedule=schedule,r$events)
  retention[[id]] <- do.call(rbind,lapply(protocol$retention_seconds,function(delay) {
    z <- advance(m,r$state,r$state$time_seconds+delay)
    data.frame(condition=condition,schedule=schedule,delay_seconds=delay,
      association_strength=predict(m,r$state,1,r$state$time_seconds+delay),
      STP=z$short[1],LTP=z$long[1],available_rate=z$available_rate[1])
  }))
}
events <- do.call(rbind,events);retention <- do.call(rbind,retention)
write_table(events,'practice-events');write_table(retention,'retention')
contrast <- merge(retention[retention$schedule=='massed',],retention[retention$schedule=='spaced',],
  by=c('condition','delay_seconds'),suffixes=c('_massed','_spaced'))
contrast$strength_advantage_spaced <- contrast$association_strength_spaced-contrast$association_strength_massed
contrast$LTP_advantage_spaced <- contrast$LTP_spaced-contrast$LTP_massed
write_table(contrast,'spacing-contrasts')
saveRDS(runs,file.path(output,'controlled-states.rds'))

# An independent continuous first-practice recovery/forgetting timeline.
first <- runs$full__massed$run$states[[2L]];m <- runs$full__massed$model
timeline <- do.call(rbind,lapply(c(0,seq(10,3600,10)),function(t) {
  z <- advance(m,first,t);data.frame(time_seconds=t,STP=z$short[1],LTP=z$long[1],
    available_rate=z$available_rate[1],analytic_rate=.1+(.1*exp(-.8)-.1)*exp(-log(2)*t/300))
}))
write_table(timeline,'first-practice-recovery')

# Freeze and report the entire illustrative sensitivity grid. No best-case selection.
grid <- expand.grid(stp_learning_rate=c(0,.1,.3,.5),ltp_learning_rate=c(.05,.1,.2),
  fatigue_sensitivity=c(0,2,8,16),recovery_half_life_seconds=c(60,300,1800))
sweep <- do.call(rbind,lapply(seq_len(nrow(grid)),function(i) {
  m <- do.call(model,c(as.list(grid[i,]),list(stp_decay_rates_per_second=matrix(.0038,1,1))))
  a <- run(m,20);b <- run(m,300)
  cbind(grid[i,],massed_LTP=a$state$long[1],spaced_LTP=b$state$long[1],
    spaced_LTP_advantage=b$state$long[1]-a$state$long[1])
}))
write_table(sweep,'sensitivity-grid')

# A small genuine autoassociation example: eight units, two unit-norm orthogonal
# patterns, explicit per-connection rates, equal exposures and common seeds.
set.seed(2004)
rates <- matrix(stats::rexp(64L,rate=1/.038)/10,8L,8L)
set.seed(2005)
patterns <- qr.Q(qr(matrix(stats::rnorm(64),8,8)))[,1:2,drop=FALSE]
saveRDS(list(rates_per_second=rates,patterns=patterns,seeds=c(decay=2004,patterns=2005)),file.path(output,'heterogeneous-inputs.rds'))
heterogeneous <- list()
for(schedule in names(gaps)) {
  mm <- do.call(model,modifyList(parameters,list(stp_decay_rates_per_second=rates)))
  z <- state(mm)
  for(i in 1:4)for(j in 1:2)z <- practice(mm,z,patterns[,j],patterns[,j],(i-1)*gaps[[schedule]]+(j-1)*10)$state
  for(j in 1:2) {
    q <- predict(mm,z,patterns[,j],z$time_seconds+86400)
    heterogeneous[[paste(schedule,j)]] <- data.frame(schedule=schedule,pattern=j,
      delayed_strength=sum(q*patterns[,j]),delayed_squared_error=sum((patterns[,j]-q)^2),
      LTP_projection=sum(as.numeric(patterns[,j]%*%z$long)*patterns[,j]),
      minimum_available_rate=min(z$available_rate),maximum_available_rate=max(z$available_rate))
  }
}
heterogeneous <- do.call(rbind,heterogeneous);write_table(heterogeneous,'heterogeneous-autoassociation')

# Analytic/causal checks, beyond merely expecting the plotted curves to look right.
checks <- list()
check <- function(name,value) {checks[[name]] <<- isTRUE(value);if(!isTRUE(value))stop('Failed diagnostic: ',name)}
check('first_event_independent_arithmetic',abs(first$short[1]-.3)<1e-12 && abs(first$long[1]-.1)<1e-12 &&
  abs(first$available_rate[1]-.1*exp(-.8))<1e-12)
check('analytic_recovery',max(abs(timeline$available_rate-timeline$analytic_rate))<1e-12)
check('nondecaying_LTP',max(abs(timeline$LTP-.1))<1e-12)
check('monotone_recovery',all(diff(timeline$available_rate)>0))
check('fatigue_each_practice',all(events$rate_after<=events$rate_before))
check('capacity_bounds',all(events$rate_after>=0 & events$rate_after<=.1))
pick <- function(n)sweep[sweep$stp_learning_rate==0 & sweep$fatigue_sensitivity==n,]
check('fatigue_specific_spacing_all_27_grid_cases',all(sweep$spaced_LTP_advantage[sweep$stp_learning_rate==0 & sweep$fatigue_sensitivity>0]>1e-12))
check('no_STP_no_fatigue_spacing_absent',max(abs(pick(0)$spaced_LTP_advantage))<1e-12)
check('no_STP_no_recovery_spacing_absent',max(abs(contrast$strength_advantage_spaced[contrast$condition=='no_STP_no_recovery']))<1e-12)
check('STP_competition_spacing_without_fatigue',all(sweep$spaced_LTP_advantage[sweep$stp_learning_rate>0 & sweep$fatigue_sensitivity==0]>1e-12))
check('full_model_delayed_spacing',all(contrast$strength_advantage_spaced[contrast$condition=='full' & contrast$delay_seconds>=3600]>0))
negative <- run(m=runs$full__spaced$model,gap=300,target=-1)
check('signed_weight_symmetry',max(abs(negative$state$long+runs$full__spaced$run$state$long))<1e-12 &&
  max(abs(negative$state$available_rate-runs$full__spaced$run$state$available_rate))<1e-12)
write_table(data.frame(check=names(checks),passed=unlist(checks)),'mechanism-checks')

# Publication/export plots use the same tables; no interactive dependency.
plot_results <- function() {
  old <- par(mfrow=c(2,2),mar=c(4.2,4.4,3,1));on.exit(par(old))
  colors <- c(massed='#b35b22',spaced='#176a9c')
  plot(NA,xlim=c(1,4),ylim=c(0,.105),xlab='Practice number',ylab='Available LTP learning rate',main='A. Fatigue and between-practice recovery')
  for(schedule in names(gaps)) {
    d <- events[events$condition=='no_STP' & events$schedule==schedule,]
    lines(d$practice,d$rate_before,type='b',col=colors[[schedule]],pch=16,lwd=2)
    segments(d$practice,d$rate_before,d$practice,d$rate_after,col=colors[[schedule]],lwd=2)
  }
  legend('bottomleft',legend=c('20-second gaps','300-second gaps'),col=colors,lty=1,pch=16,bty='n',cex=.8)
  plot(timeline$time_seconds/60,timeline$available_rate,type='l',lwd=2,col='#176a9c',
    xlab='Minutes after one practice',ylab='Available LTP learning rate',main='B. Recovery without additional practice',ylim=c(0,.105))
  abline(h=.1,lty=2,col='gray50');points(5,(.1+first$available_rate[1])/2,pch=16,col='#b35b22')
  legend('bottomright',legend=c('Available rate','Resting rate','Half-recovery at 5 minutes'),
    col=c('#176a9c','gray50','#b35b22'),lty=c(1,2,NA),pch=c(NA,NA,16),bty='n',cex=.75)
  delays <- c(0,exp(seq(log(1),log(86400),length.out=150)))
  plot(NA,xlim=c(0,log10(86401)),ylim=c(0,1),xlab='log10(1 + seconds after last practice)',
    ylab='Association strength (not probability)',main='C. Matched post-final-practice retention')
  for(schedule in names(gaps)) {
    rr <- runs[[paste0('full__',schedule)]];yy <- vapply(delays,function(t)predict(rr$model,rr$run$state,1,rr$run$state$time_seconds+t),numeric(1))
    lines(log10(1+delays),yy,col=colors[[schedule]],lwd=2)
    abline(h=rr$run$state$long[1],col=colors[[schedule]],lty=2)
  }
  legend('topright',legend=c('Massed total','Spaced total','Durable LTP levels'),col=c(colors,'gray40'),lty=c(1,1,2),bty='n',cex=.8)
  gap_grid <- seq(0,1800,length.out=100)
  plotted <- c('full','no_STP','no_fatigue','neither');pal <- c('#176a9c','#6d5098','#b35b22','#555555')
  values <- sapply(plotted,function(n) {mm<-do.call(model,conditions[[n]]);vapply(gap_grid,function(g)run(mm,g)$state$long[1],numeric(1))})
  matplot(gap_grid/60,values,type='l',lty=1:4,lwd=2,col=pal,xlab='Practice gap (minutes)',
    ylab='Durable LTP after four practices',main='D. STP/fatigue factorial controls')
  legend('bottomright',legend=c('Full model','Fatigue only','STP only','Neither'),col=pal,lty=1:4,lwd=2,bty='n',cex=.8)
}
grDevices::png(file.path(output,'mechanisms.png'),width=1600,height=1100,res=150);plot_results();grDevices::dev.off()
grDevices::svg(file.path(output,'mechanisms.svg'),width=11,height=7.6);plot_results();grDevices::dev.off()
grDevices::pdf(file.path(output,'mechanisms.pdf'),width=11,height=7.6);plot_results();grDevices::dev.off()

delayed <- retention[retention$delay_seconds==86400,]
rows <- vapply(seq_len(nrow(delayed)),function(i)with(delayed[i,],sprintf('| %s | %s | %.6f | %.6f |',condition,schedule,STP,LTP)),character(1))
positive <- sum(sweep$spaced_LTP_advantage>1e-12);neutral <- sum(abs(sweep$spaced_LTP_advantage)<=1e-12)
writeLines(c('# Controlled PDP fatigue, recovery and spacing','',
  'A controlled reconstruction of mechanisms in Pavlik (2004), not the original 200-unit network or an empirical fit. No parameters were fitted or selected from the sensitivity grid.','',
  '## Explicit equations','',
  'Before a practice separated by dt seconds: Sminus = exp(-lambda dt) S; Eminus = eta0 + (E-eta0) exp(-rho dt). LTP L does not decay. Prediction is x(Sminus+L). For error e=y-prediction, U=outer(x,e), DeltaS=etaS U, DeltaL=Eminus U. Commit the weight changes, then E=Eminus exp(-kappa abs(DeltaL)).','',
  'S, L and E are per-connection, per-learner state. Signed weight changes are retained; their magnitudes consume capacity. The exact fatigue equation and symmetric depression rule are explicit reconstruction assumptions because the paper does not provide them. Unit-norm cues and instantaneous practice events are controlled simplifications. Outputs are strengths, not recall probabilities.','',
  '## Fixed demonstration','',
  'Four practices, cue=target=1. STP learning rate .3; base LTP learning rate .1; fatigue sensitivity 8; recovery half-life 300 seconds; representative STP rate .038 per 10 seconds. Massed gaps are 20 seconds; spaced gaps are 300 seconds. Retention is matched to the last practice, not the start of training.','',
  '| Condition | Schedule | STP after one day | Durable LTP |','|---|---|---:|---:|',rows,'',
  'The no-STP condition isolates fatigue/recovery. Its spacing advantage disappears when fatigue is removed or recovery is disabled. With STP present, error-driven competition also yields a spacing advantage without fatigue, so the full-model spacing benefit cannot be attributed solely to fatigue.','',
  sprintf('All %d mechanistic checks passed. The complete 144-case grid produced %d positive, %d neutral and %d negative durable spacing contrasts. Every one of the 27 no-STP, positive-fatigue grid cases had a spacing advantage; the nine no-STP/no-fatigue cases were neutral.',length(checks),positive,neutral,nrow(sweep)-positive-neutral),'',
  'The eight-unit, two-pattern autoassociation example uses 64 fixed seeded exponential STP rates with the paper mean and independent per-connection capacities. Its complete outcomes are in heterogeneous-autoassociation.csv. It is a small extension of the diagnostic, not a reproduction of the original network.','',
  'mechanisms.png, mechanisms.svg and mechanisms.pdf show fatigue, analytic recovery, matched retention and the STP/fatigue factorial controls. practice-events.csv records every representative connection update. first-practice-recovery.csv, sensitivity-grid.csv and mechanism-checks.csv preserve all calculations.','',
  'These diagnostics establish the specified equations and their causal controls. They do not establish parameter estimates, human-data fit or predictive improvement over GRU. Choice of plastic GRU connections, interaction with retrieval, and feedback-driven within-student learning remain future design decisions.'),file.path(output,'RESULTS.md'))
jsonlite::write_json(list(complete=TRUE,mechanism_checks=checks,sensitivity_cases=nrow(sweep),
  positive_spacing_cases=positive,neutral_spacing_cases=neutral,negative_spacing_cases=nrow(sweep)-positive-neutral,
  parameters_fitted=FALSE,GRU_modified=FALSE,protocol_sha256=hash(file.path(output,'protocol.rds'))),
  file.path(output,'verification.json'),auto_unbox=TRUE,pretty=TRUE)
print(delayed);cat('PASS controlled mechanism checks and complete sensitivity grid.\n')
