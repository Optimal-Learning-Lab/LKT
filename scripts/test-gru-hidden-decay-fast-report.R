# Exercise report aggregation with explicitly synthetic checkpoint fixtures.
# These fixtures are not trained models and must never enter research results.
# ROOT TEST_OUTPUT
a <- commandArgs(TRUE);stopifnot(length(a)==2L,!dir.exists(a[2L]))
root <- a[1L];output <- a[2L]
source(file.path(root,'scripts/gru-encoder-study.R'))
source(file.path(root,'scripts/gru-hidden-decay-fast-study.R'))
dir.create(output,recursive=TRUE)
writeLines('TEST ONLY: synthetic report fixtures, not trained research models.',file.path(output,'TEST_ONLY.txt'))
for(n in c('data','partitions','models','reports'))dir.create(file.path(output,n))
d <- data.frame(Anon.Student.Id=paste0('fixture',1:10),CF..ansbin.=rep(0:1,5),comparison_source_row=1:10)
outer <- rep(1:5,each=2L)
tasks <- expand.grid(seed=1:3,candidate=c('stock_full','decay_full','stock_no_ppes','decay_no_ppes'),partition=paste0('cv',1:5),stringsAsFactors=FALSE)
tasks$run_id <- with(tasks,paste(partition,candidate,paste0('seed',seed),sep='__'))
tasks <- tasks[,c('run_id','partition','candidate','seed')]
conditions <- unique(tasks$candidate)
settings <- setNames(lapply(conditions,function(n)list(hidden_decay=grepl('^decay_',n),
  hidden_decay_parameterization='amplitude',seed=1L,epochs=1L)),conditions)
recipes <- setNames(rep(list(list(interc=FALSE)),4L),conditions)
p <- list(tasks=tasks,seeds=1:3,settings=settings,recipes=recipes,total_fits=60L,
  parameter_counts=setNames(c(43073L,43076L,43041L,43044L),conditions))
saveRDS(p,file.path(output,'study.rds'));saveRDS(d,file.path(output,'data/prepared_data.rds'))
for(k in 1:5)saveRDS(ifelse(outer==k,3L,ifelse(outer==k%%5+1L,2L,1L)),file.path(output,'partitions',paste0('cv',k,'.rds')))
for(i in seq_len(nrow(tasks))) {
  task <- tasks[i,,drop=FALSE];s <- p$settings[[task$candidate]];s$seed<-task$seed
  roles <- readRDS(file.path(output,'partitions',paste0(task$partition,'.rds')))
  confidence <- switch(task$candidate,stock_full=.75,decay_full=.8,stock_no_ppes=.7,decay_no_ppes=.85)
  prediction <- ifelse(d$CF..ansbin.==1,confidence,1-confidence)
  weights <- list(base=0)
  if(s$hidden_decay)weights <- c(weights,setNames(as.list(c(if(task$seed==1L)0 else .1,.2,.3)),paste0('hidden_decay.',c('a','b','c'))))
  fit <- list(gru_settings=s,model=list(recipe=list(interc=FALSE),weights=weights),
    fit_rows=which(roles==1L),validation_rows=which(roles==2L),evaluation_rows=which(roles==3L),
    parameter_count=p$parameter_counts[[task$candidate]],selected_epoch=1L,trained_epochs=1L,
    training_history=data.frame(epoch=1L,validation_loss=-log(confidence)),prediction=prediction)
  saved <- list(task=task,settings=s,fit=fit,elapsed=0,
    fingerprint=digest::digest(list(study=study_hash(file.path(output,'study.rds')),task=task,settings=s),algo='sha256'))
  saveRDS(saved,file.path(output,'models',paste0(task$run_id,'.rds')))
  held <- d[roles==3L,]
  write.csv(data.frame(partition=task$partition,condition=task$candidate,seed=task$seed,
    source_row=held$comparison_source_row,participant=held$Anon.Student.Id,observed=held$CF..ansbin.,prediction=prediction[roles==3L]),
    file.path(output,'reports',paste0(task$run_id,'-predictions.csv')),row.names=FALSE)
  write.csv(cbind(task,as.data.frame(fast_study_parameters(fit)),maximum_replay_probability_difference=0),
    file.path(output,'reports',paste0(task$run_id,'-parameters.csv')),row.names=FALSE)
  writeLines('TEST ONLY synthetic fixture.',file.path(output,'reports',paste0(task$run_id,'-verification.txt')))
}
result <- fast_study_report(output,p,'report');fast_study_report(output,p,'check')
stopifnot(nrow(result$fits)==60L,nrow(result$parameters)==30L,
  all(is.infinite(result$parameters$baseline_hidden_half_life_days[result$parameters$seed==1L])),
  all(abs(result$parameters$baseline_hidden_half_life_days[result$parameters$seed!=1L]-log(2)/.1)<1e-12),
  all(abs(result$parameters$DK_half_life_days-log(2)/log1p(exp(.3)))<1e-12),
  abs(result$mean_interaction$log_loss_improvement_interaction-(log(.85/.7)-log(.8/.75)))<1e-12)
cat('PASS sixty-row synthetic report fixture, thirty triplets, zero-amplitude infinite half-lives and expected factorial contrasts.\n')
