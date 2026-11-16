# Read-only audit of completed constant/evolving student-rate comparisons.
# ROOT CONSTANT_RESULTS EVOLVING_RESULTS REFERENCE_DATA_DIRECTORY OUTPUT LIBRARY
args<-commandArgs(TRUE);stopifnot(length(args)==6L)
root<-args[1];paths<-args[2:3];reference<-args[4];out<-args[5]
.libPaths(c(args[6],.libPaths()))
library(LKT,lib.loc=args[6]);stopifnot(normalizePath(find.package('LKT'),winslash='/')==paste0(normalizePath(args[6],winslash='/'),'/LKT'))
f<-function(n)getFromNamespace(n,'LKT')
hash<-function(p)digest::digest(file=p,algo='sha256')
close<-function(a,b)stopifnot(isTRUE(all.equal(as.numeric(a),as.numeric(b),tolerance=1e-12)))
stopifnot(!file.exists(out));data<-readRDS(file.path(reference,'data/prepared_data.rds'))
protocols<-lapply(paths,function(x)readRDS(file.path(x,'study.rds')))
old<-protocols[[1]];new<-protocols[[2]]
stopifnot(identical(old$tasks,new$tasks),identical(old$baseline,new$baseline),
 identical(old$partition_hashes,new$partition_hashes),old$data_sha256==new$data_sha256,
 identical(old$fixed_time_scales,new$fixed_time_scales),identical(old$recurrent_full_indices,new$recurrent_full_indices),
 new$parameter_counts[[1]]==old$parameter_counts[[1]]+2L)
s<-old$settings[[1]];ss<-new$settings[[1]]
stopifnot(s$plasticity=='constant',ss$plasticity=='error_rate',
 identical(s[setdiff(names(s),'plasticity')],ss[setdiff(names(ss),'plasticity')]))
records<-list()
for(j in 1:2) {
 path<-paths[j];p<-protocols[[j]];stopifnot(hash(file.path(path,'study.rds'))!='',hash(file.path(reference,'data/prepared_data.rds'))==p$data_sha256)
 fits<-read.csv(file.path(path,'reports/fits.csv'));stopifnot(nrow(fits)==15L,length(list.files(file.path(path,'models'),pattern='[.]rds$'))==15L)
 for(i in 1:15) {
  task<-p$tasks[i,,drop=FALSE];saved<-readRDS(file.path(path,'models',paste0(task$run_id,'.rds')));fit<-saved$fit
  settings<-p$settings[[1]];settings$seed<-as.integer(task$seed)
  foldpath<-file.path(reference,'partitions',paste0(task$partition,'.rds'));roles<-readRDS(foldpath)
  stopifnot(hash(foldpath)==p$partition_hashes[[task$partition]],identical(saved$task,task),
   identical(saved$settings,settings),identical(fit$gru_settings,settings),
   identical(saved$fingerprint,digest::digest(list(study=hash(file.path(path,'study.rds')),task=task,settings=settings),algo='sha256')),
   identical(fit$fit_rows,which(roles==1L)),identical(fit$validation_rows,which(roles==2L)),
   identical(fit$evaluation_rows,which(roles==3L)),fit$parameter_count==p$parameter_counts[[1]],
   all(is.finite(unlist(fit$model$weights))),fit$selected_epoch==fit$training_history$epoch[which.min(fit$training_history$validation_loss)])
  for(n in c('components','features','fixedpars','interc','maketimes','connectors'))stopifnot(identical(fit$model$recipe[[n]],p$baseline[[n]]))
  w<-fit$model$weights;lam<-as.numeric(w[['gate_fatigue.lambda']]);clock<-as.numeric(w[['gate_fatigue.time_scale']])
  stopifnot(length(lam)==3072L,all(lam>=0 & lam<=1),identical(clock,p$fixed_time_scales))
  row<-fits[fits$run_id==task$run_id,,drop=FALSE];stopifnot(nrow(row)==1L)
  close(row$output_eta,w[['plasticity.eta']]);close(row$lambda_median,median(lam))
  close(row$recovery_rate_per_minute,exp(as.numeric(w[['gate_fatigue.log_rho']])));
  close(row$recovery_half_life_minutes,log(2)/exp(as.numeric(w[['gate_fatigue.log_rho']])));
  if(j==2) {
   close(row$output_alpha,w[['plasticity.alpha']]);close(row$output_c,w[['plasticity.c']])
   stopifnot(all(unlist(w[c('plasticity.eta','plasticity.alpha','plasticity.c')])>=0))
   close(row$error_threshold,as.numeric(w[['plasticity.c']])/as.numeric(w[['plasticity.alpha']]))
  }
  q<-read.csv(file.path(path,'reports',paste0(task$run_id,'-predictions.csv')))
  stopifnot(identical(q$source_row,data$comparison_source_row[roles==3L]),all(q$observed==data$CF..ansbin.[roles==3L]),
   all(q$participant==data$Anon.Student.Id[roles==3L]))
  difference<-max(abs(q$prediction-fit$prediction[roles==3L]));stopifnot(difference<3e-6)
  for(k in 1:3) {
   pred<-fit$prediction[roles==k];y<-data$CF..ansbin.[roles==k]
   prefix<-c('training','validation','test')[k]
   auc<-as.numeric(pROC::auc(y,pred,quiet=TRUE,direction='<'));ll<--mean(dbinom(y,1,pred,log=TRUE))
   close(row[[paste0(prefix,'_AUC')]],auc);close(row[[paste0(prefix,'_log_loss')]],ll)
  }
  # Validate portable checkpoint loading on a successful fit and the failed fold.
  if(task$partition %in% c('cv1','cv4') && task$seed==1L) {
   runtime<-LKTGRURuntime(fit);loaded<-f('lkt_gru_weights')(runtime$network)
   stopifnot(identical(names(w),names(loaded)))
   for(n in names(w))close(w[[n]],loaded[[n]])
  }
  records[[length(records)+1L]]<-list(mode=if(j==1)'constant' else 'evolving',partition=task$partition,seed=task$seed,
   parameter_count=fit$parameter_count,selected_epoch=fit$selected_epoch,epochs_run=fit$trained_epochs,
   maximum_replay_difference=difference)
 }
}
jsonlite::write_json(list(passed=TRUE,saved_models_checked=30L,phase_metric_pairs=90L,
 matched_settings_except_output_rule=TRUE,fixed_clocks_identical=TRUE,models=records),out,pretty=TRUE,auto_unbox=TRUE)
cat('PASS 30 checkpoints: exact protocol/splits/settings, parameters, all phase metrics, fixed clocks, held-out replay CSV and portable loading.\n')
