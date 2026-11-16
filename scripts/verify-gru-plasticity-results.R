# RESULTS PREVIOUS_FAST_RESULTS FRESH_AUDIT_OUTPUT
# Independent saved-array audit: no fitting or modification of fitted artifacts.
a<-commandArgs(TRUE);stopifnot(length(a)==3L,!dir.exists(a[3]))
results<-a[1];previous<-a[2];output<-a[3];dir.create(output,recursive=TRUE)
p<-readRDS(file.path(results,'study.rds'));d<-readRDS(file.path(results,'data/prepared_data.rds'))
hash<-function(x)digest::digest(file=x,algo='sha256')
stopifnot(p$total_fits==45L,identical(p$seeds,1:3),identical(names(p$settings),c('stock','constant','fatigue')),
 hash(file.path(results,'data/prepared_data.rds'))==p$data_sha256,
 identical(p$baseline$features,c('logitdec','ppes','logitdec')))
metrics<-function(y,q)c(AUC=as.numeric(pROC::auc(y,q,quiet=TRUE,direction='<')),
 log_loss=-mean(stats::dbinom(y,1,q,log=TRUE)))
rows<-list();stock_errors<-numeric();predictions<-list()
for(i in seq_len(nrow(p$tasks))) {
 task<-p$tasks[i,,drop=FALSE];saved<-readRDS(file.path(results,'models',paste0(task$run_id,'.rds')));fit<-saved$fit
 roles<-readRDS(file.path(results,'partitions',paste0(task$partition,'.rds')))
 stopifnot(hash(file.path(results,'partitions',paste0(task$partition,'.rds')))==p$partition_hashes[[task$partition]],
  !anyDuplicated(unique(data.frame(student=d$Anon.Student.Id,role=roles))$student),
  identical(fit$fit_rows,which(roles==1L)),identical(fit$validation_rows,which(roles==2L)),
  identical(fit$evaluation_rows,which(roles==3L)))
 expected<-p$settings[[task$candidate]];expected$seed<-task$seed
 stopifnot(identical(expected,fit$gru_settings),identical(saved$settings,expected),
  identical(saved$fingerprint,digest::digest(list(study=hash(file.path(results,'study.rds')),task=task,settings=expected),algo='sha256')),
  fit$parameter_count==p$parameter_counts[[task$candidate]],!fit$gru_settings$hidden_decay,
  fit$selected_epoch==fit$training_history$epoch[which.min(fit$training_history$validation_loss)],
  all(is.finite(unlist(fit$model$weights))))
 q<-read.csv(file.path(results,'reports',paste0(task$run_id,'-predictions.csv')))
 stopifnot(identical(q$source_row,d$comparison_source_row[roles==3L]),all(q$observed==d$CF..ansbin.[roles==3L]),
  max(abs(q$prediction-fit$prediction[roles==3L]))<3e-6)
 # Held-out reporting uses the separately replayed saved-model probabilities;
 # batching can differ from the all-partition replay by float32 rounding.
 m<-unlist(lapply(1:3,function(k)metrics(d$CF..ansbin.[roles==k],
   if(k==3L)q$prediction else fit$prediction[roles==k])),use.names=FALSE)
 names(m)<-c('training_AUC','training_log_loss','validation_AUC','validation_log_loss','test_AUC','test_log_loss')
 pars<-read.csv(file.path(results,'reports',paste0(task$run_id,'-parameters.csv')))
 eta<-if(task$candidate=='stock')NA_real_ else as.numeric(fit$model$weights[['plasticity.eta']])
 kappa<-rho<-half<-NA_real_
 if(task$candidate=='fatigue') {kappa<-as.numeric(fit$model$weights[['plasticity.kappa']]);rho<-exp(as.numeric(fit$model$weights[['plasticity.log_rho']]));half<-log(2)/rho}
 for(n in c('eta','kappa','rho','half')) {
  column<-switch(n,eta='eta',kappa='kappa',rho='recovery_rate_per_minute',half='recovery_half_life_minutes')
  stopifnot(isTRUE(all.equal(get(n),as.numeric(pars[[column]]),tolerance=1e-12)))
 }
 if(task$candidate=='stock') {
  old<-readRDS(file.path(previous,'models',paste0(task$partition,'__stock_full__seed',task$seed,'.rds')))$fit
  e<-max(abs(old$prediction-fit$prediction));stock_errors<-c(stock_errors,e);stopifnot(e<3e-6)
 }
 rows[[i]]<-cbind(task,as.data.frame(as.list(m)),eta=eta,kappa=kappa,recovery_rate_per_minute=rho,
  recovery_half_life_minutes=half,negative_rate_use_fraction=pars$negative_rate_use_fraction,
  negative_rate_uses=pars$negative_rate_uses,active_connection_updates=pars$active_connection_updates,
  minimum_training_log_loss=min(fit$training_history$training_eval_loss),
  selected_epoch=fit$selected_epoch,epochs_run=fit$trained_epochs,elapsed_seconds=saved$elapsed)
 predictions[[i]]<-q
}
fits<-do.call(rbind,rows);pred<-data.table::as.data.table(do.call(rbind,predictions))
stopifnot(!anyDuplicated(pred[,.(condition,seed,source_row)]),all(pred[,.N,by=.(condition,seed)]$N==nrow(d)))
pooled<-pred[,as.list(metrics(observed,prediction)),by=.(condition,seed)]
for(name in c('fits','pooled'))write.csv(get(name),file.path(output,paste0(name,'.csv')),row.names=FALSE)
reported<-read.csv(file.path(results,'reports/fits.csv'))
for(n in c('training_AUC','training_log_loss','validation_AUC','validation_log_loss','test_AUC','test_log_loss'))
 stopifnot(max(abs(reported[[n]]-fits[[n]]))<1e-12)
report_pooled<-read.csv(file.path(results,'reports/pooled-by-seed.csv'))
ind<-merge(report_pooled,as.data.frame(pooled),by=c('condition','seed'),suffixes=c('_report','_audit'))
stopifnot(max(abs(ind$AUC_report-ind$AUC_audit))<1e-12,max(abs(ind$log_loss_report-ind$log_loss_audit))<1e-12)
summary<-aggregate(cbind(AUC,log_loss)~condition,as.data.frame(pooled),mean)
write.csv(summary,file.path(output,'summary.csv'),row.names=FALSE)
jsonlite::write_json(list(complete=TRUE,fits=45L,phase_pairs=135L,pooled_pairs=9L,
 maximum_previous_stock_probability_difference=max(stock_errors),
 all_settings_partitions_parameters_and_metrics_verified=TRUE,
 protocol_sha256=hash(file.path(results,'study.rds'))),file.path(output,'verification.json'),auto_unbox=TRUE,pretty=TRUE,digits=17)
print(summary);cat('PASS independent audit; prior stock maximum probability difference:',max(stock_errors),'\n')
