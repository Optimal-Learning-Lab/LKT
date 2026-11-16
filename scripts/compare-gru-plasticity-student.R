# ROOT OUTPUT plan|fit|report|check TASK CANDIDATE_JSON LIBRARY [REFERENCE]
# Plan references the completed no-student-logitdec preflight; report/check
# reference the completed full-input study. Exactly 15 constant-rate fits.
a<-commandArgs(TRUE);stopifnot(length(a)%in%c(6L,7L))
root<-a[1];out<-a[2];action<-a[3]
stopifnot(action%in%c('plan','fit','report','check'))
source(file.path(root,'scripts/gru-encoder-study.R'))
candidate<-study_require_release(a[5],a[6])
source(file.path(root,'scripts/gru-plasticity-study.R'))
files<-c('scripts/compare-gru-plasticity-student.R','scripts/gru-encoder-study.R','scripts/gru-plasticity-study.R')
hashes<-setNames(vapply(file.path(root,files),study_hash,character(1)),files)
if(action=='plan') {
 stopifnot(length(a)==7L,!dir.exists(out))
 prior<-a[7];p<-readRDS(file.path(prior,'study.rds'))
 eq<-read.csv(file.path(prior,'reports/stock-equivalence.csv'))
 stopifnot(identical(p$candidate,candidate),p$total_fits==45L,
  p$removed_feature=='lkt:logitdecAnon.Student.Id',sum(eq$condition=='constant')==15L,
  all(eq$maximum_logit_difference==0),all(eq$numeric_inputs==5L))
 p$preflight_protocol_sha256<-study_hash(file.path(prior,'study.rds'))
 p$preflight_equivalence_sha256<-study_hash(file.path(prior,'reports/stock-equivalence.csv'))
 p$tasks<-p$tasks[p$tasks$candidate=='constant',,drop=FALSE];rownames(p$tasks)<-NULL
 p$settings<-p$settings['constant'];p$parameter_counts<-p$parameter_counts['constant']
 p$total_fits<-15L;p$script_hashes<-hashes
 p$design<-'15 constant-rate adaptive fits without student-level logitdec, compared with the 15 completed full-input constant-rate fits. No new stock or fatigue fits.'
 dir.create(out,recursive=TRUE)
 for(n in c('data','partitions','models','training','reports'))dir.create(file.path(out,n))
 stopifnot(file.copy(file.path(prior,'data/prepared_data.rds'),file.path(out,'data/prepared_data.rds')))
 for(part in p$partitions)stopifnot(file.copy(file.path(prior,'partitions',paste0(part,'.rds')),file.path(out,'partitions',paste0(part,'.rds'))))
 study_write_rds(p,file.path(out,'study.rds'));study_write_csv(p$tasks,file.path(out,'tasks.csv'))
 writeLines(capture.output(dput(p)),file.path(out,'study.txt'))
 cat('FROZEN exactly 15 constant-rate adaptation fits; passed preflight reused.\n');quit(save='no')
}
p<-readRDS(file.path(out,'study.rds'))
stopifnot(p$total_fits==15L,nrow(p$tasks)==15L,identical(p$script_hashes,hashes),identical(p$candidate,candidate),
 study_hash(a[5])==p$candidate_file_sha256,study_hash(file.path(out,'data/prepared_data.rds'))==p$data_sha256)
for(part in p$partitions)stopifnot(study_hash(file.path(out,'partitions',paste0(part,'.rds')))==p$partition_hashes[[part]])
if(action=='fit') {
 id<-as.integer(a[4]);stopifnot(length(id)==1L,!is.na(id),id>=0L,id<15L)
 task<-p$tasks[id+1L,,drop=FALSE];d<-readRDS(file.path(out,'data/prepared_data.rds'))
 d$fold<-readRDS(file.path(out,'partitions',paste0(task$partition,'.rds')))
 plastic_study_fit(task,p$settings$constant,out,p,d)
}
if(action%in%c('report','check')) {
 stopifnot(length(a)==7L,length(list.files(file.path(out,'models'),pattern='[.]rds$'))==15L)
 reference<-a[7];stopifnot(study_hash(file.path(reference,'study.rds'))==p$reference_protocol_sha256)
 for(n in names(p$full_report_hashes))stopifnot(study_hash(file.path(reference,'reports',n))==p$full_report_hashes[[n]])
 d<-readRDS(file.path(out,'data/prepared_data.rds'));records<-predictions<-list()
 for(i in 1:15) {
  task<-p$tasks[i,,drop=FALSE];saved<-readRDS(file.path(out,'models',paste0(task$run_id,'.rds')));fit<-saved$fit
  s<-p$settings$constant;s$seed<-task$seed
  roles<-readRDS(file.path(out,'partitions',paste0(task$partition,'.rds')))
  stopifnot(identical(saved$settings,s),identical(fit$gru_settings,s),identical(saved$task,task),
   identical(saved$fingerprint,digest::digest(list(study=study_hash(file.path(out,'study.rds')),task=task,settings=s),algo='sha256')),
   identical(fit$fit_rows,which(roles==1L)),identical(fit$validation_rows,which(roles==2L)),identical(fit$evaluation_rows,which(roles==3L)),
   fit$parameter_count==43042L,fit$selected_epoch==fit$training_history$epoch[which.min(fit$training_history$validation_loss)])
  q<-read.csv(file.path(out,'reports',paste0(task$run_id,'-predictions.csv')))
  stopifnot(identical(q$source_row,d$comparison_source_row[roles==3L]),all(q$observed==d$CF..ansbin.[roles==3L]),
   max(abs(q$prediction-fit$prediction[roles==3L]))<3e-6)
  metrics<-unlist(lapply(1:3,function(k) {
   y<-d$CF..ansbin.[roles==k];pr<-if(k==3L)q$prediction else fit$prediction[roles==k]
   m<-study_metrics(y,pr)[c('AUC','log_loss')]
   stopifnot(abs(m[1]-as.numeric(pROC::auc(y,pr,quiet=TRUE,direction='<')))<1e-12,
    abs(m[2]+mean(stats::dbinom(y,1,pr,log=TRUE)))<1e-12);m
  }),use.names=FALSE)
  names(metrics)<-c('training_AUC','training_log_loss','validation_AUC','validation_log_loss','test_AUC','test_log_loss')
  records[[i]]<-cbind(task,as.data.frame(as.list(metrics)),eta=as.numeric(fit$model$weights[['plasticity.eta']]),
   epochs_run=fit$trained_epochs,selected_epoch=fit$selected_epoch,elapsed_seconds=saved$elapsed)
  predictions[[i]]<-q
 }
 fits<-do.call(rbind,records);pred<-data.table::as.data.table(do.call(rbind,predictions))
 stopifnot(!anyDuplicated(pred[,.(seed,source_row)]),all(pred[,.N,by=seed]$N==nrow(d)))
 pooled<-pred[,as.list(study_metrics(observed,prediction)[c('AUC','log_loss')]),by=seed]
 full<-read.csv(file.path(reference,'reports/fits.csv'));full<-subset(full,candidate=='constant')
 paired<-merge(full,fits,by=c('partition','seed','candidate'),suffixes=c('_full','_without'))
 baseline<-subset(read.csv(file.path(reference,'reports/pooled-by-seed.csv')),condition=='constant')
 comparison<-merge(baseline,as.data.frame(pooled),by='seed',suffixes=c('_full','_without'))
 comparison$log_loss_removal_penalty<-comparison$log_loss_without-comparison$log_loss_full
 comparison$AUC_removal_penalty<-comparison$AUC_full-comparison$AUC_without
 tables<-list(fits=fits,pooled_by_seed=pooled,paired_fits=paired,comparison_by_seed=comparison)
 for(n in names(tables)) {
  path<-file.path(out,'reports',paste0(gsub('_','-',n),'.csv'))
  if(action=='report')study_write_csv(tables[[n]],path) else {
   expected<-as.data.frame(tables[[n]]);stored<-read.csv(path)
   # CSV has no type metadata: all-NA numeric fatigue columns in a constant-only
   # comparison are read as logical. Restore declared numeric types explicitly.
   for(column in names(expected)[vapply(expected,is.numeric,logical(1))])stored[[column]]<-as.numeric(stored[[column]])
   stopifnot(isTRUE(all.equal(stored,expected,check.attributes=FALSE,tolerance=1e-12)))
  }
 }
 if(action=='report') {
  writeLines(c('STUDENT-LEVEL LOGITDEC: 15-FIT CONSTANT-ADAPTATION TEST','',
   'Only student-level logitdec and its fixed parameter were removed. Compare 15 new fits with the completed 15 full-input constant-adaptation fits. Other predictors, folds, seeds and controls match.',
   capture.output(print(comparison,row.names=FALSE,digits=9)),
   'Positive removal penalty means logitdec helped. This comparison tests whether the adaptive model retains performance without logitdec; it does not separately estimate how stock responds to removal.'),file.path(out,'reports/RESULTS.txt'))
  jsonlite::write_json(list(complete=TRUE,new_fits=15L,reference_fits=15L,phase_metric_pairs_checked=45L,
   protocol_sha256=study_hash(file.path(out,'study.rds'))),file.path(out,'reports/verification.json'),auto_unbox=TRUE,pretty=TRUE)
 }
 print(comparison)
}
