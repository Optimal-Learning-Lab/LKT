# ABLATION_RESULTS FULL_INPUT_REFERENCE report|check
a<-commandArgs(TRUE);stopifnot(length(a)==3L,a[3]%in%c('report','check'))
out<-a[1];reference<-a[2];action<-a[3]
hash<-function(p)digest::digest(file=p,algo='sha256')
p<-readRDS(file.path(out,'study.rds'));old<-readRDS(file.path(reference,'study.rds'))
stopifnot(hash(file.path(reference,'study.rds'))==p$reference_protocol_sha256,
 identical(p$settings,old$settings),identical(p$data_sha256,old$data_sha256),
 identical(p$partition_hashes,old$partition_hashes))
for(n in names(p$full_report_hashes))stopifnot(hash(file.path(reference,'reports',n))==p$full_report_hashes[[n]])
full<-read.csv(file.path(reference,'reports/fits.csv'));without<-read.csv(file.path(out,'reports/fits.csv'))
paired<-merge(full,without,by=c('partition','seed','candidate'),suffixes=c('_full','_without'))
stopifnot(nrow(paired)==45L)
paired$test_log_loss_removal_penalty<-paired$test_log_loss_without-paired$test_log_loss_full
paired$test_AUC_removal_penalty<-paired$test_AUC_full-paired$test_AUC_without
paired$training_log_loss_removal_penalty<-paired$training_log_loss_without-paired$training_log_loss_full
f<-read.csv(file.path(reference,'reports/pooled-by-seed.csv'));w<-read.csv(file.path(out,'reports/pooled-by-seed.csv'))
pooled<-merge(f,w,by=c('condition','seed'),suffixes=c('_full','_without'))
stopifnot(nrow(pooled)==9L)
pooled$log_loss_removal_penalty<-pooled$log_loss_without-pooled$log_loss_full
pooled$AUC_removal_penalty<-pooled$AUC_full-pooled$AUC_without
interactions<-list()
for(target_condition in c('constant','fatigue')) {
 x<-merge(subset(pooled,condition=='stock'),subset(pooled,condition==target_condition),by='seed',suffixes=c('_stock','_adaptive'))
 interactions[[target_condition]]<-data.frame(condition=target_condition,seed=x$seed,
  log_loss_substitution=x$log_loss_removal_penalty_stock-x$log_loss_removal_penalty_adaptive,
  AUC_substitution=x$AUC_removal_penalty_stock-x$AUC_removal_penalty_adaptive)
}
interactions<-do.call(rbind,interactions)
summary<-aggregate(cbind(AUC_full,AUC_without,log_loss_full,log_loss_without,log_loss_removal_penalty,AUC_removal_penalty)~condition,pooled,mean)
tables<-list(logitdec_paired_fits=paired,logitdec_pooled_by_seed=pooled,logitdec_substitution=interactions,logitdec_summary=summary)
for(n in names(tables)) {
 path<-file.path(out,'reports',paste0(gsub('_','-',n),'.csv'))
 if(action=='report')write.csv(tables[[n]],path,row.names=FALSE) else
  stopifnot(isTRUE(all.equal(read.csv(path),tables[[n]],check.attributes=FALSE,tolerance=1e-12)))
}
if(action=='report') {
 writeLines(c('STUDENT-LEVEL LOGITDEC REDUNDANCY TEST','',
  '45 new fits remove only student-level logitdec and its single fixed feature parameter. Compare against the completed 45 full-input fits. Stock, constant adaptation and fatigue/recovery; five unchanged participant folds and seeds 1-3.',
  'PPES, student-item logitdec, item age, prior spacing and spacing availability remain. All training settings and validation selection remain unchanged. Removing one numeric coordinate reduces each model by 32 parameters; no dummy input or compensating capacity is added. Same seeds do not imply identical weights across different input widths.','',
  capture.output(print(summary,row.names=FALSE,digits=9)),'',
  'A positive removal penalty means the feature helped. Positive substitution means the adaptive model lost less from removing student-level logitdec than stock did.',
  capture.output(print(interactions,row.names=FALSE,digits=9)),'',
  'Both raw performance without the feature and the change from each model\'s full-input reference must be inspected. Beating stock without the feature alone does not prove replacement.',
  'No held-out tuning, changes to the GRU implementation, historical-result modification or central release promotion.'),file.path(out,'reports/LOGITDEC-RESULTS.txt'))
 jsonlite::write_json(list(complete=TRUE,new_fits=45L,reference_fits=45L,paired_fold_seed_conditions=45L,
  source_protocol_sha256=hash(file.path(out,'study.rds')),reference_protocol_sha256=p$reference_protocol_sha256),
  file.path(out,'reports/logitdec-verification.json'),pretty=TRUE,auto_unbox=TRUE)
}
print(summary)
