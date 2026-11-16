# DOWNLOADED_STUDY_ROOT FULL_INPUT_RESULTS FRESH_AUDIT_DIRECTORY
a<-commandArgs(TRUE);stopifnot(length(a)==3L,!dir.exists(a[3]))
download<-a[1];out<-a[3];reference<-a[2];results<-file.path(download,'results-constant')
dir.create(out,recursive=TRUE)
hash<-function(p)digest::digest(file=p,algo='sha256')
p<-readRDS(file.path(results,'study.rds'));old<-readRDS(file.path(reference,'study.rds'))
d<-readRDS(file.path(results,'data/prepared_data.rds'))
stopifnot(p$total_fits==15L,nrow(p$tasks)==15L,all(p$tasks$candidate=='constant'),
 identical(p$settings$constant,old$settings$constant),hash(file.path(reference,'study.rds'))==p$reference_protocol_sha256,
 hash(file.path(results,'data/prepared_data.rds'))==p$data_sha256,identical(p$data_sha256,old$data_sha256),
 identical(p$partition_hashes,old$partition_hashes),identical(p$baseline$features,c('ppes','logitdec')),
 all(p$baseline$components=='KC..Default.'),identical(p$baseline$fixedpars,old$baseline$fixedpars[-1]))
for(n in names(p$script_hashes))stopifnot(hash(file.path(download,n))==p$script_hashes[[n]])
metrics<-function(y,q)c(AUC=as.numeric(pROC::auc(y,q,quiet=TRUE,direction='<')),log_loss=-mean(stats::dbinom(y,1,q,log=TRUE)))
rows<-list();predictions<-list();max_replay<-0
for(i in 1:15) {
 task<-p$tasks[i,,drop=FALSE];saved<-readRDS(file.path(results,'models',paste0(task$run_id,'.rds')));fit<-saved$fit
 roles<-readRDS(file.path(results,'partitions',paste0(task$partition,'.rds')))
 s<-p$settings$constant;s$seed<-task$seed
 stopifnot(hash(file.path(results,'partitions',paste0(task$partition,'.rds')))==p$partition_hashes[[task$partition]],
  identical(saved$settings,s),identical(fit$gru_settings,s),identical(saved$task,task),fit$parameter_count==43042L,
  identical(saved$fingerprint,digest::digest(list(study=hash(file.path(results,'study.rds')),task=task,settings=s),algo='sha256')),
  identical(fit$fit_rows,which(roles==1L)),identical(fit$validation_rows,which(roles==2L)),identical(fit$evaluation_rows,which(roles==3L)),
  fit$selected_epoch==fit$training_history$epoch[which.min(fit$training_history$validation_loss)])
 for(n in c('components','features','fixedpars','interc','connectors','maketimes'))stopifnot(identical(fit$model$recipe[[n]],p$baseline[[n]]))
 q<-read.csv(file.path(results,'reports',paste0(task$run_id,'-predictions.csv')))
 stopifnot(identical(q$source_row,d$comparison_source_row[roles==3L]),all(q$observed==d$CF..ansbin.[roles==3L]))
 max_replay<-max(max_replay,abs(q$prediction-fit$prediction[roles==3L]));stopifnot(max_replay<3e-6)
 m<-unlist(lapply(1:3,function(k)metrics(d$CF..ansbin.[roles==k],if(k==3L)q$prediction else fit$prediction[roles==k])),use.names=FALSE)
 names(m)<-c('training_AUC','training_log_loss','validation_AUC','validation_log_loss','test_AUC','test_log_loss')
 rows[[i]]<-cbind(task,as.data.frame(as.list(m)),eta=as.numeric(fit$model$weights[['plasticity.eta']]))
 q$feature_set<-'without_student_logitdec';predictions[[length(predictions)+1L]]<-q
 original<-read.csv(file.path(reference,'reports',paste0(task$run_id,'-predictions.csv')))
 stopifnot(identical(original$source_row,q$source_row),identical(original$observed,q$observed))
 original$feature_set<-'full';predictions[[length(predictions)+1L]]<-original
}
fits<-do.call(rbind,rows);reported<-read.csv(file.path(results,'reports/fits.csv'))
for(n in c('training_AUC','training_log_loss','validation_AUC','validation_log_loss','test_AUC','test_log_loss','eta'))stopifnot(max(abs(reported[[n]]-fits[[n]]))<1e-12)
pred<-data.table::as.data.table(do.call(rbind,predictions))
stopifnot(!anyDuplicated(pred[,.(feature_set,seed,source_row)]),all(pred[,.N,by=.(feature_set,seed)]$N==nrow(d)))
pooled<-pred[,as.list(metrics(observed,prediction)),by=.(feature_set,seed)]
summary<-aggregate(cbind(AUC,log_loss)~feature_set,as.data.frame(pooled),mean)
comparison<-merge(subset(as.data.frame(pooled),feature_set=='full'),subset(as.data.frame(pooled),feature_set!='full'),by='seed',suffixes=c('_full','_without'))
comparison$AUC_change<-comparison$AUC_without-comparison$AUC_full
comparison$log_loss_change<-comparison$log_loss_without-comparison$log_loss_full
for(n in c('fits','pooled','summary','comparison'))write.csv(get(n),file.path(out,paste0(n,'.csv')),row.names=FALSE)
# Recheck the previously failed paired table with explicit numeric column types.
f<-subset(read.csv(file.path(reference,'reports/fits.csv')),candidate=='constant')
expected<-merge(f,reported,by=c('partition','seed','candidate'),suffixes=c('_full','_without'))
stored<-read.csv(file.path(results,'reports/paired-fits.csv'))
for(n in names(expected)[vapply(expected,is.numeric,logical(1))])stored[[n]]<-as.numeric(stored[[n]])
stopifnot(isTRUE(all.equal(stored,expected,check.attributes=FALSE,tolerance=1e-12)))
jsonlite::write_json(list(complete=TRUE,new_fits=15L,reference_fits=15L,phase_metric_pairs=45L,pooled_metric_pairs=6L,
 exact_recipe_and_settings_verified=TRUE,maximum_saved_replay_difference=max_replay,
 report_checker_issue='All-NA numeric fatigue columns read as logical from CSV; explicit numeric types restore exact comparison. No fitted values changed.',
 protocol_sha256=hash(file.path(results,'study.rds'))),file.path(out,'verification.json'),auto_unbox=TRUE,pretty=TRUE,digits=17)
writeLines(c('15-FIT STUDENT-LEVEL LOGITDEC ABLATION','',
 'Constant-rate adaptive output connections, five matched folds and three seeds. Only student-level logitdec was removed.',
 capture.output(print(summary,row.names=FALSE,digits=9)),'',capture.output(print(comparison[,c('seed','AUC_change','log_loss_change')],row.names=FALSE,digits=9)),
 'AUC change is without minus full (positive better); log loss change is without minus full (negative better).',
 'All 15 fits completed. Independent checks verify exact settings, feature removal, partitions, model fingerprints, parameter counts and all 45 phase and six pooled metric pairs.',
 'The original report check failed because all-missing numeric fatigue parameter columns were read as logical from CSV. Explicit numeric types resolve that check; fitted results are unchanged.',
 'Performance near the full-input model supports practical redundancy within this adaptive model. This 15-fit test does not estimate the interaction against a newly fitted stock-without-logitdec condition.'),file.path(out,'RESULTS.txt'))
print(summary);print(comparison[,c('seed','AUC_change','log_loss_change')]);cat('PASS independent audit.\n')
