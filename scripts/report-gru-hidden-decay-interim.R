# ROOT SNAPSHOT LOCAL_PLAN ORIGINAL_REFERENCE OUTPUT
# Explicitly partial reporting. Do not relax the complete experiment's gates.
a <- commandArgs(TRUE);stopifnot(length(a)==5L,!dir.exists(a[5L]))
root<-a[1L];run<-a[2L];local<-a[3L];reference<-a[4L];output<-a[5L]
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
source(file.path(root,'scripts/gru-encoder-study.R'))
source(file.path(root,'scripts/gru-hidden-decay-study.R'))
p<-readRDS(file.path(run,'results/study.rds'));original<-readRDS(file.path(local,'study.rds'))
for(n in c('settings','tasks','baseline','partition_hashes','data_sha256','script_hashes','parameter_counts'))stopifnot(identical(p[[n]],original[[n]]))
for(n in names(p$script_hashes))stopifnot(study_hash(file.path(root,n))==p$script_hashes[[n]])
stopifnot(study_hash(file.path(reference,'study.rds'))==p$reference_protocol_sha256,
  study_hash(file.path(run,'results/data/prepared_data.rds'))==p$data_sha256)
d<-readRDS(file.path(run,'results/data/prepared_data.rds'))
ids<-sub('[.]rds$','',list.files(file.path(run,'results/models'),pattern='[.]rds$'))
stopifnot(length(ids)==29L,all(ids%in%p$tasks$run_id))
missing<-p$tasks[!p$tasks$run_id%in%ids,,drop=FALSE];stopifnot(nrow(missing)==1L)
records<-predictions<-list();control_differences<-numeric()
for(id in ids){
 task<-p$tasks[p$tasks$run_id==id,,drop=FALSE]
 saved<-readRDS(file.path(run,'results/models',paste0(id,'.rds')));f<-saved$fit
 s<-p$settings[[task$candidate]];s$seed<-as.integer(task$seed)
 file<-file.path(run,'results/partitions',paste0(task$partition,'.rds'));roles<-readRDS(file)
 stopifnot(study_hash(file)==p$partition_hashes[[task$partition]],identical(saved$settings,s),identical(f$gru_settings,s),
  identical(f$fit_rows,which(roles==1)),identical(f$validation_rows,which(roles==2)),identical(f$evaluation_rows,which(roles==3)),
  identical(saved$fingerprint,digest::digest(list(study=study_hash(file.path(run,'results/study.rds')),task=task,settings=s),algo='sha256')),
  f$selected_epoch==f$training_history$epoch[which.min(f$training_history$validation_loss)],
  f$parameter_count==p$parameter_counts[[task$candidate]],all(is.finite(unlist(f$model$weights))),
  file.exists(file.path(run,'results/reports',paste0(id,'-verification.txt'))))
 for(n in c('components','features','fixedpars','interc','maketimes','connectors'))stopifnot(identical(f$model$recipe[[n]],p$baseline[[n]]))
 if(task$candidate=='stock'){
  old<-readRDS(file.path(reference,'models',paste0(task$partition,'__omit1__seed',task$seed,'.rds')))$fit
  stopifnot(identical(f$model$recipe,old$model$recipe));difference<-max(abs(f$prediction-old$prediction));stopifnot(difference<3e-6)
  control_differences<-c(control_differences,difference)
 }
 q<-read.csv(file.path(run,'results/reports',paste0(id,'-predictions.csv')))
 stopifnot(identical(q$source_row,d$comparison_source_row[roles==3]),all(q$observed==d$CF..ansbin.[roles==3]),
  all(q$participant==d$Anon.Student.Id[roles==3]))
 delta<-max(abs(q$prediction-f$prediction[roles==3]));stopifnot(delta<3e-6)
 values<-numeric()
 for(k in 1:3){yy<-d$CF..ansbin.[roles==k];pp<-if(k==3)q$prediction else f$prediction[roles==k]
  m<-study_metrics(yy,pp)[c('AUC','log_loss')]
  stopifnot(abs(m[['AUC']]-as.numeric(pROC::auc(yy,pp,quiet=TRUE,direction='<')))<1e-12,
   abs(m[['log_loss']]+mean(stats::dbinom(yy,1,pp,log=TRUE)))<1e-12)
  names(m)<-paste(c('training','validation','test')[k],names(m),sep='_');values<-c(values,m)
 }
 records[[id]]<-cbind(task,condition=task$candidate,as.data.frame(as.list(values)),as.data.frame(hidden_study_parameters(f)),
  selected_epoch=f$selected_epoch,epochs_run=f$trained_epochs,parameter_count=f$parameter_count,
  maximum_replay_probability_difference=delta)
 predictions[[id]]<-q
}
fits<-do.call(rbind,records);rownames(fits)<-NULL;fits<-fits[order(fits$partition,fits$seed,fits$condition),]
paired<-merge(fits[fits$condition=='stock',],fits[fits$condition=='hidden_decay',],by=c('partition','seed'),suffixes=c('_stock','_decay'))
stopifnot(nrow(paired)==14L)
pooled<-list();pred<-do.call(rbind,predictions)
for(seed in 1:3){parts<-paired$partition[paired$seed==seed]
 for(condition in c('stock','hidden_decay')){
  q<-pred[pred$seed==seed & pred$condition==condition & pred$partition%in%parts,]
  stopifnot(!anyDuplicated(q$source_row))
  pooled[[paste(seed,condition)]]<-data.frame(condition=condition,seed=seed,folds=length(parts),complete_CV=length(parts)==5,
   as.list(study_metrics(q$observed,q$prediction)[c('AUC','log_loss')]))
 }
}
pooled<-do.call(rbind,pooled);complete<-pooled[pooled$complete_CV,]
summary<-do.call(rbind,lapply(c('stock','hidden_decay'),function(condition){x<-complete[complete$condition==condition,]
 data.frame(condition=condition,complete_seeds=paste(x$seed,collapse=','),AUC=mean(x$AUC),log_loss=mean(x$log_loss))}))
summary$AUC_change<-summary$AUC-summary$AUC[1];summary$log_loss_change<-summary$log_loss-summary$log_loss[1]
paired_mean<-do.call(rbind,lapply(c('stock','decay'),function(which){data.frame(condition=which,matched_pairs=14L,
 test_AUC=mean(paired[[paste0('test_AUC_',which)]]),test_log_loss=mean(paired[[paste0('test_log_loss_',which)]]))}))
params<-fits[fits$condition=='hidden_decay',c('partition','seed','raw_a','raw_b','raw_c','baseline_hidden_half_life_days','DK_half_life_days','test_AUC','test_log_loss')]
dir.create(output,recursive=TRUE)
for(n in c('fits','paired','pooled','summary','paired_mean','params','missing'))write.csv(get(n),file.path(output,paste0(n,'.csv')),row.names=FALSE)
summary_rows<-vapply(seq_len(nrow(summary)),function(i){r<-summary[i,];sprintf('| %s | %.8f | %.8f | %+.8f | %+.8f |',r$condition,r$AUC,r$log_loss,r$AUC_change,r$log_loss_change)},character(1))
parameter_rows<-vapply(seq_len(nrow(params)),function(i){r<-params[i,];sprintf('| %s | %d | %.6f | %.6f | %.6f | %.6f | %.6f | %.6f | %.6f |',r$partition,r$seed,r$raw_a,r$raw_b,r$raw_c,r$baseline_hidden_half_life_days,r$DK_half_life_days,r$test_AUC,r$test_log_loss)},character(1))
fit_rows<-vapply(seq_len(nrow(fits)),function(i){r<-fits[i,];sprintf('| %s | %s | %d | %.6f | %.6f | %.6f | %.6f | %.6f | %.6f |',r$condition,r$partition,r$seed,r$training_AUC,r$training_log_loss,r$validation_AUC,r$validation_log_loss,r$test_AUC,r$test_log_loss)},character(1))
writeLines(c('# Interim Cloze comparison: 29 of 30 verified fits','',
 sprintf('Missing: %s. Its stock counterpart is included in fits.csv, but excluded from paired comparisons until the decay fit is available.',missing$run_id),'',
 '## Complete-seed held-out scores','',
 'Seeds 1 and 3 each have all five test folds for both conditions. The table pools test folds within seed, then averages those two complete seeds. Seed 2 is incomplete and excluded from this headline. These are interim results, not the final three-seed comparison.','',
 '| Condition | AUC | Log loss | AUC change | Loss change |','|---|---:|---:|---:|---:|',summary_rows,'',
 'paired_mean.csv separately reports means across all fourteen matched fold/seed pairs. pooled.csv includes matched available-fold scores for every seed and marks whether all five folds are present. No missing result is filled in or compared against an unmatched cohort.','',
 '## Learned decay parameters and half-lives','',
 '| Fold | Seed | a | b | c | Hidden half-life at DK=0 (days) | DK half-life (days) | Test AUC | Test log loss |',
 '|---|---:|---:|---:|---:|---:|---:|---:|---:|',parameter_rows,'',
 'Half-lives are log(2)/softplus(a) and log(2)/softplus(c). b is a signed influence coefficient without its own half-life. All raw parameters start at zero, corresponding to one-day half-lives, not zero decay.','',
 '## All 29 available fits','',
 '| Condition | Fold | Seed | Train AUC | Train loss | Validation AUC | Validation loss | Test AUC | Test loss |',
 '|---|---|---:|---:|---:|---:|---:|---:|---:|',fit_rows,'',
 'All available checkpoints matched the frozen local settings, participant roles and feature recipe; saved-weight replay vectors matched saved probabilities within 3e-6. All 87 phase AUC/log-loss pairs were independently checked with pROC and dbinom. All fifteen fresh stock fits matched the original six-input controls. The running model and final-report gates were left unchanged. The final 30-fit report will be a separate artifact, preserving this interim snapshot.'),file.path(output,'RESULTS.md'))
jsonlite::write_json(list(interim=TRUE,verified_fits=29L,missing=missing$run_id,paired_fits=14L,complete_seeds=c(1L,3L),
 phase_metric_pairs_checked=87L,maximum_replay_probability_difference=max(fits$maximum_replay_probability_difference),
 stock_controls_checked=15L,maximum_stock_control_probability_difference=max(control_differences)),file.path(output,'verification.json'),pretty=TRUE,auto_unbox=TRUE)
print(summary);print(paired_mean);print(params);print(missing)
cat('PASS 29-fit interim report; 87 independent phase metric pairs and all fifteen stock controls checked.\n')
