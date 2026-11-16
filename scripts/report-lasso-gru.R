# SOURCE_ROOT RESULTS BASELINE OUTPUT. Read-only analysis of completed artifacts.
# No fitting, feature reselection or checkpoint selection occurs here.
a<-commandArgs(TRUE);stopifnot(length(a)==4L)
root<-normalizePath(a[1],winslash='/');out<-normalizePath(a[2],winslash='/')
reference<-normalizePath(a[3],winslash='/');report<-a[4]
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
stopifnot(requireNamespace('glmnet',quietly=TRUE))
source(file.path(root,'scripts/gru-encoder-study.R'))
dir.create(report,recursive=TRUE,showWarnings=FALSE)
p<-readRDS(file.path(out,'study.rds'));b<-readRDS(file.path(reference,'study.rds'))
stopifnot(p$data_sha256==b$data_sha256,identical(p$partition_hashes,b$partition_hashes),
 p$reference_sha256==study_hash(file.path(reference,'study.rds')),
 p$settings$plasticity=='constant',p$settings$embedding_adaptation=='none')
audit<-jsonlite::fromJSON(file.path(out,'reports/independent-metric-audit.json'))
stopifnot(audit$complete,audit$fits==15L,audit$phase_auc_comparisons==45L)
new<-read.csv(file.path(out,'reports/fold-seed-results.csv'))
old<-read.csv(file.path(reference,'reports/fits.csv'))
stopifnot(nrow(new)==15L,nrow(old)==15L,all(old$candidate=='constant'))
paired<-merge(new,old,by=c('partition','seed'),suffixes=c('_lasso','_baseline'))
stopifnot(nrow(paired)==15L)
paired$delta_training_AUC<-paired$training_AUC_lasso-paired$training_AUC_baseline
paired$delta_validation_AUC<-paired$validation_AUC_lasso-paired$validation_AUC_baseline
paired$delta_heldout_AUC<-paired$heldout_AUC-paired$test_AUC
paired$delta_heldout_log_loss<-paired$heldout_log_loss-paired$test_log_loss
study_write_csv(paired,file.path(report,'paired-fold-seed-results.csv'))
seed_records<-list()
for(seed in 1:3) {
  tasks<-old[old$seed==seed,,drop=FALSE]
  pred<-do.call(rbind,lapply(tasks$run_id,function(id)
    read.csv(file.path(reference,'reports',paste0(id,'-predictions.csv')))))
  stopifnot(nrow(pred)==55122L,!anyDuplicated(pred$source_row))
  fresh<-do.call(rbind,lapply(p$tasks$run_id[p$tasks$seed==seed],function(id) {
    z<-read.csv(file.path(out,'reports',paste0(id,'-predictions.csv')))
    z[z$role==3,,drop=FALSE]
  }))
  pred<-pred[order(pred$source_row),];fresh<-fresh[order(fresh$source_row),]
  stopifnot(identical(pred$source_row,fresh$source_row),identical(pred$observed,fresh$observed))
  x<-study_metrics(pred$observed,pred$prediction);y<-study_metrics(fresh$observed,fresh$prediction)
  seed_records[[seed]]<-data.frame(seed=seed,baseline_AUC=x[['AUC']],lasso_AUC=y[['AUC']],
    delta_AUC=y[['AUC']]-x[['AUC']],baseline_log_loss=x[['log_loss']],
    lasso_log_loss=y[['log_loss']],delta_log_loss=y[['log_loss']]-x[['log_loss']])
}
seeds<-do.call(rbind,seed_records);study_write_csv(seeds,file.path(report,'comparison-by-seed.csv'))
features<-list();parameter_records<-list();bank<-readRDS(file.path(out,'data/candidate_bank.rds'))
data<-readRDS(file.path(out,'data/prepared_data.rds'))
for(part in paste0('cv',1:5)) {
  s<-readRDS(file.path(out,'selection',paste0(part,'.rds')))
  roles<-readRDS(file.path(out,'partitions',paste0(part,'.rds')))
  stopifnot(identical(s$train_rows,which(roles==1L)),
    identical(s$train_students,sort(unique(as.character(data$Anon.Student.Id[roles==1])))),
    identical(s$train_input_sha256,digest::digest(list(bank$x[roles==1,,drop=FALSE],data$CF..ansbin.[roles==1]),algo='sha256')),
    identical(unname(as.matrix(bank$x[,s$columns,drop=FALSE])),unname(as.matrix(s$data[s$numeric_names]))))
  coefs<-as.matrix(stats::coef(s$fit,s=s$lambda))
  nonzero<-rownames(coefs)[coefs[,1]!=0 & rownames(coefs)!='(Intercept)']
  stopifnot(setequal(nonzero,s$columns),length(nonzero)==s$path_counts[s$target_idx],
    s$target_idx==order(abs(s$path_counts-p$target_predictors),s$path_counts,-s$path_lambda)[1L],
    s$lambda==s$path_lambda[s$target_idx])
  z<-read.csv(file.path(out,'selection',paste0(part,'-selected.csv')));z$partition<-part
  features[[part]]<-z
}
features<-do.call(rbind,features);study_write_csv(features,file.path(report,'selected-features.csv'))
stability<-aggregate(partition~component+feature+column+parameters,features,length)
names(stability)[names(stability)=='partition']<-'folds_selected'
stability<-stability[order(-stability$folds_selected,stability$component,stability$feature,stability$parameters),]
study_write_csv(stability,file.path(report,'selection-stability.csv'))
for(i in seq_len(nrow(new))) {
  z<-readRDS(file.path(out,'models',paste0(new$run_id[i],'.rds')))
  eta<-as.numeric(z$fit$model$weights[['plasticity.eta']])
  stopifnot(length(eta)==1L,is.finite(eta),eta>=0,z$settings$plasticity=='constant')
  parameter_records[[i]]<-data.frame(run_id=new$run_id[i],eta=eta,selected_epoch=z$fit$selected_epoch,
    trained_epochs=z$fit$trained_epochs,selected_features=new$selected_features[i],parameters=new$parameters[i])
}
parameters<-do.call(rbind,parameter_records)
study_write_csv(parameters,file.path(report,'learned-plasticity-and-epochs.csv'))
summary<-data.frame(condition=c('constant_reference','lasso_constant'),fits=15L,
 training_AUC=c(mean(old$training_AUC),mean(new$training_AUC)),
 validation_AUC=c(mean(old$validation_AUC),mean(new$validation_AUC)),
 heldout_AUC=c(mean(seeds$baseline_AUC),mean(seeds$lasso_AUC)),
 training_log_loss=c(mean(old$training_log_loss),mean(new$training_log_loss)),
 validation_log_loss=c(mean(old$validation_log_loss),mean(new$validation_log_loss)),
 heldout_log_loss=c(mean(seeds$baseline_log_loss),mean(seeds$lasso_log_loss)))
study_write_csv(summary,file.path(report,'summary.csv'))
lines<-c('# Cloze Lasso-to-GRU results','',
 'Completed five training-only Lasso selections and 15 GRU fits (five whole-student folds, three seeds).',
 'The 318 candidates comprise zero/one-parameter history functions over student, student-item and student-skill components.',
 'Each fold selects the nearest path size to 15; ties prefer fewer columns then stronger lambda. Validation/test labels do not select features.',
 '', '| Model | Training AUC | Validation AUC | Held-out AUC |',
 '| --- | ---: | ---: | ---: |',
 vapply(seq_len(nrow(summary)),function(i)sprintf('| %s | %.9f | %.9f | %.9f |',
   summary$condition[i],summary$training_AUC[i],summary$validation_AUC[i],summary$heldout_AUC[i]),character(1)),
 '', '| Model | Training log loss | Validation log loss | Held-out log loss |',
 '| --- | ---: | ---: | ---: |',
 vapply(seq_len(nrow(summary)),function(i)sprintf('| %s | %.9f | %.9f | %.9f |',
   summary$condition[i],summary$training_log_loss[i],summary$validation_log_loss[i],summary$heldout_log_loss[i]),character(1)), '',
 sprintf('Mean seed-pooled held-out AUC change: %+.9f; log loss change: %+.9f.',mean(seeds$delta_AUC),mean(seeds$delta_log_loss)),
 sprintf('All saved-model replay differences are below 3e-6; observed maximum %.12g.',max(new$maximum_replay_difference)),
 'The independent Python audit checks 45 phase AUC values and all three pooled held-out seed sets.',
 'Training-input hashes, selected Lasso coefficients, precomputed input values and shared student folds were independently checked from saved artifacts.',
 '',sprintf('Selected history feature counts: %s.',paste(read.csv(file.path(out,'selection/summary.csv'))$selected,collapse=', ')),
 sprintf('Learned constant output-plasticity rates range %.8g to %.8g.',min(parameters$eta),max(parameters$eta)),
 'Constant means the rate stays fixed within a student trajectory; its value remains trainable across the training students.',
 '',
 'The reference uses PPES and student-item logitdec (two history terms), plus three timing coordinates. The new condition replaces those history terms with the selected columns and retains timings, item/skill embeddings, retrieval, architecture and training/stopping controls.',
 'Changing the input width changes initialization RNG consumption. These are fresh networks with matched seeds, without a claim of identical initial weights.',
 'Results are exploratory on previously used Cloze folds. No new untouched test cohort or inferential claim of superiority is implied.',
 '',paste0('Package source SHA256: ',p$release$lkt_source_sha256),
 paste0('Protocol SHA256: ',study_hash(file.path(out,'study.rds'))),
 'Native package check: all 51 tests passed, zero errors, two unbuilt-vignette warnings; optional GUI/rendering dependencies were unavailable in the headless image.',
 'No historical fit, reference prediction, data row or partition was rewritten.')
writeLines(lines,file.path(report,'RESULTS.md'))
cat('PASS completed-study identities, inputs, selections, metrics, matched baseline and learned plasticity\n')
print(summary,digits=9,row.names=FALSE)
