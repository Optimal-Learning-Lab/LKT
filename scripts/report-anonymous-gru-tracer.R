# SOURCE_ROOT RESULTS SIX_INPUT_REFERENCE OUTPUT. Completed-artifact audit only.
a<-commandArgs(TRUE);stopifnot(length(a)==4L)
root<-normalizePath(a[1],winslash='/');out<-normalizePath(a[2],winslash='/')
reference<-normalizePath(a[3],winslash='/');report<-a[4]
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
source(file.path(root,'scripts/gru-encoder-study.R'))
dir.create(report,recursive=TRUE,showWarnings=FALSE)
p<-readRDS(file.path(out,'study.rds'));c<-jsonlite::fromJSON(file.path(out,'completed.json'))
audit<-jsonlite::fromJSON(file.path(out,'reports/independent-audit.json'))
runtime<-readRDS(file.path(out,'reports/runtime-checks.rds'))
model_file<-file.path(out,'models/cv1__anonymous_constant__seed1.rds')
stopifnot(c$complete,c$fits==1L,c$protocol_sha256==study_hash(file.path(out,'study.rds')),
 c$model_sha256==study_hash(model_file),c$metrics_sha256==study_hash(file.path(out,'reports/metrics.csv')),
 c$audit_sha256==study_hash(file.path(out,'reports/independent-audit.json')),audit$complete,
 audit$phase_auc_comparisons==3L,all(runtime$maximum_difference<3e-6),
 runtime$shared_weights_unchanged,runtime$fresh_item_labels_have_equal_initial_predictions,
 p$reference_sha256==study_hash(file.path(reference,'study.rds')),
 p$source_data_sha256==study_hash(file.path(reference,'data/prepared_data.rds')),
 p$partition_sha256==study_hash(file.path(reference,'partitions/cv1.rds')))
saved<-readRDS(model_file);fit<-saved$fit;s<-fit$gru_settings
skills<-isTRUE(p$supplied_skills)
expected_cols<-if(skills)c('KC..Default.','KC..Cluster.') else 'KC..Default.'
stopifnot(s$embedding_adaptation=='per_student',s$plasticity=='constant',
 identical(s$categorical_cols,expected_cols),identical(s$adaptive_embedding_cols,expected_cols),
 length(fit$model$preprocessing$numeric_names)==6L,
 all(vapply(fit$model$weights[grep('^embeddings[.]',names(fit$model$weights))],function(v)all(v==0),logical(1))))
new<-read.csv(file.path(out,'reports/metrics.csv'))
old<-read.csv(file.path(reference,'reports/fits.csv'))
old<-old[old$partition=='cv1' & old$candidate=='constant' & old$seed==1L,,drop=FALSE]
stopifnot(nrow(old)==1L,nrow(new)==3L)
new$baseline_AUC<-unlist(old[c('training_AUC','validation_AUC','test_AUC')],use.names=FALSE)
new$baseline_log_loss<-unlist(old[c('training_log_loss','validation_log_loss','test_log_loss')],use.names=FALSE)
new$delta_AUC<-new$AUC-new$baseline_AUC
new$delta_log_loss<-new$log_loss-new$baseline_log_loss
study_write_csv(new,file.path(report,'matched-comparison.csv'))
eta<-as.numeric(fit$model$weights[['plasticity.eta']])
lines<-c(if(skills)'# Initial student-local item-and-skill GRU tracer result' else '# Initial anonymous-item GRU tracer result','',
 'One original Cloze whole-student fold (cv1), seed 1. This is an initial test, not complete cross-validation.',
 '', '| Phase | Students | Responses | Anonymous tracer AUC | Log loss | Identity-informed reference AUC |',
 '| --- | ---: | ---: | ---: | ---: | ---: |',
 vapply(seq_len(nrow(new)),function(i)sprintf('| %s | %d | %d | %.6f | %.6f | %.6f |',
   new$phase[i],new$students[i],new$rows[i],new$AUC[i],new$log_loss[i],new$baseline_AUC[i]),character(1)),
 '',sprintf('Held-out AUC change against the matching fold/seed reference: %+.6f.',new$delta_AUC[new$phase=='heldout']),
 sprintf('Selected epoch: %d; trained epochs: %d; learned constant output rate: %.9f.',fit$selected_epoch,fit$trained_epochs,eta),
 '',
 if(skills)'The original six numeric values, item/skill channels, item-or-skill retrieval matching, recurrent architecture and training/stopping controls are retained. No Lasso inputs are used. Skill labels give group membership, not pretrained weights.' else
  'The original six numeric values, recurrent architecture and training/stopping controls are retained. No Lasso inputs or supplied skill labels are used.',
 p$reset_rule,
 'Shared encoders, recurrent/retrieval/prediction weights and the constant output rate learn across training students and are frozen during evaluation. Student IDs route histories, not pretrained student embeddings.',
 if(skills)'The identity-informed reference has pretrained item/skill embeddings. This variant preserves the original input channels and dimensions, but resets and locally learns both identity vector tables for each student.' else
  'The identity-informed reference has pretrained item/skill embeddings and skill-based retrieval matching. Removing those inputs changes the information available and network dimensions; the difference does not isolate embedding resets alone.',
 'Training, validation and held-out AUC are similar in this first test. The result does not establish which remaining architecture or local learning-rate choice would work best.',
 '',sprintf('Saved-prediction, identity-renaming and chunking checks: maximum probability difference %.12g.',max(runtime$maximum_difference)),
 'The independent Python audit verified all three phase AUC/log-loss values and whole-student role separation. Fresh item labels with identical observable histories produce equal initial predictions.',
 'All 51 native package tests passed; R CMD check had zero errors and two unbuilt-vignette warnings in the documented headless runtime.',
 'Simulator configurations and default model settings were not changed.',
 '',paste('Protocol SHA256:',c$protocol_sha256),paste('Fitted model SHA256:',c$model_sha256),
 paste('Source archive SHA256:',p$release$lkt_source_sha256))
writeLines(lines,file.path(report,'RESULTS.md'))
cat('PASS completed-model hashes, runtime/identity checks, phase metrics and matched baseline\n');print(new)
cat('EPOCH',fit$selected_epoch,'TRAINED',fit$trained_epochs,'ETA',eta,'\n')
print(runtime$maximum_difference)
