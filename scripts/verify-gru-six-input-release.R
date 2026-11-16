# Verify six-input promotion against immutable saved Cloze models.
# ROOT CANDIDATE_LIBRARY NUMERIC_ABLATION_RESULTS OUTPUT LEGACY_CHECKPOINT
args<-commandArgs(TRUE);stopifnot(length(args)==5L)
root<-normalizePath(args[1],winslash='/',mustWork=TRUE)
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
library(LKT,lib.loc=normalizePath(args[2],winslash='/',mustWork=TRUE))
source(file.path(root,'scripts/gru-encoder-study.R'))
study<-args[3];output<-args[4];stopifnot(!dir.exists(output));dir.create(output,recursive=TRUE)
defaults<-GRUModel()$settings
stopifnot(is.null(defaults$time_col),is.null(defaults$numeric_feature_cols),defaults$order_col=='CF..Time.',
 defaults$numeric_encoder=='combined',defaults$response_encoding=='binary',defaults$retrieval_value_mode=='raw')
p<-readRDS(file.path(study,'study.rds'));d<-readRDS(file.path(study,'data/prepared_data.rds'))
stopifnot(study_hash(file.path(study,'data/prepared_data.rds'))==p$data_sha256)
records<-list()
for(part in p$partitions){
 fold_file<-file.path(study,'partitions',paste0(part,'.rds'));stopifnot(study_hash(fold_file)==p$partition_hashes[[part]])
 fold<-readRDS(fold_file);d$fold<-fold;test<-d[fold==3,,drop=FALSE]
 for(seed in p$seeds){
  id<-paste(part,'omit1',paste0('seed',seed),sep='__');path<-file.path(study,'models',paste0(id,'.rds'));before<-study_hash(path)
  f<-readRDS(path)$fit;s<-f$gru_settings;s['time_col']<-list(NULL);s['numeric_feature_cols']<-list(NULL)
  # The frozen study explicitly rejected unknown categories; the constructor
  # already used zero encoding before this promotion. All reference categories
  # are known, so verify the actual constructor policy as well as its inputs.
  s$unknown_levels<-defaults$unknown_levels;s<-do.call(GRUModel,s)$settings
  varied<-c('seed','validation_folds')
  stopifnot(identical(s[setdiff(names(s),varied)],defaults[setdiff(names(defaults),varied)]))
  prep<-getFromNamespace('lkt_gru_preprocess','LKT')(d,fold==1,f$model$recipe,s)
  previous_prep<-getFromNamespace('lkt_gru_preprocess','LKT')(d,fold==1,f$model$recipe,f$gru_settings)
  stopifnot(identical(prep,previous_prep),length(prep$numeric_names)==6L,!any(grepl('gap_seconds',prep$numeric_names,fixed=TRUE)),
   identical(prep[setdiff(names(prep),c('center','scale'))],f$model$preprocessing[setdiff(names(prep),c('center','scale'))]))
  # Windows and Linux scaling arithmetic can differ by a few float64 ulps.
  # Establish bit-exact old/new construction on this host, then retain the
  # original learned scaling when comparing predictions from trained weights.
  for(n in c('center','scale'))stopifnot(all(abs(prep[[n]]-f$model$preprocessing[[n]]) <=
   16*.Machine$double.eps*pmax(1,abs(prep[[n]]),abs(f$model$preprocessing[[n]]))))
  roundoff<-max(abs(c(prep$center-f$model$preprocessing$center,prep$scale-f$model$preprocessing$scale)))
  net<-getFromNamespace('lkt_gru_network','LKT')(prep,s)
  stopifnot(sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))==43073L)
  original<-study_replay(f,test)
  # A transient verification view reuses arrays only; no fitted object is saved
  # or edited on disk. Its numeric schema and weights are exactly the same.
  view<-f;view$model$settings<-s;view$gru_settings<-s;view$model$preprocessing<-f$model$preprocessing
  q<-study_replay(view,test)
  stopifnot(identical(q,original),max(abs(q-f$prediction[fold==3]))<3e-6,before==study_hash(path))
  records[[id]]<-data.frame(run_id=id,kind='reviewed six-input equivalence',maximum_probability_difference=max(abs(q-f$prediction[fold==3])),new_default_view_difference=max(abs(q-original)),saved_scaling_roundoff=roundoff)
 }
 for(condition in c('all',if(part=='cv1')c('none','only1'))){
  id<-paste(part,condition,'seed1',sep='__');path<-file.path(study,'models',paste0(id,'.rds'));before<-study_hash(path)
  f<-readRDS(path)$fit;q<-study_replay(f,test)
  stopifnot(max(abs(q-f$prediction[fold==3]))<3e-6,before==study_hash(path),identical(f$gru_settings$time_col,'CF..Time.'))
  records[[id]]<-data.frame(run_id=id,kind='preserved prior settings',maximum_probability_difference=max(abs(q-f$prediction[fold==3])),new_default_view_difference=NA_real_,saved_scaling_roundoff=NA_real_)
 }
}
path<-args[5];before<-study_hash(path);f<-readRDS(path)$fit
fold<-readRDS(file.path(study,'partitions/cv1.rds'));d$fold<-fold;test<-d[fold==3,,drop=FALSE]
q<-study_replay(f,test);stopifnot(max(abs(q-f$prediction[fold==3]))<3e-6,before==study_hash(path))
records[['legacy']]<-data.frame(run_id='original pre-selector model',kind='preserved original architecture',maximum_probability_difference=max(abs(q-f$prediction[fold==3])),new_default_view_difference=NA_real_,saved_scaling_roundoff=NA_real_)
r<-do.call(rbind,records);write.csv(r,file.path(output,'checkpoint-replay.csv'),row.names=FALSE)
jsonlite::write_json(list(real_checkpoints_replayed=nrow(r),six_input_equivalences=15L,
 maximum_probability_difference=max(r$maximum_probability_difference),maximum_new_default_view_difference=0,
 new_vs_previous_preprocessing_bit_identical=TRUE,saved_preprocessing_reused=TRUE,
 maximum_saved_scaling_roundoff=max(r$saved_scaling_roundoff,na.rm=TRUE),parameter_count=43073L,all_saved_files_unchanged=TRUE),
 file.path(output,'verification.json'),auto_unbox=TRUE,pretty=TRUE)
cat('PASS',nrow(r),'real saved models, 15 exact six-input default equivalences, historical settings and unchanged saved files.\n')
