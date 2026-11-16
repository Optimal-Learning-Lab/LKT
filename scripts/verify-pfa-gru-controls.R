# ROOT FROZEN_BASELINE CANDIDATE_LIBRARY CANDIDATE_JSON OUTPUT
# Read-only executable compatibility audit; creates no fits and never edits controls.
a<-commandArgs(TRUE);stopifnot(length(a)==5L)
root<-normalizePath(a[1],winslash='/',mustWork=TRUE)
base<-normalizePath(a[2],winslash='/',mustWork=TRUE)
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
source(file.path(root,'scripts/gru-encoder-study.R'))
release<-study_require_release(a[4],a[3])
out<-a[5];stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
audit<-list(complete=FALSE,research_fits=0L,baseline_root=base,candidate=release)
json_path<-file.path(out,'verification.json')
jsonlite::write_json(audit,json_path,auto_unbox=TRUE,pretty=TRUE)
tryCatch({
  protocol<-readRDS(file.path(base,'study.rds'))
  data<-readRDS(file.path(base,'data/prepared_data.rds'))
  stopifnot(protocol$total_fits==15L,nrow(protocol$tasks)==15L,
    all(protocol$tasks$candidate=='constant'),
    study_hash(file.path(base,'data/prepared_data.rds'))==protocol$data_sha256)
  records<-list()
  f<-function(n)getFromNamespace(n,'LKT')
  for(i in seq_len(15L)) {
    task<-protocol$tasks[i,,drop=FALSE]
    path<-file.path(base,'models',paste0(task$run_id,'.rds'))
    saved<-readRDS(path);fit<-saved$fit
    roles<-readRDS(file.path(base,'partitions',paste0(task$partition,'.rds')))
    stopifnot(study_hash(file.path(base,'partitions',paste0(task$partition,'.rds')))==protocol$partition_hashes[[task$partition]],
      identical(fit$fit_rows,which(roles==1L)),identical(fit$validation_rows,which(roles==2L)),
      identical(fit$evaluation_rows,which(roles==3L)),fit$parameter_count==43042L,
      fit$selected_epoch==fit$training_history$epoch[which.min(fit$training_history$validation_loss)])
    expected<-protocol$settings$constant;expected$seed<-as.integer(task$seed)
    stopifnot(identical(saved$settings,expected),identical(fit$gru_settings,expected))
    resolved<-do.call(GRUModel,expected)$settings
    stopifnot(is.null(resolved$semantic_input),
      identical(expected,resolved[names(expected)]),
      resolved$plasticity=='constant',resolved$hidden_size==32L,resolved$num_layers==3L)
    d<-as.data.frame(data);d$fold<-roles
    runtime<-LKTGRURuntime(fit);s<-runtime$settings
    cleanup<-f('lkt_gru_runtime')(s)
    ord<-f('lkt_gru_order')(d,s);ordered<-d[ord,,drop=FALSE]
    encoded<-f('lkt_gru_encode')(ordered,fit$model$preprocessing,fit$model$recipe,s)
    pred<-f('lkt_gru_replay')(runtime$network,encoded,ordered,s)[order(ord)]
    cleanup()
    difference<-max(abs(pred-fit$prediction))
    stopifnot(all(is.finite(pred)),is.finite(difference),difference<3e-6)
    metrics<-unlist(lapply(1:3,function(k) {
      y<-d$CF..ansbin.[roles==k];q<-pred[roles==k]
      value<-study_metrics(y,q)[c('AUC','log_loss')]
      stopifnot(abs(value[1]-as.numeric(pROC::auc(y,q,quiet=TRUE,direction='<')))<1e-12)
      value
    }),use.names=FALSE)
    names(metrics)<-c('training_AUC','training_log_loss','validation_AUC','validation_log_loss','heldout_AUC','heldout_log_loss')
    records[[i]]<-cbind(task,as.data.frame(as.list(metrics)),maximum_probability_difference=difference,
      stored_parameters=fit$parameter_count,selected_epoch=fit$selected_epoch)
    write.csv(data.frame(partition=task$partition,seed=task$seed,source_row=d$comparison_source_row,
      role=roles,observed=d$CF..ansbin.,prediction=pred),
      file.path(out,paste0(task$run_id,'-all-role-predictions.csv')),row.names=FALSE)
    cat('PASS saved-control replay',task$run_id,'maximum difference',format(difference,digits=10),'\n');flush.console()
    rm(runtime,encoded,fit,saved,pred);gc(verbose=FALSE)
  }
  write.csv(do.call(rbind,records),file.path(out,'replayed-fits.csv'),row.names=FALSE)
  audit$complete<-TRUE;audit$models_replayed<-15L
  audit$maximum_probability_difference<-max(vapply(records,function(x)x$maximum_probability_difference,numeric(1)))
  audit$protocol_sha256<-study_hash(file.path(base,'study.rds'))
  audit$settings_and_partition_compatibility<-TRUE
  audit$training_runtime_equivalence<-'Not established by prediction replay; assess runtime/build provenance separately before reusing historical controls.'
  jsonlite::write_json(audit,json_path,auto_unbox=TRUE,pretty=TRUE,digits=17)
},error=function(e) {
  audit$failure<-conditionMessage(e)
  jsonlite::write_json(audit,json_path,auto_unbox=TRUE,pretty=TRUE)
  stop(e)
})
