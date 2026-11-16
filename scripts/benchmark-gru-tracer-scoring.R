# ROOT RESULTS OUTPUT. Loaded-runtime latency and score/observe semantics; no fits.
a<-commandArgs(TRUE);stopifnot(length(a)==3L)
root<-normalizePath(a[1],winslash='/');results<-a[2];out<-a[3]
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-encoder-study.R'))
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
saved<-readRDS(file.path(results,'models/cv1__anonymous_constant__seed1.rds'));fit<-saved$fit
d<-readRDS(file.path(results,'data/prepared_data.rds'))
stopifnot(fit$gru_settings$embedding_adaptation=='per_student',fit$gru_settings$plasticity=='constant')
runtime<-LKTGRURuntime(fit);weights<-serialize(fit$model$weights,NULL)
runtime_weights<-function()serialize(lapply(runtime$network$state_dict(),function(t)torch::as_array(t$detach()$cpu())),NULL)
before_runtime<-runtime_weights()
groups<-split(which(d$fold==3L),d$Anon.Student.Id[d$fold==3L]);idx<-groups[[which(lengths(groups)>=101L)[1L]]]
student<-d[idx,,drop=FALSE];items<-sort(unique(as.character(d$KC..Default.)))
stopifnot(length(items)==144L)
clock<-function(fun) {
 t<-proc.time()[['elapsed']];value<-fun();list(value=value,seconds=proc.time()[['elapsed']]-t)
}
records<-list();checks<-list();j<-0L
for(n in c(0L,25L,100L)) {
 prefix<-student[seq_len(n),,drop=FALSE]
 state<-LKTGRUState(fit)
 if(n)state<-LKTGRUStep(fit,state,prefix,runtime)$state
 state_bytes<-serialize(state,NULL)
 prepare<-function(size) {
  candidates<-student[rep(n+1L,size),,drop=FALSE]
  candidates$KC..Default.<-items[seq_len(size)];candidates$CF..ansbin.<-NA_real_
  candidates$Duration..sec.<-0;candidates$comparison_source_row<-NA_integer_
  timing_cols<-c('gru_item_age_seconds','gru_mean_prior_spacing_seconds')
  raw<-rbind(prefix[setdiff(names(prefix),timing_cols)],candidates[setdiff(names(candidates),timing_cols)])
  prepared<-LKTGRUTimeFeatures(raw)
  tail(prepared,size)
 }
 # These timing fixtures use scored rows as exposure history, not a replay/AUC
 # assertion against the original complete-exposure timing preprocessing.
 warm<-prepare(1L);LKTGRUScore(fit,state,warm,runtime)
 for(size in c(1L,110L,144L)) {
  for(repetition in 1:3) {
   prep<-clock(function()prepare(size));candidates<-prep$value
   scored<-clock(function()LKTGRUScore(fit,state,candidates,runtime))
   stopifnot(identical(state_bytes,serialize(state,NULL)),all(is.finite(scored$value)))
   j<-j+1L;records[[j]]<-data.frame(history_responses=n,candidates=size,repetition=repetition,
    preparation_seconds=prep$seconds,score_seconds=scored$seconds,
    total_seconds=prep$seconds+scored$seconds,seconds_per_candidate=scored$seconds/size)
  }
  expected<-scored$value
  reverse<-LKTGRUScore(fit,state,candidates[nrow(candidates):1L,,drop=FALSE],runtime)
  answered<-candidates;answered$CF..ansbin.<-1L
  ignored<-LKTGRUScore(fit,state,answered,runtime)
  stopifnot(max(abs(rev(reverse)-expected))<3e-6,max(abs(ignored-expected))<3e-6,
   identical(state_bytes,serialize(state,NULL)))
  selected<-which.max(expected);trial<-answered[selected,,drop=FALSE]
  update<-clock(function()LKTGRUStep(fit,state,trial,runtime))
  stopifnot(abs(update$value$predictions-expected[selected])<3e-6,
   !identical(serialize(update$value$state,NULL),state_bytes),
   identical(weights,serialize(fit$model$weights,NULL)),identical(before_runtime,runtime_weights()))
  id<-as.character(trial$KC..Default.);subject<-as.character(trial$Anon.Student.Id)
  old_items<-state$adaptive_embeddings$students[[subject]][['KC..Default.']]
  new_items<-update$value$state$adaptive_embeddings$students[[subject]][['KC..Default.']]
  for(other in setdiff(union(names(old_items),names(new_items)),id))stopifnot(identical(old_items[[other]],new_items[[other]]))
  checks[[paste(n,size)]]<-data.frame(history_responses=n,candidates=size,
   selected_item=id,observe_seconds=update$seconds,score_did_not_mutate_state=TRUE,
   candidate_order_invariant=TRUE,supplied_answers_ignored=TRUE,only_selected_item_updated=TRUE,
   shared_fitted_and_runtime_weights_unchanged=TRUE)
  cat('PASS history',n,'candidates',size,'\n');flush.console()
 }
}
timings<-do.call(rbind,records);verification<-do.call(rbind,checks)
write.csv(timings,file.path(out,'timings.csv'),row.names=FALSE)
write.csv(verification,file.path(out,'verification.csv'),row.names=FALSE)
summary<-aggregate(cbind(preparation_seconds,score_seconds,total_seconds,seconds_per_candidate)~history_responses+candidates,timings,median)
write.csv(summary,file.path(out,'median-latency.csv'),row.names=FALSE)
jsonlite::write_json(list(complete=TRUE,R=R.version.string,node=Sys.info()[['nodename']],
 release=as.list(lkt_release),threads=fit$gru_settings$num_threads,loaded_runtime=TRUE,
 repetitions=3L,scoring_api='LKTGRUScore',startup_excluded=TRUE,
 implementation='Current public API evaluates candidate rows one at a time; no batched neural scoring optimization.',
 fixture='One held-out student; scored-response prefix supplies benchmark exposure history. Not an AUC re-evaluation.',
 script_sha256=study_hash(file.path(root,'scripts/benchmark-gru-tracer-scoring.R'))),
 file.path(out,'completion.json'),auto_unbox=TRUE,pretty=TRUE)
print(summary)
