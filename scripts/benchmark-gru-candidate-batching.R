# ROOT RESULTS OUTPUT capture|verify BASELINE_DIRECTORY
# Saved-model conformance and CPU latency only. No training or scientific refit.
a <- commandArgs(TRUE);stopifnot(length(a)%in%c(5L,6L),a[4]%in%c('capture','verify'))
root <- a[1];results <- a[2];out <- a[3];mode <- a[4];baseline <- a[5]
source(file.path(root,'scripts/load-current-lkt.R'))
stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
sha <- function(p)digest::digest(file=p,algo='sha256')
model_name <- if(length(a)==6L)a[6] else 'cv1__anonymous_constant__seed1.rds'
model_path <- file.path(results,'models',model_name)
saved <- readRDS(model_path);fit <- saved$fit
d <- readRDS(file.path(results,'data/prepared_data.rds'))
run <- LKTGRURuntime(fit);initial <- LKTGRUState(fit)
clock <- function(f) {t<-proc.time()[['elapsed']];value<-f();list(value=value,seconds=proc.time()[['elapsed']]-t)}
close <- function(a,b)stopifnot(length(a)==length(b),max(abs(a-b))<3e-6)
timing_cols <- c('gru_item_age_seconds','gru_mean_prior_spacing_seconds')
if(mode=='capture') {
  groups <- split(which(d$fold==3L),d$Anon.Student.Id[d$fold==3L])
  student <- d[groups[[which.max(lengths(groups))]],,drop=FALSE]
  bank <- unique(d[c('KC..Default.','KC..Cluster.')]);bank <- bank[order(bank$KC..Default.),]
  stopifnot(nrow(bank)==144L,!anyDuplicated(bank$KC..Default.))
  probes <- list();j<-0L
  for(n in unique(c(0L,25L,100L,min(500L,nrow(student)-1L)))) {
    history <- student[seq_len(n),,drop=FALSE]
    state <- if(n)LKTGRUStep(fit,initial,history,run)$state else initial
    for(size in c(1L,10L,110L,144L)) {
      rows <- student[rep(n+1L,size),,drop=FALSE]
      rows$KC..Default. <- bank$KC..Default.[seq_len(size)]
      rows$KC..Cluster. <- bank$KC..Cluster.[seq_len(size)]
      rows$CF..ansbin. <- NA_real_;rows$Duration..sec. <- 0
      raw <- rows[setdiff(names(rows),timing_cols)]
      prep <- clock(function()do.call(rbind,lapply(seq_len(size),function(i)
        tail(LKTGRUTimeFeatures(rbind(history[names(raw)],raw[i,,drop=FALSE])),1L))))
      gold <- clock(function()LKTGRUScore(fit,state,prep$value,run))
      j<-j+1L;probes[[j]]<-list(history=history,state=state,raw=raw,rows=prep$value,
        gold=gold$value,reference_seconds=gold$seconds,reference_preparation_seconds=prep$seconds)
      cat('CAPTURE',n,size,gold$seconds,'\n');flush.console()
    }
  }
  saveRDS(list(model_sha256=sha(model_path),release=as.list(lkt_release),probes=probes),file.path(out,'baseline.rds'))
} else {
  reference <- readRDS(file.path(baseline,'baseline.rds'));stopifnot(sha(model_path)==reference$model_sha256)
  weights <- serialize(fit$model$weights,NULL)
  runtime_weights <- function()serialize(lapply(run$network$state_dict(),function(x)torch::as_array(x$detach()$cpu())),NULL)
  rw <- runtime_weights();records<-list();checks<-list();j<-0L
  for(probe in reference$probes) {
    state <- probe$state;bytes <- serialize(state,NULL)
    # The new build's original scorer/continuation must retain old-build behavior.
    close(probe$gold,LKTGRUScore(fit,state,probe$rows,run))
    size <- nrow(probe$rows)
    prep <- clock(function()LKTGRUCandidateTimeFeatures(probe$history,probe$raw))
    stopifnot(isTRUE(all.equal(prep$value[timing_cols],probe$rows[timing_cols],check.attributes=FALSE)))
    set.seed(71L);rng <- .Random.seed
    for(chunk in unique(c(1L,32L,256L)))close(probe$gold,LKTGRUScore(fit,state,prep$value,run,'batched',chunk))
    close(probe$gold,rev(LKTGRUScore(fit,state,prep$value[size:1L,,drop=FALSE],run,'batched')))
    answered <- prep$value;answered$CF..ansbin. <- 1L
    close(probe$gold,LKTGRUScore(fit,state,answered,run,'batched'))
    if(size>1L) {
      duplicate <- prep$value[rep(1L,3L),,drop=FALSE]
      values <- LKTGRUScore(fit,state,duplicate,run,'batched');stopifnot(length(unique(values))==1L)
    }
    chosen <- which.max(probe$gold)
    old <- LKTGRUStep(fit,state,answered[chosen,,drop=FALSE],run)
    close(old$predictions,probe$gold[chosen])
    # New runtime from serialized fit/state agrees; no transient cache is required.
    restored <- unserialize(serialize(state,NULL))
    close(probe$gold,LKTGRUScore(fit,restored,prep$value,LKTGRURuntime(fit),'batched'))
    stopifnot(identical(bytes,serialize(state,NULL)),identical(rng,.Random.seed),
      identical(weights,serialize(fit$model$weights,NULL)),identical(rw,runtime_weights()))
    if(nrow(probe$history)) {
      replay <- LKTGRUStep(fit,initial,probe$history,run)
      stopifnot(identical(replay$state,probe$state))
    }
    for(repetition in 1:3)for(method in c('reference','batched')) {
      score <- clock(function()LKTGRUScore(fit,state,prep$value,run,method))
      close(probe$gold,score$value)
      j<-j+1L;records[[j]]<-data.frame(history=nrow(probe$history),candidates=size,
        method=method,repetition=repetition,seconds=score$seconds,
        maximum_difference=max(abs(probe$gold-score$value)))
    }
    checks[[length(checks)+1L]]<-data.frame(history=nrow(probe$history),candidates=size,
      preparation_seconds=prep$seconds,old_preparation_seconds=probe$reference_preparation_seconds,
      old_build_predictions_unchanged=TRUE,state_and_rng_unchanged=TRUE,chunking_and_reordering=TRUE,
      serialization_and_runtime_recreation=TRUE,answered_selected_prediction=TRUE)
    cat('PASS',nrow(probe$history),size,'\n');flush.console()
  }
  # Consistent anonymous relabeling retains both item and skill matching.
  if(identical(fit$gru_settings$embedding_adaptation,'per_student')) {
    probe <- tail(reference$probes,1L)[[1L]];raw <- probe$rows;history <- probe$history
    for(col in c('Anon.Student.Id','KC..Default.','KC..Cluster.')) {
      raw[[col]]<-paste0('unseen-',raw[[col]]);history[[col]]<-paste0('unseen-',history[[col]])
    }
    renamed <- LKTGRUStep(fit,initial,history,run)$state
    close(probe$gold,LKTGRUScore(fit,renamed,raw,run,'batched'))
  }
  timings <- do.call(rbind,records);verification <- do.call(rbind,checks)
  medians <- aggregate(seconds~history+candidates+method,timings,median)
  comparison <- merge(subset(medians,method=='reference'),subset(medians,method=='batched'),
    by=c('history','candidates'),suffixes=c('_reference','_batched'))
  comparison$speedup <- comparison$seconds_reference/pmax(comparison$seconds_batched,.001)
  write.csv(timings,file.path(out,'timings.csv'),row.names=FALSE)
  write.csv(comparison,file.path(out,'comparison.csv'),row.names=FALSE)
  write.csv(verification,file.path(out,'verification.csv'),row.names=FALSE)
  jsonlite::write_json(list(passed=TRUE,model_sha256=sha(model_path),release=as.list(lkt_release),
    baseline_release=reference$release,maximum_prediction_difference=max(timings$maximum_difference),
    mode='Saved-model scoring conformance and latency, not a new AUC evaluation',
    model_file=model_name,node=Sys.info()[['nodename']],
    process_memory=readLines('/proc/self/status')[grepl('^Vm(HWM|RSS):',readLines('/proc/self/status'))],
    threads=fit$gru_settings$num_threads,script_sha256=sha(file.path(root,'scripts/benchmark-gru-candidate-batching.R'))),
    file.path(out,'completed.json'),auto_unbox=TRUE,pretty=TRUE)
  print(comparison)
}
