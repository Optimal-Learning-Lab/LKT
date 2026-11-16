library(LKT)
if(!requireNamespace('torch',quietly=TRUE)||!torch::torch_is_installed()) {
  if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required')
} else {
  source('fixtures/gru-minimal-settings.R')
  close <- function(a,b)stopifnot(max(abs(a-b))<3e-6)
  fail <- function(x)stopifnot(inherits(tryCatch({force(x);NULL},error=identity),'error'))
  d <- data.frame(Anon.Student.Id=rep(c('a','b'),each=8L),
    KC..Default.=rep(c('i','j','i','j','i','j','i','j'),2L),
    KC..Cluster.=rep(c('s','t'),8L),CF..Time.=rep(seq(0,420,60),2L),
    Duration..sec.=5,CF..ansbin.=rep(c(1,0,0,1),4L))
  timed <- LKTGRUTimeFeatures(d)
  for(adaptive in c(FALSE,TRUE))for(retrieval in c(FALSE,TRUE)) {
    fit <- LKT(timed,features=c('logitdec','ppes','logitdec'),
      components=c('Anon.Student.Id','KC..Default.','KC..Default.'),
      fixedpars=c(.7,.3,.4,.02,.7,.8),interc=FALSE,verbose=FALSE,
      model=test_gru_model(epochs=1L,hidden_size=4L,num_layers=3L,head_hidden_sizes=128L,
        categorical_cols=c('KC..Default.','KC..Cluster.'),embedding_dims=2L,
        use_lkt_features=TRUE,time_col='CF..Time.',order_col='CF..Time.',
        time_feature_cols=c('gru_item_age_seconds','gru_mean_prior_spacing_seconds'),
        time_missing_cols='gru_mean_prior_spacing_seconds',response_encoding='binary',
        plasticity='constant',plasticity_time_col='CF..Time.',
        feature_encoder='mlp',numeric_encoder='combined',encoder_output_sizes=c(numeric=4L),
        retrieval_heads=if(retrieval)1L else 0L,retrieval_size=4L,retrieval_dropout=0,
        retrieval_filter_cols=if(retrieval)c('KC..Default.','KC..Cluster.') else character(),
        retrieval_value_mode=if(adaptive&&retrieval)'raw' else 'projected',
        embedding_adaptation=if(adaptive)'per_student' else 'none',
        adaptive_embedding_cols=c('KC..Default.','KC..Cluster.'),
        validation_fraction=0,early_stopping=FALSE,seed=71L))
    # Ensure a nonzero local output adjustment even on a tiny training fixture.
    fit$model$weights[['plasticity.eta']][] <- .03
    run <- LKTGRURuntime(fit);initial <- LKTGRUState(fit)
    weights <- serialize(fit$model$weights,NULL)
    runtime_weights <- function()serialize(lapply(run$network$state_dict(),function(x)torch::as_array(x$detach()$cpu())),NULL)
    rw <- runtime_weights()
    for(n in c(0L,1L,6L)) {
      history <- timed[seq_len(n),,drop=FALSE]
      state <- if(n)LKTGRUStep(fit,initial,history,run)$state else initial
      rows <- timed[rep(7L,5L),,drop=FALSE];rows$KC..Default. <- c('i','j','i','j','i')
      rows$KC..Cluster. <- c('s','t','s','t','s');rows$CF..Time. <- c(420,430,450,420,420)
      if(adaptive){rows$KC..Default.[4L]<-'unseen-item';rows$KC..Cluster.[4L]<-'unseen-skill'}
      rows <- LKTGRUCandidateTimeFeatures(history,rows[setdiff(names(rows),c('gru_item_age_seconds','gru_mean_prior_spacing_seconds'))])
      bytes <- serialize(state,NULL);set.seed(197);rng <- .Random.seed
      ref <- LKTGRUScore(fit,state,rows,run)
      for(chunk in c(1L,2L,100L))close(ref,LKTGRUScore(fit,state,rows,run,'batched',chunk))
      for(chunk in c(1L,2L,100L)) {
        duplicate <- rows[rep(1L,3L),,drop=FALSE]
        stopifnot(length(unique(LKTGRUScore(fit,state,duplicate,run,'batched',chunk)))==1L)
      }
      if(adaptive&&n==0L)stopifnot(length(unique(ref))==1L,
        length(unique(LKTGRUScore(fit,state,rows,run,'batched')))==1L)
      close(ref,rev(LKTGRUScore(fit,state,rows[5:1,],run,'batched')))
      another <- rows;another$Anon.Student.Id <- 'b'
      mixed <- rbind(rows,another)
      close(c(ref,LKTGRUScore(fit,state,another,run)),LKTGRUScore(fit,state,mixed,run,'batched',2L))
      ignored <- rows;ignored$CF..ansbin. <- NA_real_
      close(ref,LKTGRUScore(fit,state,ignored,run,'batched'))
      stopifnot(identical(bytes,serialize(state,NULL)),identical(rng,.Random.seed),
        identical(weights,serialize(fit$model$weights,NULL)),identical(rw,runtime_weights()))
      close(ref[1L],LKTGRUStep(fit,state,rows[1L,,drop=FALSE],run)$predictions)
      path <- tempfile();saveRDS(state,path);restored <- readRDS(path);unlink(path)
      close(ref,LKTGRUScore(fit,restored,rows,run,'batched'))
      for(i in seq_len(nrow(rows))) {
        raw <- rows[i,setdiff(names(rows),c('gru_item_age_seconds','gru_mean_prior_spacing_seconds')),drop=FALSE]
        expected <- tail(LKTGRUTimeFeatures(rbind(history[names(raw)],raw)),1L)
        close(rows$gru_item_age_seconds[i],expected$gru_item_age_seconds)
        stopifnot(isTRUE(all.equal(rows$gru_mean_prior_spacing_seconds[i],expected$gru_mean_prior_spacing_seconds)))
      }
      if(n) {
        broken <- rows;broken$CF..Time. <- -1
        fail(LKTGRUScore(fit,state,broken,run,'batched'))
      }
      fail(LKTGRUScore(fit,state,rows,run,'batched',0L))
      pending <- LKTGRUStep(fit,state,ignored[1L,,drop=FALSE],run)$state
      close(ref[1L],LKTGRUScore(fit,pending,ignored[1L,,drop=FALSE],run,'batched'))
      other <- ignored[1L,,drop=FALSE];other$CF..Time. <- other$CF..Time.+1
      fail(LKTGRUScore(fit,pending,other,run,'batched'))
    }
    cat('PASS batched independent candidates adaptive=',adaptive,' retrieval=',retrieval,'\n')
  }
  # The compact historical network has its own module constructor; exercise its
  # prediction-only method too, without enabling feature encoders or retrieval.
  raw <- LKT(d,features='lineafm',components='KC..Default.',verbose=FALSE,
    model=test_gru_model(epochs=1L,hidden_size=4L,embedding_dims=2L,
      validation_fraction=0,early_stopping=FALSE,seed=17L))
  state <- LKTGRUStep(raw,LKTGRUState(raw),d[1:4,])$state
  close(LKTGRUScore(raw,state,d[5:8,]),LKTGRUScore(raw,state,d[5:8,],method='batched',candidate_batch_size=2L))
  historical <- raw;historical$model$settings$embedding_adaptation <- NULL
  historical$gru_settings$embedding_adaptation <- NULL
  close(LKTGRUScore(raw,state,d[5:8,]),LKTGRUScore(historical,state,d[5:8,],method='batched'))
  cat('PASS compact historical GRU and saved settings lacking adaptation fields\n')
}
