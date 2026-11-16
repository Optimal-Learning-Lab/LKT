library(LKT)
if(!requireNamespace('torch',quietly=TRUE)||!torch::torch_is_installed()) {
  if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required')
} else {
  close <- function(a,b)stopifnot(isTRUE(all.equal(a,b,tolerance=3e-6,check.attributes=FALSE)))
  fail <- function(x)stopifnot(inherits(tryCatch({force(x);NULL},error=identity),'error'))
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:4),each=7),
    KC..Default.=rep(c('a','b','a','b','a','b','a'),4),
    CF..Time.=rep(seq(0,600,100),4),CF..ansbin.=rep(c(0,1,0,1,1,0,1),4))
  d <- d[-c(13L,14L,26L,27L,28L),] # Uneven histories exercise padding and shrinking batches.
  for(retrieval in c(FALSE,TRUE))for(plasticity in c('none','constant')) {
    config <- list(epochs=2L,hidden_size=4L,num_layers=1L,head_hidden_sizes=3L,
      categorical_cols='KC..Default.',embedding_dims=2L,input_dropout=0,gru_dropout=0,
      head_dropout=0,use_lkt_features=FALSE,time_col=NULL,time_feature_cols=character(),
      time_missing_cols=character(),feature_encoder=if(retrieval)'mlp' else 'none',
      numeric_encoder=if(retrieval)'combined' else 'separate',
      encoder_output_sizes=if(retrieval)c(numeric=3L) else integer(),retrieval_heads=if(retrieval)1L else 0L,
      retrieval_size=3L,retrieval_value_mode=if(retrieval)'raw' else 'projected',
      retrieval_filter_cols=if(retrieval)'KC..Default.' else character(),
      embedding_adaptation='per_student',embedding_learning_rate=.01,
      plasticity=plasticity,plasticity_time_col=if(plasticity=='constant')'CF..Time.' else NULL,
      validation_fraction=0,early_stopping=FALSE,batch_size=3L,sequence_length=3L,seed=16L)
    fit <- LKT(d,features='lineafm',components='KC..Default.',verbose=FALSE,model=do.call(GRUModel,config))
    state <- LKTGRUState(fit);runtime <- LKTGRURuntime(fit)
    weights <- serialize(fit$model$weights,NULL)
    rw <- serialize(lapply(runtime$network$state_dict(),function(x)torch::as_array(x$detach()$cpu())),NULL)
    all <- LKTGRUStep(fit,state,d,runtime)
    close(all$predictions,fit$prediction)
    one <- LKTGRUStep(fit,state,d[1,,drop=FALSE],runtime)
    stopifnot(any(one$state$adaptive_embeddings$students$s1[['KC..Default.']]$a!=0),
      is.null(one$state$adaptive_embeddings$students$s1[['KC..Default.']]$b))
    stopifnot(is.null(one$state$adaptive_embeddings$students$s2))
    cand <- d[rep(2,3),];cand$KC..Default. <- c('unseen-a','unseen-b','a')
    before <- serialize(one$state,NULL)
    p <- LKTGRUScore(fit,one$state,cand,runtime)
    close(p,rev(LKTGRUScore(fit,one$state,cand[3:1,],runtime)))
    stopifnot(identical(before,serialize(one$state,NULL)))
    unanswered <- cand[1,,drop=FALSE];unanswered$CF..ansbin. <- NA_real_
    pending <- LKTGRUStep(fit,one$state,unanswered,runtime)
    stopifnot(identical(pending$state$adaptive_embeddings,one$state$adaptive_embeddings))
    answered <- LKTGRUStep(fit,pending$state,cand[1,,drop=FALSE],runtime)
    close(answered$predictions,p[1])
    stopifnot(!is.null(answered$state$adaptive_embeddings$students$s1[['KC..Default.']][['unseen-a']]),
      is.null(answered$state$adaptive_embeddings$students$s1[['KC..Default.']][['unseen-b']]))
    other <- d[8,,drop=FALSE]
    close(LKTGRUStep(fit,one$state,other,runtime)$predictions,LKTGRUStep(fit,state,other,runtime)$predictions)
    continuation <- LKTGRUStep(fit,one$state,d[2:7,],runtime)
    close(c(one$predictions,continuation$predictions),all$predictions[1:7])
    path <- tempfile(fileext='.rds');saveRDS(one$state,path);restored <- readRDS(path);unlink(path)
    close(LKTGRUStep(fit,restored,d[2:7,],runtime)$predictions,continuation$predictions)
    renamed <- d;renamed$KC..Default. <- paste0('new-',renamed$KC..Default.)
    close(LKTGRUStep(fit,state,renamed,runtime)$predictions,all$predictions)
    changed <- d;changed$CF..ansbin.[7] <- 1-changed$CF..ansbin.[7]
    close(LKTGRUStep(fit,state,changed,runtime)$predictions[1:7],all$predictions[1:7])
    stopifnot(identical(weights,serialize(fit$model$weights,NULL)),
      identical(rw,serialize(lapply(runtime$network$state_dict(),function(x)torch::as_array(x$detach()$cpu())),NULL)))
    if(plasticity=='constant') {
      # Exercise both local update mechanisms even if this tiny fixture trains
      # eta to its zero boundary. This transient view is not a fitted artifact.
      probe <- fit;probe$model$weights[['plasticity.eta']][] <- .05
      start <- LKTGRUState(probe);run <- LKTGRURuntime(probe)
      first <- LKTGRUStep(probe,start,d[1,,drop=FALSE],run)
      key <- names(first$state$hidden)[1L]
      stopifnot(!is.null(first$state$inference_context[[key]]$plastic_weights),
        any(first$state$inference_context[[key]]$plastic_weights!=0),
        any(first$state$adaptive_embeddings$students$s1[['KC..Default.']]$a!=0))
      later <- LKTGRUStep(probe,first$state,d[2:7,],run)
      whole <- LKTGRUStep(probe,start,d[1:7,],run)
      close(c(first$predictions,later$predictions),whole$predictions)
      candidates <- d[2:3,];candidates$CF..ansbin. <- NA_real_
      before_probe <- serialize(first$state,NULL)
      LKTGRUScore(probe,first$state,candidates,run)
      stopifnot(identical(before_probe,serialize(first$state,NULL)))
      fresh <- d[1:7,];fresh$Anon.Student.Id <- 'new-student'
      close(LKTGRUStep(probe,first$state,fresh,run)$predictions,whole$predictions)
      cat('PASS nonzero constant output plus local item updates, student reset and immutable scoring\n')
    }
    broken <- state;broken$adaptive_embeddings$schema <- 9L
    fail(LKTGRUStep(fit,broken,d[1,,drop=FALSE]))
    # Lost or duplicated identity names must fail, rather than silently reset a
    # retained student/item to zero or choose one of two conflicting vectors.
    for(level in c('students','columns','items','duplicate_items')) {
      broken <- one$state
      students <- broken$adaptive_embeddings$students
      if(level=='students')students <- unname(students)
      if(level=='columns')students[[1]] <- unname(students[[1]])
      if(level=='items')students[[1]][[1]] <- unname(students[[1]][[1]])
      if(level=='duplicate_items')students[[1]][[1]] <- rep(students[[1]][[1]],2L)
      broken$adaptive_embeddings$students <- students
      fail(LKTGRUStep(fit,broken,d[2,,drop=FALSE],runtime))
    }
    cat('PASS adaptive embeddings, retrieval=',retrieval,', plasticity=',plasticity,': replay, causality, unseen IDs, isolation, continuation, persistence, immutable weights\n')
  }
  # Held-out answers can adapt that student's state, but cannot train shared
  # weights, vocabulary or numeric scaling. This checks the training boundary.
  train <- d;train$fold <- ifelse(train$Anon.Student.Id=='s4',3L,1L)
  changed <- train;changed$CF..ansbin.[changed$fold==3L] <- 1-changed$CF..ansbin.[changed$fold==3L]
  config$epochs <- 1L
  fit_one <- function(data)LKT(data,features='lineafm',components='KC..Default.',usefolds=1L,
    verbose=FALSE,model=do.call(GRUModel,config))
  a <- fit_one(train);b <- fit_one(changed)
  stopifnot(identical(a$model$weights,b$model$weights),identical(a$model$preprocessing,b$model$preprocessing))
  cat('PASS held-out outcomes cannot change shared fitted weights or preprocessing\n')
  fail(GRUModel(embedding_adaptation='per_student',plasticity='fatigue',plasticity_time_col='CF..Time.'))
  fail(GRUModel(embedding_adaptation='per_student',embedding_learning_rate=-1))
}
