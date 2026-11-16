# Experiment contracts only; neural training, feature kernels and tracing stay in LKT.
tracer6_calibration_check <- function(data,control,protocol) {
  train <- which(data$fold==1L)
  roles <- unique(data[c('Anon.Student.Id','fold')])
  if(anyDuplicated(roles$Anon.Student.Id)||!identical(protocol$training_rows,train))
    stop('Calibration must use exactly the declared whole-student training rows')
  fit <- control$fit
  if(!identical(fit$newdata$fold,data$fold)||
     !identical(fit$newdata$CF..ansbin.,data$CF..ansbin.)||
     !identical(as.character(fit$newdata$Anon.Student.Id),as.character(data$Anon.Student.Id)))
    stop('Calibration observations or fold assignments differ')
  stopifnot(length(fit$prediction)==nrow(data),all(is.finite(fit$prediction)),
    all(fit$prediction>0 & fit$prediction<1),length(fit$optimizedpars$par)==6L,
    all(is.finite(fit$optimizedpars$par)),fit$optimizedpars$convergence==0L,
    is.null(fit$latencymodel))
  loss <- -sum(ifelse(data$CF..ansbin.[train]==1,
    log(fit$prediction[train]),log1p(-fit$prediction[train])))
  stopifnot(length(fit$loglike)>=1L,abs(unname(fit$loglike[1L])+loss)<1e-6,
    abs(fit$optimizedpars$value-loss)<1e-6)
  list(passed=TRUE,calibration_rows=length(train),
    calibration_students=length(unique(data$Anon.Student.Id[train])),
    training_only_objective_verified=TRUE,training_negative_log_likelihood=loss,
    reused_frozen_calibration=TRUE,additional_nonlinear_fits=0L,wald_fits=0L)
}

tracer6_preprocessing_check <- function(data,recipe,settings) {
  train <- data$fold==1L
  prep <- getFromNamespace('lkt_gru_preprocess','LKT')
  numeric <- getFromNamespace('lkt_gru_numeric','LKT')
  original <- prep(data,train,recipe,settings)
  poisoned <- data
  poisoned$CF..ansbin.[!train] <- 1-poisoned$CF..ansbin.[!train]
  for(col in settings$time_feature_cols)
    poisoned[[col]][!train] <- poisoned[[col]][!train]+1e6
  changed <- prep(poisoned,train,recipe,settings)
  stopifnot(identical(original,changed))
  x <- numeric(data,recipe,settings,fit_rows=train)$matrix
  z <- numeric(poisoned,recipe,settings,fit_rows=train)$matrix
  only_train <- numeric(data[train,,drop=FALSE],recipe,settings,
    fit_rows=rep(TRUE,sum(train)))$matrix
  strip <- function(m)unname(as.matrix(m))
  stopifnot(ncol(x)==6L,identical(strip(x[train,,drop=FALSE]),strip(z[train,,drop=FALSE])),
    identical(strip(x[train,,drop=FALSE]),strip(only_train)))
  for(col in settings$categorical_cols)
    stopifnot(setequal(original$vocabularies[[col]],as.character(data[[col]][train])))
  list(passed=TRUE,heldout_answer_and_timing_perturbation=TRUE,
    training_features_unchanged=TRUE,training_only_subset_matches=TRUE,
    training_scaling_and_vocabularies_unchanged=TRUE,numeric_inputs=ncol(x))
}

tracer6_runtime_check <- function(fit,data) {
  first_test <- which(data$fold==3L)[1L]
  idx <- head(which(data$fold==3L & data$Anon.Student.Id==data$Anon.Student.Id[first_test]),16L)
  probe <- as.data.frame(data[idx,,drop=FALSE])
  state <- LKT::LKTGRUState(fit);runtime <- LKT::LKTGRURuntime(fit)
  initial <- serialize(state,NULL);weights <- serialize(fit$model$weights,NULL)
  score <- LKT::LKTGRUScore(fit,state,probe,runtime,method='batched')
  reversed <- LKT::LKTGRUScore(fit,state,probe[nrow(probe):1L,,drop=FALSE],runtime,method='batched')
  reference <- LKT::LKTGRUScore(fit,state,probe,runtime,method='reference')
  flipped <- probe;flipped$CF..ansbin. <- 1-flipped$CF..ansbin.
  ignored_answers <- LKT::LKTGRUScore(fit,state,flipped,runtime,method='batched')
  full <- LKT::LKTGRUStep(fit,state,probe,runtime)
  cut <- max(2L,nrow(probe)%/%2L)
  flipped <- probe
  flipped$CF..ansbin.[seq.int(cut,nrow(probe))] <- 1-flipped$CF..ansbin.[seq.int(cut,nrow(probe))]
  causal <- LKT::LKTGRUStep(fit,state,flipped,runtime)
  first <- LKT::LKTGRUStep(fit,state,probe[seq_len(cut),,drop=FALSE],runtime)
  temporary <- tempfile(fileext='.rds');on.exit(unlink(temporary),add=TRUE)
  saveRDS(first$state,temporary)
  second <- LKT::LKTGRUStep(fit,readRDS(temporary),probe[-seq_len(cut),,drop=FALSE],runtime)
  renamed <- probe
  for(col in c('Anon.Student.Id',fit$model$settings$categorical_cols))
    renamed[[col]] <- paste0('unseen-',renamed[[col]])
  unfamiliar <- LKT::LKTGRUStep(fit,state,renamed,runtime)
  one <- probe[1L,,drop=FALSE];after <- LKT::LKTGRUStep(fit,state,one,runtime)
  local <- after$state$adaptive_embeddings$students[[as.character(one$Anon.Student.Id)]]
  for(col in fit$model$settings$adaptive_embedding_cols)
    stopifnot(identical(names(local[[col]]),as.character(one[[col]])),
      any(local[[col]][[as.character(one[[col]])]]!=0))
  fresh <- one;fresh$Anon.Student.Id <- 'another-student'
  reset <- LKT::LKTGRUStep(fit,after$state,fresh,runtime)
  differences <- c(replay=max(abs(full$predictions-fit$prediction[idx])),
    future_answer_isolation=max(abs(causal$predictions[seq_len(cut)]-full$predictions[seq_len(cut)])),
    ordering=max(abs(score-rev(reversed))),batching=max(abs(score-reference)),
    candidate_answers_ignored=max(abs(score-ignored_answers)),
    chunks=max(abs(c(first$predictions,second$predictions)-full$predictions)),
    renamed=max(abs(unfamiliar$predictions-full$predictions)),
    reset=max(abs(reset$predictions-after$predictions)))
  stopifnot(all(differences<3e-6),identical(initial,serialize(state,NULL)),
    identical(weights,serialize(fit$model$weights,NULL)))
  list(passed=TRUE,maximum_differences=as.list(differences),
    scoring_immutable=TRUE,shared_weights_unchanged=TRUE,
    student_item_skill_resets=TRUE,causal_updates=TRUE,checkpoint_chunking=TRUE)
}
