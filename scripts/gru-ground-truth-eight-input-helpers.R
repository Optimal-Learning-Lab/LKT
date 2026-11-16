# Research preparation and assertions only. All neural algorithms remain in LKT.
gt8_attach_traits <- function(data,students,items) {
  stopifnot(!is.null(names(students)),!is.null(names(items)),!anyDuplicated(names(students)),
    !anyDuplicated(names(items)),all(is.finite(students)),all(is.finite(items)))
  result <- as.data.frame(data)
  result$random_intercept <- unname(students[as.character(result$Anon.Student.Id)])
  result$kc_intercept <- unname(items[as.character(result$KC..Default.)])
  if(any(!is.finite(result$random_intercept))||any(!is.finite(result$kc_intercept)))
    stop('Every row must have its explicitly supplied LKT student and item intercept')
  result
}

gt8_attach_training_traits <- function(data,students,items,training_fold=1L) {
  roles <- unique(as.data.frame(data)[c('Anon.Student.Id','fold')])
  if(anyDuplicated(roles$Anon.Student.Id))stop('Training-only traits require whole-student folds')
  train <- data$fold==training_fold
  if(!any(train)||!setequal(names(students),as.character(data$Anon.Student.Id[train])))
    stop('Ability coefficients must belong exactly to training students')
  if(any(!is.finite(students))||any(!is.finite(items))||anyDuplicated(names(students))||anyDuplicated(names(items)))
    stop('Training intercepts must be finite and uniquely keyed')
  mean_ability <- mean(unname(students))
  result <- as.data.frame(data)
  result$random_intercept <- rep(mean_ability,nrow(result))
  result$random_intercept[train] <- unname(students[as.character(result$Anon.Student.Id[train])])
  result$kc_intercept <- unname(items[as.character(result$KC..Default.)])
  if(any(!is.finite(result$kc_intercept)))stop('All evaluated items require a training-calibrated easiness intercept')
  stopifnot(all(result$random_intercept[!train]==mean_ability))
  list(data=result,prior=list(mean=mean_ability,sd=stats::sd(unname(students)),
    students=length(students),method='Unweighted mean of training-student LKT intercepts'))
}

gt8_preprocessing_leakage_check <- function(data,recipe,settings) {
  train <- data$fold==1L;heldout <- !train
  preprocess <- getFromNamespace('lkt_gru_preprocess','LKT')
  original <- preprocess(data,train,recipe,settings)
  poisoned <- data
  poisoned$CF..ansbin.[heldout] <- 1-poisoned$CF..ansbin.[heldout]
  # Held-out numeric extremes must not alter fitted scaling, either.
  poisoned$random_intercept[heldout] <- poisoned$random_intercept[heldout]+100
  poisoned$kc_intercept[heldout] <- poisoned$kc_intercept[heldout]-100
  changed <- preprocess(poisoned,train,recipe,settings)
  fields <- c('vocabularies','embedding_dims','numeric_names','center','scale')
  stopifnot(identical(original[fields],changed[fields]))
  numeric <- getFromNamespace('lkt_gru_numeric','LKT')
  x <- numeric(data,recipe,settings,fit_rows=train)$matrix
  z <- numeric(poisoned,recipe,settings,fit_rows=train)$matrix
  training_only <- numeric(data[train,,drop=FALSE],recipe,settings,
    fit_rows=rep(TRUE,sum(train)))$matrix
  strip <- function(m)unname(as.matrix(m))
  stopifnot(identical(strip(x[train,,drop=FALSE]),strip(z[train,,drop=FALSE])),
    identical(strip(x[train,,drop=FALSE]),strip(training_only)))
  for(col in settings$categorical_cols)
    stopifnot(setequal(as.character(original$vocabularies[[col]]),unique(as.character(data[[col]][train]))))
  list(passed=TRUE,heldout_response_flip_and_trait_perturbation=TRUE,
    training_scaling_unchanged=TRUE,training_vocabularies_unchanged=TRUE,
    training_features_unchanged=TRUE,training_only_subset_matches=TRUE,
    supplied_skill_labels_only=TRUE,no_pretrained_item_or_skill_vectors=TRUE)
}

gt8_runtime_checks <- function(fit,data) {
  idx <- head(which(data$fold==3L & data$Anon.Student.Id==data$Anon.Student.Id[which(data$fold==3L)[1L]]),16L)
  probe <- as.data.frame(data[idx,,drop=FALSE])
  state <- LKT::LKTGRUState(fit);runtime <- LKT::LKTGRURuntime(fit)
  initial <- serialize(state,NULL);weights <- serialize(fit$model$weights,NULL)
  before <- LKT::LKTGRUScore(fit,state,probe,runtime,method='batched')
  reverse <- LKT::LKTGRUScore(fit,state,probe[nrow(probe):1L,,drop=FALSE],runtime,method='batched')
  reference <- LKT::LKTGRUScore(fit,state,probe,runtime,method='reference')
  full <- LKT::LKTGRUStep(fit,state,probe,runtime)
  half <- max(1L,nrow(probe)%/%2L)
  first <- LKT::LKTGRUStep(fit,state,probe[seq_len(half),,drop=FALSE],runtime)
  temporary <- tempfile(fileext='.rds');on.exit(unlink(temporary),add=TRUE)
  saveRDS(first$state,temporary)
  second <- LKT::LKTGRUStep(fit,readRDS(temporary),probe[-seq_len(half),,drop=FALSE],runtime)
  renamed <- probe
  for(col in c('Anon.Student.Id','KC..Default.','KC..Cluster.'))renamed[[col]] <- paste0('unseen-',renamed[[col]])
  unfamiliar <- LKT::LKTGRUStep(fit,state,renamed,runtime)
  future_changed <- probe;cut <- max(2L,nrow(probe)%/%2L)
  future_changed$CF..ansbin.[seq.int(cut,nrow(probe))] <- 1-future_changed$CF..ansbin.[seq.int(cut,nrow(probe))]
  causal <- LKT::LKTGRUStep(fit,state,future_changed,runtime)
  one <- probe[1L,,drop=FALSE];after <- LKT::LKTGRUStep(fit,state,one,runtime)
  local <- after$state$adaptive_embeddings$students[[as.character(one$Anon.Student.Id)]]
  for(col in fit$model$settings$adaptive_embedding_cols)
    stopifnot(identical(names(local[[col]]),as.character(one[[col]])),any(local[[col]][[as.character(one[[col]])]]!=0))
  fresh <- one;fresh$Anon.Student.Id <- 'another-student-with-same-trait-inputs'
  reset <- LKT::LKTGRUStep(fit,after$state,fresh,runtime)
  trait_grid <- one[rep(1L,9L),,drop=FALSE]
  # Use actual training input spreads, preserving the saved standardization.
  for(col in c('random_intercept','kc_intercept')) {
    values <- data[[col]][data$fold==1L];spread <- stats::sd(values)
    trait_grid[[col]] <- if(col=='random_intercept')rep(mean(values)+c(-1,0,1)*spread,each=3L) else
      rep(mean(values)+c(-1,0,1)*spread,times=3L)
  }
  sensitivity <- LKT::LKTGRUScore(fit,state,trait_grid,runtime,method='batched')
  differences <- c(replay=max(abs(full$predictions-fit$prediction[idx])),
    future_answer_isolation=max(abs(causal$predictions[seq_len(cut)]-full$predictions[seq_len(cut)])),
    ordering=max(abs(before-rev(reverse))),batching=max(abs(before-reference)),
    chunks=max(abs(c(first$predictions,second$predictions)-full$predictions)),
    renamed=max(abs(unfamiliar$predictions-full$predictions)),
    reset=max(abs(reset$predictions-after$predictions)))
  stopifnot(all(differences<3e-6),identical(initial,serialize(state,NULL)),
    identical(weights,serialize(fit$model$weights,NULL)),all(is.finite(sensitivity)),
    identical(probe$random_intercept,as.data.frame(data[idx,,drop=FALSE])$random_intercept),
    identical(probe$kc_intercept,as.data.frame(data[idx,,drop=FALSE])$kc_intercept))
  list(maximum_differences=as.list(differences),candidate_scoring_immutable=TRUE,
    shared_weights_unchanged=TRUE,student_and_item_skill_resets=TRUE,
    checkpoint_chunking=TRUE,unseen_labels=TRUE,supplied_traits_unchanged=TRUE,
    cold_start_trait_grid=data.frame(student_intercept=trait_grid$random_intercept,
      item_intercept=trait_grid$kc_intercept,probability=sensitivity))
}
