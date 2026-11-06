#' Create or continue sequence-model student histories
#'
#' States contain ordinary R arrays, optional per-history retrieval banks, and,
#' for engineered features, retained raw observations. They can be
#' saved with saveRDS. They are tied to one fitted model. Predictions are made
#' before response updates; network weights never change during continuation.
#' @param modelob Fitted sequence or GRU LKT model.
#' @param state State returned by LKTSequenceState or LKTSequenceStep.
#' @param data New trial rows. CF..ansbin. is optional for an unanswered final trial in each history group.
#' @param runtime Optional process-local runtime returned by LKTSequenceRuntime.
#' @return LKTSequenceState returns empty state. LKTSequenceStep returns predictions and new state.
#' @export
LKTSequenceState <- function(modelob) {
  # Tie a fresh continuation state to this fitted model. Hidden memories cannot safely
  # be transferred to another model with different learned weights.
  model <- lkt_sequence_model(modelob)
  structure(list(schema=1L,model_id=model$id,hidden=list(),history=NULL,pending=list(),
    last_order=list(),last_time=list(),inference_context=list()),
    class=if(modelob$model_name=='GRU')'LKTGRUState' else 'LKTSequenceState')
}

lkt_gru_model <- function(modelob) {
  if(!is.list(modelob) || !identical(modelob$model_name,'GRU'))
    stop('Expected a fitted GRU LKT model.',call.=FALSE)
  lkt_sequence_model(modelob)
}

lkt_sequence_model <- function(modelob) {
  if(!is.list(modelob) || !isTRUE(modelob$model_name %in% c('GRU','Sequence')) || !identical(modelob$model$schema,1L))
    stop('Expected a fitted sequence LKT model.',call.=FALSE)
  model <- modelob$model
  # Archived plain-GRU checkpoints predate the optional inference controls. Resolve
  # their saved settings through the public defaults; their network remains unchanged.
  if(modelob$model_name=='Sequence' && is.null(model$settings$unit))stop('Sequence architecture is missing.')
  model$settings <- lkt_sequence_settings(model$settings)
  model
}

lkt_gru_bind <- function(old,new) {
  if(is.null(old))return(new)
  columns <- union(names(old),names(new))
  for(n in setdiff(columns,names(old)))old[[n]] <- NA
  for(n in setdiff(columns,names(new)))new[[n]] <- NA
  rbind(old[columns],new[columns])
}

#' Load a fitted sequence model once for continued inference
#'
#' A runtime owns a loaded Torch network and belongs to its creating process.
#' Save fitted models and sequence state objects, never runtimes. Create a runtime
#' independently in each worker. Construction and prediction restore caller RNG
#' states and Torch thread settings.
#' @param modelob Fitted sequence or GRU LKT model.
#' @return A process-local sequence runtime environment.
#' @export
LKTSequenceRuntime <- function(modelob) {
  model <- lkt_sequence_model(modelob)
  cleanup <- lkt_gru_runtime(model$settings);on.exit(cleanup(),add=TRUE)
  runtime <- new.env(parent=emptyenv())
  runtime$model_id <- model$id;runtime$pid <- Sys.getpid()
  runtime$settings <- model$settings
  runtime$network <- lkt_gru_network(model$preprocessing,model$settings)
  lkt_gru_load_weights(runtime$network,model$weights,model$settings)
  runtime$network$eval()
  class(runtime) <- if(modelob$model_name=='GRU')'LKTGRURuntime' else 'LKTSequenceRuntime'
  lockEnvironment(runtime,bindings=TRUE)
  runtime
}

lkt_gru_runtime_network <- function(modelob,model,runtime) {
  if(is.null(runtime))runtime <- LKTSequenceRuntime(modelob)
  if(!inherits(runtime,c('LKTGRURuntime','LKTSequenceRuntime')) || !is.environment(runtime) ||
     !identical(runtime$pid,Sys.getpid()) || !identical(runtime$model_id,model$id) ||
     !identical(runtime$settings,model$settings))
    stop('GRU runtime must belong to this fitted model and current process.',call.=FALSE)
  runtime$network
}

#' Score independent sequence candidates without observing them
#'
#' Each row branches from the same supplied state, including when rows share a
#' student or history key. Answers are ignored. No hidden state, raw history,
#' pending trial, or ordering metadata is committed. Returned probabilities are
#' in input order. This differs from LKTSequenceStep, which processes a sequence.
#' @param modelob Fitted sequence or GRU LKT model.
#' @param state Retained sequence or GRU state.
#' @param data Candidate trial rows.
#' @param runtime Optional process-local sequence or GRU runtime.
#' @return Numeric correctness probabilities, one per candidate.
#' @export
LKTSequenceScore <- function(modelob,state,data,runtime=NULL) {
  model <- lkt_sequence_model(modelob)
  if(is.null(runtime))runtime <- LKTSequenceRuntime(modelob)
  lkt_gru_runtime_network(modelob,model,runtime)
  d <- as.data.frame(data)
  if(!nrow(d))stop('Supply at least one GRU candidate.',call.=FALSE)
  d$CF..ansbin. <- NA_real_
  if('Outcome' %in% names(d))d$Outcome <- NA_character_
  vapply(seq_len(nrow(d)),function(i)
    LKTSequenceStep(modelob,state,d[i,,drop=FALSE],runtime=runtime)$predictions[[1L]],numeric(1))
}

#' @rdname LKTSequenceState
#' @export
LKTSequenceStep <- function(modelob,state,data,runtime=NULL) {
  # Check the state and restore the trained network before processing new trials.
  # Continuation updates memory while leaving the network weights fixed.
  model <- lkt_sequence_model(modelob);s <- model$settings
  state_class <- if(modelob$model_name=='GRU')'LKTGRUState' else 'LKTSequenceState'
  if(!inherits(state,state_class) || !identical(state$schema,1L) || !identical(state$model_id,model$id))
    stop('GRU state must belong to this fitted model.',call.=FALSE)
  cleanup <- lkt_gru_runtime(s);on.exit(cleanup(),add=TRUE)
  d <- as.data.frame(data)
  if(!nrow(d))stop('Supply at least one GRU trial.',call.=FALSE)
  if(!'CF..ansbin.' %in% names(d))d$CF..ansbin. <- if('Outcome' %in% names(d))
    ifelse(d$Outcome=='CORRECT',1,ifelse(d$Outcome=='INCORRECT',0,NA_real_)) else NA_real_
  if(any(!is.na(d$CF..ansbin.) & !d$CF..ansbin. %in% c(0,1)))stop('GRU outcomes must be binary or missing.',call.=FALSE)
  d$pred <- NULL
  ord <- lkt_gru_order(d,s);d <- d[ord,,drop=FALSE]
  keys <- lkt_gru_keys(d,s$history_group_cols)
  net <- lkt_gru_runtime_network(modelob,model,runtime)
  net$eval()
  output <- numeric(nrow(d))
  torch::with_no_grad({
    for(i in seq_len(nrow(d))) {
      row <- d[i,,drop=FALSE];key <- keys[i]
      # An unanswered trial must be resolved before that history advances. Otherwise the
      # next update would have to invent a response or silently omit an interaction.
      if(!is.null(state$pending[[key]])) {
        pending <- state$pending[[key]]
        compare <- setdiff(union(names(row),names(pending)),c('CF..ansbin.','Outcome',s$weight_col,'fold'))
        if(!all(compare %in% intersect(names(row),names(pending))) ||
            !isTRUE(all.equal(row[compare],pending[compare],check.attributes=FALSE)))
          stop('Supply the answer to the pending GRU trial before advancing its history.',call.=FALSE)
      }
      subject <- as.character(row[[s$subject_col]])
      if(!is.null(s$order_col) && !is.null(state$last_order[[subject]])) {
        if(as.numeric(row[[s$order_col]])<state$last_order[[subject]])
          stop('GRU continuation order goes backwards.',call.=FALSE)
      }
      # Raw continuation requires only bounded per-group state. Engineered
      # features replay the relevant student's history, never other students.
      combined <- if(s$use_lkt_features)lkt_gru_bind(state$history[[subject]],row) else row
      encoded <- lkt_gru_encode(combined,model$preprocessing,model$recipe,s,
        previous_time=if(s$use_lkt_features)NULL else state$last_time)
      last <- nrow(combined)
      # A pending response is never used to compute the prediction. The cell is
      # evaluated only after an observed response, so no imputation is required.
      numeric <- lkt_gru_tensor(array(lkt_gru_numeric_rows(encoded,last),c(1L,1L,ncol(encoded$numeric))),s)
      categorical <- lkt_gru_tensor(array(encoded$categorical[last,,drop=FALSE],c(1L,1L,ncol(encoded$categorical))),s,TRUE)
      hidden <- if(is.null(state$hidden[[key]]))lkt_sequence_initial(s,1L,numeric) else
        lkt_sequence_state_map(state$hidden[[key]],s,'unpack')
      # Predict from current inputs and the previous top-layer state before looking at
      # the answer. This also permits a probability for a genuinely unanswered trial.
      x <- net$encode(numeric,categorical)
      prior <- lkt_sequence_read(hidden,s)
      extended <- s$inference_mode!='none' || s$feature_encoder!='none' || s$retrieval_heads>0L
      if(!extended)logit <- net$head(torch::torch_cat(list(prior,x),dim=3))
      if(extended) {
        context <- state$inference_context[[key]]
        if(!is.null(context))context <- lapply(context,function(x)if(is.character(x))x else lkt_gru_tensor(x,s))
        answer <- lkt_gru_tensor(array(if(is.na(row$CF..ansbin.))0 else row$CF..ansbin.,c(1L,1L,1L)),s)
        retrieval_ids <- if(length(s$retrieval_filter_cols))
          array(encoded$retrieval_ids[last,,drop=FALSE],c(1L,1L,length(s$retrieval_filter_cols))) else NULL
        inferred <- net(numeric,categorical,answer$squeeze(3),hidden,context,retrieval_ids)
        logit <- inferred$logits
      }
      output[i] <- pmin(pmax(torch::torch_sigmoid(logit)$item(),s$clip_epsilon),1-s$clip_epsilon)
      # Only an observed answer can advance the recurrent memory and retained feature
      # history. A pending trial is stored for later completion without changing that
      # memory.
      if(!is.na(row$CF..ansbin.)) {
        answer <- lkt_gru_tensor(array(row$CF..ansbin.,c(1L,1L,1L)),s)
        next_hidden <- if(!extended)lkt_sequence_advance(net,torch::torch_cat(list(x,answer),dim=3),hidden,s)[[2]] else inferred$hidden
        state$hidden[[key]] <- lkt_sequence_state_map(next_hidden,s,'pack')
        if(s$inference_mode!='none' || s$retrieval_heads>0L)state$inference_context[[key]] <-
          lapply(inferred$context,function(t)if(is.character(t))t else torch::as_array(t$cpu()))
        if(s$use_lkt_features) {
          if(is.null(state$history))state$history <- list()
          state$history[[subject]] <- combined
        }
        if(!is.null(s$order_col))state$last_order[[subject]] <- as.numeric(row[[s$order_col]])
        if(!is.null(s$time_col))state$last_time[[key]] <- lkt_gru_time(row,s)
        state$pending[[key]] <- NULL
      } else state$pending[[key]] <- row
    }
  })
  # Restore prediction order before returning the updated state. Callers can align
  # probabilities with their submitted rows even when chronological sorting was
  # required.
  list(predictions=output[order(ord)],state=state)
}

# Historical entry points retain their fitted-model and return-class contracts.
#' Create or continue GRU student histories
#' @inheritParams LKTSequenceState
#' @return LKTGRUState returns empty state. LKTGRUStep returns predictions and updated state.
#' @rdname LKTGRUState
#' @export
LKTGRUState <- function(modelob) {lkt_gru_model(modelob);LKTSequenceState(modelob)}
#' Load a fitted GRU once for continued inference
#' @inheritParams LKTSequenceRuntime
#' @return A process-local GRU runtime environment.
#' @rdname LKTGRURuntime
#' @export
LKTGRURuntime <- function(modelob) {lkt_gru_model(modelob);LKTSequenceRuntime(modelob)}
#' Score independent GRU candidates without observing them
#' @inheritParams LKTSequenceScore
#' @return Numeric correctness probabilities, one per candidate.
#' @rdname LKTGRUScore
#' @export
LKTGRUScore <- function(modelob,state,data,runtime=NULL) {
  lkt_gru_model(modelob);LKTSequenceScore(modelob,state,data,runtime)
}
#' @rdname LKTGRUState
#' @export
LKTGRUStep <- function(modelob,state,data,runtime=NULL) {
  lkt_gru_model(modelob);LKTSequenceStep(modelob,state,data,runtime)
}
