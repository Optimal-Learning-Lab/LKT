#' Create or continue sequence-model student histories
#'
#' States contain ordinary R arrays, optional per-history retrieval banks, and,
#' for engineered features, retained raw observations. They can be
#' saved with saveRDS. They are tied to one fitted model. Predictions are made
#' before response updates; shared network weights never change during continuation.
#' With plasticity enabled, student-specific output-weight adjustments and signed
#' learning rates change only after observed feedback and are retained in context.
#' With hidden_decay enabled, hidden units and their signed DK histories evolve
#' using elapsed time even though the learned connection weights remain fixed.
#' With per_student embedding adaptation, a versioned adaptive_embeddings member
#' stores student-local vectors; candidate scoring never changes those vectors.
#' @param modelob Fitted sequence or GRU LKT model.
#' @param state State returned by LKTSequenceState or LKTSequenceStep.
#' @param data New trial rows. Outcome is authoritative when supplied. STUDY commits an exposure without a response update; PENDING (or a missing binary answer without Outcome) is an unanswered final trial in each history group.
#' @param runtime Optional process-local runtime returned by LKTSequenceRuntime.
#' @return LKTSequenceState returns empty state. LKTSequenceStep returns predictions and new state.
#' @export
LKTSequenceState <- function(modelob) {
  # Tie a fresh continuation state to this fitted model. Hidden memories cannot safely
  # be transferred to another model with different learned weights.
  model <- lkt_sequence_model(modelob)
  state <- structure(list(schema=1L,model_id=model$id,hidden=list(),history=NULL,pending=list(),
    last_order=list(),last_time=list(),inference_context=list()),
    class=if(modelob$model_name=='GRU')'LKTGRUState' else 'LKTSequenceState')
  if(lkt_gru_clocked(model$settings))state$last_decay_time <- list()
  if(lkt_gru_adaptive(model$settings))state$adaptive_embeddings <- lkt_gru_adaptive_state()
  state
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
  # Read the historical layout explicitly before validating saved settings.
  # New constructor defaults must never reinterpret an existing checkpoint.
  if(modelob$model_name=='Sequence' && is.null(model$settings$unit))stop('Sequence architecture is missing.')
  model$settings <- lkt_sequence_settings(lkt_gru_saved_controls(model$settings))
  lkt_gru_optional_recipe(model$recipe,model$settings)
  model
}

lkt_gru_bind <- function(old,new) {
  if(is.null(old))return(new)
  # Binary-only committed histories (including saved older states) must acquire
  # their labels before columns are aligned with an Outcome-bearing new row.
  if(!'Outcome' %in% names(old))old <- lkt_normalize_outcomes(old,allow_pending=TRUE)
  if(!'Outcome' %in% names(new))new <- lkt_normalize_outcomes(new,allow_pending=TRUE)
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
  if(lkt_gru_semantic_active(model$settings)) {
    runtime$semantic_cache <- lkt_gru_semantic_cache(model$preprocessing$semantic_cache)
    lkt_gru_semantic_check_runtime(runtime$semantic_cache,model$settings)
  }
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
#' @param method reference (default) preserves historical scoring; batched opts
#'   into independent GRU candidate queries without computing learning updates.
#' @param candidate_batch_size Maximum number of neural candidate queries per chunk.
#' @return Numeric correctness probabilities, one per candidate.
#' @export
LKTSequenceScore <- function(modelob,state,data,runtime=NULL,method=c('reference','batched'),
                             candidate_batch_size=256L) {
  method <- match.arg(method)
  model <- lkt_sequence_model(modelob)
  if(is.null(runtime))runtime <- LKTSequenceRuntime(modelob)
  lkt_gru_runtime_network(modelob,model,runtime)
  d <- lkt_gru_optional_inputs(as.data.frame(data),model$settings)
  if(!nrow(d))stop('Supply at least one GRU candidate.',call.=FALSE)
  d$CF..ansbin. <- NA_real_
  d$Outcome <- 'PENDING'
  if(method=='batched')return(lkt_gru_score_candidates(modelob,model,state,d,runtime,candidate_batch_size))
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
  if(lkt_gru_clocked(s) && (!is.list(state$last_decay_time) ||
     !setequal(names(state$hidden),names(state$last_decay_time))))
    stop('Hidden decay state must retain a committed timestamp for every hidden history.',call.=FALSE)
  cleanup <- lkt_gru_runtime(s);on.exit(cleanup(),add=TRUE)
  if(lkt_gru_adaptive(s))lkt_gru_adaptive_validate_state(state$adaptive_embeddings,s)
  d <- lkt_gru_optional_inputs(as.data.frame(data),s)
  if(!nrow(d))stop('Supply at least one GRU trial.',call.=FALSE)
  d <- lkt_normalize_outcomes(d,allow_pending=TRUE)
  d$pred <- NULL
  ord <- lkt_gru_order(d,s);d <- d[ord,,drop=FALSE]
  keys <- lkt_gru_keys(d,s$history_group_cols)
  if(is.null(runtime))runtime <- LKTSequenceRuntime(modelob)
  net <- lkt_gru_runtime_network(modelob,model,runtime)
  net$eval()
  output <- numeric(nrow(d))
  execute <- function() {
    for(i in seq_len(nrow(d))) {
      row <- d[i,,drop=FALSE];key <- keys[i]
      if(lkt_gru_clocked(s) && !is.null(state$hidden[[key]])) {
        time <- state$last_decay_time[[key]]
        if(!is.numeric(time) || length(time)!=1L || !is.finite(time))
          stop('Hidden decay committed timestamps must be finite numeric scalars.',call.=FALSE)
      }
      # An unanswered trial must be resolved before that history advances. Otherwise the
      # next update would have to invent a response or silently omit an interaction.
      if(!is.null(state$pending[[key]])) {
        pending <- state$pending[[key]]
        compare <- setdiff(union(names(row),names(pending)),c('CF..ansbin.','Outcome',s$weight_col,'fold',if(lkt_gru_semantic_active(s))s$semantic_input$response_col))
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
        previous_time=if(s$use_lkt_features)NULL else state$last_time,
        previous_decay_time=if(s$use_lkt_features)NULL else state$last_decay_time,
        semantic_cache=if(lkt_gru_semantic_active(s))runtime$semantic_cache else NULL)
      last <- nrow(combined)
      # A pending response is never used to compute the prediction. Extended
      # forwards may calculate an uncommitted transition with a placeholder;
      # their causal pre-answer prediction is independent of that placeholder.
      numeric <- lkt_gru_tensor(array(lkt_gru_numeric_rows(encoded,last),c(1L,1L,ncol(encoded$numeric))),s)
      categorical <- lkt_gru_tensor(array(encoded$categorical[last,,drop=FALSE],c(1L,1L,ncol(encoded$categorical))),s,TRUE)
      hidden <- if(is.null(state$hidden[[key]]))lkt_sequence_initial(s,1L,numeric) else
        lkt_sequence_state_map(state$hidden[[key]],s,'unpack')
      # Predict from current inputs and the previous top-layer state before looking at
      # the answer. This also permits a probability for a genuinely unanswered trial.
      extended <- lkt_gru_semantic_active(s) || s$inference_mode!='none' || s$feature_encoder!='none' || s$retrieval_heads>0L || lkt_gru_plastic(s) || lkt_gru_gate_active(s) || lkt_gru_adaptive(s)
      if(!s$hidden_decay && !lkt_gru_adaptive(s)) {
        x <- net$encode(numeric,categorical)
        prior <- lkt_sequence_read(hidden,s)
        if(!extended)logit <- net$head(torch::torch_cat(list(prior,x),dim=3))
      }
      if(extended || s$hidden_decay) {
        context <- state$inference_context[[key]]
        if(!is.null(context))context <- lapply(context,function(x)if(is.character(x))x else lkt_gru_tensor(x,s))
        answer <- lkt_gru_tensor(array(if(is.na(row$CF..ansbin.))0 else row$CF..ansbin.,c(1L,1L,1L)),s)
        retrieval_ids <- if(length(s$retrieval_filter_cols))
          array(encoded$retrieval_ids[last,,drop=FALSE],c(1L,1L,length(s$retrieval_filter_cols))) else NULL
        gap <- if(lkt_gru_clocked(s))lkt_gru_tensor(matrix(encoded$decay_gaps[last],1L,1L),s) else NULL
        if(lkt_gru_adaptive(s)) {
          local <- list(state$adaptive_embeddings$students[[subject]])
          if(is.null(local[[1L]]))local[[1L]] <- list()
          values <- lkt_gru_adaptive_values(local,row,s)
          inferred <- lkt_gru_adaptive_forward(net,numeric,categorical,answer$squeeze(3),hidden,context,
            retrieval_ids,values,s,gap)
          if(!is.na(row$CF..ansbin.)) {
            loss <- lkt_gru_adaptive_loss(inferred$logits,answer$squeeze(3),s,
              if(is.null(s$weight_col))NULL else lkt_gru_tensor(matrix(row[[s$weight_col]],1L,1L),s))
            local <- lkt_gru_adaptive_update(local,row,values,loss,s,TRUE)
            state$adaptive_embeddings$students[[subject]] <- local[[1L]]
          }
        } else inferred <- lkt_gru_forward(net,numeric,categorical,answer$squeeze(3),hidden,context,
          retrieval_ids,s,gap,semantic=lkt_gru_semantic_tensor(encoded,matrix(last,1L,1L),s))
        logit <- inferred$logits
      }
      output[i] <- pmin(pmax(torch::torch_sigmoid(logit)$item(),s$clip_epsilon),1-s$clip_epsilon)
      # Only an observed answer can advance the recurrent memory and retained feature
      # history. A pending trial is stored for later completion without changing that
      # memory.
      if(!is.na(row$CF..ansbin.)) {
        answer <- lkt_gru_tensor(array(row$CF..ansbin.,c(1L,1L,1L)),s)
        next_hidden <- if(!extended && !s$hidden_decay)lkt_sequence_advance(net,torch::torch_cat(list(x,answer),dim=3),hidden,s)[[2]] else inferred$hidden
        state$hidden[[key]] <- lkt_sequence_state_map(next_hidden,s,'pack')
        if(lkt_gru_has_context(s))state$inference_context[[key]] <-
          lapply(inferred$context,function(t)if(is.character(t))t else torch::as_array(t$detach()$cpu()))
        if(s$use_lkt_features) {
          if(is.null(state$history))state$history <- list()
          state$history[[subject]] <- combined
        }
        if(!is.null(s$order_col))state$last_order[[subject]] <- as.numeric(row[[s$order_col]])
        if(!is.null(s$time_col))state$last_time[[key]] <- lkt_gru_time(row,s)
        if(lkt_gru_clocked(s))state$last_decay_time[[key]] <- if(lkt_gru_gate_active(s))lkt_gru_gate_time(row,s) else if(s$hidden_decay)lkt_gru_hidden_decay_time(row,s) else lkt_gru_plasticity_time(row,s)
        state$pending[[key]] <- NULL
      } else if(identical(row$Outcome,'STUDY')) {
        if(s$use_lkt_features)state$history[[subject]] <- combined
        if(!is.null(s$order_col))state$last_order[[subject]] <- as.numeric(row[[s$order_col]])
      } else state$pending[[key]] <- row
    }
    list(predictions=output[order(ord)],state=state)
  }
  # Restore prediction order before returning the updated state. Callers can align
  # probabilities with their submitted rows even when chronological sorting was
  # required.
  if(lkt_gru_adaptive(s))torch::with_enable_grad(execute()) else torch::with_no_grad(execute())
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
LKTGRUScore <- function(modelob,state,data,runtime=NULL,method=c('reference','batched'),
                        candidate_batch_size=256L) {
  lkt_gru_model(modelob);LKTSequenceScore(modelob,state,data,runtime,method,candidate_batch_size)
}
#' @rdname LKTGRUState
#' @export
LKTGRUStep <- function(modelob,state,data,runtime=NULL) {
  lkt_gru_model(modelob);LKTSequenceStep(modelob,state,data,runtime)
}
