# Independent candidate queries. All caches below live for one call only; no
# student data or tensors are attached to the shared process-local runtime.
lkt_gru_candidate_support <- function(s,recipe) {
  if(lkt_sequence_kind(s)!='gru' || s$inference_mode!='none' || s$hidden_decay ||
     lkt_gru_gate_active(s) || lkt_gru_connection_fatigue(s) ||
     lkt_gru_semantic_active(s) || lkt_gru_denoising_active(s) ||
     !s$plasticity%in%c('none','constant'))
    stop('Batched candidate scoring supports plain GRUs with none/constant output plasticity, without experimental inference or state mechanisms.',call.=FALSE)
  if(s$use_lkt_features && (any(!recipe$features%in%c('logitdec','ppes','lineafm')) ||
     isTRUE(recipe$interc) || any(!is.na(recipe$interacts)) ||
     any(!recipe$connectors%in%'+') || any(grepl('[%?]',recipe$components)) ||
     length(recipe$fixedpars)!=sum(lkt_feature_param_counts(recipe$features)) ||
     anyNA(recipe$fixedpars)))
    stop('Batched engineered inputs require additive logitdec/ppes/lineafm terms with fixed parameters and no identity intercepts or interactions.',call.=FALSE)
}

# Evaluate the existing feature kernels on disjoint component histories. Each
# component/time query gets its own temporary history key, so pending candidates
# cannot count as earlier exposures. Identical queries share the computed result.
# This visits relevant component histories rather than replaying the student's
# entire history and rebuilding every feature once per candidate.
lkt_gru_candidate_engineered <- function(past,rows,recipe,prep) {
  answer <- matrix(0,nrow(rows),length(recipe$features))
  offset <- 0L
  for(j in seq_along(recipe$features)) {
    component <- recipe$components[j];feature <- recipe$features[j]
    count <- lkt_feature_param_counts(feature)
    pars <- recipe$fixedpars[offset+seq_len(count)];offset <- offset+count
    lkt_require_columns(rows,c(component,'CF..Time.'),'GRU candidate features')
    query <- lkt_gru_keys(rows,c(component,if(feature=='ppes')'CF..Time.'))
    unique_query <- unique(query);pieces <- vector('list',length(unique_query))
    ends <- integer(length(pieces));total <- 0L
    for(k in seq_along(unique_query)) {
      i <- match(unique_query[k],query);row <- rows[i,,drop=FALSE]
      relevant <- if(is.null(past))NULL else past[as.character(past[[component]])==as.character(row[[component]]),,drop=FALSE]
      part <- lkt_gru_bind(relevant,row)
      # Neither kernel reads the label's meaning. Give branches disjoint exact
      # identities while retaining the original timestamps and observed answers.
      part$Anon.Student.Id <- rep(as.character(k),nrow(part))
      part[[component]] <- rep(as.character(k),nrow(part))
      pieces[[k]] <- part;total <- total+nrow(part);ends[k] <- total
    }
    combined <- do.call(rbind,pieces)
    single <- recipe;single$components <- component;single$features <- feature
    single$fixedpars <- pars;single$connectors <- character();single$interacts <- NA_character_
    single$subject_col <- 'Anon.Student.Id';single$interc <- FALSE
    frame <- lkt_gru_feature_frame(combined,single)
    column <- lkt_feature_column_name(feature,component)
    answer[,j] <- frame$data[[column]][ends][match(query,unique_query)]
  }
  colnames(answer) <- paste0('lkt:',vapply(seq_along(recipe$features),function(j)
    lkt_feature_column_name(recipe$features[j],recipe$components[j]),character(1)))
  if(anyDuplicated(colnames(answer)) || !setequal(colnames(answer),paste0('lkt:',prep$engineered$columns)))
    stop('Batched engineered predictor schema differs from the fitted recipe.',call.=FALSE)
  answer
}

lkt_gru_candidate_encode <- function(rows,past,model,state) {
  s <- model$settings
  # Encode raw numeric/time/category inputs together. Candidate gaps branch from
  # the last committed event, never from the previous candidate in this table.
  input <- s;input$use_lkt_features <- FALSE;input$numeric_feature_cols <- NULL
  input$time_col <- NULL;input$plasticity <- 'none'
  numeric <- lkt_gru_numeric(rows,model$recipe,input)$matrix
  if(!is.null(s$time_col)) {
    times <- lkt_gru_time(rows,s);keys <- lkt_gru_keys(rows,s$history_group_cols)
    gaps <- vapply(seq_len(nrow(rows)),function(i) {
      old <- state$last_time[[keys[i]]]
      if(is.null(old))0 else times[i]-old
    },numeric(1))
    if(any(gaps<0))stop('GRU timestamps go backwards within a history.',call.=FALSE)
    numeric <- cbind(numeric,'time:log1p_gap_seconds'=log1p(gaps))
  }
  if(s$use_lkt_features)numeric <- cbind(numeric,
    lkt_gru_candidate_engineered(past,rows,model$recipe,model$preprocessing))
  wanted <- model$preprocessing$numeric_names
  if(!all(wanted%in%colnames(numeric)))stop('Batched GRU numeric predictor schema changed.',call.=FALSE)
  numeric <- numeric[,wanted,drop=FALSE]
  # Categories and raw retrieval strings use the same frozen encoder as Step.
  rawprep <- model$preprocessing
  rawprep$numeric_names <- colnames(lkt_gru_numeric(rows,model$recipe,input)$matrix)
  encoded <- lkt_gru_encode(rows,rawprep,model$recipe,input)
  encoded$numeric <- numeric
  encoded
}

lkt_gru_score_candidates <- function(modelob,model,state,rows,runtime,chunk_size) {
  s <- model$settings;lkt_gru_candidate_support(s,model$recipe)
  if(length(chunk_size)!=1L || !is.numeric(chunk_size) || !is.finite(chunk_size) ||
     chunk_size<1 || chunk_size!=floor(chunk_size))stop('candidate_batch_size must be a positive integer.',call.=FALSE)
  state_class <- if(modelob$model_name=='GRU')'LKTGRUState' else 'LKTSequenceState'
  if(!inherits(state,state_class)||!identical(state$schema,1L)||!identical(state$model_id,model$id))
    stop('GRU state must belong to this fitted model.',call.=FALSE)
  if(lkt_gru_clocked(s) && (!is.list(state$last_decay_time)||!setequal(names(state$hidden),names(state$last_decay_time))))
    stop('Hidden decay state must retain a committed timestamp for every hidden history.',call.=FALSE)
  if(lkt_gru_adaptive(s))lkt_gru_adaptive_validate_state(state$adaptive_embeddings,s)
  cleanup <- lkt_gru_runtime(s);on.exit(cleanup(),add=TRUE)
  net <- lkt_gru_runtime_network(modelob,model,runtime);net$eval()
  rows$pred <- NULL;lkt_gru_order(rows,s)
  keys <- lkt_gru_keys(rows,s$history_group_cols)
  # Apply the same pending and ordering restrictions as the continuation API.
  for(i in seq_len(nrow(rows))) {
    row <- rows[i,,drop=FALSE];key <- keys[i];subject <- as.character(row[[s$subject_col]])
    pending <- state$pending[[key]]
    if(!is.null(pending)) {
      compare <- setdiff(union(names(row),names(pending)),c('CF..ansbin.','Outcome',s$weight_col,'fold'))
      if(!all(compare%in%intersect(names(row),names(pending))) ||
         !isTRUE(all.equal(row[compare],pending[compare],check.attributes=FALSE)))
        stop('Supply the answer to the pending GRU trial before advancing its history.',call.=FALSE)
    }
    if(!is.null(s$order_col)&&!is.null(state$last_order[[subject]])&&as.numeric(row[[s$order_col]])<state$last_order[[subject]])
      stop('GRU continuation order goes backwards.',call.=FALSE)
    if(lkt_gru_clocked(s)) {
      stamp <- lkt_gru_plasticity_time(row,s);prior <- state$last_decay_time[[key]]
      if(!is.null(prior)&&(!is.numeric(prior)||length(prior)!=1L||!is.finite(prior)||stamp<prior))
        stop('Plasticity requires finite chronological timestamps.',call.=FALSE)
    }
  }
  output <- numeric(nrow(rows))
  torch::with_no_grad({
    # A group has one retained neural state and one student's feature history.
    for(indices in split(seq_len(nrow(rows)),keys)) {
      key <- keys[indices[1L]];subject <- as.character(rows[[s$subject_col]][indices[1L]])
      d <- rows[indices,,drop=FALSE];past <- state$history[[subject]]
      if(s$use_lkt_features && !is.null(past) && 'CF..Time.'%in%names(d) &&
         any(d$CF..Time.<utils::tail(past$CF..Time.,1L)))stop('Engineered GRU timestamps go backwards.',call.=FALSE)
      encoded <- lkt_gru_candidate_encode(d,past,model,state)
      x <- lkt_gru_tensor(array(lkt_gru_numeric_rows(encoded,seq_len(nrow(d))),c(nrow(d),1L,length(model$preprocessing$numeric_names))),s)
      prior <- if(is.null(state$hidden[[key]]))lkt_sequence_read(lkt_sequence_initial(s,1L,x),s) else
        lkt_sequence_read(lkt_sequence_state_map(state$hidden[[key]],s,'unpack'),s)
      context <- state$inference_context[[key]]
      if(lkt_gru_plastic(s)&&xor(is.null(context$plastic_weights),is.null(context$plastic_rates)))
        stop('Incomplete plastic connection state.',call.=FALSE)
      if(!is.null(context))context <- lapply(context,function(v)if(is.character(v))v else lkt_gru_tensor(v,s))
      bank <- if(s$retrieval_heads>0L)net$retrieval$prepare_candidates(context) else NULL
      local <- state$adaptive_embeddings$students[[subject]]
      cats <- lkt_gru_tensor(array(encoded$categorical,c(nrow(d),1L,ncol(encoded$categorical))),s,TRUE)
      adaptive_values <- if(lkt_gru_adaptive(s))lkt_gru_adaptive_values(
        rep(list(if(is.null(local))list() else local),nrow(d)),d,s,requires_grad=FALSE) else NULL
      parts <- lkt_gru_categorical_parts(net,cats,model$preprocessing,s,adaptive_values,x$dtype)
      # Exact equal effective queries must remain exact ties, even when BLAS
      # uses different kernels for a batch's last row. Unknown labels retain
      # separate state identities; only equal *read-only computations* are reused.
      signature <- do.call(cbind,c(list(lkt_gru_numeric_rows(encoded,seq_len(nrow(d)))),
        lapply(parts,function(t)matrix(torch::as_array(t$cpu()),nrow(d)))))
      if(s$retrieval_heads>0L && !is.null(bank) && length(s$retrieval_filter_cols)) {
        ids <- array(encoded$retrieval_ids,c(nrow(d),1L,length(s$retrieval_filter_cols)))
        signature <- cbind(signature,lkt_gru_candidate_eligible(ids,bank$ids))
      }
      queries <- lkt_gru_candidate_unique(signature)
      probabilities <- numeric(length(queries$indices))
      for(start in seq.int(1L,length(queries$indices),by=chunk_size)) {
        positions <- seq.int(start,min(length(queries$indices),start+chunk_size-1L))
        pick <- queries$indices[positions];count <- length(pick)
        cat <- cats[pick,,,drop=FALSE]
        values <- stats::setNames(lapply(parts,function(t)t[pick,,,drop=FALSE]),as.character(seq_along(parts)))
        ids <- if(length(s$retrieval_filter_cols))array(encoded$retrieval_ids[pick,,drop=FALSE],c(count,1L,length(s$retrieval_filter_cols))) else NULL
        logit <- net$score_candidates(x[pick,,,drop=FALSE],cat,prior,bank,ids,values,context$plastic_weights)
        probability <- as.numeric(torch::as_array(torch::torch_sigmoid(logit)$cpu()))
        if(any(!is.finite(probability)))stop('Batched candidate prediction became nonfinite.',call.=FALSE)
        probabilities[positions] <- pmin(pmax(probability,s$clip_epsilon),1-s$clip_epsilon)
      }
      output[indices] <- probabilities[queries$map]
    }
  })
  output
}

lkt_gru_candidate_unique <- function(signature) {
  if(!requireNamespace('digest',quietly=TRUE))stop('Batched candidate scoring requires digest.',call.=FALSE)
  keys <- vapply(seq_len(nrow(signature)),function(i)
    digest::digest(unname(signature[i,]),algo='sha256'),character(1))
  indices <- which(!duplicated(keys));map <- match(keys,keys[indices])
  # Verify actual values too: a hash is an index, never permission to merge
  # different effective inputs. This also avoids numeric rounding in joins.
  if(any(signature!=signature[indices[map],,drop=FALSE]))stop('Candidate query identity collision.',call.=FALSE)
  list(indices=indices,map=map)
}
