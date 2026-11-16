# Opt-in, student-local embeddings. No fitted tensor or runtime binding is mutated.
lkt_gru_adaptive <- function(s)identical(s$embedding_adaptation,'per_student')

lkt_gru_validate_adaptation <- function(s) {
  if(!is.character(s$embedding_adaptation) || length(s$embedding_adaptation)!=1L ||
     is.na(s$embedding_adaptation) || !s$embedding_adaptation%in%c('none','per_student'))
    stop('embedding_adaptation must be none or per_student.',call.=FALSE)
  optional <- s$optional_adaptive_cols
  if(!is.character(optional)||anyNA(optional)||anyDuplicated(optional)||any(!nzchar(optional))||
     any(!optional%in%s$adaptive_embedding_cols)||(!lkt_gru_adaptive(s)&&length(optional)))
    stop('optional_adaptive_cols must be distinct adaptive columns in per_student mode.',call.=FALSE)
  if(!lkt_gru_adaptive(s))return(invisible(NULL))
  cols <- s$adaptive_embedding_cols
  if(!is.character(cols)||!length(cols)||anyNA(cols)||anyDuplicated(cols)||
     any(!nzchar(cols))||!all(cols%in%s$categorical_cols)||s$categorical_encoding!='embedding')
    stop('Adaptive columns must name configured categorical embedding inputs.',call.=FALSE)
  rate <- s$embedding_learning_rate
  if(!is.numeric(rate)||length(rate)!=1L||!is.finite(rate)||rate<=0)
    stop('embedding_learning_rate must be finite and positive.',call.=FALSE)
  if(!identical(s$history_group_cols,s$subject_col)||s$subject_col%in%s$categorical_cols)
    stop('Adaptive embeddings require one history per student and no student identity embedding.',call.=FALSE)
  if(s$inference_mode!='none'||isTRUE(s$hidden_decay)||!s$plasticity%in%c('none','constant')||isTRUE(s$gate_fatigue)||
     !is.null(s$initial_model_file))
    stop('Adaptive embeddings support plain GRU inference with none/constant output plasticity, without other experimental adaptation or warm starts.',call.=FALSE)
}

lkt_gru_adaptive_widths <- function(s) {
  d <- s$embedding_dims
  stats::setNames(if(is.null(names(d)))rep(d,length(s$adaptive_embedding_cols)) else
    unname(d[s$adaptive_embedding_cols]),s$adaptive_embedding_cols)
}

lkt_gru_adaptive_state <- function()list(schema=1L,students=list())

lkt_gru_optional_inputs <- function(data,s) {
  if(length(s$optional_adaptive_cols))data <- as.data.frame(data)
  for(col in s$optional_adaptive_cols)if(!col%in%names(data))data[[col]] <- rep(NA_character_,nrow(data))
  data
}
lkt_gru_optional_recipe <- function(recipe,s) {
  used <- c(recipe$components,s$history_group_cols,s$subject_col,s$numeric_cols,
    s$time_col,s$order_col,s$time_feature_cols)
  if(any(s$optional_adaptive_cols%in%used))
    stop('Optional identities cannot also be required history, numeric, time or grouping inputs.',call.=FALSE)
  invisible(TRUE)
}
lkt_gru_adaptive_id <- function(rows,col,i,s) {
  id <- as.character(rows[[col]][i])
  if(!length(id)||is.na(id)) {
    if(col%in%s$optional_adaptive_cols)return(NULL)
    stop('Missing required adaptive identity: ',col,call.=FALSE)
  }
  if(!nzchar(id))stop('Adaptive identity labels cannot be empty.',call.=FALSE)
  id
}
lkt_gru_adaptive_validate_state <- function(x,s) {
  dictionary <- function(value)is.list(value) && (!length(value) ||
    (!is.null(names(value)) && !anyNA(names(value)) &&
     all(nzchar(names(value))) && !anyDuplicated(names(value))))
  if(!is.list(x)||!identical(x$schema,1L)||!dictionary(x$students))
    stop('Missing or incompatible adaptive embedding state.',call.=FALSE)
  widths <- lkt_gru_adaptive_widths(s)
  for(student in x$students) {
    if(!dictionary(student)||any(!names(student)%in%names(widths)))stop('Invalid adaptive student state.',call.=FALSE)
    for(col in names(student)) {
      if(!dictionary(student[[col]]))stop('Invalid adaptive identity dictionary.',call.=FALSE)
      for(value in student[[col]])
        if(!is.numeric(value)||length(value)!=widths[[col]]||any(!is.finite(value)))
          stop('Invalid adaptive embedding vector.',call.=FALSE)
    }
  }
  invisible(TRUE)
}

# Each batch row belongs to a different student. IDs are strings, never vocabulary
# indices; unseen IDs therefore remain distinct. Reads do not allocate state.
lkt_gru_adaptive_values <- function(local,rows,s,requires_grad=TRUE) {
  widths <- lkt_gru_adaptive_widths(s)
  values <- lapply(names(widths),function(col) {
    vectors <- lapply(seq_along(local),function(i) {
      id <- lkt_gru_adaptive_id(rows,col,i,s)
      v <- if(is.null(id))NULL else local[[i]][[col]][[id]]
      if(is.null(v))rep(0,widths[[col]]) else v
    })
    t <- lkt_gru_tensor(array(unlist(vectors),c(widths[[col]],1L,length(local))),s)$permute(c(3L,2L,1L))$contiguous()
    t$requires_grad_(requires_grad)
    t
  })
  names(values) <- as.character(match(names(widths),s$categorical_cols))
  values
}

# One shared post-answer update used by training, replay and continuation.
# autograd_grad leaves shared parameter .grad fields untouched.
lkt_gru_adaptive_update <- function(local,rows,values,loss,s,observed,retain_graph=FALSE) {
  gradients <- torch::autograd_grad(loss,unname(values),retain_graph=retain_graph,create_graph=FALSE)
  gradients <- lapply(gradients,function(x)torch::as_array(x$detach()$cpu()))
  old <- lapply(values,function(x)torch::as_array(x$detach()$cpu()))
  for(i in which(observed)) {
    ids <- lapply(names(values),function(j)lkt_gru_adaptive_id(rows,s$categorical_cols[as.integer(j)],i,s))
    present <- which(!vapply(ids,is.null,logical(1)))
    if(!length(present))next
    g <- unlist(lapply(gradients[present],function(x)as.numeric(x[i,1,])))
    norm <- if(is.infinite(s$gradient_clip_norm))max(abs(g)) else sum(abs(g)^s$gradient_clip_norm)^(1/s$gradient_clip_norm)
    scale <- if(is.null(s$gradient_clip))1 else min(1,s$gradient_clip/(norm+1e-6))
    for(j in present) {
      col <- s$categorical_cols[as.integer(names(values)[j])];id <- ids[[j]]
      v <- as.numeric(old[[j]][i,1,])-s$embedding_learning_rate*scale*as.numeric(gradients[[j]][i,1,])
      if(any(!is.finite(v)))stop('Adaptive embedding update produced nonfinite values.',call.=FALSE)
      local[[i]][[col]][[id]] <- v
    }
  }
  local
}

#' Allow explicitly unmarked identities in an existing adaptive GRU
#' @param modelob Fitted per-student GRU. Shared weights are retained exactly.
#' @param columns Adaptive categorical columns allowed to be absent or NA.
#' @return A derived fitted model with an explicit versioned identity-policy
#'   extension and a new continuation identity. The supplied model is unchanged.
#' @export
LKTGRUOptionalIdentities <- function(modelob,columns) {
  model <- lkt_gru_model(modelob)
  if(!lkt_gru_adaptive(model$settings))stop('Optional identities require an already adaptive GRU.',call.=FALSE)
  parent <- model$id;model$settings$optional_adaptive_cols <- columns
  lkt_gru_validate_adaptation(model$settings);lkt_gru_optional_recipe(model$recipe,model$settings)
  model$optional_identity_extension <- list(schema=1L,parent_model_id=parent,columns=columns)
  model$id <- paste0('lkt-gru-optional-',digest::digest(model,algo='sha256'))
  result <- modelob;result$model <- model;result$gru_settings <- model$settings
  result
}

lkt_gru_adaptive_forward <- function(net,numeric,categorical,answers,hidden,context,ids,values,s,
                                     gaps=NULL,valid=NULL) {
  out <- if(s$feature_encoder!='none'||s$retrieval_heads>0L)
    net(numeric,categorical,answers,hidden,context,ids,embedding_values=values)
  else net(numeric,categorical,answers,hidden,embedding_values=values)
  if(lkt_gru_plastic(s))out <- lkt_gru_apply_plasticity(net,out,answers,context,s,gaps,valid,
    categorical,allow_empty=TRUE)
  out
}

lkt_gru_adaptive_loss <- function(logits,answers,s,mask=NULL) {
  loss <- torch::nnf_binary_cross_entropy_with_logits(logits,answers,reduction='none',
    pos_weight=lkt_gru_tensor(s$positive_weight,s))
  if(!is.null(mask))loss <- loss*mask
  loss$sum()
}

lkt_gru_adaptive_batch <- function(net,b,data,s,hidden=NULL,context=NULL,local=NULL,training=FALSE) {
  count <- nrow(b$rows);steps <- ncol(b$rows)
  if(is.null(local))local <- rep(list(list()),count)
  logits <- vector('list',steps)
  for(t in seq_len(steps)) {
    observed <- !is.na(b$rows[,t]);idx <- b$rows[,t]
    # Padding never commits an identity or gradient. Borrow a valid row solely
    # for tensor construction; its zero mask excludes it from both optimizers.
    idx[!observed] <- b$rows[!observed,1L]
    rows <- data[idx,,drop=FALSE]
    values <- lkt_gru_adaptive_values(local,rows,s)
    out <- lkt_gru_adaptive_forward(net,b$numeric[,t,,drop=FALSE],b$categorical[,t,,drop=FALSE],
      b$answers[,t,drop=FALSE],hidden,context,
      if(is.null(b$retrieval_ids))NULL else b$retrieval_ids[,t,,drop=FALSE],values,s,
      if(is.null(b$decay_gaps))NULL else b$decay_gaps[,t,drop=FALSE],matrix(observed,count,1L))
    loss <- lkt_gru_adaptive_loss(out$logits,b$answers[,t,drop=FALSE],s,
      b$mask[,t,drop=FALSE]*b$weights[,t,drop=FALSE])
    local <- lkt_gru_adaptive_update(local,rows,values,loss,s,observed,retain_graph=training)
    hidden <- out$hidden;context <- out$context
    logits[[t]] <- out$logits
    if(!training) {
      hidden <- lkt_sequence_state_map(hidden,s,'detach')
      context <- lkt_gru_context_subset(context,detach=TRUE)
      logits[[t]] <- logits[[t]]$detach()
    }
  }
  list(logits=torch::torch_cat(logits,dim=2),hidden=hidden,context=context,adaptive=local)
}

lkt_gru_adaptive_replay <- function(net,encoded,data,s) {
  net$eval();pred <- rep(NA_real_,nrow(data))
  groups <- split(seq_len(nrow(data)),lkt_gru_keys(data,s$history_group_cols))
  for(start in seq.int(1L,length(groups),by=s$batch_size)) {
    g <- groups[seq.int(start,min(length(groups),start+s$batch_size-1L))]
    longest <- max(lengths(g));chunk <- if(is.null(s$sequence_length))longest else s$sequence_length
    hidden <- context <- NULL;local <- rep(list(list()),length(g))
    for(offset in seq.int(1L,longest,by=chunk)) {
      active <- which(lengths(g)>=offset)
      hidden <- lkt_sequence_state_map(hidden,s,'subset',active)
      context <- lkt_gru_context_subset(context,active);local <- local[active];g <- g[active]
      positions <- lapply(g,function(idx)seq.int(offset,min(length(idx),offset+chunk-1L)))
      b <- lkt_gru_batch(encoded,data,g,positions,s)
      out <- lkt_gru_adaptive_batch(net,b,as.data.frame(data),s,hidden,context,local)
      hidden <- out$hidden;context <- out$context;local <- out$adaptive
      p <- torch::as_array(torch::torch_sigmoid(out$logits)$cpu())
      keep <- !is.na(b$rows);pred[b$rows[keep]] <- p[keep]
    }
  }
  pmin(pmax(pred,s$clip_epsilon),1-s$clip_epsilon)
}
