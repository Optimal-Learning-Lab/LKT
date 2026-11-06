# Read completed interactions without replacing the recurrent update mechanism.
# Keys describe an event before its answer; values contain the top GRU state after
# that answer. Query/key inputs share the layout [prior GRU state, current inputs].
lkt_gru_retrieval_network <- function(event_width,s) {
  self <- NULL
  heads <- s$retrieval_heads;size <- s$retrieval_size;head_width <- size/heads
  definition <- torch::nn_module(
    initialize=function() {
      self$query <- torch::nn_linear(event_width,size,bias=s$retrieval_bias)
      if(!s$retrieval_share_qk)self$key <- torch::nn_linear(event_width,size,bias=s$retrieval_bias)
      self$value <- torch::nn_linear(s$hidden_size,size,bias=s$retrieval_bias)
      self$output <- torch::nn_linear(size,s$hidden_size,bias=s$retrieval_bias)
      self$drop <- torch::nn_dropout(s$retrieval_dropout)
      if(s$retrieval_decay!='none')self$decay <- torch::nn_parameter(
        torch::torch_full(c(heads),if(s$retrieval_decay_init>20)s$retrieval_decay_init else log(expm1(s$retrieval_decay_init))))
    },
    forward=function(events,states,context=NULL,identifiers=NULL) {
      batch <- events$size(1);steps <- events$size(2)
      filtered <- length(s$retrieval_filter_cols)>0L
      # A new history has no retrievable answers. This is an explicit empty-memory
      # condition, not an invented success/failure or a shared memory between students.
      empty <- function(width)torch::torch_zeros(c(batch,0L,width),dtype=events$dtype,device=events$device)
      if(is.null(context)) {
        context <- list(retrieval_keys=empty(event_width),
          retrieval_values=empty(s$hidden_size),retrieval_valid=empty(1L))
        if(filtered)context$retrieval_ids <- array(NA_character_,c(batch,0L,length(s$retrieval_filter_cols)))
      }
      if(!all(c('retrieval_keys','retrieval_values','retrieval_valid') %in% names(context)))
        stop('Retrieval continuation context is incomplete.',call.=FALSE)
      old <- context$retrieval_keys$size(2)
      keys <- torch::torch_cat(list(context$retrieval_keys,events),dim=2)
      values <- torch::torch_cat(list(context$retrieval_values,states),dim=2)
      valid <- torch::torch_cat(list(context$retrieval_valid,
        torch::torch_ones(c(batch,steps,1L),dtype=events$dtype,device=events$device)),dim=2)
      count <- keys$size(2)
      if(filtered) {
        fields <- length(s$retrieval_filter_cols)
        if(!is.character(identifiers) || length(dim(identifiers))!=3L ||
           !all(dim(identifiers)==c(batch,steps,fields)) ||
           !is.character(context$retrieval_ids) || length(dim(context$retrieval_ids))!=3L ||
           !all(dim(context$retrieval_ids)==c(batch,old,fields)))
          stop('Retrieval filter identities are missing or incompatible.',call.=FALSE)
        # Keep exact raw identities beside the bank. Embedding unknown tokens must
        # never make two different unseen items appear to be the same item.
        bank_ids <- array(NA_character_,c(batch,count,fields))
        if(old>0L)bank_ids[,seq_len(old),] <- context$retrieval_ids
        bank_ids[,old+seq_len(steps),] <- identifiers
        eligible <- array(FALSE,c(batch,1L,steps,count))
        for(b in seq_len(batch)) {
          matched <- matrix(FALSE,steps,count)
          for(j in seq_len(fields)) {
            equal <- outer(identifiers[b,,j],bank_ids[b,,j],'==')
            equal[is.na(equal)] <- FALSE
            matched <- matched | equal
          }
          eligible[b,1L,,] <- matched
        }
      }
      split_heads <- function(x,n)x$reshape(c(batch,n,heads,head_width))$permute(c(1,3,2,4))
      query <- split_heads(self$query(events),steps)
      key <- split_heads(if(s$retrieval_share_qk)self$query(keys) else self$key(keys),count)
      value <- split_heads(self$value(values),count)
      scores <- torch::torch_matmul(query,key$transpose(3,4))/sqrt(head_width)
      # Every query may read only completed earlier events, including across BPTT
      # chunks. Window limits are in interactions and are independent of chunk length.
      query_position <- old+seq_len(steps)
      distances <- outer(query_position,seq_len(count),'-')
      allowed <- distances>0
      if(!is.null(s$retrieval_window))allowed <- allowed & distances<=s$retrieval_window
      mask <- torch::torch_tensor(array(allowed,c(1L,1L,steps,count)),dtype=torch::torch_bool(),device=events$device)
      mask <- mask & valid$permute(c(1,3,2))$unsqueeze(2)$to(dtype=torch::torch_bool())
      # Filter before either attention softmax, preserving the original causal
      # 100-event window. With no eligible prior event the read remains exactly zero.
      if(filtered)mask <- mask & torch::torch_tensor(eligible,dtype=torch::torch_bool(),device=events$device)
      distance <- torch::torch_tensor(array(pmax(distances,0),c(1L,1L,steps,count)),dtype=events$dtype,device=events$device)
      if(s$retrieval_decay!='none') {
        rate <- torch::nnf_softplus(self$decay)$reshape(c(1L,heads,1L,1L))
        if(s$retrieval_decay=='exponential')scores <- scores-rate*distance
        else {
          # AKT-style distance uses the preliminary attention mass between events.
          # As in AKT, distance is detached; projections and decay still train from
          # the prediction loss through the final attention scores.
          preliminary <- torch::nnf_softmax(scores$masked_fill(!mask,-1e9),dim=4)*mask$to(dtype=events$dtype)
          contextual <- ((preliminary$sum(dim=4,keepdim=TRUE)-preliminary$cumsum(dim=4))*distance)$clamp(min=0)$sqrt()$detach()
          scores <- scores*torch::torch_exp(-rate*contextual)
        }
      }
      weights <- torch::nnf_softmax(scores$masked_fill(!mask,-1e9),dim=4)*mask$to(dtype=events$dtype)
      weights <- weights/weights$sum(dim=4,keepdim=TRUE)$clamp(min=1e-12)
      read <- torch::torch_matmul(self$drop(weights),value)$permute(c(1,3,2,4))$reshape(c(batch,steps,size))
      has_memory <- mask$any(dim=4)$any(dim=2)$unsqueeze(3)$to(dtype=events$dtype)
      read <- self$output(read)*has_memory
      keep <- if(is.null(s$retrieval_window))count else min(count,s$retrieval_window)
      retained <- seq.int(count-keep+1L,count)
      next_context <- list(retrieval_keys=keys[,retained,,drop=FALSE],
        retrieval_values=values[,retained,,drop=FALSE],retrieval_valid=valid[,retained,,drop=FALSE])
      if(filtered)next_context$retrieval_ids <- bank_ids[,retained,,drop=FALSE]
      list(read=read,weights=weights,context=next_context)
    })
  definition()
}
