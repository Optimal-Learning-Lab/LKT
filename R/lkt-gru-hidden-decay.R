# Hidden-unit decay is student state, not optimizer weight decay. The three
# trainable scalars are shared by every layer/unit; h and DK belong to histories.
lkt_gru_hidden_decay_module <- function(parameterization='softplus') {
  self <- NULL
  definition <- torch::nn_module(
    initialize=function() {
      self$a <- torch::nn_parameter(torch::torch_zeros(1L))
      self$b <- torch::nn_parameter(torch::torch_zeros(1L))
      self$c <- torch::nn_parameter(torch::torch_zeros(1L))
    },
    forward=function(state,days) {
      faded <- torch::torch_exp(-torch::nnf_softplus(self$c)*days)*state$dk
      rate <- if(parameterization=='amplitude')self$a*torch::torch_exp(self$b*faded) else
        torch::nnf_softplus(self$a+self$b*faded)
      h <- torch::torch_exp(-rate*days)*state$h
      # The current interval's decay change affects only subsequent rates.
      list(h=h,dk=faded+(h-state$h))
    })
  definition()
}

lkt_gru_hidden_decay_time <- function(data,s) {
  lkt_gru_time(data,list(time_col=s$hidden_decay_time_col,time_units=s$time_units))
}

# The decay clock never enters the numeric predictor schema. First observations
# have no preceding interval. Continuation supplies only committed timestamps.
lkt_gru_hidden_decay_gaps <- function(data,s,previous=NULL) {
  time <- lkt_gru_hidden_decay_time(data,s)
  lkt_gru_response_gaps(data,time,s,previous)/86400
}

# Concatenate complete states through the adapter layout rather than exposing
# hidden axes in the shared training/replay loops.
lkt_sequence_state_cat <- function(states,s) {
  walk <- function(nodes,layout) {
    if(is.numeric(layout))return(torch::torch_cat(nodes,dim=layout))
    stats::setNames(lapply(names(layout),function(n)
      walk(lapply(nodes,`[[`,n),layout[[n]])),names(layout))
  }
  walk(states,lkt_sequence_layout(s))
}

# Restore batch positions for logits and context after dropping finished rows.
# Missing rows are padding. Their context is discarded before the next chunk;
# their actual final hidden/DK states are separately retained below.
lkt_gru_expand_batch <- function(x,active,batch) {
  if(is.null(x))return(NULL)
  if(is.character(x)) {
    shape <- dim(x);shape[1L] <- batch
    out <- array(NA_character_,shape);out[active,,] <- x
    return(out)
  }
  shape <- x$size();shape[1L] <- 1L
  pieces <- lapply(seq_len(batch),function(j) {
    i <- match(j,active)
    if(is.na(i))return(torch::torch_zeros(shape,dtype=x$dtype,device=x$device))
    index <- rep(list(quote(expr=)),length(shape));index[[1L]] <- i
    do.call(`[`,c(list(x),index,list(drop=FALSE)))
  })
  torch::torch_cat(pieces,dim=1L)
}

# One eventwise path is used by fitting, replay, Step and Score. Existing network
# forwards remain responsible for causal prediction, retrieval, and GRU updates.
# Disabled models call the historical fused forward without changing RNG draws.
lkt_gru_forward <- function(net,numeric,categorical,answers,hidden,context,
                            retrieval_ids,s,decay_gaps=NULL,valid=NULL,semantic=NULL) {
  call_net <- function(numeric,categorical,answers,hidden,context,ids) {
    if(lkt_gru_semantic_active(s))return(net(numeric,categorical,answers,hidden,context,ids,semantic=semantic))
    if(s$inference_mode=='none' && s$retrieval_heads==0L)
      net(numeric,categorical,answers,hidden)
    else net(numeric,categorical,answers,hidden,context,ids)
  }
  if(lkt_gru_gate_active(s)) {
    if(is.null(decay_gaps))stop('Gate fatigue requires elapsed-time gaps.',call.=FALSE)
    if(is.null(valid))valid <- matrix(TRUE,numeric$size(1L),numeric$size(2L))
    lengths <- rowSums(valid)
    if(any(lengths==0L) || any(valid!=outer(lengths,seq_len(numeric$size(2L)),`>=`)))
      stop('Gate fatigue requires nonempty histories with trailing padding.',call.=FALSE)
    mask <- torch::torch_tensor(valid,dtype=torch::torch_bool(),device=s$device)
    out <- if(s$inference_mode=='none' && s$retrieval_heads==0L)
      net(numeric,categorical,answers,hidden,decay_gaps,mask) else
      net(numeric,categorical,answers,hidden,context,retrieval_ids,decay_gaps,mask)
    if(lkt_gru_plastic(s))out <- lkt_gru_apply_plasticity(net,out,answers,context,s,decay_gaps,valid,categorical)
    return(out)
  }
  if(!s$hidden_decay) {
    out <- call_net(numeric,categorical,answers,hidden,context,retrieval_ids)
    if(lkt_gru_plastic(s))out <- lkt_gru_apply_plasticity(net,out,answers,context,s,decay_gaps,valid,categorical)
    return(out)
  }
  batch <- numeric$size(1L);steps <- numeric$size(2L)
  if(is.null(decay_gaps))stop('Hidden decay requires elapsed-time gaps.',call.=FALSE)
  if(is.null(valid))valid <- matrix(TRUE,batch,steps)
  lengths <- rowSums(valid)
  if(any(lengths==0L) || any(valid != outer(lengths,seq_len(steps),`>=`)))
    stop('Hidden decay requires nonempty histories with trailing padding only.',call.=FALSE)
  if(lkt_gru_compiled_decay(s)) {
    mask <- torch::torch_tensor(valid,dtype=torch::torch_bool(),device=s$device)
    if(s$inference_mode=='none' && s$retrieval_heads==0L)
      return(net(numeric,categorical,answers,hidden,decay_gaps,mask))
    return(net(numeric,categorical,answers,hidden,context,retrieval_ids,decay_gaps,mask))
  }
  if(is.null(hidden))hidden <- lkt_sequence_initial(s,batch,numeric)
  logits <- kl <- kl_valid <- vector('list',steps)
  finals <- vector('list',batch);previous_active <- seq_len(batch)
  for(t in seq_len(steps)) {
    active <- which(valid[,t]);indices <- match(active,previous_active)
    hidden <- lkt_sequence_state_map(hidden,s,'subset',indices)
    context <- lkt_gru_context_subset(context,indices)
    hidden <- net$hidden_decay(hidden,decay_gaps[active,t,drop=FALSE]$reshape(c(1L,length(active),1L)))
    ids <- if(is.null(retrieval_ids))NULL else retrieval_ids[active,t,,drop=FALSE]
    out <- call_net(numeric[active,t,,drop=FALSE],categorical[active,t,,drop=FALSE],
      answers[active,t,drop=FALSE],hidden,context,ids)
    logits[[t]] <- lkt_gru_expand_batch(out$logits,active,batch)
    if(s$inference_mode=='gaussian') {
      kl[[t]] <- lkt_gru_expand_batch(out$kl_per_trial,active,batch)
      kl_valid[[t]] <- lkt_gru_expand_batch(out$kl_valid,active,batch)
    }
    hidden <- out$hidden;context <- out$context
    for(j in which(lengths[active]==t))
      finals[[active[j]]] <- lkt_sequence_state_map(hidden,s,'subset',j)
    previous_active <- active
  }
  result <- list(logits=torch::torch_cat(logits,dim=2L),hidden=lkt_sequence_state_cat(finals,s))
  if(!is.null(context))result$context <- lapply(context,lkt_gru_expand_batch,active=active,batch=batch)
  if(s$inference_mode=='gaussian') {
    result$kl_per_trial <- torch::torch_cat(kl,dim=2L)
    result$kl_valid <- torch::torch_cat(kl_valid,dim=2L)
  }
  result
}

lkt_gru_compiled_decay <- function(s)isTRUE(s$hidden_decay) && s$hidden_decay_parameterization=='amplitude'

# Project the one constrained parameter after the ordinary optimizer update.
# A direct amplitude has a useful one-sided derivative at zero, unlike a square
# or ReLU parameterization initialized at zero. No forward threshold is used.
lkt_gru_project_hidden_decay <- function(net,s) {
  if(lkt_gru_compiled_decay(s))torch::with_no_grad({net$hidden_decay$a$clamp_(min=0)})
  invisible(NULL)
}

# Cache executable code only: fitted weights and history state are arguments,
# never caches inside the compiled function. Compilation failure is an error.
.lkt_gru_decay_code <- new.env(parent=emptyenv())
lkt_gru_decay_scan <- function() {
  if(!exists('scan',.lkt_gru_decay_code,inherits=FALSE)) {
    code <- '
def scan(x: Tensor, h: Tensor, dk: Tensor, days: Tensor, valid: Tensor,
         a: Tensor, b: Tensor, c: Tensor, weights: List[Tensor],
         layers: int, bias: bool, dropout: float, training: bool,
         all_inputs: bool) -> Tuple[Tensor, Tensor, Tensor, Tensor]:
    posts: List[Tensor] = []
    priors: List[Tensor] = []
    rho = torch.softplus(c)
    for t in range(x.size(1)):
        mask = valid[:, t].reshape(1, x.size(0), 1)
        dt = torch.where(valid[:, t], days[:, t], torch.zeros_like(days[:, t])).reshape(1, x.size(0), 1)
        faded = torch.exp(-rho * dt) * dk
        rate = a * torch.exp(b * faded)
        before = torch.exp(-rate * dt) * h
        priors.append(before[layers - 1].unsqueeze(1))
        interaction = x[:, t:t+1, :]
        if all_inputs:
            next_states: List[Tensor] = []
            output = interaction
            stride = 4 if bias else 2
            for layer in range(layers):
                current = interaction if layer == 0 else torch.cat([torch.dropout(output, dropout, training), interaction], 2)
                output, state = torch.gru(current, before[layer:layer+1], weights[layer*stride:(layer+1)*stride], bias, 1, 0.0, training, False, True)
                next_states.append(state)
            after = torch.cat(next_states, 0)
        else:
            output, after = torch.gru(interaction, before, weights, bias, layers, dropout, training, False, True)
        next_dk = faded + (before - h) + (after - before)
        h = torch.where(mask, after, h)
        dk = torch.where(mask, next_dk, dk)
        posts.append(h[layers - 1].unsqueeze(1))
    return torch.cat(posts, 1), torch.cat(priors, 1), h, dk
'
    .lkt_gru_decay_code$scan <- torch::jit_compile(code)$scan
  }
  .lkt_gru_decay_code$scan
}

lkt_gru_compiled_transition <- function(net,interaction,state,s,days,valid) {
  if(is.null(days) || is.null(valid))stop('Compiled hidden decay requires elapsed gaps and a validity mask.',call.=FALSE)
  gs <- lkt_sequence_gru_settings(s);module <- lkt_sequence_module(net,s)
  kinds <- c('weight_ih','weight_hh',if(gs$gru_bias)c('bias_ih','bias_hh'))
  expected <- unlist(lapply(seq_len(s$num_layers),function(layer)
    if(gs$input_to_all_layers && s$num_layers>1L)paste0('layers.',layer-1L,'.',kinds,'_l1') else paste0(kinds,'_l',layer)),use.names=FALSE)
  named <- module$named_parameters()
  if(!identical(names(named),expected))stop('Unexpected compiled GRU weight layout.',call.=FALSE)
  result <- lkt_gru_decay_scan()(interaction,state$h,state$dk,days,valid,
    net$hidden_decay$a,net$hidden_decay$b,net$hidden_decay$c,unname(named),
    torch::jit_scalar(s$num_layers),torch::jit_scalar(gs$gru_bias),torch::jit_scalar(as.numeric(gs$gru_dropout)),
    torch::jit_scalar(net$training),torch::jit_scalar(gs$input_to_all_layers && s$num_layers>1L))
  list(result[[1L]],list(h=result[[3L]],dk=result[[4L]]),prior=result[[2L]])
}
