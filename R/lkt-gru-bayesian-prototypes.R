# Persistent latent learner types for output-only adaptation. All types share the
# unchanged recurrent/retrieval/head computation; only their correction vectors
# and causal posterior differ. No additional input or student-ID parameter exists.
lkt_gru_bayesian_prototypes_module <- function() {
  self <- NULL
  definition <- torch::nn_module(initialize=function() {
    self$eta <- torch::nn_parameter(torch::torch_zeros(1L))
    self$log_rate_multipliers <- torch::nn_parameter(torch::torch_tensor(
      seq(log(.25),log(4),length.out=20L),dtype=torch::torch_float64()))
  })
  definition()
}

lkt_gru_validate_bayesian_prototype_weights <- function(weights) {
  eta <- weights[['plasticity.eta']]
  d <- weights[['plasticity.log_rate_multipliers']]
  if(!is.numeric(eta) || length(eta)!=1L || !is.finite(eta) || eta<0 ||
     !is.numeric(d) || length(d)!=20L || any(!is.finite(d)))
    stop('Invalid saved Bayesian prototype rate parameters.',call.=FALSE)
  invisible(NULL)
}

.lkt_gru_bayesian_prototype_code <- new.env(parent=emptyenv())
lkt_gru_bayesian_prototype_scan <- function() {
  if(!exists('scan',.lkt_gru_bayesian_prototype_code,inherits=FALSE)) {
    .lkt_gru_bayesian_prototype_code$scan <- torch::jit_compile('
def scan(base: Tensor, z: Tensor, answers: Tensor, valid: Tensor, v: Tensor,
         logw: Tensor, eta: Tensor, d: Tensor) -> Tuple[Tensor, Tensor, Tensor]:
    logits: List[Tensor] = []
    rates = (eta * torch.exp(d)).unsqueeze(0).unsqueeze(2)
    for t in range(z.size(1)):
        mask = valid[:, t]
        observed = mask & torch.isfinite(answers[:, t])
        features = torch.where(mask.unsqueeze(1), z[:, t, :], torch.zeros_like(z[:, t, :]))
        component = base[:, t].unsqueeze(1) + (v * features.unsqueeze(1)).sum(2)
        logp = -torch.logaddexp(torch.zeros_like(component), -component)
        logq = -torch.logaddexp(torch.zeros_like(component), component)
        # Centering at a component is an algebraic identity, not a special case.
        # Equal components produce exactly their common logit; small rounding in
        # the posterior normalizer cannot perturb baseline logits at eta=0.
        anchor = component[:, :1]
        lp0 = logp[:, :1]
        lq0 = logq[:, :1]
        marginal = anchor + (torch.logsumexp(logw + (logp - lp0), 1, True) - torch.logsumexp(logw + (logq - lq0), 1, True))
        logits.append(marginal)
        answer = torch.where(observed, answers[:, t], torch.zeros_like(answers[:, t])).unsqueeze(1)
        loglik = answer * logp + (1.0 - answer) * logq
        after = logw + loglik
        after = after - torch.logsumexp(after, 1, True)
        logw = torch.where(observed.unsqueeze(1), after, logw)
        error = torch.where(observed.unsqueeze(1), answer - torch.sigmoid(component), torch.zeros_like(component))
        v = torch.where(observed.unsqueeze(1).unsqueeze(2), v + rates * error.unsqueeze(2) * features.unsqueeze(1), v)
    return torch.cat(logits, 1), v, logw
')$scan
  }
  .lkt_gru_bayesian_prototype_code$scan
}

lkt_gru_apply_bayesian_prototypes <- function(net,out,answers,context,s,valid=NULL) {
  z <- out$plastic_features
  if(is.null(z))stop('Bayesian prototypes require final-head activations.',call.=FALSE)
  batch <- z$size(1L);steps <- z$size(2L);width <- z$size(3L)
  if(is.null(valid))valid <- matrix(TRUE,batch,steps)
  lengths <- rowSums(valid)
  if(!identical(as.integer(dim(valid)),c(as.integer(batch),as.integer(steps))) ||
     anyNA(valid) || any(lengths<1L) || any(valid!=outer(lengths,seq_len(steps),`>=`)))
    stop('Bayesian prototypes require nonempty histories with trailing padding.',call.=FALSE)
  v <- context$prototype_weights;logw <- context$prototype_log_weights
  if(xor(is.null(v),is.null(logw)))stop('Incomplete Bayesian prototype state.',call.=FALSE)
  if(is.null(v)) {
    v <- torch::torch_zeros(c(batch,20L,width),dtype=z$dtype,device=z$device)
    logw <- torch::torch_full(c(batch,20L),-log(20),dtype=z$dtype,device=z$device)
  } else {
    if(!identical(as.integer(v$size()),c(as.integer(batch),20L,as.integer(width))) ||
       !identical(as.integer(logw$size()),c(as.integer(batch),1L,20L)))
      stop('Invalid Bayesian prototype state dimensions.',call.=FALSE)
    logw <- logw$squeeze(2)
  }
  if(!torch::torch_isfinite(v)$all()$item() || !torch::torch_isfinite(logw)$all()$item())
    stop('Bayesian prototype state must be finite.',call.=FALSE)
  q <- lkt_gru_bayesian_prototype_scan()(out$logits,z,answers,
    torch::torch_tensor(valid,dtype=torch::torch_bool(),device=z$device),
    v,logw,net$plasticity$eta,net$plasticity$log_rate_multipliers)
  if(any(!vapply(q,function(x)torch::torch_isfinite(x)$all()$item(),logical(1))))
    stop('Bayesian prototype dynamics became nonfinite.',call.=FALSE)
  out$logits <- q[[1L]];out$plastic_features <- NULL
  out$context$prototype_weights <- q[[2L]]
  out$context$prototype_log_weights <- q[[3L]]$unsqueeze(2)
  out$plastic_stats <- torch::torch_zeros(c(batch,3L),dtype=z$dtype,device=z$device)
  out
}
