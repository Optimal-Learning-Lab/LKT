# Student-specific output connections. Shared network weights remain fixed during
# replay; adjustment and signed rate vectors are carried in each history context.
lkt_gru_plastic <- function(s)!is.null(s$plasticity) && s$plasticity!='none'
lkt_gru_clocked <- function(s)isTRUE(s$hidden_decay) || lkt_gru_plastic(s) || lkt_gru_gate_active(s)
lkt_gru_has_context <- function(s)s$inference_mode!='none' || s$retrieval_heads>0L || lkt_gru_plastic(s)

lkt_gru_plasticity_module <- function(mode,schema=NULL) {
  if(mode=='bayesian_prototypes')return(lkt_gru_bayesian_prototypes_module())
  if(mode=='item_skill')return(lkt_gru_item_skill_module(schema))
  self <- NULL
  definition <- torch::nn_module(initialize=function() {
    self$eta <- torch::nn_parameter(torch::torch_zeros(1L))
    if(mode=='error_rate') {
      self$alpha <- torch::nn_parameter(torch::torch_zeros(1L))
      self$c <- torch::nn_parameter(torch::torch_zeros(1L))
    }
    if(mode=='fatigue') {
      self$kappa <- torch::nn_parameter(torch::torch_ones(1L))
      # rho=log(2) per minute gives an initial one-minute recovery half-life.
      self$log_rho <- torch::nn_parameter(torch::torch_full(1L,log(log(2))))
    }
  })
  definition()
}

lkt_gru_project_plasticity <- function(net,s) {
  if(s$plasticity=='item_skill') {
    torch::with_no_grad({net$plasticity$skill_rate$clamp_(min=0)})
    return(invisible(NULL))
  }
  if(lkt_gru_plastic(s))torch::with_no_grad({
    net$plasticity$eta$clamp_(min=0)
    if(s$plasticity=='fatigue')net$plasticity$kappa$clamp_(min=0)
    if(s$plasticity=='error_rate') {net$plasticity$alpha$clamp_(min=0);net$plasticity$c$clamp_(min=0)}
  })
  invisible(NULL)
}

# Preserve the exact module names, initialization, operations and dropout draws
# of the stock sequential head, while exposing its final layer's input.
lkt_gru_head <- function(net,x,s) {
  if(!lkt_gru_plastic(s))return(list(logits=net$head(x)$squeeze(3)))
  n <- length(net$head)
  if(n>1L)for(i in seq_len(n-1L))x <- net$head[[i]](x)
  list(logits=net$head[[n]](x)$squeeze(3),plastic_features=x)
}

# Read the existing output adjustment without feedback or a next-state update.
lkt_gru_plasticity_read <- function(out,weights,s) {
  if(!lkt_gru_plastic(s) || is.null(weights))return(out$logits)
  out$logits+(weights*out$plastic_features)$sum(dim=3)
}

lkt_gru_plasticity_time <- function(data,s) {
  lkt_gru_time(data,list(time_col=s$plasticity_time_col,time_units=s$time_units))
}
lkt_gru_plasticity_gaps <- function(data,s,previous=NULL) {
  time <- lkt_gru_plasticity_time(data,s)
  lkt_gru_response_gaps(data,time,s,previous)/60
}

.lkt_gru_plasticity_code <- new.env(parent=emptyenv())
lkt_gru_plasticity_scan <- function() {
  if(!exists('scan',.lkt_gru_plasticity_code,inherits=FALSE)) {
    .lkt_gru_plasticity_code$scan <- torch::jit_compile('
def scan(base: Tensor, z: Tensor, answers: Tensor, minutes: Tensor, valid: Tensor,
         v: Tensor, e: Tensor, eta: Tensor, kappa: Tensor, log_rho: Tensor,
         fatigue: bool) -> Tuple[Tensor, Tensor, Tensor, Tensor]:
    logits: List[Tensor] = []
    stats = torch.zeros([z.size(0), 3], dtype=z.dtype, device=z.device)
    rho = torch.exp(log_rho)
    for t in range(z.size(1)):
        mask = valid[:, t].unsqueeze(1)
        dt = torch.where(valid[:, t], minutes[:, t], torch.zeros_like(minutes[:, t])).unsqueeze(1)
        before = eta + (e - eta) * torch.exp(-rho * dt) if fatigue else torch.ones_like(e) * eta
        features = torch.where(mask, z[:, t, :], torch.zeros_like(z[:, t, :]))
        logit = base[:, t] + (v * features).sum(1)
        logits.append(logit.unsqueeze(1))
        error = torch.where(valid[:, t], answers[:, t] - torch.sigmoid(logit), torch.zeros_like(logit)).unsqueeze(1)
        delta = before * error * features
        after = before * (1.0 - kappa * torch.abs(delta)) if fatigue else before
        active = mask & (features != 0)
        counts = torch.stack([(active & (before < 0)).to(z.dtype).sum(1), active.to(z.dtype).sum(1), (mask & (after < 0)).to(z.dtype).sum(1)], 1)
        stats = stats + counts
        v = torch.where(mask, v + delta, v)
        e = torch.where(mask, after, e)
    return torch.cat(logits, 1), v, e, stats
')$scan
  }
  .lkt_gru_plasticity_code$scan
}

lkt_gru_apply_plasticity <- function(net,out,answers,context,s,gaps,valid,categorical=NULL,
                                     allow_empty=FALSE) {
  if(s$plasticity=='bayesian_prototypes')return(lkt_gru_apply_bayesian_prototypes(net,out,answers,context,s,valid))
  if(is.null(gaps))stop('Plastic connections require elapsed-time gaps.',call.=FALSE)
  z <- out$plastic_features
  if(is.null(z))stop('Plastic connections require final-head activations.',call.=FALSE)
  if(is.null(valid))valid <- matrix(TRUE,z$size(1),z$size(2))
  lengths <- rowSums(valid)
  if((!allow_empty && any(lengths<1L)) || any(valid!=outer(lengths,seq_len(z$size(2)),`>=`)))
    stop('Plastic connections require nonempty histories with trailing padding.',call.=FALSE)
  v <- context$plastic_weights;e <- context$plastic_rates
  if(xor(is.null(v),is.null(e)))stop('Incomplete plastic connection state.',call.=FALSE)
  if(is.null(v)) {
    v <- torch::torch_zeros(c(z$size(1),z$size(3)),dtype=z$dtype,device=z$device)
    e <- if(s$plasticity=='item_skill')v*0 else v+net$plasticity$eta
  } else {v<-v$squeeze(2);e<-e$squeeze(2)}
  fatigue <- s$plasticity=='fatigue'
  zero <- if(s$plasticity=='item_skill')net$plasticity$skill_rate$sum()*0 else net$plasticity$eta*0
  q <- if(s$plasticity=='item_skill')lkt_gru_item_skill_scan()(out$logits,z,answers,
    torch::torch_tensor(valid,dtype=torch::torch_bool(),device=z$device),v,e,categorical,
    net$plasticity$skill_rate,net$plasticity$log_item,net$plasticity$item_parent,net$plasticity$counts) else if(s$plasticity=='error_rate')lkt_gru_error_rate_scan()(out$logits,z,answers,
    torch::torch_tensor(valid,dtype=torch::torch_bool(),device=z$device),v,e,
    net$plasticity$alpha,net$plasticity$c) else lkt_gru_plasticity_scan()(out$logits,z,answers,gaps,
    torch::torch_tensor(valid,dtype=torch::torch_bool(),device=z$device),v,e,net$plasticity$eta,
    if(fatigue)net$plasticity$kappa else zero,if(fatigue)net$plasticity$log_rho else zero,
    torch::jit_scalar(fatigue))
  if(!torch::torch_isfinite(q[[1]])$all()$item() || !torch::torch_isfinite(q[[2]])$all()$item() ||
     !torch::torch_isfinite(q[[3]])$all()$item())stop('Plastic connection dynamics became nonfinite.',call.=FALSE)
  out$logits <- q[[1]];out$plastic_features <- NULL
  out$context$plastic_weights <- q[[2]]$unsqueeze(2)
  out$context$plastic_rates <- q[[3]]$unsqueeze(2)
  out$plastic_stats <- q[[4]]
  out
}

# Student learning rates evolve after feedback; current updates use the prior rate.
.lkt_gru_error_rate_code <- new.env(parent=emptyenv())
lkt_gru_error_rate_scan <- function() {
 if(!exists('scan',.lkt_gru_error_rate_code,inherits=FALSE)) {
 .lkt_gru_error_rate_code$scan <- torch::jit_compile('
def scan(base: Tensor, z: Tensor, answers: Tensor, valid: Tensor, v: Tensor,
         rates: Tensor, alpha: Tensor, c: Tensor) -> Tuple[Tensor, Tensor, Tensor, Tensor]:
    logits: List[Tensor] = []
    stats = torch.zeros([z.size(0), 3], dtype=z.dtype, device=z.device)
    for t in range(z.size(1)):
        mask = valid[:, t].unsqueeze(1)
        features = torch.where(mask, z[:, t, :], torch.zeros_like(z[:, t, :]))
        logit = base[:, t] + (v * features).sum(1)
        logits.append(logit.unsqueeze(1))
        error = torch.where(valid[:, t], answers[:, t] - torch.sigmoid(logit), torch.zeros_like(logit)).unsqueeze(1)
        delta = rates * error * features
        after = rates + alpha * torch.abs(error) - c
        active = mask & (features != 0)
        stats = stats + torch.stack([(active & (rates < 0)).to(z.dtype).sum(1), active.to(z.dtype).sum(1), (mask & (after < 0)).to(z.dtype).sum(1)], 1)
        v = torch.where(mask, v + delta, v)
        rates = torch.where(mask, after, rates)
    return torch.cat(logits, 1), v, rates, stats
')$scan
 }
 .lkt_gru_error_rate_code$scan
}

# Two static learned tables; only student output adjustments evolve online.
lkt_gru_item_skill_schema <- function(data,train,vocab,s) {
 if(length(s$categorical_cols)<2L)stop('item_skill output adaptation requires item and skill as the first two categorical columns.',call.=FALSE)
 item<-s$categorical_cols[1L];skill<-s$categorical_cols[2L]
 map<-unique(data[train,c(item,skill),drop=FALSE])
 if(anyDuplicated(as.character(map[[item]])))stop('item_skill requires each item to belong to exactly one skill.',call.=FALSE)
 parent<-match(as.character(map[[skill]])[match(vocab[[item]],as.character(map[[item]]))],vocab[[skill]])
 if(anyNA(parent) || any(tabulate(parent,nbins=length(vocab[[skill]]))==0L))stop('Incomplete training-only item/skill mapping.',call.=FALSE)
 list(parent=as.integer(parent),items=vocab[[item]],skills=vocab[[skill]],item_col=item,skill_col=skill)
}
lkt_gru_item_skill_module <- function(schema) {
 if(is.null(schema))stop('Missing saved item/skill output-rate schema.',call.=FALSE)
 self<-NULL
 definition<-torch::nn_module(initialize=function() {
  self$skill_rate<-torch::nn_parameter(torch::torch_zeros(length(schema$skills)))
  self$log_item<-torch::nn_parameter(torch::torch_zeros(length(schema$items)))
  self$register_buffer('item_parent',torch::torch_tensor(schema$parent-1L,dtype=torch::torch_long()))
  self$register_buffer('counts',torch::torch_tensor(as.numeric(tabulate(schema$parent,nbins=length(schema$skills)))))
 })
 definition()
}
.lkt_gru_item_skill_code<-new.env(parent=emptyenv())
lkt_gru_item_skill_scan<-function() {
 if(!exists('scan',.lkt_gru_item_skill_code,inherits=FALSE)) {
 .lkt_gru_item_skill_code$scan<-torch::jit_compile('
def scan(base: Tensor, z: Tensor, answers: Tensor, valid: Tensor, v: Tensor,
         rates: Tensor, categories: Tensor, skill_rate: Tensor, log_item: Tensor,
         parent: Tensor, counts: Tensor) -> Tuple[Tensor, Tensor, Tensor, Tensor]:
    sums = torch.zeros_like(skill_rate).scatter_add(0, parent, log_item)
    multipliers = torch.exp(log_item - (sums / counts)[parent])
    item_table = torch.cat([torch.ones_like(log_item[:1]), multipliers], 0)
    skill_table = torch.cat([torch.zeros_like(skill_rate[:1]), skill_rate], 0)
    event_rates = skill_table[categories[:, :, 1] - 1] * item_table[categories[:, :, 0] - 1]
    logits: List[Tensor] = []
    stats = torch.zeros([z.size(0), 3], dtype=z.dtype, device=z.device)
    for t in range(z.size(1)):
        mask = valid[:, t].unsqueeze(1)
        features = torch.where(mask, z[:, t, :], torch.zeros_like(z[:, t, :]))
        logit = base[:, t] + (v * features).sum(1)
        logits.append(logit.unsqueeze(1))
        error = torch.where(valid[:, t], answers[:, t] - torch.sigmoid(logit), torch.zeros_like(logit)).unsqueeze(1)
        rate = event_rates[:, t].unsqueeze(1)
        v = torch.where(mask, v + rate * error * features, v)
        rates = torch.where(mask, rate.expand_as(rates), rates)
        active = mask & (features != 0)
        stats = stats + torch.stack([torch.zeros_like(base[:, t]), active.to(z.dtype).sum(1), torch.zeros_like(base[:, t])], 1)
    return torch.cat(logits, 1), v, rates, stats
')$scan
 }
 .lkt_gru_item_skill_code$scan
}
