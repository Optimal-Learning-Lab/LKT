# Activity-driven fatigue of each student's GRU write pathway, per layer/unit.
lkt_gru_gate_active <- function(s)isTRUE(s$gate_fatigue)
lkt_gru_gate_module <- function(s=NULL,module=NULL) {
  if(lkt_gru_connection_fatigue(s))return(lkt_gru_connection_module(module,s))
  self <- NULL
  definition <- torch::nn_module(initialize=function() {
    self$kappa <- torch::nn_parameter(torch::torch_zeros(1L))
    self$log_rho <- torch::nn_parameter(torch::torch_full(1L,log(log(2))))
  })
  definition()
}
lkt_gru_project_gate <- function(net,s) {
  if(lkt_gru_gate_active(s))torch::with_no_grad({
    if(lkt_gru_connection_fatigue(s))net$gate_fatigue$lambda$clamp_(min=0,max=1) else net$gate_fatigue$kappa$clamp_(min=0)
  })
  invisible(NULL)
}
lkt_gru_gate_time <- function(data,s)
  lkt_gru_time(data,list(time_col=s$gate_fatigue_time_col,time_units=s$time_units))
lkt_gru_gate_gaps <- function(data,s,previous=NULL) {
  clock <- s;clock$plasticity_time_col <- s$gate_fatigue_time_col
  lkt_gru_plasticity_gaps(data,clock,previous)
}
.lkt_gru_gate_code <- new.env(parent=emptyenv())
lkt_gru_gate_scan <- function() {
  if(!exists('scan',.lkt_gru_gate_code,inherits=FALSE)) {
    .lkt_gru_gate_code$scan <- torch::jit_compile('
def scan(x: Tensor, h: Tensor, a: Tensor, minutes: Tensor, valid: Tensor,
         kappa: Tensor, log_rho: Tensor, weights: List[Tensor],
         layers: int, bias: bool, dropout: float, training: bool,
         all_inputs: bool) -> Tuple[Tensor, Tensor, Tensor, Tensor]:
    final_h: List[Tensor] = []
    final_a: List[Tensor] = []
    priors: List[Tensor] = []
    output = x
    stride = 4 if bias else 2
    width = h.size(2)
    rho = torch.exp(log_rho)
    # Layer-major evaluation preserves the ordinary GRU layer dropout order.
    for layer in range(layers):
        if layer > 0:
            if all_inputs:
                output = torch.cat([torch.dropout(output.transpose(0, 1).contiguous(), dropout, training).transpose(0, 1), x], 2)
            else:
                output = torch.dropout(output.transpose(0, 1).contiguous(), dropout, training).transpose(0, 1)
        wi = weights[layer * stride]
        wh = weights[layer * stride + 1]
        inputs = torch.matmul(output, wi.t())
        if bias:
            inputs = inputs + weights[layer * stride + 2]
        state = h[layer]
        fatigue = a[layer]
        posts: List[Tensor] = []
        for t in range(x.size(1)):
            mask = valid[:, t].unsqueeze(1)
            dt = torch.where(valid[:, t], minutes[:, t], torch.zeros_like(minutes[:, t])).unsqueeze(1)
            recovered = 1.0 - (1.0 - fatigue) * torch.exp(-rho * dt)
            recurrent = torch.matmul(state, wh.t())
            if bias:
                recurrent = recurrent + weights[layer * stride + 3]
            ip = inputs[:, t, :]
            reset = torch.sigmoid(ip[:, :width] + recurrent[:, :width])
            retain = torch.sigmoid(ip[:, width:2*width] + recurrent[:, width:2*width])
            candidate = torch.tanh(ip[:, 2*width:] + reset * recurrent[:, 2*width:])
            write = 1.0 - retain
            # Equivalent to a GRU retention gate of 1 - recovered * write.
            effective_retain = 1.0 - recovered * write
            after = candidate + effective_retain * (state - candidate)
            after_a = recovered * torch.exp(-kappa * write)
            if layer == layers - 1:
                priors.append(state.unsqueeze(1))
            state = torch.where(mask, after, state)
            fatigue = torch.where(mask, after_a, fatigue)
            posts.append(state.unsqueeze(1))
        output = torch.cat(posts, 1)
        final_h.append(state.unsqueeze(0))
        final_a.append(fatigue.unsqueeze(0))
    return output, torch.cat(priors, 1), torch.cat(final_h, 0), torch.cat(final_a, 0)
')$scan
  }
  .lkt_gru_gate_code$scan
}
lkt_gru_gate_transition <- function(net,interaction,state,s,minutes,valid) {
  if(is.null(minutes) || is.null(valid))stop('Gate fatigue requires elapsed gaps and a validity mask.',call.=FALSE)
  if(lkt_gru_connection_fatigue(s))return(lkt_gru_connection_transition(net,interaction,state,s,minutes,valid))
  gs <- lkt_sequence_gru_settings(s);module <- lkt_sequence_module(net,s)
  kinds <- c('weight_ih','weight_hh',if(gs$gru_bias)c('bias_ih','bias_hh'))
  expected <- unlist(lapply(seq_len(s$num_layers),function(layer)
    if(gs$input_to_all_layers && s$num_layers>1L)paste0('layers.',layer-1L,'.',kinds,'_l1') else paste0(kinds,'_l',layer)),use.names=FALSE)
  named <- module$named_parameters()
  if(!identical(names(named),expected))stop('Unexpected compiled GRU weight layout.',call.=FALSE)
  q <- lkt_gru_gate_scan()(interaction,state$h,state$a,minutes,valid,
    net$gate_fatigue$kappa,net$gate_fatigue$log_rho,unname(named),
    torch::jit_scalar(s$num_layers),torch::jit_scalar(gs$gru_bias),torch::jit_scalar(as.numeric(gs$gru_dropout)),
    torch::jit_scalar(net$training),torch::jit_scalar(gs$input_to_all_layers && s$num_layers>1L))
  list(q[[1L]],list(h=q[[3L]],a=q[[4L]]),prior=q[[2L]])
}
