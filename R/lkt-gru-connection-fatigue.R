# Connection multipliers cover every scalar input/recurrent GRU weight, not biases.
lkt_gru_connection_fatigue <- function(s)isTRUE(s$gate_fatigue) && identical(s$gate_fatigue_mode,'connection')
lkt_gru_connection_scales <- function(n,s) {
  limits <- s$connection_time_scale_range
  if(is.null(limits) || identical(as.numeric(limits),c(1,1)))return(rep(1,n))
  old_kind <- RNGkind();had_seed <- exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE)
  if(had_seed)old_seed <- get('.Random.seed',envir=.GlobalEnv)
  on.exit({do.call(RNGkind,as.list(old_kind));if(had_seed)assign('.Random.seed',old_seed,envir=.GlobalEnv) else
    if(exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))rm('.Random.seed',envir=.GlobalEnv)})
  set.seed(s$connection_time_scale_seed,kind='Mersenne-Twister',normal.kind='Inversion',sample.kind='Rejection')
  exp(stats::runif(n,log(limits[1]),log(limits[2])))
}
lkt_gru_random_connection_time <- function(s)!is.null(s$connection_time_scale_range) &&
  !identical(as.numeric(s$connection_time_scale_range),c(1,1))
lkt_gru_candidate_connections <- function(s)isTRUE(s$connection_fatigue_scope %in% c('candidate','input_candidate','h1_input_candidate','recurrent_candidate'))
lkt_gru_input_connections <- function(s)isTRUE(s$connection_fatigue_scope %in% c('input_candidate','h1_input_candidate'))
lkt_gru_recurrent_connections <- function(s)identical(s$connection_fatigue_scope,'recurrent_candidate')
lkt_gru_h1_connections <- function(s)identical(s$connection_fatigue_scope,'h1_input_candidate')
lkt_gru_fatigue_indices <- function(module,s) {
 weights <- module$named_parameters()[grepl('weight_',names(module$named_parameters()))]
 offset <- 0L;indices <- integer();input_layer <- 0L
 for(name in names(weights)) {
   w <- weights[[name]];n <- as.integer(w$numel());stopifnot(n%%3L==0L)
   input <- grepl('weight_ih',name);if(input)input_layer <- input_layer+1L
   keep <- (!lkt_gru_input_connections(s) || input) && (!lkt_gru_recurrent_connections(s) || !input) && (!lkt_gru_h1_connections(s) || input_layer==1L)
   if(keep)indices <- c(indices,offset+if(lkt_gru_candidate_connections(s))2L*(n%/%3L)+seq_len(n%/%3L) else seq_len(n))
   offset <- offset+n
 }
 indices
}
lkt_gru_candidate_indices <- function(module)lkt_gru_fatigue_indices(module,list(connection_fatigue_scope='candidate'))
lkt_gru_connection_module <- function(module,s=NULL) {
  full_n <- sum(vapply(module$named_parameters()[grepl('weight_',names(module$named_parameters()))],function(w)as.numeric(w$numel()),numeric(1)))
  indices <- lkt_gru_fatigue_indices(module,s)
  n <- length(indices)
  self <- NULL
  definition <- torch::nn_module(initialize=function() {
    if(lkt_gru_random_connection_time(s))self$register_buffer('time_scale',
      torch::torch_tensor(lkt_gru_connection_scales(full_n,s)[indices],dtype=torch::torch_float32()))
    self$lambda <- torch::nn_parameter(torch::torch_ones(n))
    self$log_rho <- torch::nn_parameter(torch::torch_full(1L,log(log(2))))
  })
  definition()
}
.lkt_gru_connection_code <- new.env(parent=emptyenv())
lkt_gru_connection_scan <- function() {
 if(!exists('scan',.lkt_gru_connection_code,inherits=FALSE)) {
 .lkt_gru_connection_code$scan <- torch::jit_compile('
def scan(x: Tensor, h: Tensor, a: Tensor, minutes: Tensor, valid: Tensor,
         lam: Tensor, log_rho: Tensor, weights: List[Tensor], layers: int,
         bias: bool, dropout: float, training: bool, all_inputs: bool, scale: Tensor, candidate_only: bool, input_only: bool, h1_only: bool, recurrent_only: bool) -> Tuple[Tensor, Tensor, Tensor, Tensor]:
    final_h: List[Tensor] = []
    final_a: List[Tensor] = []
    priors: List[Tensor] = []
    output = x
    stride = 4 if bias else 2
    width = h.size(2)
    rho = torch.exp(log_rho)
    offset = 0
    if a.size(1) == 0:
        a = torch.ones([x.size(0), lam.numel()], dtype=x.dtype, device=x.device)
    for layer in range(layers):
        if layer > 0:
            output = torch.dropout(output.transpose(0, 1).contiguous(), dropout, training).transpose(0, 1)
            if all_inputs:
                output = torch.cat([output, x], 2)
        wi = weights[layer * stride]
        wh = weights[layer * stride + 1]
        rows = width if candidate_only else wi.size(0)
        active_layer = h1_only == False or layer == 0
        rows_i = rows if active_layer and recurrent_only == False else 0
        rows_h = rows if active_layer and input_only == False else 0
        ni = rows_i * wi.size(1)
        nh = rows_h * wh.size(1)
        ai = a[:, offset:offset+ni].reshape(x.size(0), rows_i, wi.size(1))
        li = lam[offset:offset+ni].reshape(1, rows_i, wi.size(1))
        si = scale[offset:offset+ni].reshape(1, rows_i, wi.size(1))
        offset = offset + ni
        ah = a[:, offset:offset+nh].reshape(x.size(0), rows_h, wh.size(1))
        lh = lam[offset:offset+nh].reshape(1, rows_h, wh.size(1))
        sh = scale[offset:offset+nh].reshape(1, rows_h, wh.size(1))
        offset = offset + nh
        state = h[layer]
        posts: List[Tensor] = []
        for t in range(x.size(1)):
            mask = valid[:, t].unsqueeze(1)
            matrix_mask = mask.unsqueeze(2)
            dt = torch.where(valid[:, t], minutes[:, t], torch.zeros_like(minutes[:, t])).reshape(x.size(0), 1, 1)
            ri = 1.0 - (1.0 - ai) * torch.exp(-rho * dt * si)
            rh = 1.0 - (1.0 - ah) * torch.exp(-rho * dt * sh)
            if ni == 0:
                effective_i = wi.unsqueeze(0).expand(x.size(0), -1, -1)
            elif candidate_only:
                effective_i = torch.cat([wi[:2*width].unsqueeze(0).expand(x.size(0), -1, -1), ri * wi[2*width:].unsqueeze(0)], 1)
            else:
                effective_i = ri * wi.unsqueeze(0)
            if nh == 0:
                effective_h = wh.unsqueeze(0).expand(x.size(0), -1, -1)
            elif candidate_only:
                effective_h = torch.cat([wh[:2*width].unsqueeze(0).expand(x.size(0), -1, -1), rh * wh[2*width:].unsqueeze(0)], 1)
            else:
                effective_h = rh * wh.unsqueeze(0)
            ip = torch.bmm(effective_i, output[:, t, :].unsqueeze(2)).squeeze(2)
            hp = torch.bmm(effective_h, state.unsqueeze(2)).squeeze(2)
            if bias:
                ip = ip + weights[layer * stride + 2]
                hp = hp + weights[layer * stride + 3]
            reset = torch.sigmoid(ip[:, :width] + hp[:, :width])
            retain = torch.sigmoid(ip[:, width:2*width] + hp[:, width:2*width])
            candidate = torch.tanh(ip[:, 2*width:] + reset * hp[:, 2*width:])
            after = candidate + retain * (state - candidate)
            if layer == layers - 1:
                priors.append(state.unsqueeze(1))
            state = torch.where(mask, after, state)
            ai = torch.where(matrix_mask, li * ri, ai)
            ah = torch.where(matrix_mask, lh * rh, ah)
            posts.append(state.unsqueeze(1))
        output = torch.cat(posts, 1)
        final_h.append(state.unsqueeze(0))
        final_a.append(ai.reshape(x.size(0), ni))
        final_a.append(ah.reshape(x.size(0), nh))
    return output, torch.cat(priors, 1), torch.cat(final_h, 0), torch.cat(final_a, 1)
')$scan
 }
 function(x,h,a,minutes,valid,lam,log_rho,weights,layers,bias,dropout,training,all_inputs,time_scale=NULL,candidate_only=FALSE,input_only=FALSE,h1_only=FALSE,recurrent_only=FALSE) {
   if(is.null(time_scale))time_scale <- torch::torch_ones_like(lam)
   .lkt_gru_connection_code$scan(x,h,a,minutes,valid,lam,log_rho,weights,layers,bias,dropout,training,all_inputs,time_scale,torch::jit_scalar(candidate_only),torch::jit_scalar(input_only),torch::jit_scalar(h1_only),torch::jit_scalar(recurrent_only))
 }
}
lkt_gru_connection_transition <- function(net,interaction,state,s,minutes,valid) {
 gs <- lkt_sequence_gru_settings(s);named <- lkt_sequence_module(net,s)$named_parameters()
 kinds <- c('weight_ih','weight_hh',if(gs$gru_bias)c('bias_ih','bias_hh'))
 expected <- unlist(lapply(seq_len(s$num_layers),function(layer)
   if(gs$input_to_all_layers && s$num_layers>1L)paste0('layers.',layer-1L,'.',kinds,'_l1') else paste0(kinds,'_l',layer)),use.names=FALSE)
 if(!identical(names(named),expected))stop('Unexpected connection-fatigue GRU weight layout.',call.=FALSE)
 scale <- if(lkt_gru_random_connection_time(s))net$gate_fatigue$time_scale else NULL
 q <- lkt_gru_connection_scan()(interaction,state$h,state$a,minutes,valid,net$gate_fatigue$lambda,net$gate_fatigue$log_rho,
   unname(named),torch::jit_scalar(s$num_layers),torch::jit_scalar(gs$gru_bias),torch::jit_scalar(as.numeric(gs$gru_dropout)),
   torch::jit_scalar(net$training),torch::jit_scalar(gs$input_to_all_layers && s$num_layers>1L),time_scale=scale,candidate_only=lkt_gru_candidate_connections(s),input_only=lkt_gru_input_connections(s),h1_only=lkt_gru_h1_connections(s),recurrent_only=lkt_gru_recurrent_connections(s))
 list(q[[1L]],list(h=q[[3L]],a=q[[4L]]),prior=q[[2L]])
}
