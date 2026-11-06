# Native Mamba-1 reference implementation. Algorithm and initialization adapted
# from state-spaces/mamba, mamba_ssm/modules/mamba_simple.py (Apache-2.0).
# The pinned reference and attribution are recorded in inst/sequence-reference.json.
# This is the primary native scan implementation, not an optimized CUDA kernel.
lkt_mamba_initial <- function(s,batch,like) {
  o <- s$unit_options;inner <- o$expand*s$hidden_size
  zero <- function(shape)torch::torch_zeros(shape,dtype=like$dtype,device=like$device)
  list(read=zero(c(batch,1L,s$hidden_size)),layers=lapply(seq_len(s$num_layers),function(i)
    list(conv=zero(c(batch,inner,o$d_conv)),ssm=zero(c(batch,inner,o$d_state)))))
}

lkt_mamba_block <- function(s) {
  self <- NULL;o <- s$unit_options;d <- s$hidden_size;inner <- d*o$expand
  rank <- if(identical(o$dt_rank,'auto'))as.integer(ceiling(d/16)) else as.integer(o$dt_rank)
  definition <- torch::nn_module(
    initialize=function() {
      self$norm <- torch::nn_parameter(torch::torch_ones(d))
      self$in_proj <- torch::nn_linear(d,2L*inner,bias=o$bias)
      self$conv <- torch::nn_conv1d(inner,inner,o$d_conv,groups=inner,bias=o$conv_bias)
      self$x_proj <- torch::nn_linear(inner,rank+2L*o$d_state,bias=FALSE)
      self$dt_proj <- torch::nn_linear(rank,inner,bias=TRUE)
      torch::nn_init_uniform_(self$dt_proj$weight,-rank^(-.5),rank^(-.5))
      dt <- torch::torch_exp(torch::torch_rand(inner)*(log(o$dt_max)-log(o$dt_min))+log(o$dt_min))$clamp(min=o$dt_init_floor)
      torch::with_no_grad({self$dt_proj$bias$copy_(dt+torch::torch_log(-torch::torch_expm1(-dt)))})
      self$A_log <- torch::nn_parameter(torch::torch_log(torch::torch_tensor(seq_len(o$d_state),dtype=torch::torch_float32()))$unsqueeze(1)$expand(c(inner,o$d_state))$clone())
      self$D <- torch::nn_parameter(torch::torch_ones(inner))
      self$out_proj <- torch::nn_linear(inner,d,bias=o$bias)
    },
    forward=function(input,state) {
      normalized <- input*torch::torch_rsqrt(input$pow(2)$mean(dim=3,keepdim=TRUE)+o$norm_epsilon)*self$norm
      xz <- self$in_proj(normalized)
      projected <- xz[,,seq_len(inner),drop=FALSE]$permute(c(1,3,2))
      full <- torch::torch_cat(list(state$conv,projected),dim=3)
      steps <- input$size(2)
      # A k-value cache produces one old output, which is omitted. All following
      # convolution outputs depend only on the prefix ending at that interaction.
      x <- torch::nnf_silu(self$conv(full)[,,seq.int(2L,steps+1L),drop=FALSE]$permute(c(1,3,2)))
      z <- xz[,,inner+seq_len(inner),drop=FALSE]
      params <- self$x_proj(x)
      delta <- torch::nnf_softplus(self$dt_proj(params[,,seq_len(rank),drop=FALSE]))
      B <- params[,,rank+seq_len(o$d_state),drop=FALSE]
      C <- params[,,rank+o$d_state+seq_len(o$d_state),drop=FALSE]
      A <- -torch::torch_exp(self$A_log)
      memory <- state$ssm;outputs <- vector('list',steps)
      for(t in seq_len(steps)) {
        dt <- delta[,t,,drop=FALSE]$squeeze(2)$unsqueeze(3)
        u <- x[,t,,drop=FALSE]$squeeze(2)
        memory <- memory*torch::torch_exp(dt*A$unsqueeze(1))+
          dt*B[,t,,drop=FALSE]*u$unsqueeze(3)
        y <- (memory*C[,t,,drop=FALSE])$sum(dim=3)+self$D*u
        outputs[[t]] <- (y*torch::nnf_silu(z[,t,,drop=FALSE]$squeeze(2)))$unsqueeze(2)
      }
      output <- input+self$out_proj(torch::torch_cat(outputs,dim=2))
      list(output,list(conv=full[,,seq.int(full$size(3)-o$d_conv+1L,full$size(3)),drop=FALSE],ssm=memory))
    })
  definition()
}

lkt_mamba_network <- function(input_size,s) {
  self <- NULL
  definition <- torch::nn_module(
    initialize=function() {
      self$input <- torch::nn_linear(input_size,s$hidden_size,bias=TRUE)
      self$layers <- torch::nn_module_list(lapply(seq_len(s$num_layers),function(i)lkt_mamba_block(s)))
      self$between <- torch::nn_dropout(s$layer_dropout)
    },
    forward=function(interaction,state=NULL) {
      if(is.null(state))state <- lkt_mamba_initial(s,interaction$size(1),interaction)
      output <- self$input(interaction);next_layers <- vector('list',s$num_layers)
      for(i in seq_len(s$num_layers)) {
        if(i>1L)output <- self$between(output)
        result <- self$layers[[i]](output,state$layers[[i]])
        output <- result[[1L]];next_layers[[i]] <- result[[2L]]
      }
      list(output,list(read=output[,output$size(2),,drop=FALSE],layers=next_layers))
    })
  definition()
}
