# Carry the last completed interaction and latent distribution across truncated
# backpropagation chunks. Tensors and raw identity arrays use batch as their first
# dimension, so finished histories are removed from every piece of context together.
lkt_gru_context_subset <- function(context,indices=NULL,detach=FALSE) {
  if(is.null(context))return(NULL)
  lapply(context,function(x) {
    if(!is.null(indices))x <- x[indices,,,drop=FALSE]
    if(detach && !is.character(x))x <- x$detach()
    x
  })
}

lkt_gru_inference_body <- function(width,hidden,activation,dropout) {
  if(!length(hidden))return(torch::nn_identity())
  layers <- list()
  for(next_width in hidden) {
    layers[[length(layers)+1L]] <- torch::nn_linear(width,next_width)
    layers[[length(layers)+1L]] <- switch(activation,relu=torch::nn_relu(),tanh=torch::nn_tanh(),gelu=torch::nn_gelu())
    layers[[length(layers)+1L]] <- torch::nn_dropout(dropout)
    width <- next_width
  }
  do.call(torch::nn_sequential,layers)
}

# The inference branch follows the causal routing described in UNVaMP, without
# claiming to reproduce its unpublished layer settings. Before predicting trial t it
# sees memory through t-2, the completed interaction t-1, and current numeric features.
lkt_gru_inference_network <- function(preprocessing,s) {
  self <- NULL
  raw_numeric_width <- length(preprocessing$numeric_names)
  learned <- s$feature_encoder=='mlp'
  layout <- lkt_gru_input_layout(preprocessing,s)
  time_indices <- layout$time;auxiliary_indices <- layout$history
  numeric_width <- layout$numeric_width;response_width <- layout$response_width
  dims <- preprocessing$embedding_dims
  categorical_width <- layout$categorical_width
  width <- layout$width
  # Question-only matching selects item/skill coordinates, excluding the student
  # state and numeric history/time towers. The GRU and prediction head still see
  # all their original inputs, and retrieved values remain post-answer GRU states.
  category_widths <- if(s$categorical_encoding=='embedding')dims else lengths(preprocessing$vocabularies)
  offsets <- c(0L,cumsum(category_widths))
  cue_indices <- unlist(lapply(match(s$retrieval_cue_cols,names(preprocessing$vocabularies)),function(j)
    numeric_width+seq.int(offsets[j]+1L,offsets[j+1L])),use.names=FALSE)
  cue_width <- if(length(cue_indices))length(cue_indices) else s$hidden_size+width
  retrieving <- s$retrieval_heads>0L
  memory_extra <- if(retrieving && s$retrieval_fusion=='concat')s$hidden_size else 0L
  inference_width <- s$hidden_size+width+response_width+numeric_width+memory_extra
  body_width <- if(length(s$inference_hidden_sizes))utils::tail(s$inference_hidden_sizes,1L) else inference_width
  definition <- torch::nn_module(
    initialize=function() {
      self$embeddings <- lkt_gru_embeddings(preprocessing,s)
      self$input_drop <- torch::nn_dropout(s$input_dropout)
      # Independent towers learn representations of responses, time and other numeric
      # predictors. Item/skill IDs retain their jointly learned embedding tables.
      if(learned) {
        tower <- function(n,out) {
          body <- lkt_gru_inference_body(n,s$encoder_hidden_sizes,s$encoder_activation,s$encoder_dropout)
          last <- if(length(s$encoder_hidden_sizes))utils::tail(s$encoder_hidden_sizes,1L) else n
          torch::nn_sequential(body,torch::nn_linear(last,out))
        }
        self$response_encoder <- tower(1L,layout$sizes[['response']])
        if(length(time_indices))self$time_encoder <- tower(length(time_indices),layout$sizes[['time']])
        if(length(auxiliary_indices))self$auxiliary_encoder <- tower(length(auxiliary_indices),layout$sizes[['history']])
      }
      self[[lkt_sequence_module_name(s)]] <- lkt_sequence_build(width+response_width,s)
      if(s$inference_mode!='none') {
        self$inference <- lkt_gru_inference_body(inference_width,s$inference_hidden_sizes,
          s$inference_activation,s$inference_dropout)
        self$mean <- torch::nn_linear(body_width,s$latent_size)
      }
      widths <- c(if(s$inference_mode=='none')s$hidden_size+width+memory_extra else s$latent_size+categorical_width,
        s$head_hidden_sizes,1L)
      layers <- list()
      for(j in seq_len(length(widths)-1L)) {
        layers[[length(layers)+1L]] <- torch::nn_linear(widths[j],widths[j+1L],bias=s$head_bias)
        if(j<length(widths)-1L) {
          layers[[length(layers)+1L]] <- switch(s$head_activation,relu=torch::nn_relu(),tanh=torch::nn_tanh(),gelu=torch::nn_gelu())
          layers[[length(layers)+1L]] <- torch::nn_dropout(s$head_dropout)
        }
      }
      self$head <- do.call(torch::nn_sequential,layers)
      # Allocate retrieval after the existing shared network. With retrieval disabled,
      # no extra weights or random draws change historical models or their predictions.
      if(retrieving)self$retrieval <- lkt_gru_retrieval_network(cue_width,s)
      # Initialize this additional output last so deterministic and Gaussian models
      # have exactly the same initial shared weights for a given training seed.
      if(s$inference_mode=='gaussian') {
        self$logvar <- torch::nn_linear(body_width,s$latent_size)
        torch::nn_init_zeros_(self$logvar$weight)
        torch::nn_init_constant_(self$logvar$bias,s$logvar_init)
      }
      # One shared current-input representation can learn interactions among all
      # four blocks before retrieval, prediction and memory update. The answer is
      # excluded, and disabled settings allocate no weights or random draws.
      if(!is.null(s$joint_encoder_output_size)) {
        body <- lkt_gru_inference_body(layout$combined_width,s$joint_encoder_hidden_sizes,
          s$joint_encoder_activation,s$joint_encoder_dropout)
        last <- if(length(s$joint_encoder_hidden_sizes))tail(s$joint_encoder_hidden_sizes,1L) else layout$combined_width
        self$joint_encoder <- torch::nn_sequential(body,torch::nn_linear(last,s$joint_encoder_output_size))
      }
    },
    encode=function(numeric,categorical) {
      numeric_parts <- list();category_parts <- list()
      if(!learned)numeric_parts <- list(numeric) else {
        if(length(time_indices))numeric_parts[[length(numeric_parts)+1L]] <- self$time_encoder(numeric[,,time_indices,drop=FALSE])
        if(length(auxiliary_indices))numeric_parts[[length(numeric_parts)+1L]] <- self$auxiliary_encoder(numeric[,,auxiliary_indices,drop=FALSE])
      }
      for(j in seq_along(preprocessing$vocabularies)) {
        ids <- categorical[,,j,drop=FALSE]$squeeze(3)
        if(s$categorical_encoding=='embedding')category_parts[[length(category_parts)+1L]] <- self$embeddings[[j]](ids)
        else category_parts[[length(category_parts)+1L]] <- torch::nnf_one_hot(ids,num_classes=length(preprocessing$vocabularies[[j]])+1L)[,,seq.int(2L,length(preprocessing$vocabularies[[j]])+1L),drop=FALSE]$to(dtype=numeric$dtype)
      }
      # First apply the declared pair sums or concatenation. The optional shared
      # encoder then sees the complete current input once, before input dropout.
      parts <- list()
      if(length(numeric_parts))parts[[length(parts)+1L]] <- lkt_gru_combine_inputs(numeric_parts,s$numeric_fusion)
      if(length(category_parts))parts[[length(parts)+1L]] <- lkt_gru_combine_inputs(category_parts,s$categorical_fusion)
      combined <- lkt_gru_combine_inputs(parts,s$input_fusion)
      if(!is.null(s$joint_encoder_output_size))combined <- self$joint_encoder(combined)
      self$input_drop(combined)
    },
    forward=function(numeric,categorical,answers,hidden=NULL,context=NULL,retrieval_ids=NULL) {
      x <- self$encode(numeric,categorical)
      batch <- x$size(1);steps <- x$size(2)
      retrieval_context <- context
      zeros <- function(n)torch::torch_zeros(c(batch,1L,n),dtype=x$dtype,device=x$device)
      if(is.null(hidden))hidden <- lkt_sequence_initial(s,batch,x)
      if(is.null(context))context <- list(older_hidden=zeros(s$hidden_size),last_interaction=zeros(width+response_width),
        mean=zeros(s$latent_size),logvar=zeros(s$latent_size),has_previous=zeros(1L))
      response <- answers$unsqueeze(3)
      if(learned)response <- self$response_encoder(response)
      interaction <- torch::torch_cat(list(x,response),dim=3)
      transition <- lkt_sequence_advance(self,interaction,hidden,s)
      prior <- torch::torch_cat(list(lkt_sequence_read(hidden,s),transition[[1]]),dim=2)[,seq_len(steps),,drop=FALSE]
      memory <- prior
      if(retrieving) {
        cue <- if(length(cue_indices))x[,,cue_indices,drop=FALSE] else torch::torch_cat(list(prior,x),dim=3)
        retrieved <- self$retrieval(cue,transition[[1]],retrieval_context,retrieval_ids)
        memory <- if(s$retrieval_fusion=='replace')retrieved$read else torch::torch_cat(list(prior,retrieved$read),dim=3)
      }
      if(s$inference_mode=='none') {
        result <- list(logits=self$head(torch::torch_cat(list(memory,x),dim=3))$squeeze(3),hidden=transition[[2]])
        if(retrieving) {
          result$context <- retrieved$context;result$retrieval_attention <- retrieved$weights
        }
        return(result)
      }
      # Shift both inputs back one interaction: the current answer cannot reach its
      # own mean, variance, or probability. The most recent completed interaction
      # bypasses the GRU, while older memory still carries the longer history.
      older <- torch::torch_cat(list(context$older_hidden,prior),dim=2)[,seq_len(steps),,drop=FALSE]
      if(retrieving)older <- if(s$retrieval_fusion=='replace')retrieved$read else torch::torch_cat(list(older,retrieved$read),dim=3)
      recent <- torch::torch_cat(list(context$last_interaction,interaction),dim=2)[,seq_len(steps),,drop=FALSE]
      evidence <- list(older,recent)
      if(numeric_width)evidence[[3L]] <- x[,,seq_len(numeric_width),drop=FALSE]
      body <- self$inference(torch::torch_cat(evidence,dim=3))
      mean <- self$mean(body);latent <- mean
      logvar <- mean*0
      kl <- mean$sum(dim=3)*0
      valid <- torch::torch_cat(list(context$has_previous,torch::torch_ones(c(batch,steps,1L),dtype=x$dtype,device=x$device)),dim=2)[,seq_len(steps),,drop=FALSE]$squeeze(3)
      if(s$inference_mode=='gaussian') {
        logvar <- self$logvar(body)$clamp(min=s$logvar_bounds[1L],max=s$logvar_bounds[2L])
        if(self$training)latent <- mean+torch::torch_exp(.5*logvar)*torch::torch_randn_like(mean)
        previous_mean <- torch::torch_cat(list(context$mean,mean),dim=2)[,seq_len(steps),,drop=FALSE]
        previous_logvar <- torch::torch_cat(list(context$logvar,logvar),dim=2)[,seq_len(steps),,drop=FALSE]
        # Closed-form diagonal Gaussian KL(new || previous), averaged over latent
        # dimensions. The loss mask excludes padding and the first state of a history.
        kl <- .5*(previous_logvar-logvar+torch::torch_exp(logvar-previous_logvar)+
          (mean-previous_mean)$pow(2)*torch::torch_exp(-previous_logvar)-1)$mean(dim=3)
      }
      decoder <- list(latent)
      if(categorical_width)decoder[[2L]] <- x[,,numeric_width+seq_len(categorical_width),drop=FALSE]
      result <- list(logits=self$head(torch::torch_cat(decoder,dim=3))$squeeze(3),hidden=transition[[2]],
        latent_mean=mean,latent_logvar=logvar,kl_per_trial=kl,kl_valid=valid,
        context=list(older_hidden=prior[,steps,,drop=FALSE],last_interaction=interaction[,steps,,drop=FALSE],
          mean=mean[,steps,,drop=FALSE],logvar=logvar[,steps,,drop=FALSE],
          has_previous=torch::torch_ones(c(batch,1L,1L),dtype=x$dtype,device=x$device)))
      if(retrieving) {
        result$context <- c(result$context,retrieved$context)
        result$retrieval_attention <- retrieved$weights
      }
      result
    })
  net <- definition()
  net$to(device=s$device,dtype=if(s$dtype=='float32')torch::torch_float32() else torch::torch_float64())
  net
}
