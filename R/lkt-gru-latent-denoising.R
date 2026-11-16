# Optional KeenKT Eq.10 transfer. The denoiser never supplies prediction,
# retrieval, recurrence or online adaptation values; only its training gradient
# reaches the clean pre-answer top GRU state.
lkt_gru_denoising_active <- function(s)isTRUE(s$latent_denoising)
lkt_gru_denoising_training <- function(net,s)
  lkt_gru_denoising_active(s) && s$denoising_loss_weight>0 && isTRUE(net$training)

lkt_gru_validate_denoising <- function(s) {
  if(!is.logical(s$latent_denoising) || length(s$latent_denoising)!=1L || is.na(s$latent_denoising))
    stop('latent_denoising must be TRUE or FALSE.',call.=FALSE)
  if(!is.numeric(s$denoising_noise_std) || length(s$denoising_noise_std)!=1L ||
     !is.finite(s$denoising_noise_std) || s$denoising_noise_std!=.20)
    stop('The fixed latent-denoising transfer requires noise SD .20.',call.=FALSE)
  if(!is.numeric(s$denoising_loss_weight) || length(s$denoising_loss_weight)!=1L ||
     !is.finite(s$denoising_loss_weight) || !s$denoising_loss_weight%in%c(0,.15))
    stop('The fixed latent-denoising loss weight is .15; zero is the explicit parity control.',call.=FALSE)
  if(!lkt_gru_denoising_active(s))return(invisible(NULL))
  if(s$hidden_size!=32L || s$inference_mode!='none' || s$hidden_decay ||
     lkt_gru_gate_active(s) || lkt_gru_adaptive(s) || !is.null(s$semantic_input) ||
     !s$plasticity%in%c('none','constant') || !is.null(s$initial_model_file))
    stop('Latent denoising supports a 32-wide plain GRU with optional feature encoders, retrieval and constant output adaptation; other experimental mechanisms and warm starts are unsupported.',call.=FALSE)
  invisible(NULL)
}

lkt_gru_denoising_module <- function() {
  # Ordinary framework initialization, allocated after every baseline module.
  # Preserve the baseline random stream, including its later dropout draws.
  rng <- torch::torch_get_rng_state()
  on.exit(torch::torch_set_rng_state(rng),add=TRUE)
  torch::nn_sequential(torch::nn_linear(32L,32L),torch::nn_relu(),torch::nn_linear(32L,32L))
}

lkt_gru_denoising_state <- function(net,out,prior,s) {
  if(lkt_gru_denoising_training(net,s))out$denoising_state <- prior
  out
}

lkt_gru_denoising_mse <- function(net,out,valid,s) {
  if(!lkt_gru_denoising_training(net,s))return(NULL)
  h <- out$denoising_state
  if(is.null(h) || h$size(3L)!=32L || !is.matrix(valid) || !is.logical(valid) ||
     anyNA(valid) || !identical(dim(valid),as.integer(c(h$size(1L),h$size(2L)))) || !any(valid))
    stop('Latent denoising requires explicit valid-event masks and causal 32-wide states.',call.=FALSE)
  # Torch flattening traverses each history's time positions together. Select
  # real events first: no padded coordinate consumes noise or enters reduction.
  indices <- which(as.vector(t(valid)))
  clean <- h$reshape(c(-1L,32L))[indices,,drop=FALSE]
  noisy <- clean+torch::torch_randn_like(clean)*s$denoising_noise_std
  reconstructed <- noisy+net$latent_denoiser(noisy)
  # Both the reconstruction input and clean target retain their graph.
  (reconstructed-clean)$pow(2)$mean()
}

lkt_gru_validate_denoising_weights <- function(weights,s) {
  if(!lkt_gru_denoising_active(s))return(invisible(NULL))
  names <- c('latent_denoiser.0.weight','latent_denoiser.0.bias',
    'latent_denoiser.2.weight','latent_denoiser.2.bias')
  if(!all(names%in%names(weights)) || any(!vapply(weights[names],
      function(x)is.numeric(x) && length(x)>0L && all(is.finite(x)),logical(1))))
    stop('Invalid saved latent-denoising weights.',call.=FALSE)
  invisible(NULL)
}
