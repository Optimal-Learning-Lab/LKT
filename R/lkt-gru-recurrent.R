# Give every recurrent layer the same current interaction when requested. Keep the
# ordinary fused stack intact by default so historical weights and RNG draws match.
lkt_gru_recurrent <- function(input_size,s) {
  if(!s$input_to_all_layers || s$num_layers==1L)return(torch::nn_gru(
    input_size,s$hidden_size,num_layers=s$num_layers,bias=s$gru_bias,
    batch_first=TRUE,dropout=s$gru_dropout,bidirectional=FALSE))
  # A zero expansion starts from an ordinary random stack. Preserve its RNG
  # endpoint so the head and retrieval projections also match the ordinary model.
  reference <- NULL
  if(s$additional_input_init=='zero') {
    reference <- torch::nn_gru(input_size,s$hidden_size,num_layers=s$num_layers,
      bias=s$gru_bias,batch_first=TRUE,dropout=s$gru_dropout,bidirectional=FALSE)
    after_reference <- torch::torch_get_rng_state()
    on.exit(torch::torch_set_rng_state(after_reference),add=TRUE)
  }
  self <- NULL
  definition <- torch::nn_module(
    initialize=function() {
      # Upper layers concatenate the lower layer's output and the original
      # interaction. Their GRU input matrices therefore gain extra columns, one
      # learned input-to-gates matrix per upper layer; no extra biases are added.
      self$layers <- torch::nn_module_list(lapply(seq_len(s$num_layers),function(layer)
        torch::nn_gru(input_size+if(layer>1L)s$hidden_size else 0L,s$hidden_size,
          num_layers=1L,bias=s$gru_bias,batch_first=TRUE,bidirectional=FALSE)))
      self$between <- torch::nn_dropout(s$gru_dropout)
    },
    forward=function(interaction,hidden=NULL) {
      states <- vector('list',s$num_layers);output <- interaction
      for(layer in seq_len(s$num_layers)) {
        # Reuse the original interaction, including its already applied input
        # dropout. Between-layer dropout affects only the lower-layer output.
        input <- if(layer==1L)interaction else
          torch::torch_cat(list(self$between(output),interaction),dim=3)
        prior <- if(is.null(hidden))NULL else hidden[layer,,,drop=FALSE]
        result <- self$layers[[layer]](input,prior)
        output <- result[[1L]];states[[layer]] <- result[[2L]]
      }
      list(output,torch::torch_cat(states,dim=1))
    })
  net <- definition()
  if(!is.null(reference)) {
    mapped <- lkt_gru_expand_input_weights(lkt_gru_weights(reference),lkt_gru_weights(net))
    lkt_gru_load_weights(net,mapped,s)
    reference <- NULL
  }
  net
}
