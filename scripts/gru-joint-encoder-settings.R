# Freeze one small shared encoder. Its hidden width is fixed; the output width
# comes solely from matching the baseline parameter budget, not from AUC tuning.
joint_encoder_settings <- function(base,prep,parameter_count) {
  stopifnot(base$feature_encoder=='mlp',base$inference_mode=='none',
    base$encoder_output_size==16L,all(prep$embedding_dims==16L),
    length(prep$vocabularies)==2L,base$hidden_size==32L,base$num_layers==3L,
    identical(base$head_hidden_sizes,128L),base$retrieval_heads==1L,
    base$retrieval_size==32L,!base$input_to_all_layers,
    !length(base$encoder_output_sizes),!length(base$retrieval_cue_cols),
    is.null(base$joint_encoder_output_size),base$numeric_fusion=='concat',
    base$categorical_fusion=='concat',base$input_fusion=='concat')
  hidden <- 32L;input <- 64L
  downstream <- 3L*base$hidden_size+base$head_hidden_sizes[1L]+base$retrieval_size
  ideal <- (downstream*input-(input+1L)*hidden)/(downstream+hidden+1L)
  widths <- unique(as.integer(c(floor(ideal),ceiling(ideal))))
  count <- function(out)parameter_count+(input+1L)*hidden+(hidden+1L)*out+downstream*(out-input)
  output <- widths[which.min(abs(vapply(widths,count,numeric(1))-parameter_count))]
  s <- base;s$joint_encoder_output_size <- output;s$joint_encoder_hidden_sizes <- hidden
  s$joint_encoder_activation <- 'relu';s$joint_encoder_dropout <- 0
  settings <- list(baseline=base,joint_encoder=do.call(GRUModel,s)$settings)
  counts <- data.frame(condition=names(settings),source_input_width=input,
    hidden_width=c(NA_integer_,hidden),input_width=c(input,output),
    parameter_count=c(parameter_count,count(output)),difference=c(0L,count(output)-parameter_count))
  counts$relative_difference <- counts$difference/parameter_count
  stopifnot(output==49L,max(abs(counts$relative_difference))<.005)
  list(settings=settings,counts=counts)
}
