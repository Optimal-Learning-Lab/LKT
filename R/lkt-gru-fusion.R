# Resolve widths from the saved input schema before allocating weights. Addition
# requires equal vector widths; incompatible inputs fail rather than being padded,
# truncated or silently projected into a different representation.
lkt_gru_input_layout <- function(prep,s) {
  time <- which(startsWith(as.character(prep$numeric_names),'time:'))
  history <- setdiff(seq_along(prep$numeric_names),time)
  learned <- s$feature_encoder=='mlp'
  sizes <- stats::setNames(rep(s$encoder_output_size,3L),c('time','history','response'))
  sizes[names(s$encoder_output_sizes)] <- s$encoder_output_sizes
  numeric_parts <- if(learned)c(if(length(time))sizes[['time']],if(length(history))sizes[['history']]) else length(prep$numeric_names)
  categories <- if(s$categorical_encoding=='embedding')prep$embedding_dims else lengths(prep$vocabularies)
  if(s$numeric_fusion=='sum' && (length(numeric_parts)!=2L || numeric_parts[1]!=numeric_parts[2]))
    stop('numeric_fusion sum requires both time and history encoders with equal output widths.',call.=FALSE)
  if(s$categorical_fusion=='sum' && length(unique(categories))!=1L)
    stop('categorical_fusion sum requires equal embedding widths.',call.=FALSE)
  numeric_width <- if(s$numeric_fusion=='sum')numeric_parts[1] else sum(numeric_parts)
  categorical_width <- if(s$categorical_fusion=='sum')categories[1] else sum(categories)
  if(s$input_fusion=='sum' && numeric_width!=categorical_width)
    stop('input_fusion sum requires equal temporal-history and item-coding widths.',call.=FALSE)
  combined_width <- as.integer(if(s$input_fusion=='sum')numeric_width else numeric_width+categorical_width)
  list(time=time,history=history,sizes=sizes,numeric_width=as.integer(numeric_width),
    categorical_width=as.integer(categorical_width),
    combined_width=combined_width,
    width=if(is.null(s$joint_encoder_output_size))combined_width else s$joint_encoder_output_size,
    response_width=if(learned)sizes[['response']] else 1L)
}

# Addition preserves the learned coordinates and their gradients. Concatenation
# preserves separate coordinates. Neither operation adds any trainable parameters.
lkt_gru_combine_inputs <- function(parts,mode) {
  if(length(parts)==1L)return(parts[[1L]])
  if(mode=='sum')Reduce(function(a,b)a+b,parts) else torch::torch_cat(parts,dim=3)
}
