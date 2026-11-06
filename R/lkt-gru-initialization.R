# Map an ordinary stack into the expanded stack without changing its function.
# Original columns and every shared tensor are copied; only added input columns
# are zero. The same mapping serves random initialization and trained warm starts.
lkt_gru_expand_input_weights <- function(source,target,prefix='') {
  pattern <- paste0('^',gsub('.','[.]',prefix,fixed=TRUE),
    'layers[.]([0-9]+)[.](weight_ih|weight_hh|bias_ih|bias_hh)_l1$')
  result <- target;used <- character()
  for(n in names(target)) {
    match <- regmatches(n,regexec(pattern,n))[[1L]]
    from <- n
    if(length(match))from <- paste0(prefix,match[3L],'_l',as.integer(match[2L])+1L)
    if(!from %in% names(source))stop('Initial GRU weights are missing ',from,'.',call.=FALSE)
    value <- source[[from]]
    if(length(match) && match[3L]=='weight_ih' && as.integer(match[2L])>0L) {
      extra <- ncol(target[[n]])-ncol(value)
      if(extra<1L)stop('Initial GRU input expansion is incompatible.',call.=FALSE)
      value <- cbind(value,matrix(0,nrow(value),extra))
    }
    if(!identical(dim(value),dim(target[[n]])) || length(value)!=length(target[[n]]))
      stop('Initial GRU tensor shape differs for ',n,'.',call.=FALSE)
    result[[n]] <- value;used <- c(used,from)
  }
  if(!setequal(used,names(source)))stop('Initial GRU weights contain incompatible tensors.',call.=FALSE)
  result
}

# Selected tables begin at a shared zero vector but still learn from item-specific
# observations. Allocate ordinary tables first so this option consumes exactly the
# same random draws and leaves all other initial weights unchanged.
lkt_gru_embeddings <- function(prep,s) {
  tables <- lapply(if(s$categorical_encoding=='embedding')seq_along(prep$embedding_dims) else integer(),function(j) {
    table <- torch::nn_embedding(length(prep$vocabularies[[j]])+1L,prep$embedding_dims[j],padding_idx=1L)
    if(names(prep$vocabularies)[j] %in% s$zero_init_embedding_cols)torch::nn_init_zeros_(table$weight)
    table
  })
  torch::nn_module_list(tables)
}

# Warm starts are an explicit expansion of an ordinary fitted model on the same
# prepared rows and partitions. Inference uses the saved resulting arrays and
# never needs the initialization file to remain present.
lkt_gru_initialize_model <- function(net,file,data,masks,recipe,prep,s) {
  if(!file.exists(file))stop('Initial GRU model file does not exist: ',file,call.=FALSE)
  source_fit <- readRDS(file);source_model <- lkt_sequence_model(source_fit)
  old <- lkt_sequence_gru_settings(source_model$settings)
  if(old$input_to_all_layers || !s$input_to_all_layers || s$additional_input_init!='zero')
    stop('Initial GRU model requires first-layer-only source and zero additional input connections.',call.=FALSE)
  # Training controls may change, while architecture and input meanings must match.
  controls <- c('epochs','batch_size','sequence_length','optimizer','learning_rate','weight_decay',
    'optimizer_options','gradient_clip','gradient_clip_norm','positive_weight','weight_col',
    'validation_folds','validation_fraction','split_seed','early_stopping','patience','min_delta',
    'restore_best','scheduler','scheduler_options','seed','num_threads','device','deterministic',
    'input_dropout','gru_dropout','head_dropout','encoder_dropout','inference_dropout','retrieval_dropout',
    'input_to_all_layers','additional_input_init','initial_model_file','retrieval_decay_lr_multiplier')
  fixed <- setdiff(names(s),controls)
  # Recomputing the same training means on another R/BLAS build can differ by
  # a few rounding bits. Permit only machine-scale numeric noise; the structural
  # contract stays exact and a separate replay check verifies the full function.
  same_preprocessing <- isTRUE(all.equal(source_model$preprocessing,prep,
    tolerance=32*.Machine$double.eps))
  if(!identical(old[fixed],s[fixed]) || !same_preprocessing ||
     !identical(source_model$recipe,recipe) || length(source_fit$prediction)!=nrow(data) ||
     !identical(source_fit$fit_rows,which(masks$train)) ||
     !identical(source_fit$validation_rows,which(masks$validation)) ||
     !identical(source_fit$evaluation_rows,which(masks$evaluation)))
    stop('Initial GRU model architecture, preprocessing, recipe or fitting partitions differ.',call.=FALSE)
  mapped <- lkt_gru_expand_input_weights(source_model$weights,lkt_gru_weights(net),'gru.')
  lkt_gru_load_weights(net,mapped,s)
  list(source_file=file,source_md5=unname(tools::md5sum(file)),
    source_selected_epoch=source_fit$selected_epoch,source_prediction=source_fit$prediction)
}
