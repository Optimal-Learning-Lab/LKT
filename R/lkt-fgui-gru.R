# Scalar form fields use a documented data-only representation. User text is
# never parsed/evaluated as R code.
# Represent settings as editable text without turning that text into R code. This allows
# vectors and named controls to round-trip through ordinary GUI fields.
lkt_gru_gui_encode <- function(settings) {
  vapply(settings,function(x) {
    if(is.null(x))return('')
    if(is.list(x))return(paste(vapply(names(x),function(n)paste(n,paste(x[[n]],collapse=','),sep='|'),character(1)),collapse='\n'))
    if(!is.null(names(x)))return(paste(names(x),x,sep='|',collapse='\n'))
    paste(x,collapse=',')
  },character(1))
}

# Parse each field according to the public control's expected type. The result passes
# through the same GRU validator used by scripted calls.
lkt_gru_gui_decode <- function(values) {
  defaults <- lkt_gru_defaults()
  nullable <- c('time_col','order_col','weight_col','sequence_length','gradient_clip','validation_folds','retrieval_window','initial_model_file','joint_encoder_output_size')
  numeric_vectors <- c('head_hidden_sizes','inference_hidden_sizes','encoder_hidden_sizes','encoder_output_sizes','joint_encoder_hidden_sizes','logvar_bounds','embedding_dims','validation_folds')
  character_vectors <- c('categorical_cols','numeric_cols','history_group_cols','time_feature_cols','time_missing_cols',
    'retrieval_filter_cols','retrieval_cue_cols','zero_init_embedding_cols')
  result <- list()
  tokens <- function(x)trimws(strsplit(x,',',fixed=TRUE)[[1L]])
  table <- function(x) {
    if(!nzchar(trimws(x)))return(list())
    rows <- strsplit(trimws(x),'\n',fixed=TRUE)[[1L]]
    pairs <- lapply(rows,function(row)trimws(strsplit(row,'|',fixed=TRUE)[[1L]]))
    if(any(lengths(pairs)!=2L))stop('Named GRU values require name|value, one per line.',call.=FALSE)
    names <- vapply(pairs,`[[`,character(1),1L)
    if(anyDuplicated(names)||any(!nzchar(names)))stop('Duplicate or empty GRU setting names.',call.=FALSE)
    stats::setNames(lapply(pairs,`[[`,2L),names)
  }
  for(n in names(defaults)) {
    x <- trimws(as.character(values[[n]]))
    if(!nzchar(x) && n %in% nullable) {result[n] <- list(NULL);next}
    if(n %in% c('optimizer_options','scheduler_options')) {
      rows <- table(x)
      result[[n]] <- lapply(rows,function(v) {
        if(v %in% c('TRUE','FALSE'))return(v=='TRUE')
        as.numeric(tokens(v))
      })
    } else if(n=='numeric_transforms')result[[n]] <- unlist(table(x),use.names=TRUE)
    else if(n %in% character_vectors)result[[n]] <- if(nzchar(x))tokens(x) else character()
    else if(n %in% numeric_vectors) {
      if(grepl('|',x,fixed=TRUE))result[[n]] <- vapply(table(x),as.numeric,numeric(1))
      else result[[n]] <- if(nzchar(x))as.numeric(tokens(x)) else integer()
    } else if(is.logical(defaults[[n]])) {
      if(!x %in% c('TRUE','FALSE','0','1'))stop('GRU logical controls require TRUE/FALSE.',call.=FALSE)
      result[[n]] <- x %in% c('TRUE','1')
    } else if(is.numeric(defaults[[n]]) || n %in% c('sequence_length','gradient_clip','joint_encoder_output_size'))result[[n]] <- as.numeric(x)
    else result[[n]] <- x
  }
  # Empty named transformations are a character vector, not NULL.
  if(is.null(result$numeric_transforms))result$numeric_transforms <- character()
  lkt_gru_settings(result)
}

# Organize the full set of GRU controls into related tabs. The editor returns validated
# settings rather than launching a separate fitting implementation.
lkt_gru_gui <- function(settings=NULL,theme='blue') {
  settings <- lkt_gru_settings(if(is.null(settings))list() else settings)
  selected <- new.env(parent=emptyenv());selected$value <- NULL
  controller <- function() {
    selected$value <- lkt_gru_gui_decode(as.list(environment()))
    tcltk::tkdestroy(fgui::guiGet('MAIN'));NULL
  }
  formals(controller) <- as.pairlist(as.list(lkt_gru_gui_encode(settings)))
  choices <- list(joint_encoder_activation=c('relu','tanh','gelu'),numeric_fusion=c('concat','sum'),categorical_fusion=c('concat','sum'),input_fusion=c('concat','sum'),additional_input_init=c('random','zero'),retrieval_fusion=c('replace','concat'),retrieval_decay=c('none','exponential','akt'),feature_encoder=c('none','mlp'),encoder_activation=c('relu','tanh','gelu'),head_activation=c('relu','tanh','gelu'),categorical_encoding=c('embedding','one_hot'),
    optimizer=c('adamw','adam','sgd'),scheduler=c('none','plateau'),time_units=c('seconds','minutes','hours','days'),
    dtype=c('float32','float64'),unknown_levels=c('error','zero'),
    inference_mode=c('none','deterministic','gaussian'),inference_activation=c('relu','tanh','gelu'))
  for(n in names(settings)[vapply(settings,is.logical,logical(1))])choices[[n]] <- c('TRUE','FALSE')
  sections <- list(
    Inputs=c('categorical_cols','numeric_cols','categorical_encoding','embedding_dims','numeric_transforms','unknown_levels'),
    History=c('subject_col','history_group_cols','order_col','time_col','time_units','time_feature_cols','time_missing_cols','use_lkt_features'),
    Architecture=c('hidden_size','num_layers','gru_bias','input_to_all_layers','head_hidden_sizes','head_activation','head_bias'),
    Initialization=c('additional_input_init','initial_model_file','zero_init_embedding_cols'),
    'Feature encoding'=c('feature_encoder','encoder_hidden_sizes','encoder_output_size','encoder_output_sizes','encoder_activation','encoder_dropout',
      'numeric_fusion','categorical_fusion','input_fusion'),
    'Joint input encoder'=c('joint_encoder_output_size','joint_encoder_hidden_sizes','joint_encoder_activation','joint_encoder_dropout'),
    Retrieval=c('retrieval_heads','retrieval_size','retrieval_window','retrieval_dropout','retrieval_fusion',
      'retrieval_decay','retrieval_decay_init','retrieval_share_qk','retrieval_bias','retrieval_filter_cols',
      'retrieval_cue_cols','retrieval_decay_lr_multiplier'),
    Inference=c('inference_mode','latent_size','inference_hidden_sizes','inference_activation','inference_dropout',
      'temporal_kl_beta','logvar_bounds','logvar_init'),
    Regularization=c('input_dropout','gru_dropout','head_dropout','standardize','positive_weight','weight_col'),
    Optimization=c('optimizer','learning_rate','weight_decay','optimizer_options','gradient_clip','gradient_clip_norm'),
    Sequences=c('epochs','batch_size','sequence_length','seed'),
    Validation=c('validation_folds','validation_fraction','split_seed','early_stopping','patience','min_delta','restore_best'),
    Scheduling=c('scheduler','scheduler_options'),
    Runtime=c('device','dtype','deterministic','num_threads','clip_epsilon'))
  labels <- stats::setNames(as.list(gsub('_',' ',names(settings),fixed=TRUE)),names(settings))
  labels$embedding_dims <- 'Embedding widths (scalar or column|width lines)'
  labels$encoder_output_sizes <- 'Output widths: time|width, history|width, response|width (blank = common)'
  labels$numeric_fusion <- 'Time and history: concatenate or sum'
  labels$categorical_fusion <- 'Item/skill embeddings: concatenate or sum'
  labels$input_fusion <- 'Temporal history and item coding: concatenate or sum'
  labels$joint_encoder_output_size <- 'Shared input representation width (blank = disabled)'
  labels$joint_encoder_hidden_sizes <- 'Shared input hidden widths (comma-separated; blank = linear)'
  labels$head_hidden_sizes <- 'Head hidden widths (comma-separated; blank = linear)'
  labels$input_to_all_layers <- 'Direct input connections to every GRU layer'
  labels$additional_input_init <- 'Additional input weights: random or zero'
  labels$initial_model_file <- 'Initial fitted GRU RDS (blank = train from scratch)'
  labels$history_group_cols <- 'History keys (comma-separated; include student ID)'
  labels$retrieval_heads <- 'Attention heads (0 = off)'
  labels$retrieval_size <- 'Total attention width (divisible by heads)'
  labels$retrieval_window <- 'Prior interactions retained (blank = all)'
  labels$retrieval_filter_cols <- 'Match ANY identity column (comma-separated; blank = unrestricted)'
  labels$retrieval_cue_cols <- 'Cue from categorical columns only (blank = state plus all inputs)'
  labels$zero_init_embedding_cols <- 'Trainable embedding tables starting at zero (comma-separated)'
  labels$retrieval_decay_lr_multiplier <- 'Decay learning rate multiplier'
  lkt_form_gui(controller,title='GRU settings',theme=theme,modal=TRUE,sections=sections,choices=choices,
    subtitle='All settings are saved. Vectors use commas; named values use name|value lines. Blank optional fields mean NULL. No training runs in this editor.',
    labels=labels,edit=list(optimizer_options=c(55,4,FALSE),scheduler_options=c(55,4,FALSE),
      embedding_dims=c(55,2,FALSE),encoder_output_sizes=c(55,3,FALSE),numeric_transforms=c(55,2,FALSE)),submit_label='Use GRU settings')
  selected$value
}
