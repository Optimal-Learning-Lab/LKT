#' Configurable recurrent knowledge tracing
#'
#' GRUModel predicts from the previous hidden state and the current predictors,
#' then updates the state with the observed response. All controls are explicit;
#' see the GRU vignette for input, split, and continuation semantics.
#' Defaults use three 32-unit GRU layers, a 128-unit prediction head, separate
#' 16-value feature encoders, item/skill embeddings, filtered one-head retrieval
#' and zero-initialized trainable item embeddings. Explicit arguments override
#' these ordinary defaults. LKT itself still defaults to LibLinearModel.
#' @param hidden_size Width of each recurrent layer.
#' @param num_layers Number of recurrent layers.
#' @param gru_bias Include recurrent biases.
#' @param head_hidden_sizes Hidden widths in the prediction MLP; integer(0) is linear.
#' @param head_activation relu, tanh, or gelu.
#' @param head_bias Include prediction-head biases.
#' @param categorical_cols Raw categorical predictor columns.
#' @param numeric_cols Raw numeric predictor columns.
#' @param categorical_encoding embedding or one_hot.
#' @param embedding_dims Positive width shared across categorical columns, or a named width per column.
#' @param input_dropout,gru_dropout,head_dropout Dropout probabilities. Recurrent dropout requires multiple layers.
#' @param standardize Standardize numeric and engineered columns using training rows.
#' @param numeric_transforms Named character vector mapping numeric columns to identity or log1p.
#' @param use_lkt_features Append the configured, fixed-parameter LKT features.
#' @param time_col Optional numeric/POSIXct timestamp column.
#' @param time_units seconds, minutes, hours, or days for numeric timestamps.
#' @param time_feature_cols Additional nonnegative timing columns in seconds, computed
#'   before removing unscored exposures. Each receives log1p and training-only scaling,
#'   and enters the same encoder as recency. See LKTGRUTimeFeatures.
#' @param time_missing_cols Subset of time_feature_cols allowed to be missing. Each
#'   adds an availability indicator to the time encoder; missing values do not enter
#'   scaling estimates and have a neutral internal value only after scaling.
#' @param subject_col Student identifier used for validation splitting.
#' @param history_group_cols History keys; must include subject_col. Add a KC column for separate KC histories.
#' @param order_col Optional numeric/POSIXct ordering column; ties retain input order.
#' @param epochs Maximum training epochs.
#' @param batch_size Number of history groups per batch.
#' @param sequence_length Backpropagation chunk length; NULL uses full histories.
#' @param optimizer adamw, adam, or sgd.
#' @param learning_rate Initial learning rate.
#' @param weight_decay Optimizer weight decay.
#' @param optimizer_options Named optimizer-specific controls: betas, eps, amsgrad for Adam; momentum, dampening, nesterov for SGD.
#' @param gradient_clip Maximum gradient norm, or NULL to disable.
#' @param gradient_clip_norm Norm type, positive numeric or Inf.
#' @param positive_weight Positive-class training weight.
#' @param weight_col Optional positive per-trial training-weight column.
#' @param validation_folds Explicit validation folds, disjoint from fitting folds.
#' @param validation_fraction Fraction of fitting students reserved when explicit validation folds are absent; zero disables splitting.
#' @param split_seed Student split seed.
#' @param early_stopping Stop on validation log loss.
#' @param patience Epochs without sufficient validation improvement before stopping.
#' @param min_delta Required absolute validation improvement.
#' @param restore_best Restore the best validation checkpoint.
#' @param scheduler none or plateau.
#' @param scheduler_options Named plateau controls: factor, patience, min_lr, threshold.
#' @param seed Training seed; caller RNG state is restored.
#' @param device cpu or an explicit CUDA device, e.g. cuda:0.
#' @param dtype float32 or float64.
#' @param deterministic Require reproducible CPU execution. Deterministic CUDA is not supported by the R runtime.
#' @param num_threads Torch CPU threads, restored after execution.
#' @param unknown_levels error or explicitly zero-encode unknown categories.
#' @param clip_epsilon Probability clipping for LKT outputs, not the training loss.
#' @param inference_mode none, deterministic, or gaussian. Inference modes summarize older memory and the latest completed interaction separately from recurrent memory.
#' @param latent_size Width of the separate student representation.
#' @param inference_hidden_sizes Hidden widths of the inference network; integer(0) is linear.
#' @param inference_activation relu, tanh, or gelu for the inference hidden layers.
#' @param inference_dropout Dropout after inference hidden layers.
#' @param temporal_kl_beta Weight of mean-reduced KL(new distribution || previous distribution); requires gaussian mode.
#' @param logvar_bounds Two finite bounds for the predicted log variance.
#' @param logvar_init Initial log-variance bias, with zero initial log-variance weights.
#' @param feature_encoder none or mlp. Optional separate response, time and numeric-auxiliary encoders.
#' @param encoder_hidden_sizes Hidden widths shared by each feature encoder; integer(0) is linear.
#' @param encoder_output_size Output width of each feature encoder.
#' @param encoder_output_sizes Named positive widths overriding encoder_output_size
#'   for time, history, or response. integer(0) uses the common width.
#' @param joint_encoder_output_size Optional output width of one shared MLP after
#'   combining the current input blocks. NULL disables it and preserves the input.
#' @param joint_encoder_hidden_sizes Hidden widths of that shared MLP; integer(0)
#'   gives a single learned linear map.
#' @param joint_encoder_activation relu, tanh, or gelu in the shared input MLP.
#' @param joint_encoder_dropout Dropout after shared input hidden layers.
#' @param numeric_fusion concat or sum for the encoded time and history values.
#' @param categorical_fusion concat or sum for categorical embedding vectors.
#' @param input_fusion concat joins the numeric and categorical groups; sum adds
#'   the two group sums. Fusion requires MLP encoders, direct prediction, and the
#'   full retrieval cue. Addition introduces no projection or extra network.
#' @param encoder_activation relu, tanh, or gelu for feature encoder hidden layers.
#' @param encoder_dropout Dropout after feature encoder hidden layers.
#' @param retrieval_heads Number of attention heads; zero disables retrieval.
#' @param retrieval_size Total query/key/value width, divisible by retrieval_heads.
#' @param retrieval_window Maximum completed interactions retained for retrieval;
#'   NULL retains the full history independently of sequence_length.
#' @param retrieval_dropout Dropout on attention weights during training.
#' @param retrieval_fusion replace substitutes retrieved memory for the usual memory
#'   read; concat supplies both. GRU memory updates remain unchanged.
#' @param retrieval_decay none, exponential (position penalty), or akt (context-aware
#'   score decay). Distances count interactions, not clock seconds.
#' @param retrieval_decay_init Initial positive decay rate for each attention head.
#' @param retrieval_share_qk Share the learned query and key projection.
#' @param retrieval_bias Include biases in retrieval projections.
#' @param retrieval_filter_cols Identity columns used to restrict retrieval. An
#' earlier observation is eligible when any configured column matches the current
#' observation exactly. character(0) leaves retrieval unrestricted within a student.
#' @param retrieval_cue_cols Categorical columns whose representations alone form
#' retrieval queries and keys. character(0) uses prior GRU state plus all current inputs.
#' @param zero_init_embedding_cols Categorical embedding tables initialized at zero;
#' these remain trainable. Other tables retain ordinary random initialization.
#' @param retrieval_decay_lr_multiplier Learning-rate multiplier for the learnable
#' retrieval decay parameter. Ordinary network weights keep learning_rate.
#' @param input_to_all_layers Connect the original predictor-and-answer interaction
#'   directly to every GRU layer, alongside the preceding layer's output.
#' @param additional_input_init Initialize added upper-layer input weights randomly
#'   or at zero while preserving all ordinary initial weights.
#' @param initial_model_file Optional RDS containing an ordinary fitted GRU LKT
#'   object on the same data and folds. Copy its weights into a zero-input expansion.
#' @return GRUModel returns an LKT model adapter. lkt_gru_defaults returns all resolved default settings without requiring Torch.
#' @export
GRUModel <- function(hidden_size=32L, num_layers=3L, gru_bias=TRUE,
                     head_hidden_sizes=128L, head_activation='relu', head_bias=TRUE,
                     categorical_cols=c('KC..Default.','KC..Cluster.'), numeric_cols=character(),
                     categorical_encoding='embedding', embedding_dims=16L,
                     input_dropout=.1, gru_dropout=.1, head_dropout=.1,
                     standardize=TRUE, numeric_transforms=character(),
                     use_lkt_features=TRUE, time_col='CF..Time.', time_units='seconds',
                     subject_col='Anon.Student.Id', history_group_cols='Anon.Student.Id', order_col='CF..Time.',
                     epochs=100L, batch_size=32L, sequence_length=100L,
                     optimizer='adamw', learning_rate=.001, weight_decay=1e-4, optimizer_options=list(),
                     gradient_clip=5, gradient_clip_norm=2, positive_weight=1, weight_col=NULL,
                     validation_folds=NULL, validation_fraction=.1, split_seed=1L,
                     early_stopping=TRUE, patience=10L, min_delta=1e-4, restore_best=TRUE,
                     scheduler='none', scheduler_options=list(), seed=1L,
                     device='cpu', dtype='float32', deterministic=TRUE, num_threads=1L,
                     unknown_levels='zero', clip_epsilon=1e-5,
                     inference_mode='none', latent_size=32L, inference_hidden_sizes=32L,
                     inference_activation='relu', inference_dropout=0,
                     temporal_kl_beta=0, logvar_bounds=c(-10,4), logvar_init=-2,
                     feature_encoder='mlp',encoder_hidden_sizes=32L,encoder_output_size=16L,
                     encoder_activation='relu',encoder_dropout=0,
                     time_feature_cols=c('gru_item_age_seconds','gru_mean_prior_spacing_seconds'),
                     time_missing_cols='gru_mean_prior_spacing_seconds',
                     retrieval_heads=1L,retrieval_size=32L,retrieval_window=100L,
                     retrieval_dropout=0,retrieval_fusion='replace',retrieval_decay='none',
                     retrieval_decay_init=.1,retrieval_share_qk=TRUE,retrieval_bias=TRUE,
                     input_to_all_layers=FALSE,additional_input_init='random',initial_model_file=NULL,
                     retrieval_filter_cols=c('KC..Default.','KC..Cluster.'),retrieval_cue_cols=character(),
                     zero_init_embedding_cols='KC..Default.',retrieval_decay_lr_multiplier=1,
                     encoder_output_sizes=integer(),numeric_fusion='concat',
                     categorical_fusion='concat',input_fusion='concat',
                     joint_encoder_output_size=NULL,joint_encoder_hidden_sizes=32L,
                     joint_encoder_activation='relu',joint_encoder_dropout=0) {
  # Resolve and validate the requested controls once before creating the model adapter.
  # The saved settings then describe the same network for fitting, prediction, GUI
  # editing, and serialization.
  settings <- lkt_validate_gru(as.list(environment()))
  adapter <- LKTCustomModel('GRU', function(request, verbose=FALSE) {
    lkt_fit_gru(request, settings, verbose)
  })
  adapter$settings <- settings
  adapter
}

#' @rdname GRUModel
#' @export
lkt_gru_defaults <- function() {
  lapply(formals(GRUModel), eval, envir=baseenv())
}

lkt_gru_settings <- function(options=list()) {
  lkt_gru_named(options, names(lkt_gru_defaults()), 'GRU options')
  do.call(GRUModel, options)$settings
}

lkt_gru_named <- function(x, allowed, label) {
  if (!is.list(x) || (length(x) && (is.null(names(x)) || anyNA(names(x)) ||
      any(!nzchar(names(x))) || anyDuplicated(names(x)) || any(!names(x) %in% allowed))))
    stop(label, ' must be a named list of supported settings.', call.=FALSE)
}

# Reject unsupported settings before allocating a network or starting a fit. Central
# validation keeps the command-line and GUI entry points consistent.
lkt_validate_gru <- function(s) {
  number <- function(n, lower=0, upper=Inf, integer=FALSE, strict=FALSE, nullable=FALSE) {
    x <- s[[n]]
    if (nullable && is.null(x)) return(invisible(NULL))
    if (!is.numeric(x) || length(x)!=1L || is.na(x) || !is.finite(x) ||
        (if(strict) x<=lower else x<lower) || x>upper || (integer && x!=floor(x)))
      stop('Invalid GRU ', n, '.', call.=FALSE)
    if(integer) s[[n]] <<- as.integer(x)
  }
  for(n in c('hidden_size','num_layers','epochs','batch_size','patience','num_threads','latent_size','encoder_output_size','retrieval_size'))
    number(n,1,.Machine$integer.max,TRUE)
  number('sequence_length',1,.Machine$integer.max,TRUE,nullable=TRUE)
  number('joint_encoder_output_size',1,.Machine$integer.max,TRUE,nullable=TRUE)
  number('retrieval_heads',0,.Machine$integer.max,TRUE)
  number('retrieval_window',1,.Machine$integer.max,TRUE,nullable=TRUE)
  number('retrieval_decay_init',strict=TRUE)
  number('retrieval_decay_lr_multiplier',strict=TRUE)
  if(s$retrieval_heads>0L && s$retrieval_size %% s$retrieval_heads!=0L)
    stop('retrieval_size must be divisible by retrieval_heads.',call.=FALSE)
  for(n in c('seed','split_seed')) number(n,0,.Machine$integer.max,TRUE)
  for(n in c('learning_rate','positive_weight')) number(n,strict=TRUE)
  for(n in c('weight_decay','min_delta','temporal_kl_beta')) number(n)
  number('gradient_clip',strict=TRUE,nullable=TRUE)
  if(!is.numeric(s$gradient_clip_norm) || length(s$gradient_clip_norm)!=1L ||
      is.na(s$gradient_clip_norm) || s$gradient_clip_norm<=0) stop('Invalid GRU gradient_clip_norm.',call.=FALSE)
  for(n in c('input_dropout','gru_dropout','head_dropout','validation_fraction','inference_dropout','encoder_dropout','retrieval_dropout','joint_encoder_dropout')) {
    number(n,0,1)
    if(s[[n]]==1) stop(n,' must be less than one.',call.=FALSE)
  }
  number('clip_epsilon',strict=TRUE,upper=.5)
  if(s$clip_epsilon==.5) stop('clip_epsilon must be less than .5.',call.=FALSE)
  for(n in c('gru_bias','head_bias','standardize','use_lkt_features','early_stopping','restore_best','deterministic','retrieval_share_qk','retrieval_bias','input_to_all_layers'))
    if(!is.logical(s[[n]]) || length(s[[n]])!=1L || is.na(s[[n]])) stop(n,' must be TRUE or FALSE.',call.=FALSE)
  choices <- list(joint_encoder_activation=c('relu','tanh','gelu'),numeric_fusion=c('concat','sum'),categorical_fusion=c('concat','sum'),input_fusion=c('concat','sum'),additional_input_init=c('random','zero'),retrieval_fusion=c('replace','concat'),retrieval_decay=c('none','exponential','akt'),feature_encoder=c('none','mlp'),encoder_activation=c('relu','tanh','gelu'),head_activation=c('relu','tanh','gelu'),categorical_encoding=c('embedding','one_hot'),
    time_units=c('seconds','minutes','hours','days'),optimizer=c('adamw','adam','sgd'),
    scheduler=c('none','plateau'),dtype=c('float32','float64'),unknown_levels=c('error','zero'),
    inference_mode=c('none','deterministic','gaussian'),inference_activation=c('relu','tanh','gelu'))
  for(n in names(choices)) {
    if(!is.character(s[[n]]) || length(s[[n]])!=1L || is.na(s[[n]]) || !s[[n]] %in% choices[[n]])
      stop('Invalid GRU ',n,'.',call.=FALSE)
  }
  if(s$num_layers==1L && s$gru_dropout>0) stop('gru_dropout requires num_layers > 1.',call.=FALSE)
  if(!is.null(s$initial_model_file) && (!is.character(s$initial_model_file) || length(s$initial_model_file)!=1L ||
     is.na(s$initial_model_file) || !nzchar(s$initial_model_file) ||
     !s$input_to_all_layers || s$additional_input_init!='zero'))
    stop('initial_model_file requires a model path, input_to_all_layers TRUE and additional_input_init zero.',call.=FALSE)
  if(!is.numeric(s$head_hidden_sizes) || anyNA(s$head_hidden_sizes) ||
      any(!is.finite(s$head_hidden_sizes) | s$head_hidden_sizes<1 | s$head_hidden_sizes>.Machine$integer.max | s$head_hidden_sizes!=floor(s$head_hidden_sizes)))
    stop('head_hidden_sizes must contain positive integer widths, or integer(0).',call.=FALSE)
  s$head_hidden_sizes <- as.integer(s$head_hidden_sizes)
  if(!is.numeric(s$inference_hidden_sizes) || anyNA(s$inference_hidden_sizes) ||
     any(!is.finite(s$inference_hidden_sizes) | s$inference_hidden_sizes<1 |
       s$inference_hidden_sizes>.Machine$integer.max | s$inference_hidden_sizes!=floor(s$inference_hidden_sizes)))
    stop('inference_hidden_sizes must contain positive integer widths, or integer(0).',call.=FALSE)
  s$inference_hidden_sizes <- as.integer(s$inference_hidden_sizes)
  if(!is.numeric(s$encoder_hidden_sizes) || anyNA(s$encoder_hidden_sizes) ||
      any(!is.finite(s$encoder_hidden_sizes) | s$encoder_hidden_sizes<1 |
        s$encoder_hidden_sizes>.Machine$integer.max | s$encoder_hidden_sizes!=floor(s$encoder_hidden_sizes)))
    stop('encoder_hidden_sizes must contain positive integer widths, or integer(0).',call.=FALSE)
  s$encoder_hidden_sizes <- as.integer(s$encoder_hidden_sizes)
  if(!is.numeric(s$joint_encoder_hidden_sizes) || anyNA(s$joint_encoder_hidden_sizes) ||
     any(!is.finite(s$joint_encoder_hidden_sizes) | s$joint_encoder_hidden_sizes<1 |
       s$joint_encoder_hidden_sizes>.Machine$integer.max |
       s$joint_encoder_hidden_sizes!=floor(s$joint_encoder_hidden_sizes)))
    stop('joint_encoder_hidden_sizes must contain positive integer widths, or integer(0).',call.=FALSE)
  s$joint_encoder_hidden_sizes <- as.integer(s$joint_encoder_hidden_sizes)
  if(!is.null(s$joint_encoder_output_size) && (s$feature_encoder!='mlp' ||
     s$inference_mode!='none' || length(s$retrieval_cue_cols)))
    stop('The joint input encoder requires MLP feature encoders, direct prediction and the full retrieval cue.',call.=FALSE)
  widths <- s$encoder_output_sizes
  if(!is.numeric(widths) || anyNA(widths) || any(!is.finite(widths) | widths<1 |
     widths>.Machine$integer.max | widths!=floor(widths)) ||
     (length(widths) && (is.null(names(widths)) || anyNA(names(widths)) ||
       anyDuplicated(names(widths)) || !all(names(widths) %in% c('time','history','response')))))
    stop('encoder_output_sizes requires named positive widths for time, history, or response.',call.=FALSE)
  s$encoder_output_sizes <- stats::setNames(as.integer(widths),names(widths))
  if(length(widths) && s$feature_encoder!='mlp')
    stop('encoder_output_sizes requires feature_encoder mlp.',call.=FALSE)
  fused <- any(c(s$numeric_fusion,s$categorical_fusion,s$input_fusion)!='concat')
  if(fused && (s$feature_encoder!='mlp' || s$inference_mode!='none' || length(s$retrieval_cue_cols)))
    stop('Input fusion requires MLP encoders, inference_mode none and the full retrieval cue.',call.=FALSE)
  if(s$categorical_fusion=='sum' && (s$categorical_encoding!='embedding' || length(s$categorical_cols)<2L))
    stop('categorical_fusion sum requires at least two categorical embedding columns.',call.=FALSE)
  if(s$input_fusion=='sum' && (s$numeric_fusion!='sum' || s$categorical_fusion!='sum'))
    stop('input_fusion sum requires both numeric_fusion and categorical_fusion sum.',call.=FALSE)
  if(!is.numeric(s$logvar_bounds) || length(s$logvar_bounds)!=2L || any(!is.finite(s$logvar_bounds)) ||
     s$logvar_bounds[1]>=s$logvar_bounds[2] || min(s$logvar_bounds)< -20 || max(s$logvar_bounds)>20)
    stop('logvar_bounds must be increasing finite bounds within [-20,20].',call.=FALSE)
  if(!is.numeric(s$logvar_init) || length(s$logvar_init)!=1L || !is.finite(s$logvar_init) ||
     s$logvar_init<s$logvar_bounds[1] || s$logvar_init>s$logvar_bounds[2])
    stop('logvar_init must lie within logvar_bounds.',call.=FALSE)
  if(s$temporal_kl_beta>0 && s$inference_mode!='gaussian')
    stop('temporal_kl_beta requires gaussian inference_mode.',call.=FALSE)
  for(n in c('categorical_cols','numeric_cols','time_feature_cols','time_missing_cols','retrieval_filter_cols',
            'retrieval_cue_cols','zero_init_embedding_cols','history_group_cols','subject_col','order_col','time_col','weight_col')) {
    x <- s[[n]]
    if(is.null(x) && n %in% c('order_col','time_col','weight_col')) next
    if(!is.character(x) || anyNA(x) || any(!nzchar(x)) || anyDuplicated(x) ||
       (n %in% c('subject_col','order_col','time_col','weight_col') && length(x)!=1L))
      stop('Invalid GRU ',n,'.',call.=FALSE)
  }
  if(!s$subject_col %in% s$history_group_cols) stop('history_group_cols must include subject_col.',call.=FALSE)
  if(length(s$retrieval_filter_cols) && s$retrieval_heads==0L)
    stop('retrieval_filter_cols requires retrieval_heads > 0.',call.=FALSE)
  if(any(s$retrieval_filter_cols %in% c('CF..ansbin.','Outcome','pred')))
    stop('Responses and predictions cannot be retrieval filter identifiers.',call.=FALSE)
  if(length(s$retrieval_cue_cols) && (s$retrieval_heads==0L || !all(s$retrieval_cue_cols %in% s$categorical_cols)))
    stop('retrieval_cue_cols requires active retrieval and configured categorical columns.',call.=FALSE)
  if(length(s$zero_init_embedding_cols) && (s$categorical_encoding!='embedding' ||
      !all(s$zero_init_embedding_cols %in% s$categorical_cols)))
    stop('zero_init_embedding_cols requires configured embedding columns.',call.=FALSE)
  if(s$retrieval_decay_lr_multiplier!=1 && (s$retrieval_heads==0L || s$retrieval_decay=='none'))
    stop('retrieval_decay_lr_multiplier requires active learnable retrieval decay.',call.=FALSE)
  if(!is.finite(s$learning_rate*s$retrieval_decay_lr_multiplier) || s$learning_rate*s$retrieval_decay_lr_multiplier<=0)
    stop('The retrieval decay learning rate must be finite and positive.',call.=FALSE)
  if(length(intersect(s$numeric_cols,s$categorical_cols))) stop('Numeric and categorical columns must be distinct.',call.=FALSE)
  if(length(intersect(s$time_feature_cols,c(s$numeric_cols,s$categorical_cols,s$time_col,'CF..ansbin.','Outcome','pred'))))
    stop('time_feature_cols must be distinct from other inputs, timestamps and responses.',call.=FALSE)
  if(!all(s$time_missing_cols %in% s$time_feature_cols))
    stop('time_missing_cols must be a subset of time_feature_cols.',call.=FALSE)
  if(any(c(s$categorical_cols,s$numeric_cols) %in% c('CF..ansbin.','Outcome','pred')))
    stop('Responses and fitted predictions cannot be current GRU predictors.',call.=FALSE)
  d <- s$embedding_dims
  if(!is.numeric(d) || !length(d) || anyNA(d) || any(!is.finite(d) | d<1 | d>.Machine$integer.max | d!=floor(d)) ||
      (length(d)>1 && (is.null(names(d)) || anyDuplicated(names(d)))) ||
      (!is.null(names(d)) && !setequal(names(d),s$categorical_cols)))
    stop('embedding_dims must be a positive integer or named widths for every categorical column.',call.=FALSE)
  tr <- s$numeric_transforms
  if(!is.character(tr) || (length(tr) && (is.null(names(tr)) || anyNA(names(tr)) || anyDuplicated(names(tr)) ||
      any(!names(tr) %in% s$numeric_cols) || anyNA(tr) || any(!tr %in% c('identity','log1p')))))
    stop('numeric_transforms must name numeric columns and identity/log1p transformations.',call.=FALSE)
  if(!is.null(s$validation_folds) && (!is.numeric(s$validation_folds) || !length(s$validation_folds) ||
      anyNA(s$validation_folds) || any(!is.finite(s$validation_folds)))) stop('Invalid validation_folds.',call.=FALSE)
  if(is.null(s$validation_folds) && s$validation_fraction==0 && (s$early_stopping || s$scheduler!='none'))
    stop('Early stopping and plateau scheduling require validation data.',call.=FALSE)
  opt <- if(s$optimizer=='sgd')list(momentum=0,dampening=0,nesterov=FALSE) else list(betas=c(.9,.999),eps=1e-8,amsgrad=FALSE)
  lkt_gru_named(s$optimizer_options,names(opt),'optimizer_options')
  opt[names(s$optimizer_options)] <- s$optimizer_options
  if(s$optimizer=='sgd') {
    if(any(!vapply(opt[c('momentum','dampening')],function(x)is.numeric(x)&&length(x)==1L&&is.finite(x)&&x>=0,logical(1))) ||
       !is.logical(opt$nesterov) || length(opt$nesterov)!=1L || is.na(opt$nesterov) ||
       (opt$nesterov && (opt$momentum<=0 || opt$dampening!=0))) stop('Invalid SGD options.',call.=FALSE)
  } else if(!is.numeric(opt$betas) || length(opt$betas)!=2 || anyNA(opt$betas) || any(opt$betas<0 | opt$betas>=1) ||
      !is.numeric(opt$eps) || length(opt$eps)!=1 || !is.finite(opt$eps) || opt$eps<=0 ||
      !is.logical(opt$amsgrad) || length(opt$amsgrad)!=1 || is.na(opt$amsgrad)) stop('Invalid Adam options.',call.=FALSE)
  s$optimizer_options <- opt
  sch <- list(factor=.5,patience=3L,min_lr=1e-6,threshold=1e-4)
  lkt_gru_named(s$scheduler_options,names(sch),'scheduler_options')
  sch[names(s$scheduler_options)] <- s$scheduler_options
  if(any(!vapply(sch,function(x)is.numeric(x)&&length(x)==1L&&is.finite(x),logical(1))) ||
     sch$factor<=0 || sch$factor>=1 || sch$patience<0 || sch$patience!=floor(sch$patience) ||
     sch$min_lr<0 || sch$min_lr>min(s$learning_rate,s$learning_rate*s$retrieval_decay_lr_multiplier) || sch$threshold<0)
    stop('Invalid scheduler_options.',call.=FALSE)
  s$scheduler_options <- sch
  if(!is.character(s$device) || length(s$device)!=1L || is.na(s$device) || !grepl('^(cpu|cuda:[0-9]+)$',s$device))
    stop('device must be cpu or an explicit cuda:N device.',call.=FALSE)
  if(s$device!='cpu' && s$deterministic) stop('Deterministic CUDA execution is unavailable; explicitly select deterministic = FALSE or CPU.',call.=FALSE)
  s
}

# Give each fit reproducible R and Torch random streams and a controlled thread count.
# The returned cleanup function restores the caller's process settings even when a fit
# fails.
lkt_gru_runtime <- function(s) {
  # Namespace/runtime startup can itself consume R randomness. Capture the
  # caller before loading Torch, and also restore it if initialization fails.
  had <- exists('.Random.seed',.GlobalEnv,inherits=FALSE)
  rng <- if(had)get('.Random.seed',.GlobalEnv) else NULL
  kind <- RNGkind();ready <- FALSE
  restore_r <- function() {
    do.call(RNGkind,as.list(kind))
    if(had)assign('.Random.seed',rng,.GlobalEnv) else if(exists('.Random.seed',.GlobalEnv,inherits=FALSE))rm('.Random.seed',envir=.GlobalEnv)
  }
  on.exit(if(!ready)restore_r(),add=TRUE)
  if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed())
    stop('GRU execution requires the optional torch package and its LibTorch runtime. Install torch, then run torch::install_torch().',call.=FALSE)
  if(s$device!='cpu' && (!torch::cuda_is_available() ||
      as.integer(sub('cuda:','',s$device))>=torch::cuda_device_count())) stop('Requested CUDA device is unavailable.',call.=FALSE)
  # Return cleanup to the caller so errors also restore process-wide settings.
  trng <- torch::torch_get_rng_state()
  crng <- if(s$device!='cpu')torch::cuda_get_rng_state() else NULL
  threads <- torch::torch_get_num_threads()
  set.seed(s$seed,kind='Mersenne-Twister',normal.kind='Inversion',sample.kind='Rejection')
  torch::torch_manual_seed(s$seed); torch::torch_set_num_threads(s$num_threads)
  ready <- TRUE
  function() {
    restore_r()
    torch::torch_set_rng_state(trng)
    if(!is.null(crng))torch::cuda_set_rng_state(crng)
    torch::torch_set_num_threads(threads)
  }
}

lkt_gru_tensor <- function(x,s,long=FALSE) {
  torch::torch_tensor(x,dtype=if(long)torch::torch_long() else if(s$dtype=='float32')torch::torch_float32() else torch::torch_float64(),device=s$device)
}

# Build the network from the saved input schema rather than guessing its dimensions from
# new data. The prediction head and recurrent update share the same encoded current
# predictors.
lkt_gru_network <- function(preprocessing,s) {
  if(s$inference_mode!='none' || s$feature_encoder!='none' || s$retrieval_heads>0L)return(lkt_gru_inference_network(preprocessing,s))
  self <- NULL # nn_module injects this binding into each method at runtime.
  dims <- preprocessing$embedding_dims
  catwidth <- if(s$categorical_encoding=='embedding')sum(dims) else sum(lengths(preprocessing$vocabularies))
  width <- catwidth + length(preprocessing$numeric_names)
  if(width<1L)stop('GRU requires at least one predictor.',call.=FALSE)
  definition <- torch::nn_module(
    initialize=function() {
      # Reserve the first embedding row for padding or an explicitly allowed unknown
      # category. Known KC identities receive their own learned vectors; the selected
      # augmented model has 72 known IDs and 16 embedding coordinates.
      self$embeddings <- lkt_gru_embeddings(preprocessing,s)
      self$input_drop <- torch::nn_dropout(s$input_dropout)
      # Append one observed-response coordinate to the encoded predictors for the memory
      # update. The GRU is forward-only, and each layer has its own recurrent state and
      # weights.
      self[[lkt_sequence_module_name(s)]] <- lkt_sequence_build(width+1L,s)
      # The prediction head receives the previous top-layer state together with current
      # predictors. Its final linear layer produces one logit; sigmoid conversion
      # happens when probabilities are requested.
      widths <- c(s$hidden_size+width,s$head_hidden_sizes,1L)
      layers <- list()
      for(j in seq_len(length(widths)-1L)) {
        layers[[length(layers)+1L]] <- torch::nn_linear(widths[j],widths[j+1L],bias=s$head_bias)
        if(j<length(widths)-1L) {
          layers[[length(layers)+1L]] <- switch(s$head_activation,relu=torch::nn_relu(),tanh=torch::nn_tanh(),gelu=torch::nn_gelu())
          layers[[length(layers)+1L]] <- torch::nn_dropout(s$head_dropout)
        }
      }
      self$head <- do.call(torch::nn_sequential,layers)
    },
    # Combine numeric columns with categorical embeddings in a stable order. Input
    # dropout is applied once here, so the prediction and update branches share the
    # resulting current-feature vector.
    encode=function(numeric,categorical) {
      parts <- list(numeric)
      for(j in seq_along(preprocessing$vocabularies)) {
        ids <- categorical[,,j,drop=FALSE]$squeeze(3)
        if(s$categorical_encoding=='embedding') parts[[length(parts)+1L]] <- self$embeddings[[j]](ids)
        else parts[[length(parts)+1L]] <- torch::nnf_one_hot(ids,num_classes=length(preprocessing$vocabularies[[j]])+1L)[,,seq.int(2L,length(preprocessing$vocabularies[[j]])+1L),drop=FALSE]$to(dtype=numeric$dtype)
      }
      self$input_drop(torch::torch_cat(parts,dim=3))
    },
    forward=function(numeric,categorical,answers,hidden=NULL) {
      x <- self$encode(numeric,categorical)
      batch <- x$size(1); steps <- x$size(2)
      # Start a new history with zero memory in every GRU layer. Histories carried from
      # an earlier chunk supply their existing states instead.
      if(is.null(hidden))hidden <- lkt_sequence_initial(s,batch,x)
      # Compute the recurrent updates for the observed sequence in one Torch call. These
      # updates may use each current answer because they represent memory after that
      # answer has been observed.
      transition <- lkt_sequence_advance(self,torch::torch_cat(list(x,answers$unsqueeze(3)),dim=3),hidden,s)
      # Shift the top-layer outputs back by one trial before using them for prediction.
      # This is what keeps the current answer out of its own prediction even though the
      # whole recurrent sequence was computed first.
      prior <- torch::torch_cat(list(lkt_sequence_read(hidden,s),transition[[1]]),dim=2)[,seq_len(steps),,drop=FALSE]
      list(logits=self$head(torch::torch_cat(list(prior,x),dim=3))$squeeze(3),hidden=transition[[2]])
    }
  )
  net <- definition()
  net$to(device=s$device,dtype=if(s$dtype=='float32')torch::torch_float32() else torch::torch_float64())
  net
}

# Save weights as ordinary R arrays rather than live Torch objects. A saved fit can
# therefore reconstruct its network in a fresh R process.
lkt_gru_weights <- function(net) lapply(net$state_dict(),function(x)torch::as_array(x$detach()$cpu()))
lkt_gru_load_weights <- function(net,weights,s) {
  net$load_state_dict(lapply(weights,lkt_gru_tensor,s=s)); invisible(net)
}

# Pad the histories in a minibatch to a common length while marking real observations.
# The mask and row map ensure padding contributes neither loss nor exported predictions.
lkt_gru_batch <- function(encoded,data,groups,positions,s) {
  batch <- length(groups); steps <- max(lengths(positions))
  numeric <- array(0,c(batch,steps,ncol(encoded$numeric)))
  categorical <- array(1L,c(batch,steps,ncol(encoded$categorical)))
  retrieval_ids <- if(length(s$retrieval_filter_cols))
    array(NA_character_,c(batch,steps,length(s$retrieval_filter_cols))) else NULL
  answers <- mask <- weights <- matrix(0,batch,steps)
  rows <- matrix(NA_integer_,batch,steps)
  for(j in seq_len(batch)) {
    idx <- groups[[j]][positions[[j]]]; k <- seq_along(idx)
    if(length(idx)) {
      numeric[j,k,] <- lkt_gru_numeric_rows(encoded,idx)
      categorical[j,k,] <- encoded$categorical[idx,,drop=FALSE]
      if(!is.null(retrieval_ids))retrieval_ids[j,k,] <- encoded$retrieval_ids[idx,,drop=FALSE]
      answers[j,k] <- data$CF..ansbin.[idx];mask[j,k] <- 1;rows[j,k] <- idx
      weights[j,k] <- if(is.null(s$weight_col))1 else data[[s$weight_col]][idx]
    }
  }
  list(numeric=lkt_gru_tensor(numeric,s),categorical=lkt_gru_tensor(categorical,s,TRUE),
       answers=lkt_gru_tensor(answers,s),mask=lkt_gru_tensor(mask,s),weights=lkt_gru_tensor(weights,s),rows=rows,
       retrieval_ids=retrieval_ids)
}

# Replay histories with dropout disabled and without gradient tracking. This produces
# deterministic evaluation predictions while leaving the fitted weights unchanged.
lkt_gru_replay <- function(net,encoded,data,s) {
  net$eval();pred <- rep(NA_real_,nrow(data))
  groups <- split(seq_len(nrow(data)),lkt_gru_keys(data,s$history_group_cols))
  torch::with_no_grad({
    for(start in seq.int(1L,length(groups),by=s$batch_size)) {
      g <- groups[seq.int(start,min(length(groups),start+s$batch_size-1L))]
      longest <- max(lengths(g));chunk <- if(is.null(s$sequence_length))longest else s$sequence_length
      hidden <- context <- NULL
      for(offset in seq.int(1L,longest,by=chunk)) {
        active <- which(lengths(g)>=offset)
        hidden <- lkt_sequence_state_map(hidden,s,'subset',active)
        context <- lkt_gru_context_subset(context,active)
        g <- g[active]
        positions <- lapply(g,function(idx)seq.int(offset,min(length(idx),offset+chunk-1L)))
        b <- lkt_gru_batch(encoded,data,g,positions,s)
        out <- if(s$inference_mode=='none' && s$retrieval_heads==0L)net(b$numeric,b$categorical,b$answers,hidden) else
          net(b$numeric,b$categorical,b$answers,hidden,context,b$retrieval_ids)
        hidden <- out$hidden
        if(s$inference_mode!='none' || s$retrieval_heads>0L)context <- out$context
        p <- torch::as_array(torch::torch_sigmoid(out$logits)$cpu())
        keep <- !is.na(b$rows);pred[b$rows[keep]] <- p[keep]
      }
    }
  })
  pmin(pmax(pred,s$clip_epsilon),1-s$clip_epsilon)
}

lkt_gru_metrics <- function(y,p) {
  c(log_loss=-mean(stats::dbinom(y,1,p,log=TRUE)),
    AUC=if(length(unique(y))==2L)as.numeric(pROC::auc(y,p,quiet=TRUE,direction='<')) else NA_real_,
    RMSE=sqrt(mean((y-p)^2)))
}

# The decay parameter can learn faster without changing updates to other weights.
# Keep the ordinary single parameter group intact by default, including historical
# optimizer ordering. Plateau scheduling uses its existing factor and scalar floor.
lkt_gru_optimizer <- function(net,s) {
  params <- net$parameters
  if(lkt_sequence_kind(s)=='mamba') {
    named <- net$named_parameters()
    no_decay <- grepl('[.](A_log|D)$',names(named))
    fast <- names(named)=='retrieval.decay' & s$retrieval_decay_lr_multiplier!=1
    params <- list(list(params=unname(named[!no_decay & !fast])),
      list(params=unname(named[no_decay]),weight_decay=0))
    if(any(fast))params[[length(params)+1L]] <- list(params=unname(named[fast]),
      lr=s$learning_rate*s$retrieval_decay_lr_multiplier)
  } else if(s$retrieval_decay_lr_multiplier!=1) {
    named <- net$named_parameters();decay <- which(names(named)=='retrieval.decay')
    if(length(decay)!=1L)stop('Expected one learnable retrieval decay tensor.',call.=FALSE)
    params <- list(list(params=unname(named[-decay])),
      list(params=unname(named[decay]),lr=s$learning_rate*s$retrieval_decay_lr_multiplier))
  }
  do.call(switch(s$optimizer,adamw=torch::optim_adamw,adam=torch::optim_adam,sgd=torch::optim_sgd),
    c(list(params=params,lr=s$learning_rate,weight_decay=s$weight_decay),s$optimizer_options))
}

lkt_gru_decay_lr <- function(opt,s) {
  if(s$retrieval_heads==0L || s$retrieval_decay=='none')return(NA_real_)
  opt$param_groups[[if(s$retrieval_decay_lr_multiplier==1)1L else length(opt$param_groups)]]$lr
}

# Fit preprocessing on training rows, then use the resulting schema for every partition.
# Validation and test rows can be predicted without influencing vocabularies, scaling,
# or weight optimization.
lkt_fit_gru <- function(request,s,verbose=FALSE) {
  cleanup <- lkt_gru_runtime(s);on.exit(cleanup(),add=TRUE)
  data <- request$data; masks <- request$masks
  prep <- lkt_gru_preprocess(data,masks$train,request$recipe,s)
  encoded <- lkt_gru_encode(data,prep,request$recipe,s)
  net <- lkt_gru_network(prep,s)
  initial_file <- if(is.null(s$unit))s$initial_model_file else s$unit_options$initial_model_file
  initial <- if(is.null(initial_file))NULL else
    lkt_gru_initialize_model(net,initial_file,data,masks,request$recipe,prep,lkt_sequence_gru_settings(s))
  # Create the requested optimizer and optional validation-loss scheduler. These
  # controls act on neural weights; nonlinear LKT feature parameters remain fixed in the
  # feature recipe.
  opt <- lkt_gru_optimizer(net,s)
  scheduler <- if(s$scheduler=='plateau')do.call(torch::lr_reduce_on_plateau,
    c(list(optimizer=opt,mode='min',threshold_mode='abs'),s$scheduler_options)) else NULL
  # Only training rows appear in the optimization graph. Chronological split
  # validation guarantees that their causal feature histories contain no holdout.
  groups <- split(which(masks$train),lkt_gru_keys(data,s$history_group_cols)[masks$train])
  best <- stop_best <- Inf; best_epoch <- 0L; stale <- 0L; best_weights <- NULL
  history <- list(); reason <- 'epochs'; started <- proc.time()[['elapsed']]
  if(!is.null(initial)) {
    p <- lkt_gru_replay(net,encoded,data,s)
    if(max(abs(p-initial$source_prediction))>3e-6)
      stop('Zero-input expansion does not reproduce the initial model predictions.',call.=FALSE)
    initial$predictions <- p;initial$source_prediction <- NULL
    tm <- lkt_gru_metrics(data$CF..ansbin.[masks$train],p[masks$train])
    vm <- if(any(masks$validation))lkt_gru_metrics(data$CF..ansbin.[masks$validation],p[masks$validation]) else rep(NA_real_,3L)
    history[[1L]] <- data.frame(epoch=0L,training_loss=NA_real_,training_bce=NA_real_,training_temporal_kl=NA_real_,
      training_eval_loss=unname(tm[1]),training_auc=unname(tm[2]),validation_loss=unname(vm[1]),
      validation_auc=unname(vm[2]),validation_rmse=unname(vm[3]),learning_rate=s$learning_rate,
      retrieval_decay_learning_rate=lkt_gru_decay_lr(opt,s),elapsed_seconds=0)
    # The trained starting point is a genuine candidate, not a rescue performed
    # after looking at test results. Subsequent weights must beat it on validation.
    if(any(masks$validation)) {
      best <- stop_best <- unname(vm[1]);best_weights <- lkt_gru_weights(net)
      if(!is.null(scheduler))scheduler$step(best)
    }
  }
  # Train in whole-history groups, shuffling their order between epochs. Observations
  # within a history stay chronological so the network always learns from prior
  # responses.
  for(epoch in seq_len(s$epochs)) {
    net$train(); total <- denom <- bce_total <- kl_total <- 0
    shuffled <- groups[sample.int(length(groups))]
    for(start in seq.int(1L,length(shuffled),by=s$batch_size)) {
      g <- shuffled[seq.int(start,min(length(shuffled),start+s$batch_size-1L))]
      longest <- max(lengths(g)); chunk <- if(is.null(s$sequence_length))longest else s$sequence_length
      hidden <- context <- NULL
      for(offset in seq.int(1L,longest,by=chunk)) {
        # Remove completed groups so padding never changes carried histories.
        active <- which(lengths(g)>=offset)
        hidden <- lkt_sequence_state_map(lkt_sequence_state_map(hidden,s,'subset',active),s,'detach')
        context <- lkt_gru_context_subset(context,active,detach=TRUE)
        g <- g[active]
        positions <- lapply(g,function(idx)seq.int(offset,min(length(idx),offset+chunk-1L)))
        b <- lkt_gru_batch(encoded,data,g,positions,s)
        opt$zero_grad()
        out <- if(s$inference_mode=='none' && s$retrieval_heads==0L)net(b$numeric,b$categorical,b$answers,hidden) else
          net(b$numeric,b$categorical,b$answers,hidden,context,b$retrieval_ids)
        # Compare each pre-answer logit with its observed outcome using a numerically
        # stable binary loss. Masks and optional trial weights restrict the average to
        # genuine training observations.
        raw <- torch::nnf_binary_cross_entropy_with_logits(out$logits,b$answers,reduction='none',
          pos_weight=lkt_gru_tensor(s$positive_weight,s))
        denominator <- (b$mask*b$weights)$sum()
        bce <- (raw*b$mask*b$weights)$sum()/denominator
        # Consecutive latent distributions are paired only within one participant.
        # The detached previous chunk supplies its final distribution so the smoothing
        # penalty includes chunk boundaries without extending backpropagation.
        kl <- bce*0
        if(s$inference_mode=='gaussian') {
          pair_weights <- b$mask*b$weights*out$kl_valid
          if(pair_weights$sum()$item()>0)
            kl <- (out$kl_per_trial*pair_weights)$sum()/pair_weights$sum()
        }
        loss <- bce+s$temporal_kl_beta*kl
        if(!is.finite(loss$item()))stop('GRU training produced nonfinite loss.',call.=FALSE)
        loss$backward()
        if(!is.null(s$gradient_clip))torch::nn_utils_clip_grad_norm_(net$parameters,s$gradient_clip,norm_type=s$gradient_clip_norm)
        # Carry the updated memory into the next chunk but detach its gradient graph.
        # This preserves the response history while limiting backpropagation to the
        # configured chunk length.
        opt$step();hidden <- lkt_sequence_state_map(out$hidden,s,'detach')
        if(s$inference_mode!='none' || s$retrieval_heads>0L)context <- lkt_gru_context_subset(out$context,detach=TRUE)
        total <- total+loss$item()*denominator$item();denom <- denom+denominator$item()
        bce_total <- bce_total+bce$item()*denominator$item()
        kl_total <- kl_total+kl$item()*denominator$item()
      }
    }
    # Evaluate the current weights with dropout disabled after the epoch. Only the
    # validation partition supplies the stopping and scheduler score; test predictions
    # are not used for those decisions.
    p <- lkt_gru_replay(net,encoded,data,s)
    vm <- if(any(masks$validation))lkt_gru_metrics(data$CF..ansbin.[masks$validation],p[masks$validation]) else c(log_loss=NA_real_,AUC=NA_real_,RMSE=NA_real_)
    tm <- lkt_gru_metrics(data$CF..ansbin.[masks$train],p[masks$train])
    history[[length(history)+1L]] <- data.frame(epoch=epoch,training_loss=total/denom,
      training_bce=bce_total/denom,training_temporal_kl=kl_total/denom,
      training_eval_loss=unname(tm[1]),training_auc=unname(tm[2]),
      validation_loss=unname(vm[1]),validation_auc=unname(vm[2]),validation_rmse=unname(vm[3]),
      learning_rate=opt$param_groups[[1]]$lr,retrieval_decay_learning_rate=lkt_gru_decay_lr(opt,s),
      elapsed_seconds=proc.time()[['elapsed']]-started)
    if(any(masks$validation)) {
      value <- unname(vm[1])
      # Keep the best checkpoint on any validation improvement, but reset patience only
      # for an improvement exceeding min_delta. These separate rules retain the best
      # observed weights without prolonging training for negligible changes.
      if(value<best) {best <- value;best_epoch <- epoch;best_weights <- lkt_gru_weights(net)}
      if((is.null(initial) && epoch==1L) || value<stop_best-s$min_delta) {stop_best <- value;stale <- 0L} else stale <- stale+1L
      if(!is.null(scheduler))scheduler$step(value)
      if(s$early_stopping && stale>=s$patience) {reason <- 'early_stopping';break}
    }
    if(verbose)cat(toupper(lkt_sequence_kind(s)),'epoch',epoch,'training loss',round(total/denom,6),'validation loss',round(vm[1],6),'\n')
  }
  # Restore the validation-selected weights before producing final predictions. Save the
  # settings, preprocessing, recipe, and arrays needed to reproduce later prediction and
  # continuation.
  if(s$restore_best && !is.null(best_weights))lkt_gru_load_weights(net,best_weights,s)
  pred <- lkt_gru_replay(net,encoded,data,s)
  model <- list(schema=1L,id=basename(tempfile('lkt-gru-')),settings=s,preprocessing=prep,
    recipe=request$recipe,weights=lkt_gru_weights(net))
  result <- LKTModelFit(model,pred,fitstat=sum(stats::dbinom(data$CF..ansbin.[masks$train],1,pred[masks$train],log=TRUE)))
  result$model_metadata <- list(gru_settings=s,fit_rows=which(masks$train),validation_rows=which(masks$validation),
    evaluation_rows=which(masks$evaluation),training_history=do.call(rbind,history),
    selected_epoch=if(s$restore_best && !is.null(best_weights))best_epoch else epoch,stopping_reason=reason,
    trained_epochs=epoch,initialization=initial,
    parameter_count=sum(vapply(net$parameters,function(p)as.numeric(p$numel()),numeric(1))),
    software_versions=list(R=R.version.string,torch=as.character(utils::packageVersion('torch')),LKT=as.character(utils::packageVersion('LKT'))))
  if(!is.null(s$unit)) {
    result$model_metadata$sequence_settings <- result$model_metadata$gru_settings
    result$model_metadata$gru_settings <- NULL
  }
  result
}
