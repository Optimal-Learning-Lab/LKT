#' Configurable recurrent knowledge tracing
#'
#' GRUModel predicts from the previous hidden state and the current predictors,
#' then updates the state with the observed response. All controls are explicit;
#' see the GRU vignette for input, split, and continuation semantics.
#' Defaults use three 32-unit GRU layers, a 128-unit prediction head, a combined
#' numeric encoder with 32 outputs, raw binary answers, item/skill embeddings,
#' and no previous-response-gap input (time_col=NULL). Additional timing inputs
#' accompany filtered one-head retrieval with one output projection. Item embeddings
#' start at zero. LKT's default interc=FALSE and the GRU configuration default
#' omit the global intercept. Explicit arguments override
#' these ordinary defaults. LKT itself still defaults to LibLinearModel.
#' @param hidden_size Width of each recurrent layer.
#' @param num_layers Number of recurrent layers.
#' @param gru_bias Include recurrent biases.
#' @param head_hidden_sizes Hidden widths in the prediction MLP; integer(0) is linear.
#' @param head_activation relu, tanh, or gelu.
#' @param head_bias Include prediction-head biases.
#' @param categorical_cols Raw categorical predictor columns.
#' @param numeric_cols Raw numeric predictor columns.
#' @param numeric_feature_cols Optional selection of final numeric input names, after
#'   timing transforms and LKT feature construction and before training-only scaling.
#'   NULL retains all inputs; character(0) omits the numeric block. Names use raw:,
#'   time:, or lkt: prefixes and must exist. Selection order is preserved. In the GUI,
#'   ALL retains all inputs and an empty field omits them.
#' @param categorical_encoding embedding or one_hot.
#' @param embedding_dims Positive width shared across categorical columns, or a named width per column.
#' @param input_dropout,gru_dropout,head_dropout Dropout probabilities. Recurrent dropout requires multiple layers.
#' @param standardize Standardize numeric and engineered columns using training rows.
#' @param numeric_transforms Named character vector mapping numeric columns to identity or log1p.
#' @param use_lkt_features Append the configured, fixed-parameter LKT features.
#' @param time_col Optional numeric/POSIXct timestamp column for the previous-response
#'   gap input. NULL (default) omits that input. Set explicitly to include it.
#'   order_col independently controls chronological ordering; additional timing
#'   columns remain controlled by time_feature_cols.
#' @param time_units seconds, minutes, hours, or days for numeric timestamps.
#' @param time_feature_cols Additional nonnegative timing columns in seconds, computed
#'   before removing unscored exposures. Each receives log1p and training-only scaling,
#'   and enters the numeric encoder. See LKTGRUTimeFeatures.
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
#' @param hidden_decay Enable elapsed-time decay of GRU hidden units, controlled by
#'   each unit's exponentially fading signed change history (DK). Three shared
#'   scalars are learned during training; connection weights stay fixed at testing.
#' @param hidden_decay_time_col Explicit timestamp column for hidden decay. Required
#'   when hidden_decay is TRUE. Uses time_units, independently of numeric time inputs.
#' @param gate_fatigue Enable compiled activity-driven GRU write-gate fatigue.
#' @param gate_fatigue_mode unit preserves the earlier unit-write model; connection
#'   scales every input/recurrent GRU weight by student-specific recovery/fatigue.
#' @param connection_fatigue_scope all scales all GRU matrix rows; candidate
#'   scales candidate rows. input_candidate excludes recurrent matrices;
#'   h1_input_candidate limits input-to-candidate fatigue to the first layer;
#'   recurrent_candidate scales recurrent-to-candidate weights only in all layers.
#' @param connection_time_scale_range Positive bounds for fixed log-uniform
#'   connection recovery-time multipliers. c(1,1) disables scaling.
#' @param connection_time_scale_seed Fixed matrix-generation seed, independent of model seed.
#' @param gate_fatigue_time_col Explicit recovery timestamp column, in time_units.
#' @param plasticity none, constant, fatigue, error_rate, item_skill, or bayesian_prototypes. Adds compiled student-specific
#'   final-output connection updates after feedback. error_rate learns alpha and c
#'   from zero starts, using eta_student_new=eta_student_old+alpha*absolute error-c.
#'   The existing learned eta initializes each student; current output updates use
#'   the prior student rate, which remains signed during training and testing.
#'   item_skill uses the first two categorical columns as nested item/skill IDs.
#'   It learns nonnegative skill rates (zero start) and positive item multipliers
#'   (unit start), normalized to geometric mean one within each skill.
#' @param plasticity_time_col Explicit recovery timestamp column, in time_units.
#' @param hidden_decay_parameterization amplitude uses a nonnegative, zero-initialized
#'   baseline rate and a compiled Torch recurrence. softplus reads the original
#'   positive-baseline decay model; its historical interpretation is preserved.
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
#'   for time, history, response, or numeric (the combined encoder).
#'   integer(0) uses the common width.
#' @param numeric_encoder separate time/history MLPs or one combined numeric MLP.
#' @param response_encoding mlp encodes binary responses when feature_encoder is
#'   mlp; binary supplies the observed zero/one response directly to memory.
#' @param retrieval_value_mode projected uses separate value/output maps; raw
#'   averages unprojected states before one output map. raw requires one head
#'   and zero attention dropout, and preserves the zero read for empty matches.
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
#' @param latent_denoising Opt-in training-only residual 32-to-32-to-32 Gaussian latent reconstruction; disabled by default. No prediction or student-state change.
#' @param denoising_noise_std Fixed .20 Gaussian corruption SD for valid pre-answer top GRU states.
#' @param denoising_loss_weight Fixed .15 auxiliary coordinate-mean MSE coefficient; 0 is the explicit parity control. Both clean and reconstructed paths retain gradients.
#' @param semantic_input NULL (disabled), or an explicit named list binding question_col,
#'   response_col, kc_col, kc_strip_number, python, helper, helper_sha256, model_dir,
#'   model_id, revision and manifest_sha256. Optional cache_file/cache_sha256 bind a
#'   reusable RDS text cache. Frozen MPNet vectors are projected 2304 -> 32 with tanh
#'   and appended only to completed recurrent updates. See docs/gru-semantic-input.md.
#' @return GRUModel returns an LKT model adapter. lkt_gru_defaults returns all resolved default settings without requiring Torch.
#' @export
#' @param embedding_adaptation none or per_student; opt-in episodic embedding learning.
#' @param adaptive_embedding_cols Categorical columns reset and learned for each student.
#' @param embedding_learning_rate Positive local post-answer SGD learning rate.
#' @param optional_adaptive_cols Adaptive categorical inputs whose missing labels
#'   mean no identity evidence: zero input, no local entry/update or retrieval match.
#' @param exercise_attributes Optional explicit hash-bound annotation and provenance
#'   bindings. Replaces only the first16-wide item lookup with difficulty, ability
#'   and expected-time embeddings projected24-to-16. See docs/gru-exercise-attributes.md.
GRUModel <- function(hidden_size=32L, num_layers=3L, gru_bias=TRUE,
                     head_hidden_sizes=128L, head_activation='relu', head_bias=TRUE,
                     categorical_cols=c('KC..Default.','KC..Cluster.'), numeric_cols=character(),
                     categorical_encoding='embedding', embedding_dims=16L,
                     input_dropout=.1, gru_dropout=.1, head_dropout=.1,
                     standardize=TRUE, numeric_transforms=character(),
                     use_lkt_features=TRUE, time_col=NULL, time_units='seconds',
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
                     encoder_output_sizes=c(numeric=32L),numeric_fusion='concat',
                     categorical_fusion='concat',input_fusion='concat',
                     joint_encoder_output_size=NULL,joint_encoder_hidden_sizes=32L,
                     joint_encoder_activation='relu',joint_encoder_dropout=0,
                     numeric_encoder='combined',response_encoding='binary',
                     retrieval_value_mode='raw',numeric_feature_cols=NULL,
                     hidden_decay=FALSE,hidden_decay_time_col=NULL,
                     hidden_decay_parameterization='amplitude',
                     plasticity='none',plasticity_time_col=NULL,
                     gate_fatigue=FALSE,gate_fatigue_time_col=NULL,gate_fatigue_mode='unit',
                     connection_time_scale_range=c(1,1),connection_time_scale_seed=1L,
                     connection_fatigue_scope='all',
                     embedding_adaptation='none',adaptive_embedding_cols='KC..Default.',
                     embedding_learning_rate=.01,optional_adaptive_cols=character(),semantic_input=NULL,exercise_attributes=NULL,
                     latent_denoising=FALSE,denoising_noise_std=.20,denoising_loss_weight=.15) {
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

# The schema-1 saved format predates the combined encoder selectors. Their
# absence identifies that historical layout, not a request for today's defaults.
# Decode it explicitly; weight loading still requires exact tensor names/shapes.
lkt_gru_saved_controls <- function(options) {
  if(!is.list(options) || !length(options))stop('Saved GRU settings must be explicit.',call.=FALSE)
  denoising <- c('latent_denoising','denoising_noise_std','denoising_loss_weight')
  if(!any(denoising%in%names(options)))options[denoising] <- list(FALSE,.20,.15)
  else if(!all(denoising%in%names(options)))stop('Saved latent-denoising controls are incomplete.',call.=FALSE)
  if(!'exercise_attributes'%in%names(options))options['exercise_attributes'] <- list(NULL)
  if(!'semantic_input'%in%names(options))options$semantic_input <- NULL
  if(!'embedding_adaptation'%in%names(options))options$embedding_adaptation <- 'none'
  if(!'optional_adaptive_cols'%in%names(options))options$optional_adaptive_cols <- character()
  # Original schema-1 models predate feature encoders, retrieval and additional
  # timing inputs. Decode that documented layout explicitly, never with today's
  # constructor defaults; exact saved tensor names/shapes are still required.
  if(!'feature_encoder'%in%names(options)) {
    if(any(c('retrieval_heads','time_feature_cols','numeric_encoder')%in%names(options)))
      stop('Saved original GRU architecture is incomplete.',call.=FALSE)
    options$feature_encoder <- 'none'
    options$retrieval_heads <- 0L
    options$retrieval_filter_cols <- character()
    options$time_feature_cols <- options$time_missing_cols <- character()
    options$zero_init_embedding_cols <- character()
  }
  if(!'hidden_decay_parameterization'%in%names(options))
    options$hidden_decay_parameterization <- if(isTRUE(options$hidden_decay))'softplus' else 'amplitude'
  selectors <- c('numeric_encoder','response_encoding','retrieval_value_mode')
  present <- selectors %in% names(options)
  if(any(present) && !all(present))stop('Saved GRU architecture selectors are incomplete.',call.=FALSE)
  if(!any(present)) {
    if('numeric' %in% names(options$encoder_output_sizes))
      stop('Saved combined encoder is missing its architecture selectors.',call.=FALSE)
    options$numeric_encoder <- 'separate'
    options$response_encoding <- 'mlp'
    options$retrieval_value_mode <- 'projected'
    if(!'encoder_output_sizes' %in% names(options))options$encoder_output_sizes <- integer()
  } else if(!'encoder_output_sizes' %in% names(options))
    stop('Saved GRU encoder output widths are missing.',call.=FALSE)
  options
}

lkt_gru_named <- function(x, allowed, label) {
  if (!is.list(x) || (length(x) && (is.null(names(x)) || anyNA(names(x)) ||
      any(!nzchar(names(x))) || anyDuplicated(names(x)) || any(!names(x) %in% allowed))))
    stop(label, ' must be a named list of supported settings.', call.=FALSE)
}

# Reject unsupported settings before allocating a network or starting a fit. Central
# validation keeps the command-line and GUI entry points consistent.
lkt_validate_gru <- function(s) {
  lkt_gru_validate_adaptation(s)
  lkt_gru_semantic_validate(s)
  lkt_gru_exercise_validate(s)
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
  for(n in c('gru_bias','head_bias','standardize','use_lkt_features','early_stopping','restore_best','deterministic','retrieval_share_qk','retrieval_bias','input_to_all_layers','hidden_decay','gate_fatigue'))
    if(!is.logical(s[[n]]) || length(s[[n]])!=1L || is.na(s[[n]])) stop(n,' must be TRUE or FALSE.',call.=FALSE)
  choices <- list(connection_fatigue_scope=c('all','candidate','input_candidate','h1_input_candidate','recurrent_candidate'),gate_fatigue_mode=c('unit','connection'),plasticity=c('none','constant','fatigue','error_rate','item_skill','bayesian_prototypes'),hidden_decay_parameterization=c('amplitude','softplus'),numeric_encoder=c('separate','combined'),response_encoding=c('mlp','binary'),
    retrieval_value_mode=c('projected','raw'),
    joint_encoder_activation=c('relu','tanh','gelu'),numeric_fusion=c('concat','sum'),categorical_fusion=c('concat','sum'),input_fusion=c('concat','sum'),additional_input_init=c('random','zero'),retrieval_fusion=c('replace','concat'),retrieval_decay=c('none','exponential','akt'),feature_encoder=c('none','mlp'),encoder_activation=c('relu','tanh','gelu'),head_activation=c('relu','tanh','gelu'),categorical_encoding=c('embedding','one_hot'),
    time_units=c('seconds','minutes','hours','days'),optimizer=c('adamw','adam','sgd'),
    scheduler=c('none','plateau'),dtype=c('float32','float64'),unknown_levels=c('error','zero'),
    inference_mode=c('none','deterministic','gaussian'),inference_activation=c('relu','tanh','gelu'))
  for(n in names(choices)) {
    if(!is.character(s[[n]]) || length(s[[n]])!=1L || is.na(s[[n]]) || !s[[n]] %in% choices[[n]])
      stop('Invalid GRU ',n,'.',call.=FALSE)
  }
  if(s$num_layers==1L && s$gru_dropout>0) stop('gru_dropout requires num_layers > 1.',call.=FALSE)
  if(s$numeric_encoder=='combined' && (s$feature_encoder!='mlp' || s$numeric_fusion!='concat'))
    stop('The combined numeric encoder requires feature_encoder mlp and numeric_fusion concat.',call.=FALSE)
  if(s$retrieval_value_mode=='raw' &&
     (s$retrieval_heads!=1L || s$retrieval_dropout!=0))
    stop('Raw retrieval values require exactly one head and zero attention dropout.',call.=FALSE)
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
       anyDuplicated(names(widths)) || !all(names(widths) %in% c('time','history','response','numeric')))))
    stop('encoder_output_sizes requires named positive widths for time, history, response, or numeric.',call.=FALSE)
  s$encoder_output_sizes <- stats::setNames(as.integer(widths),names(widths))
  if(length(widths) && s$feature_encoder!='mlp')
    stop('encoder_output_sizes requires feature_encoder mlp.',call.=FALSE)
  if(s$numeric_encoder=='combined' && any(names(widths) %in% c('time','history')) ||
     s$numeric_encoder=='separate' && 'numeric' %in% names(widths))
    stop('Numeric output widths must name the selected combined or separate encoders.',call.=FALSE)
  if(s$response_encoding=='binary' && 'response' %in% names(widths))
    stop('Binary response encoding has width one; do not set a response encoder width.',call.=FALSE)
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
  for(n in c('numeric_feature_cols','categorical_cols','numeric_cols','time_feature_cols','time_missing_cols','retrieval_filter_cols',
            'retrieval_cue_cols','zero_init_embedding_cols','history_group_cols','subject_col','order_col','time_col','weight_col','hidden_decay_time_col')) {
    x <- s[[n]]
    if(is.null(x) && n %in% c('numeric_feature_cols','order_col','time_col','weight_col','hidden_decay_time_col')) next
    if(!is.character(x) || anyNA(x) || any(!nzchar(x)) || anyDuplicated(x) ||
       (n %in% c('subject_col','order_col','time_col','weight_col','hidden_decay_time_col') && length(x)!=1L))
      stop('Invalid GRU ',n,'.',call.=FALSE)
  }
  if(s$hidden_decay && is.null(s$hidden_decay_time_col))
    stop('hidden_decay requires an explicit hidden_decay_time_col.',call.=FALSE)
  if(lkt_gru_candidate_connections(s) && !lkt_gru_connection_fatigue(s))
    stop('Candidate-only fatigue requires connection fatigue mode.',call.=FALSE)
  limits <- s$connection_time_scale_range
  if(!is.numeric(limits) || length(limits)!=2L || any(!is.finite(limits)) || any(limits<=0) || limits[1]>limits[2])
    stop('connection_time_scale_range must be two ordered positive bounds.',call.=FALSE)
  number('connection_time_scale_seed',0,.Machine$integer.max,TRUE)
  if(lkt_gru_random_connection_time(s) && !lkt_gru_connection_fatigue(s))
    stop('Random connection time scales require connection fatigue mode.',call.=FALSE)
  if(lkt_gru_gate_active(s)) {
    if(s$hidden_decay)stop('Gate fatigue cannot be combined with experimental hidden decay.',call.=FALSE)
    if(!is.character(s$gate_fatigue_time_col) || length(s$gate_fatigue_time_col)!=1L ||
       is.na(s$gate_fatigue_time_col) || !nzchar(s$gate_fatigue_time_col) ||
       s$gate_fatigue_time_col %in% c('CF..ansbin.','Outcome','pred'))
      stop('Gate fatigue requires an explicit non-response gate_fatigue_time_col.',call.=FALSE)
    if(!identical(s$history_group_cols,s$subject_col))stop('Gate fatigue requires one history per student.',call.=FALSE)
    if(lkt_gru_plastic(s) && !identical(s$gate_fatigue_time_col,s$plasticity_time_col))
      stop('Combined gate fatigue and output plasticity must use the same timestamp column.',call.=FALSE)
  } else if(!is.null(s$gate_fatigue_time_col))stop('gate_fatigue_time_col requires gate_fatigue.',call.=FALSE)
  if(lkt_gru_plastic(s)) {
    if(s$hidden_decay)stop('Plastic connections and hidden decay are separate experimental mechanisms.',call.=FALSE)
    if(!is.character(s$plasticity_time_col) || length(s$plasticity_time_col)!=1L ||
       is.na(s$plasticity_time_col) || !nzchar(s$plasticity_time_col) ||
       s$plasticity_time_col %in% c('CF..ansbin.','Outcome','pred'))
      stop('Plastic connections require an explicit non-response plasticity_time_col.',call.=FALSE)
    if(!identical(s$history_group_cols,s$subject_col))
      stop('Plastic connections require one history per student.',call.=FALSE)
  } else if(!is.null(s$plasticity_time_col))stop('plasticity_time_col requires active plasticity.',call.=FALSE)
  if(any(s$hidden_decay_time_col %in% c('CF..ansbin.','Outcome','pred')))
    stop('Responses and predictions cannot be hidden decay timestamps.',call.=FALSE)
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
  lkt_gru_validate_denoising(s)
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
  # Torch 0.17 can borrow R storage for already-contiguous multidimensional
  # float64 arrays (including a single trial). That storage has no Torch owner
  # and may be reclaimed after this call. Clone while x is still retained here;
  # neither tensor mutation nor R garbage collection may change the other side.
  value <- torch::torch_tensor(x,dtype=if(long)torch::torch_long() else if(s$dtype=='float32')torch::torch_float32() else torch::torch_float64(),device=s$device)
  value$clone()
}

# Build the network from the saved input schema rather than guessing its dimensions from
# new data. The prediction head and recurrent update share the same encoded current
# predictors.
lkt_gru_network <- function(preprocessing,s) {
  if(lkt_gru_semantic_active(s) || s$inference_mode!='none' || s$feature_encoder!='none' || s$retrieval_heads>0L)return(lkt_gru_inference_network(preprocessing,s))
  self <- NULL # nn_module injects this binding into each method at runtime.
  if(lkt_gru_exercise_active(s))lkt_gru_exercise_saved(preprocessing$exercise_attributes,s)
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
      if(s$hidden_decay)self$hidden_decay <- lkt_gru_hidden_decay_module(s$hidden_decay_parameterization)
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
      if(lkt_gru_plastic(s))self$plasticity <- lkt_gru_plasticity_module(s$plasticity,preprocessing$plasticity_item_skill)
      if(lkt_gru_gate_active(s))self$gate_fatigue <- lkt_gru_gate_module(s,lkt_sequence_module(self,s))
      if(lkt_gru_denoising_active(s))self$latent_denoiser <- lkt_gru_denoising_module()
      if(lkt_gru_exercise_active(s))self$exercise_attributes <- lkt_gru_exercise_module(preprocessing$exercise_attributes)
    },
    # Combine numeric columns with categorical embeddings in a stable order. Input
    # dropout is applied once here, so the prediction and update branches share the
    # resulting current-feature vector.
    encode=function(numeric,categorical,embedding_values=NULL) {
      parts <- list(numeric)
      parts <- c(parts,lkt_gru_categorical_parts(self,categorical,preprocessing,s,embedding_values,numeric$dtype))
      self$input_drop(torch::torch_cat(parts,dim=3))
    },
    score_candidates=function(numeric,categorical,prior,bank=NULL,retrieval_ids=NULL,
                              embedding_values=NULL,plastic_weights=NULL) {
      x <- self$encode(numeric,categorical,embedding_values)
      prior <- prior$expand(c(x$size(1),1L,s$hidden_size))
      lkt_gru_plasticity_read(lkt_gru_head(self,torch::torch_cat(list(prior,x),dim=3),s),plastic_weights,s)
    },
    forward=function(numeric,categorical,answers,hidden=NULL,decay_gaps=NULL,valid=NULL,embedding_values=NULL) {
      x <- self$encode(numeric,categorical,embedding_values)
      batch <- x$size(1); steps <- x$size(2)
      # Start a new history with zero memory in every GRU layer. Histories carried from
      # an earlier chunk supply their existing states instead.
      if(is.null(hidden))hidden <- lkt_sequence_initial(s,batch,x)
      # Compute the recurrent updates for the observed sequence in one Torch call. These
      # updates may use each current answer because they represent memory after that
      # answer has been observed.
      interaction <- torch::torch_cat(list(x,answers$unsqueeze(3)),dim=3)
      transition <- if(lkt_gru_gate_active(s))lkt_gru_gate_transition(self,interaction,hidden,s,decay_gaps,valid) else if(lkt_gru_compiled_decay(s))lkt_gru_compiled_transition(self,interaction,hidden,s,decay_gaps,valid) else
        lkt_sequence_advance(self,interaction,hidden,s)
      # Shift the top-layer outputs back by one trial before using them for prediction.
      # This is what keeps the current answer out of its own prediction even though the
      # whole recurrent sequence was computed first.
      prior <- if(lkt_gru_compiled_decay(s) || lkt_gru_gate_active(s))transition$prior else
        torch::torch_cat(list(lkt_sequence_read(hidden,s),transition[[1]]),dim=2)[,seq_len(steps),,drop=FALSE]
      result <- c(lkt_gru_head(self,torch::torch_cat(list(prior,x),dim=3),s),list(hidden=transition[[2]]))
      lkt_gru_denoising_state(self,result,prior,s)
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
  lkt_gru_validate_denoising_weights(weights,s)
  if(lkt_gru_exercise_active(s))lkt_gru_exercise_validate_weights(net,weights,s)
  if(lkt_gru_gate_active(s)) {
    if(lkt_gru_connection_fatigue(s)) {
      if(lkt_gru_random_connection_time(s)) {
        scale <- weights[['gate_fatigue.time_scale']]
        if(!is.numeric(scale) || length(scale)!=length(weights[['gate_fatigue.lambda']]) ||
           any(!is.finite(scale)) || any(scale<=0))stop('Invalid fixed connection time scales.',call.=FALSE)
      }
      lam <- weights[['gate_fatigue.lambda']];rho <- weights[['gate_fatigue.log_rho']]
      if(!is.numeric(lam) || !length(lam) || any(!is.finite(lam)) || any(lam<0 | lam>1) ||
         !is.numeric(rho) || length(rho)!=1L || !is.finite(rho))stop('Invalid saved connection fatigue parameters.',call.=FALSE)
    } else {
    pars <- weights[c('gate_fatigue.kappa','gate_fatigue.log_rho')]
    if(any(!vapply(pars,function(x)is.numeric(x) && length(x)==1L && is.finite(x),logical(1))) ||
       weights[['gate_fatigue.kappa']]<0)stop('Invalid saved gate fatigue parameters.',call.=FALSE)
    }
  }
  if(s$plasticity=='item_skill') {
    rate<-weights[['plasticity.skill_rate']];item<-weights[['plasticity.log_item']]
    if(!is.numeric(rate)||!length(rate)||any(!is.finite(rate))||any(rate<0)||
       !is.numeric(item)||!length(item)||any(!is.finite(item)))stop('Invalid saved item/skill output tables.',call.=FALSE)
    for(n in c('item_parent','counts')) {
      expected<-torch::as_array(net$plasticity[[n]]$cpu())
      if(!identical(as.numeric(weights[[paste0('plasticity.',n)]]),as.numeric(expected)))stop('Saved item/skill output mapping changed.',call.=FALSE)
    }
  } else if(s$plasticity=='bayesian_prototypes') {
    lkt_gru_validate_bayesian_prototype_weights(weights)
  } else if(lkt_gru_plastic(s)) {
    names <- paste0('plasticity.',if(s$plasticity=='fatigue')c('eta','kappa','log_rho') else if(s$plasticity=='error_rate')c('eta','alpha','c') else 'eta')
    if(any(!vapply(weights[names],function(x)is.numeric(x) && length(x)==1L && is.finite(x),logical(1))) ||
       weights[['plasticity.eta']]<0 || (s$plasticity=='fatigue' && weights[['plasticity.kappa']]<0))
      stop('Invalid saved plasticity parameters.',call.=FALSE)
    if(s$plasticity=='error_rate' && any(unlist(weights[c('plasticity.alpha','plasticity.c')])<0))
      stop('Saved error-rate alpha and c must be nonnegative.',call.=FALSE)
  }
  if(lkt_gru_compiled_decay(s) && (length(weights[['hidden_decay.a']])!=1L ||
     !is.finite(weights[['hidden_decay.a']]) || weights[['hidden_decay.a']]<0))
    stop('Amplitude hidden decay requires a finite nonnegative baseline rate.',call.=FALSE)
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
  decay_gaps <- if(lkt_gru_clocked(s))matrix(0,batch,steps) else NULL
  rows <- matrix(NA_integer_,batch,steps)
  for(j in seq_len(batch)) {
    idx <- groups[[j]][positions[[j]]]; k <- seq_along(idx)
    if(length(idx)) {
      numeric[j,k,] <- lkt_gru_numeric_rows(encoded,idx)
      categorical[j,k,] <- encoded$categorical[idx,,drop=FALSE]
      if(!is.null(retrieval_ids))retrieval_ids[j,k,] <- encoded$retrieval_ids[idx,,drop=FALSE]
      answers[j,k] <- data$CF..ansbin.[idx];mask[j,k] <- 1;rows[j,k] <- idx
      weights[j,k] <- if(is.null(s$weight_col))1 else data[[s$weight_col]][idx]
      if(lkt_gru_clocked(s))decay_gaps[j,k] <- encoded$decay_gaps[idx]
    }
  }
  list(numeric=lkt_gru_tensor(numeric,s),categorical=lkt_gru_tensor(categorical,s,TRUE),
       answers=lkt_gru_tensor(answers,s),mask=lkt_gru_tensor(mask,s),weights=lkt_gru_tensor(weights,s),rows=rows,
       semantic=lkt_gru_semantic_tensor(encoded,rows,s),
       retrieval_ids=retrieval_ids,decay_gaps=if(lkt_gru_clocked(s))lkt_gru_tensor(decay_gaps,s) else NULL)
}

# Replay histories with dropout disabled and without gradient tracking. This produces
# deterministic evaluation predictions while leaving the fitted weights unchanged.
lkt_gru_replay <- function(net,encoded,data,s) {
  if(lkt_gru_adaptive(s))return(lkt_gru_adaptive_replay(net,encoded,data,s))
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
        out <- lkt_gru_forward(net,b$numeric,b$categorical,b$answers,hidden,context,
          b$retrieval_ids,s,b$decay_gaps,!is.na(b$rows),b$semantic)
        hidden <- out$hidden
        if(lkt_gru_has_context(s))context <- out$context
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
  if(lkt_gru_connection_fatigue(s)) {
    named <- net$named_parameters()
    no_decay <- names(named)=='gate_fatigue.lambda'
    fast <- names(named)=='retrieval.decay' & s$retrieval_decay_lr_multiplier!=1
    if(sum(no_decay)!=1L)stop('Expected one connection-lambda parameter tensor.',call.=FALSE)
    params <- list(list(params=unname(named[!no_decay & !fast])),
      list(params=unname(named[no_decay]),weight_decay=0))
    if(any(fast))params[[length(params)+1L]] <- list(params=unname(named[fast]),
      lr=s$learning_rate*s$retrieval_decay_lr_multiplier)
  } else if(lkt_sequence_kind(s)=='mamba') {
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
  lkt_gru_optional_recipe(request$recipe,s)
  data <- lkt_gru_optional_inputs(request$data,s); masks <- request$masks
  # Keep the complete exposure history for engineered predictors. Only scored
  # responses enter the recurrent sequence, its loss, or preprocessing estimates.
  exposures <- data
  response_rows <- which(!is.na(data$CF..ansbin.))
  data <- data[response_rows,,drop=FALSE]
  masks <- lapply(masks,function(mask)mask[response_rows])
  if(!any(masks$train))stop('GRU has no scored fitting rows.',call.=FALSE)
  prep <- lkt_gru_preprocess(data,masks$train,request$recipe,s,exposures,response_rows)
  encoded <- lkt_gru_encode(data,prep,request$recipe,s,exposure_data=exposures,exposure_rows=response_rows)
  if(lkt_gru_semantic_active(s))prep$semantic_cache <- lkt_gru_semantic_snapshot(encoded$semantic_cache)
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
    history[[1L]] <- data.frame(epoch=0L,training_loss=NA_real_,training_bce=NA_real_,training_temporal_kl=NA_real_,training_denoising_mse=NA_real_,
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
    net$train(); total <- denom <- bce_total <- kl_total <- denoising_total <- denoising_events <- 0
    shuffled <- groups[sample.int(length(groups))]
    for(start in seq.int(1L,length(shuffled),by=s$batch_size)) {
      g <- shuffled[seq.int(start,min(length(shuffled),start+s$batch_size-1L))]
      longest <- max(lengths(g)); chunk <- if(is.null(s$sequence_length))longest else s$sequence_length
      hidden <- context <- NULL
      adaptive <- if(lkt_gru_adaptive(s))rep(list(list()),length(g)) else NULL
      for(offset in seq.int(1L,longest,by=chunk)) {
        # Remove completed groups so padding never changes carried histories.
        active <- which(lengths(g)>=offset)
        hidden <- lkt_sequence_state_map(lkt_sequence_state_map(hidden,s,'subset',active),s,'detach')
        context <- lkt_gru_context_subset(context,active,detach=TRUE)
        if(lkt_gru_adaptive(s))adaptive <- adaptive[active]
        g <- g[active]
        positions <- lapply(g,function(idx)seq.int(offset,min(length(idx),offset+chunk-1L)))
        b <- lkt_gru_batch(encoded,data,g,positions,s)
        opt$zero_grad()
        if(lkt_gru_adaptive(s)) {
          out <- lkt_gru_adaptive_batch(net,b,data,s,hidden,context,adaptive,training=TRUE)
          adaptive <- out$adaptive
        } else out <- lkt_gru_forward(net,b$numeric,b$categorical,b$answers,hidden,context,
          b$retrieval_ids,s,b$decay_gaps,!is.na(b$rows),b$semantic)
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
        denoising <- lkt_gru_denoising_mse(net,out,!is.na(b$rows),s)
        if(!is.null(denoising)) {
          loss <- loss+s$denoising_loss_weight*denoising
          denoising_total <- denoising_total+denoising$item()*sum(!is.na(b$rows))
          denoising_events <- denoising_events+sum(!is.na(b$rows))
        }
        if(!is.finite(loss$item()))stop('GRU training produced nonfinite loss.',call.=FALSE)
        loss$backward()
        if(!is.null(s$gradient_clip))torch::nn_utils_clip_grad_norm_(net$parameters,s$gradient_clip,norm_type=s$gradient_clip_norm)
        # Carry the updated memory into the next chunk but detach its gradient graph.
        # This preserves the response history while limiting backpropagation to the
        # configured chunk length.
        opt$step();lkt_gru_project_hidden_decay(net,s);lkt_gru_project_plasticity(net,s);lkt_gru_project_gate(net,s)
        hidden <- lkt_sequence_state_map(out$hidden,s,'detach')
        if(lkt_gru_has_context(s))context <- lkt_gru_context_subset(out$context,detach=TRUE)
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
      training_denoising_mse=if(denoising_events>0)denoising_total/denoising_events else 0,
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
    training_only_parameter_count=if(lkt_gru_denoising_active(s))2112L else 0L,
    prediction_parameter_count=sum(vapply(net$parameters,function(p)as.numeric(p$numel()),numeric(1)))-if(lkt_gru_denoising_active(s))2112L else 0L,
    software_versions=list(R=R.version.string,torch=as.character(utils::packageVersion('torch')),LKT=as.character(utils::packageVersion('LKT'))))
  for(n in c('fit_rows','validation_rows','evaluation_rows'))
    result$model_metadata[[n]] <- response_rows[result$model_metadata[[n]]]
  if(length(response_rows)!=nrow(exposures)) {
    # Replay committed studies through the same public continuation semantics:
    # they retain exposure history but never supply a fictitious response.
    object <- list(model_name=if(is.null(s$unit))'GRU' else 'Sequence',model=model)
    result$pred <- LKTSequenceStep(object,LKTSequenceState(object),exposures)$predictions
  }
  if(!is.null(s$unit)) {
    result$model_metadata$sequence_settings <- result$model_metadata$gru_settings
    result$model_metadata$gru_settings <- NULL
  }
  result
}
