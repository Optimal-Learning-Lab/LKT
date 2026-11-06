# Explicit small/raw models used by historical regression cases. These fixtures
# do not redefine production defaults; tests of public defaults use GRUModel().
test_gru_minimal_settings <- function()list(hidden_size=64L,num_layers=1L,
  head_hidden_sizes=32L,categorical_cols='KC..Default.',input_dropout=0,
  gru_dropout=0,head_dropout=0,use_lkt_features=FALSE,time_col=NULL,order_col=NULL,
  unknown_levels='error',feature_encoder='none',encoder_hidden_sizes=16L,
  time_feature_cols=character(),time_missing_cols=character(),retrieval_heads=0L,
  retrieval_filter_cols=character(),zero_init_embedding_cols=character())
test_gru_model <- function(...)do.call(LKT::GRUModel,
  utils::modifyList(test_gru_minimal_settings(),list(...),keep.null=TRUE))
test_sequence_model <- function(unit=c('gru','mamba'),...,layer_dropout=0,unit_options=list()) {
  controls <- test_gru_minimal_settings();controls$gru_dropout <- NULL
  do.call(LKT::SequenceModel,c(list(unit=match.arg(unit),layer_dropout=layer_dropout,unit_options=unit_options),
    utils::modifyList(controls,list(...),keep.null=TRUE)))
}

# Holdout-isolation checks require exact fitted weights. On failure, distinguish
# changed training from an unstable same-data repeat without relaxing the check.
test_gru_same_weights <- function(fit,other,label,initial_weights,repeat_fit) {
  if(identical(fit$model$weights,other$model$weights))return(invisible(TRUE))
  difference <- function(a,b) {
    if(!identical(names(a),names(b)) || !identical(lapply(a,dim),lapply(b,dim)))return(Inf)
    max(vapply(names(a),function(n)max(abs(a[[n]]-b[[n]])),numeric(1)))
  }
  repeated <- repeat_fit()
  columns <- setdiff(names(fit$training_history),'elapsed_seconds')
  stop(sprintf(paste0('%s: exact holdout-isolation weights differ; max heldout difference=%.12g; ',
    'same-data repeat difference=%.12g; saved weights changed during probes=%s; ',
    'preprocessing identical=%s; training history identical=%s; selected epochs=%s/%s/%s.'),
    label,difference(fit$model$weights,other$model$weights),
    difference(fit$model$weights,repeated$model$weights),
    !identical(initial_weights,serialize(fit$model$weights,NULL)),
    identical(fit$model$preprocessing,other$model$preprocessing),
    identical(fit$training_history[columns],other$training_history[columns]),
    fit$selected_epoch,other$selected_epoch,repeated$selected_epoch),call.=FALSE)
}
