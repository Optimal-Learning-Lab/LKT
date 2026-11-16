# Explicit opt-in alternative; this does not change GRUModel() defaults.
# Use the adapter with LKT(..., interc=FALSE) or a configuration with
# intercept=FALSE. The three engineered feature terms and their fixed nonlinear
# coefficients belong to the caller's recipe, not to this network architecture.
gru_simplified_alternative <- function(...) {
  controls <- list(time_col='CF..Time.',numeric_encoder='combined',encoder_output_sizes=c(numeric=32L),
    response_encoding='binary',retrieval_value_mode='raw')
  do.call(LKT::GRUModel,utils::modifyList(controls,list(...),keep.null=TRUE))
}

# Supply three scalar history-feature terms (with their explicit fixed nonlinear
# coefficients) for the seven-input recipe illustrated in the figure. Other
# recipes remain supported, but their numeric-input width is different.
gru_simplified_config <- function(terms,...,folds=NA,options=list()) {
  LKT::lkt_model_config(terms,intercept=FALSE,backend='GRU',folds=folds,
    options=options,gru_options=gru_simplified_alternative(...)$settings)
}
