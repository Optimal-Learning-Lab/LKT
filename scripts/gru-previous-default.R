# Previous reviewed architecture, retained as explicit settings in the same LKT
# implementation. Use interc=TRUE to reproduce its original feature recipe.
gru_previous_default <- function(...) {
  controls <- list(time_col='CF..Time.',numeric_encoder='separate',encoder_output_sizes=integer(),
    response_encoding='mlp',retrieval_value_mode='projected')
  do.call(LKT::GRUModel,utils::modifyList(controls,list(...),keep.null=TRUE))
}
