# Named research profiles; all neural algorithms remain in the maintained LKT package.
# See ../README-GRU.md. Source scripts/load-current-lkt.R before constructing models.
gru_variant_catalog <- function(root) {
  path <- file.path(normalizePath(root,winslash='/',mustWork=TRUE),'scripts/gru-variants.json')
  catalog <- jsonlite::fromJSON(path,simplifyVector=FALSE)
  stopifnot(catalog$schema==1L,
    identical(names(catalog$variants),c('baseline_six','tracer_six','ground_truth_eight')))
  catalog
}

gru_variant_model <- function(variant,root,base_settings=NULL,overrides=list()) {
  catalog <- gru_variant_catalog(root)
  if(length(variant)!=1L||!is.character(variant)||is.na(variant)||!variant%in%names(catalog$variants))
    stop('Choose baseline_six, tracer_six or ground_truth_eight')
  if(!is.list(overrides)||(length(overrides) && (is.null(names(overrides))||
    any(!nzchar(names(overrides)))||anyDuplicated(names(overrides)))))stop('overrides must be uniquely named')
  protected <- c('subject_col','history_group_cols','categorical_cols','numeric_cols','time_col',
    'time_feature_cols','time_missing_cols','numeric_feature_cols','embedding_adaptation',
    'adaptive_embedding_cols','optional_adaptive_cols','plasticity','plasticity_time_col')
  if(any(names(overrides)%in%protected))stop('Overrides cannot change a named variant input/identity contract')
  s <- if(is.null(base_settings))LKT::GRUModel()$settings else do.call(LKT::GRUModel,base_settings)$settings
  for(name in names(overrides))s[name] <- overrides[name]
  profile <- catalog$variants[[variant]];common <- catalog$common
  strings <- function(x)as.character(unlist(x,use.names=FALSE))
  s$subject_col <- common$subject_col;s$history_group_cols <- common$subject_col
  s$categorical_cols <- strings(common$categorical_cols)
  s$numeric_cols <- strings(profile$numeric_cols)
  s['time_col'] <- list(NULL);s['numeric_feature_cols'] <- list(NULL)
  s$time_feature_cols <- strings(common$time_feature_cols)
  s$time_missing_cols <- strings(common$time_missing_cols)
  s$plasticity <- common$plasticity;s$plasticity_time_col <- common$plasticity_time_col
  s$embedding_adaptation <- profile$embedding_adaptation
  s$adaptive_embedding_cols <- strings(profile$adaptive_embedding_cols)
  s$optional_adaptive_cols <- character()
  if(profile$embedding_adaptation=='per_student')s$embedding_learning_rate <- .01
  do.call(LKT::GRUModel,s)
}
