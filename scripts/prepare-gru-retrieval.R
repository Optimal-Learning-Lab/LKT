# Add an editable retrieval configuration to an existing GRU specification. The
# original specification and fitted models stay unchanged; this does not train.
# Args: ROOT SOURCE_SPECIFICATION NEW_OUTPUT_DIRECTORY ATTENTION_HEADS
args <- commandArgs(TRUE);stopifnot(length(args)==4L)
root <- normalizePath(args[1L],winslash='/',mustWork=TRUE)
source_spec <- normalizePath(args[2L],winslash='/',mustWork=TRUE)
output <- args[3L];heads <- as.numeric(args[4L])
source(file.path(root,'scripts/load-current-lkt.R'))
stopifnot(!is.na(heads),is.finite(heads),heads>0L,heads==floor(heads),!dir.exists(output))
config <- readRDS(source_spec)
settings <- do.call(GRUModel,config$gru_options)$settings
settings$retrieval_heads <- heads
config$gru_options <- do.call(GRUModel,settings)$settings
arguments <- getFromNamespace('lkt_config_arguments','LKT')(config)
stopifnot(identical(arguments$model$settings,config$gru_options))
# Freeze the configured model and where it came from. New attention weights need
# training; neither an old checkpoint nor its published AUC describes this new model.
dir.create(output,recursive=TRUE)
saveRDS(config,file.path(output,'model-specification.rds'))
saveRDS(list(source_specification=source_spec,
  source_sha256=digest::digest(file=source_spec,algo='sha256'),release=lkt_release,
  status='Unfitted retrieval configuration',settings=config$gru_options),file.path(output,'provenance.rds'))
writeLines(c(paste('Attention heads:',heads),
  paste('Total attention width:',settings$retrieval_size),
  paste('History window:',settings$retrieval_window),
  paste('Memory fusion:',settings$retrieval_fusion),paste('Decay:',settings$retrieval_decay),
  'Unfitted specification. Existing trained weights and scientific results are unchanged.'),file.path(output,'configuration.txt'))
cat('Prepared editable',heads,'head retrieval specification:',output,'\n')
