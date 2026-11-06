# Open the exact specification of a head-width checkpoint in the existing FGUI.
# Arguments: repository root, checkpoint RDS, output folder, and prepare/open.
args <- commandArgs(TRUE)
stopifnot(length(args)==4L,args[4L] %in% c('prepare','open'))
root <- normalizePath(args[1L],winslash='/',mustWork=TRUE)
checkpoint <- normalizePath(args[2L],winslash='/',mustWork=TRUE)
output <- args[3L];action <- args[4L]
source(file.path(root,'scripts/load-current-lkt.R'))
suppressPackageStartupMessages(library(LKT))

experiment <- dirname(dirname(checkpoint))
protocol_file <- file.path(experiment,'head_width_protocol.rds')
protocol <- readRDS(protocol_file);saved <- readRDS(checkpoint);fit <- saved$fit
stopifnot(identical(saved$protocol_sha256,digest::digest(protocol_file,algo='sha256',file=TRUE)),
  identical(fit$gru_settings,protocol$settings[[saved$run_id]]))
data_file <- file.path(experiment,'prepared_data.rds')
stopifnot(identical(digest::digest(data_file,algo='sha256',file=TRUE),protocol$data_sha256))

# Reconstruct ordered feature terms from the fitted recipe, retaining enough digits
# to round-trip every fixed nonlinear parameter exactly. The trained weights remain
# in the original checkpoint; the FGUI edits a specification, not those weights.
recipe <- fit$model$recipe
counts <- getFromNamespace('lkt_feature_param_counts','LKT')(recipe$features)
fixed <- split(recipe$fixedpars,rep(seq_along(counts),counts))
terms <- data.frame(component=recipe$components,feature=recipe$features,
  fixed=vapply(seq_along(counts),function(i)paste(sprintf('%.17g',fixed[[as.character(i)]]),collapse=';'),character(1)),
  interaction=ifelse(is.na(recipe$interacts),'',recipe$interacts),connector=c('+',recipe$connectors))
config <- lkt_model_config(terms,intercept=recipe$interc,backend='GRU',folds=1,
  options=list(verbose=FALSE,maketimes=recipe$maketimes),gru_options=fit$gru_settings)
arguments <- getFromNamespace('lkt_config_arguments','LKT')(config)
stopifnot(identical(arguments$model$settings,fit$gru_settings),
  identical(arguments$fixedpars,recipe$fixedpars),identical(arguments$components,recipe$components),
  identical(arguments$features,recipe$features),identical(arguments$interacts,recipe$interacts),
  identical(arguments$connectors,recipe$connectors),identical(arguments$interc,recipe$interc))
attr(config,'gui_note') <- paste0('Checkpoint ',saved$run_id,' | ',fit$parameter_count,
  ' parameters | restored epoch ',fit$selected_epoch,
  '. Exact saved configuration; opening this editor does not train or change weights.')

# Save a loadable specification and an explicit pointer to its trained checkpoint.
# Repeated openings may confirm these files but must not silently replace a different
# configuration. The original benchmark folder is kept read-only by this launcher.
dir.create(output,recursive=TRUE,showWarnings=FALSE)
output <- normalizePath(output,winslash='/',mustWork=TRUE)
specification <- file.path(output,'model-specification.rds')
if(file.exists(specification))stopifnot(identical(readRDS(specification),config)) else saveRDS(config,specification)
writeLines(c(paste('Checkpoint:',checkpoint),paste('Run:',saved$run_id),
  paste('Checkpoint SHA256:',digest::digest(checkpoint,algo='sha256',file=TRUE)),
  paste('Original protocol SHA256:',saved$protocol_sha256),
  paste('Configuration:',specification),paste('Data:',data_file),
  paste('GRU:',fit$gru_settings$num_layers,'layers of',fit$gru_settings$hidden_size,'units'),
  paste('Head hidden widths:',paste(fit$gru_settings$head_hidden_sizes,collapse=',')),
  paste('Restored epoch:',fit$selected_epoch),paste('Parameters:',fit$parameter_count),
  'PASS: all GRU settings, ordered features and fixed nonlinear parameters round-trip exactly.',
  'FGUI displays an editable specification. Original fitted weights are retained in the checkpoint.',
  'Run and save model starts a NEW fit. Preview and GRU settings do not train.'),file.path(output,'checkpoint-details.txt'))
cat('Prepared exact checkpoint configuration:',specification,'\n');flush.console()
if(action=='open') {
  stopifnot(requireNamespace('fgui',quietly=TRUE))
  lkt_gui(config,modal=TRUE,dataset='RDS file',data_file=data_file,
    output_file=file.path(output,'new-fit.rds'))
}
