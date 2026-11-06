# Load the same base specification used by the current network figure. Verify
# its historical checkpoint and data before opening the ordinary LKT editor.
args <- commandArgs(TRUE)
root <- normalizePath(if(length(args))args[1L] else '.',winslash='/',mustWork=TRUE)
action <- if(length(args)>=2L)args[2L] else 'verify'
stopifnot(action %in% c('verify','open'))
source(file.path(root,'scripts/load-current-lkt.R'))
base <- file.path(root,'docs/figures/gru-base-model')
selection <- readRDS(file.path(base,'selection.rds'))
specification <- file.path(base,'model-specification.rds')
hash <- function(path)digest::digest(file=path,algo='sha256')
checkpoint <- file.path(root,selection$checkpoint)
data_file <- file.path(root,selection$prepared_data)
stopifnot(hash(specification)==selection$specification_sha256,
  hash(checkpoint)==selection$checkpoint_sha256,
  hash(data_file)==selection$prepared_data_sha256)
config <- readRDS(specification);saved <- readRDS(checkpoint)
settings <- getFromNamespace('lkt_config_arguments','LKT')(config)$model$settings
# Historical source identity stays in selection$release. Resolve supported saved
# controls through the active package rather than selecting an old installation.
expected <- getFromNamespace('lkt_gru_settings','LKT')(saved$fit$gru_settings)
stopifnot(identical(settings,expected),
  identical(settings$zero_init_embedding_cols,'KC..Default.'),
  identical(settings$retrieval_filter_cols,c('KC..Default.','KC..Cluster.')),
  settings$retrieval_heads==1L,settings$retrieval_decay=='none',
  !length(settings$retrieval_cue_cols),!settings$input_to_all_layers)
cat('PASS: current base specification, saved checkpoint, data and central release identity.\n')
if(action=='open') {
  # Editing changes a fresh specification; the historical trained checkpoint
  # remains untouched. A new fit starts only when the user runs it in FGUI.
  output <- if(length(args)>=3L)args[3L] else file.path(root,'../LKT-outputs/gru-base-fgui')
  dir.create(output,recursive=TRUE,showWarnings=FALSE)
  lkt_gui(config,modal=TRUE,dataset='RDS file',data_file=data_file,
    output_file=tempfile('gru-base-fit-',tmpdir=output,fileext='.rds'))
}
