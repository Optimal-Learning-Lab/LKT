# Configuration must remain usable without installing the optional Torch runtime.
library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))
loaded <- 'torch'%in%loadedNamespaces();set.seed(71);before <- .Random.seed
for(unit in c('gru','mamba')) {
  model <- test_sequence_model(unit)
  stopifnot(inherits(model,'LKTModel'),model$settings$unit==unit,
    !any(c('gru_bias','gru_dropout','input_to_all_layers','additional_input_init','initial_model_file')%in%names(model$settings)))
  cfg <- lkt_model_config(data.frame(component='KC..Default.',feature='lineafm'),backend='Sequence',
    sequence_options=list(unit=unit))
  path <- tempfile(fileext='.rds');saveRDS(cfg,path);restored <- readRDS(path);unlink(path)
  call <- lkt_config_call(restored)
  stopifnot(identical(call$model[[1L]],as.name('SequenceModel')),identical(call$model$unit,unit))
  gui_error <- tryCatch({lkt_gui(cfg,modal=FALSE);NULL},error=identity)
  stopifnot(inherits(gui_error,'error'),grepl('R APIs',conditionMessage(gui_error),fixed=TRUE))
}
fail(test_sequence_model('lstm'))
fail(test_sequence_model('mamba',unit_options=list(d_state=0)))
fail(test_sequence_model('mamba',unit_options=list(d_conv=.Machine$integer.max+1)))
fail(test_sequence_model('mamba',unit_options=list(d_state=.Machine$integer.max+1)))
fail(test_sequence_model('mamba',unit_options=list(expand=.Machine$integer.max+1)))
fail(test_sequence_model('mamba',unit_options=list(dt_rank=.Machine$integer.max+1)))
fail(test_sequence_model('mamba',unit_options=list(bias=1L)))
stopifnot(identical(before,.Random.seed),identical(loaded,'torch'%in%loadedNamespaces()))
cat('PASS architecture configuration, canonical options, portable calls, invalid dimensions and optional Torch boundary\n')
