library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
fail <- function(expr,pattern) {
  e <- tryCatch({force(expr);NULL},error=identity)
  stopifnot(inherits(e,'error'),grepl(pattern,conditionMessage(e),fixed=TRUE))
}
settings <- list(input_to_all_layers=TRUE,additional_input_init='zero',initial_model_file='initial-model.rds',
  retrieval_cue_cols='KC..Default.',zero_init_embedding_cols='KC..Default.',retrieval_decay_lr_multiplier=20,
  retrieval_heads=2L,retrieval_size=6L,retrieval_window=NULL,retrieval_dropout=.12,retrieval_filter_cols=c('item','skill'),
  retrieval_fusion='concat',retrieval_decay='akt',retrieval_decay_init=.2,retrieval_share_qk=FALSE,retrieval_bias=FALSE,
  time_feature_cols=c('item_age','prior_spacing'),time_missing_cols='prior_spacing',feature_encoder='mlp',encoder_hidden_sizes=c(8L,5L),encoder_output_size=6L,
  encoder_activation='tanh',encoder_dropout=.15,hidden_size=11L,num_layers=3L,head_hidden_sizes=c(7L,4L),gru_dropout=.15,
  inference_mode='gaussian',latent_size=6L,inference_hidden_sizes=c(9L,7L),inference_activation='gelu',
  inference_dropout=.2,temporal_kl_beta=.001,logvar_bounds=c(-8,3),logvar_init=-3,
  numeric_cols=c('gap','difficulty'),numeric_transforms=c(gap='log1p'),embedding_dims=c(KC..Default.=5L),
  optimizer='sgd',optimizer_options=list(momentum=.8,nesterov=TRUE),
  scheduler='plateau',scheduler_options=list(factor=.3),validation_fraction=.15,
  history_group_cols=c('Anon.Student.Id','KC..Default.'))
settings <- utils::modifyList(test_gru_minimal_settings(),settings,keep.null=TRUE)
terms <- data.frame(component='KC..Default.',feature='intercept')
cfg <- lkt_model_config(terms,backend='GRU',gru_options=settings,options=list(verbose=FALSE))
stopifnot(identical(cfg$gru_options,do.call(test_gru_model,settings)$settings),
  identical(names(lkt_gru_defaults()),names(formals(GRUModel))))
path <- tempfile(fileext='.rds');saveRDS(cfg,path);loaded <- readRDS(path);unlink(path)
stopifnot(identical(cfg,loaded))
call <- lkt_config_call(loaded)
stopifnot(identical(eval(call$model)$settings,cfg$gru_options))
ns <- asNamespace('LKT')
encode <- get('lkt_gru_gui_encode',ns);decode <- get('lkt_gru_gui_decode',ns)
stopifnot(isTRUE(all.equal(decode(as.list(encode(cfg$gru_options))),cfg$gru_options)))
# Every public control must have an editor field and exactly one tab. Capture
# the form definition without opening windows or requiring a display server.
captured <- new.env(parent=emptyenv())
scope <- new.env(parent=ns)
scope$lkt_form_gui <- function(func,...) {captured$func<-func;captured$args<-list(...);NULL}
editor <- get('lkt_gru_gui',ns);environment(editor)<-scope
editor(cfg$gru_options)
fields <- names(formals(captured$func));tabs <- unlist(captured$args$sections,use.names=FALSE)
stopifnot(setequal(fields,names(lkt_gru_defaults())),setequal(tabs,fields),!anyDuplicated(tabs))
simple <- lkt_model_config(terms);simple$gru_options <- NULL
stopifnot(is.call(lkt_config_call(simple)))
fail(lkt_model_config(terms,backend='GRU',gru_options=list(hidden_siz=12)),'supported')
fail(lkt_model_config(terms,backend='GRU',latency='Wald'),'accuracy-only')
fail(lkt_model_config(terms,gru_options=settings),'backend GRU')
fail(lkt_model_config(data.frame(component='KC..Default.',feature='ppe'),backend='GRU'),'fixed nonlinear')
cat('PASS complete GRU defaults, config serialization, generated calls, data-only GUI roundtrip and validation\n')
