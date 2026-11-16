library(LKT)
defaults <- GRUModel()$settings
expected <- list(hidden_size=32L,num_layers=3L,head_hidden_sizes=128L,
  categorical_cols=c('KC..Default.','KC..Cluster.'),embedding_dims=16L,
  input_dropout=.1,gru_dropout=.1,head_dropout=.1,use_lkt_features=TRUE,
  time_col=NULL,order_col='CF..Time.',unknown_levels='zero',
  feature_encoder='mlp',encoder_hidden_sizes=32L,encoder_output_size=16L,
  encoder_dropout=0,retrieval_heads=1L,retrieval_size=32L,retrieval_window=100L,
  retrieval_filter_cols=c('KC..Default.','KC..Cluster.'),zero_init_embedding_cols='KC..Default.',
  retrieval_fusion='replace',retrieval_decay='none',retrieval_cue_cols=character(),
  inference_mode='none',input_to_all_layers=FALSE,joint_encoder_output_size=NULL,
  numeric_fusion='concat',categorical_fusion='concat',input_fusion='concat',
  numeric_encoder='combined',encoder_output_sizes=c(numeric=32L),
  response_encoding='binary',retrieval_value_mode='raw',
  time_feature_cols=c('gru_item_age_seconds','gru_mean_prior_spacing_seconds'),
  time_missing_cols='gru_mean_prior_spacing_seconds')
for(name in names(expected))stopifnot(identical(defaults[[name]],expected[[name]]))
stopifnot(identical(names(lkt_gru_defaults()),names(formals(GRUModel))),
  identical(formals(LKT)$model,quote(LibLinearModel())))
terms <- data.frame(component='KC..Default.',feature='intercept')
stopifnot(lkt_model_config(terms)$backend=='LibLinear',
  isTRUE(lkt_model_config(terms)$intercept),
  identical(lkt_model_config(terms,backend='GRU')$intercept,FALSE),
  isTRUE(lkt_model_config(terms,backend='GRU',intercept=TRUE)$intercept),
  identical(lkt_model_config(terms,backend='GRU')$gru_options,defaults))
ns <- asNamespace('LKT')
stopifnot(identical(do.call(GRUModel,get('lkt_sequence_gru_settings',ns)(SequenceModel('gru')$settings))$settings,defaults))
encode <- get('lkt_gru_gui_encode',ns);decode <- get('lkt_gru_gui_decode',ns)
stopifnot(isTRUE(all.equal(decode(as.list(encode(defaults))),defaults)))
captured <- new.env(parent=emptyenv());scope <- new.env(parent=ns)
scope$lkt_form_gui <- function(func,...) {captured$func<-func;captured$args<-list(...);NULL}
editor <- get('lkt_gru_gui',ns);environment(editor)<-scope;editor()
stopifnot(isTRUE(all.equal(decode(as.list(formals(captured$func))),defaults)))
override <- GRUModel(hidden_size=7L,num_layers=2L,head_hidden_sizes=9L,
  feature_encoder='none',retrieval_heads=0L,retrieval_filter_cols=character(),
  input_to_all_layers=TRUE,joint_encoder_output_size=NULL,numeric_encoder='separate',
  encoder_output_sizes=integer(),retrieval_value_mode='projected')$settings
stopifnot(override$hidden_size==7L,override$num_layers==2L,override$head_hidden_sizes==9L,
  override$feature_encoder=='none',override$retrieval_heads==0L,override$input_to_all_layers)
cat('PASS literal GRU argument defaults, explicit overrides, GUI/config defaults and unchanged LibLinear default.\n')
if(requireNamespace('torch',quietly=TRUE) && torch::torch_is_installed()) {
  set.seed(12)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:6),each=10),
    KC..Default.=rep(c('a','b'),30),KC..Cluster.=rep(c('k1','k2'),30),
    CF..Time.=rep(seq(10,100,10),6),CF..ansbin.=rbinom(60,1,.6))
  d$CF..reltime. <- d$CF..Time.
  d <- LKTGRUTimeFeatures(d)
  fit <- LKT(d,components=c('Anon.Student.Id','KC..Default.','KC..Default.'),
    features=c('logitdec','ppes','logitdec'),fixedpars=c(.9,.35,.39,.02,.72,.58),verbose=FALSE,
    model=GRUModel(epochs=1L,batch_size=3L,validation_fraction=0,early_stopping=FALSE))
  stopifnot(all(is.finite(fit$prediction)),fit$gru_settings$num_layers==3L,
    identical(dim(fit$model$weights[['head.0.weight']]),c(128L,96L)),
    identical(fit$model$recipe$interc,FALSE),length(fit$model$preprocessing$numeric_names)==6L,
    identical(dim(fit$model$weights[['numeric_encoder.0.0.weight']]),c(32L,6L)),
    identical(dim(fit$model$weights[['gru.weight_ih_l1']]),c(96L,65L)),
    !any(grepl('response_encoder|retrieval.value',names(fit$model$weights))))
  runtime <- LKTGRURuntime(fit)
  a <- LKTGRUStep(fit,LKTGRUState(fit),d[1:5,],runtime)
  b <- LKTGRUStep(fit,a$state,d[6:10,],runtime)
  stopifnot(max(abs(c(a$predictions,b$predictions)-fit$prediction[1:10]))<3e-6)
  stopifnot(!'time:log1p_gap_seconds' %in% fit$model$preprocessing$numeric_names,
    identical(fit$gru_settings$order_col,'CF..Time.'))
  # The new default must be numerically identical to the reviewed omit-gap
  # configuration, including its training-only scaling and learned weights.
  prior <- fit$gru_settings;prior$time_col <- 'CF..Time.'
  prior$numeric_feature_cols <- fit$model$preprocessing$numeric_names
  matched <- LKT(d,components=c('Anon.Student.Id','KC..Default.','KC..Default.'),
    features=c('logitdec','ppes','logitdec'),fixedpars=c(.9,.35,.39,.02,.72,.58),verbose=FALSE,
    model=do.call(GRUModel,prior))
  stopifnot(identical(fit$model$preprocessing,matched$model$preprocessing),
    identical(fit$model$weights,matched$model$weights),identical(fit$prediction,matched$prediction))
  # Ordering still protects continuation across calls with no gap coordinate.
  earlier <- d[1,,drop=FALSE];earlier$CF..Time. <- 0
  error <- tryCatch({LKTGRUStep(fit,b$state,earlier,runtime);NULL},error=identity)
  stopifnot(inherits(error,'error'),grepl('order goes backwards',conditionMessage(error),fixed=TRUE))
  cat('PASS six-input default, exact reviewed omit-gap equivalence, explicit ordering and continuation.\n')
} else if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required.')
