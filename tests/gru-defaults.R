library(LKT)
defaults <- GRUModel()$settings
expected <- list(hidden_size=32L,num_layers=3L,head_hidden_sizes=128L,
  categorical_cols=c('KC..Default.','KC..Cluster.'),embedding_dims=16L,
  input_dropout=.1,gru_dropout=.1,head_dropout=.1,use_lkt_features=TRUE,
  time_col='CF..Time.',order_col='CF..Time.',unknown_levels='zero',
  feature_encoder='mlp',encoder_hidden_sizes=32L,encoder_output_size=16L,
  encoder_dropout=0,retrieval_heads=1L,retrieval_size=32L,retrieval_window=100L,
  retrieval_filter_cols=c('KC..Default.','KC..Cluster.'),zero_init_embedding_cols='KC..Default.',
  retrieval_fusion='replace',retrieval_decay='none',retrieval_cue_cols=character(),
  inference_mode='none',input_to_all_layers=FALSE,joint_encoder_output_size=NULL,
  numeric_fusion='concat',categorical_fusion='concat',input_fusion='concat',
  time_feature_cols=c('gru_item_age_seconds','gru_mean_prior_spacing_seconds'),
  time_missing_cols='gru_mean_prior_spacing_seconds')
for(name in names(expected))stopifnot(identical(defaults[[name]],expected[[name]]))
stopifnot(identical(names(lkt_gru_defaults()),names(formals(GRUModel))),
  identical(formals(LKT)$model,quote(LibLinearModel())))
terms <- data.frame(component='KC..Default.',feature='intercept')
stopifnot(lkt_model_config(terms)$backend=='LibLinear',
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
  input_to_all_layers=TRUE,joint_encoder_output_size=NULL)$settings
stopifnot(override$hidden_size==7L,override$num_layers==2L,override$head_hidden_sizes==9L,
  override$feature_encoder=='none',override$retrieval_heads==0L,override$input_to_all_layers)
cat('PASS literal GRU argument defaults, explicit overrides, GUI/config defaults and unchanged LibLinear default.\n')
if(requireNamespace('torch',quietly=TRUE) && torch::torch_is_installed()) {
  set.seed(12)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:6),each=10),
    KC..Default.=rep(c('a','b'),30),KC..Cluster.=rep(c('k1','k2'),30),
    CF..Time.=rep(seq(10,100,10),6),CF..ansbin.=rbinom(60,1,.6))
  d <- LKTGRUTimeFeatures(d)
  fit <- LKT(d,components='KC..Default.',features='intercept',verbose=FALSE,
    model=GRUModel(epochs=1L,batch_size=3L,validation_fraction=0,early_stopping=FALSE))
  stopifnot(all(is.finite(fit$prediction)),fit$gru_settings$num_layers==3L,
    identical(dim(fit$model$weights[['head.0.weight']]),c(128L,96L)))
  runtime <- LKTGRURuntime(fit)
  a <- LKTGRUStep(fit,LKTGRUState(fit),d[1:5,],runtime)
  b <- LKTGRUStep(fit,a$state,d[6:10,],runtime)
  stopifnot(max(abs(c(a$predictions,b$predictions)-fit$prediction[1:10]))<3e-6)
  cat('PASS default architecture fit, head dimensions and continuation.\n')
} else if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required.')
