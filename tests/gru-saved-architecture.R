library(LKT)
ns <- asNamespace('LKT');restore <- get('lkt_gru_saved_controls',ns)
fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))
selectors <- c('numeric_encoder','response_encoding','retrieval_value_mode')
previous <- GRUModel(time_col='CF..Time.',numeric_encoder='separate',encoder_output_sizes=integer(),
  response_encoding='mlp',retrieval_value_mode='projected')$settings
legacy <- previous;legacy[selectors] <- NULL
stopifnot(identical(get('lkt_gru_settings',ns)(restore(legacy)),previous))
bad <- previous;bad$response_encoding <- NULL;fail(restore(bad))
bad <- GRUModel()$settings;bad[selectors] <- NULL;fail(restore(bad))
terms <- data.frame(component='KC..Default.',feature='intercept')
config <- lkt_model_config(terms,intercept=TRUE,backend='GRU',gru_options=previous)
config$gru_options[selectors] <- NULL
checked <- get('lkt_config_arguments',ns)(config)
stopifnot(identical(checked$model$settings,previous),identical(checked$interc,TRUE))
stopifnot(isTRUE(all.equal(do.call(GRUModel,as.list(lkt_config_call(config)$model)[-1L])$settings,previous)))
if(requireNamespace('torch',quietly=TRUE) && torch::torch_is_installed()) {
  set.seed(19)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:6),each=10),
    KC..Default.=rep(c('a','b'),30),KC..Cluster.=rep(c('k1','k2'),30),
    CF..Time.=rep(seq(10,100,10),6),CF..ansbin.=rbinom(60,1,.6))
  d <- LKTGRUTimeFeatures(d)
  fit <- LKT(d,components='KC..Default.',features='intercept',interc=TRUE,verbose=FALSE,
    model=GRUModel(time_col='CF..Time.',numeric_encoder='separate',encoder_output_sizes=integer(),
      response_encoding='mlp',retrieval_value_mode='projected',
      epochs=1L,batch_size=3L,validation_fraction=0,early_stopping=FALSE))
  legacy_fit <- fit
  legacy_fit$model$settings[selectors] <- NULL
  legacy_fit$gru_settings[selectors] <- NULL
  frozen <- serialize(legacy_fit,NULL)
  q <- predict_lkt(legacy_fit,d)$predictions
  stopifnot(max(abs(q-fit$prediction))<3e-6,identical(frozen,serialize(legacy_fit,NULL)))
  a <- LKTGRUStep(legacy_fit,LKTGRUState(legacy_fit),d[1:5,])
  b <- LKTGRUStep(legacy_fit,a$state,d[6:10,])
  stopifnot(max(abs(c(a$predictions,b$predictions)-fit$prediction[1:10]))<3e-6)
  cat('PASS previous saved architecture, configuration/call, unchanged weights, replay and continuation.\n')
} else if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required.')
