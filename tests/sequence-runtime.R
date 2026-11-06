library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed()) {
  if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required')
} else {
close <- function(a,b)stopifnot(isTRUE(all.equal(a,b,tolerance=3e-6,check.attributes=FALSE)))
fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))
set.seed(91)
d <- data.frame(Anon.Student.Id=rep(paste0('s',1:6),c(7,5,9,4,8,6)),
  KC..Default.=rep(c('a','b','c'),13),CF..ansbin.=rbinom(39,1,.6),ability=rep(1:6,c(7,5,9,4,8,6)))
d$CF..Time. <- ave(seq_len(nrow(d)),d$Anon.Student.Id,FUN=seq_along)*100
for(unit in c('gru','mamba'))for(mode in c('none','deterministic','gaussian')) {
  settings <- list(unit=unit,epochs=1L,hidden_size=4L,num_layers=2L,head_hidden_sizes=3L,
    embedding_dims=2L,latent_size=3L,inference_hidden_sizes=4L,inference_mode=mode,
    numeric_cols='ability',use_lkt_features=TRUE,time_col='CF..Time.',order_col='CF..Time.',
    validation_fraction=0,early_stopping=FALSE,seed=6L,sequence_length=3L,
    feature_encoder='mlp',encoder_hidden_sizes=3L,encoder_output_size=2L,
    retrieval_heads=1L,retrieval_size=4L,retrieval_window=4L,retrieval_decay='exponential',
    retrieval_decay_lr_multiplier=2,retrieval_filter_cols='KC..Default.')
  if(unit=='mamba')settings$unit_options <- list(d_state=2L,d_conv=3L)
  canonical <- do.call(test_sequence_model,settings)$settings
  stopifnot(!any(c('gru_bias','gru_dropout','input_to_all_layers','additional_input_init','initial_model_file')%in%names(canonical)))
  fit <- LKT(d,features='lineafm',components='KC..Default.',verbose=FALSE,model=do.call(test_sequence_model,settings))
  stopifnot(fit$model_name=='Sequence',all(is.finite(fit$prediction)))
  runtime <- LKTSequenceRuntime(fit);empty <- LKTSequenceState(fit)
  complete <- LKTSequenceStep(fit,empty,d,runtime)
  close(complete$predictions,fit$prediction)
  split <- LKTSequenceStep(fit,empty,d[1:3,],runtime)
  rest <- LKTSequenceStep(fit,split$state,d[4:7,],runtime)
  close(c(split$predictions,rest$predictions),fit$prediction[1:7])
  before <- serialize(split$state,NULL);rseed <- .Random.seed;trng <- torch::torch_get_rng_state();threads <- torch::torch_get_num_threads()
  candidates <- d[rep(4,3),];candidates$KC..Default. <- c('a','b','c')
  p <- LKTSequenceScore(fit,split$state,candidates,runtime)
  close(p,rev(LKTSequenceScore(fit,split$state,candidates[3:1,],runtime)))
  close(p,LKTSequenceScore(fit,split$state,candidates,runtime))
  stopifnot(identical(before,serialize(split$state,NULL)),identical(rseed,.Random.seed),
    torch::torch_equal(trng,torch::torch_get_rng_state()),threads==torch::torch_get_num_threads())
  unanswered <- d[4,,drop=FALSE];unanswered$CF..ansbin. <- NA_real_
  pending <- LKTSequenceStep(fit,split$state,unanswered,runtime)
  close(pending$predictions,fit$prediction[4]);stopifnot(identical(pending$state$hidden,split$state$hidden))
  answered <- LKTSequenceStep(fit,pending$state,d[4,,drop=FALSE],runtime)
  close(answered$predictions,pending$predictions)
  fail(LKTSequenceStep(fit,pending$state,d[5,,drop=FALSE],runtime))
  wrong <- fit;wrong$model$id <- 'wrong';fail(LKTSequenceStep(wrong,split$state,d[4,,drop=FALSE],runtime))
  changed <- d;changed$CF..ansbin.[4] <- 1-changed$CF..ansbin.[4]
  close(LKTSequenceStep(fit,empty,changed[1:4,],runtime)$predictions,fit$prediction[1:4])
  close(predict_lkt(fit,data=d)$predictions,fit$prediction)
  saved <- tempfile(fileext='.rds');saveRDS(list(fit=fit,state=split$state,data=d[4:7,],expected=rest$predictions),saved)
  restored <- readRDS(saved)
  close(LKTSequenceStep(restored$fit,restored$state,restored$data)$predictions,restored$expected)
  if(unit=='mamba' && mode=='none') {
    script <- tempfile(fileext='.R')
    writeLines(c('library(LKT)',paste0('x <- readRDS(',deparse(saved),')'),
      'p <- LKTSequenceStep(x$fit,x$state,x$data)$predictions',
      'stopifnot(max(abs(p-x$expected))<3e-6)'),script)
    stopifnot(system2(file.path(R.home('bin'),'Rscript'),c('--vanilla',shQuote(script)))==0L)
    unlink(script)
  }
  unlink(saved)
}
# Generic GRU construction must preserve the historical adapter's initial draws.
g <- list(epochs=1L,hidden_size=3L,embedding_dims=2L,validation_fraction=0,early_stopping=FALSE)
a <- LKT(d,features='lineafm',components='KC..Default.',verbose=FALSE,model=do.call(test_gru_model,g))
b <- LKT(d,features='lineafm',components='KC..Default.',verbose=FALSE,model=do.call(test_sequence_model,c(list(unit='gru'),g)))
stopifnot(identical(a$model$weights,b$model$weights),identical(a$prediction,b$prediction))
# Test holdout labels cannot influence parameters or train-only preprocessing.
d$fold <- ifelse(d$Anon.Student.Id %in% c('s5','s6'),3,1)
settings <- test_sequence_model('mamba',epochs=1L,hidden_size=3L,validation_fraction=0,early_stopping=FALSE,
  unit_options=list(d_state=2L,d_conv=2L))
a <- LKT(d,features='lineafm',components='KC..Default.',verbose=FALSE,usefolds=1,model=settings)
changed <- d;changed$CF..ansbin.[changed$fold==3] <- 1-changed$CF..ansbin.[changed$fold==3]
b <- LKT(changed,features='lineafm',components='KC..Default.',verbose=FALSE,usefolds=1,model=settings)
stopifnot(identical(a$model$weights,b$model$weights),identical(a$model$preprocessing,b$model$preprocessing))
# Portable configuration roundtrip through public fitting dispatch.
cfg <- lkt_model_config(data.frame(component='KC..Default.',feature='lineafm'),backend='Sequence',
  sequence_options=utils::modifyList(within(test_gru_minimal_settings(),rm(gru_dropout)),
    list(unit='mamba',layer_dropout=0,epochs=1L,hidden_size=3L,validation_fraction=0,early_stopping=FALSE),keep.null=TRUE))
portable <- tempfile(fileext='.rds');saveRDS(cfg,portable)
fit <- fit_lkt_config(d,readRDS(portable));unlink(portable)
stopifnot(fit$model_name=='Sequence')
generated <- lkt_config_call(cfg)
stopifnot(identical(generated$model[[1L]],as.name('SequenceModel')))
compiled <- eval(generated,list(data=d))
stopifnot(identical(compiled$model$weights,fit$model$weights),identical(compiled$prediction,fit$prediction))
fail(test_sequence_model('mamba',unit_options=list(input_to_all_layers=TRUE)))
fail(test_sequence_model('mamba',unit_options=list(d_conv=0L)))
cat('PASS sequence continuation, retrieval/inference, causality, persistence, identity, holdout isolation and configuration\n')
}
