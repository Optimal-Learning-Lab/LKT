library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed()) {
  if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required.')
} else {
  close <- function(a,b)stopifnot(isTRUE(all.equal(a,b,tolerance=3e-6,check.attributes=FALSE)))
  set.seed(93)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:8),each=12),
    KC..Default.=rep(c('a','b','c'),32),CF..Time.=rep(cumsum(seq(20,130,10)),8),
    auxiliary=rnorm(96),CF..ansbin.=rbinom(96,1,.6),fold=rep(c(1,1,1,1,1,2,3,3),each=12))
  for(mode in c('none','deterministic','gaussian')) {
    controls <- list(feature_encoder='mlp',encoder_hidden_sizes=c(8L,6L),encoder_output_size=4L,
      encoder_activation='tanh',numeric_cols='auxiliary',time_col='CF..Time.',order_col='CF..Time.',
      hidden_size=5L,num_layers=2L,embedding_dims=3L,head_hidden_sizes=7L,
      inference_mode=mode,latent_size=4L,inference_hidden_sizes=6L,
      temporal_kl_beta=if(mode=='gaussian').001 else 0,epochs=2L,batch_size=3L,sequence_length=4L,
      validation_folds=2,seed=7L)
    fit <- LKT(d,usefolds=1,components='KC..Default.',features='intercept',
      model=do.call(test_gru_model,controls),verbose=FALSE)
    close(predict_lkt(fit,d)$predictions,fit$prediction)
    runtime <- LKTGRURuntime(fit)
    a <- LKTGRUStep(fit,LKTGRUState(fit),d[1:5,],runtime)
    b <- LKTGRUStep(fit,a$state,d[6:12,],runtime)
    close(c(a$predictions,b$predictions),fit$prediction[1:12])
    pending <- d[6,,drop=FALSE];pending$CF..ansbin. <- NA_real_
    peek <- LKTGRUScore(fit,a$state,pending,runtime)
    close(peek,fit$prediction[6])
    path <- tempfile(fileext='.rds');saveRDS(list(fit=fit,state=a$state),path)
    restored <- readRDS(path);unlink(path)
    close(LKTGRUStep(restored$fit,restored$state,d[6:12,])$predictions,b$predictions)
    changed <- d;changed$CF..ansbin.[6:12] <- 1-changed$CF..ansbin.[6:12]
    close(predict_lkt(fit,changed)$predictions[1:6],fit$prediction[1:6])
    # Test labels may change future evaluation states, but never the trained weights.
    changed <- d;changed$CF..ansbin.[d$fold==3] <- 1-changed$CF..ansbin.[d$fold==3]
    other <- LKT(changed,usefolds=1,components='KC..Default.',features='intercept',
      model=do.call(test_gru_model,controls),verbose=FALSE)
    stopifnot(identical(fit$model$weights,other$model$weights))
    # Every tower participates in end-to-end optimization, including the response
    # tower whose effect reaches predictions only through previous interactions.
    net <- runtime$network;net$train();net$zero_grad()
    ns <- asNamespace('LKT');s <- fit$gru_settings
    encoded <- get('lkt_gru_encode',ns)(d[1:12,],fit$model$preprocessing,fit$model$recipe,s)
    batch <- get('lkt_gru_batch',ns)(encoded,d[1:12,],list(1:12),list(1:12),s)
    net(batch$numeric,batch$categorical,batch$answers)$logits$sum()$backward()
    parameters <- net$named_parameters()
    for(tower in c('response_encoder','time_encoder','auxiliary_encoder')) {
      selected <- parameters[startsWith(names(parameters),tower)]
      stopifnot(length(selected)>0,any(vapply(selected,function(p)
        !is.null(p$grad) && p$grad$abs()$sum()$item()>0,logical(1))))
    }
  }
  # Optional input blocks are absent rather than replaced with synthetic columns.
  for(columns in list(character(),'auxiliary')) {
    model <- LKT(d,usefolds=1,components='KC..Default.',features='intercept',verbose=FALSE,
      model=test_gru_model(feature_encoder='mlp',numeric_cols=columns,epochs=1L,hidden_size=4L,
        validation_folds=2,encoder_hidden_sizes=integer(),encoder_output_size=3L))
    stopifnot(all(is.finite(model$prediction)))
  }
  cat('PASS learned towers, all inference modes, gradients, causality, masks, runtime, continuation and persistence\n')
}
