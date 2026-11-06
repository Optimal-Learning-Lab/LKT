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
  fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))
  set.seed(12)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:8),each=12),
    KC..Default.=rep(c('a','b','c'),32),KC..Cluster.=rep(c('k1','k2','k1'),32),
    CF..Time.=rep(seq(0,1100,100),8),CF..ansbin.=rbinom(96,1,.6),
    fold=rep(c(1,1,1,1,1,2,3,3),each=12))
  settings <- list(hidden_size=5L,num_layers=2L,head_hidden_sizes=4L,
    categorical_cols=c('KC..Default.','KC..Cluster.'),embedding_dims=3L,
    latent_size=4L,inference_hidden_sizes=c(7L,6L),time_col='CF..Time.',order_col='CF..Time.',
    validation_folds=2,epochs=2L,sequence_length=5L,batch_size=3L,seed=7L)
  fit <- function(mode,beta=0) LKT(d,usefolds=1,components='KC..Default.',features='intercept',
    model=do.call(test_gru_model,c(settings,list(inference_mode=mode,temporal_kl_beta=beta))),verbose=FALSE)
  for(mode in c('deterministic','gaussian')) {
    model <- fit(mode,if(mode=='gaussian').001 else 0)
    stopifnot(all(is.finite(model$prediction)),all(is.finite(model$training_history$training_temporal_kl)))
    close(predict_lkt(model,d)$predictions,model$prediction)
    first <- LKTGRUStep(model,LKTGRUState(model),d[1:5,])
    second <- LKTGRUStep(model,first$state,d[6:12,])
    close(c(first$predictions,second$predictions),model$prediction[1:12])
    state <- LKTGRUState(model);p <- numeric(12)
    for(i in 1:12) {out <- LKTGRUStep(model,state,d[i,,drop=FALSE]);p[i] <- out$predictions;state <- out$state}
    close(p,model$prediction[1:12])
    changed <- d;changed$CF..ansbin.[5:12] <- 1-changed$CF..ansbin.[5:12]
    close(predict_lkt(model,changed)$predictions[1:5],model$prediction[1:5])
    pending <- d[6,,drop=FALSE];pending$CF..ansbin. <- NULL
    peek <- LKTGRUStep(model,first$state,pending)
    close(peek$predictions,model$prediction[6])
    stopifnot(identical(peek$state$hidden,first$state$hidden),
      identical(peek$state$inference_context,first$state$inference_context))
    resolved <- LKTGRUStep(model,peek$state,d[6,,drop=FALSE])
    close(resolved$predictions,model$prediction[6])
    path <- tempfile(fileext='.rds');saveRDS(list(model=model,state=first$state),path)
    loaded <- readRDS(path);unlink(path)
    close(LKTGRUStep(loaded$model,loaded$state,d[6:12,])$predictions,second$predictions)
    # Evaluation must be deterministic even for the sampling model. Validation labels
    # may affect stopping, but held-out test labels must never alter fitted weights.
    close(predict_lkt(model,d)$predictions,predict_lkt(model,d)$predictions)
    changed <- d;changed$CF..ansbin.[d$fold==3] <- 1-changed$CF..ansbin.[d$fold==3]
    other <- LKT(changed,usefolds=1,components='KC..Default.',features='intercept',
      model=do.call(test_gru_model,model$gru_settings),verbose=FALSE)
    stopifnot(identical(model$model$weights,other$model$weights))
  }
  ns <- asNamespace('LKT');network <- get('lkt_gru_network',ns)
  tensor <- function(x)torch::torch_tensor(x,dtype=torch::torch_float32())
  prep <- list(numeric_names='gap',embedding_dims=3L,vocabularies=list(item=c('a','b')))
  controls <- do.call(test_gru_model,c(settings,list(inference_mode='gaussian')))$settings
  torch::torch_manual_seed(19);g <- network(prep,controls)
  controls$inference_mode <- 'deterministic'
  torch::torch_manual_seed(19);det <- network(prep,controls)
  gw <- g$state_dict();dw <- det$state_dict()
  for(n in names(dw))stopifnot(isTRUE(torch::torch_equal(dw[[n]],gw[[n]])))
  numeric <- tensor(array(seq(0,.7,.1),c(1,8,1)))
  categorical <- torch::torch_tensor(array(rep(2:3,4),c(1,8,1)),dtype=torch::torch_long())
  answers <- tensor(matrix(rep(c(0,1),4),1))
  g$eval();whole <- g(numeric,categorical,answers)
  a <- g(numeric[,1:3,,drop=FALSE],categorical[,1:3,,drop=FALSE],answers[,1:3,drop=FALSE])
  b <- g(numeric[,4:8,,drop=FALSE],categorical[,4:8,,drop=FALSE],answers[,4:8,drop=FALSE],a$hidden,a$context)
  close(torch::as_array(whole$logits),cbind(torch::as_array(a$logits),torch::as_array(b$logits)))
  close(torch::as_array(whole$kl_per_trial),cbind(torch::as_array(a$kl_per_trial),torch::as_array(b$kl_per_trial)))
  stopifnot(torch::as_array(whole$kl_valid)[1]==0,all(torch::as_array(whole$kl_valid)[-1]==1))
  # Independently compute KL(new || old), including the first state after a chunk
  # boundary. This catches a reversed KL or accidental smoothing between students.
  mu <- torch::as_array(whole$latent_mean)[1,,]
  lv <- torch::as_array(whole$latent_logvar)[1,,]
  expected <- sapply(2:8,function(t).5*mean(lv[t-1,]-lv[t,]+exp(lv[t,]-lv[t-1,])+
    (mu[t,]-mu[t-1,])^2*exp(-lv[t-1,])-1))
  close(torch::as_array(whole$kl_per_trial)[1,-1],expected)
  g$train();u <- g(numeric,categorical,answers)$logits;v <- g(numeric,categorical,answers)$logits
  stopifnot(any(abs(torch::as_array(u-v))>0))
  fail(test_gru_model(inference_mode='deterministic',temporal_kl_beta=.01))
  fail(test_gru_model(logvar_bounds=c(3,-3)));fail(test_gru_model(inference_hidden_sizes=0))
  cat('PASS deterministic/Gaussian fitting, causal inference, chunk-boundary KL, shared initialization, sampling, continuation and persistence\n')
}
