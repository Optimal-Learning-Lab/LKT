library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
stopifnot(!test_gru_model()$settings$input_to_all_layers,
  inherits(tryCatch(test_gru_model(input_to_all_layers=1),error=identity),'error'))
if(requireNamespace('torch',quietly=TRUE) && torch::torch_is_installed()) {
  ns <- asNamespace('LKT');stack <- get('lkt_gru_recurrent',ns)
  weights <- get('lkt_gru_weights',ns);load <- get('lkt_gru_load_weights',ns)
  close <- function(x,y,tol=2e-6)stopifnot(isTRUE(all.equal(x,y,tolerance=tol,check.attributes=FALSE)))
  s <- test_gru_model(hidden_size=3L,num_layers=3L,input_to_all_layers=TRUE,dtype='float64')$settings
  tensor <- function(x)torch::torch_tensor(x,dtype=torch::torch_float64())
  torch::torch_manual_seed(27)
  net <- stack(2L,s)$to(dtype=torch::torch_float64());net$eval();w <- weights(net)
  x <- matrix(sin(1:8),4,2);previous <- matrix(cos(1:9)/4,3,3)
  inputs <- tensor(array(x,c(1,4,2)));hidden <- tensor(array(previous,c(3,1,3)))
  actual <- net(inputs,hidden);manual <- matrix(0,4,3);sigmoid <- function(x)1/(1+exp(-x))
  # Independent GRU equations verify which columns connect the original input to
  # each layer. Upper-layer input is [current lower output, original interaction].
  for(t in 1:4) {
    lower <- x[t,]
    for(layer in 1:3) {
      prefix <- paste0('layers.',layer-1L,'.')
      u <- if(layer==1L)x[t,] else c(lower,x[t,])
      a <- as.numeric(w[[paste0(prefix,'weight_ih_l1')]]%*%u+w[[paste0(prefix,'bias_ih_l1')]])
      b <- as.numeric(w[[paste0(prefix,'weight_hh_l1')]]%*%previous[layer,]+w[[paste0(prefix,'bias_hh_l1')]])
      reset <- sigmoid(a[1:3]+b[1:3]);update <- sigmoid(a[4:6]+b[4:6])
      candidate <- tanh(a[7:9]+reset*b[7:9])
      lower <- (1-update)*candidate+update*previous[layer,];previous[layer,] <- lower
    }
    manual[t,] <- lower
  }
  close(torch::as_array(actual[[1]])[1,,],manual,1e-10)
  close(torch::as_array(actual[[2]])[,1,],previous,1e-10)
  first <- net(inputs[,1:2,,drop=FALSE],hidden)
  second <- net(inputs[,3:4,,drop=FALSE],first[[2]])
  close(torch::as_array(second[[1]])[1,,],manual[3:4,],1e-10)
  # With the new input columns set to zero, matched shared weights must reproduce
  # the standard stack exactly. This also checks that no extra biases were added.
  old_settings <- s;old_settings$input_to_all_layers <- FALSE
  standard <- stack(2L,old_settings)$to(dtype=torch::torch_float64());standard$eval()
  old <- weights(standard);equivalent <- w
  for(layer in 1:3)for(kind in c('weight_ih','weight_hh','bias_ih','bias_hh')) {
    target <- paste0('layers.',layer-1L,'.',kind,'_l1');source <- paste0(kind,'_l',layer)
    equivalent[[target]] <- if(layer>1L && kind=='weight_ih')cbind(old[[source]],matrix(0,9,2)) else old[[source]]
  }
  load(net,equivalent,s)
  close(torch::as_array(net(inputs,hidden)[[1]]),torch::as_array(standard(inputs,hidden)[[1]]),1e-10)
  stopifnot(sum(lengths(w))-sum(lengths(old))==2L*3L*3L*2L)
  one <- s;one$num_layers <- 1L
  torch::torch_manual_seed(4);a <- weights(stack(2L,one))
  one$input_to_all_layers <- FALSE;torch::torch_manual_seed(4);b <- weights(stack(2L,one))
  stopifnot(identical(a,b))

  set.seed(109)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:8),each=12),
    KC..Default.=rep(c('a','b','c'),32),CF..Time.=rep(seq(0,1100,100),8),
    CF..ansbin.=rbinom(96,1,.6),fold=rep(c(1,1,1,1,1,2,3,3),each=12))
  d <- d[-c(11,23,34,47,57,69,82,94),]
  for(mode in c('none','deterministic','gaussian'))for(heads in 0:1) {
    s <- test_gru_model(hidden_size=6L,num_layers=3L,input_to_all_layers=TRUE,
      categorical_cols='KC..Default.',embedding_dims=4L,time_col='CF..Time.',order_col='CF..Time.',
      feature_encoder=if(heads)'mlp' else 'none',encoder_hidden_sizes=7L,encoder_output_size=4L,
      inference_mode=mode,latent_size=4L,inference_hidden_sizes=7L,
      temporal_kl_beta=if(mode=='gaussian').001 else 0,retrieval_heads=heads,retrieval_size=8L,
      retrieval_window=5L,epochs=2L,batch_size=3L,sequence_length=4L,
      gru_dropout=.1,validation_folds=2,seed=19L)$settings
    fit <- LKT(d,usefolds=1,components='KC..Default.',features='intercept',model=do.call(test_gru_model,s),verbose=FALSE)
    close(predict_lkt(fit,d)$predictions,fit$prediction)
    rt <- LKTGRURuntime(fit);first <- LKTGRUStep(fit,LKTGRUState(fit),d[1:5,],rt)
    rest <- LKTGRUStep(fit,first$state,d[6:nrow(d),],rt)
    close(c(first$predictions,rest$predictions),fit$prediction)
    pending <- d[6,,drop=FALSE];pending$CF..ansbin. <- NA_real_
    peek <- LKTGRUStep(fit,first$state,pending,rt)
    close(peek$predictions,fit$prediction[6]);stopifnot(identical(peek$state$inference_context,first$state$inference_context))
    before <- serialize(first$state,NULL)
    close(LKTGRUScore(fit,first$state,d[c(6,6),],rt),rep(fit$prediction[6],2))
    stopifnot(identical(before,serialize(first$state,NULL)))
    file <- tempfile(fileext='.rds');saveRDS(list(fit=fit,state=first$state),file)
    restored <- readRDS(file);unlink(file)
    close(LKTGRUStep(restored$fit,restored$state,d[6:nrow(d),])$predictions,rest$predictions)
    changed <- d;changed$CF..ansbin.[6:11] <- 1-changed$CF..ansbin.[6:11]
    close(predict_lkt(fit,changed)$predictions[1:6],fit$prediction[1:6])
    # The new blocks of both upper-layer input matrices must receive gradients.
    net <- rt$network;net$train();net$zero_grad()
    encoded <- get('lkt_gru_encode',ns)(d[1:11,],fit$model$preprocessing,fit$model$recipe,s)
    batch <- get('lkt_gru_batch',ns)(encoded,d[1:11,],list(1:11),list(1:11),s)
    net(batch$numeric,batch$categorical,batch$answers)$logits$sum()$backward()
    for(layer in 2:3) {
      param <- net$named_parameters()[[paste0('gru.layers.',layer-1L,'.weight_ih_l1')]]
      stopifnot(!is.null(param$grad),param$grad[,seq.int(7L,param$size(2)),drop=FALSE]$abs()$sum()$item()>0)
    }
    changed <- d;changed$CF..ansbin.[d$fold==3] <- 1-changed$CF..ansbin.[d$fold==3]
    other <- LKT(changed,usefolds=1,components='KC..Default.',features='intercept',model=do.call(test_gru_model,s),verbose=FALSE)
    stopifnot(identical(fit$model$weights,other$model$weights))
  }
  cat('PASS all-layer independent equations, zero-link equivalence, parameter count, gradients, inference/retrieval combinations, causality, padding and portable continuation\n')
} else if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required.')
