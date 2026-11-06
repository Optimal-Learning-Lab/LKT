library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
close <- function(a,b)stopifnot(isTRUE(all.equal(a,b,tolerance=3e-6,check.attributes=FALSE)))
fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))
fail(test_gru_model(retrieval_heads=-1));fail(test_gru_model(retrieval_heads=3,retrieval_size=8))
fail(test_gru_model(retrieval_window=0));fail(test_gru_model(retrieval_dropout=1))
fail(test_gru_model(retrieval_decay='clock'));fail(test_gru_model(retrieval_decay_init=0))
stopifnot(test_gru_model()$settings$retrieval_heads==0L)
if(requireNamespace('torch',quietly=TRUE) && torch::torch_is_installed()) {
  ns <- asNamespace('LKT');tensor <- function(x)torch::torch_tensor(x,dtype=torch::torch_float64())
  settings <- test_gru_model(hidden_size=3L,retrieval_heads=2L,retrieval_size=4L,
    retrieval_window=2L,retrieval_share_qk=FALSE,retrieval_bias=FALSE,dtype='float64')$settings
  events <- rbind(c(1,0),c(0,1),c(1,1),c(2,0))
  states <- rbind(c(1,0,1),c(0,2,1),c(2,1,0),c(1,3,2))
  weights <- list(query.weight=rbind(c(1,.2),c(.1,1),c(-.3,.5),c(.7,-.2)),
    key.weight=rbind(c(.4,1),c(1,-.1),c(.5,.3),c(-.2,.8)),
    value.weight=rbind(c(1,0,.2),c(0,1,.3),c(.4,.2,1),c(-.1,.7,.5)),
    output.weight=rbind(c(1,.2,.3,0),c(0,.4,1,.2),c(.1,0,.3,1)))
  # An ordinary R matrix calculation checks head splitting, scaling, masking,
  # normalization and value readout independently from the Torch implementation.
  for(decay in c('none','exponential','akt')) {
    s <- settings;s$retrieval_decay <- decay
    net <- get('lkt_gru_retrieval_network',ns)(2L,s)$to(dtype=torch::torch_float64())
    w <- weights
    if(decay!='none')w$decay <- log(expm1(c(.2,.5)))
    get('lkt_gru_load_weights',ns)(net,w,s);net$eval()
    result <- net(tensor(array(events,c(1,4,2))),tensor(array(states,c(1,4,3))))
    expected <- matrix(0,4,3);expected_attention <- array(0,c(1,2,4,4))
    q <- events%*%t(weights$query.weight);k <- events%*%t(weights$key.weight);v <- states%*%t(weights$value.weight)
    for(t in 2:4) {
      previous <- seq.int(max(1,t-2),t-1);read <- numeric(4)
      for(head in 1:2) {
        columns <- ((head-1)*2+1):(head*2)
        score <- as.numeric(k[previous,columns,drop=FALSE]%*%q[t,columns])/sqrt(2)
        distance <- t-previous;rate <- c(.2,.5)[head]
        if(decay=='exponential')score <- score-rate*distance
        if(decay=='akt') {
          preliminary <- exp(score-max(score));preliminary <- preliminary/sum(preliminary)
          distance <- sqrt(pmax(0,(sum(preliminary)-cumsum(preliminary))*distance))
          score <- score*exp(-rate*distance)
        }
        a <- exp(score-max(score));a <- a/sum(a)
        expected_attention[1,head,t,previous] <- a
        read[columns] <- as.numeric(a%*%v[previous,columns,drop=FALSE])
      }
      expected[t,] <- as.numeric(read%*%t(weights$output.weight))
    }
    close(torch::as_array(result$read)[1,,],expected)
    close(torch::as_array(result$weights),expected_attention)
    a <- net(tensor(array(events[1:2,],c(1,2,2))),tensor(array(states[1:2,],c(1,2,3))))
    b <- net(tensor(array(events[3:4,],c(1,2,2))),tensor(array(states[3:4,],c(1,2,3))),a$context)
    close(torch::as_array(b$read)[1,,],expected[3:4,])
    stopifnot(b$context$retrieval_keys$size(2)==2L)
  }
  # Empty memory must be exactly zero even when output projections have biases.
  # Unlimited memory is an explicit option, not a consequence of chunk length.
  s <- settings;s$retrieval_window <- NULL;s$retrieval_bias <- TRUE
  unlimited <- get('lkt_gru_retrieval_network',ns)(2L,s)$to(dtype=torch::torch_float64())
  unlimited$eval()
  u <- unlimited(tensor(array(events,c(1,4,2))),tensor(array(states,c(1,4,3))))
  close(torch::as_array(u$read)[1,1,],rep(0,3))
  stopifnot(u$context$retrieval_keys$size(2)==4L)
  fail(unlimited(tensor(array(events,c(1,4,2))),tensor(array(states,c(1,4,3))),list()))
  set.seed(914)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:8),each=12),
    KC..Default.=rep(c('a','b','c'),32),CF..Time.=rep(seq(0,1100,100),8),
    CF..ansbin.=rbinom(96,1,.6),fold=rep(c(1,1,1,1,1,2,3,3),each=12))
  # All inference modes, both memory fusions, raw and encoded inputs must retain
  # retrieval context across chunks, student switches, pending trials and save/load.
  for(mode in c('none','deterministic','gaussian'))for(fusion in c('replace','concat')) {
    controls <- list(hidden_size=6L,num_layers=3L,head_hidden_sizes=8L,embedding_dims=4L,
      time_col='CF..Time.',order_col='CF..Time.',feature_encoder=if(fusion=='replace')'mlp' else 'none',
      encoder_hidden_sizes=7L,encoder_output_size=4L,inference_mode=mode,latent_size=4L,
      temporal_kl_beta=if(mode=='gaussian').001 else 0,retrieval_heads=if(fusion=='replace')1L else 2L,
      retrieval_size=8L,retrieval_window=5L,retrieval_fusion=fusion,
      retrieval_decay=if(mode=='none')'none' else if(mode=='deterministic')'exponential' else 'akt',
      validation_folds=2,epochs=2L,batch_size=3L,sequence_length=4L,seed=19L)
    fit <- LKT(d,usefolds=1,components='KC..Default.',features='intercept',
      model=do.call(test_gru_model,controls),verbose=FALSE)
    close(predict_lkt(fit,d)$predictions,fit$prediction)
    rt <- LKTGRURuntime(fit)
    first <- LKTGRUStep(fit,LKTGRUState(fit),d[1:5,],rt)
    second <- LKTGRUStep(fit,first$state,d[6:12,],rt)
    close(c(first$predictions,second$predictions),fit$prediction[1:12])
    stopifnot(dim(second$state$inference_context[[1]]$retrieval_values)[2]==5L)
    # Candidate scoring branches from the same memory; it commits no retrieval
    # entries, including when another student's row is processed between two steps.
    before <- serialize(first$state,NULL)
    close(LKTGRUScore(fit,first$state,d[c(6,6),],rt),rep(fit$prediction[6],2))
    stopifnot(identical(before,serialize(first$state,NULL)))
    switched <- LKTGRUStep(fit,first$state,d[13:15,],rt)
    close(LKTGRUStep(fit,switched$state,d[6:12,],rt)$predictions,second$predictions)
    pending <- d[6,,drop=FALSE];pending$CF..ansbin. <- NA_real_
    peek <- LKTGRUStep(fit,first$state,pending,rt)
    close(peek$predictions,fit$prediction[6])
    stopifnot(identical(peek$state$inference_context,first$state$inference_context))
    close(LKTGRUStep(fit,peek$state,d[6:12,],rt)$predictions,second$predictions)
    file <- tempfile(fileext='.rds');saveRDS(list(fit=fit,state=first$state),file)
    restored <- readRDS(file);unlink(file)
    close(LKTGRUStep(restored$fit,restored$state,d[6:12,])$predictions,second$predictions)
    changed <- d;changed$CF..ansbin.[6:12] <- 1-changed$CF..ansbin.[6:12]
    close(predict_lkt(fit,changed)$predictions[1:6],fit$prediction[1:6])
    # Every retrieval projection receives a prediction-loss gradient.
    net <- rt$network;net$train();net$zero_grad()
    encoded <- get('lkt_gru_encode',ns)(d[1:12,],fit$model$preprocessing,fit$model$recipe,fit$gru_settings)
    batch <- get('lkt_gru_batch',ns)(encoded,d[1:12,],list(1:12),list(1:12),fit$gru_settings)
    net(batch$numeric,batch$categorical,batch$answers)$logits$sum()$backward()
    parameters <- net$named_parameters()
    for(prefix in c('retrieval.query','retrieval.value','retrieval.output')) {
      selected <- parameters[startsWith(names(parameters),prefix)]
      stopifnot(length(selected)>0L,any(vapply(selected,function(p)!is.null(p$grad) && p$grad$abs()$sum()$item()>0,logical(1))))
    }
    # Altering held-out labels must never alter trained weights or retrieval banks
    # used in fitting. Validation still selects the restored epoch as before.
    changed <- d;changed$CF..ansbin.[d$fold==3] <- 1-changed$CF..ansbin.[d$fold==3]
    other <- LKT(changed,usefolds=1,components='KC..Default.',features='intercept',
      model=do.call(test_gru_model,controls),verbose=FALSE)
    stopifnot(identical(fit$model$weights,other$model$weights))
  }
  short <- d[-c(10,12,24,36,48,60,72,82,83,84,95,96),]
  padded <- LKT(short,usefolds=1,components='KC..Default.',features='intercept',verbose=FALSE,
    model=test_gru_model(hidden_size=6L,retrieval_heads=1L,retrieval_size=8L,
      retrieval_window=NULL,epochs=1L,validation_folds=2,batch_size=3L,sequence_length=4L))
  close(predict_lkt(padded,short)$predictions,padded$prediction)
  cat('PASS independent attention equations, all decays, causal windows, one/multiple heads, inference modes, gradients, held-out isolation and continuation\n')
} else if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required.')
