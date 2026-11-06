library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=base::identity),'error'))
close <- function(x,y)stopifnot(isTRUE(all.equal(x,y,tolerance=3e-6,check.attributes=FALSE)))
fail(test_gru_model(retrieval_filter_cols='item'))
fail(test_gru_model(retrieval_heads=1L,retrieval_filter_cols=c('item','item')))
fail(test_gru_model(retrieval_heads=1L,retrieval_filter_cols='CF..ansbin.'))
if(requireNamespace('torch',quietly=TRUE) && torch::torch_is_installed()) {
  ns <- asNamespace('LKT');tensor <- function(x)torch::torch_tensor(x,dtype=torch::torch_float64())
  ids <- cbind(item=c('a','b','a','c','d'),skill=c('x','y','y','z','x'))
  states <- rbind(c(1,2),c(3,1),c(2,5),c(7,3),c(4,2))
  events <- array(0,c(1,5,2))
  # Uniform attention makes the expected read a plain average of exactly the
  # eligible states. These crossed identities distinguish OR from AND matching.
  for(columns in list('item','skill',c('item','skill')))for(window in list(3L,NULL)) {
    s <- test_gru_model(hidden_size=2L,retrieval_heads=1L,retrieval_size=2L,
      retrieval_window=window,retrieval_filter_cols=columns,dtype='float64')$settings
    net <- get('lkt_gru_retrieval_network',ns)(2L,s)$to(dtype=torch::torch_float64())
    w <- list(query.weight=matrix(0,2,2),query.bias=c(0,0),value.weight=diag(2),
      value.bias=c(0,0),output.weight=diag(2),output.bias=c(.3,-.2))
    get('lkt_gru_load_weights',ns)(net,w,s);net$eval()
    filter_ids <- array(ids[,columns,drop=FALSE],c(1,5,length(columns)))
    result <- net(tensor(events),tensor(array(states,c(1,5,2))),identifiers=filter_ids)
    expected <- matrix(0,5,2);attention <- array(0,c(1,1,5,5))
    for(t in 1:5) {
      previous <- which(seq_len(5)<t)
      if(!is.null(window))previous <- previous[t-previous<=window]
      matching <- vapply(previous,function(j)any(ids[t,columns]==ids[j,columns]),logical(1))
      eligible <- previous[matching]
      if(length(eligible)) {
        expected[t,] <- colMeans(states[eligible,,drop=FALSE])+w$output.bias
        attention[1,1,t,eligible] <- 1/length(eligible)
      }
    }
    close(torch::as_array(result$read)[1,,],expected)
    close(torch::as_array(result$weights),attention)
    a <- net(tensor(events[,1:2,,drop=FALSE]),tensor(array(states[1:2,],c(1,2,2))),identifiers=filter_ids[,1:2,,drop=FALSE])
    b <- net(tensor(events[,3:5,,drop=FALSE]),tensor(array(states[3:5,],c(1,3,2))),a$context,filter_ids[,3:5,,drop=FALSE])
    close(torch::as_array(b$read)[1,,],expected[3:5,])
    stopifnot(identical(b$context$retrieval_ids,filter_ids[,tail(1:5,if(is.null(window))5L else window),,drop=FALSE]))
    fail(net(tensor(events),tensor(array(states,c(1,5,2)))))
  }
  # Different batch members may share item names but never share memory values.
  s$retrieval_window <- NULL
  net <- get('lkt_gru_retrieval_network',ns)(2L,s)$to(dtype=torch::torch_float64())
  get('lkt_gru_load_weights',ns)(net,w,s);net$eval()
  both_ids <- array('',c(2,5,2));both_ids[1,,] <- ids;both_ids[2,,] <- ids
  both_states <- array(0,c(2,5,2));both_states[1,,] <- states;both_states[2,,] <- states*10
  result <- net(tensor(array(0,c(2,5,2))),tensor(both_states),identifiers=both_ids)
  close(torch::as_array(result$read)[1,3,],colMeans(states[1:2,])+w$output.bias)
  close(torch::as_array(result$read)[2,3,],colMeans(states[1:2,])*10+w$output.bias)
  # With nonuniform scores, eligible memories must still train the projections.
  random <- get('lkt_gru_retrieval_network',ns)(2L,s)$to(dtype=torch::torch_float64())
  random$zero_grad()
  random(tensor(array(seq_len(10)/10,c(1,5,2))),tensor(array(states,c(1,5,2))),identifiers=filter_ids)$read$sum()$backward()
  for(name in c('query.weight','value.weight','output.weight'))stopifnot(random$named_parameters()[[name]]$grad$abs()$sum()$item()>0)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:8),each=12),
    item=rep(c('a','b','c','d'),24),skill=rep(c('x','y','x','z'),24),
    KC..Default.=rep(c('k1','k2'),48),CF..Time.=rep(seq(0,1100,100),8),
    CF..ansbin.=rep(c(0,1,1,0,1,0),16),fold=rep(c(1,1,1,1,1,2,3,3),each=12))
  d <- d[-c(11,23,34,47,57,69,82,94),]
  # Exercise the public fit/continuation paths with shrinking minibatches, all
  # inference modes and both ordinary and expanded recurrent input wiring.
  for(mode in c('none','deterministic','gaussian'))for(all_layers in c(FALSE,TRUE)) {
    s <- test_gru_model(hidden_size=6L,num_layers=3L,categorical_cols='item',embedding_dims=4L,
      unknown_levels='zero',time_col='CF..Time.',order_col='CF..Time.',feature_encoder=if(all_layers)'mlp' else 'none',
      encoder_hidden_sizes=7L,encoder_output_size=4L,inference_mode=mode,latent_size=4L,
      temporal_kl_beta=if(mode=='gaussian').001 else 0,retrieval_heads=1L,retrieval_size=8L,
      retrieval_filter_cols=c('item','skill'),retrieval_window=5L,input_to_all_layers=all_layers,
      additional_input_init=if(all_layers)'zero' else 'random',
      epochs=2L,batch_size=3L,sequence_length=4L,validation_folds=2,seed=31L)$settings
    fit <- LKT(d,usefolds=1,components='KC..Default.',features='intercept',model=do.call(test_gru_model,s),verbose=FALSE)
    close(predict_lkt(fit,d)$predictions,fit$prediction)
    rt <- LKTGRURuntime(fit)
    first <- LKTGRUStep(fit,LKTGRUState(fit),d[1:5,],rt)
    second <- LKTGRUStep(fit,first$state,d[6:11,],rt)
    close(c(first$predictions,second$predictions),fit$prediction[1:11])
    stopifnot(is.character(second$state$inference_context[[1]]$retrieval_ids),
      dim(second$state$inference_context[[1]]$retrieval_ids)[2]==5L)
    before <- serialize(first$state,NULL)
    close(LKTGRUScore(fit,first$state,d[c(6,6),],rt),rep(fit$prediction[6],2))
    stopifnot(identical(before,serialize(first$state,NULL)))
    pending <- d[6,,drop=FALSE];pending$CF..ansbin. <- NA_real_
    peek <- LKTGRUStep(fit,first$state,pending,rt)
    stopifnot(identical(peek$state$inference_context,first$state$inference_context))
    close(LKTGRUStep(fit,peek$state,d[6:11,],rt)$predictions,second$predictions)
    switched <- LKTGRUStep(fit,first$state,d[12:14,],rt)
    close(LKTGRUStep(fit,switched$state,d[6:11,],rt)$predictions,second$predictions)
    file <- tempfile(fileext='.rds');saveRDS(list(fit=fit,state=first$state),file)
    restored <- readRDS(file);unlink(file)
    close(LKTGRUStep(restored$fit,restored$state,d[6:11,])$predictions,second$predictions)
    changed <- d;changed$CF..ansbin.[6:11] <- 1-changed$CF..ansbin.[6:11]
    close(predict_lkt(fit,changed)$predictions[1:6],fit$prediction[1:6])
    bad <- d;bad$skill[1] <- NA_character_;fail(predict_lkt(fit,bad))
    bad <- d;bad$skill <- NULL;fail(predict_lkt(fit,bad))
    novel <- d[1:3,];novel$Anon.Student.Id <- 'new-student'
    novel$item <- c('new|1','new|2','new|1');novel$skill <- c('new:x','new:y','new:z')
    encoded <- get('lkt_gru_encode',ns)(novel,fit$model$preprocessing,fit$model$recipe,s)
    stopifnot(all(encoded$categorical==1L))
    b <- get('lkt_gru_batch',ns)(encoded,novel,list(1:3),list(1:3),s)
    out <- rt$network(b$numeric,b$categorical,b$answers,retrieval_ids=b$retrieval_ids)
    attention <- torch::as_array(out$retrieval_attention)
    stopifnot(attention[1,1,3,1]==1,attention[1,1,3,2]==0)
  }
  cat('PASS exact OR identities, independent masked reads, empty matches, causal windows, student isolation, unseen categories, gradients, all inference modes, expanded inputs and portable candidate/continuation state\n')
} else if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required.')
