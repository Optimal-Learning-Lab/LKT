library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))
fail(test_gru_model(additional_input_init='invalid'))
fail(test_gru_model(initial_model_file='model.rds'))
fail(test_gru_model(input_to_all_layers=TRUE,initial_model_file='model.rds'))
if(requireNamespace('torch',quietly=TRUE) && torch::torch_is_installed()) {
  ns <- asNamespace('LKT');weights <- get('lkt_gru_weights',ns)
  expand <- get('lkt_gru_expand_input_weights',ns)
  network <- get('lkt_gru_network',ns)
  close <- function(x,y,tolerance=3e-6)stopifnot(isTRUE(all.equal(x,y,tolerance=tolerance,check.attributes=FALSE)))
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:8),each=12),
    KC..Default.=rep(c('a','b','c'),32),CF..Time.=rep(seq(0,1100,100),8),
    CF..ansbin.=rep(c(0,1,1,0,1,0),16),fold=rep(c(1,1,1,1,1,2,3,3),each=12))
  d <- d[-c(11,23,34,47,57,69,82,94),]
  for(mode in c('none','deterministic','gaussian'))for(heads in 0:1) {
    s <- test_gru_model(hidden_size=6L,num_layers=3L,categorical_cols='KC..Default.',embedding_dims=4L,
      time_col='CF..Time.',order_col='CF..Time.',feature_encoder=if(heads)'mlp' else 'none',
      encoder_hidden_sizes=7L,encoder_output_size=4L,inference_mode=mode,latent_size=4L,
      inference_hidden_sizes=7L,temporal_kl_beta=if(mode=='gaussian').001 else 0,
      retrieval_heads=heads,retrieval_size=8L,retrieval_window=5L,
      epochs=2L,batch_size=3L,sequence_length=4L,validation_folds=2,seed=31L)$settings
    base <- LKT(d,usefolds=1,components='KC..Default.',features='intercept',model=do.call(test_gru_model,s),verbose=FALSE)
    # A random ordinary network and a zero-expanded network must have the same
    # shared weights, predictions and RNG endpoint, including their later heads.
    cleanup <- get('lkt_gru_runtime',ns)(s)
    torch::torch_manual_seed(42)
    ordinary <- network(base$model$preprocessing,s);ordinary$eval()
    endpoint <- torch::torch_get_rng_state()
    z <- s;z$input_to_all_layers <- TRUE;z$additional_input_init <- 'zero'
    torch::torch_manual_seed(42)
    zero <- network(base$model$preprocessing,z);zero$eval()
    stopifnot(isTRUE(torch::torch_equal(endpoint,torch::torch_get_rng_state())))
    expected <- expand(weights(ordinary),weights(zero),'gru.')
    stopifnot(identical(expected,weights(zero)),
      any(weights(zero)[['gru.layers.0.weight_ih_l1']]!=0))
    for(layer in 1:2)stopifnot(all(weights(zero)[[paste0('gru.layers.',layer,'.weight_ih_l1')]][,7:ncol(weights(zero)[[paste0('gru.layers.',layer,'.weight_ih_l1')]])]==0))
    encoded <- get('lkt_gru_encode',ns)(d[1:11,],base$model$preprocessing,base$model$recipe,s)
    batch <- get('lkt_gru_batch',ns)(encoded,d[1:11,],list(1:11),list(1:11),s)
    a <- ordinary(batch$numeric,batch$categorical,batch$answers)
    b <- zero(batch$numeric,batch$categorical,batch$answers)
    close(torch::as_array(a$logits),torch::as_array(b$logits))
    # Zero added blocks must remain trainable; zeroing every layer is not required.
    zero$train();zero$zero_grad();zero(batch$numeric,batch$categorical,batch$answers)$logits$sum()$backward()
    for(layer in 1:2) {
      param <- zero$named_parameters()[[paste0('gru.layers.',layer,'.weight_ih_l1')]]
      stopifnot(param$grad[,seq.int(7L,param$size(2)),drop=FALSE]$abs()$sum()$item()>0)
    }
    cleanup()
    # Rebuilding identical training summaries can change their last floating
    # point bit across platforms. That must pass, while a substantive change fails.
    file <- tempfile(fileext='.rds');source_fit <- base
    source_fit$model$preprocessing$center[1] <- source_fit$model$preprocessing$center[1]+
      .Machine$double.eps*max(1,abs(source_fit$model$preprocessing$center[1]))
    saveRDS(source_fit,file)
    z$initial_model_file <- file;z$epochs <- 1L;z$patience <- 1L
    z$learning_rate <- 1e-30;z$scheduler_options$min_lr <- 0
    warm <- LKT(d,usefolds=1,components='KC..Default.',features='intercept',model=do.call(test_gru_model,z),verbose=FALSE)
    stopifnot(warm$selected_epoch==0L,warm$trained_epochs==1L,
      identical(warm$training_history$epoch,0:1),!is.null(warm$initialization))
    close(warm$initialization$predictions,base$prediction)
    close(warm$prediction,base$prediction)
    if(mode=='none' && heads==0L) {
      bad_source <- base;bad_source$model$preprocessing$center[1] <- bad_source$model$preprocessing$center[1]+.001
      saveRDS(bad_source,file)
      fail(LKT(d,usefolds=1,components='KC..Default.',features='intercept',model=do.call(test_gru_model,z),verbose=FALSE))
      saveRDS(base,file)
    }
    # A mismatched fold contract is rejected instead of accepting a leaky source.
    bad <- d;bad$fold[bad$Anon.Student.Id=='s5'] <- 2
    fail(LKT(bad,usefolds=1,components='KC..Default.',features='intercept',model=do.call(test_gru_model,z),verbose=FALSE))
    unlink(file)
    # Loading a trained model needs only its saved weights, not the original file.
    close(predict_lkt(warm,d)$predictions,warm$prediction)
    first <- LKTGRUStep(warm,LKTGRUState(warm),d[1:5,])
    rest <- LKTGRUStep(warm,first$state,d[6:nrow(d),])
    close(c(first$predictions,rest$predictions),warm$prediction)
    before <- serialize(first$state,NULL)
    close(LKTGRUScore(warm,first$state,d[c(6,6),]),rep(warm$prediction[6],2))
    stopifnot(identical(before,serialize(first$state,NULL)))
  }
  stopifnot(get('lkt_gru_metrics',ns)(c(0,1),c(.9,.1))[['AUC']]==0)
  cat('PASS zero-only added weights, ordinary initialization/RNG equivalence, gradients, trained expansion, epoch-zero selection, source contracts and portable continuation\n')
} else if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required.')
