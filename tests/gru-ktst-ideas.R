library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=base::identity),'error'))
close <- function(x,y)stopifnot(isTRUE(all.equal(x,y,tolerance=3e-6,check.attributes=FALSE)))
fail(test_gru_model(retrieval_cue_cols='KC..Default.'))
fail(test_gru_model(retrieval_heads=1L,retrieval_cue_cols='missing'))
fail(test_gru_model(zero_init_embedding_cols='missing'))
fail(test_gru_model(categorical_encoding='one_hot',zero_init_embedding_cols='KC..Default.'))
fail(test_gru_model(retrieval_decay_lr_multiplier=0))
fail(test_gru_model(retrieval_heads=1L,retrieval_decay_lr_multiplier=20))
if(requireNamespace('torch',quietly=TRUE) && torch::torch_is_installed()) {
  ns <- asNamespace('LKT');network <- get('lkt_gru_network',ns);weights <- get('lkt_gru_weights',ns)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:8),each=12),
    item=rep(c('a','b','c','d'),24),skill=rep(c('x','y','x','z'),24),
    difficulty=rep(c(.2,.7,.4,.1),24),CF..Time.=rep(seq(0,1100,100),8),
    CF..ansbin.=rep(c(0,1,1,0,1,0),16),fold=rep(c(1,1,1,1,1,2,3,3),each=12))
  d <- d[-c(11,23,34,47,57,69,82,94),]
  basic <- test_gru_model(categorical_cols=c('item','skill'),embedding_dims=c(item=4L,skill=3L),
    numeric_cols='difficulty',time_col='CF..Time.',order_col='CF..Time.',hidden_size=6L,num_layers=3L,
    head_hidden_sizes=8L,retrieval_heads=1L,retrieval_size=8L,validation_folds=2,
    epochs=2L,batch_size=3L,sequence_length=4L,seed=31L)$settings
  recipe <- list();prep <- get('lkt_gru_preprocess',ns)(d,d$fold==1,recipe,basic)
  cleanup <- get('lkt_gru_runtime',ns)(basic)
  # Zeroing selected tables must preserve every other random weight and the RNG
  # endpoint. The tables remain ordinary independent trainable item parameters.
  torch::torch_manual_seed(42);ordinary <- network(prep,basic)
  endpoint <- torch::torch_get_rng_state();zero_s <- basic;zero_s$zero_init_embedding_cols <- 'item'
  torch::torch_manual_seed(42);zero_net <- network(prep,zero_s)
  expected <- weights(ordinary);expected[['embeddings.0.weight']][] <- 0
  stopifnot(identical(expected,weights(zero_net)),
    isTRUE(torch::torch_equal(endpoint,torch::torch_get_rng_state())),
    any(weights(zero_net)[['embeddings.1.weight']]!=0))
  # A linear objective gives a known unit gradient for each parameter. It checks
  # actual updates, not just the recorded rates, for all supported optimizers.
  for(method in c('sgd','adam','adamw')) {
    s <- basic;s$optimizer <- method;s$optimizer_options <- list();s$weight_decay <- 0
    s$retrieval_decay <- 'exponential';s$retrieval_decay_init <- 1/256;s$retrieval_decay_lr_multiplier <- 20
    net <- network(prep,s);opt <- get('lkt_gru_optimizer',ns)(net,s)
    stopifnot(length(opt$param_groups)==2L,opt$param_groups[[1]]$lr==.001,opt$param_groups[[2]]$lr==.02)
    params <- net$named_parameters();before <- lapply(params,torch::as_array)
    opt$zero_grad();Reduce(`+`,lapply(params,function(p)p$sum()))$backward();opt$step()
    stopifnot(max(abs(torch::as_array(params[['retrieval.decay']])-before[['retrieval.decay']]+.02))<3e-6)
    name <- setdiff(names(params),'retrieval.decay')[1L]
    stopifnot(max(abs(torch::as_array(params[[name]])-before[[name]]+.001))<3e-6)
    scheduler <- torch::lr_reduce_on_plateau(opt,factor=.5,patience=0,threshold=100,threshold_mode='abs',min_lr=0)
    scheduler$step(1);scheduler$step(1)
    stopifnot(opt$param_groups[[1]]$lr==.0005,opt$param_groups[[2]]$lr==.01)
  }
  cleanup()
  # The same controls must work with raw/encoded features, all inference modes,
  # multiple heads, expanded recurrent inputs and the public continuation API.
  for(mode in c('none','deterministic','gaussian'))for(learned in c(FALSE,TRUE)) {
    s <- basic;s$feature_encoder <- if(learned)'mlp' else 'none'
    s$encoder_hidden_sizes <- 7L;s$encoder_output_size <- 4L
    s$inference_mode <- mode;s$latent_size <- 4L;s$inference_hidden_sizes <- 7L
    s$temporal_kl_beta <- if(mode=='gaussian').001 else 0
    s$input_to_all_layers <- learned;s$additional_input_init <- if(learned)'zero' else 'random'
    s$retrieval_heads <- if(learned)2L else 1L;s$retrieval_filter_cols <- c('item','skill')
    s$retrieval_cue_cols <- c('item','skill');s$zero_init_embedding_cols <- 'item'
    s$retrieval_decay <- 'exponential';s$retrieval_decay_init <- 1/256;s$retrieval_decay_lr_multiplier <- 20
    s$retrieval_window <- 5L
    fit <- LKT(d,usefolds=1,components='skill',features='intercept',model=do.call(test_gru_model,s),verbose=FALSE)
    close(predict_lkt(fit,d)$predictions,fit$prediction)
    stopifnot(any(fit$model$weights[['embeddings.0.weight']]!=0),
      any(apply(fit$model$weights[['embeddings.0.weight']][-1,,drop=FALSE],2,stats::sd)>0),
      all(is.finite(fit$training_history$retrieval_decay_learning_rate)),
      all(fit$training_history$retrieval_decay_learning_rate==20*fit$training_history$learning_rate))
    rt <- LKTGRURuntime(fit);net <- rt$network
    stopifnot(net$retrieval$query$weight$size(2)==7L)
    encoded <- get('lkt_gru_encode',ns)(d[1:10,],fit$model$preprocessing,fit$model$recipe,s)
    first_batch <- get('lkt_gru_batch',ns)(encoded,d[1:10,],list(1:10),list(1:8),s)
    past <- net(first_batch$numeric,first_batch$categorical,first_batch$answers,retrieval_ids=first_batch$retrieval_ids)
    expected_keys <- torch::torch_cat(lapply(1:2,function(j)
      net$embeddings[[j]](first_batch$categorical[,,j,drop=FALSE]$squeeze(3))),dim=3)
    close(torch::as_array(past$context$retrieval_keys),torch::as_array(expected_keys[,4:8,,drop=FALSE]))
    current <- get('lkt_gru_batch',ns)(encoded,d[1:10,],list(1:10),list(9L),s)
    a <- net(current$numeric,current$categorical,current$answers,past$hidden,past$context,current$retrieval_ids)
    b <- net(current$numeric+10,current$categorical,current$answers,past$hidden+5,past$context,current$retrieval_ids)
    close(torch::as_array(a$retrieval_attention),torch::as_array(b$retrieval_attention))
    close(torch::as_array(a$context$retrieval_keys),torch::as_array(b$context$retrieval_keys))
    first <- LKTGRUStep(fit,LKTGRUState(fit),d[1:5,],rt)
    second <- LKTGRUStep(fit,first$state,d[6:11,],rt)
    close(c(first$predictions,second$predictions),fit$prediction[1:11])
    stopifnot(dim(second$state$inference_context[[1]]$retrieval_keys)[3]==7L,
      dim(second$state$inference_context[[1]]$retrieval_ids)[2]==5L)
    before <- serialize(first$state,NULL)
    close(LKTGRUScore(fit,first$state,d[c(6,6),],rt),rep(fit$prediction[6],2))
    stopifnot(identical(before,serialize(first$state,NULL)))
    pending <- d[6,,drop=FALSE];pending$CF..ansbin. <- NA_real_
    peek <- LKTGRUStep(fit,first$state,pending,rt)
    stopifnot(identical(peek$state$inference_context,first$state$inference_context))
    file <- tempfile(fileext='.rds');saveRDS(list(fit=fit,state=first$state),file)
    restored <- readRDS(file);unlink(file)
    close(LKTGRUStep(restored$fit,restored$state,d[6:11,])$predictions,second$predictions)
    changed <- d;changed$CF..ansbin.[6:11] <- 1-changed$CF..ansbin.[6:11]
    close(predict_lkt(fit,changed)$predictions[1:6],fit$prediction[1:6])
    if(mode=='none' && !learned) {
      changed <- d;changed$CF..ansbin.[d$fold==3] <- 1-changed$CF..ansbin.[d$fold==3]
      other <- LKT(changed,usefolds=1,components='skill',features='intercept',model=do.call(test_gru_model,s),verbose=FALSE)
      stopifnot(identical(other$model$weights,fit$model$weights))
    }
  }
  cat('PASS question-only cues, zero-table learning/RNG equivalence, decay optimizer updates and scheduling, all inference modes, causal filtered retrieval and portable continuation\n')
} else if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required.')
