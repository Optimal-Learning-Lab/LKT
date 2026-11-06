library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=base::identity),'error'))
close <- function(a,b)stopifnot(max(abs(as.numeric(a)-as.numeric(b)))<3e-6)
fail(test_gru_model(joint_encoder_output_size=5L))
fail(test_gru_model(feature_encoder='mlp',joint_encoder_output_size=0))
fail(test_gru_model(feature_encoder='mlp',joint_encoder_output_size=4L,joint_encoder_hidden_sizes=c(3L,0L)))
fail(test_gru_model(feature_encoder='mlp',joint_encoder_output_size=4L,inference_mode='gaussian'))
fail(test_gru_model(feature_encoder='mlp',joint_encoder_output_size=4L,retrieval_heads=1L,retrieval_cue_cols='KC..Default.'))
if(requireNamespace('torch',quietly=TRUE) && torch::torch_is_installed()) {
  ns <- asNamespace('LKT');network <- get('lkt_gru_network',ns)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:8),each=12),
    item=rep(c('a','b','c','d'),24),skill=rep(c('x','y','x','z'),24),
    difficulty=rep(c(.2,.7,.4,.1),24),CF..Time.=rep(seq(0,1100,100),8),
    CF..ansbin.=rep(c(0,1,1,0,1,0),16),fold=rep(c(1,1,1,1,1,2,3,3),each=12))
  base <- test_gru_model(categorical_cols=c('item','skill'),embedding_dims=4L,
    numeric_cols='difficulty',time_col='CF..Time.',order_col='CF..Time.',
    feature_encoder='mlp',encoder_hidden_sizes=7L,encoder_output_size=4L,
    hidden_size=6L,num_layers=3L,head_hidden_sizes=8L,
    retrieval_heads=1L,retrieval_size=8L,retrieval_window=5L,
    retrieval_filter_cols=c('item','skill'),zero_init_embedding_cols='item',
    validation_folds=2,epochs=2L,batch_size=3L,sequence_length=4L,seed=31L)$settings
  prep <- get('lkt_gru_preprocess',ns)(d,d$fold==1,list(),base)
  torch::torch_manual_seed(10);ordinary <- network(prep,base)
  endpoint <- torch::torch_get_rng_state()
  torch::torch_manual_seed(10);disabled <- network(prep,do.call(test_gru_model,base)$settings)
  stopifnot(identical(get('lkt_gru_weights',ns)(ordinary),get('lkt_gru_weights',ns)(disabled)),
    isTRUE(torch::torch_equal(endpoint,torch::torch_get_rng_state())),
    !any(startsWith(names(disabled$named_parameters()),'joint_encoder')))
  for(hidden in list(7L,integer())) {
    s <- base;s$joint_encoder_output_size <- 9L;s$joint_encoder_hidden_sizes <- hidden
    stopifnot(isTRUE(all.equal(get('lkt_gru_gui_decode',ns)(get('lkt_gru_gui_encode',ns)(s)),s)))
    fit <- LKT(d,usefolds=1,components='skill',features='intercept',model=do.call(test_gru_model,s),verbose=FALSE)
    initial_weights <- serialize(fit$model$weights,NULL)
    rt <- LKTGRURuntime(fit);net <- rt$network
    e <- get('lkt_gru_encode',ns)(d[1:12,],fit$model$preprocessing,fit$model$recipe,s)
    b <- get('lkt_gru_batch',ns)(e,d[1:12,],list(1:12),list(1:12),s)
    layout <- get('lkt_gru_input_layout',ns)(fit$model$preprocessing,s)
    torch::with_no_grad({
      t <- net$time_encoder(b$numeric[,,layout$time,drop=FALSE])
      h <- net$auxiliary_encoder(b$numeric[,,layout$history,drop=FALSE])
      i <- net$embeddings[[1L]](b$categorical[,,1L,drop=FALSE]$squeeze(3))
      k <- net$embeddings[[2L]](b$categorical[,,2L,drop=FALSE]$squeeze(3))
      joined <- torch::torch_cat(list(t,h,i,k),dim=3)
      expected <- net$joint_encoder(joined)
      x <- net$encode(b$numeric,b$categorical)
      close(torch::as_array(expected),torch::as_array(x))
    })
    # All three destinations receive the shared output. Only the separately encoded
    # answer is appended for recurrent updates; the raw four-block input has no bypass.
    stopifnot(net$gru$parameters[[1L]]$size(2)==13L,
      net$head[[1L]]$weight$size(2)==15L,net$retrieval$query$weight$size(2)==15L)
    net$train();net$zero_grad()
    net(b$numeric,b$categorical,b$answers,retrieval_ids=b$retrieval_ids)$logits$sum()$backward()
    params <- net$named_parameters()
    for(prefix in c('joint_encoder','time_encoder','auxiliary_encoder','embeddings.0','embeddings.1')) {
      selected <- params[startsWith(names(params),prefix)]
      stopifnot(any(vapply(selected,function(p)!is.null(p$grad) && p$grad$abs()$sum()$item()>0,logical(1))))
    }
    close(predict_lkt(fit,d)$predictions,fit$prediction)
    a <- LKTGRUStep(fit,LKTGRUState(fit),d[1:5,]);saved <- serialize(a$state,NULL)
    pending <- d[6,,drop=FALSE];pending$CF..ansbin. <- NA_real_
    close(LKTGRUScore(fit,a$state,pending),fit$prediction[6]);stopifnot(identical(saved,serialize(a$state,NULL)))
    path <- tempfile(fileext='.rds');saveRDS(list(fit=fit,state=a$state),path)
    restored <- readRDS(path);unlink(path)
    close(LKTGRUStep(restored$fit,restored$state,d[6:12,])$predictions,fit$prediction[6:12])
    changed <- d;changed$CF..ansbin.[6:12] <- 1-changed$CF..ansbin.[6:12]
    close(predict_lkt(fit,changed)$predictions[1:6],fit$prediction[1:6])
    changed <- d;changed$CF..ansbin.[d$fold==3] <- 1-changed$CF..ansbin.[d$fold==3]
    other <- LKT(changed,usefolds=1,components='skill',features='intercept',model=do.call(test_gru_model,s),verbose=FALSE)
    test_gru_same_weights(fit,other,paste('Joint encoder',paste(hidden,collapse=',')),initial_weights,
      function()LKT(d,usefolds=1,components='skill',features='intercept',model=do.call(test_gru_model,s),verbose=FALSE))
  }
  cat('PASS shared input encoder: direct/linear maps, all three routes, gradients, disabled RNG, GUI, causality, held-out isolation, immutable scoring and continuation.\n')
} else if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required.')
