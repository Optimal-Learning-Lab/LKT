library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=base::identity),'error'))
close <- function(a,b)stopifnot(max(abs(as.numeric(a)-as.numeric(b)))<3e-6)
fail(test_gru_model(numeric_fusion='sum'))
fail(test_gru_model(feature_encoder='mlp',input_fusion='sum'))
fail(test_gru_model(feature_encoder='mlp',categorical_fusion='sum'))
fail(test_gru_model(feature_encoder='mlp',encoder_output_sizes=c(bad=4)))
fail(test_gru_model(feature_encoder='mlp',encoder_output_sizes=4))
fail(test_gru_model(feature_encoder='mlp',encoder_output_sizes=c(time=0)))
fail(test_gru_model(feature_encoder='mlp',numeric_fusion='sum',inference_mode='gaussian'))
fail(test_gru_model(feature_encoder='mlp',numeric_fusion='sum',retrieval_heads=1L,retrieval_cue_cols='KC..Default.'))
if(requireNamespace('torch',quietly=TRUE) && torch::torch_is_installed()) {
  ns <- asNamespace('LKT');network <- get('lkt_gru_network',ns)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:8),each=12),
    item=rep(c('a','b','c','d'),24),skill=rep(c('x','y','x','z'),24),
    difficulty=rep(c(.2,.7,.4,.1),24),CF..Time.=rep(seq(0,1100,100),8),
    CF..ansbin.=rep(c(0,1,1,0,1,0),16),fold=rep(c(1,1,1,1,1,2,3,3),each=12))
  base <- test_gru_model(categorical_cols=c('item','skill'),embedding_dims=4L,
    numeric_cols='difficulty',time_col='CF..Time.',order_col='CF..Time.',
    feature_encoder='mlp',encoder_hidden_sizes=7L,encoder_output_size=4L,
    encoder_output_sizes=c(time=4L,history=4L,response=3L),
    hidden_size=6L,num_layers=3L,head_hidden_sizes=8L,
    retrieval_heads=1L,retrieval_size=8L,retrieval_window=5L,
    retrieval_filter_cols=c('item','skill'),zero_init_embedding_cols='item',
    validation_folds=2,epochs=2L,batch_size=3L,sequence_length=4L,seed=31L)$settings
  choices <- list(baseline=c('concat','concat','concat'),
    temporal_sum=c('sum','concat','concat'),item_sum=c('concat','sum','concat'),
    both_sum=c('sum','sum','sum'),both_concat=c('sum','sum','concat'))
  expected_widths <- c(baseline=16L,temporal_sum=12L,item_sum=12L,both_sum=4L,both_concat=8L)
  for(condition in names(choices)) {
    s <- base;s[c('numeric_fusion','categorical_fusion','input_fusion')] <- as.list(choices[[condition]])
    # FGUI must preserve every width and combination, with no parsed R expressions.
    stopifnot(isTRUE(all.equal(get('lkt_gru_gui_decode',ns)(get('lkt_gru_gui_encode',ns)(s)),s)))
    fit <- LKT(d,usefolds=1,components='skill',features='intercept',model=do.call(test_gru_model,s),verbose=FALSE)
    initial_weights <- serialize(fit$model$weights,NULL)
    runtime <- LKTGRURuntime(fit);net <- runtime$network
    encoded <- get('lkt_gru_encode',ns)(d[1:12,],fit$model$preprocessing,fit$model$recipe,s)
    batch <- get('lkt_gru_batch',ns)(encoded,d[1:12,],list(1:12),list(1:12),s)
    # Compare the actual encoded vector with the literal two-stage sums. There
    # are no hidden projections or extra trainable fusion parameters to absorb it.
    layout <- get('lkt_gru_input_layout',ns)(fit$model$preprocessing,s)
    torch::with_no_grad({
      t <- net$time_encoder(batch$numeric[,,layout$time,drop=FALSE])
      h <- net$auxiliary_encoder(batch$numeric[,,layout$history,drop=FALSE])
      i <- net$embeddings[[1L]](batch$categorical[,,1L,drop=FALSE]$squeeze(3))
      k <- net$embeddings[[2L]](batch$categorical[,,2L,drop=FALSE]$squeeze(3))
      n <- if(s$numeric_fusion=='sum')t+h else torch::torch_cat(list(t,h),dim=3)
      c <- if(s$categorical_fusion=='sum')i+k else torch::torch_cat(list(i,k),dim=3)
      manual <- if(s$input_fusion=='sum')n+c else torch::torch_cat(list(n,c),dim=3)
      close(torch::as_array(manual),torch::as_array(net$encode(batch$numeric,batch$categorical)))
    })
    stopifnot(layout$width==expected_widths[[condition]],
      net$gru$parameters[[1L]]$size(2)==layout$width+3L,
      net$head[[1L]]$weight$size(2)==6L+layout$width,
      net$retrieval$query$weight$size(2)==6L+layout$width,
      !any(grepl('fusion',names(net$named_parameters()))))
    net$train();net$zero_grad()
    net(batch$numeric,batch$categorical,batch$answers,retrieval_ids=batch$retrieval_ids)$logits$sum()$backward()
    parameters <- net$named_parameters()
    for(prefix in c('response_encoder','time_encoder','auxiliary_encoder','embeddings.0','embeddings.1')) {
      selected <- parameters[startsWith(names(parameters),prefix)]
      stopifnot(any(vapply(selected,function(p)!is.null(p$grad) && p$grad$abs()$sum()$item()>0,logical(1))))
    }
    close(predict_lkt(fit,d)$predictions,fit$prediction)
    a <- LKTGRUStep(fit,LKTGRUState(fit),d[1:5,])
    b <- LKTGRUStep(fit,a$state,d[6:12,])
    close(c(a$predictions,b$predictions),fit$prediction[1:12])
    pending <- d[6,,drop=FALSE];pending$CF..ansbin. <- NA_real_
    state_copy <- serialize(a$state,NULL)
    close(LKTGRUScore(fit,a$state,pending),fit$prediction[6])
    stopifnot(identical(state_copy,serialize(a$state,NULL)))
    file <- tempfile(fileext='.rds');saveRDS(list(fit=fit,state=a$state),file)
    saved <- readRDS(file);unlink(file)
    close(LKTGRUStep(saved$fit,saved$state,d[6:12,])$predictions,b$predictions)
    changed <- d;changed$CF..ansbin.[6:12] <- 1-changed$CF..ansbin.[6:12]
    close(predict_lkt(fit,changed)$predictions[1:6],fit$prediction[1:6])
    changed <- d;changed$CF..ansbin.[d$fold==3] <- 1-changed$CF..ansbin.[d$fold==3]
    other <- LKT(changed,usefolds=1,components='skill',features='intercept',model=do.call(test_gru_model,s),verbose=FALSE)
    test_gru_same_weights(fit,other,paste('Input fusion',condition),initial_weights,
      function()LKT(d,usefolds=1,components='skill',features='intercept',model=do.call(test_gru_model,s),verbose=FALSE))
  }
  prep <- fit$model$preprocessing
  bad <- base;bad$numeric_fusion <- 'sum';bad$encoder_output_sizes <- c(time=3L,history=4L)
  fail(network(prep,bad))
  bad <- base;bad$categorical_fusion <- 'sum';bad$embedding_dims <- c(item=4L,skill=3L)
  p <- prep;p$embedding_dims <- c(4L,3L);fail(network(p,bad))
  bad <- base;bad$numeric_fusion <- 'sum';bad$categorical_fusion <- 'sum';bad$input_fusion <- 'sum'
  bad$encoder_output_sizes <- c(time=3L,history=3L);fail(network(prep,bad))
  cat('PASS all four input combinations: exact sums, widths, gradients, GUI round trips, causal prediction, immutable scoring, continuation and persistence.\n')
} else if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required.')
