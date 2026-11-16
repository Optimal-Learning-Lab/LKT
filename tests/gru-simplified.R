library(LKT)
fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))
close <- function(a,b,tolerance=3e-6)stopifnot(max(abs(as.numeric(a)-as.numeric(b)))<tolerance)
alternative <- list(time_col='CF..Time.',numeric_encoder='combined',response_encoding='binary',
  encoder_output_sizes=c(numeric=32L),retrieval_value_mode='raw')
fail(GRUModel(numeric_encoder='combined',feature_encoder='none'))
fail(GRUModel(numeric_encoder='combined',numeric_fusion='sum'))
fail(GRUModel(numeric_encoder='combined',encoder_output_sizes=c(time=16L)))
fail(GRUModel(numeric_encoder='separate',encoder_output_sizes=c(numeric=32L)))
fail(GRUModel(response_encoding='binary',encoder_output_sizes=c(response=16L)))
fail(GRUModel(retrieval_value_mode='raw',retrieval_heads=2L))
fail(GRUModel(retrieval_value_mode='raw',retrieval_dropout=.1))
s <- do.call(GRUModel,alternative)$settings
ns <- asNamespace('LKT')
stopifnot(isTRUE(all.equal(get('lkt_gru_gui_decode',ns)(get('lkt_gru_gui_encode',ns)(s)),s)))
if(requireNamespace('torch',quietly=TRUE) && torch::torch_is_installed()) {
  tensor <- function(x)torch::torch_tensor(x,dtype=torch::torch_float64())
  network <- get('lkt_gru_network',ns);weights <- get('lkt_gru_weights',ns)
  load <- get('lkt_gru_load_weights',ns)
  # Exact conversion of the two redundant parameterizations, independently of
  # the deliberately changed numeric encoder. Use nonzero biases and unequal
  # attention/state widths to exercise both value projections and their bias.
  prep <- list(numeric_names=c('time:gap','history'),embedding_dims=c(2L,2L),
    vocabularies=list(KC..Default.=c('a','b'),KC..Cluster.=c('x','y')))
  old <- GRUModel(hidden_size=3L,embedding_dims=2L,encoder_hidden_sizes=5L,
    encoder_output_size=4L,head_hidden_sizes=6L,dtype='float64',
    input_dropout=0,gru_dropout=0,head_dropout=0,retrieval_size=7L,
    retrieval_window=3L,numeric_encoder='separate',encoder_output_sizes=integer(),
    response_encoding='mlp',retrieval_value_mode='projected')$settings
  new <- old;new$response_encoding <- 'binary';new$retrieval_value_mode <- 'raw'
  a <- network(prep,old);b <- network(prep,new);w <- weights(a);nw <- weights(b)
  for(k in intersect(names(w),names(nw)))if(identical(dim(w[[k]]),dim(nw[[k]])))nw[[k]] <- w[[k]]
  affine <- function(prefix,x)as.numeric(w[[paste0(prefix,'.weight')]]%*%x+w[[paste0(prefix,'.bias')]])
  response <- function(y)affine('response_encoder.1',pmax(0,affine('response_encoder.0.0',y)))
  f0 <- response(0);delta <- response(1)-f0
  U <- w[['gru.weight_ih_l1']];response_columns <- 13:16
  nw[['gru.weight_ih_l1']] <- cbind(U[,1:12],U[,response_columns]%*%delta)
  nw[['gru.bias_ih_l1']] <- w[['gru.bias_ih_l1']]+as.numeric(U[,response_columns]%*%f0)
  nw[['retrieval.output.weight']] <- w[['retrieval.output.weight']]%*%w[['retrieval.value.weight']]
  nw[['retrieval.output.bias']] <- as.numeric(w[['retrieval.output.weight']]%*%w[['retrieval.value.bias']]+w[['retrieval.output.bias']])
  load(b,nw,new);a$eval();b$eval()
  nums <- tensor(array(sin(1:16),c(1,8,2)))
  cats <- torch::torch_tensor(array(rep(c(2L,3L),8),c(1,8,2)),dtype=torch::torch_long())
  answers <- tensor(matrix(c(0,1,0,1,1,0,1,0),1))
  ids <- array(rep(c('a','b'),8),c(1,8,2))
  torch::with_no_grad({
    before <- a(nums,cats,answers,retrieval_ids=ids)
    after <- b(nums,cats,answers,retrieval_ids=ids)
    close(torch::as_array(before$logits),torch::as_array(after$logits),1e-12)
    close(torch::as_array(before$hidden),torch::as_array(after$hidden),1e-12)
    close(torch::as_array(before$retrieval_attention),torch::as_array(after$retrieval_attention),1e-12)
    for(k in c('retrieval_keys','retrieval_values'))
      close(torch::as_array(before$context[[k]]),torch::as_array(after$context[[k]]),1e-12)
  })
  # One fresh fit of the complete alternative: seven numeric inputs, no
  # intercept, one encoder, and raw observed answers. This is a contract smoke
  # test, not a predictive comparison with the historical benchmark.
  set.seed(48)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:8),each=12),
    KC..Default.=rep(c('a','b','c'),32),KC..Cluster.=rep(c('k1','k2','k1'),32),
    CF..Time.=rep(seq(100,1200,100),8),CF..ansbin.=rbinom(96,1,.6),
    fold=rep(c(1,1,1,1,1,2,3,3),each=12))
  d$CF..reltime. <- d$CF..Time.
  d <- LKTGRUTimeFeatures(d)
  controls <- utils::modifyList(alternative,list(epochs=1L,validation_folds=2,
    batch_size=3L,sequence_length=4L,seed=48L),keep.null=TRUE)
  fit <- LKT(d,usefolds=1,interc=FALSE,
    components=c('Anon.Student.Id','KC..Default.','KC..Default.'),
    features=c('logitdec','ppes','logitdec'),
    fixedpars=c(.9,.35,.39,.02,.72,.58),
    model=do.call(GRUModel,controls),verbose=FALSE)
  stopifnot(length(fit$model$preprocessing$numeric_names)==7L,
    !any(grepl('Intercept',fit$model$preprocessing$numeric_names,fixed=TRUE)),
    identical(dim(fit$model$weights[['numeric_encoder.0.0.weight']]),c(32L,7L)),
    identical(dim(fit$model$weights[['numeric_encoder.1.weight']]),c(32L,32L)),
    identical(dim(fit$model$weights[['gru.weight_ih_l1']]),c(96L,65L)),
    identical(dim(fit$model$weights[['head.0.weight']]),c(128L,96L)),
    identical(dim(fit$model$weights[['retrieval.query.weight']]),c(32L,96L)),
    !any(grepl('response_encoder|time_encoder|auxiliary_encoder|retrieval.value',names(fit$model$weights))),
    all(is.finite(fit$prediction)))
  net <- LKTGRURuntime(fit)$network
  encoded <- get('lkt_gru_encode',ns)(d[1:12,],fit$model$preprocessing,fit$model$recipe,fit$gru_settings)
  batch <- get('lkt_gru_batch',ns)(encoded,d[1:12,],list(1:12),list(1:12),fit$gru_settings)
  net$eval()
  torch::with_no_grad({
    expected <- torch::torch_cat(list(net$numeric_encoder(batch$numeric),
      net$embeddings[[1L]](batch$categorical[,,1L,drop=FALSE]$squeeze(3)),
      net$embeddings[[2L]](batch$categorical[,,2L,drop=FALSE]$squeeze(3))),dim=3)
    close(torch::as_array(expected),torch::as_array(net$encode(batch$numeric,batch$categorical)))
    out <- net(batch$numeric,batch$categorical,batch$answers,retrieval_ids=batch$retrieval_ids)
    first_read <- torch::as_array(out$retrieval_attention)[1,1,1,]
    stopifnot(all(first_read==0))
  })
  net$train();net$zero_grad()
  net(batch$numeric,batch$categorical,batch$answers,retrieval_ids=batch$retrieval_ids)$logits$sum()$backward()
  parameters <- net$named_parameters()
  for(prefix in c('numeric_encoder','embeddings.0','embeddings.1','gru','retrieval','head'))
    stopifnot(any(vapply(parameters[startsWith(names(parameters),prefix)],
      function(p)!is.null(p$grad) && p$grad$abs()$sum()$item()>0,logical(1))))
  close(predict_lkt(fit,d)$predictions,fit$prediction)
  x <- LKTGRUStep(fit,LKTGRUState(fit),d[1:5,])
  y <- LKTGRUStep(fit,x$state,d[6:12,])
  close(c(x$predictions,y$predictions),fit$prediction[1:12])
  pending <- d[6,,drop=FALSE];pending$CF..ansbin. <- NA_real_
  state_copy <- serialize(x$state,NULL)
  close(LKTGRUScore(fit,x$state,pending),fit$prediction[6])
  stopifnot(identical(state_copy,serialize(x$state,NULL)))
  file <- tempfile(fileext='.rds');saveRDS(list(fit=fit,state=x$state),file)
  restored <- readRDS(file);unlink(file)
  close(LKTGRUStep(restored$fit,restored$state,d[6:12,])$predictions,y$predictions)
  changed <- d;changed$CF..ansbin.[6:12] <- 1-changed$CF..ansbin.[6:12]
  close(predict_lkt(fit,changed)$predictions[1:6],fit$prediction[1:6])
  cat('PASS simplified alternative: exact redundant-map conversion, seven-input encoder, shapes, gradients, replay, causality, persistence and continuation.\n')
} else if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required.')
