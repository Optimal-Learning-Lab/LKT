# Structural, causal and saved-replay checks; no benchmark or tuning experiment.
library(LKT)
f <- function(n)getFromNamespace(n,'LKT')
expect_error <- function(expr,pattern) {
  e <- tryCatch({force(expr);NULL},error=identity)
  stopifnot(inherits(e,'error'),grepl(pattern,conditionMessage(e)))
}
stopifnot(is.null(GRUModel()$settings$semantic_input))
config <- Sys.getenv('LKT_SEMANTIC_TEST_CONFIG')
if(!nzchar(config)) {
  cat('Semantic integration tests require explicit LKT_SEMANTIC_TEST_CONFIG; no encoder is substituted.\n')
} else {
  z <- jsonlite::fromJSON(config,simplifyVector=TRUE)
  # The fixture retains literal empty responses; no artificial feedback field exists.
  d <- data.frame(Anon.Student.Id=rep(c('a','b'),each=4),clock=rep(1:4,2),
    KC..Default.=rep(c('q1','q2'),4),KC..Cluster.=rep(c('1. Mean describes a center.','2. Variance describes spread.'),4),
    Problem.Name=rep(c('The arithmetic average is called the _____.','Squared dispersion is called _____.'),4),
    Input=c('mean','','variance','mean','variance','mean','','variance'),
    CF..ansbin.=c(1,0,1,0,0,1,0,1),stringsAsFactors=FALSE)
  for(j in 1:5)d[[paste0('n',j)]] <- seq_len(nrow(d))/10+j
  z$question_col <- 'Problem.Name';z$response_col <- 'Input';z$kc_col <- 'KC..Cluster.';z$kc_strip_number <- TRUE
  z$cache_file <- z$cache_sha256 <- NULL
  opts <- list(semantic_input=z,numeric_cols=paste0('n',1:5),use_lkt_features=FALSE,
    time_feature_cols=character(),time_missing_cols=character(),order_col='clock',
    plasticity='constant',plasticity_time_col='clock',input_dropout=0,gru_dropout=0,head_dropout=0,
    epochs=1L,sequence_length=2L,batch_size=2L,validation_fraction=0,early_stopping=FALSE,dtype='float64')
  s <- do.call(GRUModel,opts)$settings
  gui <- f('lkt_gru_gui_decode')(as.list(f('lkt_gru_gui_encode')(s)))
  stopifnot(isTRUE(all.equal(gui,s)),identical(gui$semantic_input,s$semantic_input))
  expect_error(do.call(GRUModel,utils::modifyList(opts,list(input_to_all_layers=TRUE))), 'first-layer-only')
  expect_error(SequenceModel(unit='mamba',semantic_input=z), 'only for the GRU')
  recipe <- list(components='KC..Default.',features='lineafm',fixedpars=numeric(),
    interacts=NA_character_,connectors=character(),interc=FALSE,maketimes=FALSE,subject_col='Anon.Student.Id')
  cleanup <- f('lkt_gru_runtime')(s)
  prep <- f('lkt_gru_preprocess')(d,rep(TRUE,nrow(d)),recipe,s)
  enc <- f('lkt_gru_encode')(d,prep,recipe,s)
  prep$semantic_cache <- f('lkt_gru_semantic_snapshot')(enc$semantic_cache)
  stopifnot('' %in% prep$semantic_cache$texts,ncol(prep$semantic_cache$embeddings)==768L)
  bad <- d;bad$Input[1] <- NA_character_
  expect_error(f('lkt_gru_encode')(bad,prep,recipe,s),'no NA')
  # Explicit shared-cache import is hash-bound and survives removal of its input file.
  cache_file <- tempfile(fileext='.rds');saveRDS(prep$semantic_cache,cache_file)
  zs <- z;zs$cache_file <- cache_file;zs$cache_sha256 <- f('lkt_gru_semantic_hash')(cache_file)
  shared <- s;shared$semantic_input <- zs
  cached <- f('lkt_gru_encode')(d,f('lkt_gru_preprocess')(d,rep(TRUE,nrow(d)),recipe,shared),recipe,shared)
  stopifnot(identical(enc$semantic_ids,cached$semantic_ids))
  groups <- split(seq_len(nrow(d)),d$Anon.Student.Id)
  b <- f('lkt_gru_batch')(enc,d,groups,lapply(groups,seq_along),s)
  net <- f('lkt_gru_network')(prep,s);net$eval()
  stopifnot(net$gru$weight_ih_l1$size(2)==97L,
    net$semantic_projection$weight$numel()+net$semantic_projection$bias$numel()==73760)
  plain <- s;plain$semantic_input <- NULL
  base <- f('lkt_gru_network')(prep,plain)
  count <- function(n)sum(vapply(n$parameters,function(p)as.numeric(p$numel()),numeric(1)))
  stopifnot(count(net)-count(base)==76832)
  full <- d[rep(1:8,length.out=144),];full$KC..Default. <- paste0('item',1:144)
  full$KC..Cluster. <- paste0('skill',rep(1:36,4))
  fp <- f('lkt_gru_preprocess')(full,rep(TRUE,144),recipe,s)
  stopifnot(count(f('lkt_gru_network')(fp,plain))==43042,
    count(f('lkt_gru_network')(fp,s))==119874)
  run <- function(semantic=b$semantic)f('lkt_gru_forward')(net,b$numeric,b$categorical,b$answers,
    NULL,NULL,b$retrieval_ids,s,b$decay_gaps,!is.na(b$rows),semantic)
  before <- run();changed <- b$semantic$clone();changed[,2,] <- changed[,2,]+.2
  after <- run(changed)
  a <- torch::as_array(before$logits);bb <- torch::as_array(after$logits)
  stopifnot(identical(a[,1:2],bb[,1:2]),max(abs(a[,3:4]-bb[,3:4]))>1e-10)
  loss <- torch::nnf_binary_cross_entropy_with_logits(before$logits,b$answers)
  loss$backward();stopifnot(net$semantic_projection$weight$grad$abs()$sum()$item()>0)
  # Chunked replay and one-call forward use the same semantic updates/retrieval.
  replay <- f('lkt_gru_replay')(net,enc,d,s)
  stopifnot(max(abs(replay[b$rows]-as.vector(torch::as_array(torch::torch_sigmoid(run()$logits)))))<1e-9)
  s$semantic_input <- zs
  fit <- f('lkt_fit_gru')(list(data=d,masks=list(train=rep(TRUE,8),validation=rep(FALSE,8),evaluation=rep(FALSE,8)),recipe=recipe),s)
  fit$model_name <- 'GRU'
  public <- LKT(d,components='KC..Default.',features='intercept',fixedpars=numeric(),
    model=do.call(GRUModel,s),verbose=FALSE)
  stopifnot(max(abs(public$prediction-fit$pred))<1e-9,
    identical(public$model$settings$semantic_input,s$semantic_input))
  saved <- tempfile(fileext='.rds');saveRDS(fit,saved);restored <- readRDS(saved)
  unlink(cache_file)
  state <- LKTGRUState(restored);step <- LKTGRUStep(restored,state,d)
  stopifnot(max(abs(step$predictions-fit$pred))<1e-9)
  # A genuinely unseen answer reaches later state, never its own probability.
  first <- d[1,,drop=FALSE];unseen <- first;unseen$Input <- 'previously unseen short answer'
  rt <- LKTGRURuntime(restored)
  old <- LKTGRUStep(restored,state,first,runtime=rt)
  fresh <- LKTGRUStep(restored,state,unseen,runtime=rt)
  stopifnot(identical(old$predictions,fresh$predictions),
    !identical(old$state$hidden,fresh$state$hidden),'previously unseen short answer' %in% rt$semantic_cache$texts)
  pending <- first;pending$CF..ansbin. <- NA_real_;pending$Input <- NA_character_
  pp <- LKTGRUStep(restored,state,pending,runtime=rt)
  done <- LKTGRUStep(restored,pp$state,unseen,runtime=rt)
  stopifnot(identical(done$predictions,fresh$predictions),identical(done$state$hidden,fresh$state$hidden))
  scored <- LKTGRUScore(restored,state,unseen,runtime=rt)
  stopifnot(identical(scored,fresh$predictions),!length(state$hidden))
  tampered <- s$semantic_input;tampered$helper_sha256 <- paste(rep('0',64),collapse='')
  expect_error(f('lkt_gru_semantic_helper')('new',tampered),'SHA256 mismatch')
  corrupt <- prep$semantic_cache;corrupt$embeddings[1,1] <- NA_real_
  expect_error(f('lkt_gru_semantic_cache')(corrupt),'corrupt')
  unlink(c(cache_file,saved));cleanup()
  cat('PASS semantic dimensions/counts, real local encoding/empty strings, NA rejection, gradients, causal future independence, chunked replay, training, saved replay, unseen answers, pending completion, candidate scoring and hash failures.\n')
}
