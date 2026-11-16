library(LKT)
if(!requireNamespace('torch',quietly=TRUE)||!torch::torch_is_installed()) {
  if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required')
} else {
  source('fixtures/gru-minimal-settings.R')
  close <- function(a,b)stopifnot(max(abs(a-b))<3e-6)
  fail <- function(x)stopifnot(inherits(tryCatch({force(x);NULL},error=identity),'error'))
  d <- data.frame(Anon.Student.Id=rep(c('a','b'),each=6),
    KC..Default.=rep(c('i','j','k'),4),KC..Cluster.=rep(c('s','t','s'),4),
    CF..Time.=rep(seq(0,300,60),2),CF..ansbin.=rep(c(1,0,1,0,0,1),2))
  controls <- list(epochs=1L,hidden_size=4L,head_hidden_sizes=5L,embedding_dims=2L,
    categorical_cols=c('KC..Default.','KC..Cluster.'),feature_encoder='mlp',
    encoder_output_size=3L,retrieval_heads=1L,retrieval_size=4L,
    retrieval_filter_cols=c('KC..Default.','KC..Cluster.'),retrieval_dropout=0,
    embedding_adaptation='per_student',adaptive_embedding_cols=c('KC..Default.','KC..Cluster.'),
    plasticity='constant',plasticity_time_col='CF..Time.',
    validation_fraction=0,early_stopping=FALSE,seed=71L)
  fit <- LKT(d,features='lineafm',components='KC..Default.',verbose=FALSE,
    model=do.call(test_gru_model,controls))
  original <- serialize(fit,NULL)
  optional <- LKTGRUOptionalIdentities(fit,'KC..Cluster.')
  encode <- getFromNamespace('lkt_gru_gui_encode','LKT')
  decode <- getFromNamespace('lkt_gru_gui_decode','LKT')
  decoded <- decode(as.list(encode(optional$model$settings)))
  stopifnot(isTRUE(all.equal(decoded,optional$model$settings)),
    identical(decoded$optional_adaptive_cols,'KC..Cluster.'))
  stopifnot(identical(original,serialize(fit,NULL)),
    identical(fit$model$weights,optional$model$weights),
    identical(fit$model$preprocessing,optional$model$preprocessing),fit$model$id!=optional$model$id)
  close(LKTGRUStep(fit,LKTGRUState(fit),d)$predictions,
    LKTGRUStep(optional,LKTGRUState(optional),d)$predictions)
  fail(LKTGRUStep(optional,LKTGRUState(fit),d))
  unmarked <- d[1:6,];unmarked$KC..Cluster. <- NULL
  run <- LKTGRURuntime(optional);start <- LKTGRUState(optional)
  x <- LKTGRUStep(optional,start,unmarked,run)
  stopifnot(is.null(x$state$adaptive_embeddings$students$a[['KC..Cluster.']]),
    length(x$state$adaptive_embeddings$students$a[['KC..Default.']])==3L)
  explicit <- unmarked;explicit$KC..Cluster. <- NA_character_
  close(x$predictions,LKTGRUStep(optional,start,explicit,run)$predictions)
  fail(LKTGRUStep(fit,LKTGRUState(fit),unmarked))
  later <- explicit[1:2,];later$CF..Time. <- c(360,420)
  later$KC..Default. <- c('new-item-one','new-item-two');later$KC..Cluster. <- c(NA,'new-skill')
  before <- serialize(x$state,NULL)
  close(LKTGRUScore(optional,x$state,later,run),LKTGRUScore(optional,x$state,later,run,'batched'))
  stopifnot(identical(before,serialize(x$state,NULL)))
  y <- LKTGRUStep(optional,x$state,later,run)
  stopifnot(identical(names(y$state$adaptive_embeddings$students$a[['KC..Cluster.']]),'new-skill'))
  first <- LKTGRUStep(optional,start,unmarked[1:3,],run)
  close(x$predictions,c(first$predictions,LKTGRUStep(optional,first$state,unmarked[4:6,],run)$predictions))
  path <- tempfile();saveRDS(y$state,path);saved <- readRDS(path);unlink(path)
  stopifnot(identical(saved,y$state),identical(original,serialize(fit,NULL)))
  controls$optional_adaptive_cols <- 'KC..Cluster.'
  trained <- LKT(unmarked,features='lineafm',components='KC..Default.',verbose=FALSE,
    model=do.call(test_gru_model,controls))
  stopifnot(length(trained$model$preprocessing$vocabularies[['KC..Cluster.']])==0L)
  close(trained$prediction,LKTGRUStep(trained,LKTGRUState(trained),unmarked)$predictions)
  fail(GRUModel(optional_adaptive_cols='KC..Default.'))
  blank <- explicit;blank$KC..Cluster. <- '';fail(LKTGRUStep(optional,start,blank,run))
  cat('PASS optional unmarked skills: neutral/no update, strict historical fits, supplied novel labels, batched scoring, training replay, persistence and unchanged weights\n')
}
