library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
if(!requireNamespace('torch',quietly=TRUE)||!torch::torch_is_installed()) {
  if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required')
} else {
  close <- function(a,b)stopifnot(isTRUE(all.equal(a,b,tolerance=3e-6,check.attributes=FALSE)))
  fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))
  set.seed(91)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:6),each=8),
    KC..Default.=rep(c('a','b'),24),CF..Time.=rep(seq(0,700,100),6),
    CF..ansbin.=rbinom(48,1,.6),ability=rep(seq(-1,1,length.out=6),each=8))
  for(mode in c('none','deterministic','gaussian'))for(engineered in c(FALSE,TRUE)) {
    model <- LKT(d,features='lineafm',components='KC..Default.',verbose=FALSE,
      model=test_gru_model(epochs=1L,hidden_size=4L,head_hidden_sizes=3L,embedding_dims=2L,
        latent_size=3L,inference_hidden_sizes=4L,inference_mode=mode,
        numeric_cols='ability',use_lkt_features=engineered,time_col='CF..Time.',
        order_col='CF..Time.',validation_fraction=0,early_stopping=FALSE,seed=6L))
    set.seed(81);rseed <- .Random.seed;trng <- torch::torch_get_rng_state()
    threads <- torch::torch_get_num_threads()
    runtime <- LKTGRURuntime(model)
    stopifnot(identical(rseed,.Random.seed),torch::torch_equal(trng,torch::torch_get_rng_state()),
      threads==torch::torch_get_num_threads())
    initial <- LKTGRUState(model)
    plain <- LKTGRUStep(model,initial,d[1:3,])
    loaded <- LKTGRUStep(model,initial,d[1:3,],runtime)
    close(plain$predictions,loaded$predictions)
    stopifnot(identical(plain$state,loaded$state))
    before <- loaded$state
    candidates <- d[rep(4,3),];candidates$KC..Default. <- c('a','b','a')
    p <- LKTGRUScore(model,before,candidates,runtime)
    reversed <- LKTGRUScore(model,before,candidates[3:1,],runtime)
    close(p,rev(reversed));close(p[c(1,2)],LKTGRUScore(model,before,candidates[1:2,],runtime))
    for(i in seq_len(nrow(candidates))) {
      row <- candidates[i,,drop=FALSE];row$CF..ansbin. <- NA_real_
      close(p[i],LKTGRUStep(model,before,row,runtime)$predictions)
    }
    stopifnot(identical(before,loaded$state),identical(rseed,.Random.seed),
      torch::torch_equal(trng,torch::torch_get_rng_state()),threads==torch::torch_get_num_threads())
    answered <- LKTGRUStep(model,before,d[4,,drop=FALSE],runtime)
    close(answered$predictions,LKTGRUScore(model,before,d[4,,drop=FALSE],runtime))
    path <- tempfile(fileext='.rds');saveRDS(list(model=model,state=before),path)
    restored <- readRDS(path);unlink(path)
    close(LKTGRUStep(restored$model,restored$state,d[4,,drop=FALSE],LKTGRURuntime(restored$model))$predictions,
      answered$predictions)
    broken <- runtime
    other <- model;other$model$id <- paste0(model$model$id,'_other')
    fail(LKTGRUScore(other,LKTGRUState(other),d[4,,drop=FALSE],broken))
  }
  # Exercise first-use Torch namespace loading in a genuinely fresh process.
  payload <- tempfile(fileext='.rds');script <- tempfile(fileext='.R')
  saveRDS(model,payload)
  writeLines(c('library(LKT)',paste0('model <- readRDS(',deparse(payload),')'),
    "stopifnot(!'torch'%in%loadedNamespaces())",'set.seed(83);before <- .Random.seed',
    'runtime <- LKTGRURuntime(model)',
    "stopifnot(identical(before,.Random.seed));cat('PASS cold Torch startup preserves caller RNG\\n')"),script)
  status <- system2(file.path(R.home('bin'),'Rscript'),c('--vanilla',shQuote(script)))
  unlink(c(payload,script));stopifnot(status==0L)
  cat('PASS runtime parity, independent candidates, state persistence, all inference modes, engineered/raw features, RNG and thread preservation\n')
}
