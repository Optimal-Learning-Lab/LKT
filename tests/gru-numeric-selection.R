library(LKT)
ns <- asNamespace('LKT')
fail <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))
for(x in list(1,NA_character_,c('a','a'),''))fail(GRUModel(numeric_feature_cols=x))
encode <- get('lkt_gru_gui_encode',ns);decode <- get('lkt_gru_gui_decode',ns)
for(x in list(NULL,character(),c('raw:a','time:b'))) {
  s <- GRUModel(numeric_feature_cols=x)$settings
  stopifnot(isTRUE(all.equal(decode(as.list(encode(s))),s)))
}
if(requireNamespace('torch',quietly=TRUE) && torch::torch_is_installed()) {
  set.seed(63)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:8),each=12),
    KC..Default.=rep(c('a','b','c'),32),KC..Cluster.=rep(c('k1','k2','k1'),32),
    CF..Time.=rep(seq(100,1200,100),8),CF..ansbin.=rbinom(96,1,.6),
    fold=rep(c(1,1,1,1,1,2,3,3),each=12))
  d$CF..reltime. <- d$CF..Time.;d <- LKTGRUTimeFeatures(d)
  fit <- function(selected) LKT(d,usefolds=1,interc=FALSE,
    components=c('Anon.Student.Id','KC..Default.','KC..Default.'),
    features=c('logitdec','ppes','logitdec'),fixedpars=c(.9,.35,.39,.02,.72,.58),
    model=GRUModel(time_col='CF..Time.',numeric_feature_cols=selected,epochs=1L,validation_folds=2,
      batch_size=3L,sequence_length=4L,seed=63L),verbose=FALSE)
  full <- fit(NULL);recipe <- full$model$recipe;s <- full$gru_settings
  names <- full$model$preprocessing$numeric_names
  prep <- get('lkt_gru_preprocess',ns);numeric <- get('lkt_gru_numeric',ns)
  all <- numeric(d,recipe,s,fit_rows=d$fold==1)$matrix
  selections <- c(list(character(),names),as.list(names),lapply(seq_along(names),function(j)names[-j]))
  for(selected in selections) {
    ss <- s;ss$numeric_feature_cols <- selected
    pp <- prep(d,d$fold==1,recipe,ss)
    xx <- numeric(d,recipe,ss,fit_rows=d$fold==1)$matrix
    stopifnot(identical(as.character(colnames(xx)),selected),
      isTRUE(all.equal(unname(as.matrix(xx)),unname(as.matrix(all[,selected,drop=FALSE])))),
      all(is.finite(c(pp$center,pp$scale))))
  }
  ss <- s;ss$numeric_feature_cols <- 'does-not-exist';fail(prep(d,d$fold==1,recipe,ss))
  # Omitted spacing cannot enter even through the retained availability flag.
  ss$numeric_feature_cols <- names[4];a <- numeric(d,recipe,ss,fit_rows=d$fold==1)$matrix
  changed <- d;observed <- !is.na(changed$gru_mean_prior_spacing_seconds)
  changed$gru_mean_prior_spacing_seconds[observed] <- changed$gru_mean_prior_spacing_seconds[observed]+9999
  b <- numeric(changed,recipe,ss,fit_rows=d$fold==1)$matrix
  stopifnot(identical(a,b))
  for(selected in list(character(),names,names[3],names[4],names[-3])) {
    f <- fit(selected)
    stopifnot(identical(as.character(f$model$preprocessing$numeric_names),selected),
      all(is.finite(f$prediction)),all(is.finite(unlist(f$model$weights))))
    if(!length(selected))stopifnot(!any(grepl('numeric_encoder',names(f$model$weights))))
    rt <- LKTGRURuntime(f)
    step <- LKTGRUStep(f,LKTGRUState(f),d[1:12,],rt)
    stopifnot(max(abs(step$predictions-f$prediction[1:12]))<3e-6)
    if(identical(selected,names))stopifnot(identical(f$prediction,full$prediction))
  }
  cat('PASS all 16 selections, missing-spacing independence, no-input encoder removal, full-input equivalence and five fitted continuation replays.\n')
} else if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch required.')

