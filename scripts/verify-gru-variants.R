# Read-only constructor/input contracts: no fitting or scientific output writes.
a <- commandArgs(TRUE);stopifnot(length(a)==1L);root <- a[1]
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-variants.R'))
ordinary <- LKT::GRUModel()$settings
catalog <- gru_variant_catalog(root)
d <- data.frame(Anon.Student.Id=rep(paste0('s',1:6),each=4L),
  KC..Default.=rep(c('i1','i2'),12L),KC..Cluster.=rep(c('k1','k2'),12L),
  CF..Time.=rep(as.numeric(1:4),6L),CF..ansbin.=rep(c(0,1,1,0),6L),
  Duration..sec.=rep(1,24),fold=rep(c(1L,1L,2L,2L,3L,3L),each=4L),
  random_intercept=rep(c(-.4,.4,0,0,0,0),each=4L),kc_intercept=rep(c(-.2,.2),12L))
d <- LKT::LKTGRUTimeFeatures(d)
recipe <- list(components=c('Anon.Student.Id','KC..Default.','KC..Default.'),
  features=c('logitdec','ppes','logitdec'),fixedpars=c(.8,.3,.4,.1,.7,.6),
  interc=FALSE,interacts=rep(NA_character_,3L),connectors=c('+','+'),maketimes=FALSE)
preprocess <- getFromNamespace('lkt_gru_preprocess','LKT')
for(id in names(catalog$variants)) {
  s <- gru_variant_model(id,root)$settings
  p <- preprocess(d,d$fold==1L,recipe,s)
  stopifnot(length(p$numeric_names)==catalog$variants[[id]]$numeric_input_count,
    s$plasticity=='constant',s$num_layers==3L,s$hidden_size==32L,
    !s$subject_col%in%s$categorical_cols)
  if(id=='baseline_six')stopifnot(s$embedding_adaptation=='none') else
    stopifnot(s$embedding_adaptation=='per_student',setequal(s$adaptive_embedding_cols,s$categorical_cols))
  if(id=='ground_truth_eight')stopifnot(all(c('raw:random_intercept','raw:kc_intercept')%in%p$numeric_names)) else
    stopifnot(!any(grepl('^raw:',p$numeric_names)))
  cat('PASS',id,length(p$numeric_names),'numeric inputs;',s$embedding_adaptation,'; constant output learning\n')
}
must_fail <- function(expr)stopifnot(inherits(tryCatch(force(expr),error=function(e)e),'error'))
must_fail(gru_variant_model('unknown',root))
must_fail(gru_variant_model('tracer_six',root,overrides=list(embedding_adaptation='none')))
stopifnot(identical(ordinary,LKT::GRUModel()$settings),
  jsonlite::fromJSON(file.path(root,'scripts/gru-tracer-training-only.json'))$variant_id=='tracer_six',
  jsonlite::fromJSON(file.path(root,'scripts/gru-ground-truth-eight-input.json'))$variant_id=='ground_truth_eight',
  jsonlite::fromJSON(file.path(root,'scripts/gru-ground-truth-training-only.json'))$variant_id=='ground_truth_eight')
cat('PASS ordinary defaults unchanged; input/identity contracts explicit; configuration IDs consistent\n')
