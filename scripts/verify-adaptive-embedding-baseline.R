# Capture before editing; compare the same immutable fixtures after installation.
# Rscript SCRIPT ROOT OUTPUT [CANDIDATE_LIBRARY]
a <- commandArgs(TRUE);stopifnot(length(a)%in%c(2L,3L))
root <- normalizePath(a[1],winslash='/');out <- a[2]
if(length(a)==2L)source(file.path(root,'scripts/load-current-lkt.R')) else {
  .libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
  library(LKT,lib.loc=normalizePath(a[3],winslash='/'))
}
source(file.path(root,'tests/fixtures/gru-minimal-settings.R'))
path <- file.path(out,'baseline.rds')
if(length(a)==2L) {
  stopifnot(!file.exists(path));dir.create(out,recursive=TRUE,showWarnings=FALSE)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:4),each=8),
    KC..Default.=rep(c('a','b'),16),CF..Time.=rep(seq(0,700,100),4),
    CF..ansbin.=rep(c(0,1,0,1,1,0,1,1),4))
  fits <- lapply(c('none','mlp'),function(encoder)LKT(d,features='lineafm',components='KC..Default.',
    verbose=FALSE,model=test_gru_model(epochs=1L,hidden_size=4L,head_hidden_sizes=3L,
      embedding_dims=2L,feature_encoder=encoder,encoder_output_size=3L,
      validation_fraction=0,early_stopping=FALSE,seed=6L)))
  records <- lapply(fits,function(f) {
    step <- LKTGRUStep(f,LKTGRUState(f),d[1:5,])
    list(fit=f,state=step$state,predictions=step$predictions,
      candidates=LKTGRUScore(f,step$state,d[6:8,]))
  })
  saveRDS(list(data=d,records=records,version=as.character(packageVersion('LKT'))),path)
  cat('CAPTURED baseline',path,'\n')
} else {
  b <- readRDS(path)
  for(r in b$records) {
    step <- LKTGRUStep(r$fit,LKTGRUState(r$fit),b$data[1:5,])
    stopifnot(identical(step$state,r$state),identical(step$predictions,r$predictions),
      identical(LKTGRUScore(r$fit,step$state,b$data[6:8,]),r$candidates))
    refit <- LKT(b$data,features='lineafm',components='KC..Default.',verbose=FALSE,
      model=do.call(GRUModel,r$fit$model$settings))
    stopifnot(identical(refit$model$weights,r$fit$model$weights),
      identical(refit$prediction,r$fit$prediction))
  }
  cat('PASS exact pre-change training weights, fitted predictions, state, candidate scores and old saved settings\n')
}
