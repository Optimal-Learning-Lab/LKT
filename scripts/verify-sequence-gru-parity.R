# Capture once before the interface refactor; verify against the candidate build.
args <- commandArgs(TRUE);stopifnot(length(args)==2L,args[1L]%in%c('capture','check'))
library(LKT)
set.seed(619)
d <- data.frame(Anon.Student.Id=rep(paste0('s',1:6),each=7),
  KC..Default.=rep(c('a','b','c'),14),CF..Time.=rep(seq(0,600,100),6),
  CF..ansbin.=rbinom(42,1,.6),ability=rep(seq(-1,1,length.out=6),each=7))
fixtures <- list()
for(mode in c('none','deterministic','gaussian'))for(rich in c(FALSE,TRUE)) {
  settings <- list(epochs=1L,hidden_size=4L,num_layers=2L,head_hidden_sizes=3L,
    embedding_dims=2L,latent_size=3L,inference_hidden_sizes=4L,inference_mode=mode,
    numeric_cols='ability',use_lkt_features=rich,time_col='CF..Time.',
    order_col='CF..Time.',validation_fraction=0,early_stopping=FALSE,seed=6L,
    feature_encoder=if(rich)'mlp' else 'none',encoder_hidden_sizes=3L,encoder_output_size=2L,
    retrieval_heads=if(rich)1L else 0L,retrieval_size=4L,retrieval_window=4L)
  fit <- LKT(d,features='lineafm',components='KC..Default.',verbose=FALSE,model=do.call(GRUModel,settings))
  continued <- LKTGRUStep(fit,LKTGRUState(fit),d[1:7,])
  fixtures[[paste(mode,rich)]] <- list(fit=fit,state=continued$state,predictions=continued$predictions)
}
if(args[1L]=='capture') {
  stopifnot(!file.exists(args[2L]));saveRDS(fixtures,args[2L])
} else {
  old <- readRDS(args[2L])
  for(name in names(old)) {
    a <- old[[name]];b <- fixtures[[name]]
    stopifnot(identical(a$fit$model$weights,b$fit$model$weights),
      identical(a$fit$prediction,b$fit$prediction),identical(a$predictions,b$predictions))
    a$state$model_id <- b$state$model_id
    stopifnot(identical(a$state,b$state))
    restored <- LKTGRUStep(a$fit,LKTGRUState(a$fit),d[1:7,])
    stopifnot(identical(restored$predictions,a$predictions))
  }
}
cat('PASS GRU parity:',args[1L],length(fixtures),'plain/encoder/retrieval/latent cases\n')
