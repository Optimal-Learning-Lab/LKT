library(LKT)
library(data.table)
f <- function(name)getFromNamespace(name,'LKT')
close <- function(a,b)stopifnot(isTRUE(all.equal(a,b,tolerance=3e-6,check.attributes=FALSE)))
args <- commandArgs(FALSE);file <- args[grepl('^--file=',args)]
directory <- if(length(file))dirname(normalizePath(sub('^--file=','',file[1]))) else getwd()
source(file.path(directory,'fixtures/gru-minimal-settings.R'))

d <- data.frame(Anon.Student.Id=rep(c('s1','s2'),each=8),KC..Default.='k',
  Outcome=rep(c('STUDY','CORRECT','STUDY','INCORRECT','STUDY','CORRECT','INCORRECT','STUDY'),2),
  CF..Time.=rep(seq(0,70,10),2))
d$CF..ansbin. <- as.numeric(d$Outcome=='CORRECT')
d$CF..reltime. <- d$CF..Time.
d$fold <- ifelse(d$Outcome=='STUDY',NA_integer_,1L)
recipe <- list(components=rep('KC..Default.',3),features=c('recencystudy','lineafm','ppe'),fixedpars=rep(.5,5),
  interacts=rep(NA_character_,3),connectors=c('+','+'),interc=FALSE,maketimes=FALSE)
frame <- f('lkt_gru_feature_frame')(d,recipe)$data
stopifnot(identical(frame$Outcome,d$Outcome),all(is.na(frame$CF..ansbin.[d$Outcome=='STUDY'])))
stopifnot(frame$recencystudyKC..Default.[2]>0)
close(frame$lineafmKC..Default.,rep(0:7,2))

if(requireNamespace('torch',quietly=TRUE)&&torch::torch_is_installed()) {
  scored <- which(d$Outcome!='STUDY')
  model <- test_gru_model(hidden_size=4L,head_hidden_sizes=integer(),embedding_dims=2L,
    epochs=1L,validation_fraction=0,early_stopping=FALSE,scheduler='none',
    restore_best=FALSE,batch_size=2L,seed=123L,split_seed=5L,sequence_length=3L,
    time_col='CF..Time.',order_col='CF..Time.',use_lkt_features=TRUE)
  fit <- LKT(d,usefolds=1,components=recipe$components,features=recipe$features,fixedpars=recipe$fixedpars,
    interc=FALSE,model=model,verbose=FALSE)
  stopifnot(identical(fit$newdata$Outcome,d$Outcome),identical(fit$fit_rows,scored),
    all(is.na(fit$newdata$CF..ansbin.[-scored])),all(is.finite(fit$prediction)))
  full <- LKTGRUStep(fit,LKTGRUState(fit),d)
  close(full$predictions,fit$prediction)
  stopifnot(!length(full$state$pending),identical(full$state$history$s1$Outcome,d$Outcome[1:8]))
  split <- LKTGRUStep(fit,LKTGRUState(fit),d[1:3,])
  stopifnot(!length(split$state$pending))
  hidden <- serialize(split$state$hidden,NULL)
  study <- LKTGRUStep(fit,split$state,d[3,,drop=FALSE])
  stopifnot(identical(hidden,serialize(study$state$hidden,NULL)))
  rest <- LKTGRUStep(fit,split$state,d[4:nrow(d),])
  close(c(split$predictions,rest$predictions),full$predictions)
  pending <- d[4,,drop=FALSE];pending$Outcome <- 'PENDING';pending$CF..ansbin. <- NA_real_
  state <- LKTGRUStep(fit,split$state,pending)$state
  stopifnot(length(state$pending)==1L)
  answered <- LKTGRUStep(fit,state,d[4,,drop=FALSE])
  close(answered$predictions,full$predictions[4])
  # A study has no recurrent update: without exposure-sensitive inputs, deleting
  # studies produces the same trained weights and response predictions.
  plain <- test_gru_model(hidden_size=4L,head_hidden_sizes=integer(),embedding_dims=2L,
    epochs=1L,validation_fraction=0,early_stopping=FALSE,scheduler='none',restore_best=FALSE,
    batch_size=2L,seed=123L,split_seed=5L,time_col='CF..Time.',order_col='CF..Time.')
  a <- LKT(d,components='KC..Default.',features='lineafm',model=plain,verbose=FALSE)
  b <- LKT(d[scored,],components='KC..Default.',features='lineafm',model=plain,verbose=FALSE)
  close(a$model$weights,b$model$weights);close(a$prediction[scored],b$prediction)
  close(predict_lkt(fit,d,return_stats=TRUE)$LL,fit$loglike)
  cat('PASS GRU study labels, scored training, frozen study state, pending and split continuation\n')
} else cat('SKIP Torch training: runtime unavailable; engineered outcome preservation passed\n')
