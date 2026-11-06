library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
if (!requireNamespace('torch', quietly=TRUE) || !torch::torch_is_installed()) {
  if (identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true')) stop('Torch runtime is required for this test run.')
  message('GRU neural tests skipped: optional Torch runtime is unavailable.')
} else {
  fail <- function(expr, pattern) {
    error <- tryCatch({force(expr);NULL},error=identity)
    stopifnot(inherits(error,'error'),grepl(pattern,conditionMessage(error),fixed=TRUE))
  }
  close <- function(x,y,tolerance=2e-6)stopifnot(isTRUE(all.equal(x,y,tolerance=tolerance,check.attributes=FALSE)))
  fit <- function(d,settings=list(),features='intercept',components='KC..Default.',...) {
    controls <- utils::modifyList(list(epochs=3L,hidden_size=6L,head_hidden_sizes=5L,
      batch_size=3L,sequence_length=7L,seed=27L,validation_fraction=.2),settings,keep.null=TRUE)
    LKT(d,components=components,features=features,model=do.call(test_gru_model,controls),verbose=FALSE,...)
  }
  set.seed(52)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:10),each=15),
    KC..Default.=rep(c('a','b','c'),50),CF..ansbin.=rbinom(150,1,.6),
    time=rep(seq(0,by=100,length.out=15),10),value=runif(150),weight=runif(150,.5,2))
  d$CF..Time. <- d$CF..reltime. <- d$time
  original <- d
  rng <- .Random.seed;trng <- torch::torch_get_rng_state();threads <- torch::torch_get_num_threads()
  m <- fit(d)
  stopifnot(identical(d,original),identical(rng,.Random.seed),
    isTRUE(torch::torch_equal(trng,torch::torch_get_rng_state())),torch::torch_get_num_threads()==threads,
    nrow(m$coefs)==0L,length(m$prediction)==nrow(d),m$parameter_count>0,
    !length(intersect(m$fit_rows,m$validation_rows)),length(m$training_history$epoch)==3L)
  close(predict_lkt(m,d)$predictions,m$prediction)
  a <- LKTGRUStep(m,LKTGRUState(m),d[1:67,])
  before <- a$state
  b <- LKTGRUStep(m,a$state,d[68:nrow(d),])
  stopifnot(identical(before,a$state))
  close(c(a$predictions,b$predictions),m$prediction)
  stopifnot(is.null(b$state$history)) # Raw continuation does not grow a trial log.
  steps <- LKTGRUState(m);p <- numeric(nrow(d))
  for(i in seq_len(nrow(d))) {out <- LKTGRUStep(m,steps,d[i,,drop=FALSE]);p[i] <- out$predictions;steps <- out$state}
  close(p,m$prediction)
  altered <- d;altered$CF..ansbin.[10:nrow(d)] <- 1-altered$CF..ansbin.[10:nrow(d)]
  close(predict_lkt(m,altered)$predictions[1:10],m$prediction[1:10])
  pending <- d[1,,drop=FALSE];pending$CF..ansbin. <- NULL
  state <- LKTGRUStep(m,LKTGRUState(m),pending)$state
  stopifnot(is.null(state$history),length(state$hidden)==0L)
  fail(LKTGRUStep(m,state,d[2,,drop=FALSE]),'pending')
  resolved <- LKTGRUStep(m,state,d[1,,drop=FALSE]);close(resolved$predictions,m$prediction[1])
  fail(LKTGRUStep(fit(d,list(seed=31)),state,d[1,,drop=FALSE]),'belong')
  cat('PASS causal response ordering, independent histories, continuation, pending trials, RNG/thread restoration\n')

  # Fit/test and fit/validation phase boundaries must precede feature construction.
  d$fold <- ifelse(d$Anon.Student.Id %in% c('s9','s10'),3,1)
  d$fold[d$Anon.Student.Id=='s8'] <- 2
  settings <- list(validation_folds=2,numeric_cols='value',numeric_transforms=c(value='log1p'),
    time_col='time',num_layers=2L,gru_dropout=.1,input_dropout=.1,head_dropout=.1,
    head_hidden_sizes=c(5L,3L),weight_col='weight',positive_weight=1.3,
    scheduler='plateau',scheduler_options=list(patience=0L),optimizer='adam')
  split <- fit(d,settings,usefolds=1)
  changed <- d;changed$CF..ansbin.[changed$fold==3] <- 1-changed$CF..ansbin.[changed$fold==3]
  comparison <- fit(changed,settings,usefolds=1)
  stopifnot(identical(split$model$weights,comparison$model$weights),
    identical(split$model$preprocessing,comparison$model$preprocessing),
    identical(split$selected_epoch,comparison$selected_epoch))
  # Altering validation labels cannot change optimization when checkpoint selection is disabled.
  no_restore <- settings;no_restore$restore_best <- FALSE;no_restore$early_stopping <- FALSE;no_restore$scheduler <- 'none'
  v1 <- fit(d,no_restore,usefolds=1)
  changed <- d;changed$CF..ansbin.[changed$fold==2] <- 1-changed$CF..ansbin.[changed$fold==2]
  v2 <- fit(changed,no_restore,usefolds=1)
  stopifnot(identical(v1$model$weights,v2$model$weights))
  close(split$loglike,sum(dbinom(d$CF..ansbin.[split$fit_rows],1,split$prediction[split$fit_rows],log=TRUE)))
  bad <- d;bad$fold[2] <- 3
  fail(fit(bad,settings,usefolds=1),'interleaved')
  fail(fit(d,settings,usefolds=c(1,2)),'disjoint')
  cat('PASS multilayer/dropout/MLP controls, optimizer/scheduler execution, train/validation/test isolation\n')

  # Learned input preprocessing and both categorical policies.
  onehot <- fit(d,list(categorical_encoding='one_hot',head_hidden_sizes=integer(),
    numeric_cols='value',optimizer='sgd',optimizer_options=list(momentum=.2),dtype='float64',
    history_group_cols=c('Anon.Student.Id','KC..Default.'),gradient_clip=NULL))
  close(predict_lkt(onehot,d)$predictions,onehot$prediction)
  unknown <- d[1,,drop=FALSE];unknown$KC..Default. <- 'unseen'
  fail(predict_lkt(m,unknown),'Unknown GRU category')
  zero <- fit(d,list(unknown_levels='zero'));stopifnot(is.finite(predict_lkt(zero,unknown)$predictions))
  for(activation in c('tanh','gelu'))stopifnot(all(is.finite(fit(d,list(head_activation=activation,epochs=1))$prediction)))
  fail(test_gru_model(num_layers=1,gru_dropout=.1),'requires')
  fail(test_gru_model(optimizer_options=list(made_up=1)),'supported')
  fail(test_gru_model(epochs=0),'epochs')
  fail(test_gru_model(categorical_cols='CF..ansbin.'),'Responses')
  fail(test_gru_model(device='cuda:0'),'Deterministic CUDA')
  if(!torch::cuda_is_available())fail(fit(d,list(device='cuda:0',deterministic=FALSE)),'unavailable')
  fail(fit(d,features='logitdec'),'fixed nonlinear')
  fail(fit(d,dualfit=TRUE),'accuracy-only')
  fail(fit(d,list(use_lkt_features=TRUE),features='numer',components='CF..ansbin.'),'Responses')
  fail(fit(d,list(use_lkt_features=TRUE),features='logitdecevol',fixedpars=.3),'cross-student')
  numeric_only <- fit(d,list(categorical_cols=character(),numeric_cols='value',epochs=1L))
  close(predict_lkt(numeric_only,d)$predictions,numeric_only$prediction)
  stopped <- fit(d,list(min_delta=100,patience=1L,epochs=10L))
  stopifnot(nrow(stopped$training_history)==2L,stopped$stopping_reason=='early_stopping')
  scheduled <- fit(d,list(scheduler='plateau',scheduler_options=list(patience=0L,threshold=100,min_lr=0)))
  stopifnot(tail(scheduled$training_history$learning_rate,1)<scheduled$gru_settings$learning_rate)
  renamed <- d;names(renamed)[names(renamed)=='Anon.Student.Id'] <- 'person'
  custom <- fit(renamed,list(subject_col='person',history_group_cols='person',use_lkt_features=TRUE,
    time_col='time',order_col='time',epochs=1L),features='logitdec',fixedpars=.7)
  close(predict_lkt(custom,renamed)$predictions,custom$prediction)
  cat('PASS one-hot/embeddings, linear/deep heads, grouping, numeric transforms, float64 and explicit errors\n')

  # Fixed engineered features use exactly the same recipe during training and replay.
  eng <- fit(d,list(use_lkt_features=TRUE,time_col='time'),features=c('intercept','logsuc','logitdec','ppe'),
    components=rep('KC..Default.',4),fixedpars=c(.7,.3,.4,.02,.7))
  close(predict_lkt(eng,d)$predictions,eng$prediction)
  altered <- d;altered$CF..ansbin.[10:nrow(d)] <- 1-altered$CF..ansbin.[10:nrow(d)]
  close(predict_lkt(eng,altered)$predictions[1:10],eng$prediction[1:10])
  unanswered <- d[1:8,];unanswered$CF..ansbin.[8] <- NA_real_
  close(predict_lkt(eng,unanswered)$predictions,eng$prediction[1:8])
  a <- LKTGRUStep(eng,LKTGRUState(eng),d[1:67,]);b <- LKTGRUStep(eng,a$state,d[68:nrow(d),])
  close(c(a$predictions,b$predictions),eng$prediction)
  cat('PASS frozen LKT feature recipe, spacing/PPE/decay causality, unanswered-trial feature construction\n')

  # Cross-process RDS model/state persistence, not just an in-process pointer copy.
  payload <- tempfile(fileext='.rds');answer <- tempfile(fileext='.rds');script <- tempfile(fileext='.R')
  saveRDS(list(model=eng,state=a$state,data=d[68:nrow(d),]),payload)
  writeLines(c('library(LKT)',paste0('x<-readRDS(',deparse(payload),')'),
    paste0('saveRDS(LKTGRUStep(x$model,x$state,x$data),',deparse(answer),')')),script)
  status <- system2(file.path(R.home('bin'),if(.Platform$OS.type=='windows')'Rscript.exe' else 'Rscript'),c('--vanilla',shQuote(script)))
  stopifnot(status==0L);close(readRDS(answer)$predictions,b$predictions)
  unlink(c(payload,answer,script))

  # Padding and group batch-size cannot change inference.
  shortened <- d[-c(12:15,50:60),]
  pad <- fit(shortened,list(batch_size=4L,sequence_length=4L))
  close(predict_lkt(pad,shortened)$predictions,pad$prediction)
  reversed <- d[nrow(d):1,];ordered <- fit(reversed,list(order_col='time'))
  close(predict_lkt(ordered,reversed)$predictions,ordered$prediction)
  cat('PASS fresh-process persistence, uneven histories/padding and explicit chronological sorting\n')

  # This synthetic outcome is a student's preceding answer, with a random
  # student-specific initial answer. A history-independent intercept is .5.
  set.seed(73)
  n <- 24L;len <- 16L
  synthetic <- data.frame(Anon.Student.Id=rep(paste0('person',seq_len(n)),each=len),
    KC..Default.='one',CF..ansbin.=rep(rep(c(0,1),n/2),each=len))
  learned <- fit(synthetic,list(epochs=60L,hidden_size=8L,head_hidden_sizes=8L,
    learning_rate=.02,sequence_length=NULL,batch_size=24L,patience=12L))
  hold <- learned$validation_rows
  score <- -mean(dbinom(synthetic$CF..ansbin.[hold],1,learned$prediction[hold],log=TRUE))
  stopifnot(score<.35,tail(learned$training_history$training_loss,1)<learned$training_history$training_loss[1])
  cat('PASS GRU learns a known response-history signal; validation log loss:',score,'\n')
}
