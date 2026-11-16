# Pure preparation contract checks: no neural training and no research-data writes.
a <- commandArgs(TRUE);stopifnot(length(a)==1L)
source(file.path(a[1],'scripts/gru-ground-truth-eight-input-helpers.R'))
d <- data.frame(Anon.Student.Id=c('s1','s1','s2','s2'),KC..Default.=c('i1','i2','i1','i2'),
  CF..ansbin.=c(0,1,1,0),fold=c(1L,1L,3L,3L))
students <- c(s1=-.3,s2=.8);items <- c(i1=.4,i2=-.7)
x <- gt8_attach_traits(d,students,items)
stopifnot(nrow(x)==nrow(d),identical(x[names(d)],d),
  identical(x$random_intercept,c(-.3,-.3,.8,.8)),identical(x$kc_intercept,c(.4,-.7,.4,-.7)))
flipped <- d;flipped$CF..ansbin. <- 1-flipped$CF..ansbin.
y <- gt8_attach_traits(flipped,students,items)
stopifnot(identical(x$random_intercept,y$random_intercept),identical(x$kc_intercept,y$kc_intercept),
  identical(gt8_attach_traits(d[4:1,],students,items)$random_intercept,rev(x$random_intercept)),
  inherits(tryCatch(gt8_attach_traits(d,students['s1'],items),error=identity),'error'))
cat('PASS supplied intercepts independent of answers and ordering; no row removal; missing inputs fail\n')

# The selected policy is explicit: only held-out students get the training prior.
d <- data.frame(Anon.Student.Id=rep(paste0('s',1:6),each=4L),
  KC..Default.=rep(c('i1','i2'),12L),KC..Cluster.=rep(c('k1','k2'),12L),
  CF..Time.=rep(as.numeric(1:4),6L),CF..ansbin.=rep(c(0,1,1,0),6L),
  Duration..sec.=rep(1,24),fold=rep(c(1L,1L,2L,2L,3L,3L),each=4L))
trained <- c(s1=-.3,s2=.8)
attached <- gt8_attach_training_traits(d,trained,items)
stopifnot(attached$prior$mean==mean(trained),
  all(attached$data$random_intercept[d$fold!=1L]==mean(trained)),
  identical(attached$data$random_intercept[d$fold==1L],rep(unname(trained),each=4L)))
poisoned <- d;poisoned$CF..ansbin.[d$fold!=1L] <- 1-poisoned$CF..ansbin.[d$fold!=1L]
p <- gt8_attach_training_traits(poisoned,trained,items)
stopifnot(identical(attached$data[c('random_intercept','kc_intercept')],p$data[c('random_intercept','kc_intercept')]),
  inherits(tryCatch(gt8_attach_training_traits(d,c(trained,s3=99),items),error=identity),'error'))
root <- a[1];source(file.path(root,'scripts/load-current-lkt.R'))
s <- LKT::GRUModel(categorical_cols=c('KC..Default.','KC..Cluster.'),
  numeric_cols=c('random_intercept','kc_intercept'),embedding_adaptation='per_student',
  adaptive_embedding_cols=c('KC..Default.','KC..Cluster.'),
  time_feature_cols=c('gru_item_age_seconds','gru_mean_prior_spacing_seconds'),
  time_missing_cols='gru_mean_prior_spacing_seconds')$settings
r <- list(components=c('Anon.Student.Id','KC..Default.','KC..Default.'),
  features=c('logitdec','ppes','logitdec'),fixedpars=c(.8,.3,.4,.1,.7,.6),
  interc=FALSE,interacts=rep(NA_character_,3L),connectors=c('+','+'),maketimes=FALSE)
data <- LKT::LKTGRUTimeFeatures(attached$data)
check <- gt8_preprocessing_leakage_check(data,r,s)
stopifnot(check$passed)
cat('PASS training-only ability prior, no held-out intercept lookup, held-out-response invariance, training-only numeric/skill preprocessing\n')

# Every optimizer return is preserved before a failed gate; no silent retry.
source(file.path(root,'scripts/gru-source-calibration.R'))
directory <- tempfile('source-calibration-test_');dir.create(directory)
on.exit_cleanup <- function() {
  resolved <- normalizePath(directory,winslash='/',mustWork=TRUE)
  allowed <- paste0(normalizePath(tempdir(),winslash='/',mustWork=TRUE),'/')
  stopifnot(startsWith(resolved,allowed))
  unlink(directory,recursive=TRUE)
}
config <- list(fixedpars=rep(NA_real_,8L),options=list(maxitv=1000L,factrv=10000000,epsilon=1e-12))
train <- data[data$fold==1L,,drop=FALSE]
train$comparison_source_row <- seq_len(nrow(train))
calls <- 0L
mock <- function(d,c) {
  calls <<- calls+1L
  list(newdata=d,prediction=rep(.6,nrow(d)),optimizedpars=list(par=(1:8)/10,
    convergence=1L,message='NEW_X',value=12,counts=c('function'=101L,gradient=100L)))
}
failed <- tryCatch(gru_source_calibration(train,config,file.path(directory,'limited'),
  'fixture',25L,mock),error=identity)
stopifnot(inherits(failed,'error'),calls==1L,
  grepl('optimizer code 1',conditionMessage(failed)),
  file.exists(file.path(directory,'limited/source_calibration.rds')))
saved <- readRDS(file.path(directory,'limited/source_calibration.rds'))
stopifnot(saved$report$convergence_code==1L,!saved$report$passed,
  identical(saved$fit$optimizedpars$message,'NEW_X'))
again <- tryCatch(gru_source_calibration(train,config,file.path(directory,'limited'),
  'fixture',25L,mock),error=identity)
stopifnot(inherits(again,'error'),calls==1L)
mock_ok <- function(d,c) {x <- mock(d,c);x$optimizedpars$convergence <- 0L;x}
ok <- gru_source_calibration(train,config,file.path(directory,'converged'),'fixture',25L,mock_ok)
stopifnot(ok$optimizedpars$convergence==0L,calls==2L)
cached <- gru_source_calibration(train,config,file.path(directory,'converged'),'fixture',25L,mock_ok)
stopifnot(identical(cached,ok),calls==2L)
wrong <- tryCatch(gru_source_calibration(train,config,file.path(directory,'converged'),
  'different-protocol',25L,mock_ok),error=identity)
stopifnot(inherits(wrong,'error'),calls==2L)
mock_line_search <- function(d,c) {
  x <- mock(d,c);x$optimizedpars$convergence <- 52L
  x$optimizedpars$message <- 'ERROR: ABNORMAL_TERMINATION_IN_LNSRCH'
  warning('fixture calibration warning');x
}
line_search <- suppressWarnings(tryCatch(gru_source_calibration(train,config,
  file.path(directory,'line-search'),'fixture',25L,mock_line_search),error=identity))
line_saved <- readRDS(file.path(directory,'line-search/source_calibration.rds'))
stopifnot(inherits(line_search,'error'),line_saved$report$convergence_code==52L,
  identical(line_saved$report$warnings,'fixture calibration warning'),!line_saved$report$passed)
mock_error <- function(d,c)stop('fixture fitting error')
broken <- tryCatch(gru_source_calibration(train,config,file.path(directory,'error'),
  'fixture',25L,mock_error),error=identity)
stopifnot(inherits(broken,'error'),
  readRDS(file.path(directory,'error/source_calibration.rds'))$report$error$message=='fixture fitting error')
on.exit_cleanup()
cat('PASS failed calibration diagnostic persistence, strict acceptance, cached continuation and no automatic retry\n')
