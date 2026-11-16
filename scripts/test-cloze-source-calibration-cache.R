# Cache contract tests use fixture fits only, never scientific model fitting.
a <- commandArgs(TRUE);stopifnot(length(a)==1L);root <- a[1]
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/fixed-cloze-calibration-helpers.R'))
source(file.path(root,'scripts/cloze-source-calibration-cache.R'))
cfg <- jsonlite::fromJSON(file.path(root,'scripts/gru-ground-truth-training-only.json'))
d <- data.frame(Anon.Student.Id=rep(paste0('s',1:3),each=4L),
  KC..Default.=rep(c('i1','i2'),6L),CF..ansbin.=rep(c(0,1,1,0),3L),
  fold=rep(1:3,each=4L),comparison_source_row=1:12)
identity <- cloze_source_identity(d,'fixture-data',cloze_source_request(cfg),cfg$source_seed,lkt_release)
directory <- tempfile('source-cache-tests_');dir.create(directory)
cleanup <- function() {
  resolved <- normalizePath(directory,winslash='/',mustWork=TRUE)
  stopifnot(startsWith(resolved,paste0(normalizePath(tempdir(),winslash='/'),'/')))
  unlink(resolved,recursive=TRUE)
}
calls <- 0L
mock <- function(data,configuration,diagnostics,key,seed) {
  calls <<- calls+1L
  list(newdata=data,prediction=rep(.6,nrow(data)),model=list(unchanged=TRUE),
    coefs=c(a=.4,b=.5),latencymodel=NULL,
    optimizedpars=list(par=(1:8)/10,convergence=0L,value=12))
}
first <- cloze_source_calibrate(directory,identity,d,mock)
stopifnot(calls==1L,all(is.finite(first$configuration$fixedpars)),
  identical(first$nonlinear_parameters,(1:8)/10))
forbidden <- function(...)stop('FITTING MUST NOT OCCUR ON CACHE HIT')
second <- cloze_source_calibrate(directory,identity,d,forbidden)
stopifnot(identical(first,second),calls==1L)
changed_gru <- cfg;changed_gru$seed <- 9L;changed_gru$layers <- 7L
same_identity <- cloze_source_identity(d,'fixture-data',cloze_source_request(changed_gru),
  changed_gru$source_seed,lkt_release)
stopifnot(identical(cloze_source_key(identity),cloze_source_key(same_identity)))
changed_data <- d;changed_data$CF..ansbin.[1] <- 1-changed_data$CF..ansbin.[1]
different <- cloze_source_identity(changed_data,'changed-data',cloze_source_request(cfg),cfg$source_seed,lkt_release)
stopifnot(cloze_source_key(identity)!=cloze_source_key(different))
must_fail <- function(expr)stopifnot(inherits(tryCatch(force(expr),error=function(e)e),'error'))
must_fail(cloze_source_load(directory,different,changed_data))
mixed <- d;mixed$fold[1] <- 3L
must_fail(cloze_source_identity(mixed,'fixture-data',cloze_source_request(cfg),cfg$source_seed,lkt_release))
bad_fit <- first$fit;bad_fit$optimizedpars$convergence <- 1L
must_fail(cloze_source_publish(file.path(directory,'bad'),identity,d,bad_fit,list()))
bad <- identity;bad$configuration$terms$feature[2] <- 'ppe'
stopifnot(cloze_source_key(bad)!=cloze_source_key(identity))
parameters <- jsonlite::fromJSON(file.path(first$directory,'fixed_parameters.json'))
stopifnot(identical(parameters$ppes,(1:4)/10),identical(parameters$ppef,(5:8)/10))
prior <- list(input_sha256=c(data=identity$data_sha256),calibration_rows=identity$training_rows,
  source_configuration=identity$configuration,configuration=list(source_seed=identity$seed),
  release=as.list(lkt_release),R=identity$R,packages=identity$packages)
prior_path <- file.path(directory,'prior.rds');record_path <- file.path(directory,'record.rds')
saveRDS(prior,prior_path)
saveRDS(list(fit=first$fit,protocol_sha256=cloze_source_file_hash(prior_path)),record_path)
imported <- cloze_source_import(file.path(directory,'imported'),identity,d,record_path,prior_path)
stopifnot(identical(imported$nonlinear_parameters,first$nonlinear_parameters),calls==1L)
prior$calibration_rows <- seq_len(nrow(d));saveRDS(prior,prior_path)
saveRDS(list(fit=first$fit,protocol_sha256=cloze_source_file_hash(prior_path)),record_path)
must_fail(cloze_source_import(file.path(directory,'wrong-fold'),identity,d,record_path,prior_path))
# A corrupted existing artifact must fail, never trigger an optimizer call.
writeLines('corrupt',file.path(first$directory,'fixed_parameters.json'))
must_fail(cloze_source_calibrate(directory,identity,d,forbidden))
cleanup()
cat('PASS source cache: one calibration, zero cache-hit/import fits, fixed values, GRU-independent key, data/fold/model invalidation and corruption rejection\n')
