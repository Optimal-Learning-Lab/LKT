#!/usr/bin/env Rscript
# ROOT REFERENCE STORE ensure|import|verify [SOURCE_RECORD SOURCE_PROTOCOL]
# This is the explicit calibration step; no GRU or Wald fitting occurs here.
a <- commandArgs(TRUE)
if(!length(a)%in%c(4L,6L))stop('Usage: ROOT REFERENCE STORE ensure|import|verify [RECORD PROTOCOL]')
root <- normalizePath(a[1],winslash='/',mustWork=TRUE)
reference <- normalizePath(a[2],winslash='/',mustWork=TRUE)
store <- normalizePath(a[3],winslash='/',mustWork=FALSE)
action <- match.arg(a[4],c('ensure','import','verify'))
stopifnot((action=='import')==(length(a)==6L))
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-source-calibration.R'))
source(file.path(root,'scripts/fixed-cloze-calibration-helpers.R'))
source(file.path(root,'scripts/cloze-source-calibration-cache.R'))
cfg <- jsonlite::fromJSON(file.path(root,'scripts/gru-ground-truth-training-only.json'))
data_path <- file.path(reference,'data/prepared_data.rds')
data_sha <- cloze_source_file_hash(data_path)
stopifnot(data_sha==cfg$data_sha256)
data <- as.data.frame(readRDS(data_path))
identity <- cloze_source_identity(data,data_sha,cloze_source_request(cfg),cfg$source_seed,lkt_release)
before <- if(file.exists(file.path(cloze_source_directory(store,identity),'receipt.json')))
  cloze_source_file_hash(file.path(cloze_source_directory(store,identity),'receipt.json')) else NULL
result <- switch(action,ensure=cloze_source_calibrate(store,identity,data),
  import=cloze_source_import(store,identity,data,a[5],a[6]),
  verify=cloze_source_load(store,identity,data))
if(!is.null(before))stopifnot(result$receipt_sha256==before)
cat(jsonlite::toJSON(list(passed=TRUE,action=action,directory=result$directory,key=result$key,
  reused=!is.null(before),all_parameters_fixed=all(is.finite(result$nonlinear_parameters)),
  ppes=unname(result$nonlinear_parameters[1:4]),ppef=unname(result$nonlinear_parameters[5:8]),
  receipt_sha256=result$receipt_sha256),auto_unbox=TRUE,pretty=TRUE,digits=17),'\n')
