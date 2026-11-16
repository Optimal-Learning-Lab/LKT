#!/usr/bin/env Rscript
# ROOT PROJECT DATA EXISTING_FULL_GT_RECORD OUTPUT plan|control|finish
# Reuses the full-data eight-input study's initial LKT accuracy fit. Never refits it.
a <- commandArgs(TRUE);stopifnot(length(a)==6L)
root <- normalizePath(a[1],winslash='/',mustWork=TRUE)
project <- normalizePath(a[2],winslash='/',mustWork=TRUE);setwd(project)
data_path <- normalizePath(a[3],winslash='/',mustWork=TRUE)
source_path <- normalizePath(a[4],winslash='/',mustWork=FALSE)
out <- normalizePath(a[5],winslash='/',mustWork=FALSE)
action <- match.arg(a[6],c('plan','control','finish'))
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-encoder-study.R'))
source(file.path(root,'scripts/fixed-cloze-calibration-helpers.R'))
source('workflows/simulation/shared/model_roles.R')
cfg <- jsonlite::fromJSON(file.path(root,'scripts/fixed-cloze-calibration.json'))
d <- as.data.frame(readRDS(data_path));source_protocol <- file.path(dirname(dirname(source_path)),'protocol.rds')
stopifnot(study_hash(data_path)==cfg$data_sha256,nrow(d)==55122L,
  length(unique(d$Anon.Student.Id))==478L,length(unique(d$KC..Default.))==144L,
  file.exists(source_protocol),
  readRDS(source_protocol)$input_sha256[['data']]==cfg$data_sha256)
scripts <- c('scripts/fixed-cloze-calibration.R','scripts/fixed-cloze-calibration-helpers.R',
  'scripts/fixed-cloze-calibration.json','scripts/load-current-lkt.R','scripts/gru-encoder-study.R')
protocol <- list(schema=1L,configuration=cfg,data_sha256=study_hash(data_path),
  source_accuracy_record=source_path,source_protocol_sha256=study_hash(source_protocol),
  source_sha256=setNames(vapply(file.path(root,scripts),study_hash,character(1)),scripts),
  release=as.list(lkt_release))
if(action=='plan') {
  cat(jsonlite::toJSON(list(configuration=cfg,rows=nrow(d),students=478L,
    accuracy_refits_after_calibration=0L,wald_calibrations=1L,
    existing_full_ground_truth_available=file.exists(source_path),read_only=TRUE),
    auto_unbox=TRUE,pretty=TRUE),'\n');quit(save='no')
}
if(basename(dirname(out))!='LKT-outputs')stop('Use a named LKT-outputs experiment')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
protocol_path <- file.path(out,'protocol.rds');study_write_rds(protocol,protocol_path)
control_path <- file.path(out,'control.rds');ground_truth_path <- file.path(out,'ground_truth.rds')
if(action=='control') {
  if(!file.exists(control_path)) {
    cat('START one full-data control accuracy calibration\n');flush.console();set.seed(cfg$seed)
    c <- cfg$control
    fit <- LKT::LKT(data=data.table::as.data.table(data.table::copy(d)),usefolds=NA_real_,
      components=c$components,features=c$features,interc=TRUE,fixedpars=rep(NA_real_,6L),
      epsilon=cfg$epsilon,factrv=cfg$factrv,maxitv=cfg$maxitv,verbose=FALSE)
    fit$configuration <- cloze_fixed_configuration(fit,c$components,c$features)
    study_write_rds(list(schema=1L,fit=fit,protocol_sha256=study_hash(protocol_path)),control_path)
  }
  stopifnot(readRDS(control_path)$protocol_sha256==study_hash(protocol_path))
  cat('COMPLETE full-data control; nonlinear parameters now fixed\n');quit(save='no')
}
stopifnot(file.exists(control_path),file.exists(source_path))
control <- readRDS(control_path);stopifnot(control$protocol_sha256==study_hash(protocol_path))
saved <- readRDS(source_path)
stopifnot(saved$protocol_sha256==study_hash(source_protocol))
accuracy <- saved$fit;g <- cfg$ground_truth
source_arguments <- getFromNamespace('lkt_config_arguments','LKT')(accuracy$configuration)
stopifnot(nrow(accuracy$newdata)==nrow(d),is.null(accuracy$latencymodel),
  identical(source_arguments$components,g$components),
  identical(source_arguments$features,g$features),
  identical(accuracy$newdata$comparison_source_row,d$comparison_source_row),
  identical(accuracy$newdata$CF..ansbin.,d$CF..ansbin.),
  length(accuracy$optimizedpars$par)==8L,accuracy$optimizedpars$convergence==0L)
fixed_gt <- cloze_fixed_configuration(accuracy,g$components,g$features,'Wald')
if(!file.exists(ground_truth_path)) {
  cat('START single Wald calibration on final accuracy predictions; no LKT accuracy refit\n');flush.console()
  set.seed(cfg$seed)
  real <- cloze_append_wald_once(accuracy,cfg$wald_contamination_weight)
  real$configuration <- fixed_gt;pf_validate_source(real)
  study_write_rds(list(schema=1L,fit=real,protocol_sha256=study_hash(protocol_path),
    preliminary_source_sha256=study_hash(source_path),wald_calibrations=1L,
    accuracy_refits_after_calibration=0L),ground_truth_path)
}
real <- readRDS(ground_truth_path)
stopifnot(real$protocol_sha256==study_hash(protocol_path),real$wald_calibrations==1L,
  identical(real$fit$prediction,accuracy$prediction),identical(real$fit$coefs,accuracy$coefs))
pf_validate_source(real$fit)
defaults <- list(schema=1L,dataset='Cloze',data_sha256=cfg$data_sha256,rows=nrow(d),
  ground_truth=unclass(fixed_gt),control=unclass(control$fit$configuration),
  accuracy_source_sha256=study_hash(source_path),ground_truth_sha256=study_hash(ground_truth_path),
  control_sha256=study_hash(control_path),nonlinear_parameters_fixed=TRUE,
  accuracy_refits_after_calibration=0L,wald_calibrations=1L)
jsonlite::write_json(defaults,file.path(out,'fixed_defaults.json'),auto_unbox=TRUE,pretty=TRUE,na='null',digits=17)
jsonlite::write_json(list(passed=TRUE,schema=1L,data_sha256=cfg$data_sha256,
  protocol_sha256=study_hash(protocol_path),ground_truth_sha256=study_hash(ground_truth_path),
  control_sha256=study_hash(control_path),defaults_sha256=study_hash(file.path(out,'fixed_defaults.json')),
  full_data_calibration=TRUE,rows=nrow(d),students=478L,nonlinear_parameters_fixed=TRUE,
  accuracy_refits_after_calibration=0L,wald_calibrations=1L),file.path(out,'completed.json'),auto_unbox=TRUE,pretty=TRUE)
cat('COMPLETE frozen full-Cloze models and nonlinear defaults\n')
