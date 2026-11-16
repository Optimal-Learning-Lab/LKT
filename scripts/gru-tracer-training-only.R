#!/usr/bin/env Rscript
# Canonical family: tracer_six. See ../README-GRU.md and gru-variants.json.
# ROOT REFERENCE CONTROL_RECORD OUTPUT plan|prepare|smoke|fit|report
a <- commandArgs(TRUE);stopifnot(length(a)==5L)
root <- normalizePath(a[1],winslash='/',mustWork=TRUE)
reference <- normalizePath(a[2],winslash='/',mustWork=TRUE)
control_path <- normalizePath(a[3],winslash='/',mustWork=TRUE)
out <- normalizePath(a[4],winslash='/',mustWork=FALSE)
action <- match.arg(a[5],c('plan','prepare','smoke','fit','report'))
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-encoder-study.R'))
source(file.path(root,'scripts/gru-tracer-training-only-helpers.R'))
cfg <- jsonlite::fromJSON(file.path(root,'scripts/gru-tracer-training-only.json'))
paths <- c(data=file.path(reference,'data/prepared_data.rds'),study=file.path(reference,'study.rds'),
  control=control_path,control_protocol=file.path(dirname(control_path),'protocol.rds'))
hashes <- vapply(paths,study_hash,character(1))
stopifnot(hashes[['data']]==cfg$data_sha256,hashes[['study']]==cfg$study_sha256,
  hashes[['control']]==cfg$history_control_sha256,
  hashes[['control_protocol']]==cfg$history_control_protocol_sha256)
d <- as.data.frame(readRDS(paths[['data']]));old <- readRDS(paths[['study']])
control <- readRDS(control_path);prior <- readRDS(paths[['control_protocol']])
stopifnot(control$protocol_sha256==hashes[['control_protocol']],
  prior$input_sha256[['data']]==hashes[['data']],nrow(d)==55122L,
  length(unique(d$Anon.Student.Id))==478L,
  identical(prior$comparison$control$features,old$baseline$features),
  identical(prior$comparison$control$components,old$baseline$components))
calibration_check <- tracer6_calibration_check(d,control,prior)
stopifnot(calibration_check$calibration_rows==35125L,calibration_check$calibration_students==305L,
  sum(d$fold==2L)==8895L,sum(d$fold==3L)==11102L)
settings <- old$settings
stopifnot(settings$num_layers==cfg$layers,settings$hidden_size==cfg$hidden_size,
  settings$plasticity==cfg$plasticity,settings$embedding_adaptation==cfg$embedding_adaptation,
  settings$seed==cfg$seed,settings$num_threads==1L,settings$epochs==100L,
  settings$validation_folds==2L,settings$early_stopping,settings$patience==10L,
  !length(settings$numeric_cols),is.null(settings$time_col),
  identical(settings$categorical_cols,c('KC..Default.','KC..Cluster.')),
  identical(settings$adaptive_embedding_cols,settings$categorical_cols))
settings <- do.call(LKT::GRUModel,settings)$settings
recipe <- old$baseline;recipe$fixedpars <- unname(control$fit$optimizedpars$par)
stopifnot(identical(recipe$features,c('logitdec','ppes','logitdec')),!recipe$interc)
for(idx in split(seq_len(nrow(d)),d$Anon.Student.Id))stopifnot(all(diff(d$CF..Time.[idx])>=0))
scripts <- c('scripts/gru-tracer-training-only.R','scripts/gru-tracer-training-only-helpers.R',
  'scripts/gru-tracer-training-only.json','scripts/test-gru-tracer-training-only.R',
  'scripts/gru-encoder-study.R','scripts/load-current-lkt.R',
  'scripts/verify-anonymous-gru-tracer.py','scripts/verify-pfa-gru-metrics.py',
  'scripts/hpc/run-gru-tracer-training-only.slurm')
protocol <- list(schema=1L,configuration=cfg,input_sha256=hashes,
  source_sha256=setNames(vapply(file.path(root,scripts),study_hash,character(1)),scripts),
  settings=settings,history_recipe=recipe,calibration_check=calibration_check,
  release=as.list(lkt_release),training_rows=which(d$fold==1L),
  validation_rows=which(d$fold==2L),test_rows=which(d$fold==3L),
  R=R.version.string,packages=setNames(vapply(c('LKT','torch','Matrix'),function(n)
    as.character(utils::packageVersion(n)),character(1)),c('LKT','torch','Matrix')))
if(action=='plan') {
  cat(jsonlite::toJSON(list(configuration=cfg,calibration=calibration_check,
    nonlinear_parameters=recipe$fixedpars,full_neural_fits=1L,smoke_epochs=1L,
    settings=settings,input_sha256=as.list(hashes),read_only=TRUE),auto_unbox=TRUE,pretty=TRUE),'\n')
  quit(save='no')
}
if(basename(dirname(out))!='LKT-outputs')stop('Use a named experiment under LKT-outputs')
for(folder in c('models','reports','training'))dir.create(file.path(out,folder),recursive=TRUE,showWarnings=FALSE)
protocol_path <- file.path(out,'study.rds');study_write_rds(protocol,protocol_path)
if(action=='prepare') {
  checks <- tracer6_preprocessing_check(d,recipe,settings)
  study_write_rds(list(calibration=calibration_check,preprocessing=checks),
    file.path(out,'reports/leakage_checks.rds'))
  jsonlite::write_json(list(passed=TRUE,calibration=calibration_check,preprocessing=checks,
    protocol_sha256=study_hash(protocol_path)),file.path(out,'prepared.json'),auto_unbox=TRUE,pretty=TRUE)
  cat('PASS training-only nonlinear objective, six inputs, feature isolation, scaling and vocabularies\n')
  quit(save='no')
}
prepared <- jsonlite::fromJSON(file.path(out,'prepared.json'))
stopifnot(prepared$passed,prepared$protocol_sha256==study_hash(protocol_path))
input <- d
model_path <- file.path(out,'models/cv1__anonymous_constant__seed1.rds')
if(action=='smoke') {
  roles <- unique(d[c('Anon.Student.Id','fold')])
  ids <- unlist(lapply(split(roles[[1L]],roles$fold),head,4L),use.names=FALSE)
  idx <- sort(unlist(lapply(split(which(d$Anon.Student.Id%in%ids),
    d$Anon.Student.Id[d$Anon.Student.Id%in%ids]),head,12L),use.names=FALSE))
  input <- d[idx,,drop=FALSE];settings$epochs <- 1L;settings$early_stopping <- FALSE
  settings$restore_best <- FALSE;settings$scheduler <- 'none'
  model_path <- file.path(out,'models/smoke.rds')
}
if(action%in%c('smoke','fit')) {
  if(!file.exists(model_path)) {
    cat('START',action,'training-only six-input tracer',format(Sys.time(),tz='UTC'),'\n');flush.console()
    started <- proc.time()[['elapsed']]
    fit <- LKT::LKT(data=data.table::as.data.table(data.table::copy(input)),usefolds=1L,
      components=recipe$components,features=recipe$features,fixedpars=recipe$fixedpars,interc=FALSE,
      model=do.call(LKT::GRUModel,settings),verbose=TRUE)
    fit$newdata <- NULL;fit$nullmodel <- NULL
    study_write_rds(list(fit=fit,settings=settings,protocol_sha256=study_hash(protocol_path),
      seconds=proc.time()[['elapsed']]-started,job=Sys.getenv('SLURM_JOB_ID'),session=utils::sessionInfo()),model_path)
  }
  saved <- readRDS(model_path);fit <- saved$fit
  stopifnot(saved$protocol_sha256==study_hash(protocol_path),identical(saved$settings,settings),
    identical(fit$fit_rows,which(input$fold==1L)),identical(fit$validation_rows,which(input$fold==2L)),
    identical(fit$evaluation_rows,which(input$fold==3L)),length(fit$prediction)==nrow(input),
    all(is.finite(fit$prediction)),length(fit$model$preprocessing$numeric_names)==6L,
    identical(fit$model$settings$plasticity,'constant'),
    identical(fit$model$settings$embedding_adaptation,'per_student'))
  embeddings <- grep('^embeddings[.]',names(fit$model$weights),value=TRUE)
  stopifnot(length(embeddings)==2L,all(vapply(fit$model$weights[embeddings],function(x)all(x==0),logical(1))))
  if(action=='smoke') {
    study_write_rds(tracer6_runtime_check(fit,input),file.path(out,'reports/smoke_checks.rds'))
    cat('PASS smoke: causal replay, read-only scoring, resets, relabeling and save/load\n');quit(save='no')
  }
}
saved <- readRDS(model_path);fit <- saved$fit
stopifnot(saved$protocol_sha256==study_hash(protocol_path))
checks <- tracer6_runtime_check(fit,d)
study_write_rds(checks,file.path(out,'reports/runtime_checks.rds'))
metrics <- do.call(rbind,lapply(1:3,function(k) {
  idx <- which(d$fold==k);m <- study_metrics(d$CF..ansbin.[idx],fit$prediction[idx])
  data.frame(phase=c('training','validation','heldout')[k],rows=length(idx),
    students=length(unique(d$Anon.Student.Id[idx])),AUC=m[['AUC']],log_loss=m[['log_loss']])
}))
study_write_csv(metrics,file.path(out,'reports/metrics.csv'))
study_write_csv(data.frame(source_row=d$comparison_source_row,student=d$Anon.Student.Id,role=d$fold,
  observed=d$CF..ansbin.,prediction=fit$prediction),file.path(out,'reports/predictions.csv'))
study_write_csv(fit$training_history,file.path(out,'training/history.csv'))
report <- c('# Six-input Cloze tracer: training-only calibration','',cfg$evaluation,'',
  'No ability/easiness intercept inputs, Lasso features, Wald fitting, simulator changes or parameter search.',
  'The already fitted training-only history calibration is reused by hash; no redundant optimization.',
  '',paste('Selected epoch:',fit$selected_epoch,'; trained epochs:',fit$trained_epochs),
  paste('Learned constant output rate:',format(as.numeric(fit$model$weights[['plasticity.eta']]),digits=10)),
  '',paste(capture.output(print(metrics,row.names=FALSE)),collapse='\n'))
writeLines(report,file.path(out,'reports/RESULTS.md'))
jsonlite::write_json(list(passed=TRUE,training_only_calibration=TRUE,numeric_inputs=6L,
  calibration_rows=35125L,calibration_students=305L,protocol_sha256=study_hash(protocol_path),
  model_sha256=study_hash(model_path),selected_epoch=fit$selected_epoch,trained_epochs=fit$trained_epochs,
  runtime_checks=checks,simulations=0L,defaults_changed=FALSE),
  file.path(out,'completed.json'),auto_unbox=TRUE,pretty=TRUE)
print(metrics);cat('COMPLETE training-only six-input tracer\n')
