#!/usr/bin/env Rscript
# Canonical family: ground_truth_eight. Known-trait and training-only protocols
# are separate evaluations of this family; see ../README-GRU.md.
# ROOT SHARED_PROJECT REFERENCE CONTROL_RECORD OUTPUT plan|prepare|smoke|fit|report
# A separate conditional ground-truth fit; no simulator or package modification.
a <- commandArgs(TRUE)
if(!length(a)%in%c(6L,7L,8L))stop('Usage: gru-ground-truth-eight-input.R ROOT PROJECT REFERENCE CONTROL OUTPUT ACTION [FIXED_CALIBRATION|--training-only [CALIBRATION_STORE]]')
root <- normalizePath(a[1],winslash='/',mustWork=TRUE)
project <- normalizePath(a[2],winslash='/',mustWork=TRUE)
reference <- normalizePath(a[3],winslash='/',mustWork=TRUE)
control_path <- normalizePath(a[4],winslash='/',mustWork=TRUE)
out <- normalizePath(a[5],winslash='/',mustWork=FALSE)
action <- match.arg(a[6],c('plan','calibrate','prepare','smoke','fit','report'))
training_only <- length(a)>=7L && identical(a[7],'--training-only')
if(length(a)==8L && !training_only)stop('CALIBRATION_STORE applies only to training-only mode')
if(action=='calibrate' && !training_only)stop('Separate calibrate action applies only to training-only mode')
calibration_store <- if(training_only)normalizePath(if(length(a)==8L)a[8] else
  file.path(dirname(root),'LKT-outputs','cloze-source-calibrations'),winslash='/',mustWork=FALSE) else NULL
setwd(project)
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-encoder-study.R'))
source('workflows/simulation/shared/model_roles.R')
source(file.path(root,'scripts/gru-ground-truth-eight-input-helpers.R'))
fixed_calibration <- NULL
if(length(a)==7L && !training_only) {
  source('workflows/simulation/shared/fitting/load_fixed_cloze_calibration.R')
  fixed_calibration <- pf_load_fixed_cloze_calibration(a[7],
    '964b30f32fc04233e5377faf06f553719eea359db15640aec6d202b2e317b10c')
}
cfg_path <- file.path(root,'scripts',if(training_only)'gru-ground-truth-training-only.json' else 'gru-ground-truth-eight-input.json')
cfg <- jsonlite::fromJSON(cfg_path)
paths <- c(data=file.path(reference,'data/prepared_data.rds'),study=file.path(reference,'study.rds'),
  history_control=control_path)
hashes <- vapply(paths,study_hash,character(1))
d <- as.data.frame(readRDS(paths['data']));old <- readRDS(paths['study'])
control <- readRDS(control_path)
comparison_protocol <- file.path(dirname(control_path),'protocol.rds')
if(is.null(fixed_calibration)) {
  stopifnot(file.exists(comparison_protocol),control$protocol_sha256==study_hash(comparison_protocol))
  prior <- readRDS(comparison_protocol)
} else {
  stopifnot(identical(normalizePath(dirname(control_path),winslash='/'),fixed_calibration$directory),
    control$protocol_sha256==study_hash(comparison_protocol))
  prior <- readRDS(comparison_protocol)
}
history_definition <- if(is.null(fixed_calibration))prior$comparison$control else prior$configuration$control
calibration_data_sha256 <- if(is.null(fixed_calibration))prior$input_sha256[['data']] else prior$data_sha256
stopifnot(identical(cfg$schema,1L),cfg$dataset=='Cloze',nrow(d)==55122L,
  length(unique(d$Anon.Student.Id))==478L,length(unique(d$KC..Default.))==144L,
  length(unique(d$KC..Cluster.))==36L,hashes[['data']]==cfg$data_sha256,
  hashes[['study']]==cfg$study_sha256,old$data_sha256==cfg$data_sha256,
  calibration_data_sha256==cfg$data_sha256,
  length(control$fit$optimizedpars$par)==6L,control$fit$optimizedpars$convergence==0L,
  identical(history_definition$features,c('logitdec','ppes','logitdec')),
  identical(history_definition$components,c('Anon.Student.Id','KC..Default.','KC..Default.')))
roles <- unique(d[c('Anon.Student.Id','fold')]);stopifnot(!anyDuplicated(roles[[1L]]))
calibration_rows <- if(training_only)which(d$fold==cfg$training_fold) else seq_len(nrow(d))
if(training_only) {
  stopifnot(is.null(fixed_calibration),length(calibration_rows)==35125L,
    length(unique(d$Anon.Student.Id[calibration_rows]))==305L,
    hashes[['history_control']]==cfg$history_control_sha256,
    study_hash(comparison_protocol)==cfg$history_control_protocol_sha256,
    identical(prior$training_rows,calibration_rows),
    identical(control$fit$newdata$fold,d$fold),
    identical(cfg$heldout_ability_policy,'fixed_unweighted_mean_of_training_student_intercepts'))
  source(file.path(root,'scripts/fixed-cloze-calibration-helpers.R'))
  source(file.path(root,'scripts/gru-source-calibration.R'))
  source(file.path(root,'scripts/cloze-source-calibration-cache.R'))
}
for(idx in split(seq_len(nrow(d)),d$Anon.Student.Id))stopifnot(all(diff(d$CF..Time.[idx])>=0))
settings <- old$settings
stopifnot(settings$num_layers==cfg$layers,settings$hidden_size==cfg$hidden_size,
  settings$plasticity==cfg$plasticity,settings$embedding_adaptation==cfg$embedding_adaptation,
  identical(settings$categorical_cols,c('KC..Default.','KC..Cluster.')),
  identical(settings$adaptive_embedding_cols,settings$categorical_cols),
  !length(settings$numeric_cols),is.null(settings$time_col),settings$seed==cfg$seed)
settings$numeric_cols <- cfg$numeric_cols
settings <- do.call(LKT::GRUModel,settings)$settings
source_config <- if(!is.null(fixed_calibration))fixed_calibration$ground_truth$configuration else
  LKT::lkt_model_config(data.frame(component=cfg$source_components,feature=cfg$source_features),
  intercept=TRUE,backend='LibLinear',latency='none',folds=NA_real_,
  options=list(epsilon=cfg$source_epsilon,factrv=cfg$source_factrv,maxitv=cfg$source_maxitv,verbose=FALSE))
source_calibration <- NULL
if(training_only) {
  source_identity <- cloze_source_identity(d,hashes[['data']],source_config,cfg$source_seed,lkt_release)
  if(action=='calibrate') {
    cached <- cloze_source_calibrate(calibration_store,source_identity,d)
    cat('COMPLETE separate calibration step; fixed artifact:',cached$directory,'\n')
    quit(save='no')
  }
  if(dir.exists(cloze_source_directory(calibration_store,source_identity))) {
    source_calibration <- cloze_source_load(calibration_store,source_identity,d)
    source_config <- source_calibration$configuration
  } else if(action!='plan')stop('Run the separate calibrate action before GRU preparation; store: ',calibration_store)
}
stopifnot(length(source_config$fixedpars)==8L,
  if(!is.null(fixed_calibration)||!is.null(source_calibration))all(is.finite(source_config$fixedpars)) else
    all(is.na(source_config$fixedpars)))
scripts <- c('scripts/gru-ground-truth-eight-input.R','scripts/gru-ground-truth-eight-input-helpers.R',
  'scripts/gru-ground-truth-eight-input.json','scripts/gru-encoder-study.R','scripts/load-current-lkt.R')
if(training_only)scripts <- c(setdiff(scripts,'scripts/gru-ground-truth-eight-input.json'),
  'scripts/gru-ground-truth-training-only.json','scripts/fixed-cloze-calibration-helpers.R',
  'scripts/gru-source-calibration.R','scripts/cloze-source-calibration-cache.R')
protocol <- list(schema=1L,configuration=cfg,input_sha256=hashes,
  history_control_protocol_sha256=study_hash(comparison_protocol),source_configuration=source_config,
  source_sha256=setNames(vapply(file.path(root,scripts),study_hash,character(1)),scripts),
  settings=settings,history_recipe=old$baseline,calibration_rows=calibration_rows,
  fixed_calibration=if(is.null(fixed_calibration))NULL else list(directory=fixed_calibration$directory,
    receipt_sha256=fixed_calibration$receipt_sha256),
  history_parameters=unname(control$fit$optimizedpars$par),release=as.list(lkt_release),
  R=R.version.string,packages=setNames(vapply(c('LKT','torch','Matrix'),function(n)
    as.character(utils::packageVersion(n)),character(1)),c('LKT','torch','Matrix')))
protocol$history_recipe$fixedpars <- protocol$history_parameters
if(training_only && !is.null(source_calibration))protocol$source_calibration <-
  source_calibration[c('key','directory','receipt_sha256','model_sha256')]
if(action=='plan') {
  cat(jsonlite::toJSON(list(configuration=cfg,rows=nrow(d),students=nrow(roles),
    students_by_fold=as.list(table(roles$fold)),settings=settings,
    source_fit=if(training_only)'Separate cached training-only calibration; zero LKT fits in GRU preparation/training' else
      if(is.null(fixed_calibration))'One full-Cloze PPES/PPEF LKT accuracy fit; no latency refit' else
      'Reuse frozen full-Cloze LKT calibration; no accuracy or nonlinear refit',
    full_neural_fits=1L,smoke_epochs=1L,simulations=0L,source_nonlinear_parameters=8L,
    calibration_required=training_only && is.null(source_calibration),
    calibration_store=calibration_store,nonlinear_fits_in_neural_steps=0L,
    input_sha256=as.list(hashes),read_only=TRUE),auto_unbox=TRUE,pretty=TRUE),'\n')
  quit(save='no')
}
if(!identical(basename(dirname(out)),'LKT-outputs'))stop('Use a named experiment under LKT-outputs')
dir.create(out,recursive=TRUE,showWarnings=FALSE)
for(folder in c('data','models','reports','training'))dir.create(file.path(out,folder),showWarnings=FALSE)
protocol_path <- file.path(out,'protocol.rds');study_write_rds(protocol,protocol_path)
atomic <- function(x,path) {
  if(file.exists(path))stop('Refusing to overwrite artifact: ',path)
  tmp <- tempfile('stage_',dirname(path),'.tmp');on.exit(if(file.exists(tmp))unlink(tmp))
  saveRDS(x,tmp);if(!file.rename(tmp,path))stop('Could not commit artifact: ',path)
}
source_path <- file.path(out,'models/cloze_lkt_traits.rds')
data_path <- file.path(out,'data/eight_input_data.rds')
if(action=='prepare') {
  if(!file.exists(source_path)) {
    cat(if(training_only)'REUSE saved training-only LKT calibration; zero fitting' else
      if(is.null(fixed_calibration))'START full Cloze LKT trait calibration' else
      'REUSE frozen full Cloze LKT traits; no accuracy or nonlinear fit',format(Sys.time(),tz='UTC'),'\n');flush.console()
    set.seed(cfg$source_seed);started <- proc.time()[['elapsed']]
    fit <- if(training_only)source_calibration$fit else
      if(is.null(fixed_calibration))LKT::fit_lkt_config(d[calibration_rows,,drop=FALSE],source_config) else fixed_calibration$ground_truth
    stopifnot(fit$optimizedpars$convergence==0L,length(fit$optimizedpars$par)==8L,
      nrow(fit$newdata)==length(calibration_rows),all(is.finite(fit$prediction)))
    if(training_only) {
      stopifnot(identical(fit$newdata$comparison_source_row,d$comparison_source_row[calibration_rows]),
        all(fit$newdata$fold==1L),!any(fit$model_specification[[1L]]$component=='KC..Cluster.'))
      fit$configuration <- cloze_fixed_configuration(fit,cfg$source_components,cfg$source_features)
    }
    source_record <- list(fit=fit,protocol_sha256=study_hash(protocol_path),
      seconds=proc.time()[['elapsed']]-started)
    if(training_only)source_record$source_calibration <- protocol$source_calibration
    atomic(source_record,source_path)
    cat(if(training_only)'COMPLETE reuse of fixed training-only LKT calibration\n' else
      'COMPLETE full Cloze LKT trait calibration\n');flush.console()
  }
  saved <- readRDS(source_path);stopifnot(saved$protocol_sha256==study_hash(protocol_path))
  fit <- saved$fit
  if(training_only)stopifnot(nrow(fit$newdata)==length(calibration_rows),
    identical(fit$newdata$comparison_source_row,d$comparison_source_row[calibration_rows]),
    all(is.finite(fit$configuration$fixedpars)),length(fit$configuration$fixedpars)==8L)
  students <- pf_coefficients(fit,'Anon.Student.Id');items <- pf_coefficients(fit,'KC..Default.')
  if(training_only) {
    attached <- gt8_attach_training_traits(d,students,items,cfg$training_fold)
    prepared <- attached$data
    study_write_rds(attached$prior,file.path(out,'data/ability_prior.rds'))
    audit <- gt8_preprocessing_leakage_check(prepared,protocol$history_recipe,settings)
    study_write_rds(audit,file.path(out,'reports/preprocessing_leakage_checks.rds'))
  } else prepared <- gt8_attach_traits(d,students,items)
  stopifnot(nrow(prepared)==55122L,identical(prepared$comparison_source_row,d$comparison_source_row),
    identical(prepared$fold,d$fold),identical(prepared$CF..ansbin.,d$CF..ansbin.))
  study_write_rds(prepared,data_path)
  study_write_csv(data.frame(student=names(students),intercept=unname(students)),file.path(out,'data/student_intercepts.csv'))
  study_write_csv(data.frame(item=names(items),intercept=unname(items),difficulty=-unname(items)),file.path(out,'data/item_intercepts.csv'))
  study_write_csv(as.data.frame(fit$model_specification[[1L]]),file.path(out,'reports/lkt_source_coefficients.csv'))
  prepared_receipt <- list(passed=TRUE,rows=nrow(prepared),student_intercepts=length(students),
    calibration_rows=length(calibration_rows),training_only=training_only,
    heldout_ability_prior=if(training_only)attached$prior else NULL,
    item_intercepts=length(items),prefix_rows_removed=0L,
    source_sha256=study_hash(source_path),data_sha256=study_hash(data_path),
    protocol_sha256=study_hash(protocol_path))
  if(training_only)prepared_receipt$source_calibration <- protocol$source_calibration
  jsonlite::write_json(prepared_receipt,file.path(out,'prepared.json'),auto_unbox=TRUE,pretty=TRUE)
  quit(save='no')
}
prepared <- jsonlite::fromJSON(file.path(out,'prepared.json'))
stopifnot(isTRUE(prepared$passed),prepared$protocol_sha256==study_hash(protocol_path),
  prepared$source_sha256==study_hash(source_path),prepared$data_sha256==study_hash(data_path))
input <- readRDS(data_path)
model_path <- file.path(out,'models/eight_input_ground_truth.rds')
if(action=='smoke') {
  ids <- unlist(lapply(split(roles[[1L]],roles$fold),head,4L),use.names=FALSE)
  idx <- sort(unlist(lapply(split(which(d$Anon.Student.Id%in%ids),d$Anon.Student.Id[d$Anon.Student.Id%in%ids]),head,12L),use.names=FALSE))
  input <- input[idx,,drop=FALSE];settings$epochs <- 1L;settings$early_stopping <- FALSE
  settings$restore_best <- FALSE;settings$scheduler <- 'none'
  model_path <- file.path(out,'models/smoke.rds')
}
if(action%in%c('smoke','fit')) {
  if(!file.exists(model_path)) {
    cat('START',action,'eight-input ground truth',format(Sys.time(),tz='UTC'),'\n');flush.console()
    started <- proc.time()[['elapsed']]
    fit <- LKT::LKT(data=data.table::as.data.table(data.table::copy(input)),usefolds=1L,
      components=protocol$history_recipe$components,features=protocol$history_recipe$features,
      fixedpars=protocol$history_parameters,interc=FALSE,
      model=do.call(LKT::GRUModel,settings),verbose=TRUE)
    fit$newdata <- NULL;fit$nullmodel <- NULL
    atomic(list(fit=fit,settings=settings,protocol_sha256=study_hash(protocol_path),
      seconds=proc.time()[['elapsed']]-started,job=Sys.getenv('SLURM_JOB_ID'),session=utils::sessionInfo()),model_path)
  }
  saved <- readRDS(model_path);fit <- saved$fit
  stopifnot(saved$protocol_sha256==study_hash(protocol_path),identical(saved$settings,settings),
    identical(fit$fit_rows,which(input$fold==1L)),identical(fit$validation_rows,which(input$fold==2L)),
    identical(fit$evaluation_rows,which(input$fold==3L)),length(fit$prediction)==nrow(input),
    all(is.finite(fit$prediction)),length(fit$model$preprocessing$numeric_names)==8L,
    all(paste0('raw:',cfg$numeric_cols)%in%fit$model$preprocessing$numeric_names),
    identical(fit$model$settings$plasticity,'constant'),
    identical(fit$model$settings$embedding_adaptation,'per_student'))
  embeddings <- grep('^embeddings[.]',names(fit$model$weights),value=TRUE)
  stopifnot(length(embeddings)==2L,all(vapply(fit$model$weights[embeddings],function(x)all(x==0),logical(1))))
  if(action=='smoke') {
    checks <- gt8_runtime_checks(fit,input)
    study_write_rds(checks,file.path(out,'reports/smoke_checks.rds'))
    cat('PASS eight inputs, both adaptive tables zero, scoring/replay/chunking/renaming/resets/trait controls\n')
    quit(save='no')
  }
}
saved <- readRDS(model_path);fit <- saved$fit
stopifnot(saved$protocol_sha256==study_hash(protocol_path))
checks <- gt8_runtime_checks(fit,input)
study_write_rds(checks,file.path(out,'reports/runtime_checks.rds'))
metrics <- do.call(rbind,lapply(1:3,function(fold) {
  idx <- which(input$fold==fold);m <- study_metrics(input$CF..ansbin.[idx],fit$prediction[idx])
  data.frame(phase=c('training','validation','test')[fold],rows=length(idx),
    students=length(unique(input$Anon.Student.Id[idx])),AUC=m[['AUC']],log_loss=m[['log_loss']],
    interpretation=if(training_only)'Training-only upstream calibration; held-out students use training-population ability prior' else
      'Conditional on full-Cloze LKT trait calibration')
}))
study_write_csv(metrics,file.path(out,'reports/metrics.csv'))
study_write_csv(fit$training_history,file.path(out,'training/history.csv'))
study_write_csv(data.frame(source_row=input$comparison_source_row,student=input$Anon.Student.Id,
  fold=input$fold,answer=input$CF..ansbin.,prediction=fit$prediction,
  student_intercept=input$random_intercept,item_intercept=input$kc_intercept),file.path(out,'reports/predictions.csv'))
rates <- as.numeric(fit$model$weights[['plasticity.eta']])
report <- c('# Eight-input Cloze ground-truth GRU','',cfg$evaluation,'',
  if(training_only)'LKT ability/easiness calibration uses training students only. Held-out students use a fixed training-population mean ability input. Both item and skill vectors reset per student. No prefix estimation or removed trials.' else
    'The LKT student/item intercepts are supplied unchanged from a complete Cloze fit. No prefix estimation and no removed trials.',
  'The GRU learns their effects through its numeric encoder and recurrent/prediction paths. They are not fixed output offsets.',
  'All local item/skill embeddings, recurrent memory, retrieval and constant-rate output adjustments reset at each student boundary during training and evaluation.',
  'Shared neural weights remain fixed during runtime replay. No simulator integration or default change.',
  '',paste('Selected epoch:',fit$selected_epoch,'; trained epochs:',fit$trained_epochs),
  paste('Learned constant output rate:',format(rates,digits=10)),'',
  paste(capture.output(print(metrics,row.names=FALSE)),collapse='\n'))
writeLines(report,file.path(out,'reports/RESULTS.md'))
html <- c('<!doctype html><html><head><meta charset="utf-8"><title>Eight-input Cloze GRU</title></head><body>',
  '<h1>Eight-input Cloze ground-truth GRU</h1>',paste0('<p>',cfg$evaluation,'</p>'),
  if(training_only)'<p>Training-only LKT traits; held-out ability uses training-population prior; no removed trials; item/skill vectors reset per student.</p>' else
    '<p>Complete LKT trait inputs; no removed trials; student-local resets; constant output learning.</p>',
  '<table border="1"><tr>',paste0('<th>',names(metrics)[1:5],'</th>',collapse=''),'</tr>',
  apply(metrics[1:5],1,function(row)paste0('<tr>',paste0('<td>',row,'</td>',collapse=''),'</tr>')),
  '</table>',paste0('<p>Selected epoch: ',fit$selected_epoch,'; trained: ',fit$trained_epochs,
    '; constant output rate: ',format(rates,digits=10),'</p>'),'</body></html>')
writeLines(html,file.path(out,'reports/report.html'))
jsonlite::write_json(list(passed=TRUE,protocol_sha256=study_hash(protocol_path),model_sha256=study_hash(model_path),
  rows=nrow(input),inputs=8L,selected_epoch=fit$selected_epoch,trained_epochs=fit$trained_epochs,
  constant_output_rate=rates,conditional_metrics=!training_only,source_calibration_all_rows=!training_only,
  training_only_calibration=training_only,calibration_rows=length(calibration_rows),
  prefix_calibration=FALSE,rows_removed=0L,simulator_modified=FALSE,defaults_changed=FALSE,
  runtime_checks=checks),file.path(out,'completed.json'),auto_unbox=TRUE,pretty=TRUE)
print(metrics);cat('COMPLETE separate eight-input ground-truth fit\n')
