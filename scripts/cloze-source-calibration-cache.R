# Explicit, content-addressed calibration artifacts. Neural runs only read them.
cloze_source_request <- function(cfg) {
  LKT::lkt_model_config(data.frame(component=cfg$source_components,feature=cfg$source_features),
    intercept=TRUE,backend='LibLinear',latency='none',folds=NA_real_,
    options=list(epsilon=cfg$source_epsilon,factrv=cfg$source_factrv,
      maxitv=cfg$source_maxitv,verbose=FALSE))
}

cloze_source_identity <- function(data,data_sha256,configuration,seed,release) {
  train <- which(data$fold==1L)
  roles <- unique(data[c('Anon.Student.Id','fold')])
  if(!length(train)||anyDuplicated(roles$Anon.Student.Id))stop('Whole-student training rows required')
  list(schema=1L,data_sha256=data_sha256,training_rows=train,
    training_data_sha256=digest::digest(data[train,,drop=FALSE],algo='sha256'),
    configuration=configuration,seed=seed,
    release=as.list(release[c('lkt_version','lkt_source_sha256','lkt_manifest_sha256')]),
    R=R.version.string,packages=setNames(vapply(c('LKT','torch','Matrix'),function(n)
      as.character(utils::packageVersion(n)),character(1)),c('LKT','torch','Matrix')))
}

cloze_source_key <- function(identity)digest::digest(identity,algo='sha256')
cloze_source_directory <- function(store,identity)file.path(store,cloze_source_key(identity))
cloze_source_file_hash <- function(path)digest::digest(file=path,algo='sha256')

cloze_source_validate_fit <- function(fit,identity,data) {
  train <- data[identity$training_rows,,drop=FALSE]
  stopifnot(identical(identity$training_data_sha256,digest::digest(train,algo='sha256')),
    fit$optimizedpars$convergence==0L,length(fit$optimizedpars$par)==8L,
    all(is.finite(fit$optimizedpars$par)),is.null(fit$latencymodel),
    nrow(fit$newdata)==nrow(train),all(fit$newdata$fold==1L),
    identical(fit$newdata$comparison_source_row,train$comparison_source_row),
    identical(as.character(fit$newdata$Anon.Student.Id),as.character(train$Anon.Student.Id)),
    identical(fit$newdata$CF..ansbin.,train$CF..ansbin.),
    length(fit$prediction)==nrow(train),all(is.finite(fit$prediction)),
    all(fit$prediction>0 & fit$prediction<1))
  invisible(TRUE)
}

cloze_source_load <- function(store,identity,data) {
  directory <- cloze_source_directory(store,identity)
  receipt_path <- file.path(directory,'receipt.json')
  if(!file.exists(receipt_path))stop('No complete matching source calibration. Run the separate calibrate step: ',directory)
  receipt <- jsonlite::fromJSON(receipt_path)
  stopifnot(receipt$schema==1L,receipt$key==cloze_source_key(identity),receipt$passed)
  stopifnot(setequal(names(receipt$sha256),c('calibration.rds','fixed_parameters.csv','fixed_parameters.json')))
  for(name in names(receipt$sha256))
    stopifnot(cloze_source_file_hash(file.path(directory,name))==receipt$sha256[[name]])
  saved <- readRDS(file.path(directory,'calibration.rds'))
  stopifnot(saved$schema==1L,identical(saved$identity,identity))
  cloze_source_validate_fit(saved$fit,identity,data)
  stopifnot(length(saved$configuration$fixedpars)==8L,all(is.finite(saved$configuration$fixedpars)),
    isTRUE(all.equal(unname(saved$configuration$fixedpars),unname(saved$fit$optimizedpars$par),tolerance=1e-15)),
    getFromNamespace('lkt_lkt_optimization_inputs','LKT')(
      saved$configuration$fixedpars,saved$configuration$seedpars,8L)$parameter_count==0L)
  list(fit=saved$fit,configuration=saved$configuration,identity=identity,directory=directory,
    key=receipt$key,receipt_sha256=cloze_source_file_hash(receipt_path),
    model_sha256=receipt$sha256[['calibration.rds']],nonlinear_parameters=saved$configuration$fixedpars)
}

cloze_source_publish <- function(store,identity,data,fit,provenance) {
  cloze_source_validate_fit(fit,identity,data)
  if(file.exists(cloze_source_directory(store,identity)))return(cloze_source_load(store,identity,data))
  dir.create(store,recursive=TRUE,showWarnings=FALSE)
  staging <- tempfile('.publishing_',store);dir.create(staging)
  configuration <- cloze_fixed_configuration(fit,identity$configuration$terms$component,
    identity$configuration$terms$feature)
  fit$configuration <- configuration
  saved <- list(schema=1L,identity=identity,configuration=configuration,fit=fit,provenance=provenance)
  saveRDS(saved,file.path(staging,'calibration.rds'))
  values <- unname(configuration$fixedpars)
  parameters <- data.frame(feature=rep(c('ppes','ppef'),each=4L),parameter=rep(1:4,2L),value=values)
  write.csv(parameters,file.path(staging,'fixed_parameters.csv'),row.names=FALSE)
  jsonlite::write_json(list(schema=1L,key=cloze_source_key(identity),
    calibration_scope='training_only',training_rows=length(identity$training_rows),
    ppes=values[1:4],ppef=values[5:8],all_parameters_fixed=TRUE,
    nonlinear_optimizations_in_neural_runs=0L,wald_fits=0L),
    file.path(staging,'fixed_parameters.json'),auto_unbox=TRUE,pretty=TRUE,digits=17)
  files <- c('calibration.rds','fixed_parameters.csv','fixed_parameters.json')
  sha <- setNames(vapply(file.path(staging,files),cloze_source_file_hash,character(1)),files)
  jsonlite::write_json(list(schema=1L,key=cloze_source_key(identity),passed=TRUE,
    sha256=as.list(sha),provenance=provenance),file.path(staging,'receipt.json'),
    auto_unbox=TRUE,pretty=TRUE,digits=17)
  if(!file.rename(staging,cloze_source_directory(store,identity)))stop('Cannot publish complete source calibration')
  cloze_source_load(store,identity,data)
}

cloze_source_calibrate <- function(store,identity,data,
    fit_source=gru_source_calibration) {
  directory <- cloze_source_directory(store,identity)
  # Existing but invalid artifacts fail validation; never silently refit them.
  if(file.exists(directory))return(cloze_source_load(store,identity,data))
  dir.create(store,recursive=TRUE,showWarnings=FALSE)
  lock <- paste0(directory,'.lock')
  if(!dir.create(lock,showWarnings=FALSE))stop('Source calibration is already in progress: ',lock)
  on.exit({
    resolved <- normalizePath(lock,winslash='/',mustWork=TRUE)
    allowed <- paste0(normalizePath(store,winslash='/',mustWork=TRUE),'/')
    stopifnot(startsWith(resolved,allowed));unlink(lock,recursive=TRUE)
  },add=TRUE)
  if(file.exists(directory))return(cloze_source_load(store,identity,data))
  fit <- fit_source(data[identity$training_rows,,drop=FALSE],identity$configuration,
    file.path(store,paste0(cloze_source_key(identity),'.diagnostics')),
    cloze_source_key(identity),identity$seed)
  cloze_source_publish(store,identity,data,fit,list(method='Separate training-only nonlinear calibration',
    training_rows=length(identity$training_rows)))
}

cloze_source_import <- function(store,identity,data,record_path,protocol_path) {
  saved <- readRDS(record_path);prior <- readRDS(protocol_path)
  stopifnot(saved$protocol_sha256==cloze_source_file_hash(protocol_path),
    prior$input_sha256[['data']]==identity$data_sha256,
    identical(prior$calibration_rows,identity$training_rows),
    identical(prior$source_configuration,identity$configuration),
    identical(prior$configuration$source_seed,identity$seed),
    identical(prior$release[names(identity$release)],identity$release),
    identical(prior$R,identity$R),identical(prior$packages,identity$packages))
  cloze_source_publish(store,identity,data,saved$fit,list(method='Imported converged training-only calibration; zero fitting',
    source_record=normalizePath(record_path,winslash='/'),source_sha256=cloze_source_file_hash(record_path),
    source_protocol=normalizePath(protocol_path,winslash='/'),source_protocol_sha256=cloze_source_file_hash(protocol_path)))
}
