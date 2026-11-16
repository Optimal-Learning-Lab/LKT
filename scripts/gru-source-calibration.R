# Record the maintained LKT fitter's return before applying research release gates.
# No optimizer replacement, retries, tolerance changes or alternative model here.
gru_source_calibration <- function(data,configuration,directory,protocol_sha256,seed,
    fit_source=LKT::fit_lkt_config) {
  if(!nrow(data)||!all(data$fold==1L))stop('Source calibration requires training rows only')
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  record_path <- file.path(directory,'source_calibration.rds')
  report_path <- file.path(directory,'source_calibration.json')
  identity <- list(protocol_sha256=protocol_sha256,configuration=configuration,seed=seed,
    input_sha256=digest::digest(data,algo='sha256'))
  write_once <- function(value,path,writer) {
    if(file.exists(path))stop('Refusing to replace source-calibration diagnostics: ',path)
    temporary <- tempfile('calibration_',directory,'.tmp')
    on.exit(if(file.exists(temporary))unlink(temporary))
    writer(value,temporary)
    if(!file.rename(temporary,path))stop('Cannot commit source-calibration diagnostics')
  }
  if(file.exists(record_path)) {
    saved <- readRDS(record_path)
    if(!identical(saved$identity,identity))stop('Source calibration continuation identity differs')
  } else {
    warnings <- character();failure <- NULL
    started <- proc.time()[['elapsed']];set.seed(seed)
    fit <- tryCatch(withCallingHandlers(fit_source(data,configuration),warning=function(w) {
      warnings <<- c(warnings,conditionMessage(w))
    }),error=function(e) {failure <<- list(message=conditionMessage(e),class=class(e));NULL})
    elapsed <- proc.time()[['elapsed']]-started
    opt <- fit$optimizedpars
    code <- if(length(opt$convergence)==1L)as.integer(opt$convergence) else NA_integer_
    finite <- !is.null(fit) && length(opt$par)==length(configuration$fixedpars) &&
      all(is.finite(opt$par)) && length(fit$prediction)==nrow(data) &&
      all(is.finite(fit$prediction)) && all(fit$prediction>0 & fit$prediction<1)
    training <- !is.null(fit) && nrow(fit$newdata)==nrow(data) && all(fit$newdata$fold==1L) &&
      identical(fit$newdata$comparison_source_row,data$comparison_source_row)
    passed <- is.null(failure) && identical(code,0L) && finite && training && is.null(fit$latencymodel)
    message <- if(length(opt$message))paste(opt$message,collapse='; ') else ''
    report <- list(passed=passed,protocol_sha256=protocol_sha256,
      calibration_rows=nrow(data),calibration_students=length(unique(data$Anon.Student.Id)),
      convergence_code=code,optimizer_message=message,objective=opt$value,
      evaluation_counts=as.list(opt$counts),seconds=elapsed,
      iteration_limit=configuration$options$maxitv,factr=configuration$options$factrv,
      linear_epsilon=configuration$options$epsilon,finite_fit=finite,
      training_rows_verified=training,wald_fits=0L,warnings=warnings,error=failure)
    saved <- list(identity=identity,report=report,fit=fit)
    write_once(saved,record_path,saveRDS)
  }
  # Repair only an interrupted diagnostic write, using the saved fitter return.
  if(!file.exists(report_path))write_once(saved$report,report_path,function(value,path)
    jsonlite::write_json(value,path,auto_unbox=TRUE,pretty=TRUE,digits=16,na='null'))
  cat('SOURCE_CALIBRATION',jsonlite::toJSON(saved$report,auto_unbox=TRUE,digits=10,na='null'),'\n')
  flush.console()
  if(!isTRUE(saved$report$passed)) {
    reason <- if(!is.null(saved$report$error))saved$report$error$message else
      paste('optimizer code',saved$report$convergence_code,saved$report$optimizer_message)
    stop('Training-only source calibration rejected: ',reason,
      '. Full return preserved at ',record_path,call.=FALSE)
  }
  saved$fit
}
