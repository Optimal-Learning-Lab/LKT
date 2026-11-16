# Orchestration of maintained LKT functions; no copied fitting algorithms.
cloze_fixed_configuration <- function(fit,components,features,latency='none') {
  counts <- getFromNamespace('lkt_feature_param_counts','LKT')(features)
  parameters <- unname(fit$optimizedpars$par)
  if(length(parameters)!=sum(counts)||any(!is.finite(parameters))||fit$optimizedpars$convergence!=0L)
    stop('A converged preliminary nonlinear fit is required')
  position <- 1L
  fixed <- vapply(counts,function(n) {
    if(!n)return('')
    values <- parameters[seq.int(position,length.out=n)];position <<- position+n
    paste(format(values,digits=17,scientific=FALSE,trim=TRUE),collapse=';')
  },character(1))
  config <- LKT::lkt_model_config(data.frame(component=components,feature=features,fixed=fixed),
    intercept=TRUE,backend='LibLinear',latency=latency,folds=NA_real_)
  stopifnot(length(config$fixedpars)==length(parameters),all(is.finite(config$fixedpars)),
    isTRUE(all.equal(unname(config$fixedpars),parameters,tolerance=1e-15)))
  config
}

cloze_append_wald_once <- function(accuracy,contamination=.02,verbose=TRUE,
    fit_latency=getFromNamespace('lkt_fit_latency_model','LKT')) {
  if(!is.null(accuracy$latencymodel)||length(accuracy$model_specification)!=1L)
    stop('Supply an accuracy-only calibration; Wald must not be refitted')
  before <- serialize(accuracy[c('model','coefs','prediction','predictors','optimizedpars')],NULL)
  data <- data.table::copy(accuracy$newdata)
  prepared <- getFromNamespace('lkt_prepare_latency_data','LKT')(data,usefolds=NA_real_)
  latency <- fit_latency(latency_data=prepared,distribution='Wald',verbose=verbose,
    wald_contamination_weight=contamination)
  failure <- getFromNamespace('lkt_failure_latency','LKT')(data,usefolds=NA_real_)
  result <- accuracy
  result$latencymodel <- getFromNamespace('lkt_latency_model_result','LKT')('Wald',latency$model,failure)
  result$model_specification[[2L]] <- getFromNamespace('lkt_latency_model_specification','LKT')('Wald',latency$model,failure)
  stopifnot(identical(before,serialize(result[c('model','coefs','prediction','predictors','optimizedpars')],NULL)))
  result
}
