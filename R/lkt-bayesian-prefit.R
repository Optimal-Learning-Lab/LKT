# Whole-participant prefit is orchestration around the unchanged online adapter.
# No evaluation responses enter the hierarchical fit or its feature histories.
lkt_validate_bayesian_training <- function(settings) {
  if(is.null(settings))return(NULL)
  if(!is.list(settings)||is.null(names(settings))||anyDuplicated(names(settings))||
     any(!names(settings)%in%c('training_fraction','split_seed')))
    stop('Participant split settings must name training_fraction and split_seed.',call.=FALSE)
  settings<-utils::modifyList(list(training_fraction=.7,split_seed=1L),settings)
  f<-settings$training_fraction;s<-settings$split_seed
  if(!is.numeric(f)||length(f)!=1L||!is.finite(f)||f<=0||f>=1)
    stop('Training fraction must be between zero and one.',call.=FALSE)
  if(!is.numeric(s)||length(s)!=1L||!is.finite(s)||s<0||s>.Machine$integer.max||s!=floor(s))
    stop('Participant split seed must be a nonnegative integer.',call.=FALSE)
  settings$split_seed<-as.integer(s)
  settings
}

lkt_bayesian_participant_split <- function(data,subject_col,settings) {
  settings<-lkt_validate_bayesian_training(settings)
  if(is.null(settings))stop('Configure a participant split first.',call.=FALSE)
  if(!subject_col%in%names(data))stop('Participant column is missing from the dataset.',call.=FALSE)
  subjects<-as.character(data[[subject_col]])
  if(anyNA(subjects)||any(!nzchar(subjects)))stop('Missing participant identifiers.',call.=FALSE)
  # Stable ID ordering makes the assignment independent of the input row order.
  ids<-sort(unique(subjects),method='radix')
  n<-floor(length(ids)*settings$training_fraction)
  if(n<3L||n>=length(ids))stop('The split requires at least three training participants and one evaluation participant.',call.=FALSE)
  had<-exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE)
  previous<-if(had)get('.Random.seed',envir=.GlobalEnv) else NULL
  on.exit(if(had)assign('.Random.seed',previous,envir=.GlobalEnv) else
    if(exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))rm('.Random.seed',envir=.GlobalEnv),add=TRUE)
  # Explicit RNG kind makes saved split settings reproducible across callers.
  old_kind<-RNGkind();on.exit(do.call(RNGkind,as.list(old_kind)),add=TRUE,after=FALSE)
  set.seed(settings$split_seed,kind='Mersenne-Twister',normal.kind='Inversion',sample.kind='Rejection')
  training<-ids[sample.int(length(ids),n)]
  manifest<-data.frame(participant=ids,group=ifelse(ids%in%training,'training','evaluation'),
    rows=as.integer(table(factor(subjects,levels=ids))),stringsAsFactors=FALSE)
  list(settings=settings,subject_col=subject_col,manifest=manifest,
    training_rows=which(subjects%in%training),evaluation_rows=which(!subjects%in%training))
}

lkt_bayesian_prefit_adapter <- function(personalized,subject_col) {
  LKTCustomModel('BayesianPopulationPrefit',function(request,verbose=FALSE) {
    x<-as.matrix(request$design_matrix$sparse);nm<-colnames(x)
    adapted<-if(is.null(personalized))nm else personalized
    lkt_bayesian_names(adapted,'Personalized coefficients')
    if(!all(adapted%in%nm))stop('Personalized coefficients do not match the training design matrix.',call.=FALSE)
    if(any(!is.finite(x))||qr(x)$rank!=ncol(x))
      stop('The training feature matrix must be finite and full rank; no coefficients are dropped automatically.',call.=FALSE)
    subjects<-as.character(request$data[[subject_col]])
    if(length(unique(subjects))<=length(adapted))
      stop('Use more training participants than personalized coefficients.',call.=FALSE)
    # Safe internal column names preserve exact LKT coefficient names externally.
    xn<-paste0('x',seq_len(ncol(x)));colnames(x)<-xn
    frame<-as.data.frame(x);frame$response<-request$response;frame$participant<-factor(subjects)
    formula<-stats::as.formula(paste('response ~ 0 +',paste(xn,collapse=' + '),
      '+ (0 +',paste(xn[match(adapted,nm)],collapse=' + '),'| participant)'))
    fit<-lme4::glmer(formula,data=frame,family=stats::binomial('logit'),nAGQ=1L,
      na.action=stats::na.fail,control=lme4::glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=100000L)))
    if(lme4::isSingular(fit,tol=1e-4))
      stop('Population prefit has singular participant covariance. Reduce personalized coefficients or supply an independently justified prior; no jitter or substitute prior is used.',call.=FALSE)
    messages<-fit@optinfo$conv$lme4$messages
    if(length(messages)||any(fit@optinfo$conv$opt!=0))
      stop('Population prefit did not converge: ',paste(messages,collapse='; '),call.=FALSE)
    mu<-lme4::fixef(fit)
    if(!identical(names(mu),xn))stop('Population prefit dropped or reordered coefficients.',call.=FALSE)
    names(mu)<-nm
    covariance<-as.matrix(lme4::VarCorr(fit)$participant)
    covariance<-covariance[xn[match(adapted,nm)],xn[match(adapted,nm)],drop=FALSE]
    dimnames(covariance)<-list(adapted,adapted)
    # Cholesky validation of the actual between-participant covariance.
    LKTBayesianState(mu[adapted],covariance,n_particles=2L,seed=1L)
    p<-as.numeric(stats::predict(fit,type='response'))
    p<-pmin(pmax(p,.Machine$double.eps),1-.Machine$double.eps)
    LKTModelFit(model=list(fit=fit,prior_mean=mu,prior_covariance=covariance,
      beta_alpha_terms=adapted,estimator='Correlated logistic mixed model; Laplace marginal likelihood',
      formula=formula,coefficient_map=stats::setNames(nm,xn)),pred=p,
      modelvs=matrix(mu,ncol=1L,dimnames=list(nm,'coefficient')),
      fitstat=as.numeric(stats::logLik(fit)))
  })
}

lkt_fit_bayesian_split <- function(data,config,args) {
  data<-data.table::copy(data.table::as.data.table(data))
  split<-lkt_bayesian_participant_split(data,config$bayesian_options$subject_col,config$bayesian_training)
  training<-data[split$training_rows];evaluation<-data[split$evaluation_rows]
  # Restrict features to within-participant histories; LKT already supplies the
  # student/KC history semantics. Build each group's features independently.
  prefit_args<-args
  prefit_args$model<-lkt_bayesian_prefit_adapter(config$bayesian_options$beta_alpha_terms,split$subject_col)
  prefit<-do.call(LKT,c(list(data=training),prefit_args))
  estimated<-config$bayesian_options
  estimated$prior_mean<-prefit$model$prior_mean
  estimated$prior_covariance<-prefit$model$prior_covariance
  estimated$beta_alpha_terms<-prefit$model$beta_alpha_terms
  estimated<-lkt_validate_bayesian_options(estimated)
  evaluation_config<-config;evaluation_config$bayesian_training<-NULL
  evaluation_config$bayesian_options<-estimated
  result<-fit_lkt_config(evaluation,evaluation_config)
  result$configuration<-config
  result$evaluation_configuration<-evaluation_config
  result$participant_split<-split
  result$population_prefit<-prefit
  result
}
