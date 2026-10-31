#' Available LKT model features
#' @return A data frame with feature names and nonlinear parameter counts.
#' @export
lkt_features <- function() lkt_feature_registry()

#' Construct a reproducible model configuration
#'
#' Terms remain in supplied order. In fixed, NA means estimate, and a positive
#' integer references an estimated parameter, as in LKT(). Starts are separate
#' from fixed values; supplying a start never fixes that parameter.
#' @param terms Data frame with component and feature, and optional fixed,
#'   start (semicolon-separated numbers), interaction, and connector columns.
#' @param intercept Include the global intercept.
#' @param backend LibLinear, GLMM, or OnlineBayesian.
#' @param latency none or Wald. Retired latency models are rejected.
#' @param folds Numeric fitting folds, or NA for all rows. History is retained.
#' @param options Named additional LKT arguments. Cannot override term settings.
#' @param glmm_prediction Conditional or fixed-only predictions.
#' @param glmm_unseen Error on unseen groups or assign zero random effects.
#' @param bayesian_options Named arguments for OnlineBayesianModel, including
#'   explicit prior_mean and prior_covariance from independent training data or
#'   prior knowledge. Only linear coefficients are personalized.
#' @param bayesian_training NULL for supplied priors, or a list containing
#'   training_fraction and split_seed for a whole-participant training/evaluation
#'   split. A correlated logistic mixed-model prefit estimates population means
#'   and between-participant covariance using training participants only.
#' @return A versioned LKTModelConfig list, suitable for saveRDS().
#' @export
lkt_model_config <- function(terms, intercept=TRUE, backend=c('LibLinear','GLMM','OnlineBayesian'),
                             latency=c('none','Wald'), folds=NA,
                             options=list(), glmm_prediction='conditional',
                             glmm_unseen='error',bayesian_options=NULL,bayesian_training=NULL) {
  backend<-match.arg(backend);latency<-match.arg(latency)
  terms<-as.data.frame(terms,stringsAsFactors=FALSE)
  if(!nrow(terms) || !all(c('component','feature') %in% names(terms)))
    stop('Supply at least one ordered component/feature term.',call.=FALSE)
  for(n in c('fixed','start','interaction','connector'))
    if(!n %in% names(terms))terms[[n]]<-if(n=='connector')'+' else ''
  terms[]<-lapply(terms,as.character)
  for(n in c('fixed','start','interaction'))terms[[n]][is.na(terms[[n]])]<-''
  if(anyNA(terms) || any(!nzchar(terms$component)))stop('Terms cannot be missing.',call.=FALSE)
  count<-lkt_feature_param_counts(terms$feature)
  parse_par<-function(value,n,label) {
    if(!nzchar(trimws(value)))return(rep(NA_real_,n))
    tokens<-trimws(strsplit(value,';',fixed=TRUE)[[1L]])
    if(any(!grepl('^(NA|[-+]?[0-9]*[.]?[0-9]+([eE][-+]?[0-9]+)?)$',tokens)))
      stop(label,': enter numbers or NA separated by semicolons.',call.=FALSE)
    values<-as.numeric(replace(tokens,tokens=='NA',NA_character_))
    if(length(values)!=n || any(!is.finite(values)&!is.na(values)))
      stop(label,': expected ',n,' values.',call.=FALSE)
    values
  }
  fixed<-unlist(Map(parse_par,terms$fixed,count, paste0('Fixed term ',seq_len(nrow(terms)))),use.names=FALSE)
  starts<-unlist(Map(parse_par,terms$start,count, paste0('Start term ',seq_len(nrow(terms)))),use.names=FALSE)
  links<-fixed[!is.na(fixed)&fixed>=1&fixed%%1==0]
  if(length(links)&&any(links>sum(is.na(fixed))))stop('A parameter link references a nonexistent estimated parameter.',call.=FALSE)
  if(any(!terms$connector %in% c('+',':','*')))stop('Connectors must be +, : or *.',call.=FALSE)
  random<-grepl('@$',terms$feature)
  if(any(random) && (backend!='GLMM'||any(terms$feature[random]!='intercept@')))
    stop('Random terms require GLMM and currently support intercept@ only.',call.=FALSE)
  if(backend=='GLMM'&&!any(random))stop('GLMM requires an intercept@ term.',call.=FALSE)
  if(!is.logical(intercept)||length(intercept)!=1L||is.na(intercept))stop('intercept must be TRUE or FALSE.',call.=FALSE)
  if(!(length(folds)==1L&&is.na(folds))&&(!is.numeric(folds)||!length(folds)||anyNA(folds)||any(!is.finite(folds))))
    stop('folds must be numeric or a single NA.',call.=FALSE)
  reserved<-c('data','components','features','fixedpars','seedpars','interacts','connectors','interc','model','dualfit','distribution','usefolds')
  if(!is.list(options)||length(options)&& (is.null(names(options))||anyDuplicated(names(options))||any(!nzchar(names(options)))))
    stop('options must be a named list.',call.=FALSE)
  if(any(!names(options)%in%setdiff(names(formals(LKT)),reserved)))stop('Unknown or reserved LKT option.',call.=FALSE)
  for(n in intersect(names(options),c('epsilon','cost','maxitv','factrv'))) {
    v<-options[[n]]
    if(!is.numeric(v)||length(v)!=1L||!is.finite(v)||v<=0||n=='maxitv'&&v!=as.integer(v))
      stop('Invalid positive numeric option: ',n,call.=FALSE)
  }
  if(backend=='GLMM')GLMMModel(glmm_prediction,glmm_unseen)
  if(backend=='OnlineBayesian') {
    if(latency!='none'||!all(is.na(folds))||any(!is.finite(fixed))||any(fixed>=1 & fixed%%1==0)||any(random))
      stop('Bayesian online models require latency none, folds NA and fixed nonlinear parameters without parameter ties or random-effect terms.',call.=FALSE)
    if(isTRUE(options$nosolve)||any(options$autoKC!=0)||length(options$model_options))
      stop('Bayesian models require autoKC = 0 and no nosolve; use bayesian_options for their settings.',call.=FALSE)
    bayesian_training<-lkt_validate_bayesian_training(bayesian_training)
    if(!is.null(bayesian_training)) {
      # Split mode must never reuse an illustrative or previously fitted prior.
      bayesian_options$prior_mean<-NULL;bayesian_options$prior_covariance<-NULL
    }
    bayesian_options<-lkt_validate_bayesian_options(bayesian_options,require_prior=is.null(bayesian_training))
  }
  if(backend!='OnlineBayesian'&&!is.null(bayesian_training))stop('Participant prefit requires OnlineBayesian.',call.=FALSE)
  structure(list(schema=1L,terms=terms,intercept=intercept,backend=backend,latency=latency,
    folds=folds,options=options,fixedpars=fixed,seedpars=starts,
    glmm_prediction=glmm_prediction,glmm_unseen=glmm_unseen,bayesian_options=bayesian_options,
    bayesian_training=bayesian_training),class='LKTModelConfig')
}

lkt_config_arguments <- function(config) {
  if(!inherits(config,'LKTModelConfig')||!identical(config$schema,1L))stop('Expected an LKT model configuration.',call.=FALSE)
  # Revalidate loaded files; serialized lists are not an alternative validation path.
  checked<-lkt_model_config(config$terms,config$intercept,config$backend,config$latency,
    config$folds,config$options,config$glmm_prediction,config$glmm_unseen,config$bayesian_options,config$bayesian_training)
  terms<-checked$terms;interaction<-terms$interaction;interaction[!nzchar(interaction)]<-NA_character_
  c(list(components=terms$component,features=terms$feature,interc=checked$intercept,
    fixedpars=if(length(checked$fixedpars))checked$fixedpars else NA_real_,
    seedpars=if(length(checked$seedpars))checked$seedpars else NA_real_,
    interacts=interaction,connectors=terms$connector[-1L],usefolds=checked$folds,
    model=switch(checked$backend,GLMM=GLMMModel(checked$glmm_prediction,checked$glmm_unseen),
      OnlineBayesian=do.call(OnlineBayesianModel,checked$bayesian_options),LibLinear=LibLinearModel()),
    dualfit=checked$latency!='none',distribution='Wald'),checked$options)
}

#' Fit a saved model configuration
#' @param data Full chronological human history with canonical LKT columns.
#' @param config An LKTModelConfig.
#' @return The normal LKT fitted model, with its configuration attached.
#' @export
fit_lkt_config <- function(data,config) {
  args<-lkt_config_arguments(config)
  if(!is.null(config$bayesian_training))return(lkt_fit_bayesian_split(data,config,args))
  fit<-do.call(LKT,c(list(data=data.table::copy(data.table::as.data.table(data))),args))
  fit$configuration<-config
  fit
}

#' Show the R call represented by a model configuration
#' @param config An LKTModelConfig.
#' @return A printable language object. Does not fit a model.
#' @export
lkt_config_call <- function(config) {
  args<-lkt_config_arguments(config)
  literal<-function(x)parse(text=paste(utils::capture.output(dput(x)),collapse='\n'))[[1L]]
  if(!is.null(config$bayesian_training))
    return(as.call(list(as.name('fit_lkt_config'),data=as.name('data'),config=literal(config))))
  args$model<-switch(config$backend,GLMM=call('GLMMModel',prediction_mode=config$glmm_prediction,unseen_levels=config$glmm_unseen),
    OnlineBayesian=as.call(c(list(as.name('OnlineBayesianModel')),lapply(lkt_validate_bayesian_options(config$bayesian_options),literal))),
    LibLinear=call('LibLinearModel'))
  as.call(c(list(as.name('LKT'),data=as.name('data')),args))
}
