# Bayesian configuration and its editor. No prior is estimated from evaluation
# responses. CSV fields are parsed as data; user text is never evaluated as R.
lkt_validate_bayesian_options <- function(options,require_prior=TRUE) {
  if(is.null(options)) {
    if(require_prior)stop('Open Bayesian prior settings and supply explicit prior means and covariance first.',call.=FALSE)
    options<-list()
  }
  if(!is.list(options)||(length(options)&&(is.null(names(options))||anyDuplicated(names(options))||
     any(!names(options)%in%names(formals(OnlineBayesianModel))))))
    stop('Configure Bayesian settings with a named list of OnlineBayesianModel arguments.',call.=FALSE)
  defaults<-list(prior_mean=NULL,prior_covariance=NULL,beta_alpha_terms=NULL,n_particles=2048L,
    seed=1L,subject_col='Anon.Student.Id',ess_threshold=.01,ess_action='warn',store_trace=FALSE)
  options<-utils::modifyList(defaults,options,keep.null=TRUE)
  mu<-options$prior_mean;v<-options$prior_covariance
  if(require_prior&&(is.null(mu)||is.null(v)))stop('Configure explicit Bayesian prior means and covariance first.',call.=FALSE)
  if(!is.null(mu)) {
    lkt_bayesian_names(names(mu),'prior_mean')
    if(!is.numeric(mu)||any(!is.finite(mu)))stop('Prior means must be finite numbers.',call.=FALSE)
  }
  adapted<-options$beta_alpha_terms
  if(is.null(adapted))adapted<-rownames(v)
  if(!is.null(adapted))lkt_bayesian_names(adapted,'personalized coefficients')
  if(require_prior&&!all(adapted%in%names(mu)))stop('Personalized coefficients must occur in the prior means.',call.=FALSE)
  for(n in c('n_particles','seed')) {
    x<-options[[n]];minimum<-if(n=='seed')0 else 2
    if(!is.numeric(x)||length(x)!=1L||!is.finite(x)||x<minimum||x>.Machine$integer.max||x!=as.integer(x))
      stop('Invalid Bayesian ',n,call.=FALSE)
    options[[n]]<-as.integer(x)
  }
  if(!is.numeric(options$ess_threshold)||length(options$ess_threshold)!=1L||
    !is.finite(options$ess_threshold)||options$ess_threshold<=0||options$ess_threshold>1)
    stop('ESS threshold must be a fraction in (0, 1].',call.=FALSE)
  options$ess_action<-match.arg(options$ess_action,c('warn','error','ignore'))
  if(!is.logical(options$store_trace)||length(options$store_trace)!=1L||is.na(options$store_trace))stop('Trace setting must be TRUE or FALSE.')
  if(!is.character(options$subject_col)||length(options$subject_col)!=1L||is.na(options$subject_col)||!nzchar(options$subject_col))stop('Select a subject column.')
  # Delegate joint-prior validation to the actual implementation. Two temporary
  # particles suffice for validation; its API restores the caller's RNG state.
  if(require_prior)LKTBayesianState(mu[adapted],v,n_particles=2L,seed=options$seed)
  options$beta_alpha_terms<-adapted
  options
}

lkt_bayesian_tables <- function(options=NULL) {
  if(is.null(options)||is.null(options$prior_mean)||is.null(options$prior_covariance))
    return(list(mean='coefficient|mean',covariance='coefficient'))
  mu<-options$prior_mean;v<-options$prior_covariance
  precise<-function(x)format(x,digits=17,scientific=TRUE,trim=TRUE)
  formatted<-matrix(precise(v),nrow=nrow(v),dimnames=dimnames(v))
  list(mean=lkt_gui_terms_text(data.frame(coefficient=names(mu),mean=precise(mu))),
    covariance=lkt_gui_terms_text(data.frame(coefficient=rownames(v),formatted,check.names=FALSE)))
}

lkt_bayesian_parse_tables <- function(mean_text,covariance_text) {
  read<-function(text)utils::read.table(text=text,header=TRUE,sep='|',quote='',comment.char='',
    colClasses='character',check.names=FALSE,na.strings=character(),fill=FALSE)
  mean<-read(mean_text);v<-read(covariance_text)
  if(!identical(names(mean),c('coefficient','mean'))||!nrow(mean))stop('Prior means require coefficient|mean and one row per coefficient.')
  if(ncol(v)<2L||!nrow(v)||names(v)[1L]!='coefficient')stop('Enter a named square covariance table.')
  mu<-suppressWarnings(as.numeric(mean$mean));names(mu)<-mean$coefficient
  matrix<-matrix(suppressWarnings(as.numeric(as.matrix(v[,-1L,drop=FALSE]))),nrow=nrow(v),
    dimnames=list(v[[1L]],names(v)[-1L]))
  list(prior_mean=mu,prior_covariance=matrix)
}

lkt_bayesian_gui <- function(options=NULL,theme='blue') {
  result<-new.env(parent=emptyenv());result$value<-NULL
  tables<-lkt_bayesian_tables(options)
  form<-function(prior_means='',prior_covariance='') {
    parsed<-lkt_bayesian_parse_tables(prior_means,prior_covariance)
    merged<-utils::modifyList(if(is.null(options))list() else options,parsed)
    merged$beta_alpha_terms<-rownames(parsed$prior_covariance)
    result$value<-lkt_validate_bayesian_options(merged)
    tcltk::tkdestroy(fgui::guiGet('MAIN'))
    NULL
  }
  formals(form)$prior_means<-tables$mean;formals(form)$prior_covariance<-tables$covariance
  lkt_form_gui(form,title='Bayesian prior',subtitle=paste('Supply priors from independent training subjects or prior knowledge.',
    'Covariance describes individual differences, not uncertainty in a population mean. Online settings are on the main Training and evaluation tab.'),
    sections=list('Prior means'='prior_means','Joint covariance'='prior_covariance'),
    labels=list(prior_means='Named means (coefficient|mean)',prior_covariance='Named covariance (variances on the diagonal)'),
    edit=list(prior_means=c(75,10,FALSE),prior_covariance=c(75,10,FALSE)),
    submit_label='Use these priors',modal=TRUE,theme=theme)
  result$value
}
