#' Define a generalized linear mixed-effects LKT model
#'
#' @param prediction_mode Include estimated random effects ("conditional") or
#'   fixed effects only ("fixed"). Fixed-only predictions do not integrate over
#'   the random-effect distribution.
#' @param unseen_levels "error" rejects unseen grouping levels; "zero" explicitly
#'   assigns them zero random effects in conditional predictions.
#' @param nAGQ Quadrature setting passed to lme4::glmer.
#' @param control NULL or an lme4::glmerControl object.
#' @param clip_epsilon Probability bound used by the LKT likelihood interface.
#' @return An LKT model adapter. GLMM results retain fixed coefficients, random
#'   effects and variance components separately.
#' @export
GLMMModel <- function(prediction_mode=c('conditional','fixed'),
                      unseen_levels=c('error','zero'), nAGQ=1L,
                      control=NULL, clip_epsilon=1e-5) {
  prediction_mode<-match.arg(prediction_mode);unseen_levels<-match.arg(unseen_levels)
  if(length(nAGQ)!=1L || !is.finite(nAGQ) || nAGQ<0 || nAGQ!=as.integer(nAGQ))
    stop('nAGQ must be a nonnegative integer.',call.=FALSE)
  if(length(clip_epsilon)!=1L || !is.finite(clip_epsilon) || clip_epsilon<=0 || clip_epsilon>=.5)
    stop('clip_epsilon must lie between zero and one half.',call.=FALSE)
  LKTCustomModel('GLMM',function(request,verbose=FALSE) {
    lkt_fit_glmm(request,prediction_mode,unseen_levels,nAGQ,control,clip_epsilon)
  })
}

lkt_glmm_formula <- function(formula) {
  rewrite<-function(x) {
    if(!is.call(x))return(x)
    if(identical(x[[1L]],as.name('$')) &&
       identical(x[[2L]],quote(e$data))) return(x[[3L]])
    as.call(lapply(as.list(x),rewrite))
  }
  stats::as.formula(rewrite(formula),env=environment(formula))
}

lkt_glmm_prediction <- function(fit,data,mode,unseen,epsilon) {
  p<-as.numeric(stats::predict(fit,newdata=as.data.frame(data),type='response',
    re.form=if(mode=='conditional')NULL else NA,
    allow.new.levels=identical(unseen,'zero'),na.action=stats::na.pass))
  if(length(p)!=nrow(data) || any(!is.finite(p)))
    stop('GLMM predictions must align with every supplied row and be finite.',call.=FALSE)
  pmin(pmax(p,epsilon),1-epsilon)
}

lkt_fit_glmm <- function(request,mode,unseen,nAGQ,control,epsilon) {
  data<-as.data.frame(request$data)
  keep<-if(is.na(request$usefolds)[1L]) rep(TRUE,nrow(data)) else {
    lkt_require_columns(data,'fold','GLMM fold selection')
    data$fold %in% request$usefolds
  }
  if(!any(keep)) stop('GLMM has no rows selected for fitting.',call.=FALSE)
  formula<-lkt_glmm_formula(request$formula)
  if(!length(lme4::findbars(formula)))stop('GLMMModel requires a random-effect term.',call.=FALSE)
  args<-list(formula=formula,data=data,subset=keep,family=stats::binomial('logit'),
             nAGQ=nAGQ,na.action=stats::na.fail)
  if(!is.null(control))args$control<-control
  fit<-do.call(lme4::glmer,args)
  p<-lkt_glmm_prediction(fit,data,mode,unseen,epsilon)
  co<-lme4::fixef(fit)
  # Use the fitted fixed-effect columns; random effects remain in their own
  # schema and are predicted by lme4, never by a static coefficient shortcut.
  design<-request$design_matrix
  index<-match(names(co),colnames(design$sparse))
  if(anyNA(index))stop('GLMM fixed-effect columns do not match LKT feature input.',call.=FALSE)
  sparse<-design$sparse[,index,drop=FALSE]
  csr<-SparseM::as.matrix.csr(methods::new('matrix.csc',ra=sparse@x,
    ja=sparse@i+1L,ia=sparse@p+1L,dimension=sparse@Dim))
  result<-LKTModelFit(fit,p,modelvs=matrix(co,ncol=1L,dimnames=list(names(co),'coefficient')),
    fitstat=as.numeric(stats::logLik(fit)),predictset=sparse,predictset2=csr)
  result$model_metadata<-list(random_effects=lme4::ranef(fit),
    variance_components=as.data.frame(lme4::VarCorr(fit)),
    prediction_policy=list(mode=mode,unseen_levels=unseen,clip_epsilon=epsilon),
    fit_rows=which(keep),dropped_fixed_effects=attr(lme4::getME(fit,'X'),'col.dropped'),likelihood_type='GLMM marginal likelihood')
  result
}
