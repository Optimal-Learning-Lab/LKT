# Wald normal-score latency: direct fitting and explicit calibration import.
# An explicitly supplied calibration is validated and never silently refitted.
wald_logtails <- function(x,k,lambda0) {
  r <- x/lambda0; root <- sqrt(k/r)
  a <- root*(r-1); b <- -root*(r+1)
  first <- pnorm(a,log.p=TRUE); second <- 2*k+pnorm(b,log.p=TRUE)
  high <- pmax(first,second)
  lc <- high+log1p(exp(pmin(first,second)-high))
  sf_first <- pnorm(-a,log.p=TRUE)
  delta <- second-sf_first
  if(any(delta>=0)) stop('Wald survival subtraction lost numerical resolution.')
  ls <- sf_first+log(-expm1(delta))
  list(cdf=lc,survival=ls)
}

wald_latency_from_score <- function(z,k,lambda0,t0=0) {
  scalar <- function(x) is.numeric(x)&&length(x)==1L&&is.finite(x)
  if(!is.numeric(z)||any(!is.finite(z))||!scalar(k)||k<=0||
     !scalar(lambda0)||lambda0<=0||!scalar(t0)||t0<0) stop('Invalid Wald quantile inputs.')
  if(!length(z)) return(numeric())
  # Invert the log normal tail directly: no rounded CDF or probability clipping.
  lower <- z<=0; target <- pnorm(-abs(z),log.p=TRUE)
  logx <- log(lambda0)+2*asinh(z/(2*sqrt(k)))
  for(iter in seq_len(60L)) {
    x<-exp(logx);tails<-wald_logtails(x,k,lambda0)
    tail<-ifelse(lower,tails$cdf,tails$survival)
    error<-tail-target
    if(max(abs(error))<1e-8) return(t0+x)
    r<-x/lambda0
    logpdf<-.5*(log(k*lambda0)-log(2*pi)-3*logx)-.5*k*(r-1)^2/r
    slope<-ifelse(lower,1,-1)*exp(logpdf+logx-tail)
    step<-pmax(-1,pmin(1,error/slope));logx<-logx-step
  }
  stop('Wald quantile inversion did not converge.')
}

# The Wald fit and generator use the same upper correctness-probability limit.
# This bounds the activation input; it is separate from the response-time cap.
wald_latency_probability <- function(p) {
  if(!is.numeric(p)||any(!is.finite(p))||any(p<=0|p>=1))
    stop('Invalid correctness probabilities.')
  pmin(p,0.9999)
}

wald_quantile_latency <- function(p,student_effect,trial_noise,beta1,k,lambda0,t0=0) {
  p <- wald_latency_probability(p)
  if(!is.numeric(p)||any(!is.finite(p))||any(p<=0|p>=1)) stop('Invalid correctness probabilities.')
  valid <- function(x) is.numeric(x)&&all(is.finite(x))&&length(x)%in%c(1L,length(p))
  if(!valid(student_effect)||!valid(trial_noise)||length(beta1)!=1L||!is.finite(beta1)) stop('Invalid latency effects.')
  wald_latency_from_score(-beta1*qlogis(p)+student_effect+trial_noise,k,lambda0,t0)
}

# Direct marginal-likelihood fitting for the same centered normal-score Wald
# model used by import_wald_calibration(). The uniform contamination component
# is fitting-only; generation always uses the engaged Wald component.
wald_normal_rule <- function(nodes=31L) {
  if(length(nodes)!=1L || !is.finite(nodes) || nodes<3 || nodes%%1)
    stop('nodes must be an integer >= 3.')
  J <- matrix(0,nodes,nodes); j <- seq_len(nodes-1L)
  J[cbind(j,j+1L)] <- sqrt(j)
  e <- eigen(J+t(J),symmetric=TRUE)
  list(x=e$values,logw=log(e$vectors[1,]^2))
}

wald_latency_transform <- function(rt,k,lambda0,t0=0) {
  if(any(!is.finite(rt)) || any(rt<=0) || !length(rt)) stop('RT must be finite and positive.')
  wald_latency_from_score(numeric(0),k,lambda0,t0)
  ok <- rt>t0; x <- rt[ok]-t0; ratio <- x/lambda0; root <- sqrt(k/ratio)
  a <- root*(ratio-1); b <- -root*(ratio+1)
  tails <- wald_logtails(x,k,lambda0); lower <- tails$cdf<log(.5)
  zz <- numeric(length(x))
  zz[lower] <- qnorm(tails$cdf[lower],log.p=TRUE)
  zz[!lower] <- qnorm(tails$survival[!lower],lower.tail=FALSE,log.p=TRUE)
  logpdf <- .5*(log(k*lambda0)-log(2*pi)-3*log(x))-.5*k*(ratio-1)^2/ratio
  logphi <- dnorm(zz,log=TRUE); logphia <- dnorm(a,log=TRUE)
  z <- numeric(length(rt)); lj <- rep(-Inf,length(rt))
  dz <- dd <- matrix(0,length(rt),3L)
  z[ok] <- zz; lj[ok] <- logpdf-logphi
  logR <- log(root)+logphia
  logT <- log(2*k)+2*k+pnorm(b,log.p=TRUE)
  dz[ok,1L] <- exp(logR-logphi)*expm1(logT-logR)
  dz[ok,2L] <- -root*exp(logphia-logphi)
  dz[ok,3L] <- -exp(logpdf-logphi)
  dd[ok,1L] <- .5-.5*k*(ratio-1)^2/ratio
  dd[ok,2L] <- .5+.5*k*(ratio-1/ratio)
  dd[ok,3L] <- 1.5/x+.5*k*(1/lambda0-lambda0/x^2)
  if(any(!is.finite(z)) || any(!is.finite(dz)) || any(!is.finite(dd)))
    stop('Nonfinite Wald transformation at these parameters.')
  list(z=z,log_jacobian=lj,dz=dz,density_derivative=dd)
}

# Returns an evaluator with analytic likelihood/centering derivatives.
# Each student's shared effect is integrated BEFORE taking the log likelihood.
wald_latency_likelihood <- function(rt,p,subject_id,contamination_weight=.02,
                                    contamination_upper=120,nodes=31L) {
  p <- wald_latency_probability(p)
  if(length(rt)!=length(p) || length(rt)!=length(subject_id) || !length(rt) ||
     anyNA(subject_id) || any(!is.finite(rt)) || any(rt<=0) ||
     any(!is.finite(p)) || any(p<=0 | p>=1))
    stop('Aligned positive RTs, probabilities in (0,1), and student IDs are required.')
  if(length(contamination_weight)!=1L || !is.finite(contamination_weight) ||
     contamination_weight<0 || contamination_weight>=1 ||
     length(contamination_upper)!=1L || !is.finite(contamination_upper) ||
     contamination_upper!=120 || any(rt>contamination_upper))
    stop('Use a contamination weight in [0,1) and the specified (0,120] second input support.')
  ids <- unique(as.character(subject_id)); sid <- match(as.character(subject_id),ids)
  ix <- order(sid); rt <- as.numeric(rt[ix]); A <- qlogis(p[ix]); sid <- sid[ix]
  rule <- wald_normal_rule(nodes); S <- length(ids); n <- length(rt)
  sum_group <- function(x) rowsum(x,sid,reorder=FALSE)
  logguess <- if(contamination_weight>0) log(contamination_weight/contamination_upper) else -Inf
  logadd <- function(a,b) {high<-pmax(a,b); high+log1p(exp(pmin(a,b)-high))}
  evaluate <- function(k,lambda0,beta,tau,t0,details=FALSE) {
    if(!is.finite(beta) || !is.finite(tau) || tau<=0) stop('Invalid activation or student SD.')
    tr <- wald_latency_transform(rt,k,lambda0,t0)
    z <- tr$z; lj <- tr$log_jacobian; r <- z+beta*A
    posterior <- function(u,derivative=FALSE) {
      m <- r-u[sid,,drop=FALSE]
      le <- log1p(-contamination_weight)+lj-.5*m*m-.5*log(2*pi)
      lt <- logadd(le,logguess); resp <- exp(le-lt)
      lp <- sum_group(lt)-.5*(u/tau)^2-log(tau)-.5*log(2*pi)
      if(!derivative) return(lp)
      list(lp=lp,g=sum_group(resp*m)-u/tau^2,
           h=sum_group(resp*(1-resp)*m*m-resp)-1/tau^2)
    }
    ok <- is.finite(lj)
    counts <- as.numeric(sum_group(as.numeric(ok)))
    sums <- as.numeric(sum_group(ifelse(ok,r,0)))
    mode <- matrix(sums/(counts+1/tau^2),S,1L)
    for(iter in seq_len(60L)) {
      post <- posterior(mode,TRUE)
      step <- pmax(-1,pmin(1,post$g/pmax(-post$h,1/tau^2)))
      if(max(abs(post$g))<1e-8) break
      cand <- mode+step; cp <- posterior(cand)
      for(backtrack in seq_len(25L)) {
        bad <- cp<post$lp-1e-12
        if(!any(bad)) break
        step[bad] <- step[bad]*.5; cand <- mode+step; cp <- posterior(cand)
      }
      mode <- cand
      if(max(abs(step))<1e-10) break
    }
    post <- posterior(mode,TRUE)
    if(any(!is.finite(post$h)) || any(post$h>=0) || max(abs(post$g))>1e-4)
      stop('Wald student posterior mode not resolved.')
    sd <- 1/sqrt(-post$h[,1L]); u <- mode[,1L]+outer(sd,rule$x)
    lp <- posterior(u)
    lw <- sweep(lp,2L,rule$logw+.5*rule$x^2,'+')+log(sd)+.5*log(2*pi)
    high <- apply(lw,1L,max); L <- high+log(rowSums(exp(lw-high)))
    weights <- exp(lw-L); means <- rowSums(weights*u)
    m <- r-u[sid,,drop=FALSE]
    le <- log1p(-contamination_weight)+lj-.5*m*m-.5*log(2*pi)
    resp <- exp(le-logadd(le,logguess))
    scores <- lapply(seq_len(3L),function(j)
      sum_group(resp*((z-m)*tr$dz[,j]+tr$density_derivative[,j])))
    G <- list(scores[[1L]],sum_group(-resp*m*A),u*u/tau^2-1,scores[[2L]],scores[[3L]])
    partial <- -vapply(G,function(g)sum(weights*g),numeric(1))
    center_derivative <- vapply(G,function(g)mean(rowSums(weights*(u-means)*g)),numeric(1))
    out <- list(nll=-sum(L),center=mean(means),center_derivative=center_derivative,
                nll_partial=partial,u=means,
                u_sd=sqrt(rowSums(weights*(u-means)^2)),mode_score_max=max(abs(post$g)))
    if(details) {
      ordered_probability <- rowSums(weights[sid,,drop=FALSE]*(1-resp))
      out$contamination_posterior <- ordered_probability[order(ix)]
    }
    out
  }
  centered <- function(theta,details=FALSE,t0_fixed=NULL) {
    if(length(theta)!=(if(is.null(t0_fixed))4L else 3L) || any(!is.finite(theta)))
      stop('Invalid Wald optimizer coordinates.')
    k <- exp(theta[1L]); beta <- theta[2L]; tau <- exp(theta[3L])
    max_t <- max(rt)
    t0 <- if(is.null(t0_fixed)) max_t*plogis(theta[4L]) else t0_fixed
    loglambda <- log(median(rt))
    for(iter in seq_len(40L)) {
      out <- evaluate(k,exp(loglambda),beta,tau,t0,details)
      if(abs(out$center)<2e-10) break
      slope <- out$center_derivative[4L]
      if(!is.finite(slope) || abs(slope)<1e-8) stop('Unidentified Wald centering equation.')
      loglambda <- loglambda-max(-.8,min(.8,out$center/slope))
      if(iter==40L) stop('Wald centering equation did not converge.')
    }
    use <- if(is.null(t0_fixed)) c(1L,2L,3L,5L) else 1:3
    gradient <- out$nll_partial[use]-out$nll_partial[4L]*out$center_derivative[use]/out$center_derivative[4L]
    if(is.null(t0_fixed)) gradient[4L] <- gradient[4L]*t0*(1-t0/max_t)
    c(out,list(k=k,lambda0=exp(loglambda),beta=beta,tau=tau,t0=t0,gradient=gradient))
  }
  list(evaluate=evaluate,centered=centered,ids=ids,order=ix)
}

fit_wald_activation <- function(rt,p,subject_id,contamination_weight=.02,
                                t0_fixed=NULL,control=list()) {
  p <- wald_latency_probability(p)
  # Defaults reproduce the research fitting specification, including explicit
  # t0=0 boundary fitting and positive-shift starts. No guessing is simulated.
  defaults <- list(nodes=31L,maxit=250L,reltol=1e-10,trace=0L,
                   start=c(k=2,beta=.23,tau=.8),t0_starts=c(.50037,1.50037,2.50037))
  if(length(setdiff(names(control),names(defaults)))) stop('Unknown Wald fitting control.')
  ctl <- utils::modifyList(defaults,control)
  if(length(ctl$start)!=3L || any(!is.finite(ctl$start)) || ctl$start[1L]<=0 || ctl$start[3L]<=0)
    stop('Wald start must contain k>0, beta, tau>0.')
  if(!is.null(t0_fixed) && (length(t0_fixed)!=1L || !is.finite(t0_fixed) || t0_fixed<0 || t0_fixed>=max(rt)))
    stop('Fixed t0 must be nonnegative and below the largest retained RT.')
  likelihood <- wald_latency_likelihood(rt,p,subject_id,contamination_weight,nodes=ctl$nodes)
  start <- c(log(ctl$start[1L]),ctl$start[2L],log(ctl$start[3L]))
  run <- function(initial,fixed,label) {
    cache_x <- NULL; cache <- NULL; rejected <- 0L
    value <- function(theta) {
      if(!identical(theta,cache_x)) {
        cache_x <<- theta
        cache <<- tryCatch(likelihood$centered(theta,t0_fixed=fixed),error=function(e) {
          rejected <<- rejected+1L
          list(nll=1e100,gradient=rep(0,length(theta)),invalid=conditionMessage(e))
        })
      }
      cache
    }
    first <- value(initial)
    if(!is.null(first$invalid)) stop('Invalid Wald starting point: ',first$invalid)
    opt <- optim(initial,function(x)value(x)$nll,function(x)value(x)$gradient,
                 method='BFGS',control=list(maxit=ctl$maxit,reltol=ctl$reltol,trace=ctl$trace))
    final <- likelihood$centered(opt$par,TRUE,t0_fixed=fixed)
    list(opt=opt,fit=final,label=label,rejected_evaluations=rejected,fixed=fixed)
  }
  fits <- list(run(start,if(is.null(t0_fixed))0 else t0_fixed,'fixed_boundary'))
  if(is.null(t0_fixed)) {
    if(any(!is.finite(ctl$t0_starts)) || any(ctl$t0_starts<=0)) stop('Positive finite t0 starts required.')
    for(t0 in ctl$t0_starts[ctl$t0_starts<max(rt)])
      fits[[length(fits)+1L]] <- run(c(fits[[1L]]$opt$par,qlogis(t0/max(rt))),NULL,paste0('t0_start_',t0))
  }
  losses <- vapply(fits,function(v)v$fit$nll,numeric(1)); best <- fits[[which.min(losses)]]
  v <- best$fit; ids <- likelihood$ids
  calibration <- list(schema=1L,family='Wald',centering='equal_student',
    parameters=list(k=v$k,lambda0=v$lambda0,beta=v$beta,tau=v$tau,t0=v$t0,
                    nll=v$nll,weight=contamination_weight,uniform_upper=120),
    rt=as.numeric(rt),p=as.numeric(p),subject_id=as.character(subject_id),
    subject_effects=setNames(v$u,ids),subject_effect_sd=setNames(v$u_sd,ids),
    n_free_parameters=if(is.null(t0_fixed))4L else 3L,
    source='LKT direct centered Wald marginal likelihood',
    filtering='Caller supplies latency-screened data; uniform allowance is fit-only.')
  result <- import_wald_calibration(calibration,rt,p,subject_id)
  result$conv <- best$opt$convergence
  result$optimizer <- list(method='BFGS',nodes=ctl$nodes,selected_start=best$label,
    convergence=best$opt$convergence,max_gradient=max(abs(v$gradient)),
    starts=lapply(fits,function(a)list(label=a$label,nll=a$fit$nll,convergence=a$opt$convergence,
      max_gradient=max(abs(a$fit$gradient)),rejected_evaluations=a$rejected_evaluations)))
  result$contamination_posterior <- v$contamination_posterior
  result$calibration <- calibration
  if(best$opt$convergence!=0L) warning('Wald optimizer did not converge; inspect $optimizer before using the fit.')
  result
}



import_wald_calibration <- function(calibration,rt,p,subject_id) {
  p <- wald_latency_probability(p)
  if(!is.list(calibration)||!identical(calibration$schema,1L)||
     !identical(calibration$family,'Wald')||!identical(calibration$centering,'equal_student')) stop('A verified centered Wald calibration is required.')
  # Accuracy coefficients are recalibrated by platform-specific R/BLAS. Bound
  # both response-probability and latency-score differences; do not alter them.
  probability_tolerance <- 1e-6
  score_tolerance <- 1e-5
  if(!identical(as.character(subject_id),calibration$subject_id)||length(rt)!=length(calibration$rt)||
     max(abs(rt-calibration$rt))>1e-9||max(abs(p-calibration$p))>probability_tolerance||
     calibration$parameters$beta*max(abs(qlogis(p)-qlogis(calibration$p)))>score_tolerance)
    stop('Wald calibration mismatch: n=',length(rt),'/',length(calibration$rt),'; subject order identical=',identical(as.character(subject_id),calibration$subject_id),'; max RT difference=',if(length(rt)==length(calibration$rt)) max(abs(rt-calibration$rt)) else NA_real_,'; max probability difference=',if(length(p)==length(calibration$p)) max(abs(p-calibration$p)) else NA_real_)
  v<-calibration$parameters
  if(any(!is.finite(unlist(v)))||v$k<=0||v$lambda0<=0||v$tau<=0||v$t0<0||v$weight<0||v$weight>=1||v$uniform_upper!=120)
    stop('Invalid calibrated Wald parameters.')
  ids<-as.character(subject_id);effects<-calibration$subject_effects
  if(anyNA(effects[ids])||abs(mean(effects))>1e-8) stop('Invalid centered student effects.')
  # Capture every helper so saved models work in a fresh R worker.
  wald_logtails<-wald_logtails;wald_latency_from_score<-wald_latency_from_score
  environment(wald_latency_from_score)<-environment()
  wald_latency_probability<-wald_latency_probability
  wald_quantile_latency<-wald_quantile_latency;environment(wald_quantile_latency)<-environment()
  max_rt<-max(rt)
  normal_rule<-function(n) {
    J<-matrix(0,n,n);j<-seq_len(n-1L);J[cbind(j,j+1L)]<-sqrt(j)
    e<-eigen(J+t(J),symmetric=TRUE);list(x=e$values,w=e$vectors[1,]^2)
  }
  mean_latency<-function(pnew,student_effect=0,extra_student_sd=0,nodes=41L) {
    rule<-normal_rule(nodes)
    values<-vapply(seq_along(rule$x),function(j) rule$w[j]*wald_quantile_latency(
      pnew,student_effect,sqrt(1+extra_student_sd^2)*rule$x[j],v$beta,v$k,v$lambda0,v$t0),numeric(length(pnew)))
    rowSums(matrix(values,nrow=length(pnew)))
  }
  predict_fun<-function(pnew,subject_effect=0,add_noise=FALSE,truncate=TRUE,return_components=FALSE) {
    pnew <- wald_latency_probability(pnew)
    if(!add_noise) {
      if(return_components) stop('Expected RT integrates noise and has no single generating score.')
      return(mean_latency(pnew,subject_effect))
    }
    trial_noise<-rnorm(length(pnew))
    z<--v$beta*qlogis(pnew)+subject_effect+trial_noise
    value<-wald_latency_from_score(z,v$k,v$lambda0,v$t0)
    if(truncate) value<-pmin(value,max_rt)
    if(return_components) return(list(rt=value,trial_noise=trial_noise,latent_score=z,
      log_lambda=rep(log(v$lambda0),length(pnew)),lambda=rep(v$lambda0,length(pnew)),quantile_u=pnorm(z)))
    value
  }
  fitted<-mean_latency(p,effects[ids]);variance<-c(activation=v$beta^2*var(qlogis(p)),student=v$tau^2,trial=1)
  list(estimator='normal_score_wald_quantile',distribution='Wald',k=v$k,lambda0=v$lambda0,beta1=v$beta,
    t0=v$t0,sigma_subject=v$tau,scale_subject=1,sigma_trial=1,
    contamination_weight=v$weight,contamination_upper=v$uniform_upper,
    generation='engaged_only',student_centering='equal_student',centering_residual=mean(effects),
    subject_effects=effects,subject_effect_sd=calibration$subject_effect_sd,predict=predict_fun,
    mean_latency=mean_latency,max_rt=max_rt,n_obs=length(rt),n_subjects=length(effects),
    n_free_parameters=calibration$n_free_parameters,negative_log_likelihood=v$nll,
    r_squared=1-sum((rt-fitted)^2)/sum((rt-mean(rt))^2),
    var_partition=as.list(variance/sum(variance)),conv=0L,
    calibration_source=calibration$source)
}

