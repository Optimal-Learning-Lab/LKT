# Verify freezing disables NL optimization and Wald attachment never changes accuracy.
a <- commandArgs(TRUE);stopifnot(length(a)==1L);root <- a[1]
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/fixed-cloze-calibration-helpers.R'))
fake <- list(optimizedpars=list(par=c(.1,.2,.3,.4,.5,.6),convergence=0L))
config <- cloze_fixed_configuration(fake,c('Anon.Student.Id','KC..Default.','KC..Default.'),c('logitdec','ppes','logitdec'))
opt <- getFromNamespace('lkt_lkt_optimization_inputs','LKT')(config$fixedpars,config$seedpars,6L)
stopifnot(opt$parameter_count==0L,isTRUE(all.equal(unname(config$fixedpars),fake$optimizedpars$par)))
args <- getFromNamespace('lkt_config_arguments','LKT')(config)
stopifnot(identical(args$components,c('Anon.Student.Id','KC..Default.','KC..Default.')),
  identical(args$features,c('logitdec','ppes','logitdec')),all(is.finite(args$fixedpars)))
gt <- cloze_fixed_configuration(list(optimizedpars=list(par=(1:8)/10,convergence=0L)),
  c('Anon.Student.Id','KC..Default.','KC..Default.','KC..Default.'),
  c('intercept','ppes','intercept','ppef'),'Wald')
stopifnot(length(gt$fixedpars)==8L,
  getFromNamespace('lkt_lkt_optimization_inputs','LKT')(gt$fixedpars,gt$seedpars,8L)$parameter_count==0L)
fit <- list(model=list(saved=TRUE),coefs=matrix(.7),prediction=c(.3,.4,.5,.6),predictors=matrix(1,4,1),
  optimizedpars=fake$optimizedpars,model_specification=list(data.frame(coefficient=.7)),
  latencymodel=NULL,newdata=data.frame(CF..ansbin.=c(1,1,1,0),Duration..sec.=c(4,5,6,7),
    pred=c(.3,.4,.5,.6),Anon.Student.Id=c('a','a','b','b')))
original <- serialize(fit,NULL);calls <- 0L
mock <- function(...) {calls <<- calls+1L;list(model=list(generation='engaged_only'))}
result <- cloze_append_wald_once(fit,fit_latency=mock)
stopifnot(calls==1L,identical(original,serialize(fit,NULL)),identical(result$prediction,fit$prediction),
  identical(result$coefs,fit$coefs),identical(result$predictors,fit$predictors),
  inherits(tryCatch(cloze_append_wald_once(result,fit_latency=mock),error=identity),'error'),calls==1L)
cat('PASS frozen nonlinear parameters disable optimization; Wald called once; accuracy and source unchanged\n')
