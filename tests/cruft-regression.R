library(LKT)
library(data.table)
f <- function(name) getFromNamespace(name, 'LKT')
expect_error <- function(expr, pattern) {
  e <- tryCatch({force(expr); NULL}, error = identity)
  stopifnot(inherits(e, 'error'), grepl(pattern, conditionMessage(e), fixed = TRUE))
  invisible(e)
}
cat('Testing isolated LKT', as.character(packageVersion('LKT')), find.package('LKT'), '\n')
# 1. Reused features must equal independent fresh calculations at each vector.
d <- data.table(Anon.Student.Id='S', KC='K', CF..ansbin.=c(1,1,0,1),
                Outcome=c('CORRECT','CORRECT','INCORRECT','CORRECT'),
                CF..Time.=c(0,10,1000,2000), CF..reltime.=c(0,10,20,30))
iteration <- function(dat, seed, first, features='base2', components='KC', fixed=c(.3,NA)) {
  f('lkt_build_feature_iteration')(dat,components,features,rep(NA,length(features)),fixed,seed,
    rep('+',length(features)),rep(NA,length(features)),FALSE,first,FALSE,FALSE,FALSE,list())
}
a <- iteration(copy(d),.1,TRUE)
b <- iteration(copy(a$data),.9,FALSE)
c <- iteration(copy(d),.9,TRUE)
stopifnot(identical(b$data$base2KC,c$data$base2KC),!identical(a$data$base2KC,b$data$base2KC))
a <- iteration(copy(d),.2,TRUE,c('logitdec','logitdec'),c('KC','Anon.Student.Id'),c(NA,1))
b <- iteration(copy(a$data),.8,FALSE,c('logitdec','logitdec'),c('KC','Anon.Student.Id'),c(NA,1))
c <- iteration(copy(d),.8,TRUE,c('logitdec','logitdec'),c('KC','Anon.Student.Id'),c(NA,1))
stopifnot(identical(b$data$logitdecAnon.Student.Id,c$data$logitdecAnon.Student.Id),
          !identical(a$data$logitdecAnon.Student.Id,b$data$logitdecAnon.Student.Id))
cat('PASS partial-free and tied parameter feature invalidation\n')
# 2. Final state must be at the returned optimum, even after derivative probes.
last <- NULL; calls <- 0L
objective <- function(x) { last <<- x; calls <<- calls+1L; sum((x-.37)^2) }
o <- f('lkt_optimize_lkt_parameters')(NA,.5,1L,objective,.01,.99,30,1e7)
stopifnot(identical(last,o$par),abs(o$par-.37)<1e-8,o$value==objective(o$par))
x <- data.table(Anon.Student.Id=rep(paste0('s',1:4),each=20),
                KC=rep(c('k1','k2'),40),CF..ansbin.=rep(c(0,1,1,0,1,0,1,1,1,0),8))
x[,Outcome:=ifelse(CF..ansbin.==1,'CORRECT','INCORRECT')]
set.seed(77)
m <- LKT(copy(x),components='KC',features='logitdec',interc=TRUE,
         fixedpars=NA,seedpars=.5,verbose=FALSE,epsilon=1e-9,factrv=1e7,maxitv=30)
set.seed(77)
fixed <- LKT(copy(x),components='KC',features='logitdec',interc=TRUE,
             fixedpars=m$optimizedpars$par,verbose=FALSE,epsilon=1e-9)
stopifnot(max(abs(m$prediction-fixed$prediction))<1e-9,
          max(abs(m$coefs-fixed$coefs))<1e-8,
          abs(m$loglike-fixed$loglike)<1e-8,
          abs(m$optimizedpars$value+m$loglike)<1e-10,
          all(m$model_specification[[1L]][feature=='logitdec',para]==m$optimizedpars$par))
cat('PASS returned optimizer point, stored coefficients, predictions, metadata and likelihood agree\n')
# 3. Online predictions are sequential; prediction-only requests need no labels.
for (name in c('OnlineCalibration','OnlineAdaptive')) {
  object <- list(model_name=name,prediction=c(.2,.8,.3,.7))
  no_labels <- data.frame(fold=c(1,2,1,2))
  stopifnot(identical(predict_lkt(object,no_labels)$predictions,object$prediction),
    identical(predict_lkt(object,no_labels,fold=2)$predictions,c(.8,.7)))
  labels <- data.frame(CF..ansbin.=c(0,1,0,1))
  stats <- predict_lkt(object,labels,return_stats=TRUE)
  stopifnot(abs(stats$LL-sum(log(c(.8,.8,.7,.7))))<1e-12,stats$AUC==1,
    abs(stats$RMSE-sqrt(mean(c(.2,.2,.3,.3)^2)))<1e-12)
  single <- predict_lkt(object,data.frame(CF..ansbin.=rep(1,4)),return_stats=TRUE)
  stopifnot(is.na(single$AUC),is.na(single$R2),is.finite(single$LL),is.finite(single$RMSE))
  expect_error(predict_lkt(object,labels[1:2,,drop=FALSE]),'original order')
}
object <- list(coefs=matrix(1,ncol=1,dimnames=list('x','coefficient')),
               predictors=matrix(c(-1,1),ncol=1),model_name='LibLinear')
stopifnot(identical(predict_lkt(object,data.frame(row=1:2))$predictions,plogis(c(-1,1))))
stopifnot(length(predict_lkt(object,data.frame(CF..ansbin.=c(1,1)))$predictions)==2)
expect_error(predict_lkt(object,data.frame(row=1:2),return_stats=TRUE),'require CF..ansbin.')
expect_error(predict_lkt(object,data.frame(row=1:2),min_pred_limit=1),'bounds')
expect_error(predict_lkt(object,data.frame(row=1:2),fold=1),'Fold selection')
adaptive <- LKT(copy(x), components=c('KC','KC'), features=c('logitdec','logsuc'),
  fixedpars=.7, interc=TRUE, verbose=FALSE,
  model=OnlineAdaptiveModel(online_mode='alpha_only', beta_alpha_terms='logsucKC',
    alpha_start=.05, alpha_lower=.05, alpha_upper=.05))
stopifnot(identical(predict_lkt(adaptive,adaptive$newdata)$predictions,adaptive$prediction),
  max(abs(adaptive$prediction-as.numeric(plogis(adaptive$predictors %*% adaptive$coefs[,1]))))>1e-6)
cat('PASS sequential/static predictions, fold subsets, prediction-only and single-class statistics\n')
# 4. Distinct student-item pairs must never exchange outcome or time histories.
z <- data.table(Anon.Student.Id=c('3','23','3','23'),KC=c('12','1','12','1'),
                CF..ansbin.=c(1,0,0,1),Outcome=c('CORRECT','INCORRECT','INCORRECT','CORRECT'),
                CF..Time.=c(0,100,200,400),CF..reltime.=c(0,100,200,400))
ctx <- f('lkt_prepare_component_context')(copy(z),'KC','linesuc')
spacing <- computeSpacingPredictors(copy(z),'KC')
stopifnot(identical(as.numeric(ctx$cor),c(0,0,1,0)),identical(spacing$KCspacing,c(0,0,200,300)))
k <- f('lkt_history_index')(c('12','1','1:2','1','e\u00e9'),c('3','23','3','2:3','s'))
stopifnot(!anyDuplicated(k))
expect_error(f('lkt_history_index')(c('a',NA),c('s','s')),'nonmissing')
cat('PASS collision-safe counts, spacings, separator/UTF-8 keys and invalid IDs\n')
# 5. Study/test features distinguish previous outcomes within each item history.
z <- data.table(Anon.Student.Id='s',KC=rep(c('k1','k2'),3),
                Outcome=c('STUDY','INCORRECT','CORRECT','STUDY','CORRECT','INCORRECT'),
                CF..ansbin.=c(0,0,1,0,1,0),CF..Time.=seq(0,50,10),CF..reltime.=seq(0,50,10))
p <- f('lkt_prepare_lkt_data')(copy(z),'KC','recencystudy',FALSE,FALSE)
stopifnot(identical(as.logical(p$KCpreviousstudy),c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE)))
p <- f('lkt_prepare_component_context')(p,'KC','recencystudy')
study <- computefeatures(p,'recencystudy',.5,NA,p$index,p$indexcomp,NA,NA,NA,'KC')
test <- computefeatures(p,'recencytest',.5,NA,p$index,p$indexcomp,NA,NA,NA,'KC')
stopifnot(max(abs(as.numeric(study)-c(0,0,1/sqrt(20),0,0,1/sqrt(20))))<1e-12,
          max(abs(as.numeric(test)-c(0,0,0,1/sqrt(20),1/sqrt(20),0)))<1e-12)
for (feat in c('base4','base5suc','base5fail','recencytest','recencystudy')) {
  count <- f('lkt_feature_param_counts')(feat)
  model <- LKT(copy(z),components='KC',features=feat,fixedpars=rep(.5,count),interc=TRUE,verbose=FALSE)
  stopifnot(length(model$prediction)==nrow(z),all(is.finite(model$prediction)))
}
binary <- computeSpacingPredictors(copy(z[,!'Outcome']),'KC')
stopifnot(!any(binary$KCpreviousstudy))
cat('PASS automatic feature preparation and preserved study/test meaning\n')
# 6. Correct RMS error and exact literal metadata matching.
r <- f('lkt_student_rmse')(data.table(Anon.Student.Id=c('a','a'),pred=c(.1,.5),CF..ansbin.=c(0,1)))
stopifnot(abs(r[[2]]-sqrt(.13))<1e-12)
coef <- setNames(c(1,2,3),c('baseKCk1','base2KCk1','baseKC_long.k1'))
tr <- list(list(feature='base',component='KC',pars=.2),
           list(feature='base2',component='KC',pars=c(.3,.4)),
           list(feature='base',component='KC_long',pars=.6))
sp <- f('build_model_specification')(coef,c('base','base2','base'),c('KC','KC','KC_long'),tr)
stopifnot(identical(sp$feature,c('base','base2','base')),identical(sp$component,c('KC','KC','KC_long')),
          identical(sp$para,c(.2,.3,.6)),identical(sp$component_level,c('k1','k1','.k1')),sp$parb[2]==.4)
cat('PASS student RMSE and overlapping feature/component coefficient metadata\n')
# 7. An internal backend error must propagate once, without an API retry.
calls <- 0L
error <- structure(list(message='unused argument in nested backend',call=NULL),class=c('backend_probe','error','condition'))
adapter <- LKTCustomModel('error',function(request,verbose=FALSE,...) {calls<<-calls+1L;stop(error)})
caught <- tryCatch(f('lkt_call_model_fit')(adapter,list(response=1)),error=identity)
stopifnot(identical(caught,error),calls==1L)
# A valid documented custom interface still fits through the real LKT driver.
constant <- LKTCustomModel('constant',function(request,verbose=FALSE)LKTModelFit(list(name='constant'),rep(.5,length(request$response))))
model <- LKT(copy(x),components='KC',features='intercept',model=constant,verbose=FALSE)
stopifnot(all(model$prediction==.5),model$model_name=='constant')
request_only <- LKTCustomModel('request-only',function(request)
  LKTModelFit(list(name='constant'),rep(.5,length(request$response))))
stopifnot(identical(f('lkt_call_model_fit')(request_only,list(response=1:3))$pred,rep(.5,3)))
legacy <- LKTCustomModel('old-interface',function(design_matrix,data)stop('must not run'))
expect_error(f('lkt_call_model_fit')(legacy,list(response=1)), 'must accept a request')
cat('PASS strict documented adapter interface and unmodified single error propagation\n')
# Cleanup leaves the active baserate calculation intact and connects prevstudy.
stopifnot(is.function(f('prevstudy')),is.function(f('baseratepropdec')))
for (name in c('getFeedDur','logitdec4','splittimes','texteval'))
  stopifnot(!exists(name,envir=asNamespace('LKT'),inherits=FALSE))
cat('PASS seven repairs and scoped internal cleanup\n')
