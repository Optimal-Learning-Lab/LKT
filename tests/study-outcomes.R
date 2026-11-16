library(LKT)
library(data.table)
f <- function(name)getFromNamespace(name,'LKT')
close <- function(a,b)stopifnot(isTRUE(all.equal(as.numeric(a),as.numeric(b),tolerance=1e-8)))
reject <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))

# Outcome overrides legacy binary codes, including studies previously coded zero.
d <- data.table(Anon.Student.Id='s',KC='k',
  Outcome=c('STUDY','CORRECT','STUDY','STUDY','INCORRECT','STUDY','CORRECT'),
  CF..ansbin.=c(0,0,1,0,1,0,0),CF..Time.=c(0,10,20,25,30,40,60),
  CF..reltime.=c(0,10,20,25,30,40,60))
p <- f('lkt_prepare_lkt_data')(copy(d),'KC','recencystudy',FALSE,FALSE)
close(p$CF..ansbin.,c(NA,1,NA,NA,0,NA,1))
stopifnot(identical(p$Outcome,d$Outcome))
reject(f('lkt_normalize_outcomes')(data.frame(Outcome='OTHER')))
reject(f('lkt_normalize_outcomes')(data.frame(CF..ansbin.=NA_real_)))
binary <- f('lkt_normalize_outcomes')(data.frame(CF..ansbin.=c(0,1)))
stopifnot(identical(binary$Outcome,c('INCORRECT','CORRECT')))

compute <- function(data,feature) {
  data <- f('lkt_prepare_lkt_data')(copy(data),'KC',feature,FALSE,FALSE)
  data <- f('lkt_prepare_component_context')(data,'KC',feature)
  data$pred_ed <- .6
  computefeatures(data,feature,.5,.5,data$index,data$indexcomp,.5,.5,.5,'KC')
}
scored <- which(p$Outcome!='STUDY')
response_features <- c('linesuc','linefail','logsuc','logfail',
  'logit','prop','expdecsuc','expdecfail','propdec','propdec2','logitdec',
  'logitdecevol','errordec','recencysuc','recencyfail','dashsuc','dashfail')
for(feature in response_features) {
  x <- compute(p,feature)
  stopifnot(all(is.finite(x)))
  close(x[scored],compute(p[scored],feature))
}
close(compute(p,'expdecfail'),c(0,0,0,0,0,1,1))
stopifnot(compute(p,'recencystudy')[5]>0,compute(p,'recencytest')[5]==0,
  compute(p,'recencyfail')[5]==0,compute(p,'recencysuc')[5]>0,
  compute(p,'expdecafm')[7]!=compute(p[scored],'expdecafm')[3])

# All AFM opportunity histories include studies, successes and failures. The
# current exposure is excluded, as is every other student's/component's history.
opportunities <- seq_len(nrow(p))-1L
close(compute(p,'lineafm'),opportunities)
close(compute(p,'logafm'),log1p(opportunities))
close(compute(p,'powafm'),sqrt(opportunities))
close(compute(p,'expdecafm'),2*(1-.5^opportunities))
dash <- vapply(seq_len(nrow(p)),function(i) {
  prior <- seq_len(i-1L)
  sum(exp(-(p$CF..Time.[i]-p$CF..Time.[prior])/(.5*86400)))
},numeric(1))
close(compute(p,'dashafm'),log1p(dash))
groups <- data.table(Anon.Student.Id=c('a','b','a','a','b','a'),
  KC=c('x','x','y','x','x','y'),
  Outcome=c('STUDY','STUDY','INCORRECT','CORRECT','INCORRECT','STUDY'),
  CF..Time.=seq_len(6),CF..reltime.=seq_len(6))
close(compute(groups,'lineafm'),c(0,0,0,1,1,1))
selected <- copy(p);selected$source <- c('yes',NA,'no','YES','no','yes','yes')
selected$target <- c('yes','no','yes','yes','no','yes','yes')
for(component in c('KC%source%yes','target?yes?source?yes')) {
  prepared <- f('lkt_prepare_component_context')(copy(selected),component,'lineafm')
  values <- computefeatures(prepared,'lineafm',.5,.5,prepared$index,prepared$indexcomp,.5,.5,.5,component)
  expected <- c(0,1,1,1,2,2,3)
  if(grepl('?',component,fixed=TRUE))expected <- expected*(selected$target=='yes')
  close(values,expected)
}
relabelled <- copy(p);relabelled$Outcome <- rep(c('INCORRECT','CORRECT'),length.out=nrow(p))
all_studies <- copy(p);all_studies$Outcome <- 'STUDY'
for(feature in c('lineafm','logafm','powafm','expdecafm','dashafm','ppe')) {
  close(compute(p,feature),compute(relabelled,feature))
  close(compute(p,feature),compute(all_studies,feature))
}

# PPE's repetition factor and spacing denominator use the same full exposure
# history. Calculate an independent reference directly from the prior times.
ppe <- vapply(seq_len(nrow(p)),function(i) {
  prior <- seq_len(i-1L)
  if(!length(prior))return(0)
  lags <- p$CF..Time.[i]-p$CF..Time.[prior]
  tw <- sum(lags*lags^(-.5))/sum(lags^(-.5))
  gaps <- diff(p$CF..Time.[prior])
  space <- if(length(gaps))mean(1/log(gaps+exp(1))) else 0
  length(prior)^.5*tw^(-(.5+.5*space))
},numeric(1))
close(compute(p,'ppe'),ppe)
stopifnot(any(abs(compute(p,'ppe')[scored]-compute(p[scored],'ppe'))>1e-8))
# Long and interleaved histories retain the 60-response window, not 60 rows.
long <- rbindlist(lapply(c('a','b'),function(id)data.table(Anon.Student.Id=id,KC='k',
  Outcome=rep(c('CORRECT','STUDY','INCORRECT','STUDY'),40),CF..Time.=seq_len(160))))
long <- long[order(CF..Time.,Anon.Student.Id)]
keep <- long$Outcome!='STUDY'
close(compute(long,'logitdec')[keep],compute(long[keep],'logitdec'))

# Study rows retain predictions but do not enter fitting, likelihood or metrics.
fit <- LKT(copy(p),components='KC',features='logitdec',fixedpars=.5,interc=TRUE,verbose=FALSE)
ref <- LKT(copy(p[scored]),components='KC',features='logitdec',fixedpars=.5,interc=TRUE,verbose=FALSE)
close(fit$prediction[scored],ref$prediction)
close(fit$loglike,ref$loglike)
stopifnot(length(fit$prediction)==nrow(p),all(is.finite(fit$prediction)),
  stats::nobs(fit$nullmodel)==length(scored))
stats <- predict_lkt(fit,fit$newdata,return_stats=TRUE)
close(stats$LL,fit$loglike)
close(stats$RMSE,sqrt(mean((p$CF..ansbin.[scored]-fit$prediction[scored])^2)))
folded <- copy(p);folded$fold <- c(1,1,2,1,1,2,2)
foldfit <- LKT(folded,components='KC',features='logitdec',fixedpars=.5,
  interc=TRUE,verbose=FALSE,usefolds=1)
close(foldfit$loglike,sum(dbinom(foldfit$newdata$CF..ansbin.[c(2,5)],1,foldfit$prediction[c(2,5)],log=TRUE)))

online <- OnlineCalibrationModel(beta_start=0,alpha_start=.1,alpha_lower=.1,alpha_upper=.1)
a <- LKT(copy(p),components='KC',features='logitdec',fixedpars=.5,interc=TRUE,verbose=FALSE,model=online)
b <- LKT(copy(p[scored]),components='KC',features='logitdec',fixedpars=.5,interc=TRUE,verbose=FALSE,model=online)
close(a$prediction[scored],b$prediction);close(a$loglike,b$loglike)
adaptive <- OnlineAdaptiveModel(online_mode='alpha_only',beta_alpha_terms='logitdecKC',
  alpha_start=.05,alpha_lower=.05,alpha_upper=.05)
a <- LKT(copy(p),components='KC',features='logitdec',fixedpars=.5,interc=TRUE,verbose=FALSE,model=adaptive)
b <- LKT(copy(p[scored]),components='KC',features='logitdec',fixedpars=.5,interc=TRUE,verbose=FALSE,model=adaptive)
close(a$prediction[scored],b$prediction);close(a$loglike,b$loglike)
stopifnot(all(is.finite(a$prediction)))

# Native simple-adaptive loss/gradient must skip the same unscored events.
input <- list(y=c(NA,1,NA,0,NA,1),x_recency=rep(.3,6),x_logsuc=rep(.4,6),
  x_logitdec=rep(.2,6),subject_start=1L,subject_end=6L,subject='s')
pars <- c(-.2,.3,.4,.5,.05,.07)
native <- LKTOnlineSimpleAdaptiveEval(pars,input,return_details=TRUE,require_native=TRUE)
reference <- f('lkt_online_simple_adaptive_eval_r')(pars,input,clip_epsilon=1e-6,
  denom_epsilon=1e-6,return_details=TRUE)
close(native$pred,reference$pred);close(native$nll,reference$nll)
gradient <- LKTOnlineSimpleAdaptiveGradient(pars,input,require_native=TRUE)
numeric_gradient <- vapply(seq_along(pars),function(j) {
  up <- down <- pars;up[j] <- up[j]+1e-6;down[j] <- down[j]-1e-6
  (LKTOnlineSimpleAdaptiveEval(up,input,require_native=TRUE)-
     LKTOnlineSimpleAdaptiveEval(down,input,require_native=TRUE))/2e-6
},numeric(1))
close(gradient,numeric_gradient)

mu <- c('(Intercept)'=0,logitdecKC=.2)
covariance <- diag(.1,2);dimnames(covariance) <- list(names(mu),names(mu))
bayesian <- OnlineBayesianModel(prior_mean=mu,prior_covariance=covariance,
  n_particles=64,seed=3,ess_action='ignore')
a <- LKT(copy(p),components='KC',features='logitdec',fixedpars=.5,interc=TRUE,verbose=FALSE,model=bayesian)
b <- LKT(copy(p[scored]),components='KC',features='logitdec',fixedpars=.5,interc=TRUE,verbose=FALSE,model=bayesian)
close(a$prediction[scored],b$prediction);close(a$loglike,b$loglike)
stopifnot(a$model$final_states$s$updates==length(scored))
cat('PASS authoritative outcomes, scored histories, exposure histories, fitting and statistics\n')
