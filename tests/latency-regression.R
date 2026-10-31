library(LKT)
library(data.table)
ns <- asNamespace('LKT'); f <- function(name)get(name,ns)
fail <- function(expr) stopifnot(inherits(try(force(expr),silent=TRUE),'try-error'))
# Analytical transforms and explicitly bounded latency probabilities.
p<-c(.2,.5,.9); u<-c(-.3,0,.3); noise<-c(-1,0,1)
z<--.24*qlogis(p)+u+noise
rw<-f('wald_quantile_latency')(p,u,noise,.24,2,12,0)
stopifnot(max(abs(f('wald_latency_transform')(rw,2,12,0)$z-z))<1e-7)
stopifnot(identical(f('wald_latency_probability')(c(.000001,.5,.999999)),c(.000001,.5,.9999)))
# A finite deterministic fixture avoids an expensive experimental simulation.
sid<-rep(paste0('s',1:6),each=16);p<-rep(seq(.25,.9,length.out=16),6)
u<-rep(seq(-.7,.7,length.out=6),each=16);noise<-rep(qnorm((seq_len(16)-.5)/16),6)
rt<-f('wald_quantile_latency')(p,u,noise,.24,2,12,0)
fit<-f('fit_wald_activation')(rt,p,sid,t0_fixed=0,control=list(nodes=15L,maxit=120L))
stopifnot(fit$conv==0L,fit$generation=='engaged_only',fit$contamination_weight==.02,abs(mean(fit$subject_effects))<1e-8)
cal<-fit$calibration;imp<-f('import_wald_calibration')(cal,rt,p,sid)
fail(f('import_wald_calibration')(cal,rt[-1],p[-1],sid[-1]))
set.seed(123);a<-imp$predict(p,u,TRUE,TRUE,TRUE);after<-.Random.seed
set.seed(123);b<-imp$predict(p,u,TRUE,TRUE,TRUE);stopifnot(identical(a,b),identical(after,.Random.seed),all(a$rt<=imp$max_rt))
L<-f('wald_latency_likelihood')(rt,p,sid,nodes=15L)
th<-c(log(2),.24,log(.7));g<-L$centered(th,t0_fixed=0)
fd<-vapply(seq_along(th),function(j){step<-rep(0,length(th));step[j]<-1e-5;(L$centered(th+step,t0_fixed=0)$nll-L$centered(th-step,t0_fixed=0)$nll)/2e-5},numeric(1))
stopifnot(max(abs(fd-g$gradient))<.01)
# Full histories remain present; selected fitting RTs and failures honor both masks.
d<-data.table(Anon.Student.Id=sid,KC..Default.=rep(c('k1','k2'),48),CF..ansbin.=rep(c(1,1,0,1,0,0,1,1),12),CF..Time.=rep(seq(1000,1150,10),6),Duration..sec.=rt,fold=rep(c(1L,2L),48),CF..Question.Type.=rep(c('test','test','other','other'),24))
d[,pred:=p]
ld<-f('lkt_prepare_latency_data')(d,1L,'test')
stopifnot(all(ld$correct_rt %in% d[fold==1L & CF..Question.Type.=='test' & CF..ansbin.==1,Duration..sec.]),nrow(d)==96L)
stopifnot(identical(f('lkt_failure_latency')(d,1L,'test'),mean(d[fold==1L & CF..Question.Type.=='test' & CF..ansbin.==0,Duration..sec.])))
fail(f('lkt_prepare_latency_data')(d,3L));fail(f('lkt_prepare_latency_data')(d,1L,'missing'));fail(f('lkt_prepare_latency_data')(d[,!'CF..Question.Type.'],1L,'test'))
bad<-copy(d);bad[1:4,Duration..sec.:=c(NA,Inf,0,-1)]
stopifnot(all(is.finite(f('lkt_prepare_latency_data')(bad)$clean_rt)),all(f('lkt_prepare_latency_data')(bad)$clean_rt>0))
set.seed(321)
m<-LKT(copy(d),usefolds=1L,components='KC..Default.',features='logitdec',fixedpars=.9,interc=TRUE,dualfit=TRUE,distribution='Wald',verbose=FALSE,latency_question_type_filter='test',wald_t0_fixed=0,wald_control=list(nodes=15L,maxit=120L))
stopifnot(nrow(m$newdata)==96L,m$model_specification[[2]]$distribution=='Wald',all(is.finite(m$latencymodel[[1]]$calibration$p)))
# Excluded RT changes cannot alter either accuracy or selected latency fit.
changed<-copy(d);changed[fold==2L | CF..Question.Type.!='test',Duration..sec.:=99999]
set.seed(321)
m2<-LKT(changed,usefolds=1L,components='KC..Default.',features='logitdec',fixedpars=.9,interc=TRUE,dualfit=TRUE,distribution='Wald',verbose=FALSE,latency_question_type_filter='test',wald_t0_fixed=0,wald_control=list(nodes=15L,maxit=120L))
stopifnot(identical(m$coefs,m2$coefs),identical(m$prediction,m2$prediction),identical(m$latencymodel[[1]]$calibration,m2$latencymodel[[1]]$calibration))
# Component-specific metadata must not overwrite distinct logitdec decays.
co<-setNames(c(.5,.4),c('logitdecAnon.Student.Id','logitdecKC..Default.'))
tr<-list(list(feature='logitdec',component='Anon.Student.Id',pars=c(.91,NA,NA,NA,NA)),list(feature='logitdec',component='KC..Default.',pars=c(.58,NA,NA,NA,NA)))
sp<-f('build_model_specification')(co,c('logitdec','logitdec'),c('Anon.Student.Id','KC..Default.'),tr)
stopifnot(identical(sp$para,c(.91,.58)))
for(retired in c('exp','Weibull')) {
 fail(LKT(copy(d),components='KC..Default.',features='intercept',dualfit=TRUE,distribution=retired))
 fail(lkt_model_config(data.frame(component='KC..Default.',feature='intercept'),latency=retired))
}
for(retired in c('lkt_fit_exp_latency','fit_weibull_activation','fit_quantile_latency','quantile_latency','quantile_latency_transform'))
 stopifnot(!exists(retired,envir=ns,inherits=FALSE))
path<-tempfile(fileext='.rds');saveRDS(m$model_specification[[2]],path)
script<-tempfile(fileext='.R');answer<-tempfile(fileext='.rds')
writeLines(c('library(LKT)',paste0('x<-readRDS(',deparse(path),')'),paste0('saveRDS(x$predict(c(.2,.5,.9),c(-.3,0,.3),FALSE),',deparse(answer),')')),script)
status<-system2(file.path(R.home('bin'),if(.Platform$OS.type=='windows')'Rscript.exe' else 'Rscript'),c('--vanilla',shQuote(script)))
stopifnot(status==0L,identical(readRDS(answer),m$model_specification[[2]]$predict(c(.2,.5,.9),c(-.3,0,.3),FALSE)))
unlink(c(path,script,answer))
cat('PASS integrated likelihoods, gradient, metadata, masks/history, selected failures, imports, RNG, caps and fresh-process predictor.\n')
