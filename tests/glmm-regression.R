library(LKT);library(data.table)
f<-function(n)getFromNamespace(n,'LKT')
fail<-function(expr,pattern=NULL){e<-tryCatch({force(expr);NULL},error=identity);stopifnot(inherits(e,'error'));if(!is.null(pattern))stopifnot(grepl(pattern,conditionMessage(e),fixed=TRUE));invisible(e)}
set.seed(731)
d<-data.table(Anon.Student.Id=rep(paste0('s',1:30),each=12),
 KC..Default.=rep(c('k1','k2','k3'),120),X=rep(seq(-1,1,length.out=12),30),fold=rep(c(1L,2L),180))
u<-rep(seq(-2,2,length.out=30),each=12)
d[,CF..ansbin.:=rbinom(.N,1,plogis(-.2+.8*X+u))]
d[,Outcome:=ifelse(CF..ansbin.==1,'CORRECT','INCORRECT')]
fit<-LKT(copy(d),usefolds=1L,components=c('Anon.Student.Id','X'),features=c('intercept@','numer'),interc=TRUE,verbose=FALSE)
reference<-lme4::glmer(CF..ansbin.~X+(1|Anon.Student.Id),data=as.data.frame(d),subset=fold==1L,family=binomial('logit'))
stopifnot(fit$model_name=='GLMM',identical(fit$fit_rows,which(d$fold==1)),
 nobs(fit$model)==sum(d$fold==1),nrow(fit$newdata)==nrow(d),
 max(abs(fit$prediction-predict(reference,newdata=as.data.frame(d),type='response')))<1e-7,
 abs(fit$loglike-as.numeric(logLik(reference)))<1e-7,
 max(abs(sort(fit$coefs[,1])-sort(lme4::fixef(reference))))<1e-7,
 length(fit$random_effects)==1,nrow(fit$variance_components)>=1,
 identical(predict_lkt(fit,fit$newdata)$predictions,fit$prediction))
# With intercept/numeric features, changing held-out labels must not change fit.
changed<-copy(d);changed[fold==2L,CF..ansbin.:=1L-CF..ansbin.]
changed[,Outcome:=ifelse(CF..ansbin.==1,'CORRECT','INCORRECT')]
b<-LKT(changed,usefolds=1L,components=c('Anon.Student.Id','X'),features=c('intercept@','numer'),interc=TRUE,verbose=FALSE)
stopifnot(identical(fit$coefs,b$coefs),identical(fit$prediction,b$prediction))
cat('PASS GLMM selected folds, direct glmer equivalence, complete output and held-out-label isolation\n')
# Preserve excluded responses in history-dependent features.
h<-LKT(copy(d),usefolds=1L,components=c('Anon.Student.Id','KC..Default.'),features=c('intercept@','logsuc'),interc=TRUE,verbose=FALSE)
expected<-d[,.(expected=c(0,head(cumsum(CF..ansbin.),-1))),by=.(Anon.Student.Id,KC..Default.)]
# Independent row-wise prior count, in original order.
e<-vapply(seq_len(nrow(d)),function(i)sum(d$CF..ansbin.[seq_len(i-1L)][d$Anon.Student.Id[seq_len(i-1L)]==d$Anon.Student.Id[i] & d$KC..Default.[seq_len(i-1L)]==d$KC..Default.[i]]),numeric(1))
stopifnot(max(abs(h$newdata$logsucKC..Default.-log1p(e)))<1e-12)
cat('PASS full feature histories survive GLMM fitting-fold exclusion\n')
# Held-out subjects have no fitted random effect: explicit policy is required.
z<-copy(d);z[,fold:=ifelse(Anon.Student.Id %in% c('s29','s30'),2L,1L)]
args<-list(data=z,usefolds=1L,components=c('Anon.Student.Id','X'),features=c('intercept@','numer'),interc=TRUE,verbose=FALSE)
fail(do.call(LKT,args),'new levels')
zero<-do.call(LKT,c(args,list(model=GLMMModel(unseen_levels='zero'))))
stopifnot(max(abs(zero$prediction-predict(zero$model,newdata=as.data.frame(zero$newdata),type='response',allow.new.levels=TRUE)))<1e-9)
fixed<-do.call(LKT,c(args,list(model=GLMMModel(prediction_mode='fixed'))))
stopifnot(max(abs(fixed$prediction-predict(fixed$model,newdata=as.data.frame(fixed$newdata),type='response',re.form=NA)))<1e-9)
cat('PASS explicit unseen-level and fixed-only prediction policies\n')
# Mixed $ terms must use the selected data, not capture a full e$data vector.
a<-LKT(copy(d),usefolds=1L,components=c('Anon.Student.Id','KC..Default.'),features=c('intercept@','linesuc$'),interc=TRUE,verbose=FALSE)
stopifnot(length(a$prediction)==nrow(d),all(is.finite(a$prediction)))
new<-copy(a$newdata);stopifnot(max(abs(predict_lkt(a,new)$predictions-a$prediction))<1e-10)
cat('PASS fold filtering and prediction with factor-specific feature terms\n')
fail(LKT(copy(d),usefolds=99L,components='Anon.Student.Id',features='intercept@',interc=TRUE,verbose=FALSE),'no rows')
fail(LKT(copy(d[,!'fold']),usefolds=1L,components='Anon.Student.Id',features='intercept@',interc=TRUE,verbose=FALSE),'fold')
fail(LKT(copy(d),components='Anon.Student.Id',features='intercept@',nosolve=TRUE),'nosolve')
set.seed(732)
d[,Duration..sec.:=f('wald_quantile_latency')(fit$prediction,
  rep(seq(-.6,.6,length.out=30),each=12),rnorm(.N),.24,2,12,0)]
both<-LKT(copy(d),usefolds=1L,components=c('Anon.Student.Id','X'),features=c('intercept@','numer'),interc=TRUE,verbose=FALSE,dualfit=TRUE,distribution='Wald',wald_t0_fixed=0,wald_control=list(nodes=15L,maxit=120L))
stopifnot(both$model_name=='GLMM',!is.null(both$latencymodel),length(both$prediction)==nrow(d))
cat('PASS GLMM invalid masks, explicit unsupported matrix-only mode and dual fitting\n')
