library(LKT)
fail<-function(x,pattern) {
  e<-tryCatch({force(x);NULL},error=identity)
  stopifnot(inherits(e,'error'),grepl(pattern,conditionMessage(e),fixed=TRUE))
}
matrix_from<-function(x) {
  c<-x$lassodata[[2]]
  Matrix::sparseMatrix(i=rep(seq_len(c@dimension[1]),diff(c@ia)),j=c@ja,x=c@ra,
    dims=c@dimension,dimnames=list(NULL,x$lassodata[[1]]))
}
set.seed(711)
d<-data.frame(Anon.Student.Id=rep(paste0('s',1:18),each=24),
  KC..Default.=rep(rep(c('a','b','c'),8),18),KC..Cluster.=rep(rep(c('x','x','y'),8),18),
  CF..Time.=rep(seq(0,by=60,length.out=24),18),CF..reltime.=rep(seq(0,by=20,length.out=24),18),
  CF..ansbin.=rbinom(432,1,.65),fold=rep(rep(1:3,each=6),each=24))
d$Outcome<-ifelse(d$CF..ansbin.==1,'CORRECT','INCORRECT')
before<-serialize(d,NULL)
build<-function(...)LASSOLKTData(d,gridpars=c(.25,.75),allcomponents='KC..Default.',...)
x<-build(allfeatures=c('lineafm','logitdec'))
stopifnot(identical(before,serialize(d,NULL)),ncol(matrix_from(x))==3L)
lltm<-LASSOLKTData(d,gridpars=c(.25,.75),allcomponents=c('Anon.Student.Id','KC..Default.'),
  allfeatures=character(),preset='AFMLLTM')
stopifnot(nrow(matrix_from(lltm))==nrow(d),ncol(matrix_from(lltm))>3L)
removed<-build(allfeatures='propdec',specialcomponents='Anon.Student.Id',
  specialfeatures='logitdec',specialpars=.4,removecomp='Anon.Student.Id',removefeat='logitdec')
control<-build(allfeatures='propdec')
stopifnot(identical(removed$lassodata,control$lassodata))
special<-build(allfeatures='lineafm',specialcomponents='KC..Default.',specialfeatures='base2',specialpars=c(.2,.6))
direct<-LKT(data.table::as.data.table(d),components='KC..Default.',features='base2',fixedpars=c(.2,.6),nosolve=TRUE,verbose=FALSE)
stopifnot(isTRUE(all.equal(as.numeric(matrix_from(special)[,colnames(matrix_from(direct))]),as.numeric(matrix_from(direct)))))
fail(build(allfeatures='ppes'),'Automatic grids support zero/one-parameter')
fail(build(allfeatures='lineafm',specialcomponents='KC..Default.',specialfeatures='base2',specialpars=.2),'specialpars')
features<-c('lineafm','logafm','linesuc','logsuc','linefail','logfail','prop','logit','propdec','propdec2',
 'logitdec','recency','recencysuc','recencyfail','expdecafm','expdecsuc','expdecfail','base')
bank<-LASSOLKTData(d,gridpars=(1:9)/10,allcomponents=c('Anon.Student.Id','KC..Default.','KC..Cluster.'),allfeatures=features)
stopifnot(nrow(bank$candidates)==318L,ncol(matrix_from(bank))==318L,all(is.finite(matrix_from(bank)@x)))
# Causality: a later response cannot alter predictors up to that response, and
# another student's history cannot alter this student's feature bank.
changed<-d;changed$CF..ansbin.[24]<-1-changed$CF..ansbin.[24]
changed$Outcome[24]<-ifelse(changed$CF..ansbin.[24]==1,'CORRECT','INCORRECT')
other<-LASSOLKTData(changed,gridpars=(1:9)/10,allcomponents=c('Anon.Student.Id','KC..Default.','KC..Cluster.'),allfeatures=features)
stopifnot(identical(matrix_from(bank),matrix_from(other)))
fit<-function(data)LASSOLKTModel(data,gridpars=c(.25,.75),allcomponents=c('Anon.Student.Id','KC..Default.'),
 allfeatures=c('lineafm','logsuc','logfail','logitdec','propdec'),target_n=15,test_fold=3,train_folds=1)
m<-fit(d)
stopifnot(inherits(m$train_x,'sparseMatrix'),identical(m$train_rows,which(d$fold==1)),
 length(m$selected_predictors)==m$n_features[m$target_idx],!any(m$selected_predictors=='(Intercept)'),
 all(is.finite(m$preds)))
changed<-d;changed$CF..ansbin.[changed$fold!=1]<-1-changed$CF..ansbin.[changed$fold!=1]
changed$Outcome<-ifelse(changed$CF..ansbin.==1,'CORRECT','INCORRECT')
isolation<-LASSOLKTData(changed,gridpars=(1:9)/10,
 allcomponents=c('Anon.Student.Id','KC..Default.','KC..Cluster.'),allfeatures=features)
stopifnot(identical(matrix_from(bank)[d$fold==1,,drop=FALSE],matrix_from(isolation)[d$fold==1,,drop=FALSE]))
n<-fit(changed)
stopifnot(identical(m$fit$beta,n$fit$beta),identical(m$fit$lambda,n$fit$lambda),
 identical(m$selected_predictors,n$selected_predictors),identical(m$target_idx,n$target_idx))
fail(LASSOLKTModel(d,gridpars=.5,allcomponents='KC..Default.',allfeatures='lineafm',target_n=1,test_fold=3),'at least two candidate columns')
cat('PASS Lasso frame inputs, sparse matrices, explicit parameter alignment, bounded318 pool, causal histories, intercept-free counts and train-only selection\n')
