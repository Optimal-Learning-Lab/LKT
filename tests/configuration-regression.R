library(LKT)
fail<-function(expr,pattern) {
  e<-tryCatch({force(expr);NULL},error=identity)
  stopifnot(inherits(e,'error'),grepl(pattern,conditionMessage(e),fixed=TRUE))
}
t<-data.frame(component=c('Anon.Student.Id','KC..Default.','KC..Default.'),
  feature=c('logitdec','ppes','logitdec'),fixed=c('0.9140209','0.3472927;0.39437;0.01992455;0.7234435','0.5833674'))
cfg<-lkt_model_config(t,TRUE,options=list(verbose=FALSE,epsilon=1e-12,maxitv=1000,factrv=1e4))
stopifnot(identical(cfg$fixedpars,c(.9140209,.3472927,.39437,.01992455,.7234435,.5833674)),
  identical(cfg$terms$component,t$component),identical(cfg$terms$feature,t$feature))
f<-tempfile(fileext='.rds');saveRDS(cfg,f);stopifnot(identical(readRDS(f),cfg));unlink(f)
free<-lkt_model_config(data.frame(component=c('KC..Default.','Anon.Student.Id'),
  feature=c('logitdec','logitdec'),fixed=c('NA','1'),start=c('0.23','')))
stopifnot(identical(free$fixedpars,c(NA_real_,1)),identical(free$seedpars,c(.23,NA_real_)))
blank<-lkt_model_config(data.frame(component='KC..Default.',feature='ppe',fixed=NA,start=NA,interaction=NA))
stopifnot(identical(blank$fixedpars,rep(NA_real_,4L)),identical(blank$seedpars,rep(NA_real_,4L)))
fail(lkt_model_config(data.frame(component='KC',feature='fake')),'Unknown')
fail(lkt_model_config(data.frame(component='KC',feature='ppe',fixed='0.2')),'expected 4')
fail(lkt_model_config(data.frame(component='KC',feature='logitdec',fixed='2')),'nonexistent')
fail(lkt_model_config(data.frame(component='KC',feature='intercept@')),'Random terms')
fail(lkt_model_config(data.frame(component='KC',feature='intercept'),options=list(data=1)),'reserved')
set.seed(472);d<-data.frame(Anon.Student.Id=rep(paste0('s',1:8),each=20),
  KC..Default.=rep(rep(paste0('k',1:4),5),8),CF..ansbin.=rbinom(160,1,.65))
simple<-lkt_model_config(data.frame(component='KC..Default.',feature='logsuc'),TRUE,options=list(verbose=FALSE))
a<-fit_lkt_config(d,simple)
b<-LKT(data.table::as.data.table(d),components='KC..Default.',features='logsuc',interc=TRUE,verbose=FALSE)
stopifnot(identical(a$coefs,b$coefs),identical(a$prediction,b$prediction))
cat('PASS ordered parameters, estimated/tied starts, serialization and direct LKT equivalence\n')
