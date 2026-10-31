library(LKT)
library(data.table)
f<-function(n)getFromNamespace(n,'LKT')
fail<-function(expr,pattern=NULL) {
  e<-tryCatch({force(expr);NULL},error=identity)
  stopifnot(inherits(e,'error'))
  if(!is.null(pattern))stopifnot(grepl(pattern,conditionMessage(e),fixed=TRUE))
}
set.seed(402)
# Many short KC histories per participant identify both personal coefficients.
ids<-paste0('s',seq_len(48))
b<-matrix(rnorm(96),48,2)%*%chol(matrix(c(.8,.12,.12,.25),2))
d<-rbindlist(lapply(seq_along(ids),function(i) {
  h<-data.table(Anon.Student.Id=ids[i],KC..Default.=rep(paste0('k',1:15),each=10),
    CF..Time.=seq_len(150),CF..ansbin.=0)
  for(k in unique(h$KC..Default.)) {
    s<-0
    for(j in which(h$KC..Default.==k)) {
      y<-rbinom(1,1,plogis(-.7+b[i,1]+(.7+b[i,2])*log1p(s)))
      h$CF..ansbin.[j]<-y;s<-s+y
    }
  }
  h
}))
cfg<-lkt_model_config(data.frame(component='KC..Default.',feature='logsuc'),TRUE,
  backend='OnlineBayesian',options=list(verbose=FALSE),
  bayesian_options=list(n_particles=128L,ess_action='ignore',store_trace=TRUE),
  bayesian_training=list(training_fraction=.75,split_seed=17L))
original<-copy(d);rng<-.Random.seed
s<-f('lkt_bayesian_participant_split')(d,'Anon.Student.Id',cfg$bayesian_training)
stopifnot(identical(rng,.Random.seed),sum(s$manifest$group=='training')==36L,
  sum(s$manifest$group=='evaluation')==12L,
  !length(intersect(unique(d$Anon.Student.Id[s$training_rows]),unique(d$Anon.Student.Id[s$evaluation_rows]))),
  identical(s$manifest,f('lkt_bayesian_participant_split')(d[.N:1],'Anon.Student.Id',cfg$bayesian_training)$manifest))
fit<-fit_lkt_config(d,cfg)
stopifnot(identical(rng,.Random.seed),identical(as.data.frame(d),as.data.frame(original)),
  nrow(fit$newdata)==length(s$evaluation_rows),
  nrow(fit$population_prefit$newdata)==length(s$training_rows),
  setequal(unique(fit$newdata$Anon.Student.Id),s$manifest$participant[s$manifest$group=='evaluation']),
  identical(fit$model$prior_mean,fit$population_prefit$model$prior_mean),
  identical(fit$model$prior_covariance,fit$population_prefit$model$prior_covariance))
# The exported prior is VarCorr (between participants), not vcov(fixed effects).
g<-fit$population_prefit$model$fit
v<-as.matrix(lme4::VarCorr(g)$participant);dimnames(v)<-dimnames(fit$model$prior_covariance)
stopifnot(identical(as.numeric(v),as.numeric(fit$model$prior_covariance)),
  !isTRUE(all.equal(unname(v),unname(as.matrix(stats::vcov(g))))))
# Leakage acid test: change ALL held-out outcomes; training fit/prior is identical.
changed<-copy(d);changed$CF..ansbin.[s$evaluation_rows]<-1-changed$CF..ansbin.[s$evaluation_rows]
other<-fit_lkt_config(changed,cfg)
stopifnot(identical(fit$model$prior_mean,other$model$prior_mean),
  identical(fit$model$prior_covariance,other$model$prior_covariance),
  !identical(fit$prediction,other$prediction))
# Direct sequential adapter on held-out subjects reproduces every prediction.
reference<-fit_lkt_config(d[s$evaluation_rows],fit$evaluation_configuration)
stopifnot(identical(fit$prediction,reference$prediction),
  identical(fit$model$final_states,reference$model$final_states))
for(id in names(fit$model$final_states)) {
  first<-which(fit$newdata$Anon.Student.Id==id)[1]
  # First trial logsuc is zero, so each participant starts from the same prior.
  p<-mean(plogis(fit$model$initial_state$particles[,'(Intercept)']))
  stopifnot(abs(fit$prediction[first]-p)<1e-12)
}
# Saved and printed configurations reproduce the complete split/prefit workflow.
path<-tempfile();saveRDS(fit,path);saved<-readRDS(path);unlink(path)
stopifnot(identical(saved$participant_split,s),identical(saved$configuration,cfg))
printed<-parse(text=paste(capture.output(print(lkt_config_call(cfg))),collapse='\n'))
replayed<-eval(printed,list(data=copy(d)))
stopifnot(identical(replayed$participant_split,s),identical(replayed$prediction,fit$prediction))
fail(f('lkt_validate_bayesian_training')(list(training_fraction=1)),'fraction')
fail(f('lkt_validate_bayesian_training')(list(split_seed=1.5)),'seed')
fail(f('lkt_bayesian_participant_split')(d[Anon.Student.Id%in%ids[1:3]],'Anon.Student.Id',cfg$bayesian_training),'three training')
bad<-copy(d);bad$Anon.Student.Id[1]<-NA_character_
fail(f('lkt_bayesian_participant_split')(bad,'Anon.Student.Id',cfg$bayesian_training),'Missing participant')
badcfg<-cfg;badcfg$bayesian_options$beta_alpha_terms<-'absent'
fail(fit_lkt_config(d,badcfg),'match the training')
same<-data.table(Anon.Student.Id=rep(paste0('s',1:8),each=80),KC..Default.='k',
  CF..Time.=rep(1:80,8),CF..ansbin.=rep(c(0,1,0,1,1,0,1,0),80))
fail(suppressWarnings(fit_lkt_config(same,cfg)),'singular participant covariance')
# The split restores RNG absence and caller RNG kind as well as an existing seed.
rm('.Random.seed',envir=.GlobalEnv);kind<-RNGkind()
invisible(f('lkt_bayesian_participant_split')(d,'Anon.Student.Id',cfg$bayesian_training))
stopifnot(!exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE),identical(kind,RNGkind()))
cat('PASS whole-participant split, RNG preservation, training-only prior, covariance provenance, leakage acid test, sequential evaluation/reset, saved provenance, printable replay and invalid settings\n')
