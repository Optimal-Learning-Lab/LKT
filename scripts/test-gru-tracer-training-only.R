# Contract and feature-isolation checks; no scientific fitting.
a <- commandArgs(TRUE);stopifnot(length(a)==1L);root <- a[1]
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-tracer-training-only-helpers.R'))
d <- data.frame(Anon.Student.Id=rep(paste0('s',1:6),each=4L),
  KC..Default.=rep(c('i1','i2'),12L),KC..Cluster.=rep(c('k1','k2'),12L),
  CF..Time.=rep(as.numeric(1:4),6L),CF..ansbin.=rep(c(0,1,1,0),6L),
  Duration..sec.=rep(1,24),fold=rep(c(1L,1L,2L,2L,3L,3L),each=4L))
train <- which(d$fold==1L);p <- rep(.6,nrow(d))
loss <- -sum(ifelse(d$CF..ansbin.[train]==1,log(p[train]),log1p(-p[train])))
control <- list(fit=list(newdata=d,prediction=p,loglike=-loss,
  optimizedpars=list(par=(1:6)/10,convergence=0L,value=loss)))
protocol <- list(training_rows=train)
stopifnot(tracer6_calibration_check(d,control,protocol)$passed)
bad <- control;bad$fit$loglike <- -loss*3
must_fail <- function(expr)stopifnot(inherits(tryCatch(force(expr),error=identity),'error'))
must_fail(tracer6_calibration_check(d,bad,protocol))
bad <- protocol;bad$training_rows <- seq_len(nrow(d))
must_fail(tracer6_calibration_check(d,control,bad))
bad <- d;bad$fold[1] <- 3L
must_fail(tracer6_calibration_check(bad,control,protocol))
s <- LKT::GRUModel(categorical_cols=c('KC..Default.','KC..Cluster.'),
  embedding_adaptation='per_student',adaptive_embedding_cols=c('KC..Default.','KC..Cluster.'),
  time_feature_cols=c('gru_item_age_seconds','gru_mean_prior_spacing_seconds'),
  time_missing_cols='gru_mean_prior_spacing_seconds',plasticity='constant',
  plasticity_time_col='CF..Time.')$settings
r <- list(components=c('Anon.Student.Id','KC..Default.','KC..Default.'),
  features=c('logitdec','ppes','logitdec'),fixedpars=c(.8,.3,.4,.1,.7,.6),
  interc=FALSE,interacts=rep(NA_character_,3L),connectors=c('+','+'),maketimes=FALSE)
data <- LKT::LKTGRUTimeFeatures(d)
stopifnot(tracer6_preprocessing_check(data,r,s)$passed,
  getFromNamespace('lkt_lkt_optimization_inputs','LKT')(r$fixedpars,NA_real_,6L)$parameter_count==0L)
cat('PASS reject full-data calibration and mixed-student folds; six training-only inputs; zero free nonlinear parameters\n')
