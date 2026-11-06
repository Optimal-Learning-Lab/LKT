library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
close <- function(a,b)stopifnot(isTRUE(all.equal(a,b,tolerance=3e-6,check.attributes=FALSE)))
bad <- function(expr)stopifnot(inherits(tryCatch({force(expr);NULL},error=identity),'error'))
columns <- c('gru_item_age_seconds','gru_mean_prior_spacing_seconds')
# Studies count as item exposures, but other items and students do not. At t=90,
# prior times 0,10,30 give mean spacing (10+20)/2=15, excluding the current gap 60.
d <- data.frame(Anon.Student.Id=c(rep('s',6),'other'),KC..Default.=c('a','b','a','a','a','a','a'),
  CF..Time.=c(0,5,10,30,90,90,200),Outcome=c('STUDY','CORRECT','INCORRECT','CORRECT','CORRECT','STUDY','STUDY'))
x <- LKTGRUTimeFeatures(d)
close(x[[columns[1]]],c(0,0,10,30,90,90,0))
close(x[[columns[2]]],c(NA,NA,NA,10,15,30,NA))
later <- d;later$CF..Time.[5:6] <- c(900,1000)
close(LKTGRUTimeFeatures(later)[[columns[2]]][5],15)
close(LKTGRUTimeFeatures(d[1:4,])[,columns],x[1:4,columns])
minutes <- d;minutes$CF..Time. <- minutes$CF..Time./60
close(LKTGRUTimeFeatures(minutes,time_units='minutes')[,columns],x[,columns])
calendar <- d;calendar$CF..Time. <- as.POSIXct('2020-01-01',tz='UTC')+calendar$CF..Time.
close(LKTGRUTimeFeatures(calendar)[,columns],x[,columns])
permutation <- c(4,2,1,3,5,6,7)
close(LKTGRUTimeFeatures(d[permutation,])[,columns],x[permutation,columns])
changed <- d;changed$Outcome <- 'STUDY'
close(LKTGRUTimeFeatures(changed)[,columns],x[,columns])
bad(LKTGRUTimeFeatures(x));bad(LKTGRUTimeFeatures(d,time_units='weeks'))
bad(test_gru_model(time_feature_cols='Outcome'))
bad(test_gru_model(time_feature_cols='x',numeric_cols='x'))
settings <- test_gru_model(time_col='CF..Time.',time_feature_cols=columns,time_missing_cols=columns[2])$settings
ns <- asNamespace('LKT')
numeric_inputs <- get('lkt_gru_numeric',ns)(x,NULL,settings)$matrix
close(expm1(numeric_inputs[,2:3]),as.matrix(x[columns]))
stopifnot(all(startsWith(colnames(numeric_inputs),'time:')))
bad(get('lkt_gru_numeric',ns)(d,NULL,settings))
negative <- x;negative[[columns[1]]][1] <- -1
bad(get('lkt_gru_numeric',ns)(negative,NULL,settings))
# Fit real Torch towers and replay the same rows through batched, split, saved and
# pending continuation. Both plain inputs and engineered-feature replay must agree.
if(requireNamespace('torch',quietly=TRUE) && torch::torch_is_installed()) {
  set.seed(145)
  exposure <- data.frame(Anon.Student.Id=rep(paste0('s',1:6),each=15),
    KC..Default.=rep(c('a','b','a'),30),CF..Time.=rep(cumsum(seq(10,150,10)),6),
    Outcome=rep(c('STUDY',rep('CORRECT',14)),6),fold=rep(c(1,1,1,1,2,3),each=15))
  full <- LKTGRUTimeFeatures(exposure)
  scored <- full[full$Outcome!='STUDY',];scored$CF..ansbin. <- rbinom(nrow(scored),1,.6)
  for(engineered in c(FALSE,TRUE)) {
    s <- list(time_col='CF..Time.',order_col='CF..Time.',time_feature_cols=columns,time_missing_cols=columns[2],
      feature_encoder='mlp',encoder_hidden_sizes=8L,encoder_output_size=4L,
      hidden_size=5L,num_layers=3L,head_hidden_sizes=12L,use_lkt_features=engineered,
      epochs=2L,batch_size=2L,sequence_length=4L,validation_folds=2,seed=11L)
    fit <- LKT(scored,usefolds=1,components='KC..Default.',features='intercept',
      model=do.call(test_gru_model,s),verbose=FALSE)
    stopifnot(identical(dim(fit$model$weights[['time_encoder.0.0.weight']]),c(8L,4L)))
    close(predict_lkt(fit,scored)$predictions,fit$prediction)
    rt <- LKTGRURuntime(fit)
    first <- LKTGRUStep(fit,LKTGRUState(fit),scored[1:5,],rt)
    pending <- scored[6,,drop=FALSE];pending$CF..ansbin. <- NA_real_
    peek <- LKTGRUStep(fit,first$state,pending,rt)
    close(peek$predictions,fit$prediction[6])
    stopifnot(identical(peek$state$hidden,first$state$hidden))
    close(LKTGRUStep(fit,peek$state,scored[6:14,],rt)$predictions,fit$prediction[6:14])
    file <- tempfile(fileext='.rds');saveRDS(list(fit=fit,state=first$state),file)
    saved <- readRDS(file);unlink(file)
    close(LKTGRUStep(saved$fit,saved$state,scored[6:14,])$predictions,fit$prediction[6:14])
    # Scaling comes only from fitting rows even when held-out timing magnitudes change.
    altered <- scored;altered[[columns[1]]][altered$fold==3] <- 1e9
    pre <- get('lkt_gru_preprocess',ns)(altered,altered$fold==1,fit$model$recipe,fit$gru_settings)
    stopifnot(identical(pre,fit$model$preprocessing))
  }
} else if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required.')
cat('PASS exposure timing, prior-only spacing, studies, three-input tower, training scaling and continuation\n')
