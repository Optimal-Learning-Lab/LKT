# ROOT OUTPUT -- audit the selected central release without changing its source.
a <- commandArgs(TRUE);stopifnot(length(a)==2L)
root <- normalizePath(a[1],winslash='/');source(file.path(root,'scripts/load-current-lkt.R'))
out <- a[2];stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
set.seed(711)
d <- data.frame(Anon.Student.Id=rep(sprintf('s%02d',1:24),each=30),
  KC..Default.=rep(rep(c('a','b','c'),10),24),
  KC..Cluster.=rep(rep(c('skill1','skill1','skill2'),10),24),
  CF..Time.=rep(seq(0,by=60,length.out=30),24),
  CF..ansbin.=rbinom(720,1,.65),fold=rep(rep(1:3,each=8),each=30))
d$Outcome <- ifelse(d$CF..ansbin.==1,'CORRECT','INCORRECT')
records <- list();artifacts <- list()
run <- function(name,expr) {
  result <- tryCatch(force(expr),error=identity)
  records[[name]] <<- data.frame(case=name,completed=!inherits(result,'error'),
    detail=if(inherits(result,'error'))conditionMessage(result) else 'completed')
  artifacts[[name]] <<- result
  cat(name,':',records[[name]]$detail,'\n');flush.console()
  invisible(result)
}
build <- function(features,...)LASSOLKTData(data.table::as.data.table(data.table::copy(d)),gridpars=c(.25,.75),
  allcomponents='KC..Default.',allfeatures=features,...)
run('unparameterized_matrix',build(c('lineafm','logsuc','logfail')))
run('data_frame_input',LASSOLKTData(data.table::copy(d),gridpars=c(.25,.75),
  allcomponents='KC..Default.',allfeatures='lineafm'))
run('one_parameter_grid',build(c('lineafm','logitdec')))
run('remove_parameterized_feature',build(c('logitdec','propdec'),
  removecomp='KC..Default.',removefeat='logitdec'))
run('two_parameter_feature',build('base2'))
run('four_parameter_feature',build('ppes'))
run('removed_special_parameter_alignment',{
  removed <- build('propdec',specialcomponents='Anon.Student.Id',specialfeatures='logitdec',
    specialpars=.4,removecomp='Anon.Student.Id',removefeat='logitdec')
  control <- build('propdec')
  stopifnot(identical(removed$lassodata,control$lassodata))
  TRUE
})
run('preset_PFA',LASSOLKTModel(data.table::copy(d),gridpars=c(.25,.75),
  allcomponents=c('Anon.Student.Id','KC..Default.'),preset='PFA',target_n=15,test_fold=3))
run('simple_lasso',LASSOLKTModel(data.table::copy(d),gridpars=c(.25,.75),
  allcomponents=c('Anon.Student.Id','KC..Default.','KC..Cluster.'),
  allfeatures=c('lineafm','logafm','logsuc','logfail','logitdec','propdec'),
  target_n=15,test_fold=3))
run('single_column_lasso',LASSOLKTModel(data.table::copy(d),gridpars=c(.25,.75),
  allcomponents='KC..Default.',allfeatures='lineafm',target_n=1,test_fold=3))
run('precomputed_numeric_GRU_handoff',{
  m <- artifacts$simple_lasso;stopifnot(!inherits(m,'error'))
  columns <- intersect(m$model_features$kept_features,colnames(m$train_x))
  x <- matrix(0,nrow(d),length(columns),dimnames=list(NULL,columns))
  x[d$fold!=3,] <- m$train_x[,columns,drop=FALSE]
  x[d$fold==3,] <- m$test_x[,columns,drop=FALSE]
  input <- d;names <- paste0('selected_',seq_along(columns))
  input[names] <- as.data.frame(x)
  config <- GRUModel(numeric_cols=names,use_lkt_features=FALSE,
    categorical_cols='KC..Default.',embedding_dims=2L,hidden_size=4L,num_layers=1L,gru_dropout=0,
    head_hidden_sizes=4L,time_col=NULL,time_feature_cols=character(),time_missing_cols=character(),
    retrieval_heads=1L,retrieval_size=4L,retrieval_filter_cols='KC..Default.',epochs=1L,batch_size=8L,sequence_length=10L,
    validation_fraction=0,early_stopping=FALSE,plasticity='constant',plasticity_time_col='CF..Time.')
  fit <- LKT(input,features='lineafm',components='KC..Default.',usefolds=c(1,2),model=config,verbose=FALSE)
  stopifnot(length(fit$prediction)==nrow(d),all(is.finite(fit$prediction)),
    length(fit$model$preprocessing$numeric_names)==length(columns))
  list(numeric_columns=length(columns),prediction_range=range(fit$prediction))
})
counts <- list()
for(name in c('preset_PFA','simple_lasso')) {
  m <- artifacts[[name]]
  if(inherits(m,'error'))next
  counts[[name]] <- data.frame(case=name,target_count=m$n_features[m$target_idx],
    retained_rows=nrow(m$model_features),
    intercept_rows=sum(m$model_features$kept_features=='(Intercept)'),
    retained_predictors=sum(m$model_features$kept_features!='(Intercept)'),
    finite_predictions=all(is.finite(m$preds)),train_rows=nrow(m$train_x),test_rows=nrow(m$test_x))
}
write.csv(do.call(rbind,records),file.path(out,'audit.csv'),row.names=FALSE)
if(length(counts))write.csv(do.call(rbind,counts),file.path(out,'counts.csv'),row.names=FALSE)
saveRDS(list(release=lkt_release,records=records,artifacts=artifacts,data=d),file.path(out,'audit.rds'))
capture.output(sessionInfo(),file=file.path(out,'session.txt'))
cat('Audit complete; failures are recorded, not replaced with another algorithm.\n')
