# ROOT OUTPUT REFERENCE ACTION [TASK]
# Staged feature selection and one matched constant-plasticity Cloze condition.
a<-commandArgs(TRUE);stopifnot(length(a)%in%c(4L,5L))
root<-normalizePath(a[1],winslash='/');out<-a[2];reference<-normalizePath(a[3],winslash='/')
action<-match.arg(a[4],c('plan','select','fit','report'))
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-encoder-study.R'))
internal<-function(n)getFromNamespace(n,'LKT')
if(!exists('lkt_lasso_fit_path',asNamespace('LKT'),inherits=FALSE))
  stop('The selected release does not contain the verified Lasso repair.')
atomic<-function(x,path) {
  if(file.exists(path))stop('Refusing to overwrite immutable artifact: ',path)
  tmp<-paste0(path,'.tmp');saveRDS(x,tmp);stopifnot(file.rename(tmp,path))
}
hash_object<-function(x)digest::digest(x,algo='sha256')
csr_matrix<-function(x) {
  q<-x$lassodata[[2]]
  Matrix::sparseMatrix(i=rep(seq_len(q@dimension[1]),diff(q@ia)),j=q@ja,x=q@ra,
    dims=q@dimension,dimnames=list(NULL,x$lassodata[[1]]))
}
components<-c('Anon.Student.Id','KC..Default.','KC..Cluster.')
features<-c('lineafm','logafm','linesuc','logsuc','linefail','logfail','prop','logit','propdec','propdec2',
 'logitdec','recency','recencysuc','recencyfail','expdecafm','expdecsuc','expdecfail','base')
grid<-(1:9)/10
scripts<-c('scripts/lasso-gru-study.R','scripts/run-lasso-gru-batch.py','scripts/gru-encoder-study.R',
 'scripts/verify-pfa-gru-metrics.py','scripts/hpc/run-lasso-gru.slurm','scripts/load-current-lkt.R')
script_hashes<-setNames(vapply(file.path(root,scripts),study_hash,character(1)),scripts)
runtime_identity<-function()list(R=R.version.string,platform=R.version$platform,locale=Sys.getlocale(),
  deployment_sha256=study_hash(Sys.getenv('LKT_RUNTIME_CONFIG')),
  packages=setNames(vapply(c('LKT','torch','glmnet','Matrix','LiblineaR'),function(n)
    as.character(utils::packageVersion(n)),character(1)),c('LKT','torch','glmnet','Matrix','LiblineaR')))
if(action=='plan') {
  stopifnot(!dir.exists(out))
  baseline<-readRDS(file.path(reference,'study.rds'))
  d<-readRDS(file.path(reference,'data/prepared_data.rds'))
  stopifnot(study_hash(file.path(reference,'data/prepared_data.rds'))==baseline$data_sha256,
    nrow(d)==55122L,length(unique(d$Anon.Student.Id))==478L,
    all(d$CF..ansbin.%in%0:1),!anyDuplicated(d$comparison_source_row),
    all(baseline$tasks$candidate=='constant'),nrow(baseline$tasks)==15L,
    setequal(baseline$tasks$partition,paste0('cv',1:5)),setequal(baseline$tasks$seed,1:3))
  for(ids in split(seq_len(nrow(d)),d$Anon.Student.Id))stopifnot(all(diff(d$CF..Time.[ids])>=0))
  stopifnot(sum(length(components)*length(grid)^internal('lkt_feature_param_counts')(features))==318L)
  s<-do.call(GRUModel,baseline$settings$constant)$settings
  stopifnot(s$plasticity=='constant',s$embedding_adaptation=='none',is.null(s$semantic_input),
    !s$hidden_decay,!s$gate_fatigue,s$numeric_encoder=='combined',s$num_threads==1L,
    is.null(s$time_col),identical(s$time_feature_cols,c('gru_item_age_seconds','gru_mean_prior_spacing_seconds')),
    all(s$time_feature_cols%in%names(d)))
  for(folder in c('data','partitions','selection','models','training','reports','logs'))
    dir.create(file.path(out,folder),recursive=TRUE,showWarnings=FALSE)
  stopifnot(file.copy(file.path(reference,'data/prepared_data.rds'),file.path(out,'data/prepared_data.rds')))
  for(part in paste0('cv',1:5)) {
    path<-file.path(reference,'partitions',paste0(part,'.rds'))
    stopifnot(study_hash(path)==baseline$partition_hashes[[part]])
    roles<-readRDS(path)
    stopifnot(length(roles)==nrow(d),setequal(unique(roles),1:3),
      !anyDuplicated(unique(data.frame(student=d$Anon.Student.Id,role=roles))$student))
    stopifnot(file.copy(path,file.path(out,'partitions',paste0(part,'.rds'))))
  }
  tasks<-baseline$tasks;tasks$candidate<-'lasso15'
  tasks$run_id<-sub('__constant__','__lasso15__',tasks$run_id,fixed=TRUE)
  p<-list(schema=1L,release=as.list(lkt_release),runtime=runtime_identity(),reference=reference,
    reference_sha256=study_hash(file.path(reference,'study.rds')),data_sha256=baseline$data_sha256,
    partition_hashes=baseline$partition_hashes,script_hashes=script_hashes,
    components=components,features=features,grid=grid,candidate_count=318L,target_predictors=15L,
    tasks=tasks,settings=s,baseline_recipe=baseline$baseline,
    baseline_parameter_count=unname(baseline$parameter_counts[['constant']]),
    selection='Train role1 only; closest Lasso path count excluding intercept, ties prefer fewer columns then stronger lambda. One selection per fold reused across seeds.',
    numeric_inputs='Selected precomputed history columns plus unchanged three timing coordinates; existing item/skill embeddings and constant output plasticity retained.',
    interpretation='Exploratory evaluation on existing repeatedly used Cloze folds; no validation/test labels select Lasso features.',
    initialization='Fresh network per seed. Changed numeric input width changes RNG consumption; exact shared initial weight parity is not claimed.')
  atomic(p,file.path(out,'study.rds'));study_write_csv(tasks,file.path(out,'tasks.csv'))
  jsonlite::write_json(list(schema=1L,total_fits=15L,protocol_sha256=study_hash(file.path(out,'study.rds')),
    candidate_count=318L,target_predictors=15L,components=components,features=features,grid=grid,
    data_sha256=p$data_sha256,partition_hashes=as.list(p$partition_hashes),
    script_hashes=as.list(script_hashes),release=p$release,selection=p$selection,
    interpretation=p$interpretation),file.path(out,'protocol.json'),auto_unbox=TRUE,pretty=TRUE,null='null')
  cat('FROZEN318 candidates, five train-only selections, fifteen GRU tasks\n');quit(save='no')
}
p<-readRDS(file.path(out,'study.rds'))
stopifnot(identical(p$schema,1L),identical(p$release,as.list(lkt_release)),identical(p$runtime,runtime_identity()),
  identical(p$script_hashes,script_hashes),identical(p$components,components),identical(p$features,features),
  identical(p$grid,grid),study_hash(file.path(out,'data/prepared_data.rds'))==p$data_sha256)
for(part in names(p$partition_hashes))stopifnot(
  study_hash(file.path(out,'partitions',paste0(part,'.rds')))==p$partition_hashes[[part]])
d<-readRDS(file.path(out,'data/prepared_data.rds'))
if(action=='select') {
  bank_path<-file.path(out,'data/candidate_bank.rds')
  if(!file.exists(bank_path)) {
    cat('PRECOMPUTE318 causal candidate columns\n');flush.console()
    b<-LASSOLKTData(d,gridpars=grid,allcomponents=components,allfeatures=features)
    x<-csr_matrix(b);meta<-b$candidates
    stopifnot(nrow(meta)==318L,nrow(x)==nrow(d),ncol(x)==318L,!anyDuplicated(meta$column),
      setequal(meta$column,colnames(x)),all(is.finite(x@x)))
    x<-x[,meta$column,drop=FALSE]
    atomic(list(x=x,candidates=meta,data_sha256=p$data_sha256),bank_path)
  }
  bank<-readRDS(bank_path);stopifnot(bank$data_sha256==p$data_sha256,ncol(bank$x)==318L)
  serializable<-bank$candidates
  serializable$parameters<-vapply(serializable$parameters,function(v)paste(v,collapse=';'),character(1))
  study_write_csv(serializable,file.path(out,'selection/candidate_dictionary.csv'))
  records<-list()
  for(part in paste0('cv',1:5)) {
    roles<-readRDS(file.path(out,'partitions',paste0(part,'.rds')))
    path<-file.path(out,'selection',paste0(part,'.rds'))
    identity<-hash_object(list(study=study_hash(file.path(out,'study.rds')),bank=study_hash(bank_path),part=part))
    if(!file.exists(path)) {
      cat('SELECT',part,'training students only\n');flush.console()
      m<-internal('lkt_lasso_fit_path')(bank$x[roles==1,,drop=FALSE],d$CF..ansbin.[roles==1],
        bank$x[roles==3,,drop=FALSE],d$CF..ansbin.[roles==3],p$target_predictors)
      columns<-m$selected_predictors;meta<-bank$candidates[match(columns,bank$candidates$column),,drop=FALSE]
      stopifnot(length(columns)>0L,length(columns)==m$n_features[m$target_idx],!anyNA(meta$column))
      numeric_names<-sprintf('lasso_input_%02d',seq_along(columns))
      input<-d;input$fold<-roles
      input[numeric_names]<-as.data.frame(as.matrix(bank$x[,columns,drop=FALSE]))
      s<-p$settings;s$use_lkt_features<-FALSE;s$numeric_cols<-numeric_names
      s<-do.call(GRUModel,s)$settings
      selected<-list(identity=identity,part=part,candidates=meta,numeric_names=numeric_names,
        columns=columns,data=input,settings=s,lambda=m$fit$lambda[m$target_idx],path_counts=m$n_features,
        path_lambda=m$fit$lambda,target_idx=m$target_idx,fit=m$fit,
        train_students=sort(unique(as.character(d$Anon.Student.Id[roles==1]))),
        train_rows=which(roles==1),train_input_sha256=hash_object(list(bank$x[roles==1,,drop=FALSE],d$CF..ansbin.[roles==1])))
      atomic(selected,path)
    }
    selected<-readRDS(path)
    stopifnot(selected$identity==identity,
      identical(selected$train_rows,which(roles==1)),
      identical(selected$train_students,sort(unique(as.character(d$Anon.Student.Id[roles==1])))))
    meta<-selected$candidates;meta$input_column<-selected$numeric_names
    meta$parameters<-vapply(meta$parameters,function(v)paste(v,collapse=';'),character(1))
    study_write_csv(meta,file.path(out,'selection',paste0(part,'-selected.csv')))
    records[[part]]<-data.frame(partition=part,candidates=318L,selected=length(selected$columns),
      lambda=selected$lambda,training_students=length(selected$train_students))
  }
  study_write_csv(do.call(rbind,records),file.path(out,'selection/summary.csv'))
  cat('COMPLETE five train-only feature selections\n');quit(save='no')
}
if(action=='fit') {
  stopifnot(length(a)==5L);task<-p$tasks[as.integer(a[5]),,drop=FALSE];stopifnot(nrow(task)==1L)
  selected<-readRDS(file.path(out,'selection',paste0(task$partition,'.rds')))
  input<-selected
  s<-input$settings;s$seed<-as.integer(task$seed)
  identity<-hash_object(list(study=study_hash(file.path(out,'study.rds')),selection=study_hash(file.path(out,'selection',paste0(task$partition,'.rds'))),task=task,settings=s))
  path<-file.path(out,'models',paste0(task$run_id,'.rds'))
  if(!file.exists(path)) {
    cat('START',task$run_id,'with',length(selected$columns),'Lasso predictors\n');flush.console()
    elapsed<-system.time(fit<-LKT(input$data,usefolds=1L,components='KC..Default.',features='lineafm',
      model=do.call(GRUModel,s),verbose=TRUE))[['elapsed']]
    fit$newdata<-NULL;fit$nullmodel<-NULL
    atomic(list(identity=identity,task=task,settings=s,selection=selected$candidates,
      execution=list(job=Sys.getenv('SLURM_JOB_ID'),node=Sys.info()[['nodename']],session=utils::sessionInfo()),
      fit=fit,elapsed=elapsed),path)
  }
  saved<-readRDS(path);fit<-saved$fit;roles<-input$data$fold
  stopifnot(saved$identity==identity,identical(saved$settings,s),
    identical(fit$fit_rows,which(roles==1)),identical(fit$validation_rows,which(roles==2)),
    identical(fit$evaluation_rows,which(roles==3)),all(is.finite(fit$prediction)),
    identical(fit$model$settings$numeric_cols,selected$numeric_names),
    length(fit$model$preprocessing$numeric_names)==length(selected$columns)+3L,
    fit$parameter_count==p$baseline_parameter_count+
      (length(selected$columns)-length(p$baseline_recipe$features))*p$settings$encoder_hidden_sizes[1L])
  held<-input$data[roles==3,,drop=FALSE]
  replay_saved<-function() {
    cleanup<-internal('lkt_gru_runtime')(s);on.exit(cleanup())
    model<-internal('lkt_gru_model')(fit)
    ord<-internal('lkt_gru_order')(held,s);ordered<-held[ord,,drop=FALSE]
    encoded<-internal('lkt_gru_encode')(ordered,model$preprocessing,model$recipe,s)
    net<-LKTGRURuntime(fit)$network
    internal('lkt_gru_replay')(net,encoded,ordered,s)[order(ord)]
  }
  replay<-replay_saved()
  difference<-max(abs(replay-fit$prediction[roles==3]));stopifnot(difference<3e-6)
  # Export the same canonical probabilities used for the metrics. Saved-weight
  # replay is checked above; floating-point replay rounding must not change the
  # tied-rank AUC audit's input while its reference still uses fit$prediction.
  probabilities<-fit$prediction
  study_write_csv(data.frame(source_row=input$data$comparison_source_row,student=input$data$Anon.Student.Id,
    partition=task$partition,seed=task$seed,role=roles,
    observed=input$data$CF..ansbin.,prediction=probabilities),file.path(out,'reports',paste0(task$run_id,'-predictions.csv')))
  metrics<-unlist(lapply(1:3,function(k) {
    value<-study_metrics(input$data$CF..ansbin.[roles==k],fit$prediction[roles==k])
    setNames(value,paste0(c('training','validation','heldout')[k],'_',names(value)))
  }))
  row<-cbind(task,as.data.frame(as.list(metrics)),data.frame(selected_features=length(selected$columns),
    selected_epoch=fit$selected_epoch,trained_epochs=fit$trained_epochs,elapsed_seconds=saved$elapsed,
    parameters=fit$parameter_count,maximum_replay_difference=difference))
  study_write_csv(row,file.path(out,'reports',paste0(task$run_id,'-metrics.csv')))
  study_write_csv(fit$training_history,file.path(out,'training',paste0(task$run_id,'.csv')))
  cat('PASS',task$run_id,'saved-model held-out replay\n');quit(save='no')
}
if(action=='report') {
  paths<-file.path(out,'reports',paste0(p$tasks$run_id,'-metrics.csv'))
  stopifnot(all(file.exists(paths)))
  result<-do.call(rbind,lapply(paths,read.csv))
  study_write_csv(result,file.path(out,'reports/fold-seed-results.csv'))
  records<-list()
  for(seed in 1:3) {
    tasks<-p$tasks[p$tasks$seed==seed,,drop=FALSE]
    rows<-do.call(rbind,lapply(tasks$run_id,function(id)read.csv(file.path(out,'reports',paste0(id,'-predictions.csv')))))
    rows<-rows[rows$role==3,,drop=FALSE]
    stopifnot(nrow(rows)==nrow(d),!anyDuplicated(rows$source_row))
    records[[as.character(seed)]]<-cbind(seed=seed,as.data.frame(as.list(study_metrics(rows$observed,rows$prediction))))
  }
  pooled<-do.call(rbind,records);study_write_csv(pooled,file.path(out,'reports/heldout-by-seed.csv'))
  summary<-data.frame(condition='lasso15_constant',fits=nrow(result),
    training_AUC=mean(result$training_AUC),validation_AUC=mean(result$validation_AUC),heldout_AUC=mean(pooled$AUC),
    training_log_loss=mean(result$training_log_loss),validation_log_loss=mean(result$validation_log_loss),heldout_log_loss=mean(pooled$log_loss))
  study_write_csv(summary,file.path(out,'reports/summary.csv'));print(summary)
}
