# Existing released base under author-aligned EdNet data/evaluation protocols.
# ROOT OUTPUT plan ktst|unvamp BASE_STUDY SOURCE
# ROOT OUTPUT verify|report|check
# ROOT OUTPUT fit TASK_ID
args <- commandArgs(TRUE);stopifnot(length(args) %in% c(3L,4L,6L))
root <- normalizePath(args[1],winslash='/',mustWork=TRUE);output <- args[2];action <- args[3]
stopifnot(action %in% c('plan','verify','fit','report','check'))
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-encoder-study.R'))
source(file.path(root,'scripts/ednet-author-protocol.R'))
suppressPackageStartupMessages(library(data.table))
release <- lkt_release[c('lkt_version','lkt_source_archive','lkt_source_sha256','lkt_manifest_sha256')]
script_names <- c('scripts/run-gru-ednet-author-protocol.R','scripts/ednet-author-protocol.R',
  'scripts/gru-encoder-study.R','scripts/load-current-lkt.R','workflows/simulation/shared/lkt_release.R')
script_files <- c(file.path(root,head(script_names,-1)),file.path(dirname(attr(lkt_release,'file')),'lkt_release.R'))
script_hashes <- setNames(vapply(script_files,study_hash,character(1)),script_names)

if(action=='plan') {
  stopifnot(length(args)==6L,!dir.exists(output),args[4] %in% c('ktst','unvamp'))
  protocol <- args[4];base_path <- normalizePath(args[5],winslash='/',mustWork=TRUE)
  origin <- readRDS(file.path(base_path,'study.rds'));stopifnot(identical(origin$release,release))
  s <- do.call(GRUModel,origin$settings)$settings
  stopifnot(identical(s,origin$settings),s$num_layers==3L,s$hidden_size==32L,
    s$feature_encoder=='mlp',s$retrieval_heads==1L,s$inference_mode=='none',s$unknown_levels=='zero')
  source_path <- normalizePath(args[6],winslash='/',mustWork=TRUE)
  preparation_references <- character()
  if(protocol=='ktst') {
    path <- file.path(base_path,'data/prepared_data.rds');stopifnot(study_hash(path)==origin$data_sha256)
    d <- readRDS(path);d$fold <- NULL;d$benchmark_position <- d$ktst_position
    tasks <- ednet_author_tasks(source_path)
    partitions <- setNames(lapply(tasks$author_fold,function(k)ednet_author_roles(d,k)),tasks$fold)
    author_files <- file.path(source_path,'pkg/config/reproduce/ktst/benchmark',tasks$author_config)
    provenance <- list(original_protocol_sha256=study_hash(file.path(base_path,'study.rds')),
      original_data_sha256=origin$data_sha256,cleaning=origin$input_provenance,
      author_configs=setNames(vapply(author_files,study_hash,character(1)),tasks$author_config))
    description <- 'Released KTST mean fold/seed pairs, fixed original test students; mean of five fitted-model test AUCs.'
  } else {
    provenance <- jsonlite::fromJSON(file.path(source_path,'provenance.json'),simplifyVector=FALSE)
    preparation_references <- c('provenance.json','sampled-students.csv','participant-assignments.csv','sampling-audit.csv')
    if(identical(provenance$threshold_scope,'kt1')) {
      preparation_references <- c(preparation_references,'global-item-counts.csv','global-eligible-students.csv')
    } else if(identical(provenance$cap_order,'after-filtering')) {
      eligibility_path <- file.path(source_path,'item-eligibility-before-cap.csv')
      stopifnot(study_hash(eligibility_path)==provenance$item_eligibility_before_cap_sha256)
      eligibility <- fread(eligibility_path,colClasses=c(question_id='character'))
      stopifnot(!anyDuplicated(eligibility$question_id),all(eligibility$distinct_students>=100L))
      preparation_references <- c(preparation_references,'item-eligibility-before-cap.csv')
    }
    stopifnot(all(file.exists(file.path(source_path,preparation_references))))
    input <- file.path(source_path,'ednet-lkt.csv');stopifnot(study_hash(input)==provenance$exported_data_sha256)
    d <- as.data.frame(fread(input,colClasses=c('Anon.Student.Id'='character','KC..Default.'='character',
      'KC..Cluster.'='character','comparison_source_row'='character')))
    stopifnot(nrow(d)==provenance$responses,all(d$Duration..sec.>=0),!anyNA(d$Duration..sec.))
    if(identical(provenance$threshold_scope,'sample') && identical(provenance$cap_order,'after-filtering'))
      stopifnot(all(d$KC..Default. %in% eligibility$question_id))
    d$CF..reltime. <- getFromNamespace('practiceTime','LKT')(d)
    expected <- as.numeric(ave(d$Duration..sec.,d$Anon.Student.Id,FUN=function(x)c(0,head(cumsum(x),-1L))))
    stopifnot(identical(d$CF..reltime.,expected));d$Duration..sec. <- NULL
    d <- LKTGRUTimeFeatures(d)
    partitions <- list(holdout=as.integer(d$author_phase));ednet_check_roles(d,partitions$holdout)
    tasks <- data.frame(fold='holdout',author_fold=NA_integer_,seed=1:3,author_config=NA_character_,
      id=paste0('holdout__seed',1:3),stringsAsFactors=FALSE)
    description <- 'UNVaMP paper-aligned cleaning and fixed 60/20/20 whole-student split; three fresh base-model seeds.'
  }
  stopifnot(!anyDuplicated(d$comparison_source_row),all(d$CF..ansbin. %in% 0:1),
    all(d$benchmark_position>=1),!anyNA(d$CF..Time.))
  dir.create(output,recursive=TRUE)
  for(folder in c('data','partitions','models','training','reports','reference'))dir.create(file.path(output,folder))
  study_write_rds(d,file.path(output,'data/prepared_data.rds'))
  for(name in names(partitions))study_write_rds(partitions[[name]],file.path(output,'partitions',paste0(name,'.rds')))
  if(protocol=='ktst')for(path in author_files)stopifnot(file.copy(path,file.path(output,'reference',basename(path))))
  for(name in preparation_references)stopifnot(file.copy(file.path(source_path,name),file.path(output,'reference',name)))
  p <- list(schema=1L,protocol=protocol,description=description,release=release,
    release_file_sha256=study_hash(attr(lkt_release,'file')),script_hashes=script_hashes,
    data_sha256=study_hash(file.path(output,'data/prepared_data.rds')),
    partition_hashes=setNames(vapply(names(partitions),function(n)study_hash(file.path(output,'partitions',paste0(n,'.rds'))),character(1)),names(partitions)),
    settings=s,recipe=origin$recipe,tasks=tasks,provenance=provenance,
    preparation_reference_hashes=setNames(vapply(preparation_references,function(name)
      study_hash(file.path(output,'reference',name)),character(1)),preparation_references),
    base_protocol_sha256=study_hash(file.path(base_path,'study.rds')),bootstrap_draws=2000L,bootstrap_seed=20261105L,
    policy=c('Only the existing released base; no tuning or neural package changes. All weights start fresh.',
      'Same fixed base optimizer, features, nonlinear coefficients, chunks, retrieval, stopping and checkpoint criterion.',
      'Whole-student roles; training-only vocabulary/scaling; held-out-only identities explicitly zero encoded.',
      'Primary next-response AUC excludes first response; all-response results are also reported.',
      'Average individual model AUCs, without prediction ensembling or duplicated-test-row pooling.',
      'Paper-matched data/evaluation protocol; not reproduction of authors architecture, tuning or history limits.'))
  study_write_rds(p,file.path(output,'study.rds'));study_write_csv(tasks,file.path(output,'tasks.csv'))
  counts <- do.call(rbind,lapply(names(partitions),function(name)do.call(rbind,lapply(1:3,function(role) {
    keep <- partitions[[name]]==role
    data.frame(fold=name,phase=c('training','validation','test')[role],students=length(unique(d$Anon.Student.Id[keep])),
      responses=sum(keep),next_response_targets=sum(keep & d$benchmark_position>1L))
  }))))
  study_write_csv(counts,file.path(output,'role-counts.csv'))
  jsonlite::write_json(list(protocol=protocol,description=description,fresh_fits=nrow(tasks),tasks=tasks,
    counts=counts,settings=s,policy=p$policy,provenance=provenance),file.path(output,'plan.json'),pretty=TRUE,auto_unbox=TRUE,null='null')
  cat('FROZEN',protocol,nrow(d),'responses;',nrow(tasks),'fresh fits.\n');quit(save='no')
}

p <- readRDS(file.path(output,'study.rds'))
stopifnot(identical(release,p$release),identical(script_hashes,p$script_hashes),
  study_hash(attr(lkt_release,'file'))==p$release_file_sha256,
  study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256)
for(name in names(p$preparation_reference_hashes))
  stopifnot(study_hash(file.path(output,'reference',name))==p$preparation_reference_hashes[[name]])
d <- readRDS(file.path(output,'data/prepared_data.rds'))
roles_for <- function(task) {
  path <- file.path(output,'partitions',paste0(task$fold,'.rds'))
  stopifnot(study_hash(path)==p$partition_hashes[[task$fold]])
  roles <- readRDS(path);ednet_check_roles(d,roles);roles
}

if(action=='verify') {
  for(i in seq_len(nrow(p$tasks))) {
    task <- p$tasks[i,,drop=FALSE];roles <- roles_for(task)
    if(p$protocol=='ktst')stopifnot(identical(roles,ednet_author_roles(d,task$author_fold)))
    data <- d;data$fold <- roles
    prep <- getFromNamespace('lkt_gru_preprocess','LKT')(data,roles==1L,p$recipe,p$settings)
    net <- getFromNamespace('lkt_gru_network','LKT')(prep,p$settings)
    stopifnot(all(prep$embedding_dims==16L),net$head[[1]]$weight$size(1)==128L)
    rm(net,prep);gc()
  }
  cat('PASS frozen release, all author roles, train-only base schemas and model dimensions.\n')
}

if(action=='fit') {
  stopifnot(length(args)==4L);task <- p$tasks[p$tasks$id==args[4],,drop=FALSE];stopifnot(nrow(task)==1L)
  path <- file.path(output,'models',paste0(task$id,'.rds'));stopifnot(!file.exists(path))
  roles <- roles_for(task);data <- d;data$fold <- roles;s <- p$settings;s$seed <- as.integer(task$seed)
  cat('START',task$id,format(Sys.time(),tz='UTC'),'\n');flush.console()
  elapsed <- system.time(fit <- LKT(data.table::copy(as.data.table(data)),usefolds=1L,
    components=p$recipe$components,features=p$recipe$features,fixedpars=p$recipe$fixedpars,
    interc=p$recipe$interc,model=do.call(GRUModel,s),verbose=TRUE))[['elapsed']]
  fit$newdata <- NULL;fit$nullmodel <- NULL
  stopifnot(identical(fit$gru_settings,s),identical(fit$fit_rows,which(roles==1L)),
    identical(fit$validation_rows,which(roles==2L)),identical(fit$evaluation_rows,which(roles==3L)))
  test <- data[roles==3L,,drop=FALSE];replay <- study_replay(fit,test)
  error <- max(abs(replay-fit$prediction[roles==3L]));stopifnot(error<3e-6)
  saved <- list(task=task,protocol_sha256=study_hash(file.path(output,'study.rds')),fit=fit,
    elapsed_seconds=elapsed,replay_max_error=error)
  study_write_rds(saved,path)
  study_write_csv(fit$training_history,file.path(output,'training',paste0(task$id,'.csv')))
  writeLines('PASS portable saved-weight held-out replay.',file.path(output,'reports',paste0(task$id,'-verification.txt')))
  cat('DONE',task$id,'independent replay error',error,'\n')
}

if(action %in% c('report','check')) {
  metrics <- list();test_rows <- NULL;predictions <- NULL
  for(i in seq_len(nrow(p$tasks))) {
    task <- p$tasks[i,,drop=FALSE];roles <- roles_for(task)
    saved <- readRDS(file.path(output,'models',paste0(task$id,'.rds')));fit <- saved$fit
    s <- p$settings;s$seed <- as.integer(task$seed)
    stopifnot(identical(saved$task,task),identical(fit$gru_settings,s),saved$replay_max_error<3e-6,
      saved$protocol_sha256==study_hash(file.path(output,'study.rds')),
      identical(fit$fit_rows,which(roles==1L)),identical(fit$validation_rows,which(roles==2L)),
      identical(fit$evaluation_rows,which(roles==3L)),all(is.finite(fit$prediction)),
      file.exists(file.path(output,'reports',paste0(task$id,'-verification.txt'))))
    rows <- which(roles==3L & d$benchmark_position>1L)
    if(is.null(test_rows)) {test_rows <- rows;predictions <- matrix(NA_real_,length(rows),nrow(p$tasks))}
    stopifnot(identical(test_rows,rows));predictions[,i] <- fit$prediction[rows]
    for(role in 1:3)for(include_first in c(FALSE,TRUE)) {
      keep <- roles==role & (include_first | d$benchmark_position>1L)
      metrics[[length(metrics)+1L]] <- data.frame(task=task$id,fold=task$fold,seed=task$seed,
        phase=c('training','validation','test')[role],include_first_response=include_first,responses=sum(keep),
        as.list(study_metrics(d$CF..ansbin.[keep],fit$prediction[keep])),
        selected_epoch=fit$selected_epoch,trained_epochs=fit$trained_epochs,
        parameter_count=fit$parameter_count,fit_seconds=saved$elapsed_seconds)
    }
    train_auc <- as.numeric(pROC::auc(d$CF..ansbin.[roles==1],fit$prediction[roles==1],quiet=TRUE,direction='<'))
    stopifnot(abs(train_auc-fit$training_history$training_auc[match(fit$selected_epoch,fit$training_history$epoch)])<1e-12)
    rm(fit,saved);gc()
  }
  metrics <- rbindlist(metrics)
  summary <- metrics[,.(AUC=mean(AUC),population_sd=sqrt(mean((AUC-mean(AUC))^2)),sample_sd=sd(AUC),
    mean_log_loss=mean(log_loss),mean_Brier=mean(Brier),mean_responses=mean(responses),
    min_responses=min(responses),max_responses=max(responses)),by=.(phase,include_first_response)]
  if(action=='report') {
    study_write_csv(metrics,file.path(output,'reports/metrics-by-fit.csv'))
    study_write_csv(summary,file.path(output,'reports/metrics-summary.csv'))
    draws <- ednet_author_bootstrap(d$CF..ansbin.[test_rows],predictions,d$Anon.Student.Id[test_rows],p$bootstrap_draws,p$bootstrap_seed)
    stopifnot(all(is.finite(draws)))
    study_write_rds(draws,file.path(output,'reports/test-auc-bootstrap.rds'))
    ci <- data.frame(mean_AUC=mean(metrics[phase=='test' & !include_first_response]$AUC),
      lower_95=unname(quantile(draws,.025)),upper_95=unname(quantile(draws,.975)),
      students=length(unique(d$Anon.Student.Id[test_rows])),responses=length(test_rows),fits=nrow(p$tasks))
    study_write_csv(ci,file.path(output,'reports/test-auc-bootstrap.csv'))
    writeLines('PASS every fresh fit: settings, whole-student roles, held-out replay, restored pROC AUC and fixed-test aggregation.',file.path(output,'reports/verification.txt'))
  } else stopifnot(isTRUE(all.equal(as.data.frame(fread(file.path(output,'reports/metrics-summary.csv'))),as.data.frame(summary),tolerance=1e-12,check.attributes=FALSE)))
  print(summary);cat('PASS completed author-protocol',action,'\n')
}
