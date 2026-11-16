# Matched Cloze numeric-input ablation of the current default architecture.
# ROOT OUTPUT ACTION PARTITION CANDIDATE_JSON CANDIDATE_LIBRARY [REFERENCE]
args <- commandArgs(TRUE);stopifnot(length(args) %in% c(6L,7L))
root <- normalizePath(args[1],winslash='/',mustWork=TRUE)
output <- args[2];action <- args[3];part <- args[4]
stopifnot(action %in% c('plan','verify','smoke','fit','report','check'))
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
source(file.path(root,'scripts/gru-encoder-study.R'))
candidate_file <- normalizePath(args[5],winslash='/',mustWork=TRUE)
candidate <- study_require_release(candidate_file,args[6])
stopifnot(isTRUE(candidate$validation_candidate),
  study_hash(file.path(dirname(candidate_file),candidate$source_archive))==candidate$source_sha256)
source(file.path(root,'scripts/report-gru-numeric-ablation.R'))
suppressPackageStartupMessages(library(data.table))
script_names <- c('scripts/compare-gru-numeric-ablation.R','scripts/report-gru-numeric-ablation.R',
  'scripts/gru-encoder-study.R')
hashes <- setNames(vapply(file.path(root,script_names),study_hash,character(1)),script_names)
conditions <- c('none','all',paste0('only',1:7),paste0('omit',1:7))
tasks_for <- function(part)do.call(rbind,lapply(conditions,function(condition)do.call(rbind,lapply(1:3,function(seed)
  data.frame(run_id=paste(part,condition,paste0('seed',seed),sep='__'),partition=part,
    family='augmented',stage=condition,candidate=condition,seed=seed)))))

if(action=='plan') {
  stopifnot(length(args)==7L,!dir.exists(output))
  reference <- args[7];old <- readRDS(file.path(reference,'study.rds'))
  d <- readRDS(file.path(reference,'data/prepared_data.rds'))
  stopifnot(study_hash(file.path(reference,'data/prepared_data.rds'))==old$data_sha256,
    nrow(d)==55122L,length(unique(d$Anon.Student.Id))==478L,identical(old$seeds,1:3))
  base <- do.call(GRUModel,old$settings$simplified)$settings
  stopifnot(base$numeric_encoder=='combined',base$response_encoding=='binary',
    base$retrieval_value_mode=='raw',identical(old$intercepts[['simplified']],FALSE),
    base$epochs==100L,base$batch_size==32L,base$patience==10L,base$min_delta==1e-4,
    base$learning_rate==.001,base$weight_decay==1e-4,base$validation_folds==2,
    base$sequence_length==100L,base$restore_best,base$early_stopping,is.null(base$numeric_feature_cols))
  recipe <- old$baseline;recipe$interc <- FALSE
  d$fold <- readRDS(file.path(reference,'partitions/cv1.rds'))
  numeric_names <- getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1,recipe,base)$numeric_names
  expected <- c('time:log1p_gap_seconds','time:log1p_gru_item_age_seconds',
    'time:log1p_gru_mean_prior_spacing_seconds','time:available_gru_mean_prior_spacing_seconds',
    'lkt:logitdecKC..Default.','lkt:ppesKC..Default.','lkt:logitdecAnon.Student.Id')
  stopifnot(identical(numeric_names,expected))
  labels <- c('Time since previous response','Item age','Mean prior spacing','Prior spacing available',
    'Student-item logitdec','Student-item ppes','Student logitdec')
  settings <- setNames(lapply(conditions,function(condition) {
    s <- base
    if(condition=='none')s$numeric_feature_cols <- character()
    if(startsWith(condition,'only'))s$numeric_feature_cols <- numeric_names[as.integer(sub('only','',condition))]
    if(startsWith(condition,'omit'))s$numeric_feature_cols <- numeric_names[-as.integer(sub('omit','',condition))]
    do.call(GRUModel,s)$settings
  }),conditions)
  feature_table <- data.frame(index=1:7,name=numeric_names,label=labels)
  dir.create(output,recursive=TRUE)
  for(folder in c('data','partitions','models','validation','training','reports'))dir.create(file.path(output,folder))
  stopifnot(file.copy(file.path(reference,'data/prepared_data.rds'),file.path(output,'data/prepared_data.rds')))
  parts <- paste0('cv',1:5);controls <- list()
  for(k in parts) {
    f <- file.path(reference,'partitions',paste0(k,'.rds'))
    stopifnot(study_hash(f)==old$partition_hashes[[k]],file.copy(f,file.path(output,'partitions',basename(f))))
    roles <- readRDS(f);groups <- unique(data.frame(id=d$Anon.Student.Id,role=roles))
    stopifnot(length(roles)==nrow(d),!anyDuplicated(groups$id),all(roles %in% 1:3))
    for(seed in 1:3) {
      f <- file.path(reference,'models',paste0(k,'__simplified__seed',seed,'.rds'))
      fit <- readRDS(f)$fit;s <- base;s$seed <- as.integer(seed)
      stopifnot(identical(do.call(GRUModel,fit$gru_settings)$settings,s))
      controls[[paste(k,seed)]] <- list(prediction=fit$prediction,source_sha256=study_hash(f))
    }
  }
  study_write_rds(controls,file.path(output,'data/reference-controls.rds'))
  p <- list(schema=1L,candidate=candidate,candidate_file_sha256=study_hash(candidate_file),
    script_hashes=hashes,data_sha256=old$data_sha256,
    reference_protocol_sha256=study_hash(file.path(reference,'study.rds')),
    control_sha256=study_hash(file.path(output,'data/reference-controls.rds')),
    partitions=parts,partition_hashes=old$partition_hashes[parts],seeds=1:3,total_fits=240L,shards=8L,
    settings=settings,baseline=recipe,features=feature_table,
    intercepts=setNames(rep(FALSE,16L),conditions),
    parameter_counts=setNames(c(33601L,43105L,rep(42913L,7),rep(43073L,7)),conditions),
    design='Fourteen ablations plus fresh no-input and all-seven controls; five exact existing participant folds and seeds 1:3.',
    checkpoints='Unchanged 100 epochs, patience 10, min_delta 1e-4, minimum validation-log-loss checkpoint, optimizer, widths and training settings.',
    reporting='Train/validation average fifteen restored fits; held-out AUC pools outer folds within seed then averages seeds. No model or seed selection on held-out outcomes.',
    limits=c('Exploratory previously inspected Cloze data.',
      'Inherited nonlinear feature constants remain fixed.',
      'Removing numeric inputs changes parameter counts; no artificial zero placeholders.',
      'Missing prior spacing is neutral after scaling; its availability flag is independently selected.',
      'No warm starts, training search or active release promotion.'))
  study_write_rds(p,file.path(output,'study.rds'))
  writeLines(capture.output(dput(p)),file.path(output,'study.txt'))
  cat('FROZEN: 240 fresh fits, exact existing folds, seeds and training settings.\n');quit(save='no')
}
p <- readRDS(file.path(output,'study.rds'))
stopifnot(identical(p$candidate,candidate),identical(p$script_hashes,hashes),
  study_hash(candidate_file)==p$candidate_file_sha256,
  study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256,
  study_hash(file.path(output,'data/reference-controls.rds'))==p$control_sha256)
for(k in p$partitions)stopifnot(study_hash(file.path(output,'partitions',paste0(k,'.rds')))==p$partition_hashes[[k]])

if(action=='verify') {
  d <- readRDS(file.path(output,'data/prepared_data.rds'));counts <- list()
  for(k in p$partitions)for(condition in conditions) {
    d$fold <- readRDS(file.path(output,'partitions',paste0(k,'.rds')))
    recipe <- p$baseline;recipe$interc <- unname(p$intercepts[condition]);s <- p$settings[[condition]]
    prep <- getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1L,recipe,s)
    net <- getFromNamespace('lkt_gru_network','LKT')(prep,s)
    n <- sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
    expected_names <- if(is.null(s$numeric_feature_cols))p$features$name else s$numeric_feature_cols
    stopifnot(n==p$parameter_counts[[condition]],identical(as.character(prep$numeric_names),expected_names))
    counts[[paste(k,condition)]] <- data.frame(partition=k,condition=condition,parameters=n,numeric_inputs=length(prep$numeric_names))
  }
  study_write_csv(do.call(rbind,counts),file.path(output,'reports/preflight.csv'))
  cat('PASS candidate identity, exact data/folds, all sixteen numeric-input selections and all parameter counts.\n')
}
if(action=='smoke') {
  original <- readRDS(file.path(output,'data/prepared_data.rds'))
  rows <- head(which(original$Anon.Student.Id==original$Anon.Student.Id[1L]),12L)
  d <- do.call(rbind,lapply(1:8,function(i) {
    x <- original[rows,,drop=FALSE];x$Anon.Student.Id <- paste0('synthetic',i)
    x$CF..ansbin. <- rep(c(0,1,1,0,1,0),2L);x$fold <- c(1L,1L,1L,1L,1L,2L,3L,3L)[i];x
  }))
  smoke <- file.path(dirname(output),'smoke');stopifnot(!dir.exists(smoke))
  for(folder in c('models','validation','training'))dir.create(file.path(smoke,folder),recursive=TRUE)
  study_write_rds(p,file.path(smoke,'study.rds'))
  for(condition in conditions) {
    s <- p$settings[[condition]];s$epochs <- 1L;s$early_stopping <- FALSE;s$restore_best <- FALSE
    task <- tasks_for('smoke');task <- task[task$candidate==condition & task$seed==1L,,drop=FALSE]
    study_fit_one(task,s,smoke,p,d,intercept=unname(p$intercepts[condition]))
    f <- readRDS(file.path(smoke,'models',paste0(task$run_id,'.rds')))$fit
    stopifnot(identical(f$model$recipe$interc,unname(p$intercepts[condition])),
      all(is.finite(unlist(f$model$weights))),all(is.finite(f$prediction)),
      max(abs(study_replay(f,d[d$fold==3,,drop=FALSE])-f$prediction[d$fold==3]))<3e-6)
  }
  cat('PASS all sixteen one-epoch smoke fits, intercept routing, finite weights and saved-weight replay.\n')
}
if(action=='fit') {
  bits <- strsplit(part,':',fixed=TRUE)[[1L]];stopifnot(length(bits)==2L)
  part <- bits[1];shard <- as.integer(bits[2]);stopifnot(part %in% p$partitions,shard %in% 0:7)
  shard_id <- paste0(part,'-shard',shard)
  d <- readRDS(file.path(output,'data/prepared_data.rds'))
  d$fold <- readRDS(file.path(output,'partitions',paste0(part,'.rds')));tasks <- tasks_for(part)
  tasks <- tasks[(seq_len(nrow(tasks))-1L)%%p$shards==shard,,drop=FALSE]
  stopifnot(nrow(tasks)==6L)
  workers <- as.integer(Sys.getenv('LKT_STUDY_WORKERS','1'));stopifnot(workers>=1L,workers<=6L)
  if(workers==1L)for(i in seq_len(nrow(tasks))) {
    t <- tasks[i,,drop=FALSE]
    study_fit_one(t,p$settings[[t$candidate]],output,p,d,intercept=unname(p$intercepts[t$candidate]))
  } else {
    cl <- parallel::makePSOCKcluster(workers,outfile=file.path(output,paste0(shard_id,'-workers.log')))
    tryCatch({
      parallel::clusterCall(cl,function(root,candidate_file,lib) {
        assign('root',root,envir=.GlobalEnv)
        source(file.path(root,'scripts/gru-encoder-study.R'))
        study_require_release(candidate_file,lib);NULL
      },root,candidate_file,args[6])
      parallel::clusterExport(cl,c('p','d','tasks','output'))
      parallel::parLapplyLB(cl,seq_len(nrow(tasks)),function(i) {
        t <- tasks[i,,drop=FALSE]
        study_fit_one(t,p$settings[[t$candidate]],output,p,d,intercept=unname(p$intercepts[t$candidate]))
      })
    },finally=parallel::stopCluster(cl))
  }
  records <- list();test <- d[d$fold==3,,drop=FALSE]
  for(i in seq_len(nrow(tasks))) {
    t <- tasks[i,,drop=FALSE];f <- readRDS(file.path(output,'models',paste0(t$run_id,'.rds')))$fit
    q <- study_replay(f,test);stopifnot(max(abs(q-f$prediction[d$fold==3]))<3e-6)
    records[[i]] <- data.frame(partition=part,condition=t$candidate,seed=t$seed,
      source_row=test$comparison_source_row,participant=test$Anon.Student.Id,observed=test$CF..ansbin.,prediction=q)
  }
  study_write_csv(do.call(rbind,records),file.path(output,'reports',paste0(shard_id,'-predictions.csv')))
  writeLines('PASS all six checkpoint settings/masks and held-out saved-weight replays.',file.path(output,'reports',paste0(shard_id,'-verification.txt')))
}
if(action %in% c('report','check'))numeric_ablation_report(output,p,tasks_for,action)
