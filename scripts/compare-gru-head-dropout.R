# Vary only prediction-head dropout in the exact completed Cloze baseline.
# Args: ROOT OUTPUT plan|verify|smoke|fit|report|check PARTITION SHARD [REFERENCE_RESULTS]
args <- commandArgs(TRUE);stopifnot(length(args) %in% c(5L,6L))
root <- normalizePath(args[1],winslash='/',mustWork=TRUE)
output <- args[2];action <- args[3];partition <- args[4];shard <- as.integer(args[5])
stopifnot(action %in% c('plan','verify','smoke','fit','report','check'))
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-encoder-study.R'))
suppressPackageStartupMessages(library(data.table))
script_names <- c('scripts/compare-gru-head-dropout.R','scripts/gru-training-grid-report.R','scripts/gru-encoder-study.R',
  'scripts/load-current-lkt.R','workflows/simulation/shared/lkt_release.R')
script_files <- c(file.path(root,head(script_names,-1)),file.path(dirname(attr(lkt_release,'file')),'lkt_release.R'))
script_hashes <- setNames(vapply(script_files,study_hash,character(1)),script_names)
release <- lkt_release[c('lkt_version','lkt_source_archive','lkt_source_sha256','lkt_manifest_sha256')]
stopifnot(study_hash(attr(lkt_release,'archive'))==release[['lkt_source_sha256']])

if(action=='plan') {
  stopifnot(length(args)==6L,!dir.exists(output))
  reference <- args[6];old <- readRDS(file.path(reference,'study.rds'))
  d <- readRDS(file.path(reference,'data/prepared_data.rds'))
  stopifnot(nrow(d)==55122L,length(unique(d$Anon.Student.Id))==478L,
    study_hash(file.path(reference,'data/prepared_data.rds'))==old$data_sha256,
    identical(old$seeds,1:3),identical(old$release,release))
  base <- do.call(GRUModel,old$settings$baseline)$settings
  stopifnot(base$num_layers==3L,base$hidden_size==32L,identical(base$head_hidden_sizes,128L),
    base$learning_rate==.001,base$input_dropout==.1,base$gru_dropout==.1,base$head_dropout==.1,
    base$batch_size==32L,base$weight_decay==.0001,base$feature_encoder=='mlp',base$inference_mode=='none',
    base$retrieval_heads==1L,base$retrieval_window==100L,base$retrieval_decay=='none',
    identical(base$zero_init_embedding_cols,'KC..Default.'),
    identical(base$retrieval_filter_cols,c('KC..Default.','KC..Cluster.')),
    base$numeric_fusion=='concat',base$categorical_fusion=='concat',base$input_fusion=='concat')
  # Keep input/recurrent dropout and every other control identical to baseline.
  grid <- data.frame(head_dropout=c(.01,.05,.1,.2),
    condition=c('head001','head005','baseline','head020'))
  settings <- setNames(lapply(grid$head_dropout,function(value) {
    s <- base;s$head_dropout <- value;do.call(GRUModel,s)$settings
  }),grid$condition)
  varied <- 'head_dropout'
  for(s in settings)stopifnot(identical(s[setdiff(names(s),varied)],base[setdiff(names(base),varied)]))
  stopifnot(identical(settings$baseline,base),nrow(unique(grid))==4L)
  dir.create(output,recursive=TRUE)
  for(name in c('data','partitions','models','training','validation','reports'))dir.create(file.path(output,name))
  stopifnot(file.copy(file.path(reference,'data/prepared_data.rds'),file.path(output,'data/prepared_data.rds')))
  parts <- paste0('cv',1:5)
  controls <- list()
  for(part in parts) {
    f <- file.path(reference,'partitions',paste0(part,'.rds'))
    stopifnot(study_hash(f)==old$partition_hashes[[part]],file.copy(f,file.path(output,'partitions',basename(f))))
    fold <- readRDS(f);groups <- unique(data.frame(participant=d$Anon.Student.Id,fold=fold))
    stopifnot(length(fold)==nrow(d),!anyDuplicated(groups$participant),all(fold %in% 1:3))
    for(seed in 1:3) {
      path <- file.path(reference,'models',paste0(part,'__baseline__seed',seed,'.rds'))
      fit <- readRDS(path)$fit;s <- base;s$seed <- as.integer(seed)
      stopifnot(identical(do.call(GRUModel,fit$gru_settings)$settings,s))
      controls[[paste(part,seed)]] <- list(prediction=fit$prediction,source_sha256=study_hash(path))
    }
  }
  study_write_rds(controls,file.path(output,'data/reference-controls.rds'))
  p <- list(schema=1L,release=release,release_file_sha256=study_hash(attr(lkt_release,'file')),
    script_hashes=script_hashes,data_sha256=old$data_sha256,
    reference_protocol_sha256=study_hash(file.path(reference,'study.rds')),
    control_sha256=study_hash(file.path(output,'data/reference-controls.rds')),
    partitions=parts,partition_hashes=old$partition_hashes[parts],seeds=1:3,shards=2L,total_fits=60L,
    settings=settings,grid=grid,baseline=old$baseline,parameter_count=46257L,
    design='Four head-dropout probabilities (.01, .05, .1, .2) x five existing participant folds x three fresh seeds: 60 fits. Only head_dropout varies; no development fits.',
    selection='Within each outer fold, choose the setting with highest mean participant-validation AUC over seeds 1:3; exact ties choose earlier grid order. Then score its held-out test students.',
    checkpoints='Unchanged base stopping and restoration criterion, max 100 epochs and patience 10.',
    caveats=c('The dataset and architecture were previously inspected; this nests only the current grid selection within the existing participant folds.',
      'Report all four fixed-rate held-out CV AUCs as exploratory comparisons. The secondary selected-rate analysis uses validation only within each outer fold.',
      'Only head dropout changes; input and between-GRU-layer dropout remain .1, and encoder/retrieval dropout remain zero.',
      'All four rates retain the same training budget and stopping/restoration rules. These data and architecture have already informed prior studies.',
      'Six nonlinear history-feature constants remain inherited fixed values. No history-feature search, new architecture or input relabeling is performed.',
      'AUC is computed per fitted seed and then averaged; predictions are not ensembled.'))
  study_write_rds(p,file.path(output,'study.rds'))
  study_write_csv(grid,file.path(output,'grid.csv'))
  writeLines(capture.output(dput(p)),file.path(output,'study.txt'))
  cat('FROZEN four head-dropout probabilities, five participant folds, three seeds: 60 fresh fits.\n')
  quit(save='no')
}

p <- readRDS(file.path(output,'study.rds'))
stopifnot(identical(p$release,release),identical(p$script_hashes,script_hashes),
  study_hash(attr(lkt_release,'file'))==p$release_file_sha256,
  study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256,
  study_hash(file.path(output,'data/reference-controls.rds'))==p$control_sha256)
for(part in p$partitions)stopifnot(study_hash(file.path(output,'partitions',paste0(part,'.rds')))==p$partition_hashes[[part]])
tasks_for <- function(part)do.call(rbind,lapply(names(p$settings),function(condition)do.call(rbind,lapply(p$seeds,function(seed)
  data.frame(run_id=paste(part,condition,paste0('seed',seed),sep='__'),partition=part,
    family='augmented',stage=condition,candidate=condition,seed=seed)))))

if(action=='verify') {
  # The data and architecture are common to every case. Check each requested
  # constructor and one real-data network rather than fitting toy replacements.
  d <- readRDS(file.path(output,'data/prepared_data.rds'))
  d$fold <- readRDS(file.path(output,'partitions/cv1.rds'))
  for(s in p$settings)stopifnot(identical(do.call(GRUModel,s)$settings,s))
  prep <- getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1,p$baseline,p$settings$baseline)
  torch::torch_manual_seed(1L)
  net <- getFromNamespace('lkt_gru_network','LKT')(prep,p$settings$baseline)
  stopifnot(sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))==p$parameter_count)
  initial <- getFromNamespace('lkt_gru_weights','LKT')(net)
  for(s in p$settings) {
    torch::torch_manual_seed(1L)
    other <- getFromNamespace('lkt_gru_network','LKT')(prep,s)
    stopifnot(identical(initial,getFromNamespace('lkt_gru_weights','LKT')(other)))
  }
  stopifnot(length(tasks_for('cv1')$seed)==12L)
  cat('PASS released runtime, exact data/folds, four head-dropout settings, identical seed-matched initial weights and 46,257 parameters.\n')
}

if(action=='smoke') {
  original <- readRDS(file.path(output,'data/prepared_data.rds'))
  rows <- head(which(original$Anon.Student.Id==original$Anon.Student.Id[1L]),12L)
  stopifnot(length(rows)==12L)
  d <- do.call(rbind,lapply(1:8,function(i) {
    x <- original[rows,,drop=FALSE];x$Anon.Student.Id <- paste0('synthetic',i)
    x$CF..ansbin. <- rep(c(0,1,1,0,1,0),2L);x$fold <- c(1L,1L,1L,1L,1L,2L,3L,3L)[i];x
  }))
  d$comparison_source_row <- seq_len(nrow(d))
  smoke_output <- file.path(dirname(output),'smoke')
  stopifnot(!dir.exists(smoke_output));dir.create(smoke_output)
  for(folder in c('models','training','validation'))dir.create(file.path(smoke_output,folder))
  smoke_plan <- p;smoke_plan$type <- 'synthetic one-epoch structural verification only'
  study_write_rds(smoke_plan,file.path(smoke_output,'study.rds'))
  for(condition in names(p$settings)) {
    s <- p$settings[[condition]];s$epochs <- 1L;s$early_stopping <- FALSE;s$restore_best <- FALSE
    task <- data.frame(run_id=paste0('smoke__',condition,'__seed1'),partition='smoke',
      family='augmented',stage=condition,candidate=condition,seed=1L)
    study_fit_one(task,s,smoke_output,smoke_plan,d)
    fit <- readRDS(file.path(smoke_output,'models',paste0(task$run_id,'.rds')))$fit
    stopifnot(all(is.finite(unlist(fit$model$weights))),all(is.finite(fit$prediction)),
      max(abs(study_replay(fit,d[d$fold==3L,,drop=FALSE])-fit$prediction[d$fold==3L]))<3e-6)
  }
  writeLines('PASS all four one-epoch structural fits: finite weights/predictions and saved-weight replay.',file.path(smoke_output,'verification.txt'))
  cat('PASS four head-dropout structural smoke fits.\n')
}

if(action=='fit') {
  stopifnot(partition %in% p$partitions,shard %in% 0:(p$shards-1L))
  d <- readRDS(file.path(output,'data/prepared_data.rds'))
  d$fold <- readRDS(file.path(output,'partitions',paste0(partition,'.rds')))
  all_tasks <- tasks_for(partition);tasks <- all_tasks[(seq_len(nrow(all_tasks))-1L)%%p$shards==shard,,drop=FALSE]
  stopifnot(nrow(tasks)==6L)
  workers <- as.integer(Sys.getenv('LKT_STUDY_WORKERS','1'));stopifnot(workers>=1,workers<=6)
  cluster <- parallel::makePSOCKcluster(workers,outfile=file.path(output,paste0(partition,'-shard',shard,'-workers.log')))
  tryCatch({
    parallel::clusterCall(cluster,function(root,release_file) {
      assign('root',root,envir=.GlobalEnv);Sys.setenv(LKT_RELEASE_FILE=release_file)
      sys.source(file.path(root,'scripts/load-current-lkt.R'),envir=.GlobalEnv)
      sys.source(file.path(root,'scripts/gru-encoder-study.R'),envir=.GlobalEnv);NULL
    },root,attr(lkt_release,'file'))
    parallel::clusterExport(cluster,c('p','d','tasks','output'))
    parallel::parLapplyLB(cluster,seq_len(nrow(tasks)),function(i)
      study_fit_one(tasks[i,,drop=FALSE],p$settings[[tasks$candidate[i]]],output,p,d))
  },finally=parallel::stopCluster(cluster))
  scores <- predictions <- list()
  for(i in seq_len(nrow(tasks))) {
    task <- tasks[i,];fit <- readRDS(file.path(output,'models',paste0(task$run_id,'.rds')))$fit
    test <- d[d$fold==3,,drop=FALSE];replay <- study_replay(fit,test)
    stopifnot(max(abs(replay-fit$prediction[d$fold==3]))<3e-6)
    # Canonical saved scores keep AUC tie handling identical between summaries
    # and bootstrap inputs, after independent replay has checked their correctness.
    q <- fit$prediction[d$fold==3]
    scores[[i]] <- cbind(task,AUC=unname(study_metrics(test$CF..ansbin.,q)[['AUC']]))
    predictions[[i]] <- data.frame(partition=partition,condition=task$candidate,seed=task$seed,
      source_row=test$comparison_source_row,participant=test$Anon.Student.Id,observed=test$CF..ansbin.,prediction=q)
  }
  stem <- paste0(partition,'-shard',shard)
  study_write_csv(do.call(rbind,scores),file.path(output,'reports',paste0(stem,'-test.csv')))
  study_write_csv(do.call(rbind,predictions),file.path(output,'reports',paste0(stem,'-predictions.csv')))
  writeLines('PASS all six fresh fits and saved held-out history replays.',file.path(output,'reports',paste0(stem,'-verification.txt')))
}

if(action %in% c('report','check')) {
  source(file.path(root,'scripts/gru-training-grid-report.R'))
  result <- training_grid_report(output,p,tasks_for,action)
  comparison <- as.data.frame(result$summary)
  comparison$AUC_difference_from_010 <- comparison$CV_AUC-comparison$CV_AUC[comparison$condition=='baseline']
  if(action=='report') {
    study_write_csv(comparison,file.path(output,'reports/head-dropout-comparison.csv'))
    lines <- c('# Cloze prediction-head dropout comparison','',
      '60 fresh fits: four rates, the same five participant folds, and seeds 1-3.',
      'Only prediction-head dropout changes; input and recurrent dropout remain 0.10.',
      'AUC pools held-out predictions over five folds within each seed, then averages seeds.',
      'Fixed-rate results are exploratory; held-out scores do not change defaults.','',
      '| Head dropout | Training AUC | Validation AUC | Held-out CV AUC | Difference from 0.10 |',
      '|---:|---:|---:|---:|---:|',
      vapply(seq_len(nrow(comparison)),function(i)sprintf('| %.2f | %.6f | %.6f | %.6f | %+.6f |',
        comparison$head_dropout[i],comparison$training_AUC[i],comparison$validation_AUC[i],
        comparison$CV_AUC[i],comparison$AUC_difference_from_010[i]),character(1)),
      '', 'All checkpoints passed contract and saved-weight replay checks.',
      'All 15 fresh 0.10 controls reproduced the earlier baseline within numerical tolerance.',
      'Validation-only selection is also recorded as a secondary analysis in primary-auc.csv.')
    writeLines(lines,file.path(output,'reports/RESULTS.md'))
  } else stopifnot(isTRUE(all.equal(as.data.frame(fread(file.path(output,'reports/head-dropout-comparison.csv'))),comparison,
    tolerance=1e-12,check.attributes=FALSE)))
}
