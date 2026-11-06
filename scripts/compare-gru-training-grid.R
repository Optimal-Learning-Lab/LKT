# Cross four training settings while keeping the complete base architecture fixed.
# Args: ROOT OUTPUT plan|verify|fit|report|check PARTITION SHARD [REFERENCE_RESULTS]
args <- commandArgs(TRUE);stopifnot(length(args) %in% c(5L,6L))
root <- normalizePath(args[1],winslash='/',mustWork=TRUE)
output <- args[2];action <- args[3];partition <- args[4];shard <- as.integer(args[5])
stopifnot(action %in% c('plan','verify','fit','report','check'))
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-encoder-study.R'))
suppressPackageStartupMessages(library(data.table))
script_names <- c('scripts/compare-gru-training-grid.R','scripts/gru-training-grid-report.R','scripts/gru-encoder-study.R',
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
    identical(old$seeds,1:3))
  base <- do.call(GRUModel,old$settings$baseline)$settings
  stopifnot(base$num_layers==3L,base$hidden_size==32L,identical(base$head_hidden_sizes,128L),
    base$learning_rate==.001,base$input_dropout==.1,base$gru_dropout==.1,base$head_dropout==.1,
    base$batch_size==32L,base$weight_decay==.0001,base$feature_encoder=='mlp',base$inference_mode=='none',
    base$retrieval_heads==1L,base$retrieval_window==100L,base$retrieval_decay=='none',
    identical(base$zero_init_embedding_cols,'KC..Default.'),
    identical(base$retrieval_filter_cols,c('KC..Default.','KC..Cluster.')),
    base$numeric_fusion=='concat',base$categorical_fusion=='concat',base$input_fusion=='concat')
  # These are the sixteen requested combinations, including the original control.
  # A single dropout factor changes the three existing nonzero dropout sites together.
  grid <- expand.grid(learning_rate=c(.001,.0001),dropout=c(.1,.05),
    batch_size=c(32L,64L),weight_decay=c(.0001,.00001),KEEP.OUT.ATTRS=FALSE)
  grid$condition <- c('baseline',sprintf('g%02d',2:16))
  settings <- setNames(lapply(seq_len(nrow(grid)),function(i) {
    s <- base;s$learning_rate <- grid$learning_rate[i];s$batch_size <- grid$batch_size[i]
    s$weight_decay <- grid$weight_decay[i]
    for(n in c('input_dropout','gru_dropout','head_dropout'))s[[n]] <- grid$dropout[i]
    do.call(GRUModel,s)$settings
  }),grid$condition)
  varied <- c('learning_rate','batch_size','weight_decay','input_dropout','gru_dropout','head_dropout')
  for(s in settings)stopifnot(identical(s[setdiff(names(s),varied)],base[setdiff(names(base),varied)]))
  stopifnot(identical(settings$baseline,base),nrow(unique(grid))==16L)
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
    control_sha256=study_hash(file.path(output,'data/reference-controls.rds')),
    partitions=parts,partition_hashes=old$partition_hashes[parts],seeds=1:3,shards=8L,total_fits=240L,
    settings=settings,grid=grid,baseline=old$baseline,parameter_count=46257L,
    design='16 training-setting combinations x five existing participant folds x three fresh seeds; 240 fits. No development-split fits.',
    selection='Within each outer fold, choose the setting with highest mean participant-validation AUC over seeds 1:3; exact ties choose earlier grid order. Then score its held-out test students.',
    checkpoints='Unchanged base stopping and restoration criterion, max 100 epochs and patience 10.',
    caveats=c('The dataset and architecture were previously inspected; this nests only the current grid selection within the existing participant folds.',
      'All sixteen fixed-configuration CV AUCs are descriptive. The primary tuned-procedure AUC uses validation-only selection separately within each outer fold.',
      'Input, between-GRU-layer and prediction-head dropout move together; encoder and retrieval dropout remain at zero.',
      'Smaller learning rates and larger batches have the same epoch budget, which may limit convergence. Cap counts are reported without changing the budget post hoc.',
      'Six nonlinear history-feature constants remain inherited fixed values. No history-feature search, new architecture or input relabeling is performed.',
      'AUC is computed per fitted seed and then averaged; predictions are not ensembled.'))
  study_write_rds(p,file.path(output,'study.rds'))
  study_write_csv(grid,file.path(output,'grid.csv'))
  writeLines(capture.output(dput(p)),file.path(output,'study.txt'))
  cat('FROZEN 16 configurations, five participant folds, three seeds: 240 fresh fits.\n')
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
  stopifnot(length(tasks_for('cv1')$seed)==48L)
  cat('PASS released runtime, exact data/folds, sixteen settings, identical seed-matched initial weights and 46,257 parameters.\n')
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
  training_grid_report(output,p,tasks_for,action)
}
