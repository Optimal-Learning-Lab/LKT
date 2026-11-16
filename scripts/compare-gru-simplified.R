# Matched Cloze comparison of the previous base and the simplified alternative.
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
source(file.path(root,'scripts/gru-simplified-alternative.R'))
source(file.path(root,'scripts/report-gru-simplified.R'))
suppressPackageStartupMessages(library(data.table))
script_names <- c('scripts/compare-gru-simplified.R','scripts/report-gru-simplified.R',
  'scripts/gru-encoder-study.R','scripts/gru-simplified-alternative.R')
hashes <- setNames(vapply(file.path(root,script_names),study_hash,character(1)),script_names)
conditions <- c('baseline','simplified')
tasks_for <- function(part)do.call(rbind,lapply(conditions,function(condition)do.call(rbind,lapply(1:3,function(seed)
  data.frame(run_id=paste(part,condition,paste0('seed',seed),sep='__'),partition=part,
    family='augmented',stage=condition,candidate=condition,seed=seed)))))

if(action=='plan') {
  stopifnot(length(args)==7L,!dir.exists(output))
  reference <- args[7];old <- readRDS(file.path(reference,'study.rds'))
  d <- readRDS(file.path(reference,'data/prepared_data.rds'))
  stopifnot(study_hash(file.path(reference,'data/prepared_data.rds'))==old$data_sha256,
    nrow(d)==55122L,length(unique(d$Anon.Student.Id))==478L,identical(old$seeds,1:3))
  base <- do.call(GRUModel,old$settings$baseline)$settings
  stopifnot(base$num_layers==3L,base$hidden_size==32L,identical(base$head_hidden_sizes,128L),
    base$learning_rate==.001,base$weight_decay==.0001,base$batch_size==32L,
    base$epochs==100L,base$patience==10L,base$min_delta==.0001,
    base$sequence_length==100L,base$optimizer=='adamw',base$validation_folds==2,
    base$restore_best,base$early_stopping,base$input_dropout==.1,
    base$gru_dropout==.1,base$head_dropout==.1,base$encoder_dropout==0,
    base$feature_encoder=='mlp',base$retrieval_heads==1L,base$retrieval_size==32L,
    base$retrieval_dropout==0,base$retrieval_window==100L,
    base$numeric_encoder=='separate',base$response_encoding=='mlp',
    base$retrieval_value_mode=='projected',identical(old$baseline$interc,TRUE))
  alt <- do.call(gru_simplified_alternative,base[setdiff(names(base),
    c('numeric_encoder','encoder_output_sizes','response_encoding','retrieval_value_mode'))])$settings
  varied <- c('numeric_encoder','encoder_output_sizes','response_encoding','retrieval_value_mode')
  stopifnot(identical(base[setdiff(names(base),varied)],alt[setdiff(names(alt),varied)]))
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
      f <- file.path(reference,'models',paste0(k,'__baseline__seed',seed,'.rds'))
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
    partitions=parts,partition_hashes=old$partition_hashes[parts],seeds=1:3,total_fits=30L,
    settings=list(baseline=base,simplified=alt),baseline=old$baseline,
    intercepts=c(baseline=TRUE,simplified=FALSE),
    parameter_counts=c(baseline=46257L,simplified=43105L),
    design='Two fresh conditions, five exact existing Cloze participant folds, seeds 1:3. Only the declared encoder/response/retrieval simplifications and global intercept removal differ.',
    checkpoints='Same 100 epochs, patience 10, min_delta 1e-4, validation-log-loss epoch restoration, optimizer and all training settings.',
    reporting='Training/validation AUC average the fifteen restored fits per condition. Held-out AUC pools five outer test folds within seed, then averages the three seed AUCs. No ensemble or outcome-based condition selection.',
    limits=c('Previously inspected Cloze data and architecture; exploratory fixed-architecture comparison.',
      'Six inherited nonlinear feature constants remain fixed, not reestimated within folds.',
      'No warm start, width search, training-setting search or active release promotion.',
      'The simplified network has 3152 fewer parameters; this is not a parameter-matched comparison.'))
  study_write_rds(p,file.path(output,'study.rds'))
  writeLines(capture.output(dput(p)),file.path(output,'study.txt'))
  cat('FROZEN: 30 fresh fits, exact existing folds, seeds and training settings.\n');quit(save='no')
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
    stopifnot(n==p$parameter_counts[[condition]],length(prep$numeric_names)==if(condition=='baseline')8L else 7L)
    if(condition=='simplified')stopifnot(!any(grepl('Intercept',prep$numeric_names,fixed=TRUE)),
      identical(as.integer(net$numeric_encoder[[1L]][[1L]]$weight$size()),c(32L,7L)))
    counts[[paste(k,condition)]] <- data.frame(partition=k,condition=condition,parameters=n,numeric_inputs=length(prep$numeric_names))
  }
  study_write_csv(do.call(rbind,counts),file.path(output,'reports/preflight.csv'))
  cat('PASS candidate identity, exact data/folds, seven versus eight inputs and all parameter counts.\n')
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
  cat('PASS both one-epoch smoke fits, intercept routing, finite weights and saved-weight replay.\n')
}
if(action=='fit') {
  stopifnot(part %in% p$partitions)
  d <- readRDS(file.path(output,'data/prepared_data.rds'))
  d$fold <- readRDS(file.path(output,'partitions',paste0(part,'.rds')));tasks <- tasks_for(part)
  workers <- as.integer(Sys.getenv('LKT_STUDY_WORKERS','1'));stopifnot(workers>=1L,workers<=6L)
  if(workers==1L)for(i in seq_len(nrow(tasks))) {
    t <- tasks[i,,drop=FALSE]
    study_fit_one(t,p$settings[[t$candidate]],output,p,d,intercept=unname(p$intercepts[t$candidate]))
  } else {
    cl <- parallel::makePSOCKcluster(workers,outfile=file.path(output,paste0(part,'-workers.log')))
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
  study_write_csv(do.call(rbind,records),file.path(output,'reports',paste0(part,'-predictions.csv')))
  writeLines('PASS all six checkpoint settings/masks and held-out saved-weight replays.',file.path(output,'reports',paste0(part,'-verification.txt')))
}
if(action %in% c('report','check'))simplified_report(output,p,tasks_for,action)
