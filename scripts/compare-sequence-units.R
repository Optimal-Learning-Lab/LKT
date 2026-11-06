# Frozen local experiment. Select the explicitly built candidate library; this
# never selects, installs, or promotes the central production/research release.
args <- commandArgs(TRUE)
stopifnot(length(args)%in%c(4L,5L))
action <- args[1L];root <- normalizePath(args[2L],winslash='/',mustWork=TRUE)
output <- args[3L];candidate <- normalizePath(args[4L],winslash='/',mustWork=TRUE)
stopifnot(action%in%c('plan','smoke','fit','report','verify'))
.libPaths(c(candidate,file.path(root,'.codex-r-lib'),.libPaths()))
library(LKT,lib.loc=candidate)
stopifnot(normalizePath(find.package('LKT'),winslash='/')==paste0(candidate,'/LKT'))
source(file.path(root,'scripts/gru-encoder-study.R'))
hash <- function(path)digest::digest(file=path,algo='sha256')
ns <- function(n)getFromNamespace(n,'LKT')
candidate_identity <- function()setNames(vapply(c('DESCRIPTION','R/LKT.rdb','R/LKT.rdx','research-source-manifest.json'),
  function(p)hash(file.path(candidate,'LKT',p)),character(1)),c('DESCRIPTION','R/LKT.rdb','R/LKT.rdx','manifest'))
identity <- candidate_identity()
write_rds <- function(value,path) {
  stopifnot(!file.exists(path));temp <- paste0(path,'.tmp');saveRDS(value,temp);stopifnot(file.rename(temp,path))
}
script_files <- c('scripts/compare-sequence-units.R','scripts/gru-encoder-study.R',
  'scripts/report-sequence-units.R','scripts/run-sequence-experiment.py')
script_identity <- setNames(vapply(script_files,function(p)hash(file.path(root,p)),character(1)),script_files)
if(action=='plan') {
  stopifnot(!dir.exists(output))
  reference <- file.path(root,'../LKT-outputs/gru-ktst-comparison-20261104/local-plan')
  previous <- readRDS(file.path(reference,'study.rds'))
  data_file <- file.path(reference,'data/prepared_data.rds')
  stopifnot(hash(data_file)==previous$data_sha256)
  d <- readRDS(data_file)
  stopifnot(nrow(d)==55122L,length(unique(d$Anon.Student.Id))==478L,
    length(unique(d$KC..Default.))==144L,length(unique(d$KC..Cluster.))==36L)
  for(folder in c('data','partitions','models','training','logs','reports','smoke'))dir.create(file.path(output,folder),recursive=TRUE)
  stopifnot(file.copy(data_file,file.path(output,'data/prepared_data.rds')))
  for(partition in previous$partitions) {
    path <- file.path(reference,'partitions',paste0(partition,'.rds'))
    stopifnot(hash(path)==previous$partition_hashes[[partition]],
      file.copy(path,file.path(output,'partitions',paste0(partition,'.rds'))))
  }
  base <- previous$settings$all_three
  settings <- list()
  for(recipe in c('direct','rich'))for(unit in c('gru','mamba')) {
    s <- base
    if(recipe=='direct') {
      s$feature_encoder <- 'none';s$retrieval_heads <- 0L;s$retrieval_filter_cols <- character()
      s$retrieval_cue_cols <- character();s$retrieval_decay <- 'none';s$retrieval_decay_lr_multiplier <- 1
      s$zero_init_embedding_cols <- character()
    }
    settings[[paste(recipe,unit,sep='_')]] <- ns('lkt_sequence_from_gru_settings')(s,unit)
  }
  tasks <- expand.grid(recipe=c('direct','rich'),unit=c('gru','mamba'),seed=1:3,
    partition=previous$partitions,stringsAsFactors=FALSE)
  tasks$id <- with(tasks,paste(partition,recipe,unit,paste0('seed',seed),sep='__'))
  protocol <- list(schema=1L,data_sha256=previous$data_sha256,partition_hashes=previous$partition_hashes,
    candidate=identity,driver=script_identity,reference=hash(file.path(reference,'study.rds')),
    settings=settings,recipe=previous$baseline,tasks=tasks,primary='participant-held-out pooled AUC',
    bootstrap_draws=2000L,bootstrap_seed=20261105L,initial_release=readLines(file.path(root,
      '../PolicyFactory2/workflows/simulation/shared/lkt-release.env')))
  write_rds(protocol,file.path(output,'protocol.rds'))
  write.csv(tasks,file.path(output,'tasks.csv'),row.names=FALSE)
  cat('PASS frozen protocol:',nrow(tasks),'fresh CPU fits\n');quit(save='no')
}
p <- readRDS(file.path(output,'protocol.rds'))
stopifnot(identical(identity,p$candidate),identical(script_identity,p$driver),
  hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256)
d <- readRDS(file.path(output,'data/prepared_data.rds'))
fit_task <- function(task,smoke=FALSE) {
  partition_path <- file.path(output,'partitions',paste0(task$partition,'.rds'))
  stopifnot(hash(partition_path)==p$partition_hashes[[task$partition]])
  data <- d;data$fold <- readRDS(partition_path)
  s <- p$settings[[paste(task$recipe,task$unit,sep='_')]];s$seed <- as.integer(task$seed)
  if(smoke) {s$epochs <- 1L;s$early_stopping <- FALSE}
  model <- do.call(SequenceModel,ns('lkt_sequence_arguments')(s))
  started <- proc.time()[['elapsed']]
  fit <- do.call(LKT,c(list(data=data,model=model,usefolds=1,verbose=TRUE),p$recipe))
  elapsed <- proc.time()[['elapsed']]-started
  stopifnot(all(is.finite(fit$prediction)),all(is.finite(fit$training_history$training_loss)))
  # Replay held-out histories directly from the saved network weights and frozen
  # schema, with no state inherited from training participants or prior fits.
  net <- ns('lkt_gru_network')(fit$model$preprocessing,s)
  ns('lkt_gru_load_weights')(net,fit$model$weights,s);net$eval()
  ord <- ns('lkt_gru_order')(data,s);ordered <- data[ord,,drop=FALSE]
  encoded <- ns('lkt_gru_encode')(ordered,fit$model$preprocessing,fit$model$recipe,s)
  replay <- ns('lkt_gru_replay')(net,encoded,ordered,s)[order(ord)]
  stopifnot(max(abs(replay-fit$prediction))<3e-6)
  # Fixed-size single-learner history for comparable candidate-scoring timing.
  subject <- as.character(data$Anon.Student.Id[which(data$fold==3)[1L]])
  history <- data[as.character(data$Anon.Student.Id)==subject,,drop=FALSE]
  count <- min(20L,nrow(history)-1L);history <- history[order(history[[s$order_col]]),,drop=FALSE]
  runtime <- LKTSequenceRuntime(fit)
  state <- LKTSequenceStep(fit,LKTSequenceState(fit),history[seq_len(count),],runtime)$state
  candidates <- history[rep(count+1L,10L),,drop=FALSE]
  before <- serialize(state,NULL)
  LKTSequenceScore(fit,state,candidates,runtime) # warm-up
  timing <- system.time(for(i in 1:3)LKTSequenceScore(fit,state,candidates,runtime))[['elapsed']]/30
  stopifnot(identical(before,serialize(state,NULL)))
  unit_state <- state$hidden
  list(fit=fit,task=task,fit_seconds=elapsed,score_seconds=timing,
    state_bytes=length(serialize(unit_state,NULL)),continuation_bytes=length(before),
    replay_max_error=max(abs(replay-fit$prediction)),candidate_identity=identity)
}
if(action=='smoke') {
  for(recipe in c('direct','rich'))for(unit in c('gru','mamba')) {
    task <- p$tasks[p$tasks$partition=='development' & p$tasks$recipe==recipe & p$tasks$unit==unit & p$tasks$seed==1,,drop=FALSE]
    file <- file.path(output,'smoke',paste0(recipe,'_',unit,'.rds'))
    if(!file.exists(file))write_rds(fit_task(task,TRUE),file)
    cat('PASS smoke',recipe,unit,'\n')
  }
  quit(save='no')
}
if(action=='fit') {
  stopifnot(length(args)==5L);task <- p$tasks[p$tasks$id==args[5L],,drop=FALSE];stopifnot(nrow(task)==1L)
  path <- file.path(output,'models',paste0(task$id,'.rds'));stopifnot(!file.exists(path))
  result <- fit_task(task);write_rds(result,path)
  write.csv(result$fit$training_history,file.path(output,'training',paste0(task$id,'.csv')),row.names=FALSE)
  cat('PASS fit and independent replay',task$id,'\n');quit(save='no')
}
paths <- file.path(output,'models',paste0(p$tasks$id,'.rds'))
stopifnot(all(file.exists(paths)))
results <- lapply(paths,readRDS)
for(i in seq_along(results))stopifnot(identical(results[[i]]$task,p$tasks[i,,drop=FALSE]),
  identical(results[[i]]$candidate_identity,p$candidate),results[[i]]$replay_max_error<3e-6)
if(action=='verify') {cat('PASS all 72 model identities and independent saved-weight replays\n');quit(save='no')}
source(file.path(root,'scripts/report-sequence-units.R'))
sequence_report(p,results,d,output)
