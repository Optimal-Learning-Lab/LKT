# Compare prediction-head widths while preserving a completed experiment's data,
# participant split and training settings. Arguments: root, output, plan/run/smoke,
# workers, and the completed inference experiment used as the reference.
args <- commandArgs(TRUE)
stopifnot(length(args)==5L)
root <- normalizePath(args[1L],winslash='/',mustWork=TRUE)
output <- args[2L];action <- args[3L];workers <- as.integer(args[4L])
reference <- normalizePath(args[5L],winslash='/',mustWork=TRUE)
stopifnot(action %in% c('plan','run','smoke'),length(workers)==1L,!is.na(workers),workers>=1L)
source(file.path(root,'scripts/load-current-lkt.R'))
private <- lkt_release[['lkt_library']]
suppressPackageStartupMessages(library(data.table))
stopifnot(as.character(packageVersion('torch'))=='0.17.0',torch::torch_is_installed())
source(file.path(root,'scripts/gru-grid-utils.R'))

# Reuse the exact completed data preparation rather than reconstructing a similar
# split. Every model is newly trained; previous checkpoints are never reused.
reference_file <- file.path(reference,'inference_protocol.rds')
previous <- readRDS(reference_file)
stopifnot(previous$schema==1L,!previous$smoke,file.exists(file.path(reference,'verification.txt')),
  identical(digest::digest(file.path(reference,'prepared_data.rds'),algo='sha256',file=TRUE),previous$data_sha256),
  identical(digest::digest(file.path(reference,'largerawsample_source.rds'),algo='sha256',file=TRUE),previous$input_sha256))
for(f in names(previous$source_hashes))stopifnot(identical(
  digest::digest(file.path(reference,'source',f),algo='sha256',file=TRUE),previous$source_hashes[[f]]))
for(f in names(previous$source_hashes)[startsWith(names(previous$source_hashes),'R/')])
  stopifnot(identical(digest::digest(file.path(root,f),algo='sha256',file=TRUE),previous$source_hashes[[f]]))
d <- as.data.table(readRDS(file.path(reference,'prepared_data.rds')))
if(action=='smoke')d <- d[,head(.SD,12L),by=Anon.Student.Id]
seeds <- 1:3;widths <- c(32L,64L,128L,256L)
if(action=='smoke')seeds <- 1L
families <- c('matched','raw','augmented')

# Match the original baseline exactly except for the declared head width and seed.
# Keeping one hidden layer isolates width from changes in recurrent or head depth.
tasks <- settings <- list()
for(family in families)for(width in widths)for(seed in seeds) {
  original_id <- paste(family,'baseline','seed1',sep='__')
  s <- previous$settings[[original_id]]
  stopifnot(!is.null(s),s$inference_mode=='none',s$num_layers==3L,s$hidden_size==32L,
    identical(s$head_hidden_sizes,32L))
  s$head_hidden_sizes <- width;s$seed <- seed
  if(action=='smoke') {s$epochs <- 1L;s$early_stopping <- FALSE;s$restore_best <- FALSE}
  id <- paste(family,paste0('head',width),paste0('seed',seed),sep='__')
  settings[[id]] <- s
  tasks[[id]] <- data.frame(run_id=id,family=family,head_width=width,seed=seed,
    settings_sha256=digest::digest(s,algo='sha256'))
}
runs <- do.call(rbind,tasks);rownames(runs) <- NULL
dir.create(output,recursive=TRUE,showWarnings=FALSE)
output <- normalizePath(output,winslash='/',mustWork=TRUE)
if(length(list.files(output,all.files=TRUE,no..=TRUE)) &&
   !file.exists(file.path(output,'head_width_protocol.rds')))stop('Use a new output directory.')
for(folder in c('models','validation','training','source'))dir.create(file.path(output,folder),showWarnings=FALSE)
source_files <- unique(c(names(previous$source_hashes)[startsWith(names(previous$source_hashes),'R/')],
  'scripts/gru-grid-utils.R','scripts/compare-gru-head-widths.R','scripts/summarize-gru-head-widths.R'))
hashes <- setNames(vapply(source_files,function(f)digest::digest(file.path(root,f),algo='sha256',file=TRUE),character(1)),source_files)
data_file <- file.path(output,'prepared_data.rds')
if(file.exists(data_file))stopifnot(identical(readRDS(data_file),as.data.frame(d))) else
  gru_grid_atomic_rds(as.data.frame(d),data_file)
protocol <- list(schema=1L,type='GRU head-width search',smoke=action=='smoke',widths=widths,seeds=seeds,
  runs=runs,settings=settings,baseline=previous$baseline,source_hashes=hashes,
  reference_protocol_sha256=digest::digest(reference_file,algo='sha256',file=TRUE),
  reference_data_sha256=previous$data_sha256,input_sha256=previous$input_sha256,
  data_sha256=digest::digest(data_file,algo='sha256',file=TRUE),
  software=list(R=R.version.string,LKT=as.character(packageVersion('LKT')),torch=as.character(packageVersion('torch'))),
  selection='For each family, minimize mean validation log loss over the three seeds; exact ties choose the smaller width. Freeze choices before test scoring.',
  test_reporting='Report the selected width and width-32 baseline; no test-based width selection. Metrics average single-model scores over seeds, not ensemble probabilities.',
  split=previous$split,caveats=previous$caveats)
protocol_file <- file.path(output,'head_width_protocol.rds')
if(file.exists(protocol_file))stopifnot(identical(readRDS(protocol_file),protocol)) else {
  gru_grid_atomic_rds(protocol,protocol_file)
  writeLines(capture.output(dput(protocol)),file.path(output,'head_width_protocol.txt'))
  write.csv(runs,file.path(output,'head_width_runs.csv'),row.names=FALSE)
  for(f in source_files) {
    destination <- file.path(output,'source',f);dir.create(dirname(destination),recursive=TRUE,showWarnings=FALSE)
    stopifnot(file.copy(file.path(root,f),destination,overwrite=FALSE))
  }
  # Keep the identity/filtering evidence with this run so the downloaded result is
  # understandable without relying on the previous workspace remaining available.
  audit_files <- c('inference_protocol.rds','participant_split.csv','split_counts.csv','largerawsample_source.rds',
    'response_inclusion.csv','filtering_stages.csv','threshold_removed_rows.csv','item_skill_mapping.csv',
    'item_participant_counts.csv','participant_item_counts.csv')
  dir.create(file.path(output,'reference'),showWarnings=FALSE)
  for(f in audit_files)
    stopifnot(file.copy(file.path(reference,f),file.path(output,'reference',f),overwrite=FALSE))
}
protocol_sha <- digest::digest(protocol_file,algo='sha256',file=TRUE)
cat('FROZEN:',nrow(runs),'fresh fits; widths',widths,'; seeds',seeds,'\n');flush.console()
if(action=='plan')quit(save='no',status=0)

# Workers write validation scores only. A saved checkpoint must match both the
# frozen protocol and requested settings before a resumed task may accept it.
run_candidate <- function(run_id) {
  task <- protocol$runs[match(run_id,protocol$runs$run_id),];s <- protocol$settings[[run_id]]
  path <- file.path(output,'models',paste0(run_id,'.rds'))
  if(file.exists(path)) {
    saved <- readRDS(path)
    stopifnot(identical(saved$protocol_sha256,protocol_sha),identical(saved$run_id,run_id))
  } else {
    cat('START',run_id,'\n');flush.console()
    elapsed <- system.time(fit <- LKT(copy(d),usefolds=1,interc=TRUE,
      components=protocol$baseline$components,features=protocol$baseline$features,
      fixedpars=protocol$baseline$fixedpars,model=do.call(GRUModel,s),verbose=FALSE))[['elapsed']]
    gru_grid_validate_fit(fit,s,d)
    # Remove duplicated raw data and the unused null model, retaining fitted weights,
    # preprocessing, predictions, training history and all row-partition evidence.
    fit$newdata <- NULL;fit$nullmodel <- NULL
    saved <- list(protocol_sha256=protocol_sha,run_id=run_id,elapsed=elapsed,fit=fit)
    gru_grid_atomic_rds(saved,path)
  }
  fit <- saved$fit;gru_grid_validate_fit(fit,s,d)
  score <- cbind(task,as.data.frame(as.list(gru_grid_metrics(d$CF..ansbin.[d$fold==2L],fit$prediction[d$fold==2L]))),
    selected_epoch=fit$selected_epoch,epochs_run=nrow(fit$training_history),
    hit_epoch_cap=nrow(fit$training_history)==s$epochs,parameter_count=fit$parameter_count,elapsed=saved$elapsed)
  write.csv(fit$training_history,file.path(output,'training',paste0(run_id,'.csv')),row.names=FALSE)
  gru_grid_atomic_csv(score,file.path(output,'validation',paste0(run_id,'.csv')))
  cat('DONE',run_id,'validation log loss',score$log_loss,'\n');flush.console();NULL
}
if(workers>1L) {
  cluster <- parallel::makePSOCKcluster(workers,outfile=file.path(output,'workers.log'))
  parallel::clusterCall(cluster,function(paths) {.libPaths(paths);suppressPackageStartupMessages({library(LKT);library(data.table)});NULL},.libPaths())
  parallel::clusterExport(cluster,c('protocol','protocol_sha','d','output','run_candidate',
    'gru_grid_validate_fit','gru_grid_atomic_rds','gru_grid_atomic_csv','gru_grid_metrics'),envir=.GlobalEnv)
  invisible(tryCatch(parallel::parLapplyLB(cluster,runs$run_id,run_candidate),finally=parallel::stopCluster(cluster)))
} else invisible(lapply(runs$run_id,run_candidate))
source(file.path(root,'scripts/summarize-gru-head-widths.R'))
summarize_gru_head_widths(output)
