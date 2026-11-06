# Explicit release selection is required; this driver never searches for another LKT.
# Arguments: root, output, release JSON, action, partition, shard, shard count.
args <- commandArgs(TRUE);stopifnot(length(args)==7L)
root <- normalizePath(args[1L],winslash='/',mustWork=TRUE)
output <- args[2L];release_file <- args[3L];action <- args[4L]
partition <- args[5L];shard <- as.integer(args[6L]);shards <- as.integer(args[7L])
stopifnot(action %in% c('plan','smoke','fit','finalize','report'),!is.na(shard),!is.na(shards),shards>0L,shard>=0L,shard<shards)
source(file.path(root,'scripts/load-current-lkt.R'))
library <- lkt_release[['lkt_library']]
suppressPackageStartupMessages({library(data.table);library(jsonlite)})
scripts <- file.path(root,'scripts',c('gru-encoder-study.R','run-gru-encoder-study.R','report-gru-encoder-study.R'))
source(scripts[1L]);release <- study_require_release(release_file,library)
workers <- as.integer(Sys.getenv('LKT_STUDY_WORKERS','1'));stopifnot(workers>=1L)
if(action=='plan') {
  reference <- Sys.getenv('LKT_STUDY_REFERENCE');stopifnot(nzchar(reference),dir.exists(reference))
  study_plan(output,reference,release,scripts)
} else {
  p <- study_load(output,release,scripts)
  if(action=='fit')study_fit_grid(output,p,partition,shard,shards,workers,library)
  if(action=='finalize')study_finalize(output,p,partition,workers,library)
  if(action=='report') {source(scripts[3L]);study_report(output,p)}
  if(action=='smoke') {
    # Pipeline-only short histories use a separate protocol and output directory.
    # They never satisfy a primary research task or enter width/architecture selection.
    d <- as.data.table(study_data(output,p,'development'))[,head(.SD,12L),by=Anon.Student.Id]
    smoke <- file.path(output,'smoke');dir.create(smoke,showWarnings=FALSE)
    for(folder in c('models','validation','training'))dir.create(file.path(smoke,folder),showWarnings=FALSE)
    study_write_rds(list(type='SMOKE ONLY',parent=study_hash(file.path(output,'study.rds')),
      data=d,release=release),file.path(smoke,'study.rds'))
    choices <- as.data.table(p$candidates)[,head(.SD,1L),by=.(family,stage)]
    for(i in seq_len(nrow(choices))) {
      row <- choices[i,];s <- p$settings[[row$candidate]]
      s$epochs <- 1L;s$early_stopping <- FALSE;s$restore_best <- FALSE
      task <- data.frame(run_id=paste('smoke',row$family,row$stage,sep='__'),partition='smoke',
        family=row$family,stage=row$stage,candidate=row$candidate,seed=1L)
      study_fit_one(task,s,smoke,p,as.data.frame(d))
      fit <- readRDS(file.path(smoke,'models',paste0(task$run_id,'.rds')))$fit
      q <- study_replay(fit,as.data.frame(d[d$fold==3L]))
      stopifnot(max(abs(q-fit$prediction[d$fold==3L]))<3e-6)
    }
    writeLines('PASS: 12 architecture/family smoke fits and checkpoint test replay. Not research results.',file.path(smoke,'verification.txt'))
  }
}
