# Audit a completed, frozen experiment using the maintained report implementation.
# Args: CURRENT_ROOT FROZEN_BUNDLE RESULTS NEW_REPORT_DIRECTORY
# The original training scripts and protocol keep their hashes and are not edited
# when a reporting error is corrected after training.
args <- commandArgs(TRUE);stopifnot(length(args)==4L)
root <- normalizePath(args[1],winslash='/',mustWork=TRUE)
bundle <- normalizePath(args[2],winslash='/',mustWork=TRUE)
output <- normalizePath(args[3],winslash='/',mustWork=TRUE)
report_directory <- args[4];stopifnot(!dir.exists(report_directory))
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(bundle,'scripts/gru-encoder-study.R'))
suppressPackageStartupMessages(library(data.table))
p <- readRDS(file.path(output,'study.rds'))
stopifnot(identical(p$release,lkt_release[c('lkt_version','lkt_source_archive','lkt_source_sha256','lkt_manifest_sha256')]),
  study_hash(attr(lkt_release,'file'))==p$release_file_sha256,
  study_hash(attr(lkt_release,'archive'))==p$release[['lkt_source_sha256']],
  study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256,
  study_hash(file.path(output,'data/reference-controls.rds'))==p$control_sha256)
for(name in names(p$script_hashes))stopifnot(study_hash(file.path(bundle,name))==p$script_hashes[[name]])
for(part in p$partitions)stopifnot(study_hash(file.path(output,'partitions',paste0(part,'.rds')))==p$partition_hashes[[part]])
tasks_for <- function(part)do.call(rbind,lapply(names(p$settings),function(condition)do.call(rbind,lapply(p$seeds,function(seed)
  data.frame(run_id=paste(part,condition,paste0('seed',seed),sep='__'),partition=part,
    family='augmented',stage=condition,candidate=condition,seed=seed)))))
source(file.path(root,'scripts/gru-training-grid-report.R'))
training_grid_report(output,p,tasks_for,report_directory=report_directory)
training_grid_report(output,p,tasks_for,action='check',report_directory=report_directory)
scripts <- c('scripts/audit-gru-training-grid.R','scripts/gru-training-grid-report.R')
write.csv(data.frame(script=scripts,sha256=vapply(file.path(root,scripts),study_hash,character(1))),
  file.path(report_directory,'analysis-source-checksums.csv'),row.names=FALSE)
writeLines(c('PASS original frozen training protocol and scripts unchanged.',
  'PASS corrected standalone reporting and independent report recomputation.',
  paste('Original protocol SHA256:',study_hash(file.path(output,'study.rds')))),
  file.path(report_directory,'analysis-verification.txt'))
