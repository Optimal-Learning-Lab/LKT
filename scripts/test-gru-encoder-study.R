# Small structural integration test, never a scientific benchmark. Replicate one
# short item history across synthetic participants to exercise the full orchestration.
args <- commandArgs(TRUE);stopifnot(length(args)==3L)
root <- normalizePath(args[1L],winslash='/');plan <- args[2L];output <- args[3L]
source(file.path(root,'scripts/load-current-lkt.R'))
lib <- lkt_release[['lkt_library']]
suppressPackageStartupMessages({library(LKT,lib.loc=lib);library(data.table)})
source(file.path(root,'scripts/gru-encoder-study.R'))
p <- readRDS(file.path(plan,'study.rds'))
original <- readRDS(file.path(plan,'data/prepared_data.rds'))
rows <- which(as.character(original$Anon.Student.Id)==as.character(original$Anon.Student.Id[1L]))[1:12]
d <- rbindlist(lapply(1:8,function(i) {
  x <- as.data.table(original[rows,,drop=FALSE]);x$Anon.Student.Id <- paste0('synthetic',i)
  x$fold <- c(1L,1L,1L,1L,1L,2L,3L,3L)[i]
  x$CF..ansbin. <- rep(c(0,1,1,0,1,0),2)
  x
}))
d$comparison_source_row <- seq_len(nrow(d))
dir.create(output,recursive=TRUE,showWarnings=FALSE)
for(folder in c('data','partitions','models','validation','training','selection','reports'))
  dir.create(file.path(output,folder),showWarnings=FALSE)
p$candidates <- as.data.frame(as.data.table(p$candidates)[,head(.SD,1L),by=.(family,stage)])
p$settings <- p$settings[p$candidates$candidate]
for(id in names(p$settings)) {
  p$settings[[id]]$epochs <- 1L;p$settings[[id]]$early_stopping <- FALSE;p$settings[[id]]$restore_best <- FALSE
}
study_write_rds(as.data.frame(d),file.path(output,'data/prepared_data.rds'))
study_write_rds(d$fold,file.path(output,'partitions/development.rds'))
p$type <- 'SYNTHETIC STRUCTURAL TEST ONLY';p$partitions <- 'development';p$total_fits <- 72L
p$partition_hashes <- c(development=study_hash(file.path(output,'partitions/development.rds')))
p$data_sha256 <- study_hash(file.path(output,'data/prepared_data.rds'))
study_write_rds(p,file.path(output,'study.rds'))
study_fit_grid(output,p,'development',0L,1L,2L,lib)
study_finalize(output,p,'development',2L,lib)
stopifnot(length(list.files(file.path(output,'models'),pattern='[.]rds$'))==72L,
  file.exists(file.path(output,'reports/development-verification.txt')))
source(file.path(root,'scripts/report-gru-encoder-study.R'));study_report(output,p)
cat('PASS: 72 synthetic pipeline fits; parallel work, all four architectures, all three input families, capacity and Gaussian followups, validation-only selection and test replay.\n')
