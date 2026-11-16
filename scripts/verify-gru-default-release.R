# Read-only saved-model replay for a default promotion. ROOT LIBRARY STUDY OUTPUT
args <- commandArgs(TRUE);stopifnot(length(args)==4L)
root <- normalizePath(args[1],winslash='/',mustWork=TRUE)
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
library(LKT,lib.loc=normalizePath(args[2],winslash='/',mustWork=TRUE))
study <- normalizePath(args[3],winslash='/',mustWork=TRUE);output <- args[4]
dir.create(output,recursive=TRUE,showWarnings=FALSE)
source(file.path(root,'scripts/gru-encoder-study.R'))
s <- GRUModel()$settings
stopifnot(s$numeric_encoder=='combined',s$response_encoding=='binary',
  s$retrieval_value_mode=='raw',identical(s$encoder_output_sizes,c(numeric=32L)))
p <- readRDS(file.path(study,'study.rds'));d <- readRDS(file.path(study,'data/prepared_data.rds'))
records <- list()
for(part in p$partitions) {
  fold <- readRDS(file.path(study,'partitions',paste0(part,'.rds')))
  test <- d[fold==3,,drop=FALSE]
  for(condition in names(p$settings))for(seed in p$seeds) {
    id <- paste(part,condition,paste0('seed',seed),sep='__')
    file <- file.path(study,'models',paste0(id,'.rds'));before <- study_hash(file)
    f <- readRDS(file)$fit;q <- study_replay(f,test)
    difference <- max(abs(q-f$prediction[fold==3]))
    stopifnot(difference<3e-6,before==study_hash(file))
    records[[id]] <- data.frame(run_id=id,maximum_probability_difference=difference)
  }
}
write.csv(do.call(rbind,records),file.path(output,'checkpoint-replay.csv'),row.names=FALSE)
# Original pre-selector format: a baseline checkpoint from the preceding grid.
file <- file.path(dirname(dirname(dirname(study))),'gru-training-grid-20261105/download/results/models/cv1__baseline__seed1.rds')
stopifnot(file.exists(file));before <- study_hash(file);old <- readRDS(file)$fit
fold <- readRDS(file.path(study,'partitions/cv1.rds'));test <- d[fold==3,,drop=FALSE]
stopifnot(max(abs(study_replay(old,test)-old$prediction[fold==3]))<3e-6,before==study_hash(file))
jsonlite::write_json(list(replayed_checkpoints=length(records),
  previous_and_simplified=TRUE,original_pre_selector_checkpoint=TRUE,
  maximum_probability_difference=max(vapply(records,function(x)x$maximum_probability_difference,numeric(1))),
  all_saved_files_unchanged=TRUE),file.path(output,'verification.json'),auto_unbox=TRUE,pretty=TRUE)
cat('PASS',length(records),'real Cloze checkpoint replays plus original pre-selector checkpoint; all files unchanged.\n')
