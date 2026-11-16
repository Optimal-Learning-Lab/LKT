# SOURCE_ROOT RESULTS OUTPUT. Canonical audit exports from saved fits, no replay
# or retraining. Preserves the frozen run's original exports and metrics.
a<-commandArgs(TRUE);stopifnot(length(a)==3L)
root<-normalizePath(a[1],winslash='/');out<-normalizePath(a[2],winslash='/');audit<-a[3]
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
source(file.path(root,'scripts/gru-encoder-study.R'))
p<-readRDS(file.path(out,'study.rds'));d<-readRDS(file.path(out,'data/prepared_data.rds'))
stopifnot(study_hash(file.path(out,'data/prepared_data.rds'))==p$data_sha256)
dir.create(audit,recursive=TRUE,showWarnings=FALSE)
for(i in seq_len(nrow(p$tasks))) {
  task<-p$tasks[i,,drop=FALSE]
  saved<-readRDS(file.path(out,'models',paste0(task$run_id,'.rds')))
  roles<-readRDS(file.path(out,'partitions',paste0(task$partition,'.rds')))
  fit<-saved$fit
  stopifnot(identical(saved$task,task),identical(fit$fit_rows,which(roles==1)),
    identical(fit$validation_rows,which(roles==2)),identical(fit$evaluation_rows,which(roles==3)))
  study_write_csv(data.frame(source_row=d$comparison_source_row,student=d$Anon.Student.Id,
    partition=task$partition,seed=task$seed,role=roles,observed=d$CF..ansbin.,prediction=fit$prediction),
    file.path(audit,paste0(task$run_id,'-predictions.csv')))
}
cat('PASS canonical stored-prediction exports; original frozen exports preserved\n')
