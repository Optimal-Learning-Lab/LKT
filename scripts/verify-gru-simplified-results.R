# Numeric audit only: no package loading, prediction replay or model fitting.
# ROOT COMPLETED_RESULTS OUTPUT_DIRECTORY [PREFLIGHT_STUDY_RDS [FROZEN_STUDY_ROOT]]
args <- commandArgs(TRUE);stopifnot(length(args) %in% 3:5)
root <- normalizePath(args[1],winslash='/',mustWork=TRUE)
results <- normalizePath(args[2],winslash='/',mustWork=TRUE)
output <- args[3];dir.create(output,recursive=TRUE,showWarnings=FALSE)
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
p <- readRDS(file.path(results,'study.rds'));d <- readRDS(file.path(results,'data/prepared_data.rds'))
hash <- function(f)digest::digest(file=f,algo='sha256')
stopifnot(hash(file.path(results,'data/prepared_data.rds'))==p$data_sha256,
  p$total_fits==30L,identical(p$seeds,1:3),identical(p$partitions,paste0('cv',1:5)))
frozen_root <- if(length(args)==5L)normalizePath(args[5],winslash='/',mustWork=TRUE) else root
for(name in names(p$script_hashes))stopifnot(hash(file.path(frozen_root,name))==p$script_hashes[[name]])
if(length(args)>=4L) {
  prior <- readRDS(args[4])
  for(name in c('settings','baseline','intercepts','partitions','partition_hashes','seeds','data_sha256','reference_protocol_sha256'))
    stopifnot(identical(p[[name]],prior[[name]]))
}
records <- checks <- list();pooled <- list();controls <- readRDS(file.path(results,'data/reference-controls.rds'))
auc <- function(y,q)as.numeric(pROC::auc(pROC::roc(y,q,levels=c(0,1),direction='<',quiet=TRUE)))
for(part in p$partitions) {
  fold_file <- file.path(results,'partitions',paste0(part,'.rds'))
  stopifnot(hash(fold_file)==p$partition_hashes[[part]])
  roles <- readRDS(fold_file)
  stopifnot(!anyDuplicated(unique(data.frame(id=d$Anon.Student.Id,role=roles))$id))
  for(condition in names(p$settings))for(seed in p$seeds) {
    id <- paste(part,condition,paste0('seed',seed),sep='__')
    saved <- readRDS(file.path(results,'models',paste0(id,'.rds')));f <- saved$fit
    s <- p$settings[[condition]];s$seed <- as.integer(seed)
    stopifnot(identical(f$gru_settings,s),identical(f$model$recipe$interc,unname(p$intercepts[condition])),
      identical(f$fit_rows,which(roles==1)),identical(f$validation_rows,which(roles==2)),
      identical(f$evaluation_rows,which(roles==3)),f$parameter_count==p$parameter_counts[[condition]])
    phase_auc <- vapply(1:3,function(k)auc(d$CF..ansbin.[roles==k],f$prediction[roles==k]),numeric(1))
    history <- f$training_history;selected <- match(f$selected_epoch,history$epoch)
    stopifnot(!is.na(selected),abs(phase_auc[1]-history$training_auc[selected])<1e-12,
      abs(history$validation_loss[selected]-min(history$validation_loss,na.rm=TRUE))<1e-12)
    records[[id]] <- data.frame(partition=part,condition=condition,seed=seed,
      training_AUC=phase_auc[1],validation_AUC=phase_auc[2],fold_test_AUC=phase_auc[3],selected_epoch=f$selected_epoch)
    if(condition=='baseline') {
      delta <- max(abs(f$prediction-controls[[paste(part,seed)]]$prediction))
      stopifnot(delta<3e-6);checks[[id]] <- delta
    }
    key <- paste(condition,seed)
    pooled[[key]] <- rbind(pooled[[key]],data.frame(source_row=d$comparison_source_row[roles==3],
      participant=d$Anon.Student.Id[roles==3],observed=d$CF..ansbin.[roles==3],prediction=f$prediction[roles==3]))
  }
}
r <- do.call(rbind,records);by_seed <- list();summary <- list()
for(condition in names(p$settings))for(seed in p$seeds) {
  q <- pooled[[paste(condition,seed)]]
  stopifnot(nrow(q)==nrow(d),!anyDuplicated(q$source_row),setequal(q$source_row,d$comparison_source_row))
  z <- r[r$condition==condition & r$seed==seed,]
  by_seed[[paste(condition,seed)]] <- data.frame(condition=condition,seed=seed,
    training_AUC=mean(z$training_AUC),validation_AUC=mean(z$validation_AUC),heldout_AUC=auc(q$observed,q$prediction))
}
by_seed <- do.call(rbind,by_seed)
for(condition in names(p$settings)) {
  z <- by_seed[by_seed$condition==condition,]
  summary[[condition]] <- data.frame(condition=condition,training_AUC=mean(z$training_AUC),
    validation_AUC=mean(z$validation_AUC),heldout_AUC=mean(z$heldout_AUC))
}
summary <- do.call(rbind,summary);rownames(summary) <- NULL
reported <- read.csv(file.path(results,'reports/summary.csv'))
reported <- reported[match(summary$condition,reported$condition),names(summary)]
stopifnot(isTRUE(all.equal(summary,reported,tolerance=1e-12,check.attributes=FALSE)))
write.csv(r,file.path(output,'independent-phase-auc.csv'),row.names=FALSE)
write.csv(by_seed,file.path(output,'independent-auc-by-seed.csv'),row.names=FALSE)
write.csv(summary,file.path(output,'independent-summary.csv'),row.names=FALSE)
jsonlite::write_json(list(checkpoints=30L,independent_phase_AUCs=90L,
  independent_pooled_AUCs=6L,baseline_controls=15L,maximum_control_probability_difference=max(unlist(checks)),
  all_selected_epochs_minimize_validation_loss=TRUE,phase_and_summary_tolerance=1e-12,
  study_sha256=hash(file.path(results,'study.rds')),audit_source_sha256=hash(file.path(root,'scripts/verify-gru-simplified-results.R'))),
  file.path(output,'verification.json'),auto_unbox=TRUE,pretty=TRUE)
print(summary)
cat('PASS independent pROC audit of 90 phase AUCs, six pooled AUCs, restored epochs, exact source folds and 15 fresh controls.\n')
