# ROOT MODEL_RDS DATA_RDS OUTPUT -- transient numeric fixtures, no fitting.
a <- commandArgs(TRUE);stopifnot(length(a)==4L,!file.exists(a[4]))
root <- a[1];source(file.path(root,'scripts/load-current-lkt.R'))
base <- readRDS(a[2])$fit;original <- serialize(base,NULL)
data <- readRDS(a[3]);student <- data[data$fold==3L,,drop=FALSE]
student <- student[student$Anon.Student.Id==student$Anon.Student.Id[1L],,drop=FALSE]
history <- student[1:25,,drop=FALSE]
rows <- student[26:35,setdiff(names(student),c('gru_item_age_seconds','gru_mean_prior_spacing_seconds')),drop=FALSE]
rows$CF..Time. <- max(history$CF..Time.)+60;rows$CF..ansbin. <- NA_real_
rows <- LKTGRUCandidateTimeFeatures(history,rows)
cases <- data.frame(heads=c(1L,2L,2L,2L),value=c('raw','projected','projected','projected'),
  decay=c('none','exponential','akt','akt'),share=c(FALSE,TRUE,FALSE,TRUE),
  fusion=c('concat','concat','replace','concat'),filter=c(TRUE,TRUE,TRUE,FALSE))
differences <- numeric(nrow(cases))
for(i in seq_len(nrow(cases))) {
  settings <- getFromNamespace('lkt_sequence_model','LKT')(base)$settings
  settings$retrieval_heads <- cases$heads[i];settings$retrieval_value_mode <- cases$value[i]
  settings$retrieval_decay <- cases$decay[i];settings$retrieval_share_qk <- cases$share[i]
  settings$retrieval_fusion <- cases$fusion[i];settings$retrieval_window <- 4L
  if(!cases$filter[i])settings$retrieval_filter_cols <- character()
  torch::torch_manual_seed(71L+i)
  net <- getFromNamespace('lkt_gru_network','LKT')(base$model$preprocessing,settings)
  probe <- base;probe$model$settings <- settings;probe$gru_settings <- settings
  probe$model$weights <- getFromNamespace('lkt_gru_weights','LKT')(net)
  probe$model$weights[['plasticity.eta']][] <- .03
  probe$model$id <- digest::digest(list(settings,probe$model$weights),algo='sha256')
  run <- LKTGRURuntime(probe);state <- LKTGRUStep(probe,LKTGRUState(probe),history,run)$state
  bytes <- serialize(state,NULL);weights <- serialize(probe$model$weights,NULL)
  ref <- LKTGRUScore(probe,state,rows,run)
  for(chunk in c(1L,3L,256L)) {
    p <- LKTGRUScore(probe,state,rows,run,'batched',chunk)
    differences[i] <- max(differences[i],max(abs(ref-p)))
  }
  stopifnot(differences[i]<3e-6,identical(bytes,serialize(state,NULL)),
    identical(weights,serialize(probe$model$weights,NULL)))
  cat('PASS retrieval fixture',i,'maximum difference',differences[i],'\n')
}
stopifnot(identical(original,serialize(base,NULL)))
jsonlite::write_json(list(passed=TRUE,fixtures=cases,maximum_differences=differences,
  description='Transient randomly initialized numeric fixtures; no model training or AUC evaluation.',
  original_fitted_artifact_unchanged=TRUE),a[4],auto_unbox=TRUE,pretty=TRUE)
