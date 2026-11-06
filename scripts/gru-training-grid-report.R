# One report implementation for new grids and audits of frozen completed grids.
# Reporting never changes fitted weights, protocols or the source prediction files.
training_grid_report <- function(output,p,tasks_for,action='report',
    report_directory=file.path(output,'reports')) {
  stopifnot(action %in% c('report','check'))
  dir.create(report_directory,recursive=TRUE,showWarnings=FALSE)

  # Audit the saved settings, masks, validation scores and baseline reproduction
  # before selecting any setting. Held-out outcomes never enter that selection.
  stopifnot(length(list.files(file.path(output,'models'),pattern='[.]rds$'))==240L)
  d <- readRDS(file.path(output,'data/prepared_data.rds'));records <- list();controls <- readRDS(file.path(output,'data/reference-controls.rds'))
  baseline_controls <- list();predictions <- list()
  for(part in p$partitions) {
    fold <- readRDS(file.path(output,'partitions',paste0(part,'.rds')))
    for(j in 0:(p$shards-1L)) {
      stem <- paste0(part,'-shard',j)
      stopifnot(file.exists(file.path(output,'reports',paste0(stem,'-verification.txt'))))
      predictions[[stem]] <- fread(file.path(output,'reports',paste0(stem,'-predictions.csv')))
    }
    tasks <- tasks_for(part)
    part_predictions <- rbindlist(predictions[paste0(part,'-shard',0:(p$shards-1L))])
    for(i in seq_len(nrow(tasks))) {
      t <- tasks[i,];saved <- readRDS(file.path(output,'models',paste0(t$run_id,'.rds')));f <- saved$fit
      s <- p$settings[[t$candidate]];s$seed <- as.integer(t$seed)
      stopifnot(identical(s,f$gru_settings),identical(saved$settings,s),
        identical(f$fit_rows,which(fold==1L)),identical(f$validation_rows,which(fold==2L)),
        identical(f$evaluation_rows,which(fold==3L)),f$parameter_count==p$parameter_count,
        identical(saved$fingerprint,digest::digest(list(study=study_hash(file.path(output,'study.rds')),task=saved$task,settings=s),algo='sha256')))
      v <- read.csv(file.path(output,'validation',paste0(t$run_id,'.csv')))
      vm <- study_metrics(d$CF..ansbin.[fold==2],f$prediction[fold==2])
      for(n in names(vm))stopifnot(abs(v[[n]]-vm[[n]])<1e-12)
      train_auc <- unname(study_metrics(d$CF..ansbin.[fold==1],f$prediction[fold==1])[['AUC']])
      stopifnot(abs(train_auc-f$training_history$training_auc[match(f$selected_epoch,f$training_history$epoch)])<1e-12)
      q <- part_predictions[condition==t$candidate & seed==t$seed]
      q <- q[match(d$comparison_source_row[fold==3L],source_row)]
      stopifnot(nrow(q)==sum(fold==3L),!anyNA(q),
        identical(q$source_row,d$comparison_source_row[fold==3L]),
        all(q$participant==d$Anon.Student.Id[fold==3L]),
        max(abs(q$prediction-f$prediction[fold==3L]))<1e-12)
      records[[t$run_id]] <- data.frame(partition=part,condition=t$candidate,seed=t$seed,
        training_AUC=train_auc,validation_AUC=unname(vm[['AUC']]),
        test_AUC=unname(study_metrics(d$CF..ansbin.[fold==3],f$prediction[fold==3])[['AUC']]),
        selected_epoch=f$selected_epoch,epochs_run=f$trained_epochs,cap_hit=f$trained_epochs==s$epochs)
      if(t$candidate=='baseline') {
        delta <- max(abs(f$prediction-controls[[paste(part,t$seed)]]$prediction));stopifnot(delta<3e-6)
        baseline_controls[[t$run_id]] <- data.frame(run_id=t$run_id,maximum_probability_difference=delta)
      }
    }
  }
  r <- rbindlist(records);pred <- rbindlist(predictions)
  stopifnot(!anyDuplicated(pred[,.(condition,seed,source_row)]),
    all(pred[,.N,by=.(condition,seed)]$N==nrow(d)),
    all(pred$observed==d$CF..ansbin.[match(pred$source_row,d$comparison_source_row)]))
  pooled <- pred[,.(AUC=unname(study_metrics(observed,prediction)[['AUC']])),by=.(condition,seed)]
  summary <- pooled[,.(CV_AUC=mean(AUC),seed_sd=sd(AUC)),by=condition]
  phase <- r[,.(training_AUC=mean(training_AUC),validation_AUC=mean(validation_AUC),cap_hits=sum(cap_hit)),by=condition]
  summary <- as.data.table(merge(p$grid,merge(summary,phase,by='condition'),by='condition',sort=FALSE))
  summary <- summary[match(p$grid$condition,summary$condition)]
  validation <- r[,.(mean_validation_AUC=mean(validation_AUC)),by=.(partition,condition)]
  validation$order <- match(validation$condition,p$grid$condition)
  setorder(validation,partition,-mean_validation_AUC,order)
  selected <- validation[,.SD[1],by=partition]
  tuned <- merge(pred,selected[,.(partition,condition)],by=c('partition','condition'))
  stopifnot(all(tuned[,.N,by=seed]$N==nrow(d)))
  tuned_scores <- tuned[,.(AUC=unname(study_metrics(observed,prediction)[['AUC']])),by=seed]
  tuned_fits <- merge(r,selected[,.(partition,condition)],by=c('partition','condition'))
  primary <- data.frame(procedure='validation-selected grid',CV_AUC=mean(tuned_scores$AUC),
    seed_sd=sd(tuned_scores$AUC),training_AUC=mean(tuned_fits$training_AUC),validation_AUC=mean(tuned_fits$validation_AUC),
    baseline_CV_AUC=summary[condition=='baseline',CV_AUC])
  factors <- list()
  for(axis in c('learning_rate','dropout','batch_size','weight_decay')) {
    old <- p$grid[[axis]][1L];new <- unique(p$grid[[axis]])[2L]
    factors[[axis]] <- data.frame(factor=axis,current=old,alternative=new,
      mean_CV_AUC_change=mean(summary[get(axis)==new,CV_AUC])-mean(summary[get(axis)==old,CV_AUC]))
  }
  if(action=='report') {
    for(name in c('summary','pooled','r','selected','tuned_scores','tuned','primary')) {
      filename <- c(summary='grid-auc',pooled='cv-auc-by-seed',r='fits',selected='validation-selected-settings',
        tuned_scores='selected-cv-auc-by-seed',tuned='selected-predictions',primary='primary-auc')[[name]]
      study_write_csv(get(name),file.path(report_directory,paste0(filename,'.csv')))
    }
    study_write_csv(rbindlist(factors),file.path(report_directory,'factor-effects.csv'))
    study_write_csv(rbindlist(baseline_controls),file.path(report_directory,'control-reproduction.csv'))
    writeLines('PASS 240 checkpoint contracts, restored training AUCs, 15 baseline controls, held-out replay and validation-only grid selection.',file.path(report_directory,'verification.txt'))
  } else stopifnot(isTRUE(all.equal(as.data.frame(fread(file.path(report_directory,'grid-auc.csv'))),as.data.frame(summary),tolerance=1e-12,check.attributes=FALSE)))
  print(summary);print(primary)

  invisible(list(summary=summary,primary=primary,selected=selected))
}
