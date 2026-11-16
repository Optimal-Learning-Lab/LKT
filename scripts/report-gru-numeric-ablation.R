# Aggregate only verified checkpoints. No condition or seed is selected on test AUC.
numeric_ablation_report <- function(output,p,tasks_for,action) {
  stopifnot(length(list.files(file.path(output,'models'),pattern='[.]rds$'))==p$total_fits)
  d <- readRDS(file.path(output,'data/prepared_data.rds'))
  controls <- readRDS(file.path(output,'data/reference-controls.rds'))
  records <- predictions <- reproduced <- list()
  for(part in p$partitions) {
    fold <- readRDS(file.path(output,'partitions',paste0(part,'.rds')))
    shards <- paste0(part,'-shard',0:7)
    stopifnot(all(file.exists(file.path(output,'reports',paste0(shards,'-verification.txt')))))
    pred <- data.table::rbindlist(lapply(shards,function(x)data.table::fread(file.path(output,'reports',paste0(x,'-predictions.csv')))))
    tasks <- tasks_for(part)
    for(i in seq_len(nrow(tasks))) {
      t <- tasks[i,,drop=FALSE]
      saved <- readRDS(file.path(output,'models',paste0(t$run_id,'.rds')));f <- saved$fit
      s <- p$settings[[t$candidate]];s$seed <- as.integer(t$seed)
      stopifnot(identical(s,f$gru_settings),identical(saved$settings,s),
        identical(f$model$recipe$interc,unname(p$intercepts[t$candidate])),
        identical(f$fit_rows,which(fold==1L)),identical(f$validation_rows,which(fold==2L)),
        identical(f$evaluation_rows,which(fold==3L)),f$parameter_count==p$parameter_counts[[t$candidate]],
        identical(saved$fingerprint,digest::digest(list(study=study_hash(file.path(output,'study.rds')),
          task=saved$task,settings=s),algo='sha256')),
        all(is.finite(unlist(f$model$weights))))
      v <- read.csv(file.path(output,'validation',paste0(t$run_id,'.csv')))
      vm <- study_metrics(d$CF..ansbin.[fold==2],f$prediction[fold==2])
      for(n in names(vm))stopifnot(abs(v[[n]]-vm[[n]])<1e-12)
      phase_auc <- vapply(1:3,function(k)unname(study_metrics(d$CF..ansbin.[fold==k],f$prediction[fold==k])[['AUC']]),numeric(1))
      stopifnot(abs(phase_auc[1]-f$training_history$training_auc[match(f$selected_epoch,f$training_history$epoch)])<1e-12)
      q <- pred[condition==t$candidate & seed==t$seed]
      q <- q[match(d$comparison_source_row[fold==3],source_row)]
      stopifnot(nrow(q)==sum(fold==3),!anyNA(q),!anyDuplicated(q$source_row),
        identical(q$source_row,d$comparison_source_row[fold==3]),
        all(q$participant==d$Anon.Student.Id[fold==3]),all(q$observed==d$CF..ansbin.[fold==3]),
        max(abs(q$prediction-f$prediction[fold==3]))<3e-6)
      records[[t$run_id]] <- data.frame(partition=part,condition=t$candidate,seed=t$seed,
        training_AUC=phase_auc[1],validation_AUC=phase_auc[2],fold_test_AUC=phase_auc[3],
        parameters=f$parameter_count,selected_epoch=f$selected_epoch,epochs_run=f$trained_epochs,
        cap_hit=f$trained_epochs==s$epochs,elapsed_seconds=saved$elapsed)
      if(t$candidate=='all') {
        delta <- max(abs(f$prediction-controls[[paste(part,t$seed)]]$prediction))
        stopifnot(delta<3e-6)
        reproduced[[t$run_id]] <- data.frame(run_id=t$run_id,maximum_probability_difference=delta)
      }
    }
    predictions[[part]] <- pred
  }
  r <- data.table::rbindlist(records);pred <- data.table::rbindlist(predictions)
  stopifnot(!anyDuplicated(pred[,.(condition,seed,source_row)]),
    all(pred[,.N,by=.(condition,seed)]$N==nrow(d)))
  pooled <- pred[,.(heldout_AUC=unname(study_metrics(observed,prediction)[['AUC']])),by=.(condition,seed)]
  phases <- r[,.(training_AUC=mean(training_AUC),validation_AUC=mean(validation_AUC)),by=.(condition,seed)]
  by_seed <- data.table::as.data.table(merge(phases,pooled,by=c('condition','seed'),sort=FALSE))
  summary <- by_seed[,.(training_AUC=mean(training_AUC),validation_AUC=mean(validation_AUC),
    heldout_AUC=mean(heldout_AUC),heldout_seed_sd=stats::sd(heldout_AUC)),by=condition]
  extras <- r[,.(parameters=unique(parameters),cap_hits=sum(cap_hit),mean_selected_epoch=mean(selected_epoch)),by=condition]
  summary <- data.table::as.data.table(merge(summary,extras,by='condition',sort=FALSE))
  summary <- summary[match(names(p$settings),condition)]
  summary$heldout_gain_vs_none <- summary$heldout_AUC-summary[condition=='none',heldout_AUC]
  summary$heldout_change_vs_all <- summary$heldout_AUC-summary[condition=='all',heldout_AUC]
  delta_seed <- data.table::copy(by_seed)
  for(k in p$seeds) {
    delta_seed[seed==k,gain_vs_none:=heldout_AUC-by_seed[condition=='none' & seed==k,heldout_AUC]]
    delta_seed[seed==k,change_vs_all:=heldout_AUC-by_seed[condition=='all' & seed==k,heldout_AUC]]
  }
  # Independent AUC implementation checks every pooled seed score and all phases.
  independent <- list()
  for(condition_name in names(p$settings))for(seed_value in p$seeds) {
    q <- pred[condition==condition_name & seed==seed_value]
    auc <- as.numeric(pROC::auc(pROC::roc(q$observed,q$prediction,levels=c(0,1),direction='<',quiet=TRUE)))
    reported <- by_seed[condition==condition_name & seed==seed_value,heldout_AUC]
    stopifnot(abs(auc-reported)<1e-12)
    independent[[paste(condition_name,seed_value)]] <- data.frame(condition=condition_name,seed=seed_value,
      pROC_AUC=auc,maximum_difference=abs(auc-reported))
  }
  results <- list(summary=summary,auc_by_seed=by_seed,fits=r,
    paired_seed_changes=delta_seed,control_reproduction=data.table::rbindlist(reproduced),
    independent_auc=data.table::rbindlist(independent))
  for(name in names(results)) {
    path <- file.path(output,'reports',paste0(gsub('_','-',name),'.csv'))
    if(action=='report')study_write_csv(results[[name]],path) else
      stopifnot(isTRUE(all.equal(as.data.frame(data.table::fread(path)),as.data.frame(results[[name]]),
        tolerance=1e-12,check.attributes=FALSE)))
  }
  if(action=='report') {
    table <- function(conditions,contrast) {
      vapply(conditions,function(condition_name) {
        x <- summary[condition==condition_name]
        j <- as.integer(sub('only|omit','',condition_name))
        label <- if(is.na(j))condition_name else p$features$label[j]
        sprintf('| %s | %.6f | %.6f | %.6f | %+.6f | %d |',label,
          x$training_AUC,x$validation_AUC,x$heldout_AUC,x[[contrast]],x$parameters)
      },character(1))
    }
    header <- c('| Numeric inputs | Training AUC | Validation AUC | Held-out CV AUC | AUC change | Parameters |',
      '|---|---:|---:|---:|---:|---:|')
    writeLines(c('# Cloze numeric-input ablation','',
      '240 fresh fits: 14 requested variants plus no-input and all-seven controls, five identical participant folds and seeds 1-3.',
      'Training controls and minimum validation-log-loss checkpoint restoration are fixed.','',
      '## Controls','',header,table(c('none','all'),'heldout_gain_vs_none'),'',
      '## Each input alone: change from no numeric inputs','',header,
      table(paste0('only',1:7),'heldout_gain_vs_none'),'',
      '## Leave one out: change from all seven inputs','',header,
      table(paste0('omit',1:7),'heldout_change_vs_all'),'',
      'Rows in the leave-one-out table name the omitted input. A negative change means omission hurt held-out AUC. Item/skill embeddings and response history remain available in every condition.',
      'Training/validation AUCs average fifteen restored fits. Held-out AUC pools five test folds within each seed, then averages three seed AUCs. Predictions are not ensembled.','',
      'All 240 checkpoints passed held-out saved-weight replay; all fifteen all-seven controls reproduced the prior simplified model within 0.000003.',
      paste0('Maximum all-seven control probability difference: ',format(max(results$control_reproduction$maximum_probability_difference),digits=12),'.'),
      'Independent pROC checks verified all 48 pooled AUCs. Per-fit epochs, cap hits, parameter counts and per-seed contrasts are provided in CSV files.','',
      'Feature deletion also changes model capacity. Remaining encoder widths stay fixed; no dummy zero inputs or extra compensating parameters are introduced.',
      'Missing spacing is encoded as neutral after training-only scaling; availability is a separate selectable input. Its omission does not make missingness mathematically unidentifiable.',
      'Exploratory point estimates on previously inspected Cloze data; no held-out selection or active release change.'),file.path(output,'reports/RESULTS.md'))
    writeLines('PASS 240 checkpoint contracts, restored phase AUCs, held-out replays, 15 fresh all-seven controls and 48 independent pooled AUCs.',file.path(output,'reports/verification.txt'))
  }
  print(summary);print(by_seed)
  invisible(results)
}
