# Aggregate only verified checkpoints. No condition or seed is selected on test AUC.
simplified_report <- function(output,p,tasks_for,action) {
  stopifnot(length(list.files(file.path(output,'models'),pattern='[.]rds$'))==p$total_fits)
  d <- readRDS(file.path(output,'data/prepared_data.rds'))
  controls <- readRDS(file.path(output,'data/reference-controls.rds'))
  records <- predictions <- reproduced <- list()
  for(part in p$partitions) {
    fold <- readRDS(file.path(output,'partitions',paste0(part,'.rds')))
    stopifnot(file.exists(file.path(output,'reports',paste0(part,'-verification.txt'))))
    pred <- data.table::fread(file.path(output,'reports',paste0(part,'-predictions.csv')))
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
      if(t$candidate=='baseline') {
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
  b <- summary[condition=='baseline']
  summary$heldout_change_from_baseline <- summary$heldout_AUC-b$heldout_AUC
  delta_seed <- merge(by_seed[condition=='simplified'],by_seed[condition=='baseline'],by='seed',suffixes=c('_alternative','_baseline'))
  delta_seed$heldout_AUC_change <- delta_seed$heldout_AUC_alternative-delta_seed$heldout_AUC_baseline
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
    # Keep numeric columns numeric until final formatting; coercing the whole
    # mixed-type table to a character matrix can round values prematurely.
    rows <- vapply(seq_len(nrow(summary)),function(i) {
      x <- summary[i]
      sprintf('| %s | %.6f | %.6f | %.6f | %+.6f | %s |',
        x$condition,x$training_AUC,x$validation_AUC,x$heldout_AUC,
        x$heldout_change_from_baseline,x$parameters)
    },character(1))
    writeLines(c('# Cloze: previous GRU versus simplified alternative','',
      'Thirty fresh fits: two conditions, the same five participant folds and seeds 1–3.',
      'All training controls and validation-log-loss checkpoint restoration are held fixed.','',
      '| Model | Training AUC | Validation AUC | Held-out CV AUC | Change | Parameters |',
      '|---|---:|---:|---:|---:|---:|',rows,'',
      'Training/validation AUCs average the fifteen restored fits per condition. Held-out AUC pools all five outer test folds within each seed, then averages the three seed AUCs. Predictions are not ensembled.','',
      'All thirty saved models passed held-out prediction replay. All fifteen fresh baseline controls reproduced the previous saved probabilities within 0.000003. Independent pROC calculations agreed with all six pooled AUCs within 1e-12.','',
      paste0('Maximum fresh-control probability difference: ',format(max(results$control_reproduction$maximum_probability_difference),digits=12),'.'),
      paste0('Fits at the 100-epoch cap: baseline ',b$cap_hits,'/15; alternative ',summary[condition=='simplified',cap_hits],'/15.'),'',
      'The alternative combines the seven numerical inputs into 7 -> 32 -> 32, removes the global intercept and response encoder, and consolidates the single-head retrieval value/output maps. It has 3,152 fewer parameters.','',
      'These are exploratory point estimates on previously inspected Cloze data. No architecture or seed was selected using held-out performance. No active release was changed.'),
      file.path(output,'reports/RESULTS.md'))
    writeLines('PASS 30 checkpoint contracts, restored phase AUCs, held-out replays, 15 fresh controls and six independent pooled AUCs.',file.path(output,'reports/verification.txt'))
  }
  print(summary);print(by_seed)
  invisible(results)
}
