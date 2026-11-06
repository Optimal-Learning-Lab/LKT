# Aggregate the fixed-split and outer-fold evaluations without selecting a winner
# from test results. Every development/CV choice was frozen on inner validation.
study_report <- function(output,p) {
  suppressPackageStartupMessages(library(data.table))
  predictions <- scores <- list()
  for(partition_name in p$partitions) {
    stopifnot(file.exists(file.path(output,'reports',paste0(partition_name,'-verification.txt'))))
    d <- study_data(output,p,partition_name)
    extra <- readRDS(file.path(output,'selection',paste0(partition_name,'-followup.rds')))
    tasks <- rbindlist(list(study_tasks(p,partition_name),extra$tasks),fill=TRUE)
    # Independently recompute ALL 3132 validation records, including losing candidates.
    # Test metrics for unselected candidates are intentionally not published.
    for(i in seq_len(nrow(tasks))) {
      task <- tasks[i,];saved <- readRDS(file.path(output,'models',paste0(task$run_id,'.rds')));fit <- saved$fit
      expected <- if(task$candidate %in% names(p$settings))p$settings[[task$candidate]] else extra$settings[[task$run_id]]
      expected$seed <- as.integer(task$seed)
      stopifnot(identical(fit$gru_settings,expected),
        identical(saved$task$run_id,task$run_id),
        identical(saved$fingerprint,digest::digest(list(study=study_hash(file.path(output,'study.rds')),
          task=saved$task,settings=saved$settings),algo='sha256')),
        sum(vapply(fit$model$weights,length,integer(1)))==fit$parameter_count,
        identical(fit$fit_rows,which(d$fold==1L)),identical(fit$validation_rows,which(d$fold==2L)),
        identical(fit$evaluation_rows,which(d$fold==3L)))
      val <- fread(file.path(output,'validation',paste0(task$run_id,'.csv')))
      metrics <- study_metrics(d$CF..ansbin.[d$fold==2L],fit$prediction[d$fold==2L])
      for(n in names(metrics))stopifnot(abs(val[[n]]-metrics[[n]])<1e-12)
    }
    predictions[[partition_name]] <- fread(file.path(output,'reports',paste0(partition_name,'-predictions.csv')))
    scores[[partition_name]] <- fread(file.path(output,'reports',paste0(partition_name,'-test.csv')))
  }
  pred <- rbindlist(predictions);scores <- rbindlist(scores)
  stopifnot(length(list.files(file.path(output,'models'),pattern='[.]rds$'))==p$total_fits)
  # Every CV response occurs in exactly one outer test fold for each stage/seed.
  cv <- pred[partition!='development']
  original <- readRDS(file.path(output,'data/prepared_data.rds'))
  stopifnot(!anyDuplicated(cv[,.(family,stage,seed,source_row)]),
    all(cv[,.(n=.N),by=.(family,stage,seed)]$n==nrow(original)))
  for(partition_name in p$partitions) {
    a <- pred[partition==partition_name]
    stopifnot(all(a$observed==original$CF..ansbin.[match(a$source_row,original$comparison_source_row)]))
  }
  pooled <- pred[,as.list(study_metrics(observed,prediction)),by=.(evaluation=ifelse(partition=='development','development','cross_validation'),family,stage,seed)]
  summary <- pooled[,.(log_loss=mean(log_loss),seed_sd=sd(log_loss),AUC=mean(AUC),RMSE=mean(RMSE)),by=.(evaluation,family,stage)]
  fold_summary <- scores[,.(log_loss=mean(log_loss),seed_sd=sd(log_loss),AUC=mean(AUC),RMSE=mean(RMSE),
    parameter_count=mean(parameter_count),mean_selected_epoch=mean(selected_epoch)),by=.(partition,family,stage)]
  differences <- list();set.seed(20261103)
  contrasts <- list(c('encoders','plain'),c('inference','plain'),c('combined','encoders'),c('combined','inference'),
    c('best_inference','capacity'),c('gaussian','best_inference'),c('gaussian_kl','gaussian'))
  losses <- pred[,.(loss=mean(-ifelse(observed==1,log(prediction),log1p(-prediction)))),
    by=.(evaluation=ifelse(partition=='development','development','cross_validation'),family,stage,source_row,participant)]
  for(evaluation_name in unique(losses$evaluation))for(family_name in unique(losses$family))for(pair in contrasts) {
    a <- losses[evaluation==evaluation_name & family==family_name & stage==pair[1L]]
    b <- losses[evaluation==evaluation_name & family==family_name & stage==pair[2L]]
    setorder(a,source_row);setorder(b,source_row)
    stopifnot(identical(a$source_row,b$source_row),identical(a$participant,b$participant))
    totals <- data.table(participant=a$participant,delta=a$loss-b$loss)[,.(sum=sum(delta),n=.N),by=participant]
    boot <- replicate(2000L,{idx <- sample.int(nrow(totals),replace=TRUE);sum(totals$sum[idx])/sum(totals$n[idx])})
    differences[[length(differences)+1L]] <- data.frame(evaluation=evaluation_name,family=family_name,
      new=pair[1L],reference=pair[2L],new_minus_reference=mean(a$loss-b$loss),
      lower=unname(quantile(boot,.025)),upper=unname(quantile(boot,.975)))
  }
  fwrite(summary,file.path(output,'summary.csv'));fwrite(fold_summary,file.path(output,'fold_summary.csv'))
  fwrite(scores,file.path(output,'selected_test_by_seed.csv'))
  fwrite(rbindlist(differences),file.path(output,'paired_differences.csv'))
  table <- function(x) {
    x <- as.data.frame(x);x[] <- lapply(x,function(v)if(is.numeric(v))format(round(v,6),trim=TRUE) else as.character(v))
    paste0('<table><tr>',paste0('<th>',names(x),'</th>',collapse=''),'</tr>',
      paste(vapply(seq_len(nrow(x)),function(i)paste0('<tr>',paste0('<td>',unlist(x[i,],use.names=FALSE),'</td>',collapse=''),'</tr>'),character(1)),collapse=''),'</table>')
  }
  document <- paste0('<!doctype html><html lang="en"><meta charset="utf-8"><title>GRU encoders and inference</title>',
    '<style>body{font:16px/1.5 system-ui;max-width:1250px;margin:32px auto;padding:0 20px}table{border-collapse:collapse;margin:18px 0}th,td{padding:7px 10px;border-bottom:1px solid #ddd;text-align:left}th{background:#eef4f7}</style>',
    '<h1>GRU feature encoders and separate inference</h1>',
    if(!is.null(p$type))paste0('<p><strong>',p$type,'</strong></p>') else '',
    '<p>',p$total_fits,' fresh fits in ',length(p$partitions),' partitions. ',
    'The main four architectures use a three-layer, 32-unit GRU and a 128-unit prediction head. The capacity control widens only the ordinary head to match the best inference candidate.</p>',
    '<p>Configuration selection and early stopping use validation log loss only. All three input versions and seeds 1, 2, 3 are retained. ',
    'Cross-validation repeats the complete search independently in five outer whole-participant folds with inner participant validation.</p>',
    '<h2>Cross-validation: pooled held-out metrics averaged across three seeds</h2>',table(summary[evaluation=='cross_validation']),
    '<h2>Original development split</h2>',table(summary[evaluation=='development']),
    '<h2>Per-fold results</h2>',table(fold_summary),
    '<h2>Paired response-loss differences</h2><p>Negative favors the new condition. Whole-participant bootstrap intervals condition on fitted models and chosen settings.</p>',table(rbindlist(differences)),
    '<p>This is a UNVaMP-inspired mechanism comparison, not an exact reproduction. Architecture development previously used this dataset. ',
    'Fixed engineered-feature parameters are inherited from the earlier protocol and are not reestimated per fold. Reported seed averages are not ensembles.</p>',
    '<p><a href="verification.txt">Verification</a> | <a href="candidates.csv">Grid</a> | <a href="participant_assignments.csv">Participant assignments</a> | <a href="paired_differences.csv">Paired comparisons</a></p></html>')
  writeLines(document,file.path(output,'report.html'))
  writeLines(c(paste('PASS: release/source/data/partition contracts;',p$total_fits,'complete checkpoints and all validation metrics independently recomputed.'),
    'PASS: all selections use mean inner-validation loss across three seeds; no test-based configuration or seed selection.',
    'PASS: all selected test probabilities replayed from saved weights; five-fold coverage includes each participant/response exactly once per stage and seed.',
    'CV tuning is confined to each outer training/validation partition. Fixed engineered-feature parameters are inherited, not reestimated.',
    'This dataset and development split were previously inspected. Participant bootstrap intervals condition on selected models and settings.'),file.path(output,'verification.txt'))
  print(summary);cat('ENCODER_STUDY_COMPLETE\n')
}
