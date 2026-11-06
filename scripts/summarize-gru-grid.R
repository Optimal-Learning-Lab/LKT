# Progress report contains validation only. Finalization freezes choices before
# calculating test metrics. Standalone args: output directory, progress/final.
gru_grid_report <- function(output,finalize=FALSE) {
  suppressPackageStartupMessages(library(data.table))
  protocol_file <- file.path(output,'grid_protocol.rds')
  # Read the frozen experiment before interpreting any result files. The protocol
  # supplies the expected candidates, seeds, source hashes, and evaluation rule.
  protocol <- readRDS(protocol_file)
  protocol_sha256 <- digest::digest(protocol_file,algo='sha256',file=TRUE)
  # Use the utility code saved with this run, after checking its fingerprint. A later
  # edit to the working repository must not redefine how an archived experiment is
  # summarized.
  utility <- file.path(output,'source','gru-grid-utils.R')
  stopifnot(identical(digest::digest(utility,algo='sha256',file=TRUE),
    protocol$script_sources[['scripts/gru-grid-utils.R']]))
  source(utility,local=environment())
  validation <- gru_grid_validation(output,protocol,require_complete=finalize)
  runs <- validation$runs;summary <- validation$summary
  if(nrow(runs)) {
    fwrite(runs,file.path(output,'validation_runs.csv'))
    fwrite(summary,file.path(output,'validation_grid.csv'))
  }
  # Only rank configurations for which all three seeds have finished. Otherwise an
  # unusually favorable first seed could make an incomplete candidate look like the
  # leader.
  complete <- if(nrow(summary))summary[training_seeds==length(protocol$seeds)] else summary
  leaders <- if(nrow(complete))complete[,head(.SD,5L),by=family] else complete
  # Write a progress view that contains validation information only. It remains useful
  # during training without opening a second route for choosing settings from test
  # performance.
  header <- paste0('<!doctype html><meta charset="utf-8"><title>GRU grid search</title>',
    '<style>body{font:16px system-ui;max-width:1300px;margin:32px auto;padding:0 20px;color:#18212b}table{border-collapse:collapse;margin:20px 0;font-size:14px}td,th{border:1px solid #ccd4dc;padding:7px;text-align:right}th{background:#eef3f7}li{margin:8px 0}</style>',
    '<h1>GRU full factorial grid search</h1><p>',protocol$data_description,'.</p>',
    '<p>Profile: ',protocol$profile,'. Completed fits: ',nrow(runs),' / ',nrow(protocol$runs),
    '. Configurations with all three seeds complete: ',nrow(complete),' / ',nrow(protocol$candidates),'.</p>',
    '<p>Updated ',format(Sys.time(),tz='UTC',usetz=TRUE),'. Candidate ranking uses validation only.</p>',
    '<h2>Grid axes</h2>',gru_grid_html_table(data.frame(parameter=names(protocol$axes),
      values=vapply(protocol$axes,function(x)paste(x,collapse=', '),character(1)))),
    '<h2>Validation leaders among complete three-seed configurations</h2>',gru_grid_html_table(leaders))
  writeLines(header,file.path(output,'progress.html'))
  writeLines(sprintf('%s: %s/%s fits; %s/%s complete configurations.',
    format(Sys.time(),tz='UTC',usetz=TRUE),nrow(runs),nrow(protocol$runs),nrow(complete),nrow(protocol$candidates)),
    file.path(output,'progress.txt'))
  # Stop here for a progress-only request. Final reporting continues only after the
  # expected grid is complete.
  if(!finalize)return(invisible(validation))
  stopifnot(nrow(summary)==nrow(protocol$candidates),
    all(summary$training_seeds==length(protocol$seeds)),
    setequal(summary$candidate_id,protocol$candidates$candidate_id))
  # Select the lowest mean validation log loss separately for each input version. Save
  # both the choice and the validation-file hashes before reading selected-model test
  # predictions.
  selected <- summary[,.SD[1L],by=family]
  selection <- list(protocol_sha256=protocol_sha256,selected=as.data.frame(selected),
    validation_file_hashes=setNames(vapply(protocol$runs$run_id,function(id)
      digest::digest(file.path(output,'validation',paste0(id,'.csv')),algo='sha256',file=TRUE),character(1)),protocol$runs$run_id))
  selection_file <- file.path(output,'selection.rds')
  if(file.exists(selection_file))stopifnot(identical(readRDS(selection_file),selection)) else
    gru_grid_atomic_rds(selection,selection_file)
  gru_grid_atomic_csv(selected,file.path(output,'selection.csv'))

  # Selection is now immutable. Only the chosen configurations are test-scored.
  prepared_file <- file.path(output,'prepared_data.rds')
  stopifnot(identical(digest::digest(prepared_file,algo='sha256',file=TRUE),protocol$prepared_data_sha256),
    identical(digest::digest(file.path(output,protocol$baseline_filename),algo='sha256',file=TRUE),protocol$baseline_checkpoint_sha256))
  data <- readRDS(prepared_file)
  baseline <- readRDS(file.path(output,protocol$baseline_filename))
  rows <- which(data$fold==3L);y <- data$CF..ansbin.[rows]
  ids <- as.character(data$Anon.Student.Id[rows]);base_p <- baseline$prediction[rows]
  base_loss <- -dbinom(y,1,base_p,log=TRUE)
  scores <- list(ppes=data.frame(model='PPES',family='ppes',candidate_id='configured',seed=NA_integer_,
    as.list(gru_grid_metrics(y,base_p))))
  # Keep source row numbers and participant identities beside every test prediction.
  # These identifiers support paired comparisons and an independent check of complete
  # held-out coverage.
  pred_frame <- function(p,f,candidate,seed)data.frame(source_row=data$comparison_source_row[rows],
    participant=ids,observed=y,family=f,candidate_id=candidate,seed=seed,prediction=p)
  predictions <- list(ppes=pred_frame(base_p,'ppes','configured',NA_integer_))
  test_ids <- sort(unique(ids),method='radix')
  set.seed(20261101L)
  # Resample whole test participants rather than individual responses. This preserves
  # within-person sequences when estimating uncertainty in the loss difference.
  draws <- replicate(2000L,sample.int(length(test_ids),length(test_ids),replace=TRUE))
  differences <- list()
  # Evaluate only the configuration already chosen for each input version. Each seed
  # remains a separate fitted model, and the final table averages their metrics.
  for(i in seq_len(nrow(selected))) {
    s <- selected[i];losses <- matrix(NA_real_,length(rows),length(protocol$seeds))
    for(j in seq_along(protocol$seeds)) {
      seed <- protocol$seeds[j];run_id <- paste0(s$candidate_id,'_seed',seed)
      checkpoint <- readRDS(file.path(output,'models',paste0(run_id,'.rds')))
      stopifnot(identical(checkpoint$protocol_sha256,protocol_sha256))
      fit <- checkpoint$fit
      gru_grid_validate_fit(fit,protocol$run_settings[[run_id]],data)
      p <- fit$prediction[rows]
      scores[[run_id]] <- data.frame(model=paste('GRU',s$family),family=s$family,
        candidate_id=s$candidate_id,seed=seed,as.list(gru_grid_metrics(y,p)))
      predictions[[run_id]] <- pred_frame(p,s$family,s$candidate_id,seed)
      losses[,j] <- -dbinom(y,1,p,log=TRUE)
    }
    # Compare each response with the PPES prediction for that same response. Average
    # losses across seeds before the participant bootstrap; this is not a probability
    # ensemble.
    delta <- rowMeans(losses)-base_loss
    totals <- vapply(test_ids,function(id)sum(delta[ids==id]),numeric(1))
    n <- vapply(test_ids,function(id)sum(ids==id),integer(1))
    boot <- apply(draws,2,function(idx)sum(totals[idx])/sum(n[idx]))
    ci <- quantile(boot,c(.025,.975),names=FALSE)
    differences[[s$family]] <- data.frame(family=s$family,candidate_id=s$candidate_id,
      GRU_minus_PPES_log_loss=mean(delta),lower_95=ci[1L],upper_95=ci[2L],resamples=2000L,unit='participant')
  }
  scores <- rbindlist(scores);differences <- rbindlist(differences)
  # Summarize the repeated fits and retain their seed variability. This variability
  # describes initialization and training, not different cross-validation folds.
  test_summary <- scores[,.(training_seeds=.N,log_loss=mean(log_loss),
    log_loss_sd=if(.N>1L)sd(log_loss) else 0,AUC=mean(AUC),Brier=mean(Brier),RMSE=mean(RMSE)),
    by=.(model,family,candidate_id)]
  fwrite(scores,file.path(output,'test_scores_by_seed.csv'))
  fwrite(test_summary,file.path(output,'test_summary.csv'))
  fwrite(rbindlist(predictions),file.path(output,'test_predictions.csv'))
  fwrite(differences,file.path(output,'paired_log_loss_differences.csv'))
  # Place the choices, test scores, and limitations together in one report. A reader
  # should be able to distinguish the search result from the conditions under which it
  # was measured.
  report <- paste0(header,'<h2>Validation-selected configurations</h2>',gru_grid_html_table(selected),
    '<h2>Selected configurations: test performance</h2><p>Metrics average three separately trained single models. Lower log loss, Brier and RMSE are better; higher AUC is better. These are not ensemble predictions.</p>',gru_grid_html_table(test_summary),
    '<h2>Paired participant-bootstrap log-loss differences</h2><p>Negative values favor GRU. Intervals use 2,000 whole-participant resamples and condition on this split, selected settings, and training seeds.</p>',gru_grid_html_table(differences),
    '<h2>Limits</h2><ul>',paste0('<li>',protocol$caveats,'</li>',collapse=''),'</ul>')
  writeLines(report,file.path(output,'report.html'))
  cat('SELECTED CONFIGURATIONS\n');print(selected)
  cat('TEST RESULTS\n');print(test_summary)
  invisible(list(selection=selected,test_summary=test_summary,differences=differences))
}

if(sys.nframe()==0L) {
  args <- commandArgs(TRUE)
  stopifnot(length(args)>=1L,length(args)<=2L)
  mode <- if(length(args)==2L)args[2L] else 'progress'
  stopifnot(mode %in% c('progress','final'))
  gru_grid_report(args[1L],finalize=mode=='final')
}
