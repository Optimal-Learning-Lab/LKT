# Compare an exploratory depth follow-up with an already scored benchmark.
# Args: reference benchmark directory, follow-up benchmark directory.
args <- commandArgs(TRUE)
stopifnot(length(args)==2L)
reference <- normalizePath(args[1L],winslash='/',mustWork=TRUE)
output <- normalizePath(args[2L],winslash='/',mustWork=TRUE)
stopifnot(reference!=output)
suppressPackageStartupMessages(library(data.table))
# Confirm that the earlier and follow-up experiments differ only in their declared depth
# candidates. Data, partitions, input definitions, and shared training controls must
# agree before their scores are compared.
old <- readRDS(file.path(reference,'protocol.rds'))
new <- readRDS(file.path(output,'protocol.rds'))
shared <- c('schema','dataset','data_description','timing_policy','preprocessing','preprocessing_source_sha256','input_data_sha256',
  'input_parameters_sha256','baseline','split_seed','split_fractions','counts',
  'families','seeds','common','software','sources')
stopifnot(identical(old[shared],new[shared]),
  identical(new$followup_reference$directory,reference),
  identical(new$followup_reference$protocol_sha256,
    digest::digest(file.path(reference,'protocol.rds'),algo='sha256',file=TRUE)),
  identical(new$followup_reference$test_predictions_sha256,
    digest::digest(file.path(reference,'test_predictions.csv'),algo='sha256',file=TRUE)),
  identical(as.data.frame(fread(file.path(reference,'participant_split.csv'))),
    as.data.frame(fread(file.path(output,'participant_split.csv')))))
for(directory in c(reference,output))stopifnot(file.exists(file.path(directory,'verification.txt')))

read_predictions <- function(directory)fread(file.path(directory,'test_predictions.csv'),
  colClasses=c(participant='character'))
prior_pred <- read_predictions(reference);followup_pred <- read_predictions(output)
# Pair predictions by original response row and training seed. Matching only aggregate
# scores would miss a changed sample or a mislabeled model.
align <- function(a,b) {
  keys <- c('source_row','seed')
  stopifnot(!anyDuplicated(a[,..keys]),!anyDuplicated(b[,..keys]))
  pair <- merge(a,b,by=keys,suffixes=c('_reference','_followup'))
  stopifnot(nrow(pair)==nrow(a),nrow(pair)==nrow(b),
    all(pair$participant_reference==pair$participant_followup),
    all(pair$observed_reference==pair$observed_followup),
    all(pair$phase_reference==pair$phase_followup),
    all(pair$condition_reference==pair$condition_followup))
  pair
}
# Check that the PPES reference is unchanged in the two runs. The depth comparison
# should not gain or lose accuracy because its reference predictions changed.
baseline <- align(prior_pred[family=='ppes'],followup_pred[family=='ppes'])
stopifnot(max(abs(baseline$prediction_reference-baseline$prediction_followup))<1e-12)
test_ids <- sort(unique(baseline$participant_reference),method='radix')
set.seed(20261101L)
draws <- replicate(2000L,sample.int(length(test_ids),length(test_ids),replace=TRUE))

depth <- function(protocol,architecture)vapply(architecture,function(a)
  protocol$architectures[[a]]$num_layers,integer(1))
read_validation <- function(directory,protocol) {
  d <- fread(file.path(directory,'validation_candidates.csv'))
  d$num_layers <- depth(protocol,d$architecture)
  d
}
prior_val <- read_validation(reference,old);followup_val <- read_validation(output,new)
# Combine validation results across all tried depths before reconstructing the preferred
# depth. The choice is based on validation performance even though the follow-up reused
# a previously inspected test split.
all_val <- rbindlist(list(prior_val,followup_val))
stopifnot(!anyDuplicated(all_val[,.(family,num_layers,seed)]))
validation <- all_val[,.(mean_validation_log_loss=mean(log_loss),
  sd_validation_log_loss=sd(log_loss),training_seeds=.N),by=.(family,num_layers)]
stopifnot(all(validation$training_seeds==length(old$seeds)))
setorderv(validation,c('family','mean_validation_log_loss','num_layers'))
selected <- validation[,.SD[1L],by=family]
fwrite(validation,file.path(output,'validation_depth_comparison.csv'))
fwrite(selected,file.path(output,'validation_selected_depths.csv'))

prior_selected <- fread(file.path(reference,'selected_architectures.csv'))
followup_selected <- fread(file.path(output,'selected_architectures.csv'))
prior_scores <- fread(file.path(reference,'test_summary.csv'))
followup_scores <- fread(file.path(output,'test_summary.csv'))
differences <- list();test_summary <- list()
# Compare the two depths separately within each input version. This keeps an input
# change from being mistaken for a depth effect.
for(f in names(old$families)) {
  a <- prior_pred[family==f];b <- followup_pred[family==f]
  stopifnot(setequal(unique(a$seed),old$seeds),setequal(unique(b$seed),new$seeds),
    uniqueN(a$architecture)==1L,uniqueN(b$architecture)==1L)
  a_name <- unique(a$architecture);b_name <- unique(b$architecture)
  a_depth <- depth(old,a_name);b_depth <- depth(new,b_name)
  stopifnot(a_depth!=b_depth,
    identical(a_name,prior_selected$architecture[prior_selected$family==f]),
    identical(b_name,followup_selected$architecture[followup_selected$family==f]))
  pair <- align(a,b)
  pair$delta <- -dbinom(pair$observed_followup,1,pair$prediction_followup,log=TRUE)+
    dbinom(pair$observed_reference,1,pair$prediction_reference,log=TRUE)
  # Average each response's loss difference across seeds, then collect it by
  # participant. The bootstrap can then preserve each participant's entire contribution
  # to the comparison.
  per_response <- pair[,.(participant=participant_reference[1L],delta=mean(delta),seeds=.N),by=source_row]
  stopifnot(all(per_response$seeds==length(old$seeds)))
  totals <- per_response[,.(loss_difference_sum=sum(delta),trials=.N),by=participant]
  totals <- totals[match(test_ids,participant)]
  stopifnot(!anyNA(totals),setequal(totals$participant,test_ids))
  boot <- apply(draws,2,function(i)sum(totals$loss_difference_sum[i])/sum(totals$trials[i]))
  ci <- quantile(boot,c(.025,.975),names=FALSE)
  old_score <- prior_scores[family==f];new_score <- followup_scores[family==f]
  stopifnot(nrow(old_score)==1L,nrow(new_score)==1L,
    abs(mean(per_response$delta)-(new_score$log_loss-old_score$log_loss))<1e-12)
  differences[[f]] <- data.frame(family=f,reference_layers=a_depth,followup_layers=b_depth,
    followup_minus_reference_log_loss=mean(per_response$delta),lower_95=ci[1L],upper_95=ci[2L],
    resamples=ncol(draws),unit='participant',training_seeds=length(old$seeds))
  for(which_run in c('reference','followup')) {
    row <- if(which_run=='reference')old_score else new_score
    test_summary[[paste(f,which_run)]] <- data.frame(family=f,
      num_layers=if(which_run=='reference')a_depth else b_depth,
      log_loss=row$log_loss,log_loss_sd=row$log_loss_sd,AUC=row$AUC,RMSE=row$RMSE,
      mean_selected_epoch=row$mean_selected_epoch)
  }
}
differences <- rbindlist(differences);test_summary <- rbindlist(test_summary)
fwrite(differences,file.path(output,'paired_depth_log_loss_differences.csv'))
fwrite(test_summary,file.path(output,'test_depth_comparison.csv'))
table_html <- function(d) {
  d <- as.data.frame(d)
  d[] <- lapply(d,function(x)if(is.numeric(x))format(round(x,5),trim=TRUE) else as.character(x))
  escape <- function(x)gsub('<','&lt;',gsub('&','&amp;',x,fixed=TRUE),fixed=TRUE)
  paste0('<table><tr>',paste0('<th>',escape(names(d)),'</th>',collapse=''),'</tr>',
    paste(vapply(seq_len(nrow(d)),function(i)paste0('<tr>',paste0('<td>',
      escape(unlist(d[i,],use.names=FALSE)),'</td>',collapse=''),'</tr>'),character(1)),collapse=''),'</table>')
}
# Present validation choices and test differences together. The report explicitly
# identifies this as an exploratory follow-up rather than a fresh confirmatory
# evaluation.
report <- paste0('<!doctype html><meta charset="utf-8"><title>GRU depth follow-up</title>',
  '<style>body{font:16px system-ui;max-width:1200px;margin:32px auto;padding:0 20px;color:#18212b}table{border-collapse:collapse;margin:20px 0;font-size:14px}td,th{border:1px solid #ccd4dc;padding:8px;text-align:right}th{background:#eef3f7}</style>',
  '<h1>GRU depth follow-up</h1><p>',new$data_description,'. Same participant split, seeds, shared hyperparameters, and PPES specification as the reference run.</p>',
  '<p>This follow-up was requested after inspecting the original test results. Its test scores are exploratory and reuse that test split; they are not new confirmatory evidence or cross-validation.</p>',
  '<h2>Validation performance across all tried depths</h2>',table_html(validation),
  '<h2>Depth selected using validation alone</h2>',table_html(selected),
  '<h2>Reference versus follow-up test performance</h2><p>Metrics average three separately trained models, not ensemble probabilities. Lower log loss and RMSE are better; higher AUC is better. The reference PPES test log loss is ',
  format(prior_scores$log_loss[prior_scores$family=='ppes'],digits=7),'.</p>',table_html(test_summary),
  '<h2>Paired participant-bootstrap differences</h2><p>Negative differences favor the follow-up depth. Intervals use 2,000 whole-participant resamples and average loss across the same training seeds. They are conditional on this split and the inspected architectures.</p>',table_html(differences),
  '<p>Verified identical shared protocol fields and participant assignments; source-row labels and seed pairing; unchanged PPES predictions within 1e-12; and agreement of paired loss differences with the independently verified pooled metrics within 1e-12. Per-run verification files contain the full metric and response-coverage checks.</p>')
writeLines(report,file.path(output,'depth_comparison.html'))
writeLines(c('PASS: shared protocol, split, source rows, seed pairing, and baseline predictions verified.',
  'PASS: paired loss differences match pooled score differences within 1e-12.'),
  file.path(output,'depth_comparison_verification.txt'))
cat('DEPTH TEST RESULTS\n');print(test_summary)
cat('PAIRED DEPTH DIFFERENCES\n');print(differences)
cat('VALIDATION-SELECTED DEPTHS\n');print(selected)
