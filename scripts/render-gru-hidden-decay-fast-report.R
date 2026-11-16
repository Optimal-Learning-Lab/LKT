# ROOT DOWNLOAD_DIRECTORY AUDIT_DIRECTORY PREVIOUS_RESULTS REPORT_PATH
a <- commandArgs(TRUE);stopifnot(length(a)==5L,!file.exists(a[5L]))
.libPaths(c(file.path(a[1L],'.codex-r-lib'),.libPaths()))
run <- a[2L];audit <- jsonlite::fromJSON(file.path(a[3L],'verification.json'))
stopifnot(audit$complete,audit$checkpoints==60L)
reports <- file.path(run,'results/reports')
fits <- read.csv(file.path(reports,'fits.csv'))
summary <- read.csv(file.path(reports,'summary.csv'))
gains <- read.csv(file.path(reports,'mean-decay-gains.csv'))
interaction <- read.csv(file.path(reports,'mean-interaction.csv'))
old <- read.csv(file.path(a[4L],'reports/fits.csv'))
fmt <- function(x)ifelse(is.infinite(x),'Inf',formatC(x,digits=7,format='g'))
lines <- c('# Cloze: compiled hidden decay, with and without PPES','',
  'Completed 60 fresh fits on Big Blue: stock/decay × PPES present/absent × five unchanged whole-participant folds × seeds 1–3. The preceding experiment remains preserved.','',
  '| Model | PPES | Held-out AUC | Held-out log loss |','|---|---|---:|---:|')
for(i in seq_len(nrow(summary))) {
  r <- summary[i,];lines <- c(lines,sprintf('| %s | %s | %.9f | %.9f |',
    if(grepl('^decay_',r$condition))'Compiled decay' else 'Stock',
    if(grepl('_full$',r$condition))'Present' else 'Removed',r$heldout_AUC,r$heldout_log_loss))
}
lines <- c(lines,'','Each held-out metric pools all five test folds within seed, then averages the three seed metrics. Checkpoints minimize inner-validation log loss. No held-out outcome selects an epoch or model.','',
  '| Feature set | Decay AUC gain | Decay log-loss improvement |','|---|---:|---:|')
for(i in seq_len(nrow(gains)))with(gains[i,],{lines <<- c(lines,sprintf('| %s | %+.9f | %+.9f |',
  if(feature_group=='full')'PPES present' else 'PPES removed',AUC_gain,log_loss_improvement))})
lines <- c(lines,'',sprintf('Removing PPES changes decay’s AUC benefit by %+.9f and its log-loss improvement by %+.9f (benefit without PPES minus benefit with PPES). Positive values favor a larger benefit without PPES.',
  interaction$AUC_gain_interaction,interaction$log_loss_improvement_interaction),'',
  'The rate is a exp(b F), where F is signed DK after fading at softplus(c) per day. All three shared scalars start at zero. a is constrained nonnegative by projection after each ordinary optimizer step, so its initial rate is exactly zero and its one-sided gradient can learn a positive rate. DK includes signed response-driven and decay-driven changes. Parameters remain fixed during testing; each student’s hidden state, DK and elapsed-time clock continue to evolve.','',
  'The hidden half-life below is log(2)/a at DK=0. Its current value for another faded DK is log(2)/(a exp(b F)). The DK half-life is log(2)/softplus(c). Half-lives use days; b has no standalone half-life. Inf means a fitted amplitude of exactly zero. These are learned estimates, not guaranteed global optima. At a=0, b and c cannot affect predictions and are not identified by the selected model’s predictions.')
for(group in c('full','no_ppes')) {
  decay <- fits[fits$condition==paste0('decay_',group),];stock <- fits[fits$condition==paste0('stock_',group),]
  table <- merge(decay,stock,by=c('partition','seed'),suffixes=c('_decay','_stock'))
  lines <- c(lines,'',if(group=='full')'## PPES present' else '## PPES removed','',
    '| Fold | Seed | a | b | c | Hidden half-life | DK half-life | Stock AUC | Decay AUC | Stock log loss | Decay log loss |',
    '|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|')
  for(i in seq_len(nrow(table))) {
    r <- table[i,];lines <- c(lines,paste0('| ',paste(c(r$partition,r$seed,fmt(r$raw_a_decay),fmt(r$raw_b_decay),fmt(r$raw_c_decay),
      fmt(r$baseline_hidden_half_life_days_decay),fmt(r$DK_half_life_days_decay),
      sprintf('%.7f',r$test_AUC_stock),sprintf('%.7f',r$test_AUC_decay),
      sprintf('%.7f',r$test_log_loss_stock),sprintf('%.7f',r$test_log_loss_decay)),collapse=' | '),' |'))
  }
}
phase_summary <- aggregate(cbind(training_AUC,training_log_loss,validation_AUC,validation_log_loss)~condition,fits,mean)
lines <- c(lines,'','## Training, validation and execution','',
  '| Condition | Mean training AUC | Mean training log loss | Mean validation AUC | Mean validation log loss | Median fit minutes |',
  '|---|---:|---:|---:|---:|---:|')
for(i in seq_len(nrow(phase_summary))) {
  r <- phase_summary[i,];lines <- c(lines,sprintf('| %s | %.7f | %.7f | %.7f | %.7f | %.2f |',r$condition,
    r$training_AUC,r$training_log_loss,r$validation_AUC,r$validation_log_loss,
    median(fits$elapsed_seconds[fits$condition==r$condition])/60))
}
lines <- c(lines,'',sprintf('The earlier enabled decay version took a median %.2f minutes per fit. The new compiled version with PPES takes %.2f minutes (%.2fx faster by median fit time; parameterization and resulting epoch counts also changed).',
  median(old$elapsed_seconds[old$condition=='hidden_decay'])/60,
  median(fits$elapsed_seconds[fits$condition=='decay_full'])/60,
  median(old$elapsed_seconds[old$condition=='hidden_decay'])/median(fits$elapsed_seconds[fits$condition=='decay_full'])),
  '',sprintf('Epoch cap reached: %d of 60 fits. Selected epochs range %d–%d.',sum(fits$cap_hit),min(fits$selected_epoch),max(fits$selected_epoch)),
  '','## Matching and verification','',
  'Both logitdec predictors retain their original fixed constants. PPES removal deletes its predictor and four fixed constants, producing five numeric inputs rather than six. Common predictors retain identical scaling. All other controls remain matched: three 32-unit GRU layers, 128-unit head, 16-unit embeddings, existing encoders and retrieval, optimizer, dropout, 100-epoch budget and patience 10. The parameter counts are 43,073/43,076 with PPES and 43,041/43,044 without. Initial shared weights match within stock/decay pairs. Input-width changes alter initialization across feature groups. Chronological decay training consumes dropout randomness differently from fused stock training.','',
  sprintf('Independent local audit passed for all 60 checkpoints, 180 training/validation/test metric pairs, 12 pooled metric pairs, all 30 parameter triplets and both half-lives, both conditional gains and their interaction. Maximum saved-weight held-out replay discrepancy: %.9g. All 15 full-stock fits reproduce the earlier six-input controls; maximum probability difference: %.9g.',
    audit$maximum_replay_probability_difference,audit$maximum_stock_control_probability_difference),'',
  'Zero-amplitude stock equivalence, compiled/reference values and gradients, finite differences, padding exclusion, learning away from zero, nonnegative projection, all inference modes, retrieval, continuation, immutable candidates, pending completion and saved-state loading were checked. Earlier enabled checkpoints retain the original softplus equation and replay correctly.','',
  'This is an exploratory comparison on previously inspected Cloze data. The three seed results do not establish statistical significance. Historical outputs and the central release definition were preserved.','',
  'The complete machine-readable fit table, parameter table, seed metrics, gains, interactions, checkpoint audit, source hashes and scheduler exit records accompany this report.','',
  'Including the stock model at a=0 guarantees that it is representable. It does not guarantee that the optimizer and early-stopping procedure find training or test loss as low as a separately fitted stock model. The learned coefficients should be read as fitted checkpoint estimates.')
writeLines(lines,a[5L]);cat('WROTE',a[5L],'\n')
