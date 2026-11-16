# DOWNLOADED_RUN INDEPENDENT_AUDIT OUTPUT_MARKDOWN
# Produce a readable derived report; never alter the frozen remote reports.
a <- commandArgs(TRUE);stopifnot(length(a)==3L,!file.exists(a[3L]))
run <- a[1L];audit <- jsonlite::fromJSON(file.path(a[2L],'verification.json'))
stopifnot(isTRUE(audit$complete),audit$checkpoints==30L,audit$learned_parameter_triplets==15L)
p <- readRDS(file.path(run,'results/study.rds'))
fits <- read.csv(file.path(run,'results/reports/fits.csv'))
summary <- read.csv(file.path(run,'results/reports/summary.csv'))
parameters <- read.csv(file.path(run,'results/reports/parameters.csv'))
seeds <- read.csv(file.path(run,'results/reports/pooled-by-seed.csv'))
stopifnot(nrow(fits)==30L,nrow(parameters)==15L,nrow(summary)==2L,nrow(seeds)==6L)
paired <- merge(fits[fits$condition=='stock',],fits[fits$condition=='hidden_decay',],
  by=c('partition','seed'),suffixes=c('_stock','_decay'),sort=FALSE)
paired <- paired[order(paired$partition,paired$seed),];stopifnot(nrow(paired)==15L)
parameters <- parameters[order(parameters$partition,parameters$seed),]
rows <- vapply(seq_len(nrow(summary)),function(i) {
  r <- summary[i,]
  sprintf('| %s | %.8f | %.8f | %+.8f | %+.8f |',r$condition,r$heldout_AUC,r$heldout_log_loss,
    r$heldout_AUC_change_vs_stock,r$heldout_log_loss_change_vs_stock)
},character(1))
paired_rows <- vapply(seq_len(nrow(paired)),function(i) {
  r <- paired[i,]
  sprintf('| %s | %d | %.6f | %.6f | %+.6f | %.6f | %.6f | %+.6f |',
    r$partition,r$seed,r$test_AUC_stock,r$test_AUC_decay,r$test_AUC_decay-r$test_AUC_stock,
    r$test_log_loss_stock,r$test_log_loss_decay,r$test_log_loss_decay-r$test_log_loss_stock)
},character(1))
parameter_rows <- vapply(seq_len(nrow(parameters)),function(i) {
  r <- parameters[i,]
  sprintf('| %s | %d | %.6f | %.6f | %.6f | %.6f | %.6f |',r$partition,r$seed,
    r$raw_a,r$raw_b,r$raw_c,r$baseline_hidden_half_life_days,r$DK_half_life_days)
},character(1))
seed_rows <- vapply(seq_len(nrow(seeds)),function(i) {
  r <- seeds[i,];sprintf('| %s | %d | %.8f | %.8f |',r$condition,r$seed,r$AUC,r$log_loss)
},character(1))
budget_rows <- vapply(c('stock','hidden_decay'),function(condition) {
  x <- fits[fits$condition==condition,]
  sprintf('| %s | %d | %.2f | %.2f | %d | %.2f |',condition,unique(x$parameter_count),
    mean(x$selected_epoch),mean(x$epochs_run),sum(x$cap_hit),median(x$elapsed_seconds)/60)
},character(1))
writeLines(c('# Cloze: stock GRU versus signed-DK hidden-unit decay','',
  'All 30 fresh research fits completed and passed saved-weight held-out replay. A separate local audit matched the frozen local protocol and independently checked all metrics and learned-parameter transformations.','',
  '## Aggregate held-out results','',
  'Predictions are pooled over the five participant test folds within each seed. Metrics are then averaged across the three seeds; there is no ensemble and no selection on held-out outcomes. Higher AUC and lower log loss are better.','',
  '| Condition | AUC | Log loss | AUC change vs stock | Log-loss change vs stock |',
  '|---|---:|---:|---:|---:|',rows,'',
  '## Every fold and seed: held-out comparison','',
  '| Fold | Seed | Stock AUC | Decay AUC | AUC change | Stock log loss | Decay log loss | Loss change |',
  '|---|---:|---:|---:|---:|---:|---:|---:|',paired_rows,'',
  '## Learned parameters and half-lives','',
  'These are learned estimates from each validation-selected checkpoint. Three scalars are shared across all hidden units and layers; individual student histories retain their own running DK values.','',
  '| Fold | Seed | a | b | c | Baseline hidden half-life (days) | DK half-life (days) |',
  '|---|---:|---:|---:|---:|---:|---:|',parameter_rows,'',
  'The baseline hidden half-life is log(2)/softplus(a), evaluated at faded DK=0. The DK fading half-life is log(2)/softplus(c). b is a signed influence coefficient and has no standalone half-life. For an individual unit, its current hidden half-life depends on faded DK through log(2)/softplus(a+b*DK). Rates use days internally.','',
  'Initialization uses raw a=b=c=0. Consequently both initial half-lives are one day: softplus(0)=log(2). b=0 initially removes DK influence but does not remove baseline hidden decay. This experiment therefore does not start from an exactly stock-equivalent model. Mathematically the positive softplus baseline approaches zero only as a tends to minus infinity.','',
  '## Pooled results by seed','',
  '| Condition | Seed | AUC | Log loss |','|---|---:|---:|---:|',seed_rows,'',
  '## Training budget and execution','',
  '| Condition | Parameters | Mean selected epoch | Mean epochs run | 100-epoch cap hits | Median fit minutes |',
  '|---|---:|---:|---:|---:|---:|',budget_rows,'',
  'Both models retain the same six numeric inputs, categorical inputs, fixed nonlinear LKT coefficients, three width-32 GRU layers, 128-wide head, retrieval settings, optimizer, dropout probabilities, training folds and seeds. Training uses the existing 100-epoch cap, patience 10 and minimum-validation-log-loss checkpoint restoration. The only added trainable quantities are a, b and c.','',
  'Both implementations use Torch. The decay implementation performs custom hidden/DK updates between trials through an R-level chronological loop. It therefore consumes dropout randomness differently from the fused stock path and is slower. Times describe shared-cluster execution, not isolated throughput. Pending tasks were moved to an available node with the same reported core/memory/OS configuration; running tasks were not restarted.','',
  '## Verification and evidence','',
  sprintf('- %d saved checkpoints independently audited locally.',audit$checkpoints),
  sprintf('- %d phase metric pairs and %d pooled metric pairs independently checked with pROC and dbinom.',audit$phase_metric_pairs,audit$pooled_metric_pairs),
  sprintf('- Fifteen learned triplets and both half-life transformations checked at tolerance %.1e.',audit$metric_tolerance),
  sprintf('- Maximum saved-versus-replayed held-out probability difference: %.12g.',audit$maximum_replay_probability_difference),
  '- Exact frozen local data, participant roles, neural settings, seeds and analysis source identities matched.',
  '- The central active release and historical studies were preserved.','',
  'The full-precision per-fit CSV contains training, validation and test AUC and log loss for every one of the 30 fits, along with raw parameters, rates, half-lives, epochs and times. This is an exploratory matched-budget comparison on previously inspected Cloze data. No held-out tuning or condition/seed selection was performed.'),a[3L])
cat('PASS expanded report: all fifteen matched fold/seed comparisons and parameter triplets, full seed summaries and verified execution details.\n')
