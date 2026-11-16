# Independent base-R/pROC audit of downloaded immutable comparison artifacts.
# ROOT DOWNLOADED_RUN LOCAL_FROZEN_PLAN AUDIT_OUTPUT ORIGINAL_CONTROL_REFERENCE
a <- commandArgs(TRUE);stopifnot(length(a)==5L,!dir.exists(a[4L]))
root <- a[1L];run <- a[2L];local <- a[3L];output <- a[4L];reference <- a[5L]
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
hash <- function(x)digest::digest(file=x,algo='sha256')
p <- readRDS(file.path(run,'results/study.rds'));original <- readRDS(file.path(local,'study.rds'))
required <- c('settings','tasks','baseline','recipes','partition_hashes','data_sha256','script_hashes','parameter_counts')
stopifnot(all(required%in%names(p)),all(required%in%names(original)))
stopifnot(hash(file.path(reference,'study.rds'))==p$reference_protocol_sha256)
for(n in c('settings','tasks','baseline','recipes','partition_hashes','data_sha256','script_hashes','parameter_counts'))
  stopifnot(identical(p[[n]],original[[n]]))
for(n in names(p$script_hashes))stopifnot(hash(file.path(run,n))==p$script_hashes[[n]],
  hash(file.path(root,n))==p$script_hashes[[n]])
d <- readRDS(file.path(run,'results/data/prepared_data.rds'))
stopifnot(hash(file.path(run,'results/data/prepared_data.rds'))==p$data_sha256)
reported <- read.csv(file.path(run,'results/reports/fits.csv'));parameters <- read.csv(file.path(run,'results/reports/parameters.csv'))
stopifnot(nrow(reported)==60L,nrow(parameters)==30L,!anyDuplicated(reported$run_id))
metrics <- function(y,q)c(AUC=as.numeric(pROC::auc(y,q,quiet=TRUE,direction='<')),
  log_loss=-mean(stats::dbinom(y,1,q,log=TRUE)))
records <- predictions <- controls <- list()
for(i in seq_len(nrow(p$tasks))) {
  task <- p$tasks[i,,drop=FALSE];id <- task$run_id
  path <- file.path(run,'results/models',paste0(id,'.rds'));saved <- readRDS(path);fit <- saved$fit
  s <- p$settings[[task$candidate]];s$seed <- as.integer(task$seed)
  fold_file <- file.path(run,'results/partitions',paste0(task$partition,'.rds'))
  stopifnot(hash(fold_file)==p$partition_hashes[[task$partition]])
  roles <- readRDS(fold_file);r <- reported[reported$run_id==id,,drop=FALSE]
  stopifnot(nrow(r)==1L,identical(saved$settings,s),identical(fit$gru_settings,s),
    identical(fit$fit_rows,which(roles==1L)),identical(fit$validation_rows,which(roles==2L)),
    identical(fit$evaluation_rows,which(roles==3L)),all(is.finite(unlist(fit$model$weights))),
    r$parameter_count==p$parameter_counts[[task$candidate]],
    fit$selected_epoch==fit$training_history$epoch[which.min(fit$training_history$validation_loss)])
  for(n in c('components','features','fixedpars','interc','maketimes','connectors'))
    stopifnot(identical(fit$model$recipe[[n]],p$recipes[[task$candidate]][[n]]))
  stopifnot(identical(as.character(fit$model$recipe$interacts),as.character(p$recipes[[task$candidate]]$interacts)),
    identical(fit$model$recipe$subject_col,s$subject_col))
  if(task$candidate=='stock_full') {
    control_file <- file.path(reference,'models',paste0(task$partition,'__omit1__seed',task$seed,'.rds'))
    control <- readRDS(control_file)$fit
    stopifnot(identical(fit$model$recipe,control$model$recipe),
      identical(fit$fit_rows,control$fit_rows),identical(fit$validation_rows,control$validation_rows),
      identical(fit$evaluation_rows,control$evaluation_rows))
    control_difference <- max(abs(fit$prediction-control$prediction));stopifnot(control_difference<3e-6)
    controls[[id]] <- data.frame(run_id=id,original_checkpoint_sha256=hash(control_file),
      maximum_control_probability_difference=control_difference)
  }
  q <- read.csv(file.path(run,'results/reports',paste0(id,'-predictions.csv')))
  stopifnot(identical(q$source_row,d$comparison_source_row[roles==3L]),
    all(q$participant==d$Anon.Student.Id[roles==3L]),all(q$observed==d$CF..ansbin.[roles==3L]))
  difference <- max(abs(q$prediction-fit$prediction[roles==3L]));stopifnot(difference<3e-6)
  for(k in 1:3) {
    m <- metrics(d$CF..ansbin.[roles==k],if(k==3L)q$prediction else fit$prediction[roles==k])
    prefix <- c('training','validation','test')[k]
    for(n in names(m))stopifnot(abs(r[[paste(prefix,n,sep='_')]]-m[[n]])<1e-12)
  }
  if(grepl('^decay_',task$candidate)) {
    raw <- vapply(paste0('hidden_decay.',c('a','b','c')),function(n)as.numeric(fit$model$weights[[n]]),numeric(1))
    softplus <- function(x)ifelse(x>0,x+log1p(exp(-x)),log1p(exp(x)))
    stopifnot(raw[1]>=0,s$hidden_decay_parameterization=='amplitude')
    rates <- c(raw[1],softplus(raw[3]));expected <- c(raw,rates,log(2)/rates)
    columns <- c('raw_a','raw_b','raw_c','baseline_hidden_rate_per_day','DK_fading_rate_per_day',
      'baseline_hidden_half_life_days','DK_half_life_days')
    parameter_row <- parameters[parameters$partition==task$partition & parameters$seed==task$seed & parameters$condition==task$candidate,,drop=FALSE]
    stopifnot(nrow(parameter_row)==1L)
    for(j in seq_along(columns))stopifnot(isTRUE(all.equal(r[[columns[j]]],unname(expected[j]),tolerance=1e-12)),
      isTRUE(all.equal(parameter_row[[columns[j]]],unname(expected[j]),tolerance=1e-12)))
  } else stopifnot(all(is.na(r[c('raw_a','raw_b','raw_c','baseline_hidden_half_life_days','DK_half_life_days')])))
  records[[id]] <- data.frame(run_id=id,model_sha256=hash(path),maximum_replay_probability_difference=difference,
    selected_epoch=fit$selected_epoch,fit_contract_passed=TRUE)
  predictions[[id]] <- q
}
pred <- do.call(rbind,predictions)
stopifnot(!anyDuplicated(pred[c('condition','seed','source_row')]))
pooled <- read.csv(file.path(run,'results/reports/pooled-by-seed.csv'))
for(i in seq_len(nrow(pooled))) {
  r <- pooled[i,];q <- pred[pred$condition==r$condition & pred$seed==r$seed,]
  stopifnot(nrow(q)==nrow(d));m <- metrics(q$observed,q$prediction)
  for(n in names(m))stopifnot(abs(r[[n]]-m[[n]])<1e-12)
}
summary <- read.csv(file.path(run,'results/reports/summary.csv'))
for(i in seq_len(nrow(summary))) {
  r <- summary[i,];q <- pooled[pooled$condition==r$condition,]
  stopifnot(abs(r$heldout_AUC-mean(q$AUC))<1e-12,abs(r$heldout_log_loss-mean(q$log_loss))<1e-12)
}
gains <- read.csv(file.path(run,'results/reports/decay-gains-by-seed.csv'))
interactions <- read.csv(file.path(run,'results/reports/interaction-by-seed.csv'))
for(seed in 1:3) {
  expected <- list()
  for(group in c('full','no_ppes')) {
    stock <- pooled[pooled$seed==seed & pooled$condition==paste0('stock_',group),]
    decay <- pooled[pooled$seed==seed & pooled$condition==paste0('decay_',group),]
    expected[[group]] <- c(AUC_gain=decay$AUC-stock$AUC,log_loss_improvement=stock$log_loss-decay$log_loss)
    row <- gains[gains$seed==seed & gains$feature_group==group,]
    for(n in names(expected[[group]]))stopifnot(abs(row[[n]]-expected[[group]][[n]])<1e-12)
  }
  delta <- expected$no_ppes-expected$full;row <- interactions[interactions$seed==seed,]
  stopifnot(abs(row$AUC_gain_interaction-delta[1])<1e-12,
    abs(row$log_loss_improvement_interaction-delta[2])<1e-12)
}
mean_gains <- read.csv(file.path(run,'results/reports/mean-decay-gains.csv'))
for(group in c('full','no_ppes'))for(n in c('AUC_gain','log_loss_improvement'))
  stopifnot(abs(mean_gains[mean_gains$feature_group==group,n]-mean(gains[gains$feature_group==group,n]))<1e-12)
mean_interaction <- read.csv(file.path(run,'results/reports/mean-interaction.csv'))
for(n in c('AUC_gain_interaction','log_loss_improvement_interaction'))
  stopifnot(abs(mean_interaction[[n]]-mean(interactions[[n]]))<1e-12)
dir.create(output,recursive=TRUE)
write.csv(do.call(rbind,records),file.path(output,'checkpoint-audit.csv'),row.names=FALSE)
write.csv(do.call(rbind,controls),file.path(output,'stock-control-reproduction.csv'),row.names=FALSE)
jsonlite::write_json(list(complete=TRUE,checkpoints=60L,phase_metric_pairs=180L,pooled_metric_pairs=12L,
  learned_parameter_triplets=30L,local_frozen_protocol_matched=TRUE,metric_tolerance=1e-12,
  stock_controls_checked=15L,maximum_stock_control_probability_difference=max(vapply(controls,function(x)x$maximum_control_probability_difference,numeric(1))),
  maximum_replay_probability_difference=max(vapply(records,function(x)x$maximum_replay_probability_difference,numeric(1)))),
  file.path(output,'verification.json'),auto_unbox=TRUE,pretty=TRUE)
cat('PASS independent local audit: sixty checkpoints, 180 phase and twelve pooled metric pairs, thirty learned triplets, both half-lives, factorial gains and interactions; frozen local protocol matched.\n')
