# Reporting and verification for the frozen stock-versus-hidden-decay experiment.
# Network construction and all decay mathematics remain in the maintained package.
fast_study_replay <- function(fit,d) {
  ns <- asNamespace('LKT');s <- get('lkt_sequence_model',ns)(fit)$settings
  cleanup <- get('lkt_gru_runtime',ns)(s);on.exit(cleanup())
  ord <- get('lkt_gru_order',ns)(d,s);d <- d[ord,,drop=FALSE]
  encoded <- get('lkt_gru_encode',ns)(d,fit$model$preprocessing,fit$model$recipe,s)
  get('lkt_gru_replay',ns)(LKTGRURuntime(fit)$network,encoded,d,s)[order(ord)]
}
fast_study_parameters <- function(fit) {
  values <- rep(NA_real_,7L)
  names(values) <- c('raw_a','raw_b','raw_c','baseline_hidden_rate_per_day',
    'DK_fading_rate_per_day','baseline_hidden_half_life_days','DK_half_life_days')
  if(fit$gru_settings$hidden_decay) {
    raw <- vapply(paste0('hidden_decay.',c('a','b','c')),function(n) {
      x <- fit$model$weights[[n]];stopifnot(is.numeric(x),length(x)==1L,is.finite(x));as.numeric(x)
    },numeric(1))
    softplus <- function(x)pmax(x,0)+log1p(exp(-abs(x)))
    rates <- c(raw[1L],softplus(raw[3L]))
    stopifnot(all(is.finite(rates)),rates[1L]>=0,rates[2L]>0,fit$gru_settings$hidden_decay_parameterization=='amplitude')
    values[] <- c(raw,rates,log(2)/rates)
  }
  as.list(values)
}
fast_study_fit <- function(task,s,output,p,d) {
  recipe <- p$recipes[[task$candidate]]
  s$seed <- as.integer(task$seed);s <- do.call(GRUModel,s)$settings
  path <- file.path(output,'models',paste0(task$run_id,'.rds'))
  fingerprint <- digest::digest(list(study=study_hash(file.path(output,'study.rds')),task=task,settings=s),algo='sha256')
  if(!file.exists(path)) {
    cat('START',task$run_id,'\n');flush.console()
    elapsed <- system.time(fit <- LKT(d,usefolds=1,
      components=recipe$components,features=recipe$features,fixedpars=recipe$fixedpars,
      interc=recipe$interc,interacts=recipe$interacts,connectors=recipe$connectors,maketimes=recipe$maketimes,model=do.call(GRUModel,s),verbose=TRUE))[['elapsed']]
    fit$newdata <- NULL;fit$nullmodel <- NULL
    study_write_rds(list(fingerprint=fingerprint,task=task,settings=s,fit=fit,elapsed=elapsed),path)
  }
  saved <- readRDS(path);fit <- saved$fit
  stopifnot(identical(saved$fingerprint,fingerprint),identical(saved$settings,s),
    identical(fit$gru_settings,s),identical(fit$fit_rows,which(d$fold==1L)),
    identical(fit$validation_rows,which(d$fold==2L)),identical(fit$evaluation_rows,which(d$fold==3L)),
    fit$parameter_count==p$parameter_counts[[task$candidate]],all(is.finite(unlist(fit$model$weights))))
  val <- study_metrics(d$CF..ansbin.[d$fold==2L],fit$prediction[d$fold==2L])
  study_write_csv(cbind(task,as.data.frame(as.list(val))),file.path(output,'validation',paste0(task$run_id,'.csv')))
  study_write_csv(fit$training_history,file.path(output,'training',paste0(task$run_id,'.csv')))
  held <- d[d$fold==3L,,drop=FALSE];q <- fast_study_replay(fit,held)
  difference <- max(abs(q-fit$prediction[d$fold==3L]));stopifnot(difference<3e-6)
  predictions <- data.frame(partition=task$partition,condition=task$candidate,seed=task$seed,
    source_row=held$comparison_source_row,participant=held$Anon.Student.Id,observed=held$CF..ansbin.,prediction=q)
  study_write_csv(predictions,file.path(output,'reports',paste0(task$run_id,'-predictions.csv')))
  study_write_csv(cbind(task,as.data.frame(fast_study_parameters(fit)),maximum_replay_probability_difference=difference),
    file.path(output,'reports',paste0(task$run_id,'-parameters.csv')))
  writeLines('PASS restored fitted model, exact data roles, finite parameters and saved-weight held-out replay.',
    file.path(output,'reports',paste0(task$run_id,'-verification.txt')))
  cat('DONE',task$run_id,'validation log loss',val[['log_loss']],'\n');flush.console()
}
fast_study_report <- function(output,p,action) {
  stopifnot(length(list.files(file.path(output,'models'),pattern='[.]rds$'))==p$total_fits)
  d <- readRDS(file.path(output,'data/prepared_data.rds'));records <- predictions <- list()
  for(i in seq_len(nrow(p$tasks))) {
    task <- p$tasks[i,,drop=FALSE];id <- task$run_id
    saved <- readRDS(file.path(output,'models',paste0(id,'.rds')));fit <- saved$fit
    s <- p$settings[[task$candidate]];s$seed <- as.integer(task$seed)
    roles <- readRDS(file.path(output,'partitions',paste0(task$partition,'.rds')))
    stopifnot(identical(saved$settings,s),identical(fit$gru_settings,s),identical(saved$task,task),
      identical(saved$fingerprint,digest::digest(list(study=study_hash(file.path(output,'study.rds')),task=task,settings=s),algo='sha256')),
      identical(fit$fit_rows,which(roles==1L)),identical(fit$validation_rows,which(roles==2L)),
      identical(fit$evaluation_rows,which(roles==3L)),identical(fit$model$recipe$interc,FALSE),
      fit$parameter_count==p$parameter_counts[[task$candidate]],all(is.finite(unlist(fit$model$weights))),
      file.exists(file.path(output,'reports',paste0(id,'-verification.txt'))))
    recipe <- p$recipes[[task$candidate]]
    for(n in c('components','features','fixedpars','interc','maketimes','connectors'))
      stopifnot(identical(fit$model$recipe[[n]],recipe[[n]]))
    stopifnot(identical(as.character(fit$model$recipe$interacts),as.character(recipe$interacts)))
    history <- fit$training_history
    stopifnot(fit$selected_epoch==history$epoch[which.min(history$validation_loss)],
      abs(history$validation_loss[match(fit$selected_epoch,history$epoch)]-min(history$validation_loss))<1e-12)
    q <- read.csv(file.path(output,'reports',paste0(id,'-predictions.csv')))
    stopifnot(nrow(q)==sum(roles==3L),!anyNA(q),!anyDuplicated(q$source_row),
      identical(q$source_row,d$comparison_source_row[roles==3L]),
      all(q$participant==d$Anon.Student.Id[roles==3L]),all(q$observed==d$CF..ansbin.[roles==3L]),
      max(abs(q$prediction-fit$prediction[roles==3L]))<3e-6)
    phases <- lapply(1:3,function(k) {
      yy <- d$CF..ansbin.[roles==k];pp <- if(k==3L)q$prediction else fit$prediction[roles==k]
      metrics <- study_metrics(yy,pp)
      independent_auc <- as.numeric(pROC::auc(yy,pp,quiet=TRUE,direction='<'))
      independent_loss <- -mean(stats::dbinom(yy,1,pp,log=TRUE))
      stopifnot(abs(metrics[['AUC']]-independent_auc)<1e-12,abs(metrics[['log_loss']]-independent_loss)<1e-12)
      metrics[c('AUC','log_loss')]
    })
    values <- unlist(phases,use.names=FALSE)
    names(values) <- c('training_AUC','training_log_loss','validation_AUC','validation_log_loss','test_AUC','test_log_loss')
    parameters <- fast_study_parameters(fit)
    saved_parameters <- read.csv(file.path(output,'reports',paste0(id,'-parameters.csv')))
    for(n in names(parameters))stopifnot(isTRUE(all.equal(as.numeric(saved_parameters[[n]]),parameters[[n]],tolerance=1e-12)))
    stopifnot(saved_parameters$maximum_replay_probability_difference<3e-6)
    records[[id]] <- cbind(task,condition=task$candidate,PPES=grepl('_full$',task$candidate),decay=s$hidden_decay,as.data.frame(as.list(values)),as.data.frame(parameters),
      parameter_count=fit$parameter_count,selected_epoch=fit$selected_epoch,epochs_run=fit$trained_epochs,
      cap_hit=fit$trained_epochs==s$epochs,elapsed_seconds=saved$elapsed,
      maximum_replay_probability_difference=saved_parameters$maximum_replay_probability_difference)
    predictions[[id]] <- q
  }
  fits <- data.table::as.data.table(do.call(rbind,records));pred <- data.table::as.data.table(do.call(rbind,predictions))
  stopifnot(!anyDuplicated(pred[,.(condition,seed,source_row)]),
    all(pred[,.N,by=.(condition,seed)]$N==nrow(d)))
  pooled <- pred[,as.list(study_metrics(observed,prediction)[c('AUC','log_loss')]),by=.(condition,seed)]
  for(condition_name in names(p$settings))for(seed_value in p$seeds) {
    q <- pred[condition==condition_name & seed==seed_value];m <- pooled[condition==condition_name & seed==seed_value]
    stopifnot(abs(m$AUC-as.numeric(pROC::auc(q$observed,q$prediction,quiet=TRUE,direction='<')))<1e-12,
      abs(m$log_loss+mean(stats::dbinom(q$observed,1,q$prediction,log=TRUE)))<1e-12)
  }

  summary <- pooled[,.(heldout_AUC=mean(AUC),heldout_log_loss=mean(log_loss),
    heldout_AUC_seed_sd=stats::sd(AUC),heldout_log_loss_seed_sd=stats::sd(log_loss)),by=condition]
  summary <- summary[match(names(p$settings),condition)]
  gains <- lapply(c('full','no_ppes'),function(group) {
    x <- merge(as.data.frame(pooled[condition==paste0('stock_',group)]),
      as.data.frame(pooled[condition==paste0('decay_',group)]),by='seed',suffixes=c('_stock','_decay'))
    data.frame(feature_group=group,seed=x$seed,AUC_gain=x$AUC_decay-x$AUC_stock,
      log_loss_improvement=x$log_loss_stock-x$log_loss_decay)
  })
  gains <- do.call(rbind,gains)
  interaction <- merge(gains[gains$feature_group=='full',],gains[gains$feature_group=='no_ppes',],
    by='seed',suffixes=c('_full','_no_ppes'))
  interaction <- data.frame(seed=interaction$seed,
    AUC_gain_interaction=interaction$AUC_gain_no_ppes-interaction$AUC_gain_full,
    log_loss_improvement_interaction=interaction$log_loss_improvement_no_ppes-interaction$log_loss_improvement_full)
  mean_gains <- aggregate(cbind(AUC_gain,log_loss_improvement)~feature_group,gains,mean)
  mean_interaction <- data.frame(AUC_gain_interaction=mean(interaction$AUC_gain_interaction),
    log_loss_improvement_interaction=mean(interaction$log_loss_improvement_interaction))
  parameter_names <- names(fast_study_parameters(readRDS(file.path(output,'models',paste0(p$tasks$run_id[1L],'.rds')))$fit))
  tables <- list(fits=fits,parameters=fits[decay==TRUE,c('partition','condition','seed',parameter_names),with=FALSE],
    pooled_by_seed=pooled,summary=summary,decay_gains_by_seed=gains,mean_decay_gains=mean_gains,
    interaction_by_seed=interaction,mean_interaction=mean_interaction)
  for(n in names(tables)) {
    path <- file.path(output,'reports',paste0(gsub('_','-',n),'.csv'))
    if(action=='report')study_write_csv(tables[[n]],path) else
      stopifnot(isTRUE(all.equal(as.data.frame(data.table::fread(path)),as.data.frame(tables[[n]]),tolerance=1e-12,check.attributes=FALSE)))
  }
  if(action=='report') {
    rows <- vapply(seq_len(nrow(summary)),function(i)with(as.list(summary[i]),sprintf('| %s | %.9f | %.9f |',
      condition,heldout_AUC,heldout_log_loss)),character(1))
    parameter_rows <- vapply(seq_len(nrow(tables$parameters)),function(i)with(as.list(tables$parameters[i]),
      sprintf('| %s | %s | %d | %.9g | %.9g | %.9g | %.9g | %.9g |',condition,partition,seed,raw_a,raw_b,raw_c,baseline_hidden_half_life_days,DK_half_life_days)),character(1))
    writeLines(c('# Cloze: compiled hidden decay and PPES factorial comparison','',
      'Sixty fresh fits: stock/decay crossed with PPES present/absent, five unchanged whole-participant outer folds and seeds 1-3. Checkpoints minimize inner-validation log loss.','',
      '| Condition | Held-out AUC | Held-out log loss |','|---|---:|---:|',rows,'',
      'Held-out predictions are replayed from saved weights, pooled across five folds within seed, then metrics are averaged across three seeds. All 180 phase metric pairs and 12 pooled pairs are independently checked.','',
      'Decay gains and the difference of gains (no PPES minus full) appear in mean-decay-gains.csv and mean-interaction.csv. A positive AUC gain or log-loss improvement is favorable. A positive interaction means removing PPES increased decay benefit.','',
      'Rate = a exp(b faded-DK), a >= 0; all three learned scalars start at zero. Baseline hidden half-life = log(2)/a, infinite at a=0. DK half-life = log(2)/softplus(c). Both are days. b has no standalone half-life. These are fitted estimates, not guaranteed global optima.','',
      '| Condition | Fold | Seed | a | b | c | Hidden half-life (days, DK=0) | DK half-life (days) |',
      '|---|---|---:|---:|---:|---:|---:|---:|',parameter_rows,'',
      'fits.csv contains all fold/seed AUC and log loss for training, validation and held-out testing, parameters, half-lives, selected epochs, times and replay discrepancies.','',
      'PPES removal deletes its actual predictor and four fixed nonlinear constants. Both logitdec terms retain their original constants. Numeric input count is six with PPES and five without it. Shared controls, encoders, retrieval, widths, optimizer, dropout, 100-epoch cap, patience 10 and participant assignments are unchanged. Decay adds three scalars: 43,073/43,076 parameters with PPES, 43,041/43,044 without. Chronological training consumes dropout randomness differently from fused stock execution.','',
      'Exploratory fixed-budget comparison on previously inspected Cloze data. No held-out selection, historical-result edits or central release promotion.'),file.path(output,'reports/RESULTS.md'))
    jsonlite::write_json(list(complete=TRUE,fits=p$total_fits,learned_parameter_triplets=30L,
      phase_metric_pairs_independently_checked=180L,pooled_metric_pairs_independently_checked=12L,
      maximum_replay_probability_difference=max(fits$maximum_replay_probability_difference),
      source_protocol_sha256=study_hash(file.path(output,'study.rds'))),file.path(output,'reports/verification.json'),auto_unbox=TRUE,pretty=TRUE)
  }
  print(summary);print(mean_gains);print(mean_interaction);invisible(tables)
}
