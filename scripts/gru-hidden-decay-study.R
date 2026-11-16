# Reporting and verification for the frozen stock-versus-hidden-decay experiment.
# Network construction and all decay mathematics remain in the maintained package.
hidden_study_replay <- function(fit,d) {
  ns <- asNamespace('LKT');s <- get('lkt_sequence_model',ns)(fit)$settings
  cleanup <- get('lkt_gru_runtime',ns)(s);on.exit(cleanup())
  ord <- get('lkt_gru_order',ns)(d,s);d <- d[ord,,drop=FALSE]
  encoded <- get('lkt_gru_encode',ns)(d,fit$model$preprocessing,fit$model$recipe,s)
  get('lkt_gru_replay',ns)(LKTGRURuntime(fit)$network,encoded,d,s)[order(ord)]
}
hidden_study_parameters <- function(fit) {
  values <- rep(NA_real_,7L)
  names(values) <- c('raw_a','raw_b','raw_c','baseline_hidden_rate_per_day',
    'DK_fading_rate_per_day','baseline_hidden_half_life_days','DK_half_life_days')
  if(fit$gru_settings$hidden_decay) {
    raw <- vapply(paste0('hidden_decay.',c('a','b','c')),function(n) {
      x <- fit$model$weights[[n]];stopifnot(is.numeric(x),length(x)==1L,is.finite(x));as.numeric(x)
    },numeric(1))
    softplus <- function(x)pmax(x,0)+log1p(exp(-abs(x)))
    rates <- softplus(raw[c(1L,3L)])
    stopifnot(all(is.finite(rates)),all(rates>0))
    values[] <- c(raw,rates,log(2)/rates)
  }
  as.list(values)
}
hidden_study_fit <- function(task,s,output,p,d) {
  s$seed <- as.integer(task$seed);s <- do.call(GRUModel,s)$settings
  path <- file.path(output,'models',paste0(task$run_id,'.rds'))
  fingerprint <- digest::digest(list(study=study_hash(file.path(output,'study.rds')),task=task,settings=s),algo='sha256')
  if(!file.exists(path)) {
    cat('START',task$run_id,'\n');flush.console()
    elapsed <- system.time(fit <- LKT(d,usefolds=1,
      components=p$baseline$components,features=p$baseline$features,fixedpars=p$baseline$fixedpars,
      interc=p$baseline$interc,model=do.call(GRUModel,s),verbose=TRUE))[['elapsed']]
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
  held <- d[d$fold==3L,,drop=FALSE];q <- hidden_study_replay(fit,held)
  difference <- max(abs(q-fit$prediction[d$fold==3L]));stopifnot(difference<3e-6)
  predictions <- data.frame(partition=task$partition,condition=task$candidate,seed=task$seed,
    source_row=held$comparison_source_row,participant=held$Anon.Student.Id,observed=held$CF..ansbin.,prediction=q)
  study_write_csv(predictions,file.path(output,'reports',paste0(task$run_id,'-predictions.csv')))
  study_write_csv(cbind(task,as.data.frame(hidden_study_parameters(fit)),maximum_replay_probability_difference=difference),
    file.path(output,'reports',paste0(task$run_id,'-parameters.csv')))
  writeLines('PASS restored fitted model, exact data roles, finite parameters and saved-weight held-out replay.',
    file.path(output,'reports',paste0(task$run_id,'-verification.txt')))
  cat('DONE',task$run_id,'validation log loss',val[['log_loss']],'\n');flush.console()
}
hidden_study_report <- function(output,p,action) {
  stopifnot(length(list.files(file.path(output,'models'),pattern='[.]rds$'))==30L)
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
    parameters <- hidden_study_parameters(fit)
    saved_parameters <- read.csv(file.path(output,'reports',paste0(id,'-parameters.csv')))
    for(n in names(parameters))stopifnot(isTRUE(all.equal(as.numeric(saved_parameters[[n]]),parameters[[n]],tolerance=1e-12)))
    stopifnot(saved_parameters$maximum_replay_probability_difference<3e-6)
    records[[id]] <- cbind(task,condition=task$candidate,as.data.frame(as.list(values)),as.data.frame(parameters),
      parameter_count=fit$parameter_count,selected_epoch=fit$selected_epoch,epochs_run=fit$trained_epochs,
      cap_hit=fit$trained_epochs==s$epochs,elapsed_seconds=saved$elapsed,
      maximum_replay_probability_difference=saved_parameters$maximum_replay_probability_difference)
    predictions[[id]] <- q
  }
  fits <- data.table::as.data.table(do.call(rbind,records));pred <- data.table::as.data.table(do.call(rbind,predictions))
  stopifnot(!anyDuplicated(pred[,.(condition,seed,source_row)]),
    all(pred[,.N,by=.(condition,seed)]$N==nrow(d)))
  pooled <- pred[,as.list(study_metrics(observed,prediction)[c('AUC','log_loss')]),by=.(condition,seed)]
  for(condition_name in c('stock','hidden_decay'))for(seed_value in p$seeds) {
    q <- pred[condition==condition_name & seed==seed_value];m <- pooled[condition==condition_name & seed==seed_value]
    stopifnot(abs(m$AUC-as.numeric(pROC::auc(q$observed,q$prediction,quiet=TRUE,direction='<')))<1e-12,
      abs(m$log_loss+mean(stats::dbinom(q$observed,1,q$prediction,log=TRUE)))<1e-12)
  }
  summary <- pooled[,.(heldout_AUC=mean(AUC),heldout_log_loss=mean(log_loss),
    heldout_AUC_seed_sd=stats::sd(AUC),heldout_log_loss_seed_sd=stats::sd(log_loss)),by=condition]
  summary <- summary[match(c('stock','hidden_decay'),condition)]
  for(n in c('heldout_AUC','heldout_log_loss'))summary[[paste0(n,'_change_vs_stock')]] <- summary[[n]]-summary[[n]][1L]
  paired <- merge(as.data.frame(pooled[condition=='stock']),as.data.frame(pooled[condition=='hidden_decay']),
    by='seed',suffixes=c('_stock','_hidden_decay'))
  paired$AUC_change <- paired$AUC_hidden_decay-paired$AUC_stock
  paired$log_loss_change <- paired$log_loss_hidden_decay-paired$log_loss_stock
  tables <- list(fits=fits,parameters=fits[condition=='hidden_decay',c('partition','seed',names(hidden_study_parameters(readRDS(file.path(output,'models',paste0(p$tasks$run_id[1L],'.rds')))$fit))),with=FALSE],
    pooled_by_seed=pooled,summary=summary,paired_seed_changes=paired)
  for(n in names(tables)) {
    path <- file.path(output,'reports',paste0(gsub('_','-',n),'.csv'))
    if(action=='report')study_write_csv(tables[[n]],path) else
      stopifnot(isTRUE(all.equal(as.data.frame(data.table::fread(path)),as.data.frame(tables[[n]]),tolerance=1e-12,check.attributes=FALSE)))
  }
  if(action=='report') {
    rows <- vapply(seq_len(nrow(summary)),function(i)with(as.list(summary[i]),sprintf('| %s | %.8f | %.8f | %+.8f | %+.8f |',
      condition,heldout_AUC,heldout_log_loss,heldout_AUC_change_vs_stock,heldout_log_loss_change_vs_stock)),character(1))
    writeLines(c('# Cloze: stock GRU versus hidden-unit decay','',
      'Thirty fresh fits: two conditions, five unchanged whole-participant outer folds and seeds 1-3. Checkpoints minimize inner-validation log loss; held-out outcomes select neither epochs nor parameters.','',
      '| Condition | Held-out AUC | Held-out log loss | AUC change | Log-loss change |',
      '|---|---:|---:|---:|---:|',rows,'',
      'Held-out probabilities are independently replayed from saved weights, pooled over the five folds within seed, then metrics are averaged across three seeds. All 90 phase AUC and log-loss calculations are independently checked.','',
      'fits.csv contains every fold, condition and seed, training/validation/test metrics, selected epochs, caps, times, raw a/b/c and rates/half-lives. parameters.csv contains the fifteen learned decay triplets.','',
      'Baseline hidden half-life is log(2)/softplus(a), evaluated at DK=0. DK half-life is log(2)/softplus(c). Both are in days. b has no standalone half-life: the current hidden-unit half-life depends on its faded DK through log(2)/softplus(a+b*DK).','',
      'The current six-input recipe, fixed LKT nonlinear constants, retrieval, widths, embeddings, optimizer, dropout probabilities, 100-epoch cap, patience 10 and all remaining training controls are matched. The decay model adds three scalars (43,076 versus 43,073 parameters). Enabled chronological execution consumes dropout randomness differently from the fused stock path.','',
      'This is an exploratory fixed-budget comparison on previously inspected Cloze data. No held-out model selection, feature tuning, historical-result edits or central release promotion.'),file.path(output,'reports/RESULTS.md'))
    jsonlite::write_json(list(complete=TRUE,fits=30L,learned_parameter_triplets=15L,phase_metric_pairs_independently_checked=90L,
      maximum_replay_probability_difference=max(fits$maximum_replay_probability_difference),
      stock_parameter_count=43073L,hidden_decay_parameter_count=43076L,
      source_protocol_sha256=study_hash(file.path(output,'study.rds'))),file.path(output,'reports/verification.json'),auto_unbox=TRUE,pretty=TRUE)
  }
  print(summary);invisible(tables)
}
