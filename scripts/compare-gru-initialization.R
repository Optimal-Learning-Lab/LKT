# Compare function-preserving zero initialization and trained expansion, with retrieval off/on.
# All neural implementation comes from the centrally selected LKT package.
# Args: ROOT OUTPUT plan|verify|fit|report|check PARTITION [PREPARED_MODEL REFERENCE]
args <- commandArgs(TRUE);stopifnot(length(args) %in% c(4L,6L))
root <- normalizePath(args[1L],winslash='/',mustWork=TRUE)
output <- args[2L];action <- args[3L];partition <- args[4L]
stopifnot(action %in% c('plan','verify','fit','report','check'))
source(file.path(root,'scripts/load-current-lkt.R'))
suppressPackageStartupMessages(library(data.table))
source(file.path(root,'scripts/gru-encoder-study.R'))
script_names <- c('scripts/compare-gru-initialization.R','scripts/gru-encoder-study.R',
  'scripts/load-current-lkt.R','workflows/simulation/shared/lkt_release.R')
script_files <- c(file.path(root,head(script_names,-1L)),
  file.path(dirname(attr(lkt_release,'file')),'lkt_release.R'))
script_hashes <- setNames(vapply(script_files,study_hash,character(1)),script_names)
release_identity <- lkt_release[c('lkt_version','lkt_source_archive','lkt_source_sha256','lkt_manifest_sha256')]
stopifnot(study_hash(attr(lkt_release,'archive'))==lkt_release[['lkt_source_sha256']])

if(action=='plan') {
  stopifnot(length(args)==6L,!dir.exists(output))
  prepared <- args[5L];reference <- args[6L]
  d <- readRDS(file.path(prepared,'prepared_data.rds'))
  config <- readRDS(file.path(prepared,'model-specification.rds'))
  settings <- do.call(GRUModel,config$gru_options)$settings
  stopifnot(settings$feature_encoder=='mlp',settings$inference_mode=='none',
    settings$num_layers==3L,settings$hidden_size==32L,settings$head_hidden_sizes==128L,
    identical(settings$encoder_hidden_sizes,32L),settings$encoder_output_size==16L,
    identical(settings$time_feature_cols,c('gru_item_age_seconds','gru_mean_prior_spacing_seconds')),
    identical(settings$time_missing_cols,'gru_mean_prior_spacing_seconds'),nrow(d)==55122L,
    length(unique(d$Anon.Student.Id))==478L,sum(is.na(d$gru_mean_prior_spacing_seconds))==32991L)
  call <- getFromNamespace('lkt_config_arguments','LKT')(config)
  recipe <- call[c('components','features','fixedpars','interc','interacts','connectors','maketimes')]
  stopifnot(isTRUE(recipe$interc))
  partitions <- c('development',paste0('cv',1:5))
  dir.create(output,recursive=TRUE)
  for(folder in c('data','partitions','models','validation','training','reports','initial'))dir.create(file.path(output,folder))
  stopifnot(file.copy(file.path(prepared,'prepared_data.rds'),file.path(output,'data/prepared_data.rds')))
  reference_protocol <- readRDS(file.path(reference,'study.rds'))
  stopifnot(study_hash(file.path(prepared,'prepared_data.rds'))==reference_protocol$data_sha256,
    identical(reference_protocol$seeds,1:3),identical(reference_protocol$partitions,partitions))
  # Reuse the exact data and participant assignments. Four arms start from
  # ordinary random weights; two expand the matching trained control below.
  for(name in partitions) {
    path <- file.path(reference,'partitions',paste0(name,'.rds'))
    stopifnot(study_hash(path)==reference_protocol$partition_hashes[[name]],
      file.copy(path,file.path(output,'partitions',paste0(name,'.rds'))))
    fold <- readRDS(path)
    groups <- unique(data.frame(participant=d$Anon.Student.Id,fold=fold))
    stopifnot(length(fold)==nrow(d),all(fold %in% 1:3),!anyDuplicated(groups$participant))
    if(name=='development')stopifnot(identical(as.integer(d$fold),fold))
  }
  # Export only the prior fold-matched control fits as explicit initialization
  # files. Their data, partitions and fitting masks must match this experiment.
  initial_records <- list()
  for(part in partitions)for(control in c('standard','retrieval'))for(seed in 1:3) {
    id <- paste(part,control,paste0('seed',seed),sep='__')
    original <- file.path(reference,'models',paste0(id,'.rds'))
    saved <- readRDS(original);fit <- saved$fit
    expected <- do.call(GRUModel,reference_protocol$settings[[control]])$settings;expected$seed <- as.integer(seed)
    stopifnot(identical(do.call(GRUModel,fit$gru_settings)$settings,expected),
      !expected$input_to_all_layers,expected$retrieval_heads==as.integer(control=='retrieval'))
    fold <- readRDS(file.path(output,'partitions',paste0(part,'.rds')))
    stopifnot(identical(fit$fit_rows,which(fold==1)),identical(fit$validation_rows,which(fold==2)),
      identical(fit$evaluation_rows,which(fold==3)),length(fit$prediction)==nrow(d))
    destination <- file.path(output,'initial',paste0(id,'.rds'))
    study_write_rds(fit,destination)
    initial_records[[id]] <- data.frame(run_id=id,partition=part,control=control,seed=seed,
      source_checkpoint_sha256=study_hash(original),export_sha256=study_hash(destination))
  }
  initial_records <- do.call(rbind,initial_records)
  study_write_csv(initial_records,file.path(output,'initial/sources.csv'))
  design <- data.frame(condition=c('standard','retrieval','zero_plain','zero_retrieval','warm_plain','warm_retrieval'),
    input_to_all_layers=c(FALSE,FALSE,TRUE,TRUE,TRUE,TRUE),
    retrieval_heads=c(0L,1L,0L,1L,0L,1L),warm_start=c(FALSE,FALSE,FALSE,FALSE,TRUE,TRUE))
  new_settings <- setNames(lapply(seq_len(nrow(design)),function(i) {
    s <- settings;s$input_to_all_layers <- design$input_to_all_layers[i]
    s$additional_input_init <- if(s$input_to_all_layers)'zero' else 'random';s$initial_model_file <- NULL
    s$retrieval_heads <- design$retrieval_heads[i];s$retrieval_size <- 32L
    s$retrieval_window <- 100L;s$retrieval_dropout <- 0;s$retrieval_fusion <- 'replace'
    s$retrieval_decay <- 'none';s$retrieval_share_qk <- TRUE;s$retrieval_bias <- TRUE
    do.call(GRUModel,s)$settings
  }),design$condition)
  for(s in new_settings) {
    fixed <- setdiff(names(s),c('input_to_all_layers','retrieval_heads','additional_input_init','initial_model_file'))
    stopifnot(identical(s[fixed],new_settings$standard[fixed]))
  }
  p <- list(schema=2L,release=release_identity,release_file_sha256=study_hash(attr(lkt_release,'file')),
    script_hashes=script_hashes,data_sha256=study_hash(file.path(output,'data/prepared_data.rds')),
    partition_hashes=setNames(vapply(partitions,function(n)study_hash(file.path(output,'partitions',paste0(n,'.rds'))),character(1)),partitions),
    settings=new_settings,factorial=design,baseline=recipe,seeds=1:3,partitions=partitions,total_fits=108L,
    initial_hashes=setNames(vapply(list.files(file.path(output,'initial'),full.names=TRUE),study_hash,character(1)),list.files(file.path(output,'initial'))),
    parameter_counts=c(standard=41041L,retrieval=46257L,zero_plain=56401L,zero_retrieval=61617L,warm_plain=56401L,warm_retrieval=61617L),
    comparisons=list(c('zero_plain','standard'),c('zero_retrieval','retrieval'),c('warm_plain','standard'),c('warm_retrieval','retrieval'),c('warm_plain','zero_plain'),c('warm_retrieval','zero_retrieval')),
    design='Six fresh arms: ordinary controls, zero-added-input initialization from scratch, and trained-model zero expansion; each with retrieval off/on. The first layer is retained, never zeroed.',
    selection='No hyperparameter search. Early stopping and restored epoch use each fit participant validation loss.',
    caveats=c('Architecture was chosen on previously inspected development data. These folds assess a fixed architecture, not nested architecture selection.',
      'Engineered-feature nonlinear parameters are fixed inherited constants, not reestimated per fold.',
      'Studies contribute to item-age and prior-spacing features, not to scored targets or recurrent answer/retrieval updates.',
      'Individual-model metrics are averaged across seeds; predictions are not ensembled.',
      'Retrieval uses one 32-value head over the previous 100 completed scored events, with decay disabled. Direct input links send the same 80-value predictor-and-observed-answer vector to every GRU layer; extra matrices add 15,360 weights.',
      'Retrieval replaces the direct memory input to the prediction head; it does not feed back into the GRU.',
      'Warm starts copy fold-matched fitted controls, zero only new blocks, reset optimizer state, and include the unmodified starting weights in validation selection. Further training is additional to the source fit.'))
  study_write_rds(p,file.path(output,'study.rds'))
  writeLines(capture.output(dput(p)),file.path(output,'study.txt'))
  cat('FROZEN: 108 fresh fits; six initialization conditions, three seeds, six participant partitions.\n')
  quit(save='no')
}

p <- readRDS(file.path(output,'study.rds'))
stopifnot(identical(p$schema,2L),identical(p$release,release_identity),identical(p$script_hashes,script_hashes),
  study_hash(attr(lkt_release,'file'))==p$release_file_sha256,
  study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256)
for(name in names(p$initial_hashes))stopifnot(study_hash(file.path(output,'initial',name))==p$initial_hashes[[name]])
for(name in p$partitions)stopifnot(study_hash(file.path(output,'partitions',paste0(name,'.rds')))==p$partition_hashes[[name]])
if(!is.null(p$reference))for(name in names(p$reference$hashes))stopifnot(
  study_hash(file.path(output,p$reference$directory,name))==p$reference$hashes[[name]])
tasks_for <- function(part,protocol=p)do.call(rbind,lapply(names(protocol$settings),function(condition)do.call(rbind,lapply(protocol$seeds,function(seed)
  data.frame(run_id=paste(part,condition,paste0('seed',seed),sep='__'),partition=part,
    family='augmented',stage=condition,candidate=condition,seed=seed)))))

settings_for <- function(task,protocol=p) {
  s <- protocol$settings[[task$candidate]];s$seed <- as.integer(task$seed)
  if(startsWith(task$candidate,'warm_')) {
    control <- if(s$retrieval_heads)'retrieval' else 'standard'
    s$initial_model_file <- file.path('initial',paste0(paste(task$partition,control,paste0('seed',task$seed),sep='__'),'.rds'))
  }
  s
}

if(action=='verify') {
  stopifnot(torch::torch_is_installed(),as.character(packageVersion('torch'))=='0.17.0')
  manifest <- jsonlite::fromJSON(system.file('research-source-manifest.json',package='LKT'))
  stopifnot(manifest$version==p$release[['lkt_version']],
    study_hash(system.file('research-source-manifest.json',package='LKT'))==p$release[['lkt_manifest_sha256']])
  # Validate real data on the deployed runtime, including its missing-value mask,
  # without allowing a smoke fit to substitute for any of the 108 research fits.
  d <- readRDS(file.path(output,'data/prepared_data.rds'))
  for(condition in names(p$settings)) {
    prep <- getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1,p$baseline,p$settings[[condition]])
    net <- getFromNamespace('lkt_gru_network','LKT')(prep,p$settings[[condition]])
    stopifnot(sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))==p$parameter_counts[[condition]])
    stopifnot(net$time_encoder[[1]][[1]]$weight$size(2)==
      1L+length(p$settings[[condition]]$time_feature_cols)+length(p$settings[[condition]]$time_missing_cols))
  }
  cat('PASS release, protocol hashes, existing participant folds and all six deployed configurations.\n')
}

if(action=='fit') {
  stopifnot(partition %in% p$partitions)
  d <- readRDS(file.path(output,'data/prepared_data.rds'))
  d$fold <- readRDS(file.path(output,'partitions',paste0(partition,'.rds')))
  tasks <- tasks_for(partition)
  setwd(output)
  workers <- as.integer(Sys.getenv('LKT_STUDY_WORKERS','1'));stopifnot(workers>=1L,workers<=6L)
  if(workers==1L) {
    for(i in seq_len(nrow(tasks)))study_fit_one(tasks[i,],settings_for(tasks[i,,drop=FALSE]),output,p,d)
  } else {
    cluster <- parallel::makePSOCKcluster(workers,outfile=file.path(output,paste0(partition,'-workers.log')))
    tryCatch({
      parallel::clusterCall(cluster,function(root,release_file,output) {
        setwd(output)
        assign('root',root,envir=.GlobalEnv);Sys.setenv(LKT_RELEASE_FILE=release_file)
        sys.source(file.path(root,'scripts/load-current-lkt.R'),envir=.GlobalEnv)
        sys.source(file.path(root,'scripts/gru-encoder-study.R'),envir=.GlobalEnv);NULL
      },root,attr(lkt_release,'file'),output)
      parallel::clusterExport(cluster,c('p','d','tasks','output','settings_for'))
      parallel::parLapplyLB(cluster,seq_len(nrow(tasks)),function(i)
        study_fit_one(tasks[i,,drop=FALSE],settings_for(tasks[i,,drop=FALSE]),output,p,d))
    },finally=parallel::stopCluster(cluster))
  }
  # Replay every test history from saved weights, independently of fit-time scores.
  # Current answers may update later memory but never their own predictions.
  scores <- predictions <- list()
  for(i in seq_len(nrow(tasks))) {
    task <- tasks[i,];fit <- readRDS(file.path(output,'models',paste0(task$run_id,'.rds')))$fit
    test <- d[d$fold==3,,drop=FALSE];q <- study_replay(fit,test)
    stopifnot(max(abs(q-fit$prediction[d$fold==3]))<3e-6)
    scores[[i]] <- cbind(task,as.data.frame(as.list(study_metrics(test$CF..ansbin.,q))),
      parameter_count=fit$parameter_count,selected_epoch=fit$selected_epoch,epochs_run=sum(fit$training_history$epoch>0L))
    predictions[[i]] <- data.frame(partition=partition,condition=task$candidate,seed=task$seed,
      source_row=test$comparison_source_row,participant=test$Anon.Student.Id,observed=test$CF..ansbin.,prediction=q)
  }
  study_write_csv(do.call(rbind,scores),file.path(output,'reports',paste0(partition,'-test.csv')))
  study_write_csv(do.call(rbind,predictions),file.path(output,'reports',paste0(partition,'-predictions.csv')))
  writeLines('PASS all eighteen settings/masks, fresh fits and held-out history weight replay.',file.path(output,'reports',paste0(partition,'-verification.txt')))
}

if(action %in% c('report','check')) {
  # Read each checkpoint and recompute validation metrics and mask contracts before
  # aggregating out-of-fold predictions. No fold or seed is selected on test results.
  sources <- list(fresh=list(directory=output,protocol=p))
  all_predictions <- all_scores <- list();fit_auc <- initialization_checks <- list()
  for(source_name in names(sources)) {
    directory <- sources[[source_name]]$directory;protocol <- sources[[source_name]]$protocol
    stopifnot(identical(protocol$data_sha256,p$data_sha256),identical(protocol$partition_hashes,p$partition_hashes),
      identical(protocol$release,p$release),length(list.files(file.path(directory,'models'),pattern='[.]rds$'))==protocol$total_fits)
    for(part in p$partitions) {
      stopifnot(file.exists(file.path(directory,'reports',paste0(part,'-verification.txt'))))
      d <- readRDS(file.path(directory,'data/prepared_data.rds'))
      fold <- readRDS(file.path(directory,'partitions',paste0(part,'.rds')))
      d$fold <- fold
      tasks <- tasks_for(part,protocol)
      part_predictions <- fread(file.path(directory,'reports',paste0(part,'-predictions.csv')))
      for(i in seq_len(nrow(tasks))) {
      task <- tasks[i,];saved <- readRDS(file.path(directory,'models',paste0(task$run_id,'.rds')));fit <- saved$fit
      settings <- settings_for(task,protocol)
      stopifnot(identical(settings,fit$gru_settings),identical(saved$settings,settings),
        identical(fit$fit_rows,which(fold==1)),identical(fit$validation_rows,which(fold==2)),
        identical(fit$evaluation_rows,which(fold==3)),
        identical(saved$fingerprint,digest::digest(list(study=study_hash(file.path(directory,'study.rds')),task=saved$task,settings=settings),algo='sha256')),
        fit$parameter_count==p$parameter_counts[[task$candidate]])
      record <- read.csv(file.path(directory,'validation',paste0(task$run_id,'.csv')))
      metrics <- study_metrics(d$CF..ansbin.[fold==2],fit$prediction[fold==2])
      for(n in names(metrics))stopifnot(abs(record[[n]]-metrics[[n]])<1e-12)
      # Evaluate all phases from the restored model with dropout disabled. This
      # makes training AUC directly comparable with validation and held-out AUC.
      for(phase in 1:3)fit_auc[[paste(task$run_id,phase)]] <- data.frame(partition=part,
        condition=task$candidate,seed=task$seed,phase=c('training','validation','test')[phase],
        AUC=unname(study_metrics(d$CF..ansbin.[fold==phase],fit$prediction[fold==phase])[['AUC']]))
      if(startsWith(task$candidate,'warm_') || task$candidate %in% c('standard','retrieval')) {
        control <- if(settings$retrieval_heads)'retrieval' else 'standard'
        initial_file <- file.path(directory,'initial',paste0(paste(part,control,paste0('seed',task$seed),sep='__'),'.rds'))
        initial_fit <- readRDS(initial_file)
        warm <- startsWith(task$candidate,'warm_')
        delta <- max(abs((if(warm)fit$initialization$predictions else fit$prediction)-initial_fit$prediction))
        stopifnot(delta<3e-6)
        if(warm) {
          history <- fit$training_history
          initial_auc <- study_metrics(d$CF..ansbin.[fold==1],fit$initialization$predictions[fold==1])[['AUC']]
          stopifnot(history$epoch[1L]==0L,abs(history$training_auc[1L]-initial_auc)<1e-12,
            identical(fit$initialization$source_md5,unname(tools::md5sum(initial_file))),
            fit$initialization$source_selected_epoch==initial_fit$selected_epoch,
            fit$selected_epoch==history$epoch[which.min(history$validation_loss)],
            abs(metrics[['log_loss']]-min(history$validation_loss))<3e-6,
            sum(history$epoch>0L)==fit$trained_epochs)
        }
        initialization_checks[[task$run_id]] <- data.frame(run_id=task$run_id,
          check=if(warm)'trained expansion before optimization' else 'fresh control reproduces prior fit',
          maximum_probability_difference=delta,selected_epoch=fit$selected_epoch)
      }
      rows <- part_predictions[condition==task$candidate & seed==task$seed]
      stopifnot(nrow(rows)==sum(fold==3),!anyDuplicated(rows$source_row),
        setequal(rows$source_row,d$comparison_source_row[fold==3]),
        max(abs(rows$prediction-fit$prediction[match(rows$source_row,d$comparison_source_row)]))<3e-6)

      }
    }
    all_predictions[[source_name]] <- rbindlist(lapply(p$partitions,function(n)fread(file.path(directory,'reports',paste0(n,'-predictions.csv')))))
    all_scores[[source_name]] <- rbindlist(lapply(p$partitions,function(n)fread(file.path(directory,'reports',paste0(n,'-test.csv')))))
  }
  pred <- rbindlist(all_predictions);scores <- rbindlist(all_scores)
  fit_auc <- rbindlist(fit_auc);initialization_checks <- rbindlist(initialization_checks)
  fit_auc_summary <- fit_auc[,.(AUC=mean(AUC)),
    by=.(evaluation=ifelse(partition=='development','development','cross_validation'),condition,phase)]
  conditions <- unique(pred$condition)
  original <- readRDS(file.path(output,'data/prepared_data.rds'))
  cv <- pred[partition!='development']
  stopifnot(!anyDuplicated(cv[,.(condition,seed,source_row)]),
    all(cv[, .N,by=.(condition,seed)]$N==nrow(original)),
    all(pred$observed==original$CF..ansbin.[match(pred$source_row,original$comparison_source_row)]))
  for(part in p$partitions)for(condition_name in conditions)for(seed_value in p$seeds) {
    rows <- pred[partition==part & condition==condition_name & seed==seed_value]
    record <- scores[partition==part & candidate==condition_name & seed==seed_value]
    metrics <- study_metrics(rows$observed,rows$prediction)
    for(n in names(metrics))stopifnot(abs(record[[n]]-metrics[[n]])<1e-12)
  }
  pooled <- pred[,as.list(study_metrics(observed,prediction)),
    by=.(evaluation=ifelse(partition=='development','development','cross_validation'),condition,seed)]
  summary <- pooled[,.(AUC=mean(AUC),AUC_seed_sd=sd(AUC),log_loss=mean(log_loss),log_loss_seed_sd=sd(log_loss)),by=.(evaluation,condition)]
  display_order <- c('standard','retrieval','zero_plain','zero_retrieval','warm_plain','warm_retrieval')
  summary <- summary[order(match(evaluation,c('development','cross_validation')),match(condition,display_order))]
  folds <- scores[,.(AUC=mean(AUC),log_loss=mean(log_loss),mean_epoch=mean(selected_epoch)),by=.(partition,condition=candidate)]
  # Cluster the paired loss bootstrap by participant, retaining all their responses
  # and averaging losses across the three seeds. It conditions on the fitted models.
  loss <- pred[,.(loss=mean(-ifelse(observed==1,log(prediction),log1p(-prediction)))),
    by=.(evaluation=ifelse(partition=='development','development','cross_validation'),condition,participant,source_row)]
  differences <- list();set.seed(20261104)
  for(e in unique(summary$evaluation))for(pair in p$comparisons) {
    a <- loss[evaluation==e & condition==pair[1L]];b <- loss[evaluation==e & condition==pair[2L]]
    setorder(a,source_row);setorder(b,source_row);stopifnot(identical(a$source_row,b$source_row))
    totals <- data.table(participant=a$participant,delta=a$loss-b$loss)[,.(sum=sum(delta),n=.N),by=participant]
    boot <- replicate(2000L,{idx <- sample.int(nrow(totals),replace=TRUE);sum(totals$sum[idx])/sum(totals$n[idx])})
    comparison <- paste(pair,collapse=' minus ')
    differences[[paste(e,comparison)]] <- data.frame(evaluation=e,comparison=comparison,
      AUC_gain=summary[evaluation==e & condition==pair[1L],AUC]-summary[evaluation==e & condition==pair[2L],AUC],
      log_loss_change=mean(a$loss-b$loss),lower95=unname(quantile(boot,.025)),upper95=unname(quantile(boot,.975)))
  }
  differences <- rbindlist(differences)
  if(action=='report') {
    study_write_csv(as.data.frame(summary),file.path(output,'reports/summary.csv'))
    study_write_csv(as.data.frame(folds),file.path(output,'reports/fold-summary.csv'))
    study_write_csv(as.data.frame(pooled),file.path(output,'reports/metrics-by-seed.csv'))
    study_write_csv(as.data.frame(differences),file.path(output,'reports/paired-differences.csv'))
    study_write_csv(as.data.frame(fit_auc),file.path(output,'reports/fit-auc-by-partition.csv'))
    study_write_csv(as.data.frame(fit_auc_summary),file.path(output,'reports/fit-auc-summary.csv'))
    study_write_csv(as.data.frame(initialization_checks),file.path(output,'reports/initialization-verification.csv'))
    table <- function(x)paste0('<table border="1"><tr>',paste0('<th>',names(x),'</th>',collapse=''),'</tr>',
      paste(apply(as.data.frame(x),1,function(row)paste0('<tr>',paste0('<td>',row,'</td>',collapse=''),'</tr>')),collapse=''),'</table>')
    writeLines(paste0('<!doctype html><meta charset="utf-8"><title>GRU initialization comparison</title>',
      '<style>body{font:17px sans-serif;max-width:1100px;margin:40px auto}td,th{padding:8px}table{border-collapse:collapse}</style>',
      '<h1>GRU initialization comparison</h1><p>Augmented encoder model; three seeds; fixed architecture. AUC: higher is better.</p>',
      table(summary[,.(evaluation,condition,AUC,AUC_seed_sd)]),'<h2>Training and validation AUC</h2>',
      table(fit_auc_summary[phase!='test']),'<h2>Paired AUC changes</h2>',table(differences[,.(evaluation,comparison,AUC_gain)]),
      '<p>CV AUC pools out-of-fold predictions separately per seed, then averages metrics. Internal validation diagnostics are in the CSV records.</p>',
      '<h2>Per partition</h2>',table(folds[,.(partition,condition,AUC,mean_epoch)]),'<h2>Interpretation limits</h2><ul>',
      paste0('<li>',p$caveats,'</li>',collapse=''),'</ul>'),file.path(output,'reports/report.html'))
    writeLines(paste('PASS',nrow(scores),'checkpoint contracts and metrics; all held-out test histories replayed; every CV response scored exactly once per condition/seed.'),file.path(output,'reports/verification.txt'))
  } else {
    recorded <- fread(file.path(output,'reports/summary.csv'))
    stopifnot(isTRUE(all.equal(as.data.frame(recorded),as.data.frame(summary),tolerance=1e-12,check.attributes=FALSE)))
  }
  print(summary);print(differences)
}
