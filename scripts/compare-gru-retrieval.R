# Compare retrieval off, one head and two heads at the same total attention width.
# All neural implementation comes from the centrally selected LKT package.
# Args: ROOT OUTPUT plan|verify|fit|report|check PARTITION [PREPARED_MODEL REFERENCE]
args <- commandArgs(TRUE);stopifnot(length(args) %in% c(4L,6L))
root <- normalizePath(args[1L],winslash='/',mustWork=TRUE)
output <- args[2L];action <- args[3L];partition <- args[4L]
stopifnot(action %in% c('plan','verify','fit','report','check'))
source(file.path(root,'scripts/load-current-lkt.R'))
suppressPackageStartupMessages(library(data.table))
source(file.path(root,'scripts/gru-encoder-study.R'))
script_names <- c('scripts/compare-gru-retrieval.R','scripts/gru-encoder-study.R',
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
  for(folder in c('data','partitions','models','validation','training','reports'))dir.create(file.path(output,folder))
  stopifnot(file.copy(file.path(prepared,'prepared_data.rds'),file.path(output,'data/prepared_data.rds')))
  reference_protocol <- readRDS(file.path(reference,'study.rds'))
  stopifnot(study_hash(file.path(prepared,'prepared_data.rds'))==reference_protocol$data_sha256,
    identical(reference_protocol$seeds,1:3),identical(reference_protocol$partitions,partitions))
  # Reuse only the exact data and participant assignments. Historical models are
  # not imported: all three arms are trained fresh with the current release.
  for(name in partitions) {
    path <- file.path(reference,'partitions',paste0(name,'.rds'))
    stopifnot(study_hash(path)==reference_protocol$partition_hashes[[name]],
      file.copy(path,file.path(output,'partitions',paste0(name,'.rds'))))
    fold <- readRDS(path)
    groups <- unique(data.frame(participant=d$Anon.Student.Id,fold=fold))
    stopifnot(length(fold)==nrow(d),all(fold %in% 1:3),!anyDuplicated(groups$participant))
    if(name=='development')stopifnot(identical(as.integer(d$fold),fold))
  }
  # Fix total attention width at 32. Two heads divide those coordinates into
  # two 16-value views, keeping their projection parameter count equal to one head.
  new_settings <- lapply(0:2,function(heads) {
    s <- settings;s$retrieval_heads <- as.integer(heads);s$retrieval_size <- 32L
    s$retrieval_window <- 100L;s$retrieval_dropout <- 0;s$retrieval_fusion <- 'replace'
    s$retrieval_decay <- 'none';s$retrieval_share_qk <- TRUE;s$retrieval_bias <- TRUE
    do.call(GRUModel,s)$settings
  });names(new_settings) <- c('off','one_head','two_heads')
  # Apart from the number of heads, these settings must be identical. This
  # isolates the retrieval change from the already selected encoders and time inputs.
  for(s in new_settings)stopifnot(identical(s[names(s)!='retrieval_heads'],new_settings$off[names(s)!='retrieval_heads']))
  p <- list(schema=2L,release=release_identity,release_file_sha256=study_hash(attr(lkt_release,'file')),
    script_hashes=script_hashes,data_sha256=study_hash(file.path(output,'data/prepared_data.rds')),
    partition_hashes=setNames(vapply(partitions,function(n)study_hash(file.path(output,'partitions',paste0(n,'.rds'))),character(1)),partitions),
    settings=new_settings,baseline=recipe,seeds=1:3,partitions=partitions,total_fits=54L,
    parameter_counts=c(off=41041L,one_head=46257L,two_heads=46257L),
    comparisons=list(c('one_head','off'),c('two_heads','off'),c('two_heads','one_head')),
    design='Three fresh retrieval conditions on the augmented encoder GRU with all three time inputs; no imported fitted controls.',
    selection='No hyperparameter search. Early stopping and restored epoch use each fit participant validation loss.',
    caveats=c('Architecture was chosen on previously inspected development data. These folds assess a fixed architecture, not nested architecture selection.',
      'Engineered-feature nonlinear parameters are fixed inherited constants, not reestimated per fold.',
      'Studies contribute to item-age and prior-spacing features, not to scored targets or recurrent answer/retrieval updates.',
      'Individual-model metrics are averaged across seeds; predictions are not ensembled.',
      'Attention uses the previous 100 completed scored events; decay is disabled. One versus two heads keeps total width and parameter count fixed.',
      'Retrieval replaces the direct memory input to the prediction head; it does not feed back into the GRU.'))
  study_write_rds(p,file.path(output,'study.rds'))
  writeLines(capture.output(dput(p)),file.path(output,'study.txt'))
  cat('FROZEN: 54 fresh fits; three retrieval conditions, three seeds, six participant partitions.\n')
  quit(save='no')
}

p <- readRDS(file.path(output,'study.rds'))
stopifnot(identical(p$schema,2L),identical(p$release,release_identity),identical(p$script_hashes,script_hashes),
  study_hash(attr(lkt_release,'file'))==p$release_file_sha256,
  study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256)
for(name in p$partitions)stopifnot(study_hash(file.path(output,'partitions',paste0(name,'.rds')))==p$partition_hashes[[name]])
if(!is.null(p$reference))for(name in names(p$reference$hashes))stopifnot(
  study_hash(file.path(output,p$reference$directory,name))==p$reference$hashes[[name]])
tasks_for <- function(part,protocol=p)do.call(rbind,lapply(names(protocol$settings),function(condition)do.call(rbind,lapply(protocol$seeds,function(seed)
  data.frame(run_id=paste(part,condition,paste0('seed',seed),sep='__'),partition=part,
    family='augmented',stage=condition,candidate=condition,seed=seed)))))

if(action=='verify') {
  stopifnot(torch::torch_is_installed(),as.character(packageVersion('torch'))=='0.17.0')
  manifest <- jsonlite::fromJSON(system.file('research-source-manifest.json',package='LKT'))
  stopifnot(manifest$version==p$release[['lkt_version']],
    study_hash(system.file('research-source-manifest.json',package='LKT'))==p$release[['lkt_manifest_sha256']])
  # Validate real data on the deployed runtime, including its missing-value mask,
  # without allowing a smoke fit to substitute for any of the 54 research fits.
  d <- readRDS(file.path(output,'data/prepared_data.rds'))
  for(condition in names(p$settings)) {
    prep <- getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1,p$baseline,p$settings[[condition]])
    net <- getFromNamespace('lkt_gru_network','LKT')(prep,p$settings[[condition]])
    stopifnot(sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))==p$parameter_counts[[condition]])
    stopifnot(net$time_encoder[[1]][[1]]$weight$size(2)==
      1L+length(p$settings[[condition]]$time_feature_cols)+length(p$settings[[condition]]$time_missing_cols))
  }
  cat('PASS release, protocol hashes, existing participant folds and all three deployed retrieval configurations.\n')
}

if(action=='fit') {
  stopifnot(partition %in% p$partitions)
  d <- readRDS(file.path(output,'data/prepared_data.rds'))
  d$fold <- readRDS(file.path(output,'partitions',paste0(partition,'.rds')))
  tasks <- tasks_for(partition)
  workers <- as.integer(Sys.getenv('LKT_STUDY_WORKERS','1'));stopifnot(workers>=1L,workers<=6L)
  if(workers==1L) {
    for(i in seq_len(nrow(tasks)))study_fit_one(tasks[i,],p$settings[[tasks$candidate[i]]],output,p,d)
  } else {
    cluster <- parallel::makePSOCKcluster(workers,outfile=file.path(output,paste0(partition,'-workers.log')))
    tryCatch({
      parallel::clusterCall(cluster,function(root,release_file) {
        assign('root',root,envir=.GlobalEnv);Sys.setenv(LKT_RELEASE_FILE=release_file)
        sys.source(file.path(root,'scripts/load-current-lkt.R'),envir=.GlobalEnv)
        sys.source(file.path(root,'scripts/gru-encoder-study.R'),envir=.GlobalEnv);NULL
      },root,attr(lkt_release,'file'))
      parallel::clusterExport(cluster,c('p','d','tasks','output'))
      parallel::parLapplyLB(cluster,seq_len(nrow(tasks)),function(i)
        study_fit_one(tasks[i,,drop=FALSE],p$settings[[tasks$candidate[i]]],output,p,d))
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
      parameter_count=fit$parameter_count,selected_epoch=fit$selected_epoch,epochs_run=nrow(fit$training_history))
    predictions[[i]] <- data.frame(partition=partition,condition=task$candidate,seed=task$seed,
      source_row=test$comparison_source_row,participant=test$Anon.Student.Id,observed=test$CF..ansbin.,prediction=q)
  }
  study_write_csv(do.call(rbind,scores),file.path(output,'reports',paste0(partition,'-test.csv')))
  study_write_csv(do.call(rbind,predictions),file.path(output,'reports',paste0(partition,'-predictions.csv')))
  writeLines('PASS all nine settings/masks, fresh fits and held-out history weight replay.',file.path(output,'reports',paste0(partition,'-verification.txt')))
}

if(action %in% c('report','check')) {
  # Read each checkpoint and recompute validation metrics and mask contracts before
  # aggregating out-of-fold predictions. No fold or seed is selected on test results.
  sources <- list(fresh=list(directory=output,protocol=p))
  all_predictions <- all_scores <- list()
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
      settings <- protocol$settings[[task$candidate]];settings$seed <- as.integer(task$seed)
      stopifnot(identical(settings,fit$gru_settings),identical(saved$settings,settings),
        identical(fit$fit_rows,which(fold==1)),identical(fit$validation_rows,which(fold==2)),
        identical(fit$evaluation_rows,which(fold==3)),
        identical(saved$fingerprint,digest::digest(list(study=study_hash(file.path(directory,'study.rds')),task=saved$task,settings=settings),algo='sha256')),
        fit$parameter_count==p$parameter_counts[[task$candidate]])
      record <- read.csv(file.path(directory,'validation',paste0(task$run_id,'.csv')))
      metrics <- study_metrics(d$CF..ansbin.[fold==2],fit$prediction[fold==2])
      for(n in names(metrics))stopifnot(abs(record[[n]]-metrics[[n]])<1e-12)
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
  display_order <- c('off','one_head','two_heads')
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
    table <- function(x)paste0('<table border="1"><tr>',paste0('<th>',names(x),'</th>',collapse=''),'</tr>',
      paste(apply(as.data.frame(x),1,function(row)paste0('<tr>',paste0('<td>',row,'</td>',collapse=''),'</tr>')),collapse=''),'</table>')
    writeLines(paste0('<!doctype html><meta charset="utf-8"><title>GRU retrieval comparison</title>',
      '<style>body{font:17px sans-serif;max-width:1100px;margin:40px auto}td,th{padding:8px}table{border-collapse:collapse}</style>',
      '<h1>Retrieval comparison</h1><p>Augmented encoder model; three seeds; fixed architecture. AUC: higher is better. Log loss: lower is better.</p>',
      table(summary),'<h2>Paired changes</h2>',table(differences),
      '<p>Loss intervals: 2,000 paired participant bootstrap samples, conditional on the fitted models. CV AUC pools out-of-fold predictions separately per seed, then averages metrics.</p>',
      '<h2>Per partition</h2>',table(folds),'<h2>Interpretation limits</h2><ul>',
      paste0('<li>',p$caveats,'</li>',collapse=''),'</ul>'),file.path(output,'reports/report.html'))
    writeLines(paste('PASS',nrow(scores),'checkpoint contracts and metrics; all held-out test histories replayed; every CV response scored exactly once per condition/seed.'),file.path(output,'reports/verification.txt'))
  } else {
    recorded <- fread(file.path(output,'reports/summary.csv'))
    stopifnot(isTRUE(all.equal(as.data.frame(recorded),as.data.frame(summary),tolerance=1e-12,check.attributes=FALSE)))
  }
  print(summary);print(differences)
}
