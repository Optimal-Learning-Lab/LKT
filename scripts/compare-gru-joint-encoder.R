# Validate one shared learned input representation against the current zero-item, skill-filtered GRU.
# All neural implementation comes from the explicitly verified LKT candidate.
# Args: ROOT OUTPUT ACTION PARTITION CANDIDATE_JSON CANDIDATE_LIBRARY [PREPARED_MODEL REFERENCE]
args <- commandArgs(TRUE);stopifnot(length(args) %in% c(6L,8L))
root <- normalizePath(args[1L],winslash='/',mustWork=TRUE)
output <- args[2L];action <- args[3L];partition <- args[4L]
stopifnot(action %in% c('plan','verify','fit','report','check'))
# This is pre-release validation of an explicitly selected frozen package candidate.
# The active central release is recorded but is neither loaded nor modified.
source(file.path(root,'scripts/gru-encoder-study.R'))
candidate_file <- normalizePath(args[5L],winslash='/',mustWork=TRUE)
candidate <- study_require_release(candidate_file,args[6L])
suppressPackageStartupMessages(library(data.table))
source(file.path(root,'scripts/gru-joint-encoder-settings.R'))
active_release_file <- Sys.getenv('LKT_RELEASE_FILE',unset=file.path(dirname(root),
  'PolicyFactory2/workflows/simulation/shared/lkt-release.env'))
source(file.path(dirname(active_release_file),'lkt_release.R'))
active_release <- lkt_read_release(active_release_file)
candidate_archive <- file.path(dirname(candidate_file),candidate$source_archive)
stopifnot(isTRUE(candidate$validation_candidate),study_hash(candidate_archive)==candidate$source_sha256)
script_names <- c('scripts/compare-gru-joint-encoder.R','scripts/gru-encoder-study.R',
  'scripts/gru-joint-encoder-settings.R','workflows/simulation/shared/lkt_release.R')
script_files <- c(file.path(root,head(script_names,-1L)),file.path(dirname(active_release_file),'lkt_release.R'))
script_hashes <- setNames(vapply(script_files,study_hash,character(1)),script_names)
release_identity <- c(lkt_version=candidate$version,lkt_source_archive=candidate$source_archive,
  lkt_source_sha256=candidate$source_sha256,lkt_manifest_sha256=candidate$manifest_sha256)

if(action=='plan') {
  stopifnot(length(args)==8L,!dir.exists(output))
  prepared <- args[7L];reference <- args[8L]
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
  # not imported: both conditions are trained fresh with the verified candidate.
  for(name in partitions) {
    path <- file.path(reference,'partitions',paste0(name,'.rds'))
    stopifnot(study_hash(path)==reference_protocol$partition_hashes[[name]],
      file.copy(path,file.path(output,'partitions',paste0(name,'.rds'))))
    fold <- readRDS(path)
    groups <- unique(data.frame(participant=d$Anon.Student.Id,fold=fold))
    stopifnot(length(fold)==nrow(d),all(fold %in% 1:3),!anyDuplicated(groups$participant))
    if(name=='development')stopifnot(identical(as.integer(d$fold),fold))
  }
  # Keep the four blocks unchanged and add only one predeclared joint encoder.
  # Match the output width by counting weights; outcomes do not select its size.
  base <- do.call(GRUModel,reference_protocol$settings$baseline)$settings
  stopifnot(base$retrieval_heads==1L,base$retrieval_decay=='none',!base$input_to_all_layers,
    identical(base$retrieval_filter_cols,c('KC..Default.','KC..Cluster.')),
    !length(base$retrieval_cue_cols),identical(base$zero_init_embedding_cols,'KC..Default.'))
  prep <- getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1,recipe,base)
  chosen <- joint_encoder_settings(base,prep,46257L);new_settings <- chosen$settings
  study_write_csv(chosen$counts,file.path(output,'parameter-matching.csv'))
  varied <- c('joint_encoder_output_size','joint_encoder_hidden_sizes','joint_encoder_activation','joint_encoder_dropout')
  for(s in new_settings)stopifnot(identical(s[setdiff(names(s),varied)],base[setdiff(names(base),varied)]))
  stopifnot(identical(recipe,reference_protocol$baseline))
  # Freeze prior predictions solely to audit that the newly fitted baseline
  # control reproduces the original. No fitted weights are imported for training.
  controls <- list()
  for(part in partitions)for(seed in 1:3) {
    id <- paste(part,'baseline',paste0('seed',seed),sep='__')
    file <- file.path(reference,'models',paste0(id,'.rds'));fit <- readRDS(file)$fit
    expected <- new_settings$baseline;expected$seed <- as.integer(seed)
    stopifnot(identical(do.call(GRUModel,fit$gru_settings)$settings,expected))
    controls[[paste(part,seed,sep='__')]] <- list(prediction=fit$prediction,source_sha256=study_hash(file))
  }
  study_write_rds(controls,file.path(output,'data/reference-controls.rds'))
  p <- list(schema=2L,release=release_identity,release_file_sha256=study_hash(candidate_file),active_release=active_release,active_release_file_sha256=study_hash(active_release_file),
    script_hashes=script_hashes,data_sha256=study_hash(file.path(output,'data/prepared_data.rds')),
    partition_hashes=setNames(vapply(partitions,function(n)study_hash(file.path(output,'partitions',paste0(n,'.rds'))),character(1)),partitions),
    control_sha256=study_hash(file.path(output,'data/reference-controls.rds')),settings=new_settings,baseline=recipe,seeds=1:3,partitions=partitions,total_fits=36L,
    joint_layout=chosen$counts,parameter_counts=setNames(chosen$counts$parameter_count,chosen$counts$condition),
    comparisons=list(c('joint_encoder','baseline')),
    design='One shared 64 -> 32 ReLU -> 49 input encoder versus the unchanged four-block base. The same learned representation feeds retrieval, prediction and the GRU; no raw-input bypass. Two fresh conditions, three seeds and six existing participant partitions.',
    selection='No AUC-based width search. A fixed 32-unit hidden layer and analytically matched output width; restored epochs use the prior validation criterion.',
    caveats=c('This validates a frozen package candidate while separate sequence-unit work remains unpublished. The active central release is unchanged, and this experiment trains only GRUs.',
      'Parameter totals are approximately matched: 46,114 versus 46,257, within 0.31 percent. All four source blocks remain 16 wide; recurrent layers, answer encoder and prediction head stay fixed.',
      'Architecture was chosen on previously inspected data; this is exploratory fixed-architecture participant CV, not nested evaluation of prior searches.',
      'Engineered-feature nonlinear parameters remain inherited constants. Studies contribute to timing but do not update recurrent answers.',
      'All conditions preserve 144/36 identities, zero trainable item initialization, skill-inclusive filtering within 100 global scored events and prediction-only retrieval.',
      'Individual-model AUCs are averaged over three seeds rather than ensembled. Paired bootstrap intervals condition on these fitted models.'))
  study_write_rds(p,file.path(output,'study.rds'))
  writeLines(capture.output(dput(p)),file.path(output,'study.txt'))
  cat('FROZEN: 36 fresh fits; joint encoder plus baseline, three seeds, six participant partitions.\n')
  quit(save='no')
}

p <- readRDS(file.path(output,'study.rds'))
stopifnot(identical(p$schema,2L),identical(p$release,release_identity),identical(p$script_hashes,script_hashes),
  study_hash(candidate_file)==p$release_file_sha256,study_hash(active_release_file)==p$active_release_file_sha256,
  study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256,
  study_hash(file.path(output,'data/reference-controls.rds'))==p$control_sha256)
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
  # without allowing a smoke fit to substitute for any of the 36 research fits.
  d <- readRDS(file.path(output,'data/prepared_data.rds'))
  for(condition in names(p$settings)) {
    prep <- getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1,p$baseline,p$settings[[condition]])
    net <- getFromNamespace('lkt_gru_network','LKT')(prep,p$settings[[condition]])
    stopifnot(sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))==p$parameter_counts[[condition]])
    stopifnot(net$time_encoder[[1]][[1]]$weight$size(2)==
      1L+length(p$settings[[condition]]$time_feature_cols)+length(p$settings[[condition]]$time_missing_cols))
  }
  cat('PASS release, protocol hashes, existing participant folds and both deployed candidate configurations.\n')
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
      # Every worker must select the same frozen candidate explicitly. The
      # active release loader would select a different, already published build.
      parallel::clusterCall(cluster,function(root,candidate_file,candidate_library) {
        assign('root',root,envir=.GlobalEnv)
        sys.source(file.path(root,'scripts/gru-encoder-study.R'),envir=.GlobalEnv)
        study_require_release(candidate_file,candidate_library);NULL
      },root,candidate_file,args[6L])
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
  writeLines('PASS all six settings/masks, fresh fits and held-out history weight replay.',file.path(output,'reports',paste0(partition,'-verification.txt')))
}

if(action %in% c('report','check')) {
  # Read each checkpoint and recompute validation metrics and mask contracts before
  # aggregating out-of-fold predictions. No fold or seed is selected on test results.
  sources <- list(fresh=list(directory=output,protocol=p))
  all_predictions <- all_scores <- list();fit_auc <- control_checks <- list()
  controls <- readRDS(file.path(output,'data/reference-controls.rds'))
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
      # Score all phases at the restored weights, with dropout disabled. Training
      # AUC and held-out AUC therefore refer to the same selected model.
      for(phase in 1:3)fit_auc[[paste(task$run_id,phase)]] <- data.frame(partition=part,
        condition=task$candidate,seed=task$seed,phase=c('training','validation','test')[phase],
        AUC=unname(study_metrics(d$CF..ansbin.[fold==phase],fit$prediction[fold==phase])[['AUC']]))
      if(task$candidate=='baseline') {
        delta <- max(abs(fit$prediction-controls[[paste(part,task$seed,sep='__')]]$prediction))
        stopifnot(delta<3e-6)
        control_checks[[task$run_id]] <- data.frame(run_id=task$run_id,maximum_probability_difference=delta)
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
  fit_auc <- rbindlist(fit_auc);control_checks <- rbindlist(control_checks)
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
  display_order <- names(p$settings)
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
    study_write_csv(as.data.frame(control_checks),file.path(output,'reports/control-reproduction.csv'))
    table <- function(x)paste0('<table border="1"><tr>',paste0('<th>',names(x),'</th>',collapse=''),'</tr>',
      paste(apply(as.data.frame(x),1,function(row)paste0('<tr>',paste0('<td>',row,'</td>',collapse=''),'</tr>')),collapse=''),'</table>')
    writeLines(paste0('<!doctype html><meta charset="utf-8"><title>GRU joint encoder comparison</title>',
      '<style>body{font:17px sans-serif;max-width:1100px;margin:40px auto}td,th{padding:8px}table{border-collapse:collapse}</style>',
      '<h1>Shared learned input representation comparison</h1><p>Augmented encoder model; three seeds; fixed architecture. AUC: higher is better.</p>',
      table(summary[,.(evaluation,condition,AUC,AUC_seed_sd)]),'<h2>Training and validation AUC</h2>',
      table(fit_auc_summary[phase!='test']),'<h2>Paired AUC changes</h2>',table(differences[,.(evaluation,comparison,AUC_gain)]),
      '<p>CV AUC pools out-of-fold predictions separately per seed, then averages metrics. Internal validation diagnostics remain in CSV records.</p>',
      '<h2>Per partition</h2>',table(folds[,.(partition,condition,AUC,mean_epoch)]),'<h2>Interpretation limits</h2><ul>',
      paste0('<li>',p$caveats,'</li>',collapse=''),'</ul>'),file.path(output,'reports/report.html'))
    writeLines(paste('PASS',nrow(scores),'checkpoint contracts and metrics; all held-out test histories replayed; every CV response scored exactly once per condition/seed.'),file.path(output,'reports/verification.txt'))
  } else {
    recorded <- fread(file.path(output,'reports/summary.csv'))
    stopifnot(isTRUE(all.equal(as.data.frame(recorded),as.data.frame(summary),tolerance=1e-12,check.attributes=FALSE)))
  }
  print(summary[,.(evaluation,condition,AUC,AUC_seed_sd)]);print(differences[,.(evaluation,comparison,AUC_gain)])
}
