# Test only the existing base GRU on the authors' cleaned EdNet sample.
# Args: ROOT OUTPUT plan|verify|fit|report|check [PREPARED_DIRECTORY for plan; SEED for fit]
args <- commandArgs(TRUE);stopifnot(length(args) %in% 3:4)
root <- normalizePath(args[1],winslash='/',mustWork=TRUE)
output <- args[2];action <- args[3]
stopifnot(action %in% c('plan','verify','fit','report','check'))
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-encoder-study.R'))
suppressPackageStartupMessages(library(data.table))
release <- lkt_release[c('lkt_version','lkt_source_archive','lkt_source_sha256','lkt_manifest_sha256')]
stopifnot(study_hash(attr(lkt_release,'archive'))==release[['lkt_source_sha256']])
script_names <- c('scripts/run-gru-ednet-ktst.R','scripts/gru-encoder-study.R',
  'scripts/load-current-lkt.R','workflows/simulation/shared/lkt_release.R')
script_files <- c(file.path(root,head(script_names,-1)),file.path(dirname(attr(lkt_release,'file')),'lkt_release.R'))
script_hashes <- setNames(vapply(script_files,study_hash,character(1)),script_names)

if(action=='plan') {
  stopifnot(!dir.exists(output))
  stopifnot(length(args)==4L)
  prepared <- normalizePath(args[4],winslash='/',mustWork=TRUE)
  provenance <- jsonlite::fromJSON(file.path(prepared,'provenance.json'))
  input <- file.path(prepared,'ednet-lkt.csv')
  stopifnot(study_hash(input)==provenance$exported_data_sha256)
  d <- as.data.frame(fread(input,colClasses=c('Anon.Student.Id'='character',
    'KC..Default.'='character','KC..Cluster.'='character')))
  stopifnot(nrow(d)==provenance$responses,!anyDuplicated(d$comparison_source_row),
    all(d$CF..ansbin. %in% 0:1),all(d$ktst_validation_fold %in% -1:4))
  # Use validation fold zero and the authors' fixed test participants. Every
  # response from a participant stays in one phase; no new random split is made.
  d$fold <- ifelse(d$ktst_validation_fold==-1L,3L,ifelse(d$ktst_validation_fold==0L,2L,1L))
  groups <- unique(d[c('Anon.Student.Id','fold')]);stopifnot(!anyDuplicated(groups$Anon.Student.Id))
  # LKT's spacing preparation also requires a practice clock. Derive it from
  # actual elapsed durations, shifted by one response so the current answer's
  # duration cannot enter its own predictors; calendar timing stays separate.
  durations_file <- file.path(dirname(prepared),'raw/ednet/ednet_sample_process.csv')
  durations <- fread(durations_file,select=c('index','user_id','timestamp','elapsed_time'))
  idx <- match(d$comparison_source_row-1L,durations$index)
  stopifnot(!anyNA(idx),all(as.character(durations$user_id[idx])==d$Anon.Student.Id),
    all(durations$timestamp[idx]==d$original_timestamp_ms),
    all(is.finite(durations$elapsed_time[idx])),all(durations$elapsed_time[idx]>=0))
  d$Duration..sec. <- durations$elapsed_time[idx]/1000
  d$CF..reltime. <- getFromNamespace('practiceTime','LKT')(d)
  expected_clock <- as.numeric(ave(d$Duration..sec.,d$Anon.Student.Id,
    FUN=function(x)c(0,head(cumsum(x),-1L))))
  stopifnot(identical(d$CF..reltime.,expected_clock))
  d$Duration..sec. <- NULL
  d <- LKTGRUTimeFeatures(d)
  origin <- file.path(root,'../LKT-outputs/gru-input-fusion-comparison-20261105/download/results/models/development__baseline__seed1.rds')
  original <- readRDS(origin)$fit
  base <- do.call(GRUModel,original$gru_settings)$settings
  s <- base
  # Unlike the small Cloze study, EdNet can contain held-out-only identities.
  # Select the documented zero encoding explicitly, retaining those test rows.
  s$unknown_levels <- 'zero'
  s <- do.call(GRUModel,s)$settings
  stopifnot(identical(s[setdiff(names(s),'unknown_levels')],base[setdiff(names(base),'unknown_levels')]),
    s$num_layers==3L,s$hidden_size==32L,identical(s$head_hidden_sizes,128L),s$feature_encoder=='mlp',
    s$encoder_output_size==16L,s$retrieval_heads==1L,s$retrieval_window==100L,
    identical(s$zero_init_embedding_cols,'KC..Default.'),s$inference_mode=='none',
    !s$input_to_all_layers,s$numeric_fusion=='concat',s$categorical_fusion=='concat',s$input_fusion=='concat')
  dir.create(output,recursive=TRUE)
  for(name in c('data','models','reports','training'))dir.create(file.path(output,name))
  saveRDS(d,file.path(output,'data/prepared_data.rds'))
  # Carry over the existing history transforms as fixed architecture choices.
  # Neural weights are fitted afresh; no fitted Cloze weights are transferred.
  p <- list(schema=1L,release=release,release_file_sha256=study_hash(attr(lkt_release,'file')),
    script_hashes=script_hashes,settings=s,recipe=original$model$recipe,
    source_checkpoint_sha256=study_hash(origin),input_provenance=provenance,
    practice_duration_source_sha256=study_hash(durations_file),
    data_sha256=study_hash(file.path(output,'data/prepared_data.rds')),seeds=1:3,validation_fold=0L,total_fits=3L,
    policy=c('Only the pre-existing base model; no joint encoder, alternative architecture or hyperparameter search.',
      'Fresh weights, one fixed KTST validation fold and fixed test students, three seeds. This is not five-fold cross-validation.',
      'Original skill-tag combinations are categories for the unchanged single-skill embedding and exact-match retrieval filter.',
      'Unknown held-out identities are zero encoded. Vocabularies and numeric scaling use only training rows.',
      'All retained histories remain chronological and uncapped. Base BPTT length 100 and retrieval window 100 remain unchanged.',
      'Primary test AUC excludes each student first response, matching KTST evaluation targets. Full-response AUC is also saved.',
      'The three history transforms and their six nonlinear constants remain frozen from the prior base; not retuned on EdNet.',
      'Timing uses question-presentation timestamps in seconds. Current response duration is never a predictor.',
      'This uses KTST cleaning and participant assignments, not its model, input aggregation, history window or optimization search.'))
  saveRDS(p,file.path(output,'study.rds'))
  jsonlite::write_json(list(participants=nrow(groups),responses=nrow(d),
    rows=as.list(table(factor(d$fold,levels=1:3,labels=c('training','validation','test')))),
    students=as.list(table(factor(groups$fold,levels=1:3,labels=c('training','validation','test')))),
    settings=s,recipe=p$recipe,policy=p$policy),file.path(output,'plan.json'),auto_unbox=TRUE,pretty=TRUE,null='null')
  cat('FROZEN',nrow(d),'responses;',nrow(groups),'students; one KTST split; three fresh base fits.\n')
  quit(save='no')
}

p <- readRDS(file.path(output,'study.rds'))
stopifnot(identical(release,p$release),identical(script_hashes,p$script_hashes),
  study_hash(attr(lkt_release,'file'))==p$release_file_sha256,
  study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256)
d <- readRDS(file.path(output,'data/prepared_data.rds'))

if(action=='verify') {
  # Exercise the complete dataset's feature preparation before allocating the
  # research fits. Missing spacing is allowed only with its availability flag.
  prep <- getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1L,p$recipe,p$settings)
  net <- getFromNamespace('lkt_gru_network','LKT')(prep,p$settings)
  stopifnot(length(prep$vocabularies)==2L,all(prep$embedding_dims==16L),
    net$time_encoder[[1]][[1]]$weight$size(2)==4L,
    net$head[[1]]$weight$size(1)==128L)
  unknown <- do.call(rbind,lapply(names(prep$vocabularies),function(n)data.frame(column=n,
    validation=sum(!d[[n]][d$fold==2] %in% prep$vocabularies[[n]]),
    test=sum(!d[[n]][d$fold==3] %in% prep$vocabularies[[n]]))))
  study_write_csv(unknown,file.path(output,'reports/unknown-identities.csv'))
  cat('PASS deployed release, complete EdNet preprocessing, train-only vocabularies, base dimensions and missing-value handling.\n')
}

if(action=='fit') {
  stopifnot(length(args)==4L);seed <- as.integer(args[4]);stopifnot(seed %in% p$seeds)
  path <- file.path(output,'models',paste0('base-seed',seed,'.rds'));stopifnot(!file.exists(path))
  s <- p$settings;s$seed <- seed
  cat('START base seed',seed,format(Sys.time(),tz='UTC'),'\n');flush.console()
  elapsed <- system.time(fit <- LKT(data.table::copy(as.data.table(d)),usefolds=1L,
    components=p$recipe$components,features=p$recipe$features,fixedpars=p$recipe$fixedpars,
    interc=p$recipe$interc,model=do.call(GRUModel,s),verbose=TRUE))[['elapsed']]
  fit$newdata <- NULL;fit$nullmodel <- NULL
  stopifnot(identical(fit$gru_settings,s),identical(fit$fit_rows,which(d$fold==1L)),
    identical(fit$validation_rows,which(d$fold==2L)),identical(fit$evaluation_rows,which(d$fold==3L)))
  saved <- list(seed=seed,protocol_sha256=study_hash(file.path(output,'study.rds')),fit=fit,elapsed_seconds=elapsed)
  study_write_rds(saved,path)
  study_write_csv(fit$training_history,file.path(output,'training',paste0('base-seed',seed,'.csv')))
  # Replay every test history from portable saved weights, after fitting. This
  # verifies causality and row alignment independently of fit-time predictions.
  restored <- readRDS(path)$fit;test <- d[d$fold==3L,,drop=FALSE]
  replay <- study_replay(restored,test)
  stopifnot(max(abs(replay-fit$prediction[d$fold==3]))<3e-6)
  study_write_csv(data.frame(source_row=test$comparison_source_row,participant=test$Anon.Student.Id,
    position=test$ktst_position,observed=test$CF..ansbin.,prediction=replay),
    file.path(output,'reports',paste0('base-seed',seed,'-predictions.csv')))
  writeLines('PASS saved held-out history replay.',file.path(output,'reports',paste0('base-seed',seed,'-verification.txt')))
  cat('DONE base seed',seed,'test AUC',unname(study_metrics(test$CF..ansbin.[test$ktst_position>1],replay[test$ktst_position>1])[['AUC']]),'\n')
}

if(action %in% c('report','check')) {
  metrics <- list()
  for(seed in p$seeds) {
    saved <- readRDS(file.path(output,'models',paste0('base-seed',seed,'.rds')));fit <- saved$fit
    s <- p$settings;s$seed <- as.integer(seed)
    stopifnot(identical(fit$gru_settings,s),saved$protocol_sha256==study_hash(file.path(output,'study.rds')),
      identical(fit$fit_rows,which(d$fold==1L)),identical(fit$validation_rows,which(d$fold==2L)),
      identical(fit$evaluation_rows,which(d$fold==3L)),
      file.exists(file.path(output,'reports',paste0('base-seed',seed,'-verification.txt'))))
    q <- fread(file.path(output,'reports',paste0('base-seed',seed,'-predictions.csv')))
    stopifnot(identical(q$source_row,d$comparison_source_row[d$fold==3]),
      max(abs(q$prediction-fit$prediction[d$fold==3]))<3e-6)
    for(phase in 1:3)for(first in c(FALSE,TRUE)) {
      keep <- d$fold==phase & (first | d$ktst_position>1L)
      metrics[[paste(seed,phase,first)]] <- data.frame(seed=seed,phase=c('training','validation','test')[phase],
        include_first_response=first,responses=sum(keep),AUC=unname(study_metrics(d$CF..ansbin.[keep],fit$prediction[keep])[['AUC']]),
        selected_epoch=fit$selected_epoch,trained_epochs=fit$trained_epochs,parameter_count=fit$parameter_count)
    }
    train_auc <- unname(study_metrics(d$CF..ansbin.[d$fold==1],fit$prediction[d$fold==1])[['AUC']])
    stopifnot(abs(train_auc-fit$training_history$training_auc[match(fit$selected_epoch,fit$training_history$epoch)])<1e-12)
  }
  metrics <- rbindlist(metrics)
  summary <- metrics[,.(AUC=mean(AUC),seed_sd=sd(AUC),responses=unique(responses)),by=.(phase,include_first_response)]
  if(action=='report') {
    study_write_csv(metrics,file.path(output,'reports/auc-by-seed.csv'))
    study_write_csv(summary,file.path(output,'reports/auc-summary.csv'))
    writeLines('PASS all three base settings, participant masks, saved held-out replays and restored training AUCs.',file.path(output,'reports/verification.txt'))
  } else stopifnot(isTRUE(all.equal(as.data.frame(fread(file.path(output,'reports/auc-summary.csv'))),as.data.frame(summary),tolerance=1e-12,check.attributes=FALSE)))
  print(summary)
}
