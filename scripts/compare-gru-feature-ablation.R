# Full versus no engineered-LKT/time inputs on Cloze and ASSISTments 2017.
# ROOT OUTPUT ACTION DATASET PARTITION [CLOZE_REFERENCE ASSISTMENTS_PREPARED]
args <- commandArgs(TRUE);stopifnot(length(args) %in% c(5L,7L))
root <- normalizePath(args[1],winslash='/',mustWork=TRUE)
output <- args[2];action <- args[3];dataset <- args[4];part <- args[5]
stopifnot(action %in% c('plan','verify','smoke','fit','report','check'))
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-encoder-study.R'))
suppressPackageStartupMessages(library(data.table))
release <- lkt_release[c('lkt_version','lkt_source_archive','lkt_source_sha256','lkt_manifest_sha256')]
scripts <- c('scripts/compare-gru-feature-ablation.R','scripts/report-gru-feature-ablation.R',
  'scripts/prepare-gru-assistments.R','scripts/gru-encoder-study.R','scripts/load-current-lkt.R',
  'workflows/simulation/shared/lkt_release.R')
files <- c(file.path(root,head(scripts,-1)),file.path(dirname(attr(lkt_release,'file')),'lkt_release.R'))
hashes <- setNames(vapply(files,study_hash,character(1)),scripts)
conditions <- c('baseline','no_features');datasets <- c('cloze','assistments')
tasks_for <- function(part)do.call(rbind,lapply(conditions,function(condition)do.call(rbind,lapply(1:3,function(seed)
  data.frame(run_id=paste(part,condition,paste0('seed',seed),sep='__'),partition=part,
    family='augmented',stage=condition,candidate=condition,seed=seed)))))

if(action=='plan') {
  stopifnot(length(args)==7L,!dir.exists(output))
  reference <- args[6];prepared <- args[7];old <- readRDS(file.path(reference,'study.rds'))
  stopifnot(identical(old$release,release),identical(old$seeds,1:3))
  base <- old$settings$baseline;base$unknown_levels <- 'zero'
  base <- do.call(GRUModel,base)$settings
  stripped <- base;stripped$use_lkt_features <- FALSE;stripped['time_col'] <- list(NULL)
  stripped$time_feature_cols <- stripped$time_missing_cols <- character()
  stripped <- do.call(GRUModel,stripped)$settings
  changed <- names(base)[!vapply(names(base),function(n)identical(base[[n]],stripped[[n]]),logical(1))]
  stopifnot(setequal(changed,c('use_lkt_features','time_col','time_feature_cols','time_missing_cols')),
    base$head_dropout==.1,base$input_dropout==.1,base$gru_dropout==.1,
    base$num_layers==3L,base$hidden_size==32L,base$head_hidden_sizes==128L,
    base$inference_mode=='none',base$retrieval_heads==1L,length(base$numeric_cols)==0L)
  dir.create(output,recursive=TRUE)
  for(name in datasets) {
    target <- file.path(output,name)
    for(folder in c('data','partitions','models','training','validation','reports'))dir.create(file.path(target,folder),recursive=TRUE)
    if(name=='cloze') {
      file <- file.path(reference,'data/prepared_data.rds')
      stopifnot(study_hash(file)==old$data_sha256);d <- readRDS(file)
      partitions <- setNames(lapply(old$partitions,function(k)readRDS(file.path(reference,'partitions',paste0(k,'.rds')))),old$partitions)
      for(k in old$partitions)stopifnot(study_hash(file.path(reference,'partitions',paste0(k,'.rds')))==old$partition_hashes[[k]])
      controls <- list()
      for(k in old$partitions)for(seed in 1:3) {
        f <- file.path(reference,'models',paste0(k,'__baseline__seed',seed,'.rds'))
        controls[[paste(k,seed)]] <- list(prediction=readRDS(f)$fit$prediction,source_sha256=study_hash(f))
      }
      study_write_rds(controls,file.path(target,'data/reference-controls.rds'))
      control_sha <- study_hash(file.path(target,'data/reference-controls.rds'))
      provenance <- list(reference_protocol_sha256=study_hash(file.path(reference,'study.rds')))
    } else {
      provenance <- jsonlite::read_json(file.path(prepared,'provenance.json'),simplifyVector=TRUE)
      file <- file.path(prepared,'prepared_data.rds');stopifnot(study_hash(file)==provenance$prepared_sha256)
      d <- readRDS(file);ids <- sort(unique(d$Anon.Student.Id),method='radix')
      set.seed(20261103L);outer <- sample(rep(1:5,length.out=length(ids)));partitions <- list()
      for(k in 1:5) {
        training <- ids[outer!=k];set.seed(20261103L+k)
        validation <- sample(training,ceiling(.2*length(training)))
        partitions[[paste0('cv',k)]] <- ifelse(d$Anon.Student.Id %in% ids[outer==k],3L,
          ifelse(d$Anon.Student.Id %in% validation,2L,1L))
      }
      control_sha <- NULL
    }
    study_write_rds(d,file.path(target,'data/prepared_data.rds'))
    for(k in names(partitions)) {
      role <- partitions[[k]];groups <- unique(data.frame(id=d$Anon.Student.Id,role=role))
      stopifnot(!anyDuplicated(groups$id),all(role %in% 1:3),length(role)==nrow(d))
      study_write_rds(role,file.path(target,'partitions',paste0(k,'.rds')))
    }
    p <- list(schema=1L,dataset=name,release=release,release_file_sha256=study_hash(attr(lkt_release,'file')),
      script_hashes=hashes,data_sha256=study_hash(file.path(target,'data/prepared_data.rds')),
      partitions=names(partitions),partition_hashes=setNames(vapply(names(partitions),function(k)
        study_hash(file.path(target,'partitions',paste0(k,'.rds'))),character(1)),names(partitions)),
      seeds=1:3,total_fits=30L,settings=list(baseline=base,no_features=stripped),baseline=old$baseline,
      reference_controls_sha256=control_sha,provenance=provenance,
      contrast='Full minus no LKT/time features, within dataset. Main interaction: Cloze contrast minus ASSISTments contrast.',
      inference='2000 paired whole-participant bootstrap draws within dataset, shared across conditions/seeds; independent draws between datasets. Conditional on fitted models.',
      limits=c('Previously inspected Cloze architecture/data; fixed shared feature recipe and nonlinear constants, no per-dataset tuning.',
        'Feature removal changes input-dependent parameter counts; this is not parameter matching.',
        'Timestamps order interactions in both conditions; only baseline receives elapsed-time and engineered feature values.',
        'Unknown categories use zero encoding in both conditions/datasets. Cloze control must reproduce the prior predictions.'))
    study_write_rds(p,file.path(target,'study.rds'));writeLines(capture.output(dput(p)),file.path(target,'study.txt'))
  }
  writeLines('60 fresh fits: two datasets x two feature conditions x five folds x three seeds.',file.path(output,'PLAN.txt'))
  cat('PASS frozen 60-fit cross-dataset feature-ablation protocol.\n');quit(save='no')
}

load_study <- function(name) {
  target <- file.path(output,name);p <- readRDS(file.path(target,'study.rds'))
  stopifnot(identical(p$release,release),identical(p$script_hashes,hashes),
    study_hash(attr(lkt_release,'file'))==p$release_file_sha256,
    study_hash(file.path(target,'data/prepared_data.rds'))==p$data_sha256)
  for(k in p$partitions)stopifnot(study_hash(file.path(target,'partitions',paste0(k,'.rds')))==p$partition_hashes[[k]])
  p
}

if(action %in% c('verify','smoke'))for(name in datasets) {
  p <- load_study(name);target <- file.path(output,name)
  original <- readRDS(file.path(target,'data/prepared_data.rds'))
  for(k in p$partitions) {
    d <- original;d$fold <- readRDS(file.path(target,'partitions',paste0(k,'.rds')))
    prep <- getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1,p$baseline,p$settings$no_features)
    stopifnot(length(prep$numeric_names)==0L)
  }
  if(action=='verify') {
    counts <- list()
    for(condition in conditions) {
      prep <- getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1,p$baseline,p$settings[[condition]])
      net <- getFromNamespace('lkt_gru_network','LKT')(prep,p$settings[[condition]])
      counts[[condition]] <- sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
    }
    cat('PASS',name,'all five folds: no numeric inputs when ablated; parameter counts',unlist(counts),'\n')
  } else {
    histories <- split(seq_len(nrow(original)),original$Anon.Student.Id)
    rows <- head(histories[[which.max(lengths(histories))]],12L);stopifnot(length(rows)==12L)
    d <- do.call(rbind,lapply(1:8,function(i) {
      x <- original[rows,,drop=FALSE];x$Anon.Student.Id <- paste0('synthetic',i)
      x$CF..ansbin. <- rep(c(0,1,1,0,1,0),2L);x$fold <- c(1L,1L,1L,1L,1L,2L,3L,3L)[i];x
    }))
    d$comparison_source_row <- seq_len(nrow(d))
    smoke <- file.path(dirname(output),'smoke-verification',name);stopifnot(!dir.exists(smoke))
    for(folder in c('models','training','validation'))dir.create(file.path(smoke,folder),recursive=TRUE)
    study_write_rds(p,file.path(smoke,'study.rds'))
    for(condition in conditions) {
      s <- p$settings[[condition]];s$epochs <- 1L;s$early_stopping <- FALSE;s$restore_best <- FALSE
      task <- tasks_for('smoke');task <- task[task$candidate==condition & task$seed==1L,,drop=FALSE]
      study_fit_one(task,s,smoke,p,d)
      f <- readRDS(file.path(smoke,'models',paste0(task$run_id,'.rds')))$fit
      test <- d[d$fold==3L,,drop=FALSE];q <- study_replay(f,test)
      stopifnot(all(is.finite(unlist(f$model$weights))),max(abs(q-f$prediction[d$fold==3L]))<3e-6)
      if(condition=='no_features') {
        changed <- test
        timing_columns <- intersect(c('CF..Time.','CF..reltime.','gru_item_age_seconds','gru_mean_prior_spacing_seconds'),names(changed))
        for(column in timing_columns)changed[[column]] <- changed[[column]]*2+7
        stopifnot(identical(q,study_replay(f,changed)))
      }
    }
    cat('PASS',name,'two synthetic smoke fits and exact timing-invariance of ablated predictions.\n')
  }
}

if(action=='fit') {
  stopifnot(dataset %in% datasets);p <- load_study(dataset);stopifnot(part %in% p$partitions)
  output <- file.path(output,dataset);d <- readRDS(file.path(output,'data/prepared_data.rds'))
  d$fold <- readRDS(file.path(output,'partitions',paste0(part,'.rds')));tasks <- tasks_for(part)
  workers <- as.integer(Sys.getenv('LKT_STUDY_WORKERS','1'));stopifnot(workers>=1L,workers<=6L)
  cluster <- parallel::makePSOCKcluster(workers,outfile=file.path(output,paste0(part,'-workers.log')))
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
  predictions <- list()
  for(i in seq_len(nrow(tasks))) {
    t <- tasks[i,];f <- readRDS(file.path(output,'models',paste0(t$run_id,'.rds')))$fit
    test <- d[d$fold==3L,,drop=FALSE];q <- study_replay(f,test)
    stopifnot(max(abs(q-f$prediction[d$fold==3L]))<3e-6)
    predictions[[i]] <- data.frame(dataset=dataset,partition=part,condition=t$candidate,seed=t$seed,
      source_row=test$comparison_source_row,participant=test$Anon.Student.Id,observed=test$CF..ansbin.,prediction=f$prediction[d$fold==3L])
  }
  study_write_csv(do.call(rbind,predictions),file.path(output,'reports',paste0(part,'-predictions.csv')))
  writeLines('PASS six fresh fits and independent saved-weight held-out replay.',file.path(output,'reports',paste0(part,'-verification.txt')))
}
if(action %in% c('report','check')) {
  source(file.path(root,'scripts/report-gru-feature-ablation.R'))
  feature_ablation_report(output,lapply(setNames(datasets,datasets),load_study),tasks_for,action)
}
