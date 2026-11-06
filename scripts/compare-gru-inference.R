# Staged inference-network comparison. Arguments: repository root, new output folder,
# action (plan/run/smoke), workers (3), families (all or comma-separated), optional
# design RDS. Planning freezes the complete experiment without fitting any models.
args <- commandArgs(TRUE)
stopifnot(length(args)>=2L,length(args)<=6L)
root <- normalizePath(args[1L],winslash='/',mustWork=TRUE)
output <- args[2L]
action <- if(length(args)>=3L)args[3L] else 'plan'
workers <- if(length(args)>=4L)as.integer(args[4L]) else 3L
families <- if(length(args)>=5L && args[5L]!='all')strsplit(args[5L],',',fixed=TRUE)[[1L]] else
  c('matched','raw','augmented')
stopifnot(action %in% c('plan','run','smoke'),workers>=1L,!anyNA(workers),
  length(families)>0L,!anyDuplicated(families),all(families %in% c('matched','raw','augmented')))
source(file.path(root,'scripts/load-current-lkt.R'))
private_library <- lkt_release[['lkt_library']]
suppressPackageStartupMessages(library(data.table))
stopifnot(torch::torch_is_installed())
source(file.path(root,'scripts/gru-benchmark-data.R'))
source(file.path(root,'scripts/gru-grid-utils.R'))

# The default experiment fixes the recurrent backbone and uses the previously tried
# training controls. Additional positive KL weights can be declared before fitting;
# every declared weight is reported rather than choosing one on the test set.
design <- list(seeds=1:3,kl_betas=.001,policy_root=file.path(dirname(root),'PolicyFactory2'),
  common=list(hidden_size=32L,num_layers=3L,gru_dropout=.1,head_hidden_sizes=32L,
    head_activation='relu',embedding_dims=16L,input_dropout=.1,head_dropout=.1,
    latent_size=32L,inference_hidden_sizes=32L,inference_activation='relu',inference_dropout=0,
    logvar_bounds=c(-10,4),logvar_init=-2,optimizer='adamw',learning_rate=.001,weight_decay=1e-4,
    epochs=100L,batch_size=32L,sequence_length=100L,gradient_clip=5,validation_folds=2,
    early_stopping=TRUE,patience=10L,min_delta=1e-4,restore_best=TRUE,
    device='cpu',num_threads=1L,order_col='CF..Time.',clip_epsilon=1e-5))
if(length(args)>=6L) {
  design <- readRDS(args[6L])
  stopifnot(is.list(design),setequal(names(design),c('seeds','kl_betas','policy_root','common')))
}
stopifnot(is.numeric(design$seeds),length(design$seeds)>0L,!anyNA(design$seeds),
  all(design$seeds>=0 & design$seeds==floor(design$seeds)),!anyDuplicated(design$seeds),
  is.numeric(design$kl_betas),length(design$kl_betas)>0L,all(is.finite(design$kl_betas)),
  all(design$kl_betas>0),!anyDuplicated(design$kl_betas),
  !any(c('seed','inference_mode','temporal_kl_beta','categorical_cols','time_col','use_lkt_features') %in% names(design$common)))
if(action=='smoke') {
  design$seeds <- design$seeds[1L]
  design$common$epochs <- 1L;design$common$early_stopping <- FALSE
  design$common$restore_best <- FALSE
}
if(dir.exists(output) && length(list.files(output,all.files=TRUE,no..=TRUE)) &&
   !file.exists(file.path(output,'inference_protocol.rds')))stop('Use a new output directory.')
dir.create(output,recursive=TRUE,showWarnings=FALSE)
output <- normalizePath(output,winslash='/',mustWork=TRUE)
for(folder in c('models','validation','source'))dir.create(file.path(output,folder),showWarnings=FALSE)
workflow <- file.path(normalizePath(design$policy_root,winslash='/',mustWork=TRUE),'workflows/simulation/paper_rerun_wald')
parameters <- new.env(parent=baseenv());sys.source(file.path(workflow,'model_parameters.R'),envir=parameters)
baseline <- list(components=c('Anon.Student.Id','KC..Default.','KC..Default.'),
  features=c('logitdec','ppes','logitdec'),fixedpars=parameters$paper_ppes_tracing_parameters)
prepared <- prepare_gru_benchmark_data('largerawsample',root,output,workflow)
d <- prepared$data
if(action=='smoke') {
  # Keep short real histories for a pipeline check. This is a different, explicitly
  # labeled experiment and must never be interpreted as benchmark performance.
  d <- d[,head(.SD,12L),by=Anon.Student.Id]
  stopifnot(uniqueN(d[fold==1L]$KC..Default.)==144L,uniqueN(d[fold==1L]$KC..Cluster.)==36L)
}
recipe <- c(baseline,list(interacts=rep(NA_character_,3),connectors=c('+','+'),interc=TRUE,
  maketimes=FALSE,subject_col='Anon.Student.Id'))
source_files <- c('R/lkt-model-gru.R','R/lkt-gru-inference.R','R/lkt-gru-input.R',
  'R/lkt-gru-prediction.R','R/lkt-prediction.R','R/LKTfunctions.R','R/lkt-feature-computation.R',
  'scripts/gru-benchmark-data.R','scripts/gru-grid-utils.R','scripts/compare-gru-inference.R',
  'scripts/summarize-gru-inference.R')
hashes <- setNames(vapply(source_files,function(f)digest::digest(file.path(root,f),algo='sha256',file=TRUE),character(1)),source_files)

# Parameter counts come from the real networks. The extra-head baseline is the nearest
# integer head width to the deterministic inference model's total parameter count;
# its recurrent backbone and inputs remain unchanged.
count_parameters <- function(settings,prep) {
  cleanup <- getFromNamespace('lkt_gru_runtime','LKT')(settings);on.exit(cleanup())
  net <- getFromNamespace('lkt_gru_network','LKT')(prep,settings)
  sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
}
tasks <- list();resolved <- list()
for(family in families) {
  base <- do.call(GRUModel,utils::modifyList(design$common,prepared$families[[family]],keep.null=TRUE))$settings
  # All conditions in one input family share the same training-only preprocessing.
  # Construct that schema once; changing the head does not change input columns.
  prep <- getFromNamespace('lkt_gru_preprocess','LKT')(as.data.frame(d),d$fold==1L,recipe,base)
  det <- base;det$inference_mode <- 'deterministic'
  det_count <- count_parameters(det,prep)
  plain_count <- count_parameters(base,prep)
  input_width <- base$hidden_size+length(prep$numeric_names)+
    if(base$categorical_encoding=='embedding')sum(prep$embedding_dims) else sum(lengths(prep$vocabularies))
  # This control is intentionally one hidden layer so its width has an explicit,
  # monotone parameter-count formula rather than a hidden tuning search.
  if(length(base$head_hidden_sizes)!=1L)stop('The capacity control requires one baseline head hidden layer.')
  head_factor <- input_width+1L+as.integer(base$head_bias)
  output_bias <- as.integer(base$head_bias)
  nonhead <- plain_count-(base$head_hidden_sizes*head_factor+output_bias)
  control_width <- max(1L,as.integer(round((det_count-nonhead-output_bias)/head_factor)))
  stages <- list(baseline=list(inference_mode='none'),
    capacity_control=list(inference_mode='none',head_hidden_sizes=control_width),
    deterministic=list(inference_mode='deterministic'),gaussian=list(inference_mode='gaussian'))
  kl_names <- paste0('gaussian_kl_',format(design$kl_betas,scientific=TRUE,trim=TRUE,digits=16L))
  if(anyDuplicated(kl_names))stop('KL weights do not have unique stage labels.')
  for(j in seq_along(design$kl_betas))stages[[kl_names[j]]] <-
    list(inference_mode='gaussian',temporal_kl_beta=design$kl_betas[j])
  for(stage in names(stages)) {
    settings <- do.call(GRUModel,utils::modifyList(base,stages[[stage]],keep.null=TRUE))$settings
    parameter_count <- count_parameters(settings,prep)
    for(seed in design$seeds) {
      settings$seed <- as.integer(seed)
      run_id <- paste(family,stage,paste0('seed',seed),sep='__')
      resolved[[run_id]] <- settings
      tasks[[run_id]] <- data.frame(run_id=run_id,family=family,stage=stage,seed=seed,
        inference_mode=settings$inference_mode,temporal_kl_beta=settings$temporal_kl_beta,
        parameter_count=parameter_count,head_width=settings$head_hidden_sizes,
        deterministic_parameter_count=det_count,
        settings_sha256=digest::digest(settings,algo='sha256'))
    }
  }
}
runs <- do.call(rbind,tasks);rownames(runs) <- NULL
data_file <- file.path(output,'prepared_data.rds')
if(file.exists(data_file))stopifnot(identical(readRDS(data_file),as.data.frame(d))) else
  gru_grid_atomic_rds(as.data.frame(d),data_file)
protocol <- list(schema=1L,type='staged GRU inference ablation',smoke=action=='smoke',
  dataset='largerawsample',preprocessing=prepared$preprocessing,baseline=baseline,design=design,
  runs=runs,settings=resolved,source_hashes=hashes,
  input_sha256=digest::digest(prepared$data_file,algo='sha256',file=TRUE),
  data_sha256=digest::digest(data_file,algo='sha256',file=TRUE),
  software=list(R=R.version.string,LKT=as.character(packageVersion('LKT')),torch=as.character(packageVersion('torch'))),
  selection='No stage selected on test performance; all prespecified stages and KL weights reported. Early stopping uses validation log loss only.',
  split='Previously inspected seed-1 70/15/15 whole-participant split; exploratory, not cross-validation.',
  inference_evidence='Memory through t-2, encoded completed interaction t-1, and current numeric predictors. Current categorical embeddings enter the decoder. The latent is not fed back into recurrence.',
  gaussian_prediction='One sample per trial during training; mean vector during validation, testing and continuation.',
  temporal_penalty='KL(new || previous), mean over latent dimensions and valid adjacent within-history pairs; includes truncated-chunk boundaries, excludes first states and padding.',
  caveats=c(prepared$caveats,'Architectural ablation, not a reproduction of UNVaMP unpublished settings.',
    'The deterministic stage changes routing and adds a representation network; its difference from baseline is not solely extra depth.',
    'The matched-capacity control changes head width, not recurrent width. Its integer parameter-count mismatch is reported.',
    'Fixed-architecture comparison, not a separate hyperparameter search for every stage.'))
protocol_file <- file.path(output,'inference_protocol.rds')
if(file.exists(protocol_file))stopifnot(identical(readRDS(protocol_file),protocol)) else {
  gru_grid_atomic_rds(protocol,protocol_file)
  writeLines(capture.output(dput(protocol)),file.path(output,'inference_protocol.txt'))
  write.csv(runs,file.path(output,'inference_runs.csv'),row.names=FALSE)
  write.csv(prepared$partition,file.path(output,'participant_split.csv'),row.names=FALSE)
  write.csv(d[,.(participants=uniqueN(Anon.Student.Id),responses=.N),by=fold],file.path(output,'split_counts.csv'),row.names=FALSE)
  for(f in source_files) {
    destination <- file.path(output,'source',f);dir.create(dirname(destination),recursive=TRUE,showWarnings=FALSE)
    stopifnot(file.copy(file.path(root,f),destination,overwrite=FALSE))
  }
}
protocol_sha <- digest::digest(protocol_file,algo='sha256',file=TRUE)
cat('FROZEN:',nrow(runs),'fits;',length(families),'input families;',length(design$seeds),'seeds.\n')
print(unique(runs[,c('family','stage','parameter_count','head_width')]))
if(action=='plan')quit(save='no',status=0)

# Each worker receives immutable data/settings and has its own Torch random stream.
# Checkpoints are accepted only when their frozen protocol and resolved settings match.
run_candidate <- function(run_id) {
  settings <- protocol$settings[[run_id]]
  path <- file.path(output,'models',paste0(run_id,'.rds'))
  if(file.exists(path)) {
    saved <- readRDS(path);stopifnot(identical(saved$protocol_sha256,protocol_sha))
    fit <- saved$fit;elapsed <- saved$elapsed
  } else {
    cat('START',run_id,'\n');flush.console()
    elapsed <- system.time(fit <- LKT(copy(d),usefolds=1,interc=TRUE,
      components=protocol$baseline$components,features=protocol$baseline$features,
      fixedpars=protocol$baseline$fixedpars,model=do.call(GRUModel,settings),verbose=FALSE))[['elapsed']]
    gru_grid_validate_fit(fit,settings,d)
    gru_grid_atomic_rds(list(protocol_sha256=protocol_sha,run_id=run_id,elapsed=elapsed,fit=fit),path)
  }
  gru_grid_validate_fit(fit,settings,d)
  score <- cbind(protocol$runs[match(run_id,protocol$runs$run_id),],
    as.data.frame(as.list(gru_grid_metrics(d$CF..ansbin.[d$fold==2],fit$prediction[d$fold==2]))),
    selected_epoch=fit$selected_epoch,epochs_run=nrow(fit$training_history),elapsed=elapsed)
  gru_grid_atomic_csv(score,file.path(output,'validation',paste0(run_id,'.csv')))
  cat('DONE',run_id,'validation log loss',score$log_loss,'\n');flush.console()
  NULL
}
if(workers>1L) {
  cluster <- parallel::makePSOCKcluster(workers,outfile=file.path(output,'workers.log'))
  parallel::clusterCall(cluster,function(paths) {.libPaths(paths);suppressPackageStartupMessages({library(LKT);library(data.table)});NULL},.libPaths())
  parallel::clusterExport(cluster,c('protocol','protocol_sha','d','output','run_candidate',
    'gru_grid_validate_fit','gru_grid_atomic_rds','gru_grid_atomic_csv','gru_grid_metrics'),envir=.GlobalEnv)
  invisible(tryCatch(parallel::parLapplyLB(cluster,runs$run_id,run_candidate),finally=parallel::stopCluster(cluster)))
} else invisible(lapply(runs$run_id,run_candidate))
source(file.path(root,'scripts/summarize-gru-inference.R'))
summarize_gru_inference(output)
