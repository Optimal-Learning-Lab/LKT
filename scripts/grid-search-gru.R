# Full factorial GRU search on a saved benchmark's participant split.
# Args: LKT root, PolicyFactory root, reference benchmark, output directory,
# workers (3), families (all or comma-separated names), profile (compact/broader),
# action (plan/run), optional additional cache directories separated by semicolons,
# cache policy (exact/none). Cross-platform runs use none and refit every model.
# Step 1: read the requested search and execution settings. Keeping these choices
# explicit lets the same script prepare a plan, run it locally, or run it on Big Blue.
args <- commandArgs(TRUE)
stopifnot(length(args)>=4L,length(args)<=10L)
root <- normalizePath(args[1L],winslash='/',mustWork=TRUE)
policy <- normalizePath(args[2L],winslash='/',mustWork=TRUE)
reference <- normalizePath(args[3L],winslash='/',mustWork=TRUE)
output <- args[4L]
workers <- if(length(args)>=5L)as.integer(args[5L]) else 3L
family_names <- if(length(args)>=6L && args[6L]!='all')trimws(strsplit(args[6L],',',fixed=TRUE)[[1L]]) else
  c('matched','raw','augmented')
profile <- if(length(args)>=7L)args[7L] else 'compact'
action <- if(length(args)>=8L)args[8L] else 'run'
cache_policy <- if(length(args)>=10L)args[10L] else 'exact'
stopifnot(cache_policy %in% c('exact','none'))
cache_dirs <- if(cache_policy=='none')character() else unique(c(reference,
  if(length(args)>=9L && nzchar(args[9L]))strsplit(args[9L],';',fixed=TRUE)[[1L]] else character()))
cache_dirs <- vapply(cache_dirs,normalizePath,character(1),winslash='/',mustWork=TRUE)
stopifnot(length(workers)==1L,!is.na(workers),workers>=1L,!anyDuplicated(family_names),
  length(family_names)>0L,all(family_names %in% c('matched','raw','augmented')),
  action %in% c('plan','run'))
source(file.path(root,'scripts/load-current-lkt.R'))
private_library <- lkt_release[['lkt_library']]
suppressPackageStartupMessages(library(data.table))
stopifnot(torch::torch_is_installed())
source(file.path(root,'scripts/gru-benchmark-data.R'))
source(file.path(root,'scripts/gru-grid-config.R'))
source(file.path(root,'scripts/gru-grid-utils.R'))
if(dir.exists(output) && length(list.files(output)) && !file.exists(file.path(output,'grid_protocol.rds')))
  stop('Existing output has no grid protocol; choose a new directory.')
dir.create(output,recursive=TRUE,showWarnings=FALSE)
output <- normalizePath(output,winslash='/',mustWork=TRUE)
stopifnot(output!=reference,!output %in% cache_dirs)
for(folder in c('models','validation','training','source'))dir.create(file.path(output,folder),showWarnings=FALSE)
# Step 2: recover the reference experiment and its participant split. A hyperparameter
# comparison is interpretable only when the dataset, feature definitions, and held-out
# participants stay the same.
prior <- readRDS(file.path(reference,'protocol.rds'))
if(!identical(prior$schema,3L))stop('Reference uses an archived preprocessing protocol. Use its frozen scripts, or create a new item144/skill36 benchmark before searching.')
stopifnot(identical(prior$preprocessing_source_sha256,
  digest::digest(file.path(root,'scripts/gru-benchmark-data.R'),algo='sha256',file=TRUE)))
workflow <- file.path(policy,'workflows/simulation/paper_rerun_wald')
snapshot <- if(prior$dataset %in% c('largerawsample','samplelkt'))
  file.path(reference,paste0(prior$dataset,'_source.rds')) else NULL
prepared <- prepare_gru_benchmark_data(prior$dataset,root,output,workflow,source_snapshot=snapshot)
data <- prepared$data
# Save both the original input and the prepared rows. The original supports an
# independent label audit, while the prepared copy records the exact ordering and folds
# seen by every model.
prepared_file <- file.path(output,'prepared_data.rds')
if(file.exists(prepared_file))stopifnot(identical(readRDS(prepared_file),as.data.frame(data))) else
  gru_grid_atomic_rds(as.data.frame(data),prepared_file)
input_file <- file.path(output,'input_data.rds')
if(!file.exists(input_file))stopifnot(file.copy(prepared$data_file,input_file,overwrite=FALSE))
stopifnot(identical(digest::digest(input_file,algo='sha256',file=TRUE),prior$input_data_sha256))
# Check the inputs and model implementation before starting expensive fits. These checks
# prevent an unnoticed data, parameter, or code change from being presented as a
# hyperparameter effect.
core_files <- names(prior$sources)
core_hashes <- setNames(vapply(core_files,function(f)
  digest::digest(file.path(root,f),algo='sha256',file=TRUE),character(1)),core_files)
software <- list(R=R.version.string,LKT=as.character(packageVersion('LKT')),
  torch=as.character(packageVersion('torch')))
stopifnot(identical(core_hashes,prior$sources),
  identical(software[c('LKT','torch')],prior$software[c('LKT','torch')]),
  identical(digest::digest(prepared$data_file,algo='sha256',file=TRUE),prior$input_data_sha256),
  identical(digest::digest(file.path(workflow,'model_parameters.R'),algo='sha256',file=TRUE),prior$input_parameters_sha256),
  identical(prepared$partition,read.csv(file.path(reference,'participant_split.csv'),stringsAsFactors=FALSE)),
  identical(as.data.frame(prepared$counts),prior$counts),
  identical(prepared$preprocessing,prior$preprocessing),identical(prepared$families,prior$families))

# Index only provenance-compatible earlier fits, then reuse only exact resolved
# GRU settings matches, including the seed, stopping controls and epoch budget.
if(cache_policy=='exact')stopifnot(identical(software,prior$software))
cache_index <- list();cache_protocols <- list()
shared <- c('schema','dataset','data_description','timing_policy','preprocessing','preprocessing_source_sha256','input_data_sha256',
  'input_parameters_sha256','baseline','split_seed','split_fractions','counts',
  'families','seeds','common','software','sources')
for(directory in cache_dirs) {
  p <- readRDS(file.path(directory,'protocol.rds'))
  stopifnot(identical(p[shared],prior[shared]),
    identical(prepared$partition,read.csv(file.path(directory,'participant_split.csv'),stringsAsFactors=FALSE)))
  scores <- read.csv(file.path(directory,'validation_candidates.csv'),stringsAsFactors=FALSE)
  cache_protocols[[directory]] <- digest::digest(file.path(directory,'protocol.rds'),algo='sha256',file=TRUE)
  for(i in seq_len(nrow(scores))) {
    row <- scores[i,]
    path <- file.path(directory,paste0(paste(row$family,row$architecture,row$seed,sep='_'),'.rds'))
    fit <- readRDS(path)
    gru_grid_validate_fit(fit,fit$gru_settings,data)
    cache_index[[length(cache_index)+1L]] <- data.frame(
      settings_sha256=digest::digest(fit$gru_settings,algo='sha256'),cache_file=path,
      cache_sha256=digest::digest(path,algo='sha256',file=TRUE),cache_fit_seconds=row$fit_seconds)
  }
}
cache_index <- if(length(cache_index))do.call(rbind,cache_index) else
  data.frame(settings_sha256=character(),cache_file=character(),cache_sha256=character(),cache_fit_seconds=numeric())
stopifnot(!anyDuplicated(cache_index$settings_sha256))
# Step 3: expand every combination in the chosen grid. The compact profile varies GRU
# depth, hidden width, and learning rate while holding the other controls fixed.
axes <- gru_grid_axes(profile)
stopifnot(identical(names(axes),c('num_layers','hidden_size','learning_rate','dropout','weight_decay')),
  all(lengths(axes)>0L),all(vapply(axes,function(x)!anyNA(x)&&!anyDuplicated(x),logical(1))))
grid <- do.call(expand.grid,c(axes,list(KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE)))
grid$grid_id <- sprintf('g%03d',seq_len(nrow(grid)))
candidates <- list();runs <- list();run_settings <- list()
# Create a separate task for each input version, configuration, and training seed.
# Resolving the complete GRU settings now makes the intended experiment inspectable
# before any training starts.
for(f in family_names)for(i in seq_len(nrow(grid))) {
  row <- grid[i,]
  candidate_id <- paste(f,row$grid_id,sep='_')
  candidates[[candidate_id]] <- cbind(data.frame(candidate_id=candidate_id,family=f),row)
  for(seed in prior$seeds) {
    run_id <- paste0(candidate_id,'_seed',seed)
    opts <- prior$common
    opts[names(prior$families[[f]])] <- prior$families[[f]]
    opts[c('num_layers','hidden_size','learning_rate','weight_decay',
           'input_dropout','head_dropout','gru_dropout','seed')] <- list(
      row$num_layers,                              # GRU depth
      row$hidden_size,                             # Units in each layer
      row$learning_rate,                           # Optimizer step size
      row$weight_decay,                            # AdamW regularization
      row$dropout,                                 # Input dropout
      row$dropout,                                 # Prediction-head dropout
      if(row$num_layers>1L)row$dropout else 0,      # Between-layer dropout
      seed                                        # Repeated training seed
    )
    settings <- do.call(GRUModel,opts)$settings
    settings_sha256 <- digest::digest(settings,algo='sha256')
    # A previous fit is reusable only when its entire resolved settings list matches,
    # including the seed. A similar architecture or a different stopping budget is a
    # different experiment.
    cached <- cache_index[match(settings_sha256,cache_index$settings_sha256),]
    run_settings[[run_id]] <- settings
    runs[[run_id]] <- cbind(data.frame(run_id=run_id,candidate_id=candidate_id,family=f,seed=seed),
      row[,names(axes)],data.frame(settings_sha256=settings_sha256),cached[,c('cache_file','cache_sha256','cache_fit_seconds')])
  }
}
candidates <- do.call(rbind,candidates);rownames(candidates) <- NULL
runs <- do.call(rbind,runs);rownames(runs) <- NULL
stopifnot(!anyDuplicated(runs$settings_sha256))
# Record the search scripts as well as the model source. This preserves the experiment
# driver that decided which candidates to fit and how to choose among them.
script_files <- c('scripts/grid-search-gru.R','scripts/gru-grid-config.R',
  'scripts/gru-grid-utils.R','scripts/gru-benchmark-data.R',
  'scripts/summarize-gru-grid.R','scripts/verify-gru-grid.R')
script_hashes <- setNames(vapply(script_files,function(f)
  digest::digest(file.path(root,f),algo='sha256',file=TRUE),character(1)),script_files)
# Keep a copy of the fitted PPES reference in the output. Final scoring can then use the
# original comparison model without depending on a later change to that external folder.
baseline_source <- file.path(reference,'ppes_baseline.rds')
baseline_file <- file.path(output,'ppes_baseline.rds')
if(!file.exists(baseline_file))stopifnot(file.copy(baseline_source,baseline_file,overwrite=FALSE))
stopifnot(identical(digest::digest(baseline_source,algo='sha256',file=TRUE),
  digest::digest(baseline_file,algo='sha256',file=TRUE)))
# Step 4: assemble the complete search protocol, including candidates, seeds,
# provenance, and selection rule. The saved record distinguishes a fresh Linux search
# from reuse of compatible earlier fits.
protocol <- list(schema=2L,type='GRU full factorial grid search',profile=profile,
  dataset=prior$dataset,data_description=prepared$data_description,timing_policy=prepared$timing_policy,
  preprocessing=prepared$preprocessing,
  input_data_sha256=prior$input_data_sha256,input_parameters_sha256=prior$input_parameters_sha256,
  prepared_data_sha256=digest::digest(prepared_file,algo='sha256',file=TRUE),
  baseline=prior$baseline,baseline_filename='ppes_baseline.rds',baseline_source=baseline_source,
  baseline_checkpoint_sha256=digest::digest(baseline_file,algo='sha256',file=TRUE),
  split_seed=prior$split_seed,counts=prior$counts,seeds=prior$seeds,
  axes=axes,candidates=candidates,runs=runs,run_settings=run_settings,
  common=prior$common,families=prior$families[family_names],
  selection='Lowest mean validation log loss across all three training seeds, separately for each input family. Ties use candidate_id. Test scores are calculated only after the complete grid choices are saved.',
  cache_policy=cache_policy,cache_protocols=cache_protocols,reference_directory=reference,
  reference_protocol_sha256=digest::digest(file.path(reference,'protocol.rds'),algo='sha256',file=TRUE),
  reference_test_predictions_sha256=digest::digest(file.path(reference,'test_predictions.csv'),algo='sha256',file=TRUE),
  core_sources=core_hashes,script_sources=script_hashes,software=software,
  reference_software=prior$software,execution_platform=R.version$platform,
  execution_container_sha256=Sys.getenv('LKT_GRU_CONTAINER_SHA'),
  caveats=c(prepared$caveats,
    'This grid uses the previously inspected participant split. It is exploratory, not a new confirmatory test or cross-validation.',
    'Selection uses validation only; the test set is not used to rank grid candidates. Response inclusion follows the saved preprocessing and filtering audits.',
    'The three seed metrics describe repeated single models, not an ensemble. Bootstrap intervals condition on the chosen configuration and this participant split.',
    'A 100-epoch maximum and fixed early-stopping controls apply to every fit. Epoch-cap hits are reported; this grid does not search every exposed GRU control.',
    'Dropout is a shared grid setting for input/head dropout and, with multiple layers, between-layer GRU dropout. Those dropout locations are not searched independently.',
    'PPES nonlinear parameters remain fixed; this grid tunes the GRUs only.'))
# Freeze the protocol before fitting. A resumed run must match it exactly, so an
# interrupted search cannot quietly resume with changed settings.
protocol_file <- file.path(output,'grid_protocol.rds')
if(file.exists(protocol_file)) {
  if(!identical(protocol,readRDS(protocol_file)))stop('Existing grid protocol differs; choose a new output directory.')
} else {
  gru_grid_atomic_rds(protocol,protocol_file)
  writeLines(capture.output(dput(protocol)),file.path(output,'grid_protocol.txt'))
  write.csv(candidates,file.path(output,'grid_candidates.csv'),row.names=FALSE)
  write.csv(runs,file.path(output,'grid_runs.csv'),row.names=FALSE)
  write.csv(prepared$partition,file.path(output,'participant_split.csv'),row.names=FALSE)
  write.csv(prepared$counts,file.path(output,'split_counts.csv'),row.names=FALSE)
  for(f in script_files)file.copy(file.path(root,f),file.path(output,'source',basename(f)),overwrite=FALSE)
}
protocol_sha256 <- digest::digest(protocol_file,algo='sha256',file=TRUE)
cat('GRID PROTOCOL FROZEN\nConfigurations:',nrow(candidates),'Seeds:',length(prior$seeds),
  'Total fits:',nrow(runs),'Exact prior fits:',sum(!is.na(runs$cache_file)),
  'New fits:',sum(is.na(runs$cache_file)),'Workers:',workers,'\n')
print(prepared$counts)
# Planning ends here without training a model. This gives us a concrete grid and
# provenance record to inspect before allocating the full compute budget.
if(action=='plan')quit(save='no',status=0)

# Check numerical identity of the baseline's engineered inputs before fitting.
baseline <- readRDS(file.path(output,protocol$baseline_filename))
recipe <- list(components=protocol$baseline$components,features=protocol$baseline$features,
  fixedpars=protocol$baseline$fixedpars,interacts=rep(NA_character_,length(protocol$baseline$features)),
  connectors=rep('+',length(protocol$baseline$features)-1L),interc=TRUE,maketimes=FALSE,subject_col='Anon.Student.Id')
frame <- getFromNamespace('lkt_gru_feature_frame','LKT')(data,recipe)
x <- model.matrix(frame$formula,frame$data);base_x <- as.matrix(baseline$predictors)
colnames(base_x) <- rownames(baseline$coefs)
stopifnot(setequal(colnames(x),colnames(base_x)))
difference <- max(abs(x-base_x[,colnames(x),drop=FALSE]))
stopifnot(is.finite(difference),difference<1e-12)
writeLines(sprintf('Maximum absolute engineered feature difference: %.17g',difference),file.path(output,'feature_identity.txt'))
rm(baseline,frame,x,base_x);gc()
# Step 5: distribute independent fits across the available R workers. Each candidate
# controls its own random seed and Torch thread count, so worker scheduling does not
# select its initial conditions.
if(workers>1L) {
  cluster <- parallel::makePSOCKcluster(workers,outfile=file.path(output,'workers.log'))
  parallel::clusterCall(cluster,function(paths) {
    .libPaths(paths);suppressPackageStartupMessages({library(LKT);library(data.table)});NULL
  },.libPaths())
  parallel::clusterExport(cluster,c('protocol','protocol_sha256','data','output',
    'gru_grid_atomic_rds','gru_grid_atomic_csv','gru_grid_validate_fit','gru_grid_metrics','gru_grid_run_candidate'),envir=.GlobalEnv)
  scores <- tryCatch(parallel::parLapplyLB(cluster,protocol$runs$run_id,
    gru_grid_run_candidate,chunk.size=1L),finally=parallel::stopCluster(cluster))
} else scores <- lapply(protocol$runs$run_id,gru_grid_run_candidate)
cat('All grid fits completed. Saving validation-only choices and scoring selected models.\n')
# Step 6: choose configurations using validation scores, then score their test
# predictions. The final independent verifier reconstructs the choices and metrics from
# the saved artifacts.
source(file.path(root,'scripts/summarize-gru-grid.R'),local=TRUE)
gru_grid_report(output,finalize=TRUE)
source(file.path(root,'scripts/verify-gru-grid.R'),local=TRUE)
verify_gru_grid(output)
