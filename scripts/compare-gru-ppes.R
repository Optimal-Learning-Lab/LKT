# Reproducible predictive comparison with the next simulation's configured
# PPES tracer. Args: LKT root, PolicyFactory root, new output directory, workers,
# dataset (largerawsample by default, samplelkt, or calibration), optional
# comma-separated layer counts (default 1,2), optional prior benchmark directory.
# Original data/settings are preserved.
# Step 1: choose the dataset, output folder, and GRU depths to compare. A separate
# output folder preserves earlier experiments when the comparison is extended.
args <- commandArgs(TRUE)
root <- normalizePath(if(length(args)>=1L)args[1L] else '.',winslash='/')
policy <- normalizePath(if(length(args)>=2L)args[2L] else 'C:/dev/PolicyFactory2',winslash='/')
dataset <- if(length(args)>=5L)args[5L] else 'largerawsample'
if(!dataset %in% c('calibration','samplelkt','largerawsample'))stop('Select calibration, samplelkt, or largerawsample explicitly.')
layer_tokens <- trimws(strsplit(if(length(args)>=6L)args[6L] else '1,2',',',fixed=TRUE)[[1L]])
layers <- suppressWarnings(as.integer(layer_tokens))
stopifnot(length(layers)>0L,all(grepl('^[1-9][0-9]*$',layer_tokens)),!anyNA(layers),!anyDuplicated(layers))
reference <- if(length(args)>=7L)normalizePath(args[7L],winslash='/',mustWork=TRUE) else NULL
output <- if(length(args)>=3L)args[3L] else file.path(root,'../LKT-outputs',
  paste0(if(dataset=='calibration')'gru-vs-ppes-20261101' else paste0('gru-vs-ppes-',dataset,'-20261101'),
    if(dataset=='largerawsample')'-item144-skill36' else '',
    if(!identical(layers,c(1L,2L)))paste0('-layers-',paste(layers,collapse='-')) else ''))
workers <- if(length(args)>=4L)as.integer(args[4L]) else 3L
stopifnot(length(workers)==1L,!is.na(workers),workers>=1L)
source(file.path(root,'scripts/load-current-lkt.R'))
suppressPackageStartupMessages({library(LKT);library(data.table)})
stopifnot(torch::torch_is_installed())
if(dir.exists(output) && length(list.files(output,all.files=FALSE)) && !file.exists(file.path(output,'protocol.rds')))
  stop('Existing output has no comparison protocol; use a new directory.')
if(file.exists(file.path(output,'protocol.rds')) &&
   !identical(readRDS(file.path(output,'protocol.rds'))$schema,3L))
  stop('Archived comparison protocol: use its frozen scripts or choose a new output directory.')
dir.create(output,recursive=TRUE,showWarnings=FALSE)

# Read the PPES specification from the upcoming simulation rather than inventing a new
# reference model. Its nonlinear settings stay fixed while the baseline fits its linear
# coefficients on training participants.
workflow <- file.path(policy,'workflows/simulation/paper_rerun_wald')
parameter_file <- file.path(workflow,'model_parameters.R')
settings <- new.env(parent=baseenv());sys.source(parameter_file,envir=settings)
parameters <- settings$paper_ppes_tracing_parameters
components <- c('Anon.Student.Id','KC..Default.','KC..Default.')
features <- c('logitdec','ppes','logitdec')
source(file.path(root,'scripts/gru-benchmark-data.R'))
prepared <- prepare_gru_benchmark_data(dataset,root,output,workflow)
data <- prepared$data;data_file <- prepared$data_file
data_description <- prepared$data_description;timing_policy <- prepared$timing_policy
caveats <- prepared$caveats;partition <- prepared$partition;counts <- prepared$counts

# Declare every candidate before fitting or scoring GRU test metrics in this run.
# A supplied reference explicitly records that this is a follow-up experiment.
# Input families separate the matched current-feature comparison from a raw
# KC/time model and the effect of adding the cognitive features to raw inputs.
# Step 2: separate the effect of recurrent prediction from the effect of adding inputs.
# The matched version uses PPES features, the raw version uses KC/time inputs, and the
# augmented version combines them.
families <- prepared$families
architecture_names <- ifelse(layers==1L,'single',ifelse(layers==2L,'stacked',paste0('layers_',layers)))
architectures <- stats::setNames(lapply(layers,function(n)
  list(hidden_size=32L,num_layers=n,gru_dropout=if(n>1L).1 else 0)),architecture_names)
seeds <- 1:3
# Keep the remaining training controls identical across candidates. The value
# validation_folds = 2 selects partition label 2; it does not request two-fold cross-
# validation.
common <- list(
  head_hidden_sizes = 32L,
  head_activation = 'relu',
  input_dropout = .1,
  head_dropout = .1,
  embedding_dims = 16L,
  optimizer = 'adamw',
  learning_rate = .001,
  weight_decay = 1e-4,
  epochs = 100L,
  batch_size = 32L,
  sequence_length = 100L,
  gradient_clip = 5,
  validation_folds = 2,
  early_stopping = TRUE,
  patience = 10L,
  min_delta = 1e-4,
  restore_best = TRUE,
  device = 'cpu',
  num_threads = 1L,
  order_col = 'CF..Time.',
  clip_epsilon = 1e-5
)
# Save the complete design and source fingerprints before fitting. This makes the
# comparison reviewable and prevents incompatible checkpoints from being resumed
# together.
protocol <- list(schema=3L,dataset=dataset,data_description=data_description,timing_policy=timing_policy,
  preprocessing=prepared$preprocessing,
  preprocessing_source_sha256=digest::digest(file.path(root,'scripts/gru-benchmark-data.R'),algo='sha256',file=TRUE),
  input_data_sha256=digest::digest(data_file,algo='sha256',file=TRUE),
  input_parameters_sha256=digest::digest(parameter_file,algo='sha256',file=TRUE),
  baseline=list(components=components,features=features,fixedpars=parameters,intercept=TRUE),
  split_seed=1L,split_fractions=c(.7,.15,.15),counts=as.data.frame(counts),
  selection='Lowest mean validation log loss across three training seeds, separately by input family.',
  families=families,architectures=architectures,seeds=seeds,common=common,
  caveats=c(caveats,
    'This evaluates human response prediction, not scheduling, learning gains, simulated retention, or simulation throughput.'),
  software=list(R=R.version.string,LKT=as.character(packageVersion('LKT')),torch=as.character(packageVersion('torch'))),
  sources=stats::setNames(vapply(c('R/lkt-model-gru.R','R/lkt-gru-input.R','R/lkt-gru-prediction.R',
    'R/lkt-feature-computation.R','R/LKTfunctions.R'),function(f)digest::digest(file.path(root,f),algo='sha256',file=TRUE),character(1)),
    c('R/lkt-model-gru.R','R/lkt-gru-input.R','R/lkt-gru-prediction.R','R/lkt-feature-computation.R','R/LKTfunctions.R')))
if(!is.null(reference)) {
  stopifnot(normalizePath(output,winslash='/',mustWork=TRUE)!=reference)
  prior <- readRDS(file.path(reference,'protocol.rds'))
  shared <- c('schema','dataset','data_description','timing_policy','preprocessing','preprocessing_source_sha256','input_data_sha256',
    'input_parameters_sha256','baseline','split_seed','split_fractions','counts',
    'families','seeds','common','software','sources')
  stopifnot(identical(protocol[shared],prior[shared]),
    identical(partition,read.csv(file.path(reference,'participant_split.csv'),stringsAsFactors=FALSE)))
  protocol$followup_reference <- list(directory=reference,
    protocol_sha256=digest::digest(file.path(reference,'protocol.rds'),algo='sha256',file=TRUE),
    test_predictions_sha256=digest::digest(file.path(reference,'test_predictions.csv'),algo='sha256',file=TRUE))
  protocol$caveats <- c(protocol$caveats,
    'Exploratory follow-up requested after inspecting the reference benchmark test results. This reuses that test split and is not a fresh confirmatory evaluation or cross-validation.')
}
if(file.exists(file.path(output,'protocol.rds'))) {
  if(!identical(readRDS(file.path(output,'protocol.rds')),protocol))stop('Existing results use a different protocol/source; choose a new directory.')
} else {
  saveRDS(protocol,file.path(output,'protocol.rds'))
  writeLines(capture.output(dput(protocol)),file.path(output,'protocol.txt'))
  write.csv(partition,file.path(output,'participant_split.csv'),row.names=FALSE)
  write.csv(counts,file.path(output,'split_counts.csv'),row.names=FALSE)
}
cat('Frozen comparison protocol saved. Counts:\n');print(counts)

metrics <- function(y,p) {
  stopifnot(length(y)==length(p),length(y)>0,all(is.finite(p)),all(p>0 & p<1))
  c(log_loss=-mean(dbinom(y,1,p,log=TRUE)),AUC=as.numeric(pROC::auc(y,p,levels=c(0,1),direction='<',quiet=TRUE)),
    Brier=mean((y-p)^2),RMSE=sqrt(mean((y-p)^2)))
}
score_fit <- function(fit,label,family,architecture,seed,seconds) {
  do.call(rbind,lapply(1:3,function(fold) {
    keep <- data$fold==fold
    data.frame(model=label,family=family,architecture=architecture,seed=seed,
      partition=c('training','validation','test')[fold],participants=uniqueN(data$Anon.Student.Id[keep]),
      trials=sum(keep),as.list(metrics(data$CF..ansbin.[keep],fit$prediction[keep])),
      fit_seconds=seconds,selected_epoch=if(is.null(fit$selected_epoch))NA_integer_ else fit$selected_epoch,
      epochs_run=if(is.null(fit$training_history))NA_integer_ else nrow(fit$training_history),
      parameter_count=if(is.null(fit$parameter_count))nrow(fit$coefs) else fit$parameter_count)
  }))
}

# Step 3: obtain the PPES reference using the same prepared response rows. Reuse its
# saved fit only within the already verified comparison protocol.
if(file.exists(file.path(output,'ppes_baseline.rds'))) {
  baseline <- readRDS(file.path(output,'ppes_baseline.rds'))
  baseline_scores <- read.csv(file.path(output,'ppes_scores.csv'))
} else {
  elapsed <- system.time(baseline <- LKT(copy(data),usefolds=1,interc=TRUE,components=components,
    features=features,fixedpars=parameters,epsilon=1e-12,factrv=1e4,maxitv=1000,verbose=FALSE))[['elapsed']]
  saveRDS(baseline,file.path(output,'ppes_baseline.rds'))
  baseline_scores <- score_fit(baseline,'PPES + student/KC logitdec','ppes','configured',NA_integer_,elapsed)
  write.csv(baseline_scores,file.path(output,'ppes_scores.csv'),row.names=FALSE)
}

# Verify numeric identity of the historical cached baseline features and the
# GRU's rebuilt causal features before calling their inputs matched.
# Verify that the matched GRU and PPES really start from the same engineered design
# columns. A feature mismatch would make their performance difference difficult to
# interpret.
recipe <- list(components=components,features=features,fixedpars=parameters,interacts=rep(NA_character_,3),
  connectors=c('+','+'),interc=TRUE,maketimes=FALSE,subject_col='Anon.Student.Id')
frame <- getFromNamespace('lkt_gru_feature_frame','LKT')(data,recipe)
x <- model.matrix(frame$formula,frame$data)
base_x <- as.matrix(baseline$predictors)
colnames(base_x) <- rownames(baseline$coefs)
stopifnot(setequal(colnames(x),colnames(base_x)))
feature_difference <- max(abs(x-base_x[,colnames(x),drop=FALSE]))
stopifnot(is.finite(feature_difference),feature_difference<1e-12)
writeLines(sprintf('Maximum absolute difference between baseline and GRU engineered inputs: %.17g',feature_difference),
  file.path(output,'feature_identity.txt'))
cat('Baseline fitted; matched feature identity verified:',feature_difference,'\n')

previous_scores <- if(file.exists(file.path(output,'validation_candidates.csv')))read.csv(file.path(output,'validation_candidates.csv')) else NULL
# Step 4: fit one input version, depth, and seed, or resume its completed checkpoint.
# Return validation results without using test scores to choose the architecture.
run_candidate <- function(task) {
  family <- task$family;architecture <- task$architecture;seed <- task$seed
  label <- paste(family,architecture,seed,sep='_')
  opts <- c(common,families[[family]],architectures[[architecture]],list(seed=seed))
  stopifnot(!anyDuplicated(names(opts)))
  model_file <- file.path(output,paste0(label,'.rds'))
  score_file <- file.path(output,paste0(label,'_validation.csv'))
  if(file.exists(model_file)) {
    fit <- readRDS(model_file)
    stopifnot(identical(fit$gru_settings,do.call(GRUModel,opts)$settings))
    old <- if(file.exists(score_file))read.csv(score_file) else if(!is.null(previous_scores))
      previous_scores[previous_scores$family==family & previous_scores$architecture==architecture & previous_scores$seed==seed,] else NULL
    elapsed <- if(!is.null(old) && nrow(old)==1L)old$fit_seconds else NA_real_
    cat('RESUME',label,'\n');flush.console()
  } else {
    cat('START',label,format(Sys.time(),'%H:%M:%S'),'\n');flush.console()
    elapsed <- system.time(fit <- LKT(copy(data),usefolds=1,interc=TRUE,components=components,
      features=features,fixedpars=parameters,model=do.call(GRUModel,opts),verbose=FALSE))[['elapsed']]
    # The shared source is already hashed. Keep each checkpoint self-contained
    # for prediction without duplicating 58 raw-data columns in every fit.
    compact <- fit;compact$newdata <- NULL;compact$nullmodel <- NULL
    temporary <- paste0(model_file,'.tmp')
    saveRDS(compact,temporary)
    if(!file.rename(temporary,model_file))stop('Cannot commit candidate checkpoint: ',label)
    write.csv(fit$training_history,file.path(output,paste0(label,'_training.csv')),row.names=FALSE)
  }
  stopifnot(identical(fit$fit_rows,which(data$fold==1)),identical(fit$validation_rows,which(data$fold==2)),
    identical(fit$evaluation_rows,which(data$fold==3)))
  # The selection table contains validation only. No test metric is printed or
  # used until all candidates have been trained and the choices are saved.
  val <- metrics(data$CF..ansbin.[data$fold==2],fit$prediction[data$fold==2])
  score <- data.frame(family=family,architecture=architecture,seed=seed,
    as.list(val),fit_seconds=elapsed,selected_epoch=fit$selected_epoch,epochs_run=nrow(fit$training_history),parameter_count=fit$parameter_count)
  write.csv(score,score_file,row.names=FALSE)
  cat('DONE',label,'validation log loss',format(val['log_loss'],digits=6),'epochs',nrow(fit$training_history),
    'selected',fit$selected_epoch,'seconds',round(elapsed,1),'\n');flush.console()
  list(label=label,score=score,validation=fit$prediction[data$fold==2],test=fit$prediction[data$fold==3])
}
tasks <- list()
for(family in names(families))for(architecture in names(architectures))for(seed in seeds)
  tasks[[length(tasks)+1L]] <- list(family=family,architecture=architecture,seed=seed)
# Run independent candidates in separate R processes. Each task has an explicit seed, so
# parallel scheduling is not part of the model specification.
if(workers>1L) {
  cluster <- parallel::makePSOCKcluster(workers,outfile=file.path(output,'workers.log'))
  parallel::clusterCall(cluster,function(paths) {
    .libPaths(paths);suppressPackageStartupMessages({library(LKT);library(data.table)});NULL
  },.libPaths())
  parallel::clusterExport(cluster,c('common','families','architectures','data','components','features','parameters',
    'output','previous_scores','metrics','run_candidate'),envir=.GlobalEnv)
  results <- tryCatch(parallel::parLapplyLB(cluster,tasks,run_candidate,chunk.size=1L),finally=parallel::stopCluster(cluster))
} else results <- lapply(tasks,run_candidate)
scores <- stats::setNames(lapply(results,`[[`,'score'),vapply(results,`[[`,character(1),'label'))
test_predictions <- stats::setNames(lapply(results,`[[`,'test'),names(scores))
write.csv(do.call(rbind,scores),file.path(output,'validation_candidates.csv'),row.names=FALSE)
validation <- as.data.table(do.call(rbind,scores))
# Step 5: choose depth by average validation log loss across the three seeds. Save the
# choices before calculating the reported GRU test metrics.
selection <- validation[,.(mean_validation_log_loss=mean(log_loss),sd_validation_log_loss=sd(log_loss)),by=.(family,architecture)]
setorderv(selection,c('family','mean_validation_log_loss','architecture'))
chosen <- selection[,.SD[1L],by=family]
write.csv(selection,file.path(output,'validation_architectures.csv'),row.names=FALSE)
write.csv(chosen,file.path(output,'selected_architectures.csv'),row.names=FALSE)

test_rows <- which(data$fold==3);y <- data$CF..ansbin.[test_rows]
participant <- as.character(data$Anon.Student.Id[test_rows]);base_p <- baseline$prediction[test_rows]
test_scores <- list(baseline=data.frame(model='PPES + student/KC logitdec',family='ppes',architecture='configured',seed=NA_integer_,
  as.list(metrics(y,base_p)),fit_seconds=baseline_scores$fit_seconds[1],selected_epoch=NA_integer_))
long <- list(data.frame(source_row=data$comparison_source_row[test_rows],participant=participant,observed=y,
  phase=data$Level..Phase.[test_rows],condition=data$condition_2_name[test_rows],family='ppes',architecture='configured',seed=NA_integer_,prediction=base_p))
differences <- list();per_participant <- list()
base_loss <- -dbinom(y,1,base_p,log=TRUE)
set.seed(20261101L)
test_ids <- sort(unique(participant),method='radix')
# Estimate uncertainty by resampling test participants with their whole response
# sequences. Response-level resampling would ignore dependence within a participant.
draws <- replicate(2000L,sample.int(length(test_ids),length(test_ids),replace=TRUE))
for(family in chosen$family) {
  architecture <- chosen$architecture[chosen$family==family]
  losses <- matrix(NA_real_,length(y),length(seeds))
  for(j in seq_along(seeds)) {
    seed <- seeds[j];label <- paste(family,architecture,seed,sep='_');p <- test_predictions[[label]]
    candidate_index <- which(validation$family==family & validation$architecture==architecture & validation$seed==seed)
    candidate <- validation[candidate_index]
    test_scores[[label]] <- data.frame(model=paste('GRU',family),family=family,architecture=architecture,seed=seed,
      as.list(metrics(y,p)),fit_seconds=candidate$fit_seconds,selected_epoch=candidate$selected_epoch)
    losses[,j] <- -dbinom(y,1,p,log=TRUE)
    long[[label]] <- data.frame(source_row=data$comparison_source_row[test_rows],participant=participant,observed=y,
      phase=data$Level..Phase.[test_rows],condition=data$condition_2_name[test_rows],family=family,architecture=architecture,seed=seed,prediction=p)
  }
  # Average losses across independent training seeds, not probabilities: these
  # are repeated single models, not an ensemble.
  delta <- rowMeans(losses)-base_loss
  totals <- vapply(test_ids,function(id)sum(delta[participant==id]),numeric(1))
  n <- vapply(test_ids,function(id)sum(participant==id),integer(1))
  bootstrap <- apply(draws,2,function(idx)sum(totals[idx])/sum(n[idx]))
  ci <- quantile(bootstrap,c(.025,.975),names=FALSE)
  differences[[family]] <- data.frame(family=family,GRU_minus_PPES_log_loss=mean(delta),
    lower_95=ci[1],upper_95=ci[2],resamples=2000L,unit='participant',training_seeds=length(seeds))
  per_participant[[family]] <- data.frame(participant=test_ids,family=family,trials=n,
    GRU_log_loss=vapply(test_ids,function(id)mean(rowMeans(losses)[participant==id]),numeric(1)),
    PPES_log_loss=vapply(test_ids,function(id)mean(base_loss[participant==id]),numeric(1)))
}
test_scores <- as.data.table(do.call(rbind,test_scores))
write.csv(test_scores,file.path(output,'test_scores_by_seed.csv'),row.names=FALSE)
write.csv(do.call(rbind,long),file.path(output,'test_predictions.csv'),row.names=FALSE)
write.csv(do.call(rbind,per_participant),file.path(output,'participant_scores.csv'),row.names=FALSE)
write.csv(do.call(rbind,differences),file.path(output,'paired_log_loss_differences.csv'),row.names=FALSE)
# Average the three fitted models' metrics rather than averaging their probabilities.
# The reported seed variation therefore describes repeated single models.
summary <- test_scores[,.(runs=.N,log_loss=mean(log_loss),log_loss_sd=if(.N>1L)sd(log_loss) else 0,
  AUC=mean(AUC),AUC_sd=if(.N>1L)sd(AUC) else 0,Brier=mean(Brier),RMSE=mean(RMSE),
  mean_fit_seconds=mean(fit_seconds),mean_selected_epoch=mean(selected_epoch)),by=.(model,family,architecture)]
write.csv(summary,file.path(output,'test_summary.csv'),row.names=FALSE)
saveRDS(list(protocol=protocol,selection=chosen,scores=test_scores,summary=summary,
  differences=do.call(rbind,differences)),file.path(output,'comparison.rds'))

html_table <- function(d,digits=5L) {
  d <- as.data.frame(d)
  d[] <- lapply(d,function(x)if(is.numeric(x))format(round(x,digits),trim=TRUE,scientific=FALSE) else as.character(x))
  escape <- function(x)gsub('<','&lt;',gsub('&','&amp;',x,fixed=TRUE),fixed=TRUE)
  paste0('<table><thead><tr>',paste0('<th>',escape(names(d)),'</th>',collapse=''),'</tr></thead><tbody>',
    paste(vapply(seq_len(nrow(d)),function(i)paste0('<tr>',paste0('<td>',escape(unlist(d[i,],use.names=FALSE)),'</td>',collapse=''),'</tr>'),character(1)),collapse=''),'</tbody></table>')
}
report <- paste0('<!doctype html><meta charset="utf-8"><title>GRU versus upcoming PPES tracer</title>',
  '<style>body{font:16px system-ui;max-width:1200px;margin:32px auto;padding:0 20px;color:#18212b}table{border-collapse:collapse;margin:20px 0;font-size:14px}td,th{border:1px solid #ccd4dc;padding:8px;text-align:right}th{background:#eef3f7}h1,h2{color:#173e5c}li{margin:8px 0}code{background:#eef3f7;padding:2px 5px}</style>',
  '<h1>GRU versus the upcoming simulation PPES tracer</h1><p>Predictive comparison on ',data_description,
  '. Whole-participant split, seed 1; every response retained.</p><p>Timing: ',timing_policy,'</p>',
  '<h2>Partitions</h2>',html_table(counts),
  '<h2>Held-out results</h2><p>GRU metrics are means across three separately trained models, not an ensemble. Log-loss and AUC SDs describe training-seed variation. Lower log loss, Brier, and RMSE are better; higher AUC is better.</p>',html_table(summary),
  '<h2>Paired participant-bootstrap log-loss differences</h2><p>Negative values favor GRU. Intervals resample held-out participants, preserving their complete response sequences; they are conditional on the dataset, frozen PPES settings, selected architecture, and three training seeds.</p>',html_table(do.call(rbind,differences)),
  '<h2>Inputs and selection</h2><ul><li>PPES: global intercept plus student logitdec, KC PPES, and student-KC logitdec. Nonlinear parameters: ',paste(parameters,collapse=', '),'. Linear coefficients are fitted on training participants.</li>',
  '<li>GRU matched: exactly those four current design-matrix columns plus causal response history; no raw KC or elapsed-time predictor. Engineered inputs agree numerically with the PPES baseline to ',format(feature_difference,digits=6),'.</li>',
  '<li>GRU raw: KC embeddings, log(1 + elapsed seconds since the previous student interaction), and past responses.</li>',
  '<li>GRU augmented: raw inputs plus the fixed PPES/logitdec design-matrix columns.</li>',
  '<li>',length(architectures),' declared architecture(s) per family (GRU layer counts: ',paste(layers,collapse=', '),
  '; 32 units per layer), three seeds each; one 32-unit ReLU prediction-head layer. Select among the declared architectures by mean validation log loss only. Other resolved controls and source hashes are in protocol.txt and the fitted RDS files.</li></ul>',
  '<h2>Validation-selected architectures</h2>',html_table(chosen),
  '<h2>Limits</h2><ul>',paste0('<li>',protocol$caveats,'</li>',collapse=''),
  '<li>One participant split and a limited prespecified neural search do not establish a universal winner. The participant-bootstrap interval does not include uncertainty from earlier item/parameter selection or all possible hyperparameters.</li>',
  '<li>Fit wall times include preprocessing and validation under varying local worker load. They are descriptive costs, not a controlled throughput benchmark.</li></ul>')
writeLines(report,file.path(output,'report.html'))
cat('\nHELD-OUT SUMMARY\n');print(summary)
cat('\nPAIRED LOG-LOSS DIFFERENCES\n');print(do.call(rbind,differences))
cat('\nCompleted comparison:',normalizePath(output,winslash='/'),'\n')
