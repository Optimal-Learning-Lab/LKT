# Prepare a new model specification; historical data, fits and submissions are read-only.
# Usage: Rscript scripts/prepare-gru-three-time-inputs.R ROOT OUTPUT [STUDY_RESULTS]
args <- commandArgs(TRUE)
stopifnot(length(args) %in% 2:3)
root <- normalizePath(args[1],winslash='/',mustWork=TRUE)
output <- args[2]
study <- if(length(args)==3L)args[3] else file.path(root,
  '../LKT-outputs/encoder-inference-20261103/development-review/results')
source(file.path(root,'scripts/load-current-lkt.R'))
if(dir.exists(output))stop('Choose a new output directory; existing experiment records are immutable.')
dir.create(output,recursive=TRUE)
timing <- c('gru_item_age_seconds','gru_mean_prior_spacing_seconds')

# Reconstruct the complete native export before selecting the historical response rows.
# This preserves the participant split and item identities while allowing study events
# to contribute only to the two new timing inputs, not invented correctness labels.
original <- as.data.frame(LKT::largerawsample)
original$CF..Time. <- as.numeric(as.POSIXct(original$Time,format='%Y-%m-%d %H:%M:%S',tz='UTC'))
exposures <- LKT::LKTGRUTimeFeatures(original)
d <- readRDS(file.path(study,'data/prepared_data.rds'))
indices <- d$comparison_source_row
stopifnot(all(d$KC..Default.==original$KC..Default.[indices]),
  all(d$Anon.Student.Id==original$Anon.Student.Id[indices]),
  all(d$CF..Time.==original$CF..Time.[indices]),!any(original$Outcome[indices]=='STUDY'))
d[timing] <- exposures[indices,timing]

# Reuse the validation-selected encoder-only architecture, changing only its time
# input width. The new specification starts a new fit; old fitted weights cannot
# accept the extra two coordinates and must never be presented as the new model.
choices <- read.csv(file.path(study,'selection/development-primary.csv'))
choice <- choices[choices$family=='augmented' & choices$stage=='encoders',]
stopifnot(nrow(choice)==1)
source_checkpoint <- file.path(study,'models',paste0('development__augmented__encoders__',choice$candidate,'__seed1.rds'))
baseline <- readRDS(source_checkpoint)$fit
s <- baseline$gru_settings;s$time_feature_cols <- timing;s$time_missing_cols <- timing[2L]
model <- do.call(LKT::GRUModel,s)
recipe <- baseline$model$recipe
counts <- getFromNamespace('lkt_feature_param_counts','LKT')(recipe$features)
fixed <- split(recipe$fixedpars,rep(seq_along(counts),counts))
terms <- data.frame(component=recipe$components,feature=recipe$features,
  fixed=vapply(seq_along(counts),function(i)paste(sprintf('%.17g',fixed[[as.character(i)]]),collapse=';'),character(1)),
  interaction=ifelse(is.na(recipe$interacts),'',recipe$interacts),connector=c('+',recipe$connectors))
config <- LKT::lkt_model_config(terms,intercept=recipe$interc,backend='GRU',folds=1,
  options=list(verbose=FALSE,maketimes=recipe$maketimes),gru_options=model$settings)
saveRDS(d,file.path(output,'prepared_data.rds'))
saveRDS(config,file.path(output,'model-specification.rds'))
saveRDS(model$settings,file.path(output,'gru-settings.rds'))
saveRDS(list(release=lkt_release,baseline_checkpoint=source_checkpoint,
  baseline_sha256=digest::digest(file=source_checkpoint,algo='sha256'),
  original_exposures=nrow(original),study_exposures=sum(original$Outcome=='STUDY'),
  scored_responses=nrow(d),time_inputs=c('recency',timing),
  studies='Counted for item age and prior spacing only; excluded from scored targets and recurrent answer updates.',
  missing_spacing='NA before two prior item exposures, with explicit availability indicator at network input.',
  model_status='New unfitted specification; no performance claim.'),file.path(output,'provenance.rds'))
write.csv(data.frame(source_row=indices,participant=d$Anon.Student.Id,item=d$KC..Default.,
  time=d$CF..Time.,d[timing]),file.path(output,'timing-audit.csv'),row.names=FALSE,na='')
cat('Prepared',nrow(d),'responses from',nrow(original),'exposures, including',sum(original$Outcome=='STUDY'),'studies.\n')
cat('Time encoder: 3 timing values + spacing availability -> 32 ReLU -> 16. Three GRU layers and the 128-unit head are unchanged.\n')
