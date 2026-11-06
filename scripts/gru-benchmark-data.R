# Shared bundled-data preparation for GRU benchmark and grid-search scripts.
# Callers attach data.table and LKT before invoking this function.

# Keep original item labels separate from the broader sentence/feedback clusters.
# Repeatedly remove sparse participants and items because removing either can make
# the other fall below its threshold on the next pass.
filter_gru_cloze_data <- function(data,min_items_per_student=10L,min_students_per_item=100L) {
  required <- c('Anon.Student.Id','KC..Default.','KC..Cluster.','comparison_source_row')
  stopifnot(all(required %in% names(data)),length(min_items_per_student)==1L,
    length(min_students_per_item)==1L,min_items_per_student>=1L,min_students_per_item>=1L,
    !anyDuplicated(data$comparison_source_row))
  for(column in required)if(anyNA(data[[column]]) || any(!nzchar(as.character(data[[column]]))))
    stop('Missing Cloze identity: ',column)
  data <- data.table::as.data.table(data.table::copy(data))
  mapping <- unique(data[,c('KC..Default.','KC..Cluster.'),with=FALSE])
  if(anyDuplicated(mapping$KC..Default.))stop('Each Cloze item must map to exactly one skill.')
  count_stage <- function(x,iteration,stage) data.frame(iteration=iteration,stage=stage,
    responses=nrow(x),participants=data.table::uniqueN(x$Anon.Student.Id),
    items=data.table::uniqueN(x$KC..Default.),skills=data.table::uniqueN(x$KC..Cluster.))
  audit <- list(count_stage(data,0L,'scored responses before threshold filtering'))
  removed <- list();iteration <- 0L
  repeat {
    iteration <- iteration+1L;before <- nrow(data)
    participant_counts <- data[,.(distinct_items=data.table::uniqueN(KC..Default.)),by=Anon.Student.Id]
    drop <- data$Anon.Student.Id %in% participant_counts$Anon.Student.Id[
      participant_counts$distinct_items<min_items_per_student]
    removed[[length(removed)+1L]] <- data.frame(source_row=data$comparison_source_row[drop],
      iteration=rep(iteration,sum(drop)),reason=rep('participant below distinct-item minimum',sum(drop)))
    data <- data[!drop]
    audit[[length(audit)+1L]] <- count_stage(data,iteration,'after participant threshold')
    item_counts <- data[,.(distinct_participants=data.table::uniqueN(Anon.Student.Id)),by=KC..Default.]
    drop <- data$KC..Default. %in% item_counts$KC..Default.[
      item_counts$distinct_participants<min_students_per_item]
    removed[[length(removed)+1L]] <- data.frame(source_row=data$comparison_source_row[drop],
      iteration=rep(iteration,sum(drop)),reason=rep('item below distinct-participant minimum',sum(drop)))
    data <- data[!drop]
    audit[[length(audit)+1L]] <- count_stage(data,iteration,'after item threshold')
    if(!nrow(data))stop('Cloze threshold filtering removed every scored response.')
    if(nrow(data)==before)break
  }
  list(data=data,stages=do.call(rbind,audit),removed=do.call(rbind,removed),
    participant_counts=data[,.(distinct_items=data.table::uniqueN(KC..Default.),responses=.N),by=Anon.Student.Id],
    item_counts=data[,.(distinct_participants=data.table::uniqueN(Anon.Student.Id),responses=.N),
      by=c('KC..Default.','KC..Cluster.')])
}

# Both raw neural input families receive item identity and skill identity. The matched
# family deliberately receives only the PPES design columns so its original comparison
# question remains explicit.
gru_benchmark_families <- function(dataset) {
  categorical <- if(dataset=='largerawsample')c('KC..Default.','KC..Cluster.') else 'KC..Default.'
  list(matched=list(categorical_cols=character(),time_col=NULL,use_lkt_features=TRUE),
    raw=list(categorical_cols=categorical,time_col='CF..Time.',use_lkt_features=FALSE),
    augmented=list(categorical_cols=categorical,time_col='CF..Time.',use_lkt_features=TRUE))
}

prepare_gru_benchmark_data <- function(dataset,root,output,workflow,source_snapshot=NULL) {
  # Prepare data identically for the depth comparison and the grid search. Sharing this
  # step prevents two drivers from silently using different row definitions or
  # participant splits.
  stopifnot(dataset %in% c('largerawsample','samplelkt','calibration'))
  preprocessing <- list(identity='dataset-native',filtering='binary scored responses')
# For bundled data, confirm that the installed export matches the repository asset.
# Preserve the original rows so later checks can trace every prediction back to its
# source response.
if(dataset %in% c('samplelkt','largerawsample')) {
  original <- as.data.frame(getExportedValue('LKT',dataset))
  # Check the installed export against the repository's actual bundled asset.
  asset <- new.env(parent=emptyenv());load(file.path(root,paste0('data/',dataset,'.rda')),envir=asset)
  stopifnot(identical(original,as.data.frame(asset[[dataset]])))
  data_file <- file.path(output,paste0(dataset,'_source.rds'))
  if(!is.null(source_snapshot))stopifnot(identical(readRDS(source_snapshot),original))
  if(file.exists(data_file))stopifnot(identical(readRDS(data_file),original)) else {
    if(is.null(source_snapshot))saveRDS(original,data_file) else
      stopifnot(file.copy(source_snapshot,data_file,overwrite=FALSE))
  }
  data <- as.data.table(copy(original))
  data$comparison_source_row <- seq_len(nrow(data))
}
# Preserve the exported 144 item labels and 36 skill labels, including their feedback
# distinctions. Study events are excluded consistently from both targets and histories.
if(dataset=='largerawsample') {
  stopifnot(all(data$Outcome %in% c('CORRECT','INCORRECT','STUDY')))
  stopifnot(uniqueN(data$KC..Default.)==144L,uniqueN(data$KC..Cluster.)==36L)
  # Use a fixed timezone reference when converting the exported timestamp strings into
  # seconds. This avoids introducing a host-dependent clock conversion into a comparison
  # run on Windows and Linux.
  data$CF..Time. <- as.numeric(as.POSIXct(data$Time,format='%Y-%m-%d %H:%M:%S',tz='UTC'))
  stopifnot(!anyNA(data$CF..Time.),all(is.finite(data$CF..Time.)))
  data <- data[Outcome %in% c('CORRECT','INCORRECT')]
  data$CF..ansbin. <- as.integer(data$Outcome=='CORRECT')
  # Apply the paper's published thresholds to the original item identities, before
  # partitioning. Save an audit even when the thresholds remove no responses.
  filtered <- filter_gru_cloze_data(data)
  data <- filtered$data
  preprocessing <- list(identity='cloze-item144-skill36',item_col='KC..Default.',
    skill_col='KC..Cluster.',min_items_per_student=10L,min_students_per_item=100L,
    filtering='binary scored responses, then iterative participant/item thresholds before splitting')
  write.csv(filtered$stages,file.path(output,'filtering_stages.csv'),row.names=FALSE)
  write.csv(filtered$removed,file.path(output,'threshold_removed_rows.csv'),row.names=FALSE)
  write.csv(filtered$participant_counts,file.path(output,'participant_item_counts.csv'),row.names=FALSE)
  write.csv(filtered$item_counts,file.path(output,'item_participant_counts.csv'),row.names=FALSE)
  write.csv(unique(data[,.(item=KC..Default.,skill=KC..Cluster.,problem_text=Problem.Name)]),
    file.path(output,'item_skill_mapping.csv'),row.names=FALSE)
  ledger <- data.frame(source_row=seq_len(nrow(original)),retained=seq_len(nrow(original)) %in% data$comparison_source_row,
    reason=ifelse(original$Outcome=='STUDY','unscored STUDY event','retained'))
  if(nrow(filtered$removed))ledger$reason[filtered$removed$source_row] <- filtered$removed$reason
  write.csv(ledger,file.path(output,'response_inclusion.csv'),row.names=FALSE)
  data$Level..Phase. <- 'Unlabeled'
  data$condition_2_name <- data$Condition.Name.2
  data_description <- sprintf('LKT::largerawsample: %s retained scored responses, %s participants, %s original items and %s skills (%s original records)',
    nrow(data),uniqueN(data$Anon.Student.Id),uniqueN(data$KC..Default.),uniqueN(data$KC..Cluster.),nrow(original))
  timing_policy <- 'Recorded Time values parsed into seconds using UTC as a fixed reference because the export does not specify a timezone; original timestamp differences are preserved without a host-dependent daylight-saving conversion.'
  caveats <- c('Nonlinear PPES settings are frozen carried-over values from the upcoming simulation, not independently optimized on largerawsample. Linear coefficients are fitted on training participants.',
    sprintf('Exclude %s unscored STUDY events, then apply iterative minima of 10 distinct items per participant and 100 distinct participants per item; retain %s responses. Study events do not update either model.',sum(original$Outcome=='STUDY'),nrow(data)),
    'The original item and cluster labels preserve feedback-condition distinctions. The paper reports matching counts but does not identify its exact source-column mapping.',
    'Published Cloze preprocessing reports 45,115 responses; this preparation is not an exact replication until the retained rows and split assignments are reconciled.',
    'The export does not identify a timezone. Fixed-reference timestamps cannot resolve an unrecorded daylight-saving clock change.',
    'Condition summaries retain the literal Condition.Name.2 export labels. No pretest/posttest or question-type meaning is inferred from numeric units or condition filenames.')
} else if(dataset=='samplelkt') {
  data$CF..ansbin. <- as.integer(as.character(data$Outcome)=='CORRECT')
  stopifnot(all(as.character(data$Outcome) %in% c('CORRECT','INCORRECT')))
  # The small export has response durations rather than calendar timestamps. Its
  # practice clock accumulates only completed earlier durations, so the current response
  # duration cannot enter its own prediction.
  data$CF..reltime. <- getFromNamespace('practiceTime','LKT')(data)
  data$CF..Time. <- data$CF..reltime.
  data$Level..Phase. <- 'Unlabeled';data$condition_2_name <- 'Unlabeled'
  data_description <- sprintf('LKT::samplelkt: %s responses, %s participants, %s KCs',
    nrow(data),uniqueN(data$Anon.Student.Id),uniqueN(data$KC..Default.))
  timing_policy <- 'Native LKT practiceTime: cumulative completed prior response durations in seconds, preserving original within-participant order. No calendar retention gaps are reconstructed.'
  caveats <- c('PPES nonlinear settings are frozen carried-over values from the upcoming simulation, not independently estimated on samplelkt.',
    'Only 17 participants, including four test participants, are available in this fixed split. Bootstrap intervals and model selection are exploratory.',
    'The sample records response durations but no calendar timestamps; the practice clock does not recover between-session retention gaps.',
    'The sample has no phase, practice-condition or question-type fields. No such subgroup labels are inferred.')
} else {
  data_file <- file.path(workflow,'data/v4-110-items-all-conditions-all-phases-model-ready.rds')
  data <- as.data.table(readRDS(data_file))
  stopifnot(nrow(data)==22899L,uniqueN(data$Anon.Student.Id)==154L,uniqueN(data$KC..Default.)==110L)
  data_description <- 'Frozen human calibration dataset: 22,899 responses, 154 participants, 110 KCs'
  timing_policy <- 'Recorded CF..Time. calendar timestamps in seconds.'
  caveats <- c('Nonlinear PPES settings are frozen carried-over values from the upcoming simulation, not independently estimated here.',
    'The 110 items were previously selected using response outcomes across this curated dataset. This is a conditional exploratory comparison, not a new external validation.',
    'All observations are multiple choice; the dataset cannot test problem-type by forgetting interactions.')
}
stopifnot(all(data$CF..ansbin. %in% c(0,1)),!anyNA(data$CF..Time.))
if(!'comparison_source_row' %in% names(data))data$comparison_source_row <- seq_len(nrow(data))
# Put each participant into chronological order and retain the original row number as a
# deterministic tie-breaker. The row number also lets the independent verifier align
# predictions with the untouched export.
setorderv(data,c('Anon.Student.Id','CF..Time.','comparison_source_row'))
# Assign whole participants to training, validation, and test partitions using a fixed
# seed. This evaluates prediction for new participants on the known problem universe.
ids <- sort(unique(as.character(data$Anon.Student.Id)),method='radix')
set.seed(1L);ids <- ids[sample.int(length(ids))]
nt <- floor(.7*length(ids));nv <- floor(.15*length(ids))
partition <- data.frame(participant=ids,fold=c(rep(1L,nt),rep(2L,nv),rep(3L,length(ids)-nt-nv)))
partition$partition <- c('training','validation','test')[partition$fold]
data$fold <- partition$fold[match(as.character(data$Anon.Student.Id),partition$participant)]
# Require every participant to have a partition and every evaluation KC to occur in
# training. This experiment does not silently replace unknown items with another
# encoding policy.
stopifnot(!anyNA(data$fold),!length(setdiff(unique(data$KC..Default.),unique(data$KC..Default.[data$fold==1L]))))
if(dataset=='largerawsample')stopifnot(!length(setdiff(unique(data$KC..Cluster.),
  unique(data$KC..Cluster.[data$fold==1L]))))
# Save participant and response counts for each partition. These counts make the
# evaluation sample visible without requiring a reader to infer it from the fitting
# calls.
counts <- data[,.(participants=uniqueN(Anon.Student.Id),trials=.N),by=fold][order(fold)]
counts$partition <- c('training','validation','test')[counts$fold]
  list(data=data,data_file=data_file,data_description=data_description,
    timing_policy=timing_policy,caveats=caveats,partition=partition,counts=counts,
    preprocessing=preprocessing,families=gru_benchmark_families(dataset))
}
