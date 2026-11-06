# Derived phase/condition summaries use already selected held-out predictions.
# No additional fitting or architecture selection occurs in this script.
args <- commandArgs(TRUE)
stopifnot(length(args)>=2L)
output <- args[1L];data_file <- args[2L]
suppressPackageStartupMessages(library(data.table))
protocol <- readRDS(file.path(output,'protocol.rds'))
stopifnot(identical(digest::digest(data_file,algo='sha256',file=TRUE),protocol$input_data_sha256))
# Recover subgroup labels and outcomes from the original source snapshot. This avoids
# inferring experimental conditions from the predictions themselves.
source_data <- as.data.table(readRDS(data_file))
has_phases <- 'Level..Phase.' %in% names(source_data)
has_conditions <- 'condition_2_name' %in% names(source_data)
if(identical(protocol$dataset,'largerawsample')) {
  stopifnot(all(source_data$Outcome %in% c('CORRECT','INCORRECT','STUDY')))
  source_data$CF..ansbin. <- ifelse(source_data$Outcome=='STUDY',NA_integer_,as.integer(source_data$Outcome=='CORRECT'))
  source_data$condition_2_name <- source_data$Condition.Name.2
  has_conditions <- TRUE
}
if(!'CF..ansbin.' %in% names(source_data)) {
  stopifnot(all(as.character(source_data$Outcome) %in% c('CORRECT','INCORRECT')))
  source_data$CF..ansbin. <- as.integer(as.character(source_data$Outcome)=='CORRECT')
}
if(!has_phases)source_data$Level..Phase. <- 'Unlabeled'
if(!has_conditions)source_data$condition_2_name <- 'Unlabeled'
# Current Cloze runs have an explicit inclusion ledger because threshold filtering can
# remove scored responses. Historical protocols retain their original all-scored rule.
included <- source_data$CF..ansbin. %in% c(0,1)
if(identical(protocol$schema,3L) && identical(protocol$dataset,'largerawsample')) {
  ledger <- fread(file.path(output,'response_inclusion.csv'))
  stopifnot(identical(ledger$source_row,seq_len(nrow(source_data))),
    !anyNA(ledger$retained),all(!ledger$retained | included))
  included <- ledger$retained
}
# Check source-row alignment before calculating any subgroup scores. A shuffled export
# can otherwise produce plausible but incorrect phase or condition results.
pred <- fread(file.path(output,'test_predictions.csv'),colClasses=c(participant='character'))
stopifnot(all(pred$observed==source_data$CF..ansbin.[pred$source_row]),
  all(pred$phase==source_data$Level..Phase.[pred$source_row]),
  all(pred$participant==as.character(source_data$Anon.Student.Id[pred$source_row])))
split <- fread(file.path(output,'participant_split.csv'),colClasses=c(participant='character'))
stopifnot(all(split$fold[match(pred$participant,split$participant)]==3L))
# Require each reported model to cover every scored test response once. This rules out
# an apparent performance gain caused by omitting difficult responses.
expected_test_rows <- which(as.character(source_data$Anon.Student.Id) %in%
  split$participant[split$fold==3L] & included)
coverage <- pred[,.(complete=identical(sort(as.integer(source_row)),sort(as.integer(expected_test_rows)))),
  by=.(family,architecture,seed)]
stopifnot(all(coverage$complete))
counts <- fread(file.path(output,'split_counts.csv'))
stopifnot(sum(counts$trials)==sum(included),
  sum(counts$participants)==uniqueN(source_data$Anon.Student.Id[included]))
candidate <- fread(file.path(output,'validation_candidates.csv'))
# Reconstruct the architecture choice from validation scores alone. Subgroup analyses
# must use those choices rather than selecting a different model within each subgroup.
expected <- candidate[,.(mean_validation_log_loss=mean(log_loss)),by=.(family,architecture)]
setorderv(expected,c('family','mean_validation_log_loss','architecture'))
expected <- expected[,.SD[1L],by=family]
selected <- fread(file.path(output,'selected_architectures.csv'))
declared <- expand.grid(family=names(protocol$families),architecture=names(protocol$architectures),
  seed=protocol$seeds,stringsAsFactors=FALSE)
candidate_keys <- paste(candidate$family,candidate$architecture,candidate$seed,sep='/')
stopifnot(nrow(candidate)==nrow(declared),!anyDuplicated(candidate_keys),
  setequal(candidate_keys,paste(declared$family,declared$architecture,declared$seed,sep='/')),
  nrow(selected)==length(protocol$families),
  identical(selected$architecture[match(expected$family,selected$family)],expected$architecture))
# Derive protocol labels from the source row, independently of any display field.
pred$condition <- source_data$condition_2_name[pred$source_row]
pred$phase_group <- ifelse(grepl('^pretest',pred$phase),'Pretest',
  ifelse(grepl('^posttest',pred$phase),'Posttest',ifelse(pred$phase=='Unlabeled','All trials','Learning')))
score <- function(y,p)list(log_loss=-mean(dbinom(y,1,p,log=TRUE)),
  AUC=if(uniqueN(y)==2L)as.numeric(pROC::auc(y,p,levels=c(0,1),direction='<',quiet=TRUE)) else NA_real_,
  Brier=mean((y-p)^2),RMSE=sqrt(mean((y-p)^2)))
# Calculate subgroup metrics separately for each training seed before averaging them.
# The subgroup tables remain descriptive and do not introduce another tuning step.
summarize <- function(group) {
  per_seed <- pred[,c(list(participants=uniqueN(participant),trials=.N),score(observed,prediction)),
    by=c('family','architecture','seed',group)]
  per_seed[,.(participants=participants[1L],trials=trials[1L],runs=.N,log_loss=mean(log_loss),
    log_loss_sd=if(.N>1L)sd(log_loss) else 0,AUC=mean(AUC),RMSE=mean(RMSE)),
    by=c('family','architecture',group)]
}
phases <- summarize('phase_group');conditions <- summarize('condition')
fwrite(phases,file.path(output,'test_scores_by_phase.csv'))
fwrite(conditions,file.path(output,'test_scores_by_condition.csv'))
fwrite(pred,file.path(output,'test_predictions_with_condition_names.csv'))

# Independent verification of every reported pooled metric from saved rows.
# Independently recompute the pooled scores from saved probabilities. This checks that
# the subgroup export remains consistent with the main report.
saved <- fread(file.path(output,'test_scores_by_seed.csv'))
recomputed <- pred[,score(observed,prediction),by=.(family,architecture,seed)]
for(i in seq_len(nrow(saved))) {
  index <- which(recomputed$family==saved$family[i] & recomputed$architecture==saved$architecture[i] &
    (if(is.na(saved$seed[i]))is.na(recomputed$seed) else recomputed$seed==saved$seed[i]))
  stopifnot(length(index)==1L)
  for(metric in c('log_loss','AUC','Brier','RMSE'))
    stopifnot(abs(saved[[metric]][i]-recomputed[[metric]][index])<1e-12)
}
table_html <- function(d) {
  d <- as.data.frame(d);d[] <- lapply(d,function(x)if(is.numeric(x))format(round(x,5),trim=TRUE) else as.character(x))
  paste0('<table><tr>',paste0('<th>',names(d),'</th>',collapse=''),'</tr>',
    paste(vapply(seq_len(nrow(d)),function(i)paste0('<tr>',paste0('<td>',unlist(d[i,],use.names=FALSE),'</td>',collapse=''),'</tr>'),character(1)),collapse=''),'</table>')
}
additional <- paste0('<h2>Held-out performance by phase</h2><p>These are descriptive results using the already validation-selected architectures. GRU metrics average the same three single-model seeds; no subgroup tuning occurs.</p>',
  if(!has_phases)'<p>The source dataset has no phase labels. All trials are shown together; no pretest/posttest distinction is inferred.</p>' else '',
  table_html(phases),'<h2>Held-out performance by practice condition</h2>',table_html(conditions),
  if(!has_conditions)'<p>The source dataset has no practice-condition labels.</p>' else '',
  '<p>Held-out participants share the training KC universe. Raw and augmented GRUs learn KC-specific embeddings; these scores do not establish performance on unseen synthetic item/KC identities.</p>',
  '<p>Fit wall times include preprocessing and validation under varying local worker load, and are descriptive rather than a controlled throughput benchmark.</p>',
  '<p>All pooled metrics were independently recomputed from the saved source-row-aligned predictions and matched within 1e-12. Source-row participant labels, held-out membership, and validation-only architecture selection were also independently verified.</p>')
report_file <- file.path(output,'report.html')
report <- paste(readLines(report_file,warn=FALSE),collapse='\n')
if(!grepl('Held-out performance by phase',report,fixed=TRUE))writeLines(paste0(report,additional),report_file)
writeLines(c('PASS: source-row labels and all pooled metrics verified independently to 1e-12.',
  'PASS: every scored source response is accounted for in the partitions; every selected model covers the complete held-out response set exactly once.'),file.path(output,'verification.txt'))
cat('PHASE RESULTS\n');print(phases)
cat('PASS independent metric verification and phase/condition summaries.\n')
