# Independently check a downloaded Big Blue search against the local frozen plan.
# Arguments: results directory and plan directory. No fitting or selection changes.
args <- commandArgs(TRUE);stopifnot(length(args)==2L)
output <- args[1L];plan <- args[2L]
suppressPackageStartupMessages(library(data.table))
p <- readRDS(file.path(output,'head_width_protocol.rds'))
q <- readRDS(file.path(plan,'head_width_protocol.rds'))
stopifnot(!p$smoke,identical(p$runs,q$runs),identical(p$settings,q$settings),
  identical(p$source_hashes,q$source_hashes),identical(p$baseline,q$baseline),
  identical(p$reference_protocol_sha256,q$reference_protocol_sha256),
  identical(readRDS(file.path(output,'prepared_data.rds')),readRDS(file.path(plan,'prepared_data.rds'))),
  identical(read.csv(file.path(output,'reference/participant_split.csv')),
    read.csv(file.path(plan,'reference/participant_split.csv'))),
  length(list.files(file.path(output,'models'),pattern='[.]rds$'))==36L,
  length(list.files(file.path(output,'validation'),pattern='[.]csv$'))==36L,
  file.exists(file.path(output,'verification.txt')))
for(f in names(p$source_hashes))stopifnot(identical(
  digest::digest(file.path(output,'source',f),algo='sha256',file=TRUE),p$source_hashes[[f]]))
d <- as.data.table(readRDS(file.path(output,'prepared_data.rds')))
original <- readRDS(file.path(output,'reference/inference_protocol.rds'))
for(id in names(p$settings)) {
  task <- p$runs[p$runs$run_id==id,];s <- p$settings[[id]]
  base <- original$settings[[paste(task$family,'baseline','seed1',sep='__')]]
  s$seed <- base$seed;s$head_hidden_sizes <- base$head_hidden_sizes
  stopifnot(identical(s,base))
}
validation <- fread(file.path(output,'validation_by_seed.csv'))
selection <- fread(file.path(output,'selected_widths.csv'))
predictions <- fread(file.path(output,'test_predictions.csv'))
scores <- fread(file.path(output,'test_by_seed.csv'))
stopifnot(nrow(validation)==36L,!anyDuplicated(validation$run_id),
  setequal(validation$run_id,p$runs$run_id),all(d[,.(n=.N),by=fold]$n==c(38530L,8191L,8401L)))
independent_loss <- function(y,probability)-mean(ifelse(y==1,log(probability),log1p(-probability)))
protocol_sha <- digest::digest(file.path(output,'head_width_protocol.rds'),algo='sha256',file=TRUE)

# Each checkpoint is read once to verify its provenance and validation score.
# For the selected/baseline models also check the transferred test predictions.
for(id in p$runs$run_id) {
  saved <- readRDS(file.path(output,'models',paste0(id,'.rds')));fit <- saved$fit
  stopifnot(identical(saved$protocol_sha256,protocol_sha),identical(saved$run_id,id),
    identical(fit$gru_settings,p$settings[[id]]),identical(fit$fit_rows,which(d$fold==1L)),
    identical(fit$validation_rows,which(d$fold==2L)),identical(fit$evaluation_rows,which(d$fold==3L)),
    abs(validation[run_id==id]$log_loss-independent_loss(d$CF..ansbin.[d$fold==2L],fit$prediction[d$fold==2L]))<1e-12)
  if(id %in% scores$run_id) {
    test <- predictions[run_id==id]
    stopifnot(nrow(test)==8401L,identical(test$source_row,d$comparison_source_row[d$fold==3L]),
      all(test$observed==d$CF..ansbin.[d$fold==3L]),
      max(abs(test$prediction-fit$prediction[d$fold==3L]))<3e-6,
      abs(scores[run_id==id]$log_loss-independent_loss(test$observed,test$prediction))<1e-12)
  }
}
ranking <- validation[,.(validation_log_loss=mean(log_loss)),by=.(family,head_width)]
setorder(ranking,family,validation_log_loss,head_width)
chosen <- ranking[,head(.SD,1L),by=family]
setorder(selection,family)
stopifnot(identical(chosen$family,selection$family),identical(chosen$head_width,selection$head_width),
  max(abs(chosen$validation_log_loss-selection$validation_log_loss))<1e-12)
for(family_name in chosen$family) {
  widths <- scores[family==family_name]$head_width
  stopifnot(setequal(widths,c(32L,chosen[family==family_name]$head_width)))
}
message <- c('PASS: all 36 settings, seeds, frozen source hashes, engineered-feature parameters and reference protocol match the local frozen plan.',
  'PASS: exact prepared data and participant assignments match; only head width and initialization seed differ from the reference baseline.',
  'PASS: all 36 complete checkpoints transferred; every validation log loss independently recomputed locally.',
  'PASS: independently reproduced width selection by mean validation loss; selected and baseline test scores/predictions verified.',
  paste('Linux runtime:',p$software$R,'; LKT',p$software$LKT,'; Torch',p$software$torch),
  'The server verification additionally replays selected/baseline test predictions from each checkpoint.')
writeLines(message,file.path(output,'local_transfer_verification.txt'))
cat(paste(message,collapse='\n'),'\n')
