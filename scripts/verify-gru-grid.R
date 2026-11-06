# Independent checks of every candidate's validation predictions, selection,
# source-response coverage, and final test metrics. Standalone arg: output.
verify_gru_grid <- function(output) {
  suppressPackageStartupMessages(library(data.table))
  protocol_file <- file.path(output,'grid_protocol.rds')
  protocol <- readRDS(protocol_file)
  if(!identical(protocol$schema,2L))stop('Use the archived verifier for archived grid protocols.')
  protocol_hash <- digest::digest(protocol_file,algo='sha256',file=TRUE)
  hash <- function(name)digest::digest(file.path(output,name),algo='sha256',file=TRUE)
  stopifnot(identical(hash('input_data.rds'),protocol$input_data_sha256),
    identical(hash('prepared_data.rds'),protocol$prepared_data_sha256),
    identical(hash(protocol$baseline_filename),protocol$baseline_checkpoint_sha256))
  # Start from the saved original export, not just the prepared training table. This
  # gives an independent basis for checking labels and which responses were retained.
  original <- as.data.frame(readRDS(file.path(output,'input_data.rds')))
  d <- as.data.table(readRDS(file.path(output,'prepared_data.rds')))
  if('CF..ansbin.' %in% names(original))y_source <- original$CF..ansbin. else {
    stopifnot(all(as.character(original$Outcome) %in% c('CORRECT','INCORRECT','STUDY')))
    y_source <- ifelse(as.character(original$Outcome)=='STUDY',NA_integer_,
      as.integer(as.character(original$Outcome)=='CORRECT'))
  }
  expected_rows <- which(y_source %in% c(0,1))
  if(protocol$dataset=='largerawsample') {
    # Independently reconstruct the fixed point from unique participant/item pairs.
    # Do not call the preparation helper: an error in that helper must be detectable.
    stopifnot(protocol$preprocessing$identity=='cloze-item144-skill36')
    repeat {
      previous <- expected_rows
      pairs <- unique(original[expected_rows,c('Anon.Student.Id','KC..Default.')])
      students <- names(which(table(pairs$Anon.Student.Id)>=protocol$preprocessing$min_items_per_student))
      expected_rows <- expected_rows[original$Anon.Student.Id[expected_rows] %in% students]
      pairs <- unique(original[expected_rows,c('Anon.Student.Id','KC..Default.')])
      items <- names(which(table(pairs$KC..Default.)>=protocol$preprocessing$min_students_per_item))
      expected_rows <- expected_rows[original$KC..Default.[expected_rows] %in% items]
      if(identical(previous,expected_rows))break
    }
  }
  stopifnot(!anyDuplicated(d$comparison_source_row),
    identical(sort(d$comparison_source_row),expected_rows),
    all(d$CF..ansbin.==y_source[d$comparison_source_row]),
    all(as.character(d$Anon.Student.Id)==as.character(original$Anon.Student.Id[d$comparison_source_row])))
  if(protocol$dataset=='largerawsample')stopifnot(
    all(d$KC..Default.==original$KC..Default.[d$comparison_source_row]),
    all(d$KC..Cluster.==original$KC..Cluster.[d$comparison_source_row]),
    all(d$CF..Time.==as.numeric(as.POSIXct(original$Time[d$comparison_source_row],
      format='%Y-%m-%d %H:%M:%S',tz='UTC'))))
  # Check participant assignments against the saved split and confirm chronological
  # histories. The same person must not move between fitting and held-out groups.
  split <- fread(file.path(output,'participant_split.csv'),colClasses=c(participant='character'))
  stopifnot(!anyDuplicated(split$participant),
    all(d$fold==split$fold[match(as.character(d$Anon.Student.Id),split$participant)]))
  for(idx in split(seq_len(nrow(d)),as.character(d$Anon.Student.Id)))
    stopifnot(identical(idx,idx[order(d$CF..Time.[idx],d$comparison_source_row[idx])]))
  stopifnot(!length(setdiff(unique(d$KC..Default.),unique(d$KC..Default.[d$fold==1L]))))
  # Recompute scores without calling the search runner's scoring helper. In particular,
  # the AUC calculation uses ranks as a separate check of the ROC implementation.
  independent_metrics <- function(y,p) {
    stopifnot(length(y)==length(p),length(y)>0L,all(y %in% c(0,1)),all(is.finite(p)),all(p>0 & p<1))
    n1 <- sum(y==1);n0 <- sum(y==0)
    c(log_loss=-mean(ifelse(y==1,log(p),log1p(-p))),
      AUC=(sum(rank(p,ties.method='average')[y==1])-n1*(n1+1)/2)/(n1*n0),
      Brier=mean((y-p)^2),RMSE=sqrt(mean((y-p)^2)))
  }
  # Require exactly the declared candidate-seed combinations. Missing or duplicated fits
  # must not be hidden by an aggregate mean.
  val <- rbindlist(lapply(list.files(file.path(output,'validation'),pattern='[.]csv$',full.names=TRUE),fread))
  stopifnot(nrow(val)==nrow(protocol$runs),!anyDuplicated(val$run_id),
    setequal(val$run_id,protocol$runs$run_id))
  # Inspect every checkpoint and recompute its validation scores. The fitted settings,
  # row masks, and recorded stopping epoch must agree with the manifest.
  for(run_id in protocol$runs$run_id) {
    task <- protocol$runs[match(run_id,protocol$runs$run_id),]
    checkpoint <- readRDS(file.path(output,'models',paste0(run_id,'.rds')))
    fit <- checkpoint$fit;settings <- protocol$run_settings[[run_id]]
    stopifnot(identical(checkpoint$protocol_sha256,protocol_hash),identical(checkpoint$run_id,run_id),
      identical(fit$gru_settings,settings),
      identical(digest::digest(settings,algo='sha256'),task$settings_sha256),
      identical(fit$fit_rows,which(d$fold==1L)),
      identical(fit$validation_rows,which(d$fold==2L)),
      identical(fit$evaluation_rows,which(d$fold==3L)),
      settings$num_layers==task$num_layers,settings$hidden_size==task$hidden_size,
      settings$learning_rate==task$learning_rate,settings$weight_decay==task$weight_decay,
      settings$input_dropout==task$dropout,settings$head_dropout==task$dropout,
      settings$gru_dropout==if(task$num_layers>1L)task$dropout else 0,
      settings$epochs==protocol$common$epochs,settings$seed==task$seed)
    position <- match(run_id,val$run_id)
    v <- val[position]
    stopifnot(nrow(v)==1L,v$candidate_id==task$candidate_id,v$family==task$family,v$seed==task$seed,
      v$selected_epoch==fit$selected_epoch,v$epochs_run==nrow(fit$training_history),
      v$hit_epoch_cap==(v$epochs_run==settings$epochs))
    m <- independent_metrics(d$CF..ansbin.[d$fold==2L],fit$prediction[d$fold==2L])
    for(metric in names(m))stopifnot(abs(m[[metric]]-v[[metric]])<1e-12)
  }
  # Reconstruct the declared selection rule from independently checked scores.
  # Rebuild the configuration choices from the checked validation records. This verifies
  # the selection rule independently of the test results.
  choices <- val[,.(mean_validation_log_loss=mean(log_loss),training_seeds=.N),by=.(family,candidate_id)]
  stopifnot(nrow(choices)==nrow(protocol$candidates),all(choices$training_seeds==length(protocol$seeds)))
  setorderv(choices,c('family','mean_validation_log_loss','candidate_id'))
  choices <- choices[,.SD[1L],by=family]
  selection <- readRDS(file.path(output,'selection.rds'))
  selected <- selection$selected
  stopifnot(identical(selection$protocol_sha256,protocol_hash),
    identical(selected$candidate_id[match(choices$family,selected$family)],choices$candidate_id))
  for(id in protocol$runs$run_id)stopifnot(identical(
    selection$validation_file_hashes[[id]],hash(file.path('validation',paste0(id,'.csv')))))
  # Align final test predictions to original response rows and participants. Every
  # selected model must cover the complete held-out response set exactly once.
  pred <- fread(file.path(output,'test_predictions.csv'),colClasses=c(participant='character'))
  expected_rows <- d$comparison_source_row[d$fold==3L]
  stopifnot(nrow(pred)==length(expected_rows)*(1L+nrow(selected)*length(protocol$seeds)),
    all(pred$observed==y_source[pred$source_row]),
    all(pred$participant==as.character(original$Anon.Student.Id[pred$source_row])),
    all(split$fold[match(pred$participant,split$participant)]==3L))
  groups <- pred[,.(complete=identical(sort(as.integer(source_row)),sort(as.integer(expected_rows)))),
    by=.(family,candidate_id,seed)]
  stopifnot(all(groups$complete),nrow(groups)==1L+nrow(selected)*length(protocol$seeds))
  saved <- fread(file.path(output,'test_scores_by_seed.csv'))
  stopifnot(nrow(saved)==nrow(groups))
  baseline <- readRDS(file.path(output,protocol$baseline_filename))
  # Recompute each test score and compare the exported probabilities with its
  # checkpoint. This catches a correct-looking summary paired with the wrong model or
  # response ordering.
  for(i in seq_len(nrow(groups))) {
    g <- groups[i]
    p <- pred[family==g$family & candidate_id==g$candidate_id &
      (if(is.na(g$seed))is.na(seed) else seed==g$seed)]
    m <- independent_metrics(p$observed,p$prediction)
    score <- saved[family==g$family & candidate_id==g$candidate_id &
      (if(is.na(g$seed))is.na(seed) else seed==g$seed)]
    stopifnot(nrow(score)==1L)
    for(metric in names(m))stopifnot(abs(m[[metric]]-score[[metric]])<1e-12)
    if(g$family=='ppes')fit <- baseline else {
      stopifnot(g$candidate_id==selected$candidate_id[selected$family==g$family],g$seed %in% protocol$seeds)
      fit <- readRDS(file.path(output,'models',paste0(g$candidate_id,'_seed',g$seed,'.rds')))$fit
    }
    stopifnot(max(abs(p$prediction-fit$prediction[match(p$source_row,d$comparison_source_row)]))<1e-12)
  }
  # Rebuild the means and seed standard deviations in the final table. Agreement here
  # checks the final reporting step as well as the individual prediction files.
  recomputed <- saved[,.(training_seeds=.N,log_loss=mean(log_loss),
    log_loss_sd=if(.N>1L)sd(log_loss) else 0,AUC=mean(AUC),Brier=mean(Brier),RMSE=mean(RMSE)),
    by=.(family,candidate_id)]
  summary <- fread(file.path(output,'test_summary.csv'))
  stopifnot(nrow(summary)==nrow(recomputed))
  for(i in seq_len(nrow(summary))) {
    row <- recomputed[family==summary$family[i] & candidate_id==summary$candidate_id[i]]
    stopifnot(nrow(row)==1L)
    for(metric in c('log_loss','log_loss_sd','AUC','Brier','RMSE'))
      stopifnot(abs(row[[metric]]-summary[[metric]][i])<1e-12)
  }
  writeLines(c('PASS: input/prepared data hashes, original response labels, all scored-response coverage, participant separation, and chronology.',
    'PASS: every candidate checkpoint, resolved settings, fitting/validation/test masks, and validation metrics independently checked.',
    'PASS: validation-only selection reconstructed; immutable selection hashes verified.',
    'PASS: every selected model covers the complete test response set exactly once; predictions match saved checkpoints.',
    'PASS: all pooled validation/test metrics independently reproduced within 1e-12; AUC checked using ranks.'),
    file.path(output,'verification.txt'))
  cat('PASS independent verification of the complete grid and selected-model test results.\n')
  invisible(TRUE)
}

if(sys.nframe()==0L) {
  args <- commandArgs(TRUE);stopifnot(length(args)==1L)
  verify_gru_grid(args[1L])
}
