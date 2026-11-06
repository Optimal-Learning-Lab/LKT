# Verify the completed search, freeze validation-only choices, then evaluate the
# chosen widths and the width-32 baseline. Test scores never enter selection.
summarize_gru_head_widths <- function(output) {
  suppressPackageStartupMessages({library(LKT);library(data.table)})
  source(file.path(output,'source/scripts/gru-grid-utils.R'))
  protocol_file <- file.path(output,'head_width_protocol.rds');p <- readRDS(protocol_file)
  stopifnot(p$schema==1L,p$type=='GRU head-width search')
  protocol_sha <- digest::digest(protocol_file,algo='sha256',file=TRUE)
  stopifnot(identical(digest::digest(file.path(output,'prepared_data.rds'),algo='sha256',file=TRUE),p$data_sha256),
    identical(digest::digest(file.path(output,'reference/inference_protocol.rds'),algo='sha256',file=TRUE),p$reference_protocol_sha256),
    identical(digest::digest(file.path(output,'reference/largerawsample_source.rds'),algo='sha256',file=TRUE),p$input_sha256))
  for(f in names(p$source_hashes))stopifnot(identical(
    digest::digest(file.path(output,'source',f),algo='sha256',file=TRUE),p$source_hashes[[f]]))
  d <- as.data.table(readRDS(file.path(output,'prepared_data.rds')))
  original <- readRDS(file.path(output,'reference/largerawsample_source.rds'))
  partition <- fread(file.path(output,'reference/participant_split.csv'),colClasses=c(participant='character'))
  stopifnot(all(d$fold==partition$fold[match(as.character(d$Anon.Student.Id),partition$participant)]),
    all(d$KC..Default.==original$KC..Default.[d$comparison_source_row]),
    all(d$KC..Cluster.==original$KC..Cluster.[d$comparison_source_row]),
    all(d$CF..ansbin.==as.integer(original$Outcome[d$comparison_source_row]=='CORRECT')),
    !any(original$Outcome[d$comparison_source_row]=='STUDY'))
  independent_metrics <- function(y,q) {
    stopifnot(length(y)==length(q),all(y %in% c(0,1)),all(is.finite(q)),all(q>0 & q<1))
    n1 <- sum(y==1);n0 <- sum(y==0)
    c(log_loss=-mean(ifelse(y==1,log(q),log1p(-q))),
      AUC=(sum(rank(q,ties.method='average')[y==1])-n1*(n1+1)/2)/(n1*n0),
      Brier=mean((y-q)^2),RMSE=sqrt(mean((y-q)^2)))
  }
  files <- list.files(file.path(output,'validation'),pattern='[.]csv$',full.names=TRUE)
  validation <- rbindlist(lapply(files,fread))
  stopifnot(nrow(validation)==nrow(p$runs),!anyDuplicated(validation$run_id),setequal(validation$run_id,p$runs$run_id))

  # Recompute every validation score from its checkpoint before comparing widths.
  # This also checks that each fit used the declared training and validation rows.
  for(id in p$runs$run_id) {
    saved <- readRDS(file.path(output,'models',paste0(id,'.rds')))
    stopifnot(identical(saved$protocol_sha256,protocol_sha),identical(saved$run_id,id))
    fit <- saved$fit;gru_grid_validate_fit(fit,p$settings[[id]],d)
    metrics <- independent_metrics(d$CF..ansbin.[d$fold==2L],fit$prediction[d$fold==2L])
    row <- validation[run_id==id]
    for(name in names(metrics))stopifnot(abs(row[[name]]-metrics[[name]])<1e-12)
    stopifnot(row$parameter_count==fit$parameter_count,row$selected_epoch==fit$selected_epoch,
      row$epochs_run==nrow(fit$training_history))
  }
  summary <- validation[,.(seeds=.N,validation_log_loss=mean(log_loss),validation_seed_sd=sd(log_loss),
    validation_AUC=mean(AUC),parameter_count=unique(parameter_count),
    mean_selected_epoch=mean(selected_epoch),cap_hits=sum(hit_epoch_cap)),by=.(family,head_width)]
  stopifnot(nrow(summary)==12L,all(summary$seeds==length(p$seeds)))
  setorder(summary,family,validation_log_loss,head_width)
  selection <- summary[,head(.SD,1L),by=family]
  # These immutable files are committed before the first test metric is computed.
  # On re-verification their exact contents must agree with the original choices.
  gru_grid_atomic_csv(as.data.frame(summary),file.path(output,'validation_width_summary.csv'))
  gru_grid_atomic_csv(as.data.frame(selection),file.path(output,'selected_widths.csv'))
  selection_sha <- digest::digest(file.path(output,'selected_widths.csv'),algo='sha256',file=TRUE)

  # Replay selected and baseline test probabilities from saved weights with dropout
  # disabled. No test-response information is allowed into its own prediction.
  replay <- function(fit) {
    s <- fit$gru_settings;stopifnot(s$inference_mode=='none')
    cleanup <- getFromNamespace('lkt_gru_runtime','LKT')(s);on.exit(cleanup())
    net <- getFromNamespace('lkt_gru_network','LKT')(fit$model$preprocessing,s)
    getFromNamespace('lkt_gru_load_weights','LKT')(net,fit$model$weights,s);net$eval()
    test <- as.data.frame(d[d$fold==3L])
    encoded <- getFromNamespace('lkt_gru_encode','LKT')(test,fit$model$preprocessing,fit$model$recipe,s)
    groups <- split(seq_len(nrow(test)),getFromNamespace('lkt_gru_keys','LKT')(test,s$history_group_cols))
    probability <- numeric(nrow(test))
    torch::with_no_grad({
      for(start in seq.int(1L,length(groups),by=s$batch_size)) {
        g <- groups[seq.int(start,min(length(groups),start+s$batch_size-1L))]
        longest <- max(lengths(g));chunk <- if(is.null(s$sequence_length))longest else s$sequence_length
        hidden <- NULL
        for(offset in seq.int(1L,longest,by=chunk)) {
          active <- which(lengths(g)>=offset);g <- g[active]
          if(!is.null(hidden))hidden <- hidden[,active,,drop=FALSE]
          positions <- lapply(g,function(idx)seq.int(offset,min(length(idx),offset+chunk-1L)))
          b <- getFromNamespace('lkt_gru_batch','LKT')(encoded,test,g,positions,s)
          out <- net(b$numeric,b$categorical,b$answers,hidden)
          q <- torch::as_array(torch::torch_sigmoid(out$logits)$cpu())
          keep <- !is.na(b$rows);probability[b$rows[keep]] <- q[keep];hidden <- out$hidden
        }
      }
    })
    probability <- pmin(pmax(probability,s$clip_epsilon),1-s$clip_epsilon)
    stopifnot(max(abs(probability-fit$prediction[d$fold==3L]))<3e-6)
    probability
  }
  scores <- predictions <- list()
  for(family_name in selection$family) {
    chosen <- selection[family==family_name]$head_width
    evaluated <- p$runs[p$runs$family==family_name & p$runs$head_width %in% c(32L,chosen),]
    for(i in seq_len(nrow(evaluated))) {
      task <- evaluated[i,];id <- task$run_id
      fit <- readRDS(file.path(output,'models',paste0(id,'.rds')))$fit
      q <- replay(fit)
      scores[[id]] <- cbind(task,selected_width=task$head_width==chosen,
        as.data.frame(as.list(independent_metrics(d$CF..ansbin.[d$fold==3L],q))))
      predictions[[id]] <- data.table(run_id=id,family=family_name,head_width=task$head_width,seed=task$seed,
        source_row=d$comparison_source_row[d$fold==3L],participant=d$Anon.Student.Id[d$fold==3L],
        observed=d$CF..ansbin.[d$fold==3L],prediction=q)
    }
  }
  scores <- rbindlist(scores);predictions <- rbindlist(predictions)
  test_summary <- scores[,.(seeds=.N,log_loss=mean(log_loss),log_loss_seed_sd=sd(log_loss),AUC=mean(AUC),
    Brier=mean(Brier),RMSE=mean(RMSE)),by=.(family,head_width,selected_width)]

  # Pair the selected and baseline response losses, averaging across seeds first.
  # Whole-participant resampling respects dependence within each response history.
  losses <- predictions[,.(loss=mean(-ifelse(observed==1,log(prediction),log1p(-prediction)))),
    by=.(family,head_width,source_row,participant)]
  differences <- list();set.seed(20261102)
  for(family_name in selection$family) {
    chosen <- selection[family==family_name]$head_width
    a <- losses[family==family_name & head_width==chosen];setorder(a,source_row)
    b <- losses[family==family_name & head_width==32L];setorder(b,source_row)
    stopifnot(identical(a$source_row,b$source_row),identical(a$participant,b$participant))
    totals <- data.table(participant=a$participant,delta=a$loss-b$loss)[,.(sum=sum(delta),n=.N),by=participant]
    boot <- replicate(2000L,{idx <- sample.int(nrow(totals),replace=TRUE);sum(totals$sum[idx])/sum(totals$n[idx])})
    differences[[family_name]] <- data.frame(family=family_name,selected_width=chosen,baseline_width=32L,
      selected_minus_baseline_log_loss=mean(a$loss-b$loss),lower=unname(quantile(boot,.025)),upper=unname(quantile(boot,.975)))
  }
  stopifnot(identical(digest::digest(file.path(output,'selected_widths.csv'),algo='sha256',file=TRUE),selection_sha))
  fwrite(validation,file.path(output,'validation_by_seed.csv'));fwrite(scores,file.path(output,'test_by_seed.csv'))
  fwrite(test_summary,file.path(output,'test_summary.csv'));fwrite(predictions,file.path(output,'test_predictions.csv'))
  fwrite(rbindlist(differences),file.path(output,'paired_selected_vs_baseline.csv'))
  writeLines(c('PASS: frozen source, reference protocol, original identities, scored responses and participant assignments.',
    paste('PASS:',nrow(p$runs),'fresh declared fits; settings and fitting masks checked; all validation metrics independently recomputed.'),
    'PASS: each family selected by mean validation log loss across seeds, with smaller width breaking exact ties.',
    paste('Selection frozen before test scoring; selected_widths.csv SHA256:',selection_sha),
    'PASS: selected and width-32 baseline test predictions independently replayed from saved weights; all test metrics recomputed.',
    if(p$smoke)'SMOKE ONLY: short histories and one epoch; not research performance.' else
      'Exploratory prediction comparison on the previously inspected 70/15/15 participant split; not cross-validation.',
    'Paired intervals use 2000 whole-participant resamples of seed-averaged response losses, conditional on these selected settings and this split.'),
    file.path(output,'verification.txt'))
  html <- paste0('<!doctype html><html lang="en"><meta charset="utf-8"><title>GRU head-width comparison</title>',
    '<style>body{font:16px/1.5 system-ui;max-width:1100px;margin:35px auto;padding:0 20px}table{border-collapse:collapse;margin:20px 0}td,th{padding:8px 12px;border-bottom:1px solid #ddd;text-align:left}th{background:#eef3f7}</style>',
    '<h1>GRU prediction-head width comparison</h1><p>All three input versions; widths 32, 64, 128, 256; ',length(p$seeds),
    ' seeds per setting. Three-layer, 32-unit GRU and one ReLU head hidden layer throughout.</p>',
    '<p><strong>Width selection uses mean validation log loss only.</strong> Lower log loss is better. Scores average individual model metrics across seeds; they are not ensemble scores.</p>',
    if(p$smoke)'<p><strong>SMOKE CHECK ONLY. Not research performance.</strong></p>' else '',
    '<h2>Selected widths</h2>',gru_grid_html_table(selection),
    '<h2>All validation results</h2>',gru_grid_html_table(summary),
    '<h2>Test: selected widths and width-32 baseline</h2>',gru_grid_html_table(test_summary),
    '<h2>Paired test differences</h2><p>Selected minus baseline; negative favors the selected width. Intervals condition on this split and selection.</p>',
    gru_grid_html_table(rbindlist(differences)),
    '<p>Exploratory results on the previously inspected seed-1 70/15/15 whole-participant split, not cross-validation. The three seeds measure initialization variability. ',
    'Every selected and baseline test prediction was replayed from its checkpoint. Widths 79/80/92 from the previous experiment were not candidates in this search.</p>',
    '<p><a href="verification.txt">Verification</a> · <a href="validation_by_seed.csv">Per-seed validation</a> · <a href="test_by_seed.csv">Per-seed test</a></p></html>')
  writeLines(html,file.path(output,'report.html'))
  print(selection);print(test_summary);cat('HEAD WIDTH VERIFIED\n')
  invisible(selection)
}
if(sys.nframe()==0L) {
  args <- commandArgs(TRUE);stopifnot(length(args)==1L)
  summarize_gru_head_widths(args[1L])
}
