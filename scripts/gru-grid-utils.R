# Utilities shared by the grid runner, progress report, and result evaluator.
# Use the same scoring definitions for every candidate. Log loss is the selection
# criterion; the other metrics describe ranking and probability accuracy.
gru_grid_metrics <- function(y,p) {
  stopifnot(length(y)==length(p),length(y)>0L,all(y %in% c(0,1)),
    all(is.finite(p)),all(p>0 & p<1))
  c(log_loss=-mean(dbinom(y,1,p,log=TRUE)),
    AUC=as.numeric(pROC::auc(y,p,levels=c(0,1),direction='<',quiet=TRUE)),
    Brier=mean((y-p)^2),RMSE=sqrt(mean((y-p)^2)))
}

# Commit a model checkpoint only after its complete file has been written. An
# interrupted write must not be mistaken for a successfully finished fit.
gru_grid_atomic_rds <- function(object,path) {
  stopifnot(!file.exists(path))
  temporary <- paste0(path,'.tmp')
  saveRDS(object,temporary)
  if(!file.rename(temporary,path))stop('Cannot commit checkpoint: ',path)
  invisible(path)
}

# Make each validation-score file a complete, immutable record. A resumed task may
# confirm the same file, but it must not replace it with different scores.
gru_grid_atomic_csv <- function(object,path) {
  temporary <- paste0(path,'.tmp')
  write.csv(object,temporary,row.names=FALSE)
  if(file.exists(path)) {
    stopifnot(identical(readLines(path,warn=FALSE),readLines(temporary,warn=FALSE)))
    unlink(temporary)
  } else if(!file.rename(temporary,path))stop('Cannot commit score file: ',path)
  invisible(path)
}

# Check that the fitted object actually used the requested settings and row partitions.
# This guards both newly trained models and models loaded from an earlier checkpoint.
gru_grid_validate_fit <- function(fit,settings,data) {
  stopifnot(identical(fit$gru_settings,settings),
    identical(fit$fit_rows,which(data$fold==1L)),
    identical(fit$validation_rows,which(data$fold==2L)),
    identical(fit$evaluation_rows,which(data$fold==3L)),
    length(fit$prediction)==nrow(data),all(is.finite(fit$prediction)))
  invisible(TRUE)
}

# Run one configuration with one training seed. Keeping this unit independent makes
# parallel execution and resumption use the same code path.
gru_grid_run_candidate <- function(run_id) {
  task <- protocol$runs[match(run_id,protocol$runs$run_id),]
  stopifnot(nrow(task)==1L)
  settings <- protocol$run_settings[[run_id]]
  checkpoint_file <- file.path(output,'models',paste0(run_id,'.rds'))
  # Reuse a completed checkpoint from this exact grid when resuming. Its protocol
  # fingerprint must match before we trust its fitted weights or predictions.
  if(file.exists(checkpoint_file)) {
    checkpoint <- readRDS(checkpoint_file)
    stopifnot(identical(checkpoint$protocol_sha256,protocol_sha256))
    cat('RESUME',run_id,'\n');flush.console()
  } else {
    # The optional earlier-fit cache is separate from resuming this grid. Its source
    # file is verified before reuse, and cross-platform runs explicitly disable this
    # cache.
    if(!is.na(task$cache_file)) {
      stopifnot(identical(digest::digest(task$cache_file,algo='sha256',file=TRUE),task$cache_sha256))
      fit <- readRDS(task$cache_file)
      elapsed <- task$cache_fit_seconds
      origin <- 'reused'
      cat('REUSE',run_id,'\n');flush.console()
    } else {
      cat('START',run_id,format(Sys.time(),'%H:%M:%S'),'\n');flush.console()
      # Fit on partition 1 and use partition 2 for the GRU stopping rule. Partition 3
      # remains outside weight optimization and configuration selection.
      elapsed <- system.time(
        fit <- LKT(
          data.table::copy(data),
          usefolds = 1,
          interc = TRUE,
          components = protocol$baseline$components,
          features = protocol$baseline$features,
          fixedpars = protocol$baseline$fixedpars,
          model = do.call(GRUModel, settings),
          verbose = FALSE
        )
      )[['elapsed']]
      origin <- 'trained'
    }
    gru_grid_validate_fit(fit,settings,data)
    # Keep the trained network and its diagnostic metadata without duplicating the full
    # raw dataset in every checkpoint. The separately saved input and prepared data
    # provide the common data record.
    fit$newdata <- NULL;fit$nullmodel <- NULL
    checkpoint <- list(protocol_sha256=protocol_sha256,run_id=run_id,fit=fit,
      fit_seconds=elapsed,origin=origin,completed_utc=format(Sys.time(),tz='UTC',usetz=TRUE))
    gru_grid_atomic_rds(checkpoint,checkpoint_file)
  }
  fit <- checkpoint$fit
  stopifnot(identical(checkpoint$run_id,run_id))
  gru_grid_validate_fit(fit,settings,data)
  # Per-fit CSV files contain validation metrics only. Test scoring occurs only
  # after the complete grid's configuration choices have been written to disk.
  # Score validation responses here, because these are the scores used to compare
  # configurations. Test scoring is deferred until the entire grid has finished and the
  # choices are frozen.
  keep <- data$fold==2L
  score <- cbind(task[,c('run_id','candidate_id','family','seed','num_layers',
    'hidden_size','learning_rate','dropout','weight_decay')],
    as.data.frame(as.list(gru_grid_metrics(data$CF..ansbin.[keep],fit$prediction[keep]))),
    data.frame(selected_epoch=fit$selected_epoch,epochs_run=nrow(fit$training_history),
      hit_epoch_cap=nrow(fit$training_history)==settings$epochs,
      parameter_count=fit$parameter_count,fit_seconds=checkpoint$fit_seconds,origin=checkpoint$origin))
  write.csv(fit$training_history,file.path(output,'training',paste0(run_id,'.csv')),row.names=FALSE)
  gru_grid_atomic_csv(score,file.path(output,'validation',paste0(run_id,'.csv')))
  cat('DONE',run_id,'validation log loss',format(score$log_loss,digits=6),
    'epochs',score$epochs_run,'selected',score$selected_epoch,'\n');flush.console()
  score
}

# Combine the completed per-fit records into a readable search table. Final selection
# requires every declared fit, whereas a progress report can show an incomplete search.
gru_grid_validation <- function(output,protocol,require_complete=FALSE) {
  files <- list.files(file.path(output,'validation'),pattern='[.]csv$',full.names=TRUE)
  if(!length(files))return(list(runs=data.table::data.table(),summary=data.table::data.table()))
  runs <- data.table::rbindlist(lapply(files,data.table::fread))
  stopifnot(!anyDuplicated(runs$run_id),all(runs$run_id %in% protocol$runs$run_id))
  if(require_complete)stopifnot(setequal(runs$run_id,protocol$runs$run_id))
  # Average scores across training seeds for each configuration. This compares repeated
  # single-model fits rather than averaging their probabilities into an ensemble.
  summary <- runs[,.(training_seeds=.N,mean_validation_log_loss=mean(log_loss),
    sd_validation_log_loss=if(.N>1L)sd(log_loss) else NA_real_,
    mean_validation_AUC=mean(AUC),mean_selected_epoch=mean(selected_epoch),
    epoch_cap_runs=sum(hit_epoch_cap),reused_runs=sum(origin=='reused')),
    by=.(candidate_id,family,num_layers,hidden_size,learning_rate,dropout,weight_decay)]
  data.table::setorderv(summary,c('family','mean_validation_log_loss','candidate_id'))
  list(runs=runs,summary=summary)
}

gru_grid_html_table <- function(d) {
  d <- as.data.frame(d)
  if(!nrow(d))return('<p>No completed fits yet.</p>')
  d[] <- lapply(d,function(x)if(is.numeric(x))format(round(x,5),trim=TRUE) else as.character(x))
  escape <- function(x)gsub('<','&lt;',gsub('&','&amp;',x,fixed=TRUE),fixed=TRUE)
  paste0('<table><tr>',paste0('<th>',escape(names(d)),'</th>',collapse=''),'</tr>',
    paste(vapply(seq_len(nrow(d)),function(i)paste0('<tr>',paste0('<td>',
      escape(unlist(d[i,],use.names=FALSE)),'</td>',collapse=''),'</tr>'),character(1)),collapse=''),'</table>')
}
