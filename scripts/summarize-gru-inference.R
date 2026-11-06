# Recompute every reported metric from saved predictions and compare prespecified
# stages using paired participant losses. No stage or KL weight is selected here.
summarize_gru_inference <- function(output) {
  suppressPackageStartupMessages({library(LKT);library(data.table)})
  protocol_file <- file.path(output,'inference_protocol.rds')
  protocol <- readRDS(protocol_file)
  stopifnot(identical(protocol$schema,1L))
  protocol_sha <- digest::digest(protocol_file,algo='sha256',file=TRUE)
  d <- as.data.table(readRDS(file.path(output,'prepared_data.rds')))
  stopifnot(identical(digest::digest(file.path(output,'prepared_data.rds'),algo='sha256',file=TRUE),protocol$data_sha256),
    identical(digest::digest(file.path(output,'largerawsample_source.rds'),algo='sha256',file=TRUE),protocol$input_sha256))
  for(f in names(protocol$source_hashes))stopifnot(identical(
    digest::digest(file.path(output,'source',f),algo='sha256',file=TRUE),protocol$source_hashes[[f]]))
  original <- readRDS(file.path(output,'largerawsample_source.rds'))
  stopifnot(all(d$KC..Default.==original$KC..Default.[d$comparison_source_row]),
    all(d$KC..Cluster.==original$KC..Cluster.[d$comparison_source_row]),
    all(d$CF..ansbin.==as.integer(original$Outcome[d$comparison_source_row]=='CORRECT')),
    !any(original$Outcome[d$comparison_source_row]=='STUDY'))
  if(!protocol$smoke) {
    ledger <- fread(file.path(output,'response_inclusion.csv'))
    stopifnot(identical(sort(d$comparison_source_row),ledger$source_row[ledger$retained]))
  }
  split <- fread(file.path(output,'participant_split.csv'),colClasses=c(participant='character'))
  stopifnot(all(d$fold==split$fold[match(as.character(d$Anon.Student.Id),split$participant)]))
  for(rows in split(seq_len(nrow(d)),d$Anon.Student.Id))
    stopifnot(all(diff(d$CF..Time.[rows])>=0))
  independent_metrics <- function(y,p) {
    stopifnot(length(y)==length(p),all(y %in% c(0,1)),all(is.finite(p)),all(p>0 & p<1))
    n1 <- sum(y==1);n0 <- sum(y==0)
    c(log_loss=-mean(ifelse(y==1,log(p),log1p(-p))),
      AUC=(sum(rank(p,ties.method='average')[y==1])-n1*(n1+1)/2)/(n1*n0),
      Brier=mean((y-p)^2),RMSE=sqrt(mean((y-p)^2)))
  }

  # Replay held-out histories independently of the cached fit probabilities. Inference
  # diagnostics use means, disable dropout/sampling, and exclude padding and first
  # states from adjacent-state smoothness and KL averages.
  diagnose <- function(fit) {
    s <- fit$gru_settings
    cleanup <- getFromNamespace('lkt_gru_runtime','LKT')(s);on.exit(cleanup())
    net <- getFromNamespace('lkt_gru_network','LKT')(fit$model$preprocessing,s)
    getFromNamespace('lkt_gru_load_weights','LKT')(net,fit$model$weights,s);net$eval()
    test <- as.data.frame(d[d$fold==3L])
    encoded <- getFromNamespace('lkt_gru_encode','LKT')(test,fit$model$preprocessing,fit$model$recipe,s)
    groups <- split(seq_len(nrow(test)),getFromNamespace('lkt_gru_keys','LKT')(test,s$history_group_cols))
    p <- numeric(nrow(test));pairs <- variation <- kl <- variance <- observations <- bound <- 0
    torch::with_no_grad({
      for(start in seq.int(1L,length(groups),by=s$batch_size)) {
        g <- groups[seq.int(start,min(length(groups),start+s$batch_size-1L))]
        longest <- max(lengths(g));chunk <- if(is.null(s$sequence_length))longest else s$sequence_length
        hidden <- context <- NULL
        for(offset in seq.int(1L,longest,by=chunk)) {
          active <- which(lengths(g)>=offset);g <- g[active]
          if(!is.null(hidden))hidden <- hidden[,active,,drop=FALSE]
          context <- getFromNamespace('lkt_gru_context_subset','LKT')(context,active)
          positions <- lapply(g,function(idx)seq.int(offset,min(length(idx),offset+chunk-1L)))
          b <- getFromNamespace('lkt_gru_batch','LKT')(encoded,test,g,positions,s)
          out <- if(s$inference_mode=='none')net(b$numeric,b$categorical,b$answers,hidden) else
            net(b$numeric,b$categorical,b$answers,hidden,context)
          pred <- torch::as_array(torch::torch_sigmoid(out$logits)$cpu())
          keep <- !is.na(b$rows);p[b$rows[keep]] <- pred[keep]
          if(s$inference_mode!='none') {
            current <- out$latent_mean
            prefix <- if(is.null(context))current[,1L,,drop=FALSE]*0 else context$mean
            previous <- torch::torch_cat(list(prefix,current),dim=2)[,seq_len(current$size(2)),,drop=FALSE]
            valid <- b$mask*out$kl_valid
            pairs <- pairs+valid$sum()$item()
            variation <- variation+((current-previous)$abs()$mean(dim=3)*valid)$sum()$item()
            kl <- kl+(out$kl_per_trial*valid)$sum()$item()
            if(s$inference_mode=='gaussian') {
              variance <- variance+(out$latent_logvar$exp()$mean(dim=3)*b$mask)$sum()$item()
              hit <- (out$latent_logvar<=s$logvar_bounds[1L]+1e-6) |
                (out$latent_logvar>=s$logvar_bounds[2L]-1e-6)
              bound <- bound+(hit$to(dtype=current$dtype)$mean(dim=3)*b$mask)$sum()$item()
              observations <- observations+b$mask$sum()$item()
            }
            context <- out$context
          }
          hidden <- out$hidden
        }
      }
    })
    p <- pmin(pmax(p,s$clip_epsilon),1-s$clip_epsilon)
    stopifnot(max(abs(p-fit$prediction[d$fold==3L]))<3e-6)
    c(latent_mean_total_variation=if(pairs>0)variation/pairs else NA_real_,
      latent_adjacent_kl=if(pairs>0 && s$inference_mode=='gaussian')kl/pairs else NA_real_,
      latent_mean_variance=if(observations>0)variance/observations else NA_real_,
      logvar_bound_fraction=if(observations>0)bound/observations else NA_real_)
  }

  scores <- predictions <- list()
  for(i in seq_len(nrow(protocol$runs))) {
    task <- protocol$runs[i,];run_id <- task$run_id
    saved <- readRDS(file.path(output,'models',paste0(run_id,'.rds')));fit <- saved$fit
    stopifnot(identical(saved$protocol_sha256,protocol_sha),identical(saved$run_id,run_id),
      identical(fit$gru_settings,protocol$settings[[run_id]]),
      identical(fit$fit_rows,which(d$fold==1L)),identical(fit$validation_rows,which(d$fold==2L)),
      identical(fit$evaluation_rows,which(d$fold==3L)),fit$parameter_count==task$parameter_count)
    val <- fread(file.path(output,'validation',paste0(run_id,'.csv')))
    v <- independent_metrics(d$CF..ansbin.[d$fold==2],fit$prediction[d$fold==2])
    for(name in names(v))stopifnot(abs(val[[name]]-v[[name]])<1e-12)
    diag <- diagnose(fit)
    for(fold in c(2L,3L))scores[[paste(run_id,fold)]] <- cbind(task,
      partition=c('training','validation','test')[fold],
      as.data.frame(as.list(independent_metrics(d$CF..ansbin.[d$fold==fold],fit$prediction[d$fold==fold]))),
      as.data.frame(as.list(if(fold==3L)diag else setNames(rep(NA_real_,length(diag)),names(diag)))),
      selected_epoch=fit$selected_epoch,epochs_run=nrow(fit$training_history),
      hit_epoch_cap=nrow(fit$training_history)==fit$gru_settings$epochs)
    predictions[[run_id]] <- data.table(run_id=run_id,family=task$family,stage=task$stage,seed=task$seed,
      source_row=d$comparison_source_row[d$fold==3L],participant=d$Anon.Student.Id[d$fold==3L],
      observed=d$CF..ansbin.[d$fold==3L],prediction=fit$prediction[d$fold==3L])
  }
  scores <- rbindlist(scores);predictions <- rbindlist(predictions)
  summary <- scores[,.(log_loss=mean(log_loss),log_loss_seed_sd=sd(log_loss),AUC=mean(AUC),RMSE=mean(RMSE),
    parameter_count=unique(parameter_count),mean_selected_epoch=mean(selected_epoch),cap_hits=sum(hit_epoch_cap),
    latent_mean_total_variation=mean(latent_mean_total_variation),latent_adjacent_kl=mean(latent_adjacent_kl),
    latent_mean_variance=mean(latent_mean_variance),logvar_bound_fraction=mean(logvar_bound_fraction)),
    by=.(family,stage,partition)]

  # Average each response's losses across seeds, then resample whole participants.
  # These intervals condition on this inspected split; they are not cross-validation
  # uncertainty or a new confirmatory test of architectures chosen after prior results.
  losses <- predictions[,.(loss=mean(-ifelse(observed==1,log(prediction),log1p(-prediction)))),
    by=.(family,stage,source_row,participant)]
  differences <- list();set.seed(20261102)
  for(family_name in unique(losses$family)) {
    stages <- unique(losses$stage[losses$family==family_name])
    contrasts <- list(c('capacity_control','baseline'),c('deterministic','baseline'),
      c('deterministic','capacity_control'),c('gaussian','deterministic'))
    for(stage in stages[startsWith(stages,'gaussian_kl_')])contrasts[[length(contrasts)+1L]] <- c(stage,'gaussian')
    for(pair in contrasts) {
      a <- losses[family==family_name & stage==pair[1L]]
      b <- losses[family==family_name & stage==pair[2L]]
      setorder(a,source_row);setorder(b,source_row)
      stopifnot(identical(a$source_row,b$source_row),identical(a$participant,b$participant))
      paired <- data.table(participant=a$participant,delta=a$loss-b$loss)
      totals <- paired[,.(sum=sum(delta),n=.N),by=participant]
      bootstrap <- replicate(2000L,{idx <- sample.int(nrow(totals),replace=TRUE);sum(totals$sum[idx])/sum(totals$n[idx])})
      differences[[length(differences)+1L]] <- data.frame(family=family_name,new_stage=pair[1L],reference_stage=pair[2L],
        new_minus_reference_log_loss=mean(paired$delta),lower=unname(quantile(bootstrap,.025)),upper=unname(quantile(bootstrap,.975)))
    }
  }
  fwrite(scores,file.path(output,'scores_by_seed.csv'));fwrite(summary,file.path(output,'stage_summary.csv'))
  fwrite(predictions,file.path(output,'test_predictions.csv'))
  fwrite(rbindlist(differences),file.path(output,'paired_stage_differences.csv'))
  writeLines(c('PASS: protocol, frozen source hashes, original identities, split assignments and fitting masks.',
    'PASS: all validation/test metrics recomputed independently; test probabilities replayed from each checkpoint.',
    'PASS: all declared stages and seeds present; no test-set stage or KL-weight selection.',
    if(protocol$smoke)'SMOKE CHECK ONLY: short real histories, one epoch, one seed; not scientific performance results.' else
      'FULL EXPLORATORY COMPARISON: previously inspected participant split; not cross-validation.',
    'Paired intervals use 2000 whole-participant resamples of seed-averaged response losses.',
    'Mean-state variation, adjacent Gaussian KL, mean variance and variance-bound frequency are diagnostics, not calibrated proficiency or uncertainty.'),
    file.path(output,'verification.txt'))
  cat('VERIFIED. Summary:',file.path(output,'stage_summary.csv'),'\n')
  invisible(summary)
}
if(sys.nframe()==0L) {
  args <- commandArgs(TRUE);stopifnot(length(args)==1L)
  summarize_gru_inference(args[1L])
}
