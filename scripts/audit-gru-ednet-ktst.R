# Recompute EdNet metrics after correcting double-count arithmetic in the shared AUC helper.
# Preserve the frozen training protocol and original failed report; no fitted object is changed.
# Args: ROOT RESULTS FROZEN_BUNDLE
args <- commandArgs(TRUE);stopifnot(length(args)==3L)
root <- normalizePath(args[1],winslash='/',mustWork=TRUE);output <- args[2];frozen <- args[3]
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-encoder-study.R'))
suppressPackageStartupMessages(library(data.table))
p <- readRDS(file.path(output,'study.rds'))
release <- lkt_release[c('lkt_version','lkt_source_archive','lkt_source_sha256','lkt_manifest_sha256')]
stopifnot(identical(release,p$release),study_hash(attr(lkt_release,'file'))==p$release_file_sha256,
  study_hash(attr(lkt_release,'archive'))==release[['lkt_source_sha256']],
  study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256)
for(name in names(p$script_hashes))stopifnot(study_hash(file.path(frozen,name))==p$script_hashes[[name]])
# The only correction to the shared helper is coercing class counts to doubles.
# Check this exact difference so none of the fitting or replay functions change.
old <- readLines(file.path(frozen,'scripts/gru-encoder-study.R'))
expected <- c()
for(line in old) {
  if(identical(line,'  n1 <- sum(y==1);n0 <- sum(y==0)'))expected <- c(expected,
    "  # Counts must be doubles before multiplication: a large, valid dataset can",
    "  # exceed R's signed-integer product limit even when its row count is modest.",
    '  n1 <- as.double(sum(y==1));n0 <- as.double(sum(y==0))') else expected <- c(expected,line)
}
stopifnot(identical(expected,readLines(file.path(root,'scripts/gru-encoder-study.R'))))
d <- readRDS(file.path(output,'data/prepared_data.rds'));action <- 'report'

  metrics <- list()
  for(seed in p$seeds) {
    saved <- readRDS(file.path(output,'models',paste0('base-seed',seed,'.rds')));fit <- saved$fit
    s <- p$settings;s$seed <- as.integer(seed)
    stopifnot(identical(fit$gru_settings,s),saved$protocol_sha256==study_hash(file.path(output,'study.rds')),
      identical(fit$fit_rows,which(d$fold==1L)),identical(fit$validation_rows,which(d$fold==2L)),
      identical(fit$evaluation_rows,which(d$fold==3L)),
      file.exists(file.path(output,'reports',paste0('base-seed',seed,'-verification.txt'))))
    q <- fread(file.path(output,'reports',paste0('base-seed',seed,'-predictions.csv')))
    stopifnot(identical(q$source_row,d$comparison_source_row[d$fold==3]),
      max(abs(q$prediction-fit$prediction[d$fold==3]))<3e-6)
    for(phase in 1:3)for(first in c(FALSE,TRUE)) {
      keep <- d$fold==phase & (first | d$ktst_position>1L)
      metrics[[paste(seed,phase,first)]] <- data.frame(seed=seed,phase=c('training','validation','test')[phase],
        include_first_response=first,responses=sum(keep),AUC=unname(study_metrics(d$CF..ansbin.[keep],fit$prediction[keep])[['AUC']]),
        selected_epoch=fit$selected_epoch,trained_epochs=fit$trained_epochs,parameter_count=fit$parameter_count)
    }
    train_auc <- unname(study_metrics(d$CF..ansbin.[d$fold==1],fit$prediction[d$fold==1])[['AUC']])
    stopifnot(abs(train_auc-fit$training_history$training_auc[match(fit$selected_epoch,fit$training_history$epoch)])<1e-12)
  }
  metrics <- rbindlist(metrics)
  summary <- metrics[,.(AUC=mean(AUC),seed_sd=sd(AUC),responses=unique(responses)),by=.(phase,include_first_response)]
  if(action=='report') {
    study_write_csv(metrics,file.path(output,'reports/auc-by-seed.csv'))
    study_write_csv(summary,file.path(output,'reports/auc-summary.csv'))
    writeLines('PASS all three base settings, participant masks, saved held-out replays and restored training AUCs.',file.path(output,'reports/verification.txt'))
  } else stopifnot(isTRUE(all.equal(as.data.frame(fread(file.path(output,'reports/auc-summary.csv'))),as.data.frame(summary),tolerance=1e-12,check.attributes=FALSE)))
  print(summary)

cat('PASS corrected large-sample AUC audit; original training source and models preserved.\n')
