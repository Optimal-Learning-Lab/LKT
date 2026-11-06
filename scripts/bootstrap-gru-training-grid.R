# Paired participant bootstrap for the validation-selected procedure vs baseline.
# Whole participant histories move together. This measures uncertainty conditional
# on the fitted models and fixed selections; it does not rerun training or tuning.
args <- commandArgs(TRUE);stopifnot(length(args)==3L)
root <- args[1];output <- args[2];reports <- args[3]
source(file.path(root,'scripts/gru-encoder-study.R'))
suppressPackageStartupMessages(library(data.table))
p <- readRDS(file.path(output,'study.rds'))
stopifnot(study_hash(file.path(root,'scripts/gru-encoder-study.R'))==p$script_hashes[['scripts/gru-encoder-study.R']])
selected <- fread(file.path(reports,'selected-predictions.csv'));selected$procedure <- 'selected'
baseline <- rbindlist(lapply(p$partitions,function(part)rbindlist(lapply(0:(p$shards-1L),function(j)
  fread(file.path(output,'reports',paste0(part,'-shard',j,'-predictions.csv')))[condition=='baseline']))))
baseline$procedure <- 'baseline'
x <- rbindlist(list(selected,baseline),use.names=TRUE,fill=TRUE)
stopifnot(!anyDuplicated(x[,.(procedure,seed,source_row)]))
participants <- sort(unique(x$participant));stopifnot(length(participants)==478L)
prepared <- list()
for(procedure_name in c('baseline','selected'))for(seed_number in p$seeds) {
  rows <- x[procedure==procedure_name & seed==seed_number];setorder(rows,prediction)
  stopifnot(nrow(rows)==55122L)
  expected <- unname(study_metrics(rows$observed,rows$prediction)[['AUC']])
  independent <- as.numeric(pROC::auc(rows$observed,rows$prediction,levels=c(0,1),direction='<',quiet=TRUE))
  stopifnot(abs(expected-independent)<1e-12)
  prepared[[paste(procedure_name,seed_number)]] <- list(participant=match(rows$participant,participants),
    y=rows$observed,ends=c(which(diff(rows$prediction)!=0),nrow(rows)),expected=expected)
}
weighted_auc <- function(z,counts) {
  w <- counts[z$participant]
  pos <- cumsum(w*z$y)[z$ends];neg <- cumsum(w*(1-z$y))[z$ends]
  pg <- diff(c(0,pos));ng <- diff(c(0,neg))
  sum(pg*(c(0,head(neg,-1))+.5*ng))/(tail(pos,1)*tail(neg,1))
}
values <- function(counts) {
  v <- vapply(prepared,weighted_auc,numeric(1),counts=counts)
  c(baseline=mean(v[paste('baseline',p$seeds)]),selected=mean(v[paste('selected',p$seeds)]))
}
unit_values <- vapply(prepared,weighted_auc,numeric(1),counts=rep(1,length(participants)))
stopifnot(max(abs(unit_values-vapply(prepared,`[[`,numeric(1),'expected')))<1e-12)
point <- values(rep(1,length(participants)))
primary <- fread(file.path(reports,'primary-auc.csv'))
stopifnot(abs(point[['selected']]-primary$CV_AUC)<1e-12,
  abs(point[['baseline']]-primary$baseline_CV_AUC)<1e-12)
set.seed(20261105L);B <- 2000L;draws <- numeric(B)
for(i in seq_len(B)) {
  counts <- as.double(tabulate(sample.int(length(participants),length(participants),replace=TRUE),nbins=length(participants)))
  a <- values(counts);draws[i] <- a[['selected']]-a[['baseline']]
}
result <- data.frame(comparison='validation-selected procedure minus baseline',
  AUC_gain=point[['selected']]-point[['baseline']],lower95=unname(quantile(draws,.025)),
  upper95=unname(quantile(draws,.975)),participants=length(participants),replicates=B)
study_write_csv(result,file.path(reports,'paired-participant-auc-bootstrap.csv'))
study_write_rds(list(draws=draws,participants=participants,replicates=B,
  source_sha256=study_hash(file.path(root,'scripts/bootstrap-gru-training-grid.R'))),
  file.path(reports,'paired-participant-auc-bootstrap.rds'))
print(result)
cat('PASS six independently checked pROC AUCs and canonical point estimates.\n')
