# A known zero-effect experiment must produce exactly zero paired effects and
# intervals, including tied predictions and shared participant bootstrap draws.
source('scripts/gru-encoder-study.R');source('scripts/report-sequence-units.R')
d <- data.frame(Anon.Student.Id=rep(paste0('s',1:10),each=4),CF..ansbin.=rep(c(0,1,1,0),10))
tasks <- expand.grid(recipe=c('direct','rich'),unit=c('gru','mamba'),seed=1:3,
  partition=c('development',paste0('cv',1:5)),stringsAsFactors=FALSE)
scratch <- file.path(getwd(),'../LKT-outputs/sequence-mamba')
dir.create(scratch,recursive=TRUE,showWarnings=FALSE)
out <- tempfile('sequence-report-',tmpdir=scratch);dir.create(out);dir.create(file.path(out,'partitions'));dir.create(file.path(out,'reports'))
for(i in 1:5)saveRDS(ifelse(rep(rep(1:5,each=2),each=4)==i,3L,1L),file.path(out,'partitions',paste0('cv',i,'.rds')))
pred <- rep(c(.2,.7,.2,.7),10)
results <- lapply(seq_len(nrow(tasks)),function(i)list(fit=list(prediction=pred,
  fit_rows=1:20,validation_rows=21:28,evaluation_rows=29:40,parameter_count=50,
  selected_epoch=2L,trained_epochs=4L),fit_seconds=1,score_seconds=.01,state_bytes=100,continuation_bytes=200))
p <- list(tasks=tasks,bootstrap_seed=7L,bootstrap_draws=2000L)
sequence_report(p,results,d,out)
comparisons <- read.csv(file.path(out,'reports/paired-auc-comparisons.csv'))
summary <- read.csv(file.path(out,'reports/cv-summary.csv'))
stopifnot(all(as.matrix(comparisons[-1])==0),max(abs(summary$AUC-study_metrics(d$CF..ansbin.,pred)[['AUC']]))<1e-12)
# Every participant has the same four-row profile. These hand-computable AUCs
# (1/2, 3/4, 3/4, 1/4) give fixed paired effects under every bootstrap draw.
profiles <- list(direct_gru=c(.2,.7,.2,.7),direct_mamba=c(.2,.7,.55,.6),
  rich_gru=c(.2,.7,.55,.6),rich_mamba=1-c(.2,.7,.55,.6))
for(i in seq_along(results))results[[i]]$fit$prediction <-
  rep(profiles[[paste(tasks$recipe[i],tasks$unit[i],sep='_')]],10L)
sequence_report(p,results,d,out)
comparisons <- read.csv(file.path(out,'reports/paired-auc-comparisons.csv'))
expected <- c(.25,-.5,-.75)
stopifnot(max(abs(comparisons$AUC_difference-expected))<1e-12,
  max(abs(comparisons$lower_95-expected))<1e-12,max(abs(comparisons$upper_95-expected))<1e-12)
# Unequal histories and heterogeneous students: independently replicate every
# sampled participant's rows and compare ordinary rank AUC with weighted AUC.
group <- rep(1:10,2:11);within <- sequence(2:11)
variable <- data.frame(Anon.Student.Id=paste0('s',group),CF..ansbin.=c(0,1,1,0)[(within-1L)%%4L+1L])
fold <- rep(1:5,each=2)[group]
for(i in 1:5)saveRDS(ifelse(fold==i,3L,1L),file.path(out,'partitions',paste0('cv',i,'.rds')))
matrices <- lapply(profiles,function(profile)sapply(1:3,function(seed) {
  values <- profile[(within-1L)%%4L+1L]
  inverse <- group%%(seed+1L)==0L;values[inverse] <- 1-values[inverse]
  values
}))
for(i in seq_along(results))results[[i]]$fit$prediction <-
  matrices[[paste(tasks$recipe[i],tasks$unit[i],sep='_')]][,tasks$seed[i]]
p$bootstrap_draws <- 200L
sequence_report(p,results,variable,out)
actual <- readRDS(file.path(out,'reports/paired-bootstrap.rds'))
ids <- sort(unique(variable$Anon.Student.Id));set.seed(p$bootstrap_seed)
for(i in 1:20) {
  sampled <- sample.int(length(ids),length(ids),replace=TRUE)
  rows <- unlist(lapply(sampled,function(j)which(variable$Anon.Student.Id==ids[j])),use.names=FALSE)
  auc <- vapply(matrices,function(predictions)mean(vapply(1:3,function(seed)
    study_metrics(variable$CF..ansbin.[rows],predictions[rows,seed])[['AUC']],numeric(1))),numeric(1))
  direct <- auc[['direct_mamba']]-auc[['direct_gru']];rich <- auc[['rich_mamba']]-auc[['rich_gru']]
  stopifnot(max(abs(actual[i,]-c(direct,rich,rich-direct)))<1e-12)
}
resolved <- normalizePath(out,winslash='/',mustWork=TRUE)
allowed <- paste0(normalizePath(scratch,winslash='/',mustWork=TRUE),'/sequence-report-')
stopifnot(startsWith(resolved,allowed))
unlink(resolved,recursive=TRUE)
cat('PASS tied-score AUC, known effects, interaction signs and weighted bootstrap versus literal participant replication\n')
