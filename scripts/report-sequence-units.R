sequence_report <- function(protocol,results,data,output) {
  metric <- function(y,p) {
    m <- study_metrics(y,p)
    calibration <- stats::glm(y~stats::qlogis(p),family=stats::binomial())
    c(m,calibration_intercept=unname(stats::coef(calibration)[1]),calibration_slope=unname(stats::coef(calibration)[2]))
  }
  tasks <- protocol$tasks;keys <- c('direct_gru','direct_mamba','rich_gru','rich_mamba')
  pooled <- setNames(vector('list',4L),keys);summary <- development <- list()
  for(key in keys) {
    bits <- strsplit(key,'_',fixed=TRUE)[[1]]
    selected <- which(tasks$recipe==bits[1] & tasks$unit==bits[2] & tasks$partition!='development')
    predictions <- matrix(NA_real_,nrow(data),3L)
    for(i in selected) {
      folds <- readRDS(file.path(output,'partitions',paste0(tasks$partition[i],'.rds')))
      rows <- which(folds==3L)
      stopifnot(all(is.na(predictions[rows,tasks$seed[i]])))
      predictions[rows,tasks$seed[i]] <- results[[i]]$fit$prediction[rows]
    }
    stopifnot(!anyNA(predictions));pooled[[key]] <- predictions
    measurements <- sapply(1:3,function(seed)metric(data$CF..ansbin.,predictions[,seed]))
    train <- vapply(selected,function(i) {
      fit <- results[[i]]$fit;rows <- fit$fit_rows
      study_metrics(data$CF..ansbin.[rows],fit$prediction[rows])[['AUC']]
    },numeric(1))
    valid <- vapply(selected,function(i) {
      fit <- results[[i]]$fit;rows <- fit$validation_rows
      study_metrics(data$CF..ansbin.[rows],fit$prediction[rows])[['AUC']]
    },numeric(1))
    summary[[key]] <- data.frame(recipe=bits[1],unit=bits[2],training_AUC=mean(train),validation_AUC=mean(valid),
      as.list(rowMeans(measurements)),mean_fit_seconds=mean(vapply(results[selected],`[[`,numeric(1),'fit_seconds')),
      mean_score_seconds=mean(vapply(results[selected],`[[`,numeric(1),'score_seconds')),
      mean_state_bytes=mean(vapply(results[selected],`[[`,numeric(1),'state_bytes')),
      mean_continuation_bytes=mean(vapply(results[selected],`[[`,numeric(1),'continuation_bytes')),
      parameter_count=results[[selected[1]]]$fit$parameter_count,
      mean_selected_epoch=mean(vapply(results[selected],function(x)as.numeric(x$fit$selected_epoch),numeric(1))),
      epoch_cap_count=sum(vapply(results[selected],function(x)x$fit$trained_epochs==100L,logical(1))))
    selected_dev <- which(tasks$recipe==bits[1] & tasks$unit==bits[2] & tasks$partition=='development')
    development[[key]] <- data.frame(recipe=bits[1],unit=bits[2],
      as.list(rowMeans(sapply(selected_dev,function(i) {
        rows <- results[[i]]$fit$evaluation_rows
        metric(data$CF..ansbin.[rows],results[[i]]$fit$prediction[rows])
      }))))
  }
  summary <- do.call(rbind,summary);development <- do.call(rbind,development)
  # Sorting is independent of bootstrap weights. Tie groups receive half credit.
  subjects <- match(as.character(data$Anon.Student.Id),sort(unique(as.character(data$Anon.Student.Id))))
  auc_functions <- lapply(pooled,function(predictions)lapply(1:3,function(seed) {
    order <- order(predictions[,seed]);p <- predictions[order,seed];y <- data$CF..ansbin.[order];id <- subjects[order]
    ties <- rle(p);ends <- cumsum(ties$lengths);groups <- rep(seq_along(ends),ties$lengths)
    function(counts) {
      positive <- counts[id]*y;negative <- counts[id]*(1-y);cumulative <- cumsum(negative)
      before <- c(0,cumulative[head(ends,-1L)])
      credit <- before[groups]+.5*(cumulative[ends][groups]-before[groups])
      sum(positive*credit)/(sum(positive)*sum(negative))
    }
  }))
  set.seed(protocol$bootstrap_seed);n <- max(subjects)
  bootstrap <- matrix(NA_real_,protocol$bootstrap_draws,3L,
    dimnames=list(NULL,c('direct_mamba_minus_gru','rich_mamba_minus_gru','rich_minus_direct_effect')))
  for(i in seq_len(nrow(bootstrap))) {
    counts <- tabulate(sample.int(n,n,replace=TRUE),nbins=n)
    auc <- vapply(auc_functions,function(f)mean(vapply(f,function(g)g(counts),numeric(1))),numeric(1))
    direct <- auc[['direct_mamba']]-auc[['direct_gru']];rich <- auc[['rich_mamba']]-auc[['rich_gru']]
    bootstrap[i,] <- c(direct,rich,rich-direct)
  }
  observed <- setNames(summary$AUC,paste(summary$recipe,summary$unit,sep='_'))
  direct <- observed[['direct_mamba']]-observed[['direct_gru']]
  rich <- observed[['rich_mamba']]-observed[['rich_gru']]
  comparisons <- data.frame(comparison=colnames(bootstrap),AUC_difference=c(direct,rich,rich-direct),
    lower_95=apply(bootstrap,2,stats::quantile,.025),upper_95=apply(bootstrap,2,stats::quantile,.975))
  reports <- file.path(output,'reports')
  write.csv(summary,file.path(reports,'cv-summary.csv'),row.names=FALSE)
  write.csv(development,file.path(reports,'development-summary.csv'),row.names=FALSE)
  write.csv(comparisons,file.path(reports,'paired-auc-comparisons.csv'),row.names=FALSE)
  saveRDS(bootstrap,file.path(reports,'paired-bootstrap.rds'))
  table <- function(d) {
    d[] <- lapply(d,function(x)if(is.numeric(x))formatC(x,format='f',digits=6) else as.character(x))
    c(paste0('| ',paste(names(d),collapse=' | '),' |'),paste0('| ',paste(rep('---',ncol(d)),collapse=' | '),' |'),
      apply(d,1,function(row)paste0('| ',paste(row,collapse=' | '),' |')))
  }
  lines <- c('# GRU versus native Mamba-1: completed fixed-setting experiment','',
    'All 72 fits completed and passed independent saved-weight replay. The primary result is held-out AUC.','',
    table(summary[c('recipe','unit','training_AUC','validation_AUC','AUC','log_loss','Brier')]),'',
    '## Paired architecture effects','',table(comparisons),'',
    'Intervals use 2,000 paired whole-participant bootstrap draws shared across conditions and seeds. They condition on fitted models and are exploratory, without multiplicity adjustment.','',
    '## Resource use and calibration','',table(summary[c('recipe','unit','parameter_count','mean_fit_seconds','mean_score_seconds','mean_state_bytes','mean_selected_epoch','epoch_cap_count')]),'',
    table(summary[c('recipe','unit','calibration_intercept','calibration_slope','RMSE','mean_continuation_bytes')]),'',
    'Scoring seconds are warm per-candidate measurements with the existing engineered-feature history reconstruction included. State bytes measure the serialized unit memory after 20 responses; continuation bytes additionally include retained feature history and retrieval context. Different folds can have different training-vocabulary sizes, so the displayed parameter count is the first CV fit, with per-fit counts retained in saved models.','',
    '## Separate development results','',table(development),'',
    '## Design and limits','',
    'The experiment reuses 55,122 scored responses from 478 participants, 144 items, and 36 skills. Five participant-held-out folds are pooled within each seed, then three seed AUCs are averaged. The development split is excluded from the CV result. Training and validation AUCs average the 15 restored CV fits per condition.','',
    'The direct recipe removes learned feature towers, retrieval, and zero item initialization; the rich recipe retains the frozen all-three configuration. Both use three layers, a 32-value readable memory, the same 128-unit head, and the same prepared predictors. Internal state size and parameter counts differ.','',
    'Every fit starts fresh. Validation log loss selects checkpoints; held-out labels select neither epochs nor hyperparameters. Training uses fixed GRU-derived settings, not architecture-specific tuning. Fixed engineered-feature coefficients and prior architectural choices are inherited from earlier work. These repeatedly inspected data do not provide a new confirmatory test.','',
    'Native R/Torch Mamba uses a differentiable reference scan. Its timing measures this implementation, not fused CUDA Mamba. No result should be described as an optimized-kernel benchmark or a reproduction of Mamba4KT, DGMKT, or MCKT.')
  writeLines(lines,file.path(reports,'RESULTS.md'))
  cat('PASS completed report and paired participant bootstrap\n')
}
