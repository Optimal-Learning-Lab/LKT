# Shared fixed-condition reporting and participant-level cross-dataset interaction.
feature_ablation_weighted_auc <- function(z,counts) {
  w <- counts[z$participant];pos <- cumsum(w*z$y)[z$ends];neg <- cumsum(w*(1-z$y))[z$ends]
  stopifnot(tail(pos,1)>0,tail(neg,1)>0)
  sum(diff(c(0,pos))*(c(0,head(neg,-1))+.5*diff(c(0,neg))))/(tail(pos,1)*tail(neg,1))
}

feature_ablation_report <- function(output,plans,tasks_for,action='report') {
  stopifnot(action %in% c('report','check'))
  summary <- seeds <- records <- list();prepared <- list();controls <- list()
  for(name in names(plans)) {
    p <- plans[[name]];target <- file.path(output,name)
    stopifnot(length(list.files(file.path(target,'models'),pattern='[.]rds$'))==30L)
    d <- readRDS(file.path(target,'data/prepared_data.rds'));predictions <- list();fits <- list()
    reference <- NULL
    if(!is.null(p$reference_controls_sha256)) {
      path <- file.path(target,'data/reference-controls.rds')
      stopifnot(study_hash(path)==p$reference_controls_sha256);reference <- readRDS(path)
    }
    for(part in p$partitions) {
      stopifnot(file.exists(file.path(target,'reports',paste0(part,'-verification.txt'))))
      q <- data.table::fread(file.path(target,'reports',paste0(part,'-predictions.csv')))
      fold <- readRDS(file.path(target,'partitions',paste0(part,'.rds')));tasks <- tasks_for(part)
      for(i in seq_len(nrow(tasks))) {
        t <- tasks[i,];saved <- readRDS(file.path(target,'models',paste0(t$run_id,'.rds')));f <- saved$fit
        s <- p$settings[[t$candidate]];s$seed <- as.integer(t$seed)
        stopifnot(identical(s,f$gru_settings),identical(saved$settings,s),
          identical(f$fit_rows,which(fold==1L)),identical(f$validation_rows,which(fold==2L)),
          identical(f$evaluation_rows,which(fold==3L)),all(is.finite(unlist(f$model$weights))),
          identical(saved$fingerprint,digest::digest(list(study=study_hash(file.path(target,'study.rds')),
            task=saved$task,settings=s),algo='sha256')))
        if(t$candidate=='no_features')stopifnot(length(f$model$preprocessing$numeric_names)==0L)
        z <- q[condition==t$candidate & seed==t$seed]
        z <- z[match(d$comparison_source_row[fold==3L],source_row)]
        stopifnot(nrow(z)==sum(fold==3L),!anyNA(z),
          all(z$source_row==d$comparison_source_row[fold==3L]),
          all(z$participant==d$Anon.Student.Id[fold==3L]),
          max(abs(z$prediction-f$prediction[fold==3L]))<1e-12)
        tr <- study_metrics(d$CF..ansbin.[fold==1L],f$prediction[fold==1L])
        va <- study_metrics(d$CF..ansbin.[fold==2L],f$prediction[fold==2L])
        stopifnot(abs(tr[['AUC']]-f$training_history$training_auc[match(f$selected_epoch,f$training_history$epoch)])<1e-12)
        v <- read.csv(file.path(target,'validation',paste0(t$run_id,'.csv')))
        for(metric in names(va))stopifnot(abs(v[[metric]]-va[[metric]])<1e-12)
        fits[[t$run_id]] <- data.frame(dataset=name,partition=part,condition=t$candidate,seed=t$seed,
          training_AUC=unname(tr[['AUC']]),validation_AUC=unname(va[['AUC']]),
          selected_epoch=f$selected_epoch,epochs_run=f$trained_epochs,cap_hit=f$trained_epochs==s$epochs,
          parameter_count=f$parameter_count,fit_seconds=saved$elapsed)
        if(!is.null(reference) && t$candidate=='baseline') {
          delta <- max(abs(f$prediction-reference[[paste(part,t$seed)]]$prediction))
          stopifnot(delta<3e-6)
          controls[[t$run_id]] <- data.frame(run_id=t$run_id,maximum_probability_difference=delta)
        }
      }
      predictions[[part]] <- q
    }
    x <- data.table::rbindlist(predictions)
    stopifnot(!anyDuplicated(x[,.(condition,seed,source_row)]),
      all(x[,.N,by=.(condition,seed)]$N==nrow(d)),
      all(x$observed==d$CF..ansbin.[match(x$source_row,d$comparison_source_row)]))
    pooled <- x[,as.list(study_metrics(observed,prediction)),by=.(dataset,condition,seed)]
    independent <- x[,.(reference_AUC=as.numeric(pROC::auc(observed,prediction,levels=c(0,1),direction='<',quiet=TRUE))),by=.(condition,seed)]
    joined <- merge(pooled,independent,by=c('condition','seed'))
    stopifnot(max(abs(joined$AUC-joined$reference_AUC))<1e-12)
    r <- data.table::rbindlist(fits)
    a <- pooled[,.(CV_AUC=mean(AUC),CV_log_loss=mean(log_loss),CV_Brier=mean(Brier),seed_sd=sd(AUC)),by=.(dataset,condition)]
    b <- r[,.(training_AUC=mean(training_AUC),validation_AUC=mean(validation_AUC),cap_hits=sum(cap_hit),
      parameters_min=min(parameter_count),parameters_max=max(parameter_count)),by=.(dataset,condition)]
    summary[[name]] <- merge(a,b,by=c('dataset','condition'));seeds[[name]] <- pooled;records[[name]] <- r
    ids <- sort(unique(x$participant));prepared[[name]] <- list(ids=ids,groups=list())
    for(condition_name in c('baseline','no_features'))for(seed_number in p$seeds) {
      z <- x[condition==condition_name & seed==seed_number]
      data.table::setorder(z,prediction)
      key <- paste(condition_name,seed_number)
      prepared[[name]]$groups[[key]] <- list(participant=match(z$participant,ids),y=z$observed,
        prediction=z$prediction,ends=c(which(diff(z$prediction)!=0),nrow(z)))
    }
  }
  weighted_auc <- feature_ablation_weighted_auc
  contrast <- function(z,counts) {
    scores <- vapply(z$groups,weighted_auc,numeric(1),counts=counts)
    mean(scores[paste('baseline',1:3)])-mean(scores[paste('no_features',1:3)])
  }
  out <- file.path(output,'reports');dir.create(out,showWarnings=FALSE)
  point <- vapply(prepared,function(z)contrast(z,rep(1,length(z$ids))),numeric(1))
  bootstrap_file <- file.path(out,'participant-bootstrap.rds')
  if(action=='report') {
    draws <- matrix(NA_real_,2000L,2L,dimnames=list(NULL,names(prepared)))
    for(i in seq_along(prepared)) {
      z <- prepared[[i]];set.seed(20261106L+i)
      for(b in 1:2000) {
        counts <- as.double(tabulate(sample.int(length(z$ids),length(z$ids),replace=TRUE),nbins=length(z$ids)))
        draws[b,i] <- contrast(z,counts)
      }
    }
    study_write_rds(list(draws=draws,seed_by_dataset=setNames(20261106L+seq_along(prepared),names(prepared)),
      inference='Whole participants paired across feature conditions/seeds, independent across datasets; fitted models fixed.'),bootstrap_file)
  } else draws <- readRDS(bootstrap_file)$draws
  # Independent literal replication validates the first ten actual-data draws.
  maximum_bootstrap_error <- 0
  for(i in seq_along(prepared)) {
    z <- prepared[[i]];set.seed(20261106L+i)
    for(b in 1:10) {
      counts <- tabulate(sample.int(length(z$ids),length(z$ids),replace=TRUE),nbins=length(z$ids))
      scores <- vapply(z$groups,function(g) {
        rows <- rep(seq_along(g$y),counts[g$participant])
        as.numeric(pROC::auc(g$y[rows],g$prediction[rows],levels=c(0,1),direction='<',quiet=TRUE))
      },numeric(1))
      literal <- mean(scores[paste('baseline',1:3)])-mean(scores[paste('no_features',1:3)])
      maximum_bootstrap_error <- max(maximum_bootstrap_error,abs(literal-draws[b,i]))
    }
  }
  stopifnot(maximum_bootstrap_error<1e-12)
  diffs <- data.frame(dataset=names(point),AUC_loss_on_removal=as.numeric(point),
    lower95=apply(draws,2,quantile,.025),upper95=apply(draws,2,quantile,.975))
  interaction <- data.frame(contrast='Cloze AUC loss minus ASSISTments AUC loss',
    estimate=unname(point[['cloze']]-point[['assistments']]),
    lower95=unname(quantile(draws[,'cloze']-draws[,'assistments'],.025)),
    upper95=unname(quantile(draws[,'cloze']-draws[,'assistments'],.975)))
  final <- as.data.frame(data.table::rbindlist(summary));allseeds <- data.table::rbindlist(seeds);allfits <- data.table::rbindlist(records)
  if(action=='report') {
    study_write_csv(final,file.path(out,'dataset-condition-metrics.csv'))
    study_write_csv(as.data.frame(allseeds),file.path(out,'pooled-seed-metrics.csv'))
    study_write_csv(as.data.frame(allfits),file.path(out,'fits.csv'))
    study_write_csv(diffs,file.path(out,'feature-benefit-by-dataset.csv'))
    study_write_csv(interaction,file.path(out,'cross-dataset-interaction.csv'))
    study_write_csv(do.call(rbind,controls),file.path(out,'cloze-control-reproduction.csv'))
    lines <- c('# LKT and time feature ablation: Cloze versus ASSISTments 2017','',
      '60 fresh fits, five whole-participant outer folds and seeds 1-3 per dataset/condition.',
      'Only engineered LKT and elapsed-time input groups are removed. Chronological ordering remains.',
      'AUC pools outer test predictions within seed, then averages the three AUCs.','',
      '| Dataset | Full AUC | No LKT/time AUC | AUC loss on removal | 95% participant-bootstrap interval |',
      '|---|---:|---:|---:|---|',
      vapply(names(plans),function(name) {
        f <- final[final$dataset==name,];z <- diffs[diffs$dataset==name,]
        sprintf('| %s | %.6f | %.6f | %+.6f | [%+.6f, %+.6f] |',name,
          f$CV_AUC[f$condition=='baseline'],f$CV_AUC[f$condition=='no_features'],
          z$AUC_loss_on_removal,z$lower95,z$upper95)
      },character(1)),'',sprintf('Cloze-minus-ASSISTments feature-benefit contrast: %+.6f; 95%% interval [%+.6f, %+.6f].',
        interaction$estimate,interaction$lower95,interaction$upper95),'',
      'Positive cross-dataset values support a larger feature benefit on Cloze.',
      'Intervals use 2000 whole-participant draws paired across conditions/seeds within each dataset, independent between datasets, conditional on fitted models.',
      'These are exploratory comparisons with fixed shared settings and inherited nonlinear constants; Cloze already informed architecture choices. Input-dependent parameter counts differ.',
      sprintf('ASSISTments preparation retains %d responses, %d students, %d items and %d skills; it is an explicit procedural approximation, not the unpublished UNVaMP preparation.',
        plans$assistments$provenance$responses,plans$assistments$provenance$students,plans$assistments$provenance$items,plans$assistments$provenance$skills))
    writeLines(lines,file.path(out,'RESULTS.md'))
    writeLines(sprintf('PASS 60 fit contracts, ten saved-weight replay receipts, 15 prior Cloze controls, 12 independent pROC AUCs and 20 literal participant-bootstrap draws (max error %.3g).',maximum_bootstrap_error),file.path(out,'verification.txt'))
  } else {
    stopifnot(isTRUE(all.equal(read.csv(file.path(out,'dataset-condition-metrics.csv')),final,tolerance=1e-12,check.attributes=FALSE)),
      isTRUE(all.equal(read.csv(file.path(out,'cross-dataset-interaction.csv')),interaction,tolerance=1e-12,check.attributes=FALSE)))
  }
  print(final);print(diffs);print(interaction)
}
