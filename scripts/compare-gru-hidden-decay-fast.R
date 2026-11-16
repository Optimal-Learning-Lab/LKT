# ROOT OUTPUT ACTION TASK CANDIDATE_JSON CANDIDATE_LIBRARY [REFERENCE]
a <- commandArgs(TRUE);stopifnot(length(a)%in%c(6L,7L))
root <- normalizePath(a[1L],winslash='/',mustWork=TRUE);output <- a[2L];action <- a[3L]
stopifnot(action%in%c('plan','verify','smoke','fit','report','check'))
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
source(file.path(root,'scripts/gru-encoder-study.R'))
candidate_file <- normalizePath(a[5L],winslash='/',mustWork=TRUE)
candidate <- study_require_release(candidate_file,a[6L])
stopifnot(candidate$validation_candidate,study_hash(file.path(dirname(candidate_file),candidate$source_archive))==candidate$source_sha256,
  all(c('hidden_decay','hidden_decay_time_col','hidden_decay_parameterization')%in%names(lkt_gru_defaults())))
source(file.path(root,'scripts/gru-hidden-decay-fast-study.R'))
files <- c('scripts/compare-gru-hidden-decay-fast.R','scripts/gru-hidden-decay-fast-study.R','scripts/gru-encoder-study.R')
hashes <- setNames(vapply(file.path(root,files),study_hash,character(1)),files)
if(action=='plan') {
  stopifnot(length(a)==7L,!dir.exists(output));reference <- a[7L]
  old <- readRDS(file.path(reference,'study.rds'));d <- readRDS(file.path(reference,'data/prepared_data.rds'))
  stopifnot(study_hash(file.path(reference,'data/prepared_data.rds'))==old$data_sha256,
    nrow(d)==55122L,length(unique(d$Anon.Student.Id))==478L,identical(old$seeds,1:3),
    identical(old$partitions,paste0('cv',1:5)))
  base <- do.call(GRUModel,old$settings$omit1)$settings
  base['time_col'] <- list(NULL);base['numeric_feature_cols'] <- list(NULL)
  base$unknown_levels <- lkt_gru_defaults()$unknown_levels
  base <- do.call(GRUModel,base)$settings
  fixed <- setdiff(names(base),c('seed','validation_folds'))
  stopifnot(identical(base[fixed],GRUModel()$settings[fixed]),base$validation_folds==2,
    identical(old$baseline$interc,FALSE))
  decay <- base;decay$hidden_decay <- TRUE;decay$hidden_decay_time_col <- 'CF..Time.'
  decay <- do.call(GRUModel,decay)$settings
  matched <- setdiff(names(base),c('hidden_decay','hidden_decay_time_col','hidden_decay_parameterization'))
  stopifnot(identical(base[matched],decay[matched]))
  stopifnot(decay$hidden_decay_parameterization=='amplitude')
  recipe <- old$baseline
  stopifnot(identical(recipe$features,c('logitdec','ppes','logitdec')))
  counts <- getFromNamespace('lkt_feature_param_counts','LKT')(recipe$features)
  keep <- recipe$features!='ppes'
  no_ppes <- recipe
  for(n in c('components','features','interacts'))no_ppes[[n]] <- recipe[[n]][keep]
  no_ppes$fixedpars <- recipe$fixedpars[rep(keep,counts)]
  no_ppes$connectors <- rep('+',sum(keep)-1L)
  stopifnot(sum(counts[!keep])==4L,length(no_ppes$fixedpars)==2L)
  settings <- list(stock_full=base,decay_full=decay,stock_no_ppes=base,decay_no_ppes=decay)
  recipes <- list(stock_full=recipe,decay_full=recipe,stock_no_ppes=no_ppes,decay_no_ppes=no_ppes)
  dir.create(output,recursive=TRUE)
  for(n in c('data','partitions','models','validation','training','reports'))dir.create(file.path(output,n))
  stopifnot(file.copy(file.path(reference,'data/prepared_data.rds'),file.path(output,'data/prepared_data.rds')))
  for(part in old$partitions) {
    path <- file.path(reference,'partitions',paste0(part,'.rds'))
    stopifnot(study_hash(path)==old$partition_hashes[[part]],file.copy(path,file.path(output,'partitions',basename(path))))
    roles <- readRDS(path);identity <- unique(data.frame(student=d$Anon.Student.Id,role=roles))
    stopifnot(length(roles)==nrow(d),!anyDuplicated(identity$student),all(roles%in%1:3))
  }
  tasks <- expand.grid(seed=1:3,candidate=names(settings),partition=old$partitions,stringsAsFactors=FALSE)
  tasks$seed <- as.integer(tasks$seed)
  tasks$run_id <- with(tasks,paste(partition,candidate,paste0('seed',seed),sep='__'))
  tasks <- tasks[,c('run_id','partition','candidate','seed')];rownames(tasks) <- NULL
  p <- list(schema=1L,candidate=candidate,candidate_file_sha256=study_hash(candidate_file),script_hashes=hashes,
    data_sha256=old$data_sha256,reference_protocol_sha256=study_hash(file.path(reference,'study.rds')),
    partition_hashes=old$partition_hashes,partitions=old$partitions,seeds=1:3,total_fits=60L,tasks=tasks,
    settings=settings,recipes=recipes,baseline=old$baseline,
    parameter_counts=c(stock_full=43073L,decay_full=43076L,stock_no_ppes=43041L,decay_no_ppes=43044L),
    design='Sixty fresh fits. Stock/compiled hidden decay crossed with PPES present/absent; exact existing whole-participant folds, seeds and training budget.',
    checkpoint_selection='Minimum inner-validation log loss. No held-out selection.',
    half_lives='Baseline hidden half-life at DK=0 and DK fading half-life, both in days. b has no standalone half-life.',
    active_release_promoted=FALSE)
  study_write_rds(p,file.path(output,'study.rds'));study_write_csv(tasks,file.path(output,'tasks.csv'))
  writeLines(capture.output(dput(p)),file.path(output,'study.txt'))
  cat('FROZEN 60 fresh fits; factorial stock/decay and PPES present/absent, exact participant folds and seeds.\n');quit(save='no')
}
p <- readRDS(file.path(output,'study.rds'))
stopifnot(identical(p$candidate,candidate),study_hash(candidate_file)==p$candidate_file_sha256,
  identical(p$script_hashes,hashes),study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256)
for(part in p$partitions)stopifnot(study_hash(file.path(output,'partitions',paste0(part,'.rds')))==p$partition_hashes[[part]])
data_for <- function(part) {
  d <- readRDS(file.path(output,'data/prepared_data.rds'))
  d$fold <- readRDS(file.path(output,'partitions',paste0(part,'.rds')));d
}
if(action=='verify') {
  records <- list()
  for(part in p$partitions) {
    d <- data_for(part);preps <- weights <- encodings <- list()
    for(condition in names(p$settings)) {
      s <- p$settings[[condition]];cleanup <- getFromNamespace('lkt_gru_runtime','LKT')(s)
      prep <- getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1,p$recipes[[condition]],s)
      encoded <- getFromNamespace('lkt_gru_encode','LKT')(d,prep,p$recipes[[condition]],s)
      net <- getFromNamespace('lkt_gru_network','LKT')(prep,s)
      n <- sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
      stopifnot(n==p$parameter_counts[[condition]],length(prep$numeric_names)==if(grepl('_full$',condition))6L else 5L,
        !'time:log1p_gap_seconds'%in%prep$numeric_names)
      preps[[condition]] <- prep;weights[[condition]] <- getFromNamespace('lkt_gru_weights','LKT')(net)
      encodings[[condition]] <- encoded
      if(s$hidden_decay)stopifnot(all(is.finite(encoded$decay_gaps)),all(encoded$decay_gaps>=0),
        all(unlist(weights[[condition]][paste0('hidden_decay.',c('a','b','c'))])==0))
      cleanup()
      records[[paste(part,condition)]] <- data.frame(partition=part,condition=condition,parameters=n,numeric_inputs=length(prep$numeric_names))
    }
    for(group in c('full','no_ppes')) {
      stock <- paste0('stock_',group);decay <- paste0('decay_',group)
      stopifnot(identical(preps[[stock]],preps[[decay]]),
        identical(weights[[stock]],weights[[decay]][names(weights[[stock]])]))
      # Real learner histories, shared initial weights, zero amplitude.
      s <- p$settings[[decay]];cleanup <- getFromNamespace('lkt_gru_runtime','LKT')(s)
      groups <- split(seq_len(nrow(d)),getFromNamespace('lkt_gru_keys','LKT')(d,s$history_group_cols))
      groups <- head(groups,4L);positions <- lapply(groups,function(x)seq_len(min(length(x),100L)))
      q <- list()
      for(n in c(stock,decay)) {
        sn <- p$settings[[n]];net <- getFromNamespace('lkt_gru_network','LKT')(preps[[n]],sn)
        getFromNamespace('lkt_gru_load_weights','LKT')(net,weights[[n]],sn);net$eval()
        batch <- getFromNamespace('lkt_gru_batch','LKT')(encodings[[n]],d,groups,positions,sn)
        result <- torch::with_no_grad({getFromNamespace('lkt_gru_forward','LKT')(net,batch$numeric,batch$categorical,
          batch$answers,NULL,NULL,batch$retrieval_ids,sn,batch$decay_gaps,!is.na(batch$rows))})
        q[[n]] <- torch::as_array(result$logits)[!is.na(batch$rows)]
      }
      stopifnot(max(abs(q[[stock]]-q[[decay]]))<3e-6)
      cleanup()
    }
    common <- intersect(preps$stock_full$numeric_names,preps$stock_no_ppes$numeric_names)
    stopifnot(length(common)==5L)
    for(field in c('center','scale'))stopifnot(identical(
      unname(preps$stock_full[[field]][match(common,preps$stock_full$numeric_names)]),
      unname(preps$stock_no_ppes[[field]][match(common,preps$stock_no_ppes$numeric_names)])))
  }
  study_write_csv(do.call(rbind,records),file.path(output,'reports/preflight.csv'))
  cat('PASS all twenty fold/condition schemas, exact matched initial network weights, three zero-initialized extra scalars and decay clocks.\n')
}
if(action=='smoke') {
  raw <- data_for('cv1');idx <- head(which(raw$Anon.Student.Id==raw$Anon.Student.Id[1L]),12L)
  d <- do.call(rbind,lapply(1:8,function(i){x<-raw[idx,,drop=FALSE];x$Anon.Student.Id<-paste0('smoke',i)
    x$CF..ansbin.<-rep(c(0,1,1,0,1,0),2L);x$fold<-c(1,1,1,1,1,2,3,3)[i];x}))
  smoke <- file.path(dirname(output),'smoke');stopifnot(!dir.exists(smoke))
  for(n in c('models','validation','training','reports'))dir.create(file.path(smoke,n),recursive=TRUE)
  pp <- p
  for(condition in names(p$settings)) {
    prep <- getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1,p$recipes[[condition]],p$settings[[condition]])
    net <- getFromNamespace('lkt_gru_network','LKT')(prep,p$settings[[condition]])
    pp$parameter_counts[[condition]] <- sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
  }
  study_write_rds(pp,file.path(smoke,'study.rds'))
  for(condition in names(p$settings)) {
    s <- p$settings[[condition]];s$epochs<-1L;s$early_stopping<-FALSE;s$restore_best<-FALSE
    task <- data.frame(run_id=paste0('smoke__',condition,'__seed1'),partition='smoke',candidate=condition,seed=1L)
    fast_study_fit(task,s,smoke,pp,d)
  }
  cat('PASS four bounded one-epoch smoke fits, parameter extraction and saved-weight replay.\n')
}
if(action=='fit') {
  id <- as.integer(a[4L]);stopifnot(length(id)==1L,!is.na(id),id>=0L,id<p$total_fits)
  task <- p$tasks[id+1L,,drop=FALSE]
  fast_study_fit(task,p$settings[[task$candidate]],output,p,data_for(task$partition))
}
if(action%in%c('report','check'))fast_study_report(output,p,action)
