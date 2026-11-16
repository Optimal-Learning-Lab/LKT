# ROOT OUTPUT ACTION TASK CANDIDATE_JSON CANDIDATE_LIBRARY [REFERENCE]
# Historical constant condition is the baseline_six reference. For a new fit on
# the central release, use the baseline rerun in ../README-GRU.md and gru-variants.R.
a<-commandArgs(TRUE);stopifnot(length(a)%in%c(6L,7L))
root<-normalizePath(a[1],winslash='/',mustWork=TRUE);output<-a[2];action<-a[3]
stopifnot(action%in%c('plan','verify','smoke','fit','report','check'))
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
source(file.path(root,'scripts/gru-encoder-study.R'))
candidate_file<-normalizePath(a[5],winslash='/',mustWork=TRUE)
candidate<-study_require_release(candidate_file,a[6])
stopifnot(candidate$validation_candidate,study_hash(file.path(dirname(candidate_file),candidate$source_archive))==candidate$source_sha256)
source(file.path(root,'scripts/gru-plasticity-study.R'))
files<-c('scripts/compare-gru-plasticity.R','scripts/gru-plasticity-study.R','scripts/gru-encoder-study.R')
hashes<-setNames(vapply(file.path(root,files),study_hash,character(1)),files)
if(action=='plan') {
  stopifnot(length(a)==7L,!dir.exists(output));reference<-a[7]
  old<-readRDS(file.path(reference,'study.rds'));d<-readRDS(file.path(reference,'data/prepared_data.rds'))
  stopifnot(study_hash(file.path(reference,'data/prepared_data.rds'))==old$data_sha256,
    nrow(d)==55122L,length(unique(d$Anon.Student.Id))==478L,identical(old$seeds,1:3),identical(old$partitions,paste0('cv',1:5)))
  base<-do.call(GRUModel,old$settings$omit1)$settings
  base['time_col']<-list(NULL);base['numeric_feature_cols']<-list(NULL)
  base$unknown_levels<-lkt_gru_defaults()$unknown_levels;base<-do.call(GRUModel,base)$settings
  fixed<-setdiff(names(base),c('seed','validation_folds'))
  stopifnot(identical(base[fixed],GRUModel()$settings[fixed]),base$validation_folds==2,
    !base$hidden_decay,identical(old$baseline$interc,FALSE),identical(old$baseline$features,c('logitdec','ppes','logitdec')))
  constant<-base;constant$plasticity<-'constant';constant$plasticity_time_col<-'CF..Time.'
  fatigue<-constant;fatigue$plasticity<-'fatigue'
  settings<-list(stock=base,constant=do.call(GRUModel,constant)$settings,fatigue=do.call(GRUModel,fatigue)$settings)
  matched<-setdiff(names(base),c('plasticity','plasticity_time_col'))
  stopifnot(identical(base[matched],settings$constant[matched]),identical(base[matched],settings$fatigue[matched]))
  dir.create(output,recursive=TRUE)
  for(n in c('data','partitions','models','training','reports'))dir.create(file.path(output,n))
  stopifnot(file.copy(file.path(reference,'data/prepared_data.rds'),file.path(output,'data/prepared_data.rds')))
  for(part in old$partitions) {
    path<-file.path(reference,'partitions',paste0(part,'.rds'))
    stopifnot(study_hash(path)==old$partition_hashes[[part]],file.copy(path,file.path(output,'partitions',basename(path))))
    roles<-readRDS(path);identity<-unique(data.frame(student=d$Anon.Student.Id,role=roles))
    stopifnot(length(roles)==nrow(d),!anyDuplicated(identity$student),all(roles%in%1:3))
  }
  tasks<-expand.grid(seed=1:3,candidate=names(settings),partition=old$partitions,stringsAsFactors=FALSE)
  tasks$seed<-as.integer(tasks$seed);tasks$run_id<-with(tasks,paste(partition,candidate,paste0('seed',seed),sep='__'))
  tasks<-tasks[,c('run_id','partition','candidate','seed')];rownames(tasks)<-NULL
  p<-list(schema=1L,candidate=candidate,candidate_file_sha256=study_hash(candidate_file),script_hashes=hashes,
    data_sha256=old$data_sha256,reference_protocol_sha256=study_hash(file.path(reference,'study.rds')),
    partition_hashes=old$partition_hashes,partitions=old$partitions,seeds=1:3,total_fits=45L,tasks=tasks,
    settings=settings,baseline=old$baseline,parameter_counts=c(stock=43073L,constant=43074L,fatigue=43076L),
    design='45 fresh fits: stock, constant-rate adaptive output connections, fatigue/recovery adaptive output connections; unchanged PPES, participant folds, seeds and training controls.',
    initialization='eta=0; fatigue kappa=1, rho=log(2) per minute. Both extensions reproduce stock. No parameter tuning on held-out data.',
    checkpoint_selection='Minimum inner-validation log loss; 100 epochs, patience 10.',active_release_promoted=FALSE)
  study_write_rds(p,file.path(output,'study.rds'));study_write_csv(tasks,file.path(output,'tasks.csv'))
  writeLines(capture.output(dput(p)),file.path(output,'study.txt'))
  cat('FROZEN 45 matched fits.\n');quit(save='no')
}
p<-readRDS(file.path(output,'study.rds'))
stopifnot(identical(p$candidate,candidate),study_hash(candidate_file)==p$candidate_file_sha256,
  identical(p$script_hashes,hashes),study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256)
for(part in p$partitions)stopifnot(study_hash(file.path(output,'partitions',paste0(part,'.rds')))==p$partition_hashes[[part]])
data_for<-function(part) {d<-readRDS(file.path(output,'data/prepared_data.rds'));d$fold<-readRDS(file.path(output,'partitions',paste0(part,'.rds')));d}
if(action=='verify') {
  f<-function(n)getFromNamespace(n,'LKT');records<-list()
  for(part in p$partitions)for(seed in p$seeds) {
    d<-data_for(part);preps<-weights<-preds<-list()
    for(condition in names(p$settings)) {
      s<-p$settings[[condition]];s$seed<-seed;cleanup<-f('lkt_gru_runtime')(s)
      prep<-f('lkt_gru_preprocess')(d,d$fold==1,p$baseline,s)
      encoded<-f('lkt_gru_encode')(d,prep,p$baseline,s);net<-f('lkt_gru_network')(prep,s);net$eval()
      n<-sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
      stopifnot(n==p$parameter_counts[[condition]],length(prep$numeric_names)==6L)
      groups<-head(split(seq_len(nrow(d)),f('lkt_gru_keys')(d,s$history_group_cols)),4)
      positions<-lapply(groups,function(x)seq_len(min(length(x),100L)))
      b<-f('lkt_gru_batch')(encoded,d,groups,positions,s)
      q<-torch::with_no_grad({f('lkt_gru_forward')(net,b$numeric,b$categorical,b$answers,NULL,NULL,b$retrieval_ids,s,b$decay_gaps,!is.na(b$rows))})
      preps[[condition]]<-prep;weights[[condition]]<-f('lkt_gru_weights')(net);preds[[condition]]<-torch::as_array(q$logits)[!is.na(b$rows)]
      if(condition!='stock')stopifnot(weights[[condition]][['plasticity.eta']]==0,all(is.finite(encoded$decay_gaps)),all(encoded$decay_gaps>=0))
      cleanup()
    }
    for(condition in c('constant','fatigue')) {
      stopifnot(identical(preps$stock,preps[[condition]]),identical(weights$stock,weights[[condition]][names(weights$stock)]),identical(preds$stock,preds[[condition]]))
      records[[length(records)+1L]]<-data.frame(partition=part,seed=seed,condition=condition,maximum_logit_difference=max(abs(preds$stock-preds[[condition]])))
    }
  }
  study_write_csv(do.call(rbind,records),file.path(output,'reports/stock-equivalence.csv'))
  cat('PASS all folds and seeds: identical shared initial weights, schemas and EXACT stock logits.\n')
}
if(action=='smoke') {
  raw<-data_for('cv1');idx<-head(which(raw$Anon.Student.Id==raw$Anon.Student.Id[1]),12)
  d<-do.call(rbind,lapply(1:8,function(i) {x<-raw[idx,,drop=FALSE];x$Anon.Student.Id<-paste0('smoke',i)
    x$CF..ansbin.<-rep(c(0,1,1,0,1,0),2);x$fold<-c(1,1,1,1,1,2,3,3)[i];x}))
  smoke<-file.path(dirname(output),'smoke');stopifnot(!dir.exists(smoke))
  for(n in c('models','training','reports'))dir.create(file.path(smoke,n),recursive=TRUE)
  pp<-p
  for(condition in names(p$settings)) {
    prep<-getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1,p$baseline,p$settings[[condition]])
    net<-getFromNamespace('lkt_gru_network','LKT')(prep,p$settings[[condition]])
    pp$parameter_counts[[condition]]<-sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
  }
  study_write_rds(pp,file.path(smoke,'study.rds'))
  for(condition in names(p$settings)) {
    s<-p$settings[[condition]];s$epochs<-1L;s$early_stopping<-FALSE;s$restore_best<-FALSE
    task<-data.frame(run_id=paste0('smoke__',condition,'__seed1'),partition='smoke',candidate=condition,seed=1L)
    plastic_study_fit(task,s,smoke,pp,d)
  }
  cat('PASS three one-epoch smoke fits and saved-weight replay.\n')
}
if(action=='fit') {
  id<-as.integer(a[4]);stopifnot(length(id)==1L,!is.na(id),id>=0L,id<45L)
  task<-p$tasks[id+1L,,drop=FALSE];plastic_study_fit(task,p$settings[[task$candidate]],output,p,data_for(task$partition))
}
if(action%in%c('report','check'))plastic_study_report(output,p,action)
