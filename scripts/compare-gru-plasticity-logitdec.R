# ROOT OUTPUT ACTION TASK CANDIDATE_JSON CANDIDATE_LIBRARY [REFERENCE]
a<-commandArgs(TRUE);stopifnot(length(a)%in%c(6L,7L))
root<-normalizePath(a[1],winslash='/',mustWork=TRUE);output<-a[2];action<-a[3]
stopifnot(action%in%c('plan','verify','smoke','fit','report','check'))
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
source(file.path(root,'scripts/gru-encoder-study.R'))
candidate_file<-normalizePath(a[5],winslash='/',mustWork=TRUE)
candidate<-study_require_release(candidate_file,a[6])
stopifnot(candidate$validation_candidate,study_hash(file.path(dirname(candidate_file),candidate$source_archive))==candidate$source_sha256)
source(file.path(root,'scripts/gru-plasticity-study.R'))
files<-c('scripts/compare-gru-plasticity-logitdec.R','scripts/gru-plasticity-study.R','scripts/gru-encoder-study.R')
hashes<-setNames(vapply(file.path(root,files),study_hash,character(1)),files)
if(action=='plan') {
  stopifnot(length(a)==7L,!dir.exists(output));reference<-a[7]
  old<-readRDS(file.path(reference,'study.rds'));d<-readRDS(file.path(reference,'data/prepared_data.rds'))
  stopifnot(study_hash(file.path(reference,'data/prepared_data.rds'))==old$data_sha256,
    nrow(d)==55122L,length(unique(d$Anon.Student.Id))==478L,identical(old$seeds,1:3),identical(old$partitions,paste0('cv',1:5)))
  stopifnot(identical(names(old$settings),c('stock','constant','fatigue')),
    identical(candidate,old$candidate),identical(old$baseline$features,c('logitdec','ppes','logitdec')))
  settings<-old$settings
  for(n in names(settings))stopifnot(identical(settings[[n]],do.call(GRUModel,settings[[n]])$settings))
  recipe<-old$baseline
  remove<-recipe$features=='logitdec' & recipe$components=='Anon.Student.Id'
  stopifnot(sum(remove)==1L)
  counts<-getFromNamespace('lkt_feature_param_counts','LKT')(recipe$features)
  keep<-!remove
  for(n in c('components','features','interacts'))recipe[[n]]<-recipe[[n]][keep]
  removed_parameter<-recipe$fixedpars[rep(remove,counts)]
  recipe$fixedpars<-recipe$fixedpars[rep(keep,counts)]
  recipe$connectors<-rep('+',sum(keep)-1L)
  stopifnot(length(removed_parameter)==1L,identical(recipe$features,old$baseline$features[keep]),
    identical(recipe$components,old$baseline$components[keep]),
    all(recipe$components=='KC..Default.'),sum(recipe$features=='ppes')==1L,sum(recipe$features=='logitdec')==1L)
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
    settings=settings,baseline=recipe,full_baseline=old$baseline,
    removed_feature='lkt:logitdecAnon.Student.Id',removed_fixed_parameter=removed_parameter,
    full_report_hashes=setNames(vapply(file.path(reference,'reports',c('fits.csv','pooled-by-seed.csv')),study_hash,character(1)),c('fits.csv','pooled-by-seed.csv')),
    parameter_counts=c(stock=43041L,constant=43042L,fatigue=43044L),
    design='45 new fits removing only student-level logitdec; stock/constant/fatigue, exact preceding folds, seeds and controls. Compare against the preceding 45 full-input fits. PPES, age, prior spacing and student-item logitdec remain.',
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
  for(part in p$partitions) {
    d<-data_for(part);s<-p$settings$stock
    full<-f('lkt_gru_preprocess')(d,d$fold==1,p$full_baseline,s)
    prep<-f('lkt_gru_preprocess')(d,d$fold==1,p$baseline,s)
    stopifnot(identical(prep$numeric_names,setdiff(full$numeric_names,p$removed_feature)),length(prep$numeric_names)==5L)
    common<-match(prep$numeric_names,full$numeric_names)
    for(n in c('center','scale'))stopifnot(identical(unname(prep[[n]]),unname(full[[n]][common])))
    encoded<-f('lkt_gru_encode')(d,prep,p$baseline,p$settings$fatigue)
    full_encoded<-f('lkt_gru_encode')(d,full,p$full_baseline,s)
    stopifnot(isTRUE(all.equal(as.matrix(encoded$numeric),as.matrix(full_encoded$numeric[,common,drop=FALSE]),check.attributes=FALSE,tolerance=0)))
    groups<-head(split(seq_len(nrow(d)),f('lkt_gru_keys')(d,s$history_group_cols)),4)
    positions<-lapply(groups,function(x)seq_len(min(length(x),100L)))
    for(seed in p$seeds) {
      weights<-preds<-list()
      for(condition in names(p$settings)) {
        ss<-p$settings[[condition]];ss$seed<-seed;cleanup<-f('lkt_gru_runtime')(ss)
        net<-f('lkt_gru_network')(prep,ss);net$eval()
        n<-sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
        stopifnot(n==p$parameter_counts[[condition]])
        b<-f('lkt_gru_batch')(encoded,d,groups,positions,ss)
        q<-torch::with_no_grad({f('lkt_gru_forward')(net,b$numeric,b$categorical,b$answers,NULL,NULL,b$retrieval_ids,ss,b$decay_gaps,!is.na(b$rows))})
        weights[[condition]]<-f('lkt_gru_weights')(net);preds[[condition]]<-torch::as_array(q$logits)[!is.na(b$rows)]
        cleanup()
      }
      for(condition in c('constant','fatigue')) {
        stopifnot(identical(weights$stock,weights[[condition]][names(weights$stock)]),identical(preds$stock,preds[[condition]]))
        records[[length(records)+1L]]<-data.frame(partition=part,seed=seed,condition=condition,numeric_inputs=5L,
          removed_feature=p$removed_feature,maximum_logit_difference=max(abs(preds$stock-preds[[condition]])))
      }
    }
  }
  study_write_csv(do.call(rbind,records),file.path(output,'reports/stock-equivalence.csv'))
  cat('PASS exact feature deletion, unchanged remaining values/scaling and exact zero-rate stock predictions for all folds/seeds.\n')
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
