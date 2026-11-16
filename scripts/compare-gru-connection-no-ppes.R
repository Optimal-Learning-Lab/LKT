# ROOT OUTPUT ACTION TASK CANDIDATE_JSON LIBRARY FULL_GATE_REFERENCE
args<-commandArgs(TRUE);stopifnot(length(args)==7L)
root<-args[1];out<-args[2];action<-args[3]
source(file.path(root,'scripts/gru-encoder-study.R'))
candidate<-study_require_release(args[5],args[6])
source(file.path(root,'scripts/gru-plasticity-study.R'))
source(file.path(root,'scripts/gru-connection-fatigue-study.R'))
f<-function(n)getFromNamespace(n,'LKT')
files<-c('scripts/compare-gru-connection-no-ppes.R','scripts/gru-encoder-study.R','scripts/gru-plasticity-study.R','scripts/gru-connection-fatigue-study.R')
hashes<-setNames(vapply(file.path(root,files),study_hash,character(1)),files)
if(action=='plan') {
 stopifnot(!dir.exists(out));full<-readRDS(file.path(args[7],'study.rds'))
 s<-do.call(GRUModel,full$settings$constant)$settings;s$gate_fatigue<-TRUE;s$gate_fatigue_time_col<-'CF..Time.';s$gate_fatigue_mode<-'connection'
 recipe<-full$baseline;keep<-recipe$features=='logitdec' & recipe$components!='Anon.Student.Id'
 stopifnot(sum(!keep)==2L)
 counts<-f('lkt_feature_param_counts')(recipe$features)
 for(n in c('components','features','interacts'))recipe[[n]]<-recipe[[n]][keep]
 recipe$fixedpars<-recipe$fixedpars[rep(keep,counts)];recipe$connectors<-rep('+',sum(keep)-1L)
 stopifnot(s$plasticity=='constant',identical(recipe$features,'logitdec'),identical(recipe$components,'KC..Default.'))
 dir.create(out,recursive=TRUE)
 for(n in c('data','partitions','models','training','reports'))dir.create(file.path(out,n))
 for(n in c('data/prepared_data.rds',paste0('partitions/',full$partitions,'.rds')))
   stopifnot(file.copy(file.path(args[7],n),file.path(out,n)))
 tasks<-expand.grid(seed=1:3,candidate='connection_no_ppes',partition=full$partitions,stringsAsFactors=FALSE)
 tasks$seed<-as.integer(tasks$seed);tasks$run_id<-with(tasks,paste(partition,candidate,paste0('seed',seed),sep='__'))
 tasks<-tasks[,c('run_id','partition','candidate','seed')]
 p<-list(candidate=candidate,candidate_file_sha256=study_hash(args[5]),script_hashes=hashes,baseline=recipe,
   full_baseline=full$baseline,settings=list(connection_no_ppes=s),tasks=tasks,total_fits=15L,
   data_sha256=full$data_sha256,partition_hashes=full$partition_hashes,parameter_counts=c(connection_no_ppes=NA_integer_),
   reference_hashes=c(full=study_hash(file.path(args[7],'study.rds'))),
   design='15 connection-fatigue plus constant-output fits; student logitdec and PPES removed (four numeric inputs). Same folds/seeds and training controls. No new control fits.',
   active_release_promoted=FALSE)
 d<-readRDS(file.path(out,'data/prepared_data.rds'));d$fold<-readRDS(file.path(out,'partitions/cv1.rds'))
 prep<-f('lkt_gru_preprocess')(d,d$fold==1,recipe,s);net<-f('lkt_gru_network')(prep,s)
 stopifnot(length(prep$numeric_names)==4L)
 p$numeric_names<-prep$numeric_names;p$parameter_counts[[1]]<-sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
 study_write_rds(p,file.path(out,'study.rds'));study_write_csv(tasks,file.path(out,'tasks.csv'))
 cat('FROZEN 15 fits; student logitdec and PPES removed; combined mechanisms; four numeric inputs.\n')
 quit(save='no')
}
p<-readRDS(file.path(out,'study.rds'))
stopifnot(identical(hashes,p$script_hashes),identical(candidate,p$candidate),study_hash(args[5])==p$candidate_file_sha256,
 study_hash(file.path(out,'data/prepared_data.rds'))==p$data_sha256,
 study_hash(file.path(args[7],'study.rds'))==p$reference_hashes[['full']])
data_for<-function(part){d<-readRDS(file.path(out,'data/prepared_data.rds'));path<-file.path(out,'partitions',paste0(part,'.rds'));stopifnot(study_hash(path)==p$partition_hashes[[part]]);d$fold<-readRDS(path);d}
if(action=='smoke') {
 d<-data_for('cv1');s<-p$settings$connection_no_ppes
 full<-f('lkt_gru_preprocess')(d,d$fold==1,p$full_baseline,s);prep<-f('lkt_gru_preprocess')(d,d$fold==1,p$baseline,s)
 common<-match(prep$numeric_names,full$numeric_names)
 stopifnot(length(prep$numeric_names)==4L,length(full$numeric_names)==6L,!anyNA(common),
   identical(unname(prep$center),unname(full$center[common])),identical(unname(prep$scale),unname(full$scale[common])))
 enc<-f('lkt_gru_encode')(d,prep,p$baseline,s);all<-f('lkt_gru_encode')(d,full,p$full_baseline,s)
 stopifnot(isTRUE(all.equal(as.matrix(enc$numeric),as.matrix(all$numeric[,common,drop=FALSE]),tolerance=0,check.attributes=FALSE)))
 idx<-head(which(d$Anon.Student.Id==d$Anon.Student.Id[1]),12)
 d<-do.call(rbind,lapply(1:8,function(i){x<-d[idx,,drop=FALSE];x$Anon.Student.Id<-paste0('smoke',i);x$CF..ansbin.<-rep(c(0,1,1,0,1,0),2);x$fold<-c(1,1,1,1,1,2,3,3)[i];x}))
 smoke<-file.path(dirname(out),'smoke');stopifnot(!dir.exists(smoke))
 for(n in c('models','training','reports'))dir.create(file.path(smoke,n),recursive=TRUE)
 pp<-p;prep<-f('lkt_gru_preprocess')(d,d$fold==1,p$baseline,s);net<-f('lkt_gru_network')(prep,s)
 pp$parameter_counts[[1]]<-sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
 study_write_rds(pp,file.path(smoke,'study.rds'));s$epochs<-1L;s$early_stopping<-FALSE;s$restore_best<-FALSE
 task<-data.frame(run_id='smoke__connection_no_ppes__seed1',partition='smoke',candidate='connection_no_ppes',seed=1L)
 connection_study_fit(task,s,smoke,pp,d)
 cat('PASS matching inputs/scaling and one short smoke fit.\n')
}
if(action=='fit') {
 id<-as.integer(args[4]);stopifnot(id>=0L,id<15L)
 task<-p$tasks[id+1L,,drop=FALSE];connection_study_fit(task,p$settings$connection_no_ppes,out,p,data_for(task$partition))
}
if(action=='report')connection_study_report(out,p,data_for)
