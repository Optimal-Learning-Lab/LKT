# ROOT OUTPUT ACTION TASK CANDIDATE_JSON LIBRARY FULL_GATE_REFERENCE NO_PPES_REFERENCE
args<-commandArgs(TRUE);stopifnot(length(args)==8L)
root<-args[1];out<-args[2];action<-args[3]
source(file.path(root,'scripts/gru-encoder-study.R'))
candidate<-study_require_release(args[5],args[6])
source(file.path(root,'scripts/gru-plasticity-study.R'))
source(file.path(root,'scripts/gru-gate-fatigue-study.R'))
f<-function(n)getFromNamespace(n,'LKT')
files<-c('scripts/compare-gru-gate-no-ppes.R','scripts/gru-encoder-study.R','scripts/gru-plasticity-study.R','scripts/gru-gate-fatigue-study.R')
hashes<-setNames(vapply(file.path(root,files),study_hash,character(1)),files)
if(action=='plan') {
 stopifnot(!dir.exists(out));full<-readRDS(file.path(args[7],'study.rds'));old<-readRDS(file.path(args[8],'study.rds'))
 s<-full$settings$gate;recipe<-full$baseline;keep<-recipe$features!='ppes'
 stopifnot(sum(!keep)==1L)
 counts<-f('lkt_feature_param_counts')(recipe$features)
 for(n in c('components','features','interacts'))recipe[[n]]<-recipe[[n]][keep]
 recipe$fixedpars<-recipe$fixedpars[rep(keep,counts)];recipe$connectors<-rep('+',sum(keep)-1L)
 base<-s;base$gate_fatigue<-FALSE;base$gate_fatigue_time_col<-NULL
 stopifnot(identical(base,do.call(GRUModel,old$settings$stock_no_ppes)$settings),
   identical(recipe,old$recipes$stock_no_ppes),identical(old$data_sha256,full$data_sha256),
   identical(old$partition_hashes,full$partition_hashes),s$plasticity=='none')
 dir.create(out,recursive=TRUE)
 for(n in c('data','partitions','models','training','reports'))dir.create(file.path(out,n))
 for(n in c('data/prepared_data.rds',paste0('partitions/',full$partitions,'.rds')))
   stopifnot(file.copy(file.path(args[7],n),file.path(out,n)))
 tasks<-expand.grid(seed=1:3,candidate='gate_no_ppes',partition=full$partitions,stringsAsFactors=FALSE)
 tasks$seed<-as.integer(tasks$seed);tasks$run_id<-with(tasks,paste(partition,candidate,paste0('seed',seed),sep='__'))
 tasks<-tasks[,c('run_id','partition','candidate','seed')]
 p<-list(candidate=candidate,candidate_file_sha256=study_hash(args[5]),script_hashes=hashes,baseline=recipe,
   full_baseline=full$baseline,settings=list(gate_no_ppes=s),tasks=tasks,total_fits=15L,
   data_sha256=full$data_sha256,partition_hashes=full$partition_hashes,parameter_counts=c(gate_no_ppes=43043L),
   reference_hashes=c(full=study_hash(file.path(args[7],'study.rds')),no_ppes=study_hash(file.path(args[8],'study.rds'))),
   design='15 gate-fatigue fits with PPES removed; reuse completed matching stock-no-PPES controls. Output adaptation off.',
   active_release_promoted=FALSE)
 study_write_rds(p,file.path(out,'study.rds'));study_write_csv(tasks,file.path(out,'tasks.csv'))
 cat('FROZEN 15 fits; exact previous no-PPES recipe/settings/folds verified.\n')
 quit(save='no')
}
p<-readRDS(file.path(out,'study.rds'))
stopifnot(identical(hashes,p$script_hashes),identical(candidate,p$candidate),study_hash(args[5])==p$candidate_file_sha256,
 study_hash(file.path(out,'data/prepared_data.rds'))==p$data_sha256,
 study_hash(file.path(args[7],'study.rds'))==p$reference_hashes[['full']],study_hash(file.path(args[8],'study.rds'))==p$reference_hashes[['no_ppes']])
data_for<-function(part){d<-readRDS(file.path(out,'data/prepared_data.rds'));path<-file.path(out,'partitions',paste0(part,'.rds'));stopifnot(study_hash(path)==p$partition_hashes[[part]]);d$fold<-readRDS(path);d}
if(action=='smoke') {
 d<-data_for('cv1');s<-p$settings$gate_no_ppes
 full<-f('lkt_gru_preprocess')(d,d$fold==1,p$full_baseline,s);prep<-f('lkt_gru_preprocess')(d,d$fold==1,p$baseline,s)
 common<-match(prep$numeric_names,full$numeric_names)
 stopifnot(length(prep$numeric_names)==5L,length(full$numeric_names)==6L,!anyNA(common),
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
 task<-data.frame(run_id='smoke__gate_no_ppes__seed1',partition='smoke',candidate='gate_no_ppes',seed=1L)
 gate_study_fit(task,s,smoke,pp,d)
 cat('PASS matching inputs/scaling and one short smoke fit.\n')
}
if(action=='fit') {
 id<-as.integer(args[4]);stopifnot(id>=0L,id<15L)
 task<-p$tasks[id+1L,,drop=FALSE];gate_study_fit(task,p$settings$gate_no_ppes,out,p,data_for(task$partition))
}
if(action=='report') {
 records<-predictions<-list()
 for(i in seq_len(nrow(p$tasks))) {
   task<-p$tasks[i,,drop=FALSE];saved<-readRDS(file.path(out,'models',paste0(task$run_id,'.rds')));fit<-saved$fit
   d<-data_for(task$partition);roles<-d$fold
   stopifnot(identical(fit$fit_rows,which(roles==1L)),identical(fit$evaluation_rows,which(roles==3L)),fit$parameter_count==43043L)
   metrics<-unlist(lapply(1:3,function(k)study_metrics(d$CF..ansbin.[roles==k],fit$prediction[roles==k])[c('AUC','log_loss')]),use.names=FALSE)
   names(metrics)<-c('training_AUC','training_log_loss','validation_AUC','validation_log_loss','test_AUC','test_log_loss')
   records[[i]]<-cbind(task,as.data.frame(gate_study_parameters(fit)),as.data.frame(as.list(metrics)),selected_epoch=fit$selected_epoch)
   predictions[[i]]<-data.frame(seed=task$seed,source_row=d$comparison_source_row[roles==3],observed=d$CF..ansbin.[roles==3],prediction=fit$prediction[roles==3])
 }
 pred<-data.table::as.data.table(do.call(rbind,predictions));stopifnot(!anyDuplicated(pred[,.(seed,source_row)]),all(pred[,.N,by=seed]$N==55122L))
 pooled<-pred[,as.list(study_metrics(observed,prediction)[c('AUC','log_loss')]),by=seed]
 control<-read.csv(file.path(args[8],'reports/pooled-by-seed.csv'));control<-control[control$condition=='stock_no_ppes',]
 comparison<-merge(pooled,control,by='seed',suffixes=c('_gate','_stock'))
 study_write_csv(do.call(rbind,records),file.path(out,'reports/fits.csv'));study_write_csv(pooled,file.path(out,'reports/pooled-by-seed.csv'));study_write_csv(comparison,file.path(out,'reports/comparison.csv'))
 jsonlite::write_json(list(complete=TRUE,new_fits=15L,reused_stock_no_ppes=15L),file.path(out,'reports/verification.json'),auto_unbox=TRUE,pretty=TRUE)
 print(pooled);print(comparison)
}
