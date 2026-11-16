# ROOT OUTPUT plan|verify|smoke|fit|report|check TASK CANDIDATE_JSON LIBRARY [REFERENCE]
a<-commandArgs(TRUE);stopifnot(length(a)%in%c(6L,7L))
root<-a[1];output<-a[2];action<-a[3]
stopifnot(action%in%c('plan','verify','smoke','fit','report','check'))
source(file.path(root,'scripts/gru-encoder-study.R'))
candidate<-study_require_release(a[5],a[6])
source(file.path(root,'scripts/gru-plasticity-study.R'))
source(file.path(root,'scripts/gru-gate-fatigue-study.R'))
f<-function(n)getFromNamespace(n,'LKT')
files<-c('scripts/compare-gru-gate-fatigue.R','scripts/gru-gate-fatigue-study.R','scripts/gru-plasticity-study.R','scripts/gru-encoder-study.R')
hashes<-setNames(vapply(file.path(root,files),study_hash,character(1)),files)
if(action=='plan') {
 stopifnot(length(a)==7L,!dir.exists(output));reference<-a[7]
 old<-readRDS(file.path(reference,'study.rds'))
 stopifnot(old$total_fits==45L,identical(old$seeds,1:3),identical(old$partitions,paste0('cv',1:5)))
 base<-do.call(GRUModel,old$settings$stock)$settings
 gate<-base;gate$gate_fatigue<-TRUE;gate$gate_fatigue_time_col<-'CF..Time.'
 combined<-gate;combined$plasticity<-'constant';combined$plasticity_time_col<-'CF..Time.'
 settings<-list(gate=do.call(GRUModel,gate)$settings,gate_constant=do.call(GRUModel,combined)$settings)
 matched<-setdiff(names(base),c('gate_fatigue','gate_fatigue_time_col','plasticity','plasticity_time_col'))
 for(s in settings)stopifnot(identical(s[matched],base[matched]))
 dir.create(output,recursive=TRUE)
 for(n in c('data','partitions','models','training','reports','controls'))dir.create(file.path(output,n))
 for(n in c('data/prepared_data.rds',paste0('partitions/',old$partitions,'.rds')))
   stopifnot(file.copy(file.path(reference,n),file.path(output,n)))
 controls<-old$tasks[old$tasks$candidate%in%c('stock','constant'),,drop=FALSE];rownames(controls)<-NULL
 control_files<-c('study.rds',paste0('models/',controls$run_id,'.rds'),paste0('reports/',controls$run_id,'-predictions.csv'))
 control_hashes<-setNames(vapply(file.path(reference,control_files),study_hash,character(1)),control_files)
 # Copy verified historical artifacts only into the results archive, never edit them.
 for(n in control_files) {
   dest<-file.path(output,'controls',n);dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE)
   stopifnot(file.copy(file.path(reference,n),dest))
 }
 tasks<-expand.grid(seed=1:3,candidate=names(settings),partition=old$partitions,stringsAsFactors=FALSE)
 tasks$seed<-as.integer(tasks$seed);tasks$run_id<-with(tasks,paste(partition,candidate,paste0('seed',seed),sep='__'))
 tasks<-tasks[,c('run_id','partition','candidate','seed')]
 p<-list(schema=1L,candidate=candidate,candidate_file_sha256=study_hash(a[5]),script_hashes=hashes,
   data_sha256=old$data_sha256,partition_hashes=old$partition_hashes,partitions=old$partitions,seeds=1:3,
   settings=settings,baseline=old$baseline,tasks=tasks,total_fits=30L,
   parameter_counts=c(gate=43075L,gate_constant=43076L),controls=controls,control_hashes=control_hashes,
   design='30 new fits: gate fatigue alone and gate fatigue plus constant-rate output adaptation; reuse 15 stock and 15 constant-output controls.',
   initialization='Gate kappa=0, rho=log(2) per minute; output eta=0. Activity driver is ordinary GRU write fraction g.',
   checkpoint_selection='Minimum inner-validation log loss; same folds, seeds, six inputs, 100 epochs and patience 10.',active_release_promoted=FALSE)
 study_write_rds(p,file.path(output,'study.rds'));study_write_csv(tasks,file.path(output,'tasks.csv'))
 writeLines(capture.output(dput(p)),file.path(output,'study.txt'));cat('FROZEN 30 new fits; 30 completed controls reused.\n');quit(save='no')
}
p<-readRDS(file.path(output,'study.rds'))
stopifnot(p$total_fits==30L,identical(p$script_hashes,hashes),identical(p$candidate,candidate),study_hash(a[5])==p$candidate_file_sha256,
 study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256)
for(part in p$partitions)stopifnot(study_hash(file.path(output,'partitions',paste0(part,'.rds')))==p$partition_hashes[[part]])
for(n in names(p$control_hashes))stopifnot(study_hash(file.path(output,'controls',n))==p$control_hashes[[n]])
data_for<-function(part){d<-readRDS(file.path(output,'data/prepared_data.rds'));d$fold<-readRDS(file.path(output,'partitions',paste0(part,'.rds')));d}
if(action=='verify') {
 records<-list()
 for(part in p$partitions)for(seed in p$seeds)for(condition in names(p$settings)) {
   d<-data_for(part);s<-p$settings[[condition]];s$seed<-seed
   cleanup<-f('lkt_gru_runtime')(s);prep<-f('lkt_gru_preprocess')(d,d$fold==1,p$baseline,s)
   encoded<-f('lkt_gru_encode')(d,prep,p$baseline,s);net<-f('lkt_gru_network')(prep,s);net$eval()
   stopifnot(length(prep$numeric_names)==6L,sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))==p$parameter_counts[[condition]])
   groups<-head(split(seq_len(nrow(d)),f('lkt_gru_keys')(d,s$history_group_cols)),4)
   b<-f('lkt_gru_batch')(encoded,d,groups,lapply(groups,function(x)seq_len(min(length(x),100L))),s)
   baseline<-s;baseline$gate_fatigue<-FALSE;baseline$gate_fatigue_time_col<-NULL
   # Copy shared weights rather than changing RNG: exact initial weights are separately checked.
   base<-f('lkt_gru_network')(prep,baseline);base$eval()
   w<-f('lkt_gru_weights')(net);f('lkt_gru_load_weights')(base,w[!grepl('^gate_fatigue[.]',names(w))],baseline)
   run<-function(net,s)torch::with_no_grad({f('lkt_gru_forward')(net,b$numeric,b$categorical,b$answers,NULL,NULL,b$retrieval_ids,s,b$decay_gaps,!is.na(b$rows))})
   q<-run(net,s);r<-run(base,baseline);difference<-max(abs(torch::as_array(q$logits-r$logits)[!is.na(b$rows)]))
   stopifnot(difference<3e-6,w[['gate_fatigue.kappa']]==0)
   # Recreate ordinary network under the same seed and check all shared initial draws.
   cleanup();cleanup<-f('lkt_gru_runtime')(baseline);seeded<-f('lkt_gru_network')(prep,baseline)
   stopifnot(identical(f('lkt_gru_weights')(seeded),w[!grepl('^gate_fatigue[.]',names(w))]));cleanup()
   records[[length(records)+1L]]<-data.frame(partition=part,seed=seed,condition=condition,maximum_logit_difference=difference)
 }
 study_write_csv(do.call(rbind,records),file.path(output,'reports/stock-equivalence.csv'))
 # Confirm the unchanged disabled path reads and reproduces all reused held-out controls.
 reuse<-list()
 for(i in seq_len(nrow(p$controls))) {
   task<-p$controls[i,,drop=FALSE];fit<-readRDS(file.path(output,'controls/models',paste0(task$run_id,'.rds')))$fit
   d<-data_for(task$partition);held<-d[d$fold==3,,drop=FALSE]
   pred<-plastic_study_replay(fit,held)$prediction
   difference<-max(abs(pred-fit$prediction[d$fold==3]));stopifnot(difference<3e-6)
   reuse[[i]]<-cbind(task,maximum_probability_difference=difference)
 }
 study_write_csv(do.call(rbind,reuse),file.path(output,'reports/reused-control-replay.csv'))
 cat('PASS 30 zero-fatigue comparisons, identical shared initialization, and 30 reused control replays.\n')
}
if(action=='smoke') {
 raw<-data_for('cv1');idx<-head(which(raw$Anon.Student.Id==raw$Anon.Student.Id[1]),12)
 d<-do.call(rbind,lapply(1:8,function(i){x<-raw[idx,,drop=FALSE];x$Anon.Student.Id<-paste0('smoke',i);x$CF..ansbin.<-rep(c(0,1,1,0,1,0),2);x$fold<-c(1,1,1,1,1,2,3,3)[i];x}))
 smoke<-file.path(dirname(output),'smoke');stopifnot(!dir.exists(smoke));pp<-p
 for(n in c('models','training','reports'))dir.create(file.path(smoke,n),recursive=TRUE)
 for(condition in names(p$settings)) {
   prep<-f('lkt_gru_preprocess')(d,d$fold==1,p$baseline,p$settings[[condition]])
   net<-f('lkt_gru_network')(prep,p$settings[[condition]])
   pp$parameter_counts[[condition]]<-sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
 }
 study_write_rds(pp,file.path(smoke,'study.rds'))
 for(condition in names(p$settings)) {
   s<-p$settings[[condition]];s$epochs<-1L;s$early_stopping<-FALSE;s$restore_best<-FALSE
   task<-data.frame(run_id=paste0('smoke__',condition,'__seed1'),partition='smoke',candidate=condition,seed=1L)
   gate_study_fit(task,s,smoke,pp,d)
 }
 cat('PASS two one-epoch smoke fits with saved-model replay.\n')
}
if(action=='fit') {
 id<-as.integer(a[4]);stopifnot(!is.na(id),id>=0L,id<30L)
 task<-p$tasks[id+1L,,drop=FALSE];gate_study_fit(task,p$settings[[task$candidate]],output,p,data_for(task$partition))
}
if(action%in%c('report','check'))gate_study_report(output,p,action)
