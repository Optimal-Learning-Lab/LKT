# ROOT OUTPUT ACTION TASK CANDIDATE_JSON LIBRARY CANDIDATE_REFERENCE
args<-commandArgs(TRUE);stopifnot(length(args)==7L)
root<-args[1];out<-args[2];action<-args[3]
source(file.path(root,'scripts/gru-encoder-study.R'));candidate<-study_require_release(args[5],args[6])
source(file.path(root,'scripts/gru-plasticity-study.R'));source(file.path(root,'scripts/gru-connection-fatigue-study.R'))
source(file.path(root,'scripts/gru-connection-input-study.R'));f<-function(n)getFromNamespace(n,'LKT')
files<-c('scripts/compare-gru-connection-input.R','scripts/gru-encoder-study.R','scripts/gru-plasticity-study.R','scripts/gru-connection-fatigue-study.R','scripts/gru-connection-input-study.R')
hashes<-setNames(vapply(file.path(root,files),study_hash,character(1)),files)
if(action=='plan') {
 stopifnot(!dir.exists(out));prior<-readRDS(file.path(args[7],'study.rds'))
 stopifnot(prior$total_fits==15L,prior$settings[[1]]$connection_fatigue_scope=='candidate')
 p<-prior;p$prior_candidate<-p$candidate;p$candidate<-candidate;p$candidate_file_sha256<-study_hash(args[5]);p$script_hashes<-hashes
 p$reference_hashes<-c(full=study_hash(file.path(args[7],'study.rds')))
 p$prior_report_hashes<-setNames(vapply(file.path(args[7],'reports',c('fits.csv','pooled-by-seed.csv')),study_hash,character(1)),c('fits.csv','pooled-by-seed.csv'))
 base<-do.call(GRUModel,prior$settings[[1]])$settings
 p$settings<-list(input_all=base,input_h1=base);p$settings$input_all$connection_fatigue_scope<-'input_candidate';p$settings$input_h1$connection_fatigue_scope<-'h1_input_candidate'
 d<-readRDS(file.path(args[7],'data/prepared_data.rds'));d$fold<-readRDS(file.path(args[7],'partitions/cv1.rds'))
 prep<-f('lkt_gru_preprocess')(d,d$fold==1,p$baseline,base)
 p[c('candidate_full_indices','fixed_time_scales','fixed_time_scale_sha256','full_time_scale_sha256')]<-NULL
 p$full_indices<-p$fixed_times<-p$fixed_time_hashes<-list();p$parameter_counts<-c(input_all=NA_integer_,input_h1=NA_integer_)
 for(n in names(p$settings)) {
  s<-p$settings[[n]];matched<-setdiff(names(s),'connection_fatigue_scope');stopifnot(identical(s[matched],base[matched]))
  net<-f('lkt_gru_network')(prep,s);idx<-f('lkt_gru_fatigue_indices')(f('lkt_sequence_module')(net,s),s)
  source<-match(idx,prior$candidate_full_indices);stopifnot(!anyNA(source))
  scale<-as.numeric(torch::as_array(net$gate_fatigue$time_scale));stopifnot(identical(scale,prior$fixed_time_scales[source]),length(scale)==if(n=='input_all')4128L else 2080L)
  p$full_indices[[n]]<-idx;p$fixed_times[[n]]<-scale;p$fixed_time_hashes[[n]]<-digest::digest(scale,algo='sha256')
  count<-sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
  stopifnot(count==prior$parameter_counts[[1]]-(7200L-length(scale)));p$parameter_counts[[n]]<-count
 }
 tasks<-expand.grid(seed=1:3,candidate=names(p$settings),partition=unique(prior$tasks$partition),stringsAsFactors=FALSE)
 tasks$seed<-as.integer(tasks$seed);tasks$run_id<-with(tasks,paste(partition,candidate,paste0('seed',seed),sep='__'));p$tasks<-tasks[,c('run_id','partition','candidate','seed')]
 p$total_fits<-30L;p$design<-'30 matched fits: 15 input-to-candidate all layers and 15 input-to-candidate H1 only. Exact retained saved clocks; no recurrent or gate fatigue. All other settings unchanged.'
 dir.create(out,recursive=TRUE);for(n in c('data','partitions','models','training','reports'))dir.create(file.path(out,n))
 for(n in c('data/prepared_data.rds',paste0('partitions/',unique(p$tasks$partition),'.rds')))stopifnot(file.copy(file.path(args[7],n),file.path(out,n)))
 for(n in names(p$settings))study_write_csv(data.frame(full_connection_index=p$full_indices[[n]],time_scale=p$fixed_times[[n]]),file.path(out,'reports',paste0('fixed-time-scales-',n,'.csv')))
 study_write_rds(p,file.path(out,'study.rds'));study_write_csv(p$tasks,file.path(out,'tasks.csv'))
 cat('FROZEN 30 fits: input-all 15, H1-only 15; exact retained clocks and matched settings.\n');quit(save='no')
}
p<-readRDS(file.path(out,'study.rds'))
stopifnot(identical(hashes,p$script_hashes),identical(candidate,p$candidate),study_hash(args[5])==p$candidate_file_sha256,
 study_hash(file.path(out,'data/prepared_data.rds'))==p$data_sha256,study_hash(file.path(args[7],'study.rds'))==p$reference_hashes[['full']])
data_for<-function(part){d<-readRDS(file.path(out,'data/prepared_data.rds'));path<-file.path(out,'partitions',paste0(part,'.rds'));stopifnot(study_hash(path)==p$partition_hashes[[part]]);d$fold<-readRDS(path);d}
if(action=='smoke') {
 raw<-data_for('cv1');idx<-head(which(raw$Anon.Student.Id==raw$Anon.Student.Id[1]),12)
 d<-do.call(rbind,lapply(1:8,function(i){x<-raw[idx,,drop=FALSE];x$Anon.Student.Id<-paste0('smoke',i);x$CF..ansbin.<-rep(c(0,1,1,0,1,0),2);x$fold<-c(1,1,1,1,1,2,3,3)[i];x}))
 smoke<-file.path(dirname(out),'smoke');stopifnot(!dir.exists(smoke));for(n in c('models','training','reports'))dir.create(file.path(smoke,n),recursive=TRUE)
 pp<-p
 for(n in names(p$settings)) {
  s<-p$settings[[n]];prep<-f('lkt_gru_preprocess')(d,d$fold==1,p$baseline,s);net<-f('lkt_gru_network')(prep,s)
  pp$parameter_counts[[n]]<-sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
 }
 study_write_rds(pp,file.path(smoke,'study.rds'))
 for(n in names(p$settings)) {
  s<-p$settings[[n]];s$epochs<-1L;s$early_stopping<-FALSE;s$restore_best<-FALSE
  task<-data.frame(run_id=paste0('smoke__',n,'__seed1'),partition='smoke',candidate=n,seed=1L)
  input_study_fit(task,s,smoke,pp,d)
 }
 cat('PASS two short smoke fits and exact saved-clock/replay checks.\n')
}
if(action=='fit') {
 id<-as.integer(args[4]);stopifnot(id>=0L,id<30L);task<-p$tasks[id+1L,,drop=FALSE]
 input_study_fit(task,p$settings[[task$candidate]],out,p,data_for(task$partition))
}
if(action=='report')input_study_report(out,p,data_for,args[7])
