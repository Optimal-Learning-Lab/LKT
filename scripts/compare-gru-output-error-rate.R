# ROOT OUTPUT ACTION TASK CANDIDATE_JSON LIBRARY CONSTANT_RECURRENT_REFERENCE
args<-commandArgs(TRUE);stopifnot(length(args)==7L)
root<-args[1];out<-args[2];action<-args[3]
source(file.path(root,'scripts/gru-encoder-study.R'))
candidate<-study_require_release(args[5],args[6])
source(file.path(root,'scripts/gru-plasticity-study.R'))
source(file.path(root,'scripts/gru-connection-fatigue-study.R'))
source(file.path(root,'scripts/gru-connection-recurrent-study.R'))
source(file.path(root,'scripts/gru-output-error-rate-study.R'))
f<-function(n)getFromNamespace(n,'LKT')
files<-c('scripts/compare-gru-output-error-rate.R','scripts/gru-encoder-study.R','scripts/gru-plasticity-study.R','scripts/gru-connection-fatigue-study.R','scripts/gru-connection-recurrent-study.R','scripts/gru-output-error-rate-study.R')
hashes<-setNames(vapply(file.path(root,files),study_hash,character(1)),files)
if(action=='plan') {
 stopifnot(!dir.exists(out));prior<-readRDS(file.path(args[7],'study.rds'))
 stopifnot(prior$total_fits==15L,prior$settings[[1]]$gate_fatigue_mode=='connection',prior$settings[[1]]$plasticity=='constant')
 p<-prior;p$prior_candidate<-p$candidate;p$candidate<-candidate;p$candidate_file_sha256<-study_hash(args[5]);p$script_hashes<-hashes
 p$reference_hashes<-c(full=study_hash(file.path(args[7],'study.rds')))
 p$prior_report_hashes<-setNames(vapply(file.path(args[7],'reports',c('fits.csv','pooled-by-seed.csv')),study_hash,character(1)),c('fits.csv','pooled-by-seed.csv'))
 p$design<-'15 evolving student-rate output fits matched to constant-rate recurrent-only fatigue. v += eta_student*(y-p)*z; eta_student += alpha*abs(y-p)-c. Train baseline eta from zero and alpha/c from zero; exact saved clocks, inputs, folds, seeds, training controls and lambda decay exclusion.'
 p$output_initialization<-c(eta=0,alpha=0,c=0)
 p$lambda_optimizer_weight_decay<-0
 for(n in names(p$settings)) {
   base<-do.call(GRUModel,p$settings[[n]])$settings;s<-base
   stopifnot(s$connection_fatigue_scope=='recurrent_candidate');s$plasticity<-'error_rate'
   matched<-setdiff(names(s),'plasticity')
   stopifnot(identical(s[matched],base[matched]));p$settings[[n]]<-s
 }
 d<-readRDS(file.path(args[7],'data/prepared_data.rds'));d$fold<-readRDS(file.path(args[7],'partitions/cv1.rds'))
 s<-p$settings[[1]];prep<-f('lkt_gru_preprocess')(d,d$fold==1,p$baseline,s);net<-f('lkt_gru_network')(prep,s)
 stopifnot(identical(f('lkt_gru_fatigue_indices')(f('lkt_sequence_module')(net,s),s),prior$recurrent_full_indices))
 stopifnot(identical(as.numeric(torch::as_array(net$gate_fatigue$time_scale)),prior$fixed_time_scales),length(prior$fixed_time_scales)==3072L,
   sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))==prior$parameter_counts[[1]]+2L,
   net$plasticity$eta$item()==0,net$plasticity$alpha$item()==0,net$plasticity$c$item()==0)
 p$parameter_counts[[1]]<-prior$parameter_counts[[1]]+2L
 dir.create(out,recursive=TRUE)
 for(n in c('data','partitions','models','training','reports'))dir.create(file.path(out,n))
 for(n in c('data/prepared_data.rds',paste0('partitions/',unique(p$tasks$partition),'.rds')))
   stopifnot(file.copy(file.path(args[7],n),file.path(out,n)))
 study_write_csv(data.frame(connection_index=seq_along(p$fixed_time_scales),time_scale=p$fixed_time_scales),file.path(out,'reports/fixed-time-scales.csv'))
 study_write_rds(p,file.path(out,'study.rds'));study_write_csv(p$tasks,file.path(out,'tasks.csv'))
 cat('FROZEN 15 signed output fits; baseline eta/alpha/c trained; two added scalars; exact recurrent clocks and matched controls.\n');quit(save='no')
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
 error_rate_study_fit(task,s,smoke,pp,d)
 cat('PASS matching inputs/scaling and one short smoke fit.\n')
}
if(action=='fit') {
 id<-as.integer(args[4]);stopifnot(id>=0L,id<15L)
 task<-p$tasks[id+1L,,drop=FALSE];error_rate_study_fit(task,p$settings$connection_no_ppes,out,p,data_for(task$partition))
}
if(action=='report') {
 error_rate_study_report(out,p,data_for)
 for(n in names(p$prior_report_hashes))stopifnot(study_hash(file.path(args[7],'reports',n))==p$prior_report_hashes[[n]])
 old<-read.csv(file.path(args[7],'reports/fits.csv'));new<-read.csv(file.path(out,'reports/fits.csv'))
 paired<-merge(old,new,by=c('partition','seed'),suffixes=c('_constant','_evolving_rate'));stopifnot(nrow(paired)==15L)
 study_write_csv(paired,file.path(out,'reports/paired-with-original.csv'))
 old<-read.csv(file.path(args[7],'reports/pooled-by-seed.csv'));new<-read.csv(file.path(out,'reports/pooled-by-seed.csv'))
 paired<-merge(old,new,by='seed',suffixes=c('_constant','_evolving_rate'));stopifnot(nrow(paired)==3L)
 study_write_csv(paired,file.path(out,'reports/pooled-with-original.csv'))
 summary<-data.frame(output_adaptation=c('constant','evolving_rate'),AUC=c(mean(old$AUC),mean(new$AUC)),log_loss=c(mean(old$log_loss),mean(new$log_loss)))
 study_write_csv(summary,file.path(out,'reports/comparison-summary.csv'));print(summary)
}
