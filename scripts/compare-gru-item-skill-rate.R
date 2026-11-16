# ROOT OUTPUT ACTION TASK CANDIDATE_JSON LIBRARY CONSTANT_RECURRENT_REFERENCE
args<-commandArgs(TRUE);stopifnot(length(args)==7L)
root<-args[1];out<-args[2];action<-args[3]
source(file.path(root,'scripts/gru-encoder-study.R'))
candidate<-study_require_release(args[5],args[6])
source(file.path(root,'scripts/gru-plasticity-study.R'))
source(file.path(root,'scripts/gru-item-skill-rate-study.R'))
f<-function(n)getFromNamespace(n,'LKT')
files<-c('scripts/compare-gru-item-skill-rate.R','scripts/gru-encoder-study.R','scripts/gru-plasticity-study.R','scripts/gru-item-skill-rate-study.R')
hashes<-setNames(vapply(file.path(root,files),study_hash,character(1)),files)
if(action=='plan') {
 stopifnot(!dir.exists(out));prior<-readRDS(file.path(args[7],'study.rds'))
 stopifnot(prior$total_fits==45L,prior$settings$constant$plasticity=='constant')
 p<-prior;p$prior_candidate<-p$candidate;p$candidate<-candidate;p$candidate_file_sha256<-study_hash(args[5]);p$script_hashes<-hashes
 p$reference_hashes<-c(full=study_hash(file.path(args[7],'study.rds')))
 p$prior_report_hashes<-setNames(vapply(file.path(args[7],'reports',c('fits.csv','pooled-by-seed.csv')),study_hash,character(1)),c('fits.csv','pooled-by-seed.csv'))
 base<-do.call(GRUModel,prior$settings$constant)$settings;s<-base;s$plasticity<-'item_skill'
 stopifnot(!s$gate_fatigue,!s$hidden_decay,identical(s[setdiff(names(s),'plasticity')],base[setdiff(names(base),'plasticity')]))
 p$settings<-list(item_skill=s);p$total_fits<-15L
 p$tasks<-prior$tasks[prior$tasks$candidate=='constant',,drop=FALSE];p$tasks$candidate<-'item_skill';p$tasks$run_id<-sub('__constant__','__item_skill__',p$tasks$run_id,fixed=TRUE);rownames(p$tasks)<-NULL
 p$design<-'15 joint item/skill output-rate fits vs six-input constant-rate GRU; no connection fatigue; same recipe/folds/seeds/optimizer/stopping; skill rates zero and item multipliers one initially.'
 d<-readRDS(file.path(args[7],'data/prepared_data.rds'));d$fold<-readRDS(file.path(args[7],'partitions/cv1.rds'))
 prep<-f('lkt_gru_preprocess')(d,d$fold==1,p$baseline,s);net<-f('lkt_gru_network')(prep,s)
 stopifnot(length(prep$numeric_names)==6L,length(prep$plasticity_item_skill$items)==144L,length(prep$plasticity_item_skill$skills)==36L,
  all(tabulate(prep$plasticity_item_skill$parent)==4L),all(torch::as_array(net$plasticity$skill_rate)==0),all(torch::as_array(net$plasticity$log_item)==0))
 p$item_count<-144L;p$skill_count<-36L;p$numeric_names<-prep$numeric_names;p$parameter_counts<-c(item_skill=sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1))))
 stopifnot(p$parameter_counts[[1]]==prior$parameter_counts[['constant']]+179L)
 dir.create(out,recursive=TRUE)
 for(n in c('data','partitions','models','training','reports'))dir.create(file.path(out,n))
 for(n in c('data/prepared_data.rds',paste0('partitions/',unique(p$tasks$partition),'.rds')))
   stopifnot(file.copy(file.path(args[7],n),file.path(out,n)))
 study_write_rds(p,file.path(out,'study.rds'));study_write_csv(p$tasks,file.path(out,'tasks.csv'))
 cat('FROZEN 15 joint item/skill fits; six inputs; 144 item multipliers and 36 skill rates; original constant controls retained.\n');quit(save='no')
}
p<-readRDS(file.path(out,'study.rds'))
stopifnot(identical(hashes,p$script_hashes),identical(candidate,p$candidate),study_hash(args[5])==p$candidate_file_sha256,
 study_hash(file.path(out,'data/prepared_data.rds'))==p$data_sha256,
 study_hash(file.path(args[7],'study.rds'))==p$reference_hashes[['full']])
data_for<-function(part){d<-readRDS(file.path(out,'data/prepared_data.rds'));path<-file.path(out,'partitions',paste0(part,'.rds'));stopifnot(study_hash(path)==p$partition_hashes[[part]]);d$fold<-readRDS(path);d}
if(action=='smoke') {
 d<-data_for('cv1');s<-p$settings$item_skill
 idx<-head(which(d$Anon.Student.Id==d$Anon.Student.Id[1]),12)
 d<-do.call(rbind,lapply(1:8,function(i){x<-d[idx,,drop=FALSE];x$Anon.Student.Id<-paste0('smoke',i);x$CF..ansbin.<-rep(c(0,1,1,0,1,0),2);x$fold<-c(1,1,1,1,1,2,3,3)[i];x}))
 smoke<-file.path(dirname(out),'smoke');stopifnot(!dir.exists(smoke))
 for(n in c('models','training','reports'))dir.create(file.path(smoke,n),recursive=TRUE)
 pp<-p;prep<-f('lkt_gru_preprocess')(d,d$fold==1,p$baseline,s);net<-f('lkt_gru_network')(prep,s)
 pp$item_count<-length(prep$plasticity_item_skill$items);pp$skill_count<-length(prep$plasticity_item_skill$skills)
 pp$parameter_counts[[1]]<-sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
 study_write_rds(pp,file.path(smoke,'study.rds'));s$epochs<-1L;s$early_stopping<-FALSE;s$restore_best<-FALSE
 task<-data.frame(run_id='smoke__item_skill__seed1',partition='smoke',candidate='item_skill',seed=1L)
 item_skill_study_fit(task,s,smoke,pp,d)
 cat('PASS matching inputs/scaling and one short smoke fit.\n')
}
if(action=='fit') {
 id<-as.integer(args[4]);stopifnot(id>=0L,id<15L)
 task<-p$tasks[id+1L,,drop=FALSE];item_skill_study_fit(task,p$settings$item_skill,out,p,data_for(task$partition))
}
if(action=='report') {
 item_skill_study_report(out,p,data_for)
 for(n in names(p$prior_report_hashes))stopifnot(study_hash(file.path(args[7],'reports',n))==p$prior_report_hashes[[n]])
 old<-read.csv(file.path(args[7],'reports/fits.csv'));old<-old[old$candidate=='constant',];new<-read.csv(file.path(out,'reports/fits.csv'))
 paired<-merge(old,new,by=c('partition','seed'),suffixes=c('_constant','_item_skill'));stopifnot(nrow(paired)==15L)
 study_write_csv(paired,file.path(out,'reports/paired-with-original.csv'))
 old<-read.csv(file.path(args[7],'reports/pooled-by-seed.csv'));old<-old[old$condition=='constant',];new<-read.csv(file.path(out,'reports/pooled-by-seed.csv'))
 paired<-merge(old,new,by='seed',suffixes=c('_constant','_item_skill'));stopifnot(nrow(paired)==3L)
 study_write_csv(paired,file.path(out,'reports/pooled-with-original.csv'))
 summary<-data.frame(output_adaptation=c('constant','item_skill'),AUC=c(mean(old$AUC),mean(new$AUC)),log_loss=c(mean(old$log_loss),mean(new$log_loss)))
 study_write_csv(summary,file.path(out,'reports/comparison-summary.csv'));print(summary)
}
