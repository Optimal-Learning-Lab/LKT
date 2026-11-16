# Matched numeric-subset follow-up using an explicitly verified package candidate.
# ROOT OUTPUT ACTION PART CANDIDATE_JSON CANDIDATE_LIBRARY [REFERENCE]
a <- commandArgs(TRUE);stopifnot(length(a) %in% 6:7)
root <- normalizePath(a[1],winslash='/',mustWork=TRUE);output <- a[2];action <- a[3];part <- a[4]
stopifnot(action %in% c('plan','verify','smoke','fit','report','check'))
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
source(file.path(root,'scripts/gru-encoder-study.R'))
candidate_file <- normalizePath(a[5],winslash='/',mustWork=TRUE)
candidate <- study_require_release(candidate_file,a[6])
stopifnot(candidate$validation_candidate,study_hash(file.path(dirname(candidate_file),candidate$source_archive))==candidate$source_sha256)
source(file.path(root,'scripts/report-gru-single-additions.R'))
files <- c('scripts/compare-gru-single-additions.R','scripts/report-gru-single-additions.R','scripts/gru-encoder-study.R')
hashes <- setNames(vapply(file.path(root,files),study_hash,character(1)),files)
conditions <- c('add_item_logitdec','add_student_logitdec','add_item_ppes')
tasks_for <- function(part)do.call(rbind,lapply(conditions,function(condition)data.frame(
 run_id=paste(part,condition,paste0('seed',1:3),sep='__'),partition=part,
 family='augmented',stage=condition,candidate=condition,seed=1:3)))
if(action=='plan') {
 stopifnot(length(a)==7L,!dir.exists(output));ref <- a[7];old <- readRDS(file.path(ref,'study.rds'))
 stopifnot(old$total_fits==15L,identical(old$seeds,1:3),identical(old$partitions,paste0('cv',1:5)))
 base <- do.call(GRUModel,old$settings$timing)$settings
 columns <- old$numeric_names;stopifnot(length(columns)==4L,all(startsWith(columns,'time:')),!base$use_lkt_features)
 feature_names <- c('lkt:logitdecKC..Default.','lkt:logitdecAnon.Student.Id','lkt:ppesKC..Default.')
 labels <- setNames(c('Four + student-item logitdec','Four + student logitdec','Four + student-item ppes'),conditions)
 settings <- setNames(lapply(seq_along(conditions),function(j){s<-base;s$use_lkt_features<-TRUE;s$numeric_feature_cols<-c(columns,feature_names[j]);do.call(GRUModel,s)$settings}),conditions)
 baseline <- old$baseline
 counts <- getFromNamespace('lkt_feature_param_counts','LKT')(baseline$features)
 ends <- cumsum(counts);starts <- c(1L,head(ends,-1L)+1L)
 indices <- c(which(baseline$components=='KC..Default.' & baseline$features=='logitdec'),
  which(baseline$components=='Anon.Student.Id' & baseline$features=='logitdec'),
  which(baseline$components=='KC..Default.' & baseline$features=='ppes'))
 stopifnot(length(indices)==3L,!anyDuplicated(indices))
 recipes <- setNames(lapply(indices,function(j){r<-baseline;r$components<-baseline$components[j];r$features<-baseline$features[j];r$fixedpars<-baseline$fixedpars[starts[j]:ends[j]];r$interacts<-baseline$interacts[j];r$connectors<-character();r}),conditions)
 for(s in settings)stopifnot(identical(s[setdiff(names(s),c('numeric_feature_cols','use_lkt_features'))],base[setdiff(names(base),c('numeric_feature_cols','use_lkt_features'))]))
 dir.create(output,recursive=TRUE)
 for(folder in c('data','partitions','models','validation','training','reports','reference','reference/models'))dir.create(file.path(output,folder))
 required <- c('study.rds','data/prepared_data.rds','reports/summary.csv','reports/auc-by-seed.csv','reports/verification.txt')
 for(k in old$partitions) {
   required <- c(required,paste0('partitions/',k,'.rds'),paste0('reports/',k,'-predictions.csv'),paste0('reports/',k,'-verification.txt'),paste0('reference/',k,'-predictions.csv'))
   for(seed in 1:3)required <- c(required,paste0('models/',k,'__timing__seed',seed,'.rds'),paste0('reference/models/',k,'__all__seed',seed,'.rds'))
 }
 input_hashes <- setNames(vapply(file.path(ref,required),study_hash,character(1)),required)
 stopifnot(input_hashes[['data/prepared_data.rds']]==old$data_sha256,
 file.copy(file.path(ref,'data/prepared_data.rds'),file.path(output,'data/prepared_data.rds')))
 d <- readRDS(file.path(output,'data/prepared_data.rds'))
 for(k in old$partitions) {
   stopifnot(file.copy(file.path(ref,'partitions',paste0(k,'.rds')),file.path(output,'partitions',paste0(k,'.rds'))))
   roles <- readRDS(file.path(output,'partitions',paste0(k,'.rds')))
   stopifnot(!anyDuplicated(unique(data.frame(id=d$Anon.Student.Id,role=roles))$id))
   q <- read.csv(file.path(ref,'reports',paste0(k,'-predictions.csv')))
   all <- read.csv(file.path(ref,'reference',paste0(k,'-predictions.csv')));all<-all[all$condition=='all',]
   study_write_csv(rbind(q,all),file.path(output,'reference',paste0(k,'-predictions.csv')))
   for(condition in c('timing','all'))for(seed in 1:3) {
     name <- paste0(k,'__',condition,'__seed',seed,'.rds')
     source <- if(condition=='timing')file.path(ref,'models',name) else file.path(ref,'reference/models',name)
     stopifnot(file.copy(source,file.path(output,'reference/models',name)))
   }
 }
 for(name in c('summary.csv','auc-by-seed.csv'))stopifnot(file.copy(file.path(ref,'reports',name),file.path(output,'reference',name)))
 p <- list(schema=1L,candidate=candidate,candidate_file_sha256=study_hash(candidate_file),script_hashes=hashes,
   reference_input_hashes=input_hashes,reference_protocol_sha256=input_hashes[['study.rds']],
   reference_protocol_hashes=c(timing=input_hashes[['study.rds']],all=old$reference_protocol_sha256),
   data_sha256=old$data_sha256,partitions=old$partitions,partition_hashes=old$partition_hashes,seeds=1:3,
   total_fits=45L,conditions=conditions,settings=settings,baseline=baseline,recipes=recipes,
   reference_settings=list(timing=old$settings$timing,all=old$reference_settings$all),
   reference_parameter_counts=c(timing=43009L,all=43105L),
   intercepts=setNames(rep(FALSE,3),conditions),parameter_counts=setNames(rep(43041L,3),conditions),
   numeric_names=lapply(settings,`[[`,'numeric_feature_cols'),labels=labels,
   design='Forty-five fresh fits: four data inputs plus one of three LKT predictors, five exact existing participant folds and seeds. Thirty saved timing-only/all-seven controls.',
   changes='Each fresh model uses a single-feature LKT recipe, inherited fixed nonlinear constants, and five numeric coordinates. All neural/training controls remain fixed.',
   reporting='Mean training/validation AUC across fifteen restored fits per condition; pool test folds within seed then average three seeds.',
   limits='Exploratory matched-budget comparison on previously inspected Cloze; no held-out selection or active-release change.')
 generated <- c('reference/summary.csv','reference/auc-by-seed.csv',paste0('reference/',old$partitions,'-predictions.csv'))
 p$reference_generated_hashes <- setNames(vapply(file.path(output,generated),study_hash,character(1)),generated)
 study_write_rds(p,file.path(output,'study.rds'));writeLines(capture.output(dput(p)),file.path(output,'study.txt'))
 cat('FROZEN 45 single-addition fits; 30 saved control fits preserved.\n');quit(save='no')
}
p <- readRDS(file.path(output,'study.rds'))
stopifnot(identical(p$candidate,candidate),study_hash(candidate_file)==p$candidate_file_sha256,
 identical(p$script_hashes,hashes),study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256)
for(k in p$partitions)stopifnot(study_hash(file.path(output,'partitions',paste0(k,'.rds')))==p$partition_hashes[[k]])
for(f in names(p$reference_generated_hashes))stopifnot(study_hash(file.path(output,f))==p$reference_generated_hashes[[f]])
for(f in grep('^(models/|reference/models/)',names(p$reference_input_hashes),value=TRUE))stopifnot(study_hash(file.path(output,'reference/models',basename(f)))==p$reference_input_hashes[[f]])
if(action=='verify') {
 d<-readRDS(file.path(output,'data/prepared_data.rds'));rows<-list()
 for(k in p$partitions){
  d$fold<-readRDS(file.path(output,'partitions',paste0(k,'.rds')))
  full<-p$reference_settings$all
  y<-getFromNamespace('lkt_gru_numeric','LKT')(d,p$baseline,full,fit_rows=d$fold==1)$matrix
  for(condition in conditions){s<-p$settings[[condition]];recipe<-p$recipes[[condition]]
   prep<-getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1,recipe,s)
   net<-getFromNamespace('lkt_gru_network','LKT')(prep,s)
   n<-sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
   x<-getFromNamespace('lkt_gru_numeric','LKT')(d,recipe,s,fit_rows=d$fold==1)$matrix
   stopifnot(identical(prep$numeric_names,p$numeric_names[[condition]]),length(prep$engineered$columns)==1L,n==43041L,
    isTRUE(all.equal(as.matrix(x),as.matrix(y[,p$numeric_names[[condition]],drop=FALSE]),check.attributes=FALSE)))
   rows[[paste(k,condition)]]<-data.frame(partition=k,condition=condition,numeric_inputs=5L,LKT_inputs=1L,parameters=n)
  }
 }
 study_write_csv(do.call(rbind,rows),file.path(output,'reports/preflight.csv'))
 cat('PASS all 15 fold/condition schemas: four timing plus exactly one original-valued LKT predictor; 43041 parameters.\n')
}
if(action=='smoke') {
 raw <- readRDS(file.path(output,'data/prepared_data.rds'));idx <- head(which(raw$Anon.Student.Id==raw$Anon.Student.Id[1]),12)
 d <- do.call(rbind,lapply(1:8,function(i){x<-raw[idx,,drop=FALSE];x$Anon.Student.Id<-paste0('synthetic',i);x$CF..ansbin.<-rep(c(0,1,1,0,1,0),2);x$fold<-c(1,1,1,1,1,2,3,3)[i];x}))
 out <- file.path(dirname(output),'smoke');stopifnot(!dir.exists(out))
 for(f in c('models','validation','training'))dir.create(file.path(out,f),recursive=TRUE)
 study_write_rds(p,file.path(out,'study.rds'))
 for(condition in conditions){s<-p$settings[[condition]];s$epochs<-1L;s$early_stopping<-FALSE;s$restore_best<-FALSE
  task<-tasks_for('smoke');task<-task[task$candidate==condition & task$seed==1,,drop=FALSE]
  pp<-p;pp$baseline<-p$recipes[[condition]];study_fit_one(task,s,out,pp,d,intercept=FALSE)
  fit<-readRDS(file.path(out,'models',paste0(task$run_id,'.rds')))$fit
  stopifnot(fit$gru_settings$use_lkt_features,length(fit$model$preprocessing$engineered$columns)==1L,
   identical(fit$model$preprocessing$numeric_names,p$numeric_names[[condition]]),
   max(abs(study_replay(fit,d[d$fold==3,,drop=FALSE])-fit$prediction[d$fold==3]))<3e-6)
 }
 cat('PASS all three one-epoch single-addition fits and saved-weight replays.\n')
}
if(action=='fit') {
 bits<-strsplit(part,':',fixed=TRUE)[[1]];stopifnot(length(bits)==2L);part<-bits[1];condition<-bits[2];stopifnot(part %in% p$partitions,condition %in% conditions);job_key<-paste(part,condition,sep='-');d<-readRDS(file.path(output,'data/prepared_data.rds'))
 d$fold<-readRDS(file.path(output,'partitions',paste0(part,'.rds')));tasks<-tasks_for(part);tasks<-tasks[tasks$candidate==condition,,drop=FALSE];fit_p<-p;fit_p$baseline<-p$recipes[[condition]]
 workers<-as.integer(Sys.getenv('LKT_STUDY_WORKERS','1'));stopifnot(workers %in% 1:3)
 if(workers==1L)for(i in 1:3)study_fit_one(tasks[i,,drop=FALSE],p$settings[[condition]],output,fit_p,d,intercept=FALSE) else {
  cl<-parallel::makePSOCKcluster(workers,outfile=file.path(output,paste0(job_key,'-workers.log')))
  tryCatch({
   parallel::clusterCall(cl,function(root,candidate_file,lib){assign('root',root,envir=.GlobalEnv);source(file.path(root,'scripts/gru-encoder-study.R'));study_require_release(candidate_file,lib);NULL},root,candidate_file,a[6])
   parallel::clusterExport(cl,c('p','fit_p','condition','d','tasks','output'))
   parallel::parLapplyLB(cl,1:3,function(i)study_fit_one(tasks[i,,drop=FALSE],p$settings[[condition]],output,fit_p,d,intercept=FALSE))
  },finally=parallel::stopCluster(cl))
 }
 q<-list();test<-d[d$fold==3,,drop=FALSE]
 for(i in 1:3){t<-tasks[i,,drop=FALSE];f<-readRDS(file.path(output,'models',paste0(t$run_id,'.rds')))$fit
  pred<-study_replay(f,test);stopifnot(max(abs(pred-f$prediction[d$fold==3]))<3e-6)
  q[[i]]<-data.frame(partition=part,condition=condition,seed=t$seed,source_row=test$comparison_source_row,participant=test$Anon.Student.Id,observed=test$CF..ansbin.,prediction=pred)}
 study_write_csv(do.call(rbind,q),file.path(output,'reports',paste0(job_key,'-predictions.csv')))
 writeLines('PASS all three single-addition saved-weight held-out replays.',file.path(output,'reports',paste0(job_key,'-verification.txt')))
}
if(action %in% c('report','check'))single_additions_report(output,p,action)
