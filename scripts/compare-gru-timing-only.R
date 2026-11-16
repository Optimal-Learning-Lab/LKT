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
source(file.path(root,'scripts/report-gru-timing-only.R'))
files <- c('scripts/compare-gru-timing-only.R','scripts/report-gru-timing-only.R','scripts/gru-encoder-study.R')
hashes <- setNames(vapply(file.path(root,files),study_hash,character(1)),files)
tasks_for <- function(part)data.frame(run_id=paste(part,'timing',paste0('seed',1:3),sep='__'),
 partition=part,family='augmented',stage='timing',candidate='timing',seed=1:3)
if(action=='plan') {
 stopifnot(length(a)==7L,!dir.exists(output));ref <- a[7];old <- readRDS(file.path(ref,'study.rds'))
 stopifnot(old$total_fits==240L,identical(old$seeds,1:3),identical(old$partitions,paste0('cv',1:5)))
 s <- do.call(GRUModel,old$settings$all)$settings
 columns <- old$features$name[1:4];stopifnot(all(startsWith(columns,'time:')),s$use_lkt_features,
   s$numeric_encoder=='combined',s$response_encoding=='binary',s$retrieval_value_mode=='raw')
 s$numeric_feature_cols <- columns;s$use_lkt_features <- FALSE;s <- do.call(GRUModel,s)$settings
 stopifnot(identical(s[setdiff(names(s),c('numeric_feature_cols','use_lkt_features'))],
   old$settings$all[setdiff(names(s),c('numeric_feature_cols','use_lkt_features'))]))
 dir.create(output,recursive=TRUE)
 for(folder in c('data','partitions','models','validation','training','reports','reference','reference/models'))dir.create(file.path(output,folder))
 required <- c('study.rds','data/prepared_data.rds','reports/summary.csv','reports/auc-by-seed.csv','reports/verification.txt')
 for(k in old$partitions) {
   required <- c(required,paste0('partitions/',k,'.rds'),paste0('reports/',k,'-shard',0:7,'-predictions.csv'),paste0('reports/',k,'-shard',0:7,'-verification.txt'))
   for(condition in c('none','all'))for(seed in 1:3)required <- c(required,paste0('models/',k,'__',condition,'__seed',seed,'.rds'))
 }
 input_hashes <- setNames(vapply(file.path(ref,required),study_hash,character(1)),required)
 stopifnot(input_hashes[['data/prepared_data.rds']]==old$data_sha256,
 file.copy(file.path(ref,'data/prepared_data.rds'),file.path(output,'data/prepared_data.rds')))
 d <- readRDS(file.path(output,'data/prepared_data.rds'))
 for(k in old$partitions) {
   stopifnot(file.copy(file.path(ref,'partitions',paste0(k,'.rds')),file.path(output,'partitions',paste0(k,'.rds'))))
   roles <- readRDS(file.path(output,'partitions',paste0(k,'.rds')))
   stopifnot(!anyDuplicated(unique(data.frame(id=d$Anon.Student.Id,role=roles))$id))
   q <- as.data.frame(data.table::rbindlist(lapply(0:7,function(j)data.table::fread(file.path(ref,'reports',paste0(k,'-shard',j,'-predictions.csv'))))))
   q <- q[q$condition %in% c('none','all'),];study_write_csv(q,file.path(output,'reference',paste0(k,'-predictions.csv')))
   for(condition in c('none','all'))for(seed in 1:3) {
     name <- paste0(k,'__',condition,'__seed',seed,'.rds')
     stopifnot(file.copy(file.path(ref,'models',name),file.path(output,'reference/models',name)))
   }
 }
 for(name in c('summary.csv','auc-by-seed.csv'))stopifnot(file.copy(file.path(ref,'reports',name),file.path(output,'reference',name)))
 p <- list(schema=1L,candidate=candidate,candidate_file_sha256=study_hash(candidate_file),script_hashes=hashes,
   reference_input_hashes=input_hashes,reference_protocol_sha256=input_hashes[['study.rds']],
   data_sha256=old$data_sha256,partitions=old$partitions,partition_hashes=old$partition_hashes,seeds=1:3,
   total_fits=15L,settings=list(timing=s),baseline=old$baseline,
   reference_settings=old$settings[c('none','all')],reference_parameter_counts=old$parameter_counts[c('none','all')],
   intercepts=c(timing=FALSE),parameter_counts=c(timing=43009L),numeric_names=columns,
   feature_labels=old$features$label[1:4],
   design='Fifteen fresh timing-only fits. Exact prior Cloze participant folds and seeds; saved all-seven and no-numeric controls, not refitted.',
   changes='Select four timing inputs and disable LKT feature construction; no intercept, no other architecture or training changes.',
   reporting='Mean training/validation AUC across fifteen restored fits. Pool outer test folds within seed then average three seed AUCs. Canonical held-out replay vectors.',
   limits='Exploratory fixed-budget comparison on previously inspected Cloze. Deleting three inputs removes 96 parameters; no held-out selection or active-release change.')
 generated <- c('reference/summary.csv','reference/auc-by-seed.csv',paste0('reference/',old$partitions,'-predictions.csv'))
 p$reference_generated_hashes <- setNames(vapply(file.path(output,generated),study_hash,character(1)),generated)
 study_write_rds(p,file.path(output,'study.rds'));writeLines(capture.output(dput(p)),file.path(output,'study.txt'))
 cat('FROZEN 15 timing-only fits; 30 saved control fits preserved.\n');quit(save='no')
}
p <- readRDS(file.path(output,'study.rds'))
stopifnot(identical(p$candidate,candidate),study_hash(candidate_file)==p$candidate_file_sha256,
 identical(p$script_hashes,hashes),study_hash(file.path(output,'data/prepared_data.rds'))==p$data_sha256)
for(k in p$partitions)stopifnot(study_hash(file.path(output,'partitions',paste0(k,'.rds')))==p$partition_hashes[[k]])
for(f in names(p$reference_generated_hashes))stopifnot(study_hash(file.path(output,f))==p$reference_generated_hashes[[f]])
for(f in grep('^models/',names(p$reference_input_hashes),value=TRUE))stopifnot(study_hash(file.path(output,'reference',f))==p$reference_input_hashes[[f]])
if(action=='verify') {
 d <- readRDS(file.path(output,'data/prepared_data.rds'));rows <- list()
 for(k in p$partitions) {
  d$fold <- readRDS(file.path(output,'partitions',paste0(k,'.rds')));s <- p$settings$timing
  prep <- getFromNamespace('lkt_gru_preprocess','LKT')(d,d$fold==1,p$baseline,s)
  net <- getFromNamespace('lkt_gru_network','LKT')(prep,s)
  n <- sum(vapply(net$parameters,function(x)as.numeric(x$numel()),numeric(1)))
  stopifnot(identical(prep$numeric_names,p$numeric_names),is.null(prep$engineered),n==43009L)
  enabled <- s;enabled$use_lkt_features <- TRUE
  x <- getFromNamespace('lkt_gru_numeric','LKT')(d,p$baseline,s,fit_rows=d$fold==1)$matrix
  y <- getFromNamespace('lkt_gru_numeric','LKT')(d,p$baseline,enabled,fit_rows=d$fold==1)$matrix
  stopifnot(isTRUE(all.equal(as.matrix(x),as.matrix(y),check.attributes=FALSE)))
  rows[[k]] <- data.frame(partition=k,numeric_inputs=4L,parameters=n,LKT_features=FALSE)
 }
 study_write_csv(do.call(rbind,rows),file.path(output,'reports/preflight.csv'))
 cat('PASS four timing inputs, no engineered schema, exact selected inputs, 43009 parameters on five folds.\n')
}
if(action=='smoke') {
 raw <- readRDS(file.path(output,'data/prepared_data.rds'));idx <- head(which(raw$Anon.Student.Id==raw$Anon.Student.Id[1]),12)
 d <- do.call(rbind,lapply(1:8,function(i){x<-raw[idx,,drop=FALSE];x$Anon.Student.Id<-paste0('synthetic',i);x$CF..ansbin.<-rep(c(0,1,1,0,1,0),2);x$fold<-c(1,1,1,1,1,2,3,3)[i];x}))
 out <- file.path(dirname(output),'smoke');stopifnot(!dir.exists(out))
 for(f in c('models','validation','training'))dir.create(file.path(out,f),recursive=TRUE)
 study_write_rds(p,file.path(out,'study.rds'));s<-p$settings$timing;s$epochs<-1L;s$early_stopping<-FALSE;s$restore_best<-FALSE
 task<-tasks_for('smoke')[1,,drop=FALSE];study_fit_one(task,s,out,p,d,intercept=FALSE)
 fit<-readRDS(file.path(out,'models',paste0(task$run_id,'.rds')))$fit
 stopifnot(!fit$gru_settings$use_lkt_features,is.null(fit$model$preprocessing$engineered),
  identical(fit$model$preprocessing$numeric_names,p$numeric_names),
  max(abs(study_replay(fit,d[d$fold==3,,drop=FALSE])-fit$prediction[d$fold==3]))<3e-6)
 cat('PASS one-epoch timing-only fit and saved-weight replay.\n')
}
if(action=='fit') {
 stopifnot(part %in% p$partitions);d<-readRDS(file.path(output,'data/prepared_data.rds'))
 d$fold<-readRDS(file.path(output,'partitions',paste0(part,'.rds')));tasks<-tasks_for(part)
 workers<-as.integer(Sys.getenv('LKT_STUDY_WORKERS','1'));stopifnot(workers %in% 1:3)
 if(workers==1L)for(i in 1:3)study_fit_one(tasks[i,,drop=FALSE],p$settings$timing,output,p,d,intercept=FALSE) else {
  cl<-parallel::makePSOCKcluster(workers,outfile=file.path(output,paste0(part,'-workers.log')))
  tryCatch({
   parallel::clusterCall(cl,function(root,candidate_file,lib){assign('root',root,envir=.GlobalEnv);source(file.path(root,'scripts/gru-encoder-study.R'));study_require_release(candidate_file,lib);NULL},root,candidate_file,a[6])
   parallel::clusterExport(cl,c('p','d','tasks','output'))
   parallel::parLapplyLB(cl,1:3,function(i)study_fit_one(tasks[i,,drop=FALSE],p$settings$timing,output,p,d,intercept=FALSE))
  },finally=parallel::stopCluster(cl))
 }
 q<-list();test<-d[d$fold==3,,drop=FALSE]
 for(i in 1:3){t<-tasks[i,,drop=FALSE];f<-readRDS(file.path(output,'models',paste0(t$run_id,'.rds')))$fit
  pred<-study_replay(f,test);stopifnot(max(abs(pred-f$prediction[d$fold==3]))<3e-6)
  q[[i]]<-data.frame(partition=part,condition='timing',seed=t$seed,source_row=test$comparison_source_row,participant=test$Anon.Student.Id,observed=test$CF..ansbin.,prediction=pred)}
 study_write_csv(do.call(rbind,q),file.path(output,'reports',paste0(part,'-predictions.csv')))
 writeLines('PASS all three timing-only saved-weight held-out replays.',file.path(output,'reports',paste0(part,'-verification.txt')))
}
if(action %in% c('report','check'))timing_only_report(output,p,action)
