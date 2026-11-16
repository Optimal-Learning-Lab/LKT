# ROOT OUTPUT REFERENCE ACTION: initial six-input anonymous-item tracer test.
a<-commandArgs(TRUE);stopifnot(length(a)==4L)
root<-normalizePath(a[1],winslash='/');out<-a[2];reference<-normalizePath(a[3],winslash='/')
action<-match.arg(a[4],c('plan','smoke','fit','report'))
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-encoder-study.R'))
deployment<-jsonlite::fromJSON(Sys.getenv('LKT_RUNTIME_CONFIG'))
supplied_skills<-isTRUE(deployment$local_skill_labels)
f<-function(n)getFromNamespace(n,'LKT')
hash<-function(x)digest::digest(x,algo='sha256')
atomic<-function(x,path) {
 if(file.exists(path))stop('Refusing to overwrite immutable artifact: ',path)
 tmp<-paste0(path,'.tmp');saveRDS(x,tmp);stopifnot(file.rename(tmp,path))
}
scripts<-c('scripts/anonymous-gru-tracer-study.R','scripts/gru-encoder-study.R',
 'scripts/run-anonymous-gru-tracer.py',
 'scripts/load-current-lkt.R','scripts/hpc/run-anonymous-gru-tracer.slurm',
 'scripts/verify-anonymous-gru-tracer.py','scripts/verify-pfa-gru-metrics.py')
script_hashes<-setNames(vapply(file.path(root,scripts),study_hash,character(1)),scripts)
runtime<-function()list(R=R.version.string,platform=R.version$platform,locale=Sys.getlocale(),
 deployment_sha256=study_hash(Sys.getenv('LKT_RUNTIME_CONFIG')),
 packages=setNames(vapply(c('LKT','torch','Matrix'),function(n)as.character(utils::packageVersion(n)),character(1)),c('LKT','torch','Matrix')))
if(action=='plan') {
 stopifnot(!dir.exists(out))
 old<-readRDS(file.path(reference,'study.rds'));d<-readRDS(file.path(reference,'data/prepared_data.rds'))
 stopifnot(nrow(d)==55122L,length(unique(d$Anon.Student.Id))==478L,
  study_hash(file.path(reference,'data/prepared_data.rds'))==old$data_sha256,
  identical(old$baseline$features,c('logitdec','ppes','logitdec')),
  identical(old$baseline$components,c('Anon.Student.Id','KC..Default.','KC..Default.')),
  identical(old$baseline$interc,FALSE))
 s<-do.call(GRUModel,old$settings$constant)$settings
 stopifnot(s$plasticity=='constant',s$embedding_adaptation=='none',
  identical(s$categorical_cols,c('KC..Default.','KC..Cluster.')),is.null(s$time_col),
  s$hidden_size==32L,s$num_layers==3L,s$num_threads==1L)
 # Labels identify repeated items/skills; their vectors never carry pretrained
 # identity information. Supplied grouping retains the baseline skill channel.
 if(!supplied_skills) {
  s$categorical_cols<-'KC..Default.';s$retrieval_filter_cols<-'KC..Default.'
 }
 s$embedding_dims<-16L
 s$embedding_adaptation<-'per_student';s$adaptive_embedding_cols<-s$categorical_cols
 s$embedding_learning_rate<-.01;s$seed<-1L;s$validation_folds<-2L
 s<-do.call(GRUModel,s)$settings
 roles<-readRDS(file.path(reference,'partitions/cv1.rds'))
 stopifnot(study_hash(file.path(reference,'partitions/cv1.rds'))==old$partition_hashes[['cv1']],
  length(roles)==nrow(d),!anyDuplicated(unique(data.frame(student=d$Anon.Student.Id,role=roles))$student))
 for(folder in c('data','models','reports','logs','training'))dir.create(file.path(out,folder),recursive=TRUE,showWarnings=FALSE)
 # Retain only declared observations and, in the explicit skill-label variant,
 # supplied group labels. No fitted identity traits or content enters the model.
 columns<-c('Anon.Student.Id','KC..Default.','CF..Time.','Duration..sec.','CF..ansbin.',
  'comparison_source_row','gru_item_age_seconds','gru_mean_prior_spacing_seconds')
 if(supplied_skills)columns<-c(columns,'KC..Cluster.')
 stopifnot(all(columns%in%names(d)))
 d<-as.data.frame(d)[columns];d$fold<-roles
 if(supplied_skills) {
  mapping<-unique(d[c('KC..Default.','KC..Cluster.')])
  stopifnot(!anyDuplicated(mapping[['KC..Default.']]),!anyNA(mapping),
   all(nzchar(as.character(mapping[['KC..Cluster.']]))))
 }
 for(idx in split(seq_len(nrow(d)),d$Anon.Student.Id))stopifnot(all(diff(d$CF..Time.[idx])>=0))
 atomic(d,file.path(out,'data/prepared_data.rds'))
 p<-list(schema=1L,release=as.list(lkt_release),runtime=runtime(),script_hashes=script_hashes,
  baseline=old$baseline,baseline_parameter_count=unname(old$parameter_counts[['constant']]),
  settings=s,reference_sha256=study_hash(file.path(reference,'study.rds')),
  source_data_sha256=old$data_sha256,data_sha256=study_hash(file.path(out,'data/prepared_data.rds')),
  partition_sha256=old$partition_hashes[['cv1']],partition='cv1',seed=1L,supplied_skills=supplied_skills,
  supplied_skill_mapping=if(supplied_skills)mapping else NULL,
  reset_rule=paste('Every new student and every training-epoch replay starts with zero GRU memory, empty retrieval, zero output adjustments and zero',if(supplied_skills)'item and skill vectors.' else 'item vectors.',
   'Only this student\'s observed answers update local state. Labels give identity/group membership, not pretrained vectors. Shared weights and constant eta learn across training students; evaluation freezes shared weights.'),
  design=paste('Initial AUC test: cv1/seed1, unchanged six-input recipe and validation stopping.',
   if(supplied_skills)'Supplied skill grouping and original retrieval matching retained; item and skill vectors both reset per student.' else 'No supplied skill labels.',
   'No Lasso, simulator integration or default changes.'),
  interpretation=if(supplied_skills)'Exploratory first-fold result. Original categorical channels and dimensions retained, with local item/skill learning instead of pretrained identity vectors.' else
   'Exploratory first-fold result. Removing skill input changes the information available; not an isolated reset ablation.')
 atomic(p,file.path(out,'study.rds'))
 jsonlite::write_json(list(schema=1L,protocol_sha256=study_hash(file.path(out,'study.rds')),
  data_sha256=p$data_sha256,source_data_sha256=p$source_data_sha256,script_hashes=as.list(script_hashes),
  release=p$release,design=p$design,reset_rule=p$reset_rule,supplied_skills=supplied_skills),file.path(out,'protocol.json'),auto_unbox=TRUE,pretty=TRUE)
 cat('FROZEN separate six-input tracer, cv1 seed1, identical training/testing resets\n');quit(save='no')
}
p<-readRDS(file.path(out,'study.rds'));d<-readRDS(file.path(out,'data/prepared_data.rds'))
stopifnot(identical(p$release,as.list(lkt_release)),identical(p$runtime,runtime()),
 identical(p$script_hashes,script_hashes),study_hash(file.path(out,'data/prepared_data.rds'))==p$data_sha256,
 identical(p$supplied_skills,supplied_skills),identical('KC..Cluster.'%in%names(d),supplied_skills))
settings<-p$settings
if(action=='smoke') {
 stopifnot(!file.exists(file.path(out,'smoke.rds')))
 # A bounded pipeline check on the same fold roles, not an AUC result.
 idx<-unlist(lapply(split(seq_len(nrow(d)),d$Anon.Student.Id),head,12L),use.names=FALSE)
 input<-d[sort(idx),,drop=FALSE];settings$epochs<-1L;settings$early_stopping<-FALSE
 settings$restore_best<-FALSE;settings$scheduler<-'none'
} else input<-d
if(action%in%c('smoke','fit')) {
 path<-file.path(out,if(action=='smoke')'smoke.rds' else 'models/cv1__anonymous_constant__seed1.rds')
 if(!file.exists(path)) {
  cat('START',action,format(Sys.time(),tz='UTC'),p$reset_rule,'\n');flush.console()
  elapsed<-system.time(fit<-LKT(input,usefolds=1L,components=p$baseline$components,
   features=p$baseline$features,fixedpars=p$baseline$fixedpars,interc=FALSE,
   model=do.call(GRUModel,settings),verbose=TRUE))[['elapsed']]
  fit$newdata<-NULL;fit$nullmodel<-NULL
  atomic(list(protocol_sha256=study_hash(file.path(out,'study.rds')),settings=settings,fit=fit,
   elapsed=elapsed,job=Sys.getenv('SLURM_JOB_ID'),node=Sys.info()[['nodename']],session=utils::sessionInfo()),path)
 }
 saved<-readRDS(path);fit<-saved$fit
 stopifnot(saved$protocol_sha256==study_hash(file.path(out,'study.rds')),
  identical(saved$settings,settings),length(fit$model$preprocessing$numeric_names)==6L,
  identical(names(fit$model$preprocessing$vocabularies),settings$categorical_cols),
  identical(fit$fit_rows,which(input$fold==1L)),identical(fit$validation_rows,which(input$fold==2L)),
  identical(fit$evaluation_rows,which(input$fold==3L)),all(is.finite(fit$prediction)))
 e<-grep('^embeddings[.]',names(fit$model$weights),value=TRUE)
 stopifnot(length(e)==length(settings$categorical_cols),all(vapply(fit$model$weights[e],function(v)all(v==0),logical(1))))
 if(supplied_skills && action!='smoke')stopifnot(fit$parameter_count==p$baseline_parameter_count)
 if(action=='smoke') {
  row<-input[which(input$fold==3L)[1L],,drop=FALSE]
  start<-LKTGRUState(fit);runtime<-LKTGRURuntime(fit)
  answered<-LKTGRUStep(fit,start,row,runtime)
  personal<-answered$state$adaptive_embeddings$students[[as.character(row$Anon.Student.Id)]]
  for(col in settings$adaptive_embedding_cols) {
   stopifnot(identical(names(personal[[col]]),as.character(row[[col]])),
    any(personal[[col]][[as.character(row[[col]])]]!=0))
  }
  fresh<-row;fresh$Anon.Student.Id<-'fresh-student'
  same<-LKTGRUStep(fit,answered$state,fresh,runtime)
  stopifnot(max(abs(same$predictions-answered$predictions))<3e-6)
  cat('PASS smoke: six inputs, all identity tables zero, item/skill local updates, fresh-student reset, isolated folds\n');quit(save='no')
 }
}
saved<-readRDS(file.path(out,'models/cv1__anonymous_constant__seed1.rds'));fit<-saved$fit
roles<-d$fold;records<-list()
student<-as.character(d$Anon.Student.Id[which(roles==3L)[1L]])
idx<-head(which(roles==3L & as.character(d$Anon.Student.Id)==student),24L)
probe<-d[idx,,drop=FALSE];start<-LKTGRUState(fit);run<-LKTGRURuntime(fit)
weights_before<-serialize(fit$model$weights,NULL)
q<-LKTGRUStep(fit,start,probe,run)
unknown<-probe[rep(1L,2L),,drop=FALSE];unknown$KC..Default.<-c('fresh-item-A','fresh-item-B')
cold<-LKTGRUScore(fit,start,unknown,run)
stopifnot(abs(diff(cold))<3e-6)
renamed<-probe;renamed$Anon.Student.Id<-'unfamiliar-student';renamed$KC..Default.<-paste0('unfamiliar-item-',renamed$KC..Default.)
if(supplied_skills)renamed$KC..Cluster.<-paste0('unfamiliar-skill-',renamed$KC..Cluster.)
r<-LKTGRUStep(fit,start,renamed,run)
split_at<-max(1L,nrow(probe)%/%2L)
first<-LKTGRUStep(fit,start,probe[seq_len(split_at),,drop=FALSE],run)
second<-LKTGRUStep(fit,first$state,probe[-seq_len(split_at),,drop=FALSE],run)
differences<-c(saved=max(abs(q$predictions-fit$prediction[idx])),
 renamed=max(abs(r$predictions-q$predictions)),chunked=max(abs(c(first$predictions,second$predictions)-q$predictions)))
stopifnot(all(differences<3e-6),identical(weights_before,serialize(fit$model$weights,NULL)))
atomic_if_missing<-function(x,path)if(file.exists(path))stopifnot(identical(readRDS(path),x)) else atomic(x,path)
atomic_if_missing(list(maximum_difference=differences,rows=nrow(probe),
 shared_weights_unchanged=TRUE,unfamiliar_student_and_items=TRUE,
 fresh_item_labels_have_equal_initial_predictions=TRUE),file.path(out,'reports/runtime-checks.rds'))
for(k in 1:3) {
 value<-study_metrics(d$CF..ansbin.[roles==k],fit$prediction[roles==k])
 records[[k]]<-data.frame(phase=c('training','validation','heldout')[k],rows=sum(roles==k),
  students=length(unique(d$Anon.Student.Id[roles==k])),AUC=value[['AUC']],log_loss=value[['log_loss']])
}
metrics<-do.call(rbind,records);study_write_csv(metrics,file.path(out,'reports/metrics.csv'))
study_write_csv(data.frame(source_row=d$comparison_source_row,student=d$Anon.Student.Id,role=roles,
 observed=d$CF..ansbin.,prediction=fit$prediction),file.path(out,'reports/predictions.csv'))
study_write_csv(fit$training_history,file.path(out,'training/cv1__anonymous_constant__seed1.csv'))
stopifnot(identical(fit$gru_settings$plasticity,'constant'))
writeLines(c(p$design,p$reset_rule,paste('Selected epoch:',fit$selected_epoch),
 paste('Trained epochs:',fit$trained_epochs),paste('Learned constant output rate:',as.numeric(fit$model$weights[['plasticity.eta']]))),
 file.path(out,'reports/design-and-fit.txt'))
print(metrics);cat('COMPLETE initial tracer fit\n')
