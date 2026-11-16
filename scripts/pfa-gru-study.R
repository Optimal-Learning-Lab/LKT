# ROOT OUTPUT plan|fit|report TASK CANDIDATE_JSON LIBRARY BASELINE SPEC_JSON
# One frozen independent mechanism or one shared control; exactly five folds x three seeds.
a<-commandArgs(TRUE);stopifnot(length(a)==8L,a[3]%in%c('plan','fit','report'))
root<-normalizePath(a[1],winslash='/',mustWork=TRUE);out<-a[2];action<-a[3]
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
source(file.path(root,'scripts/gru-encoder-study.R'))
source(file.path(root,'scripts/verify-pfa-exercise-attributes.R'))
hash<-study_hash;internal<-function(n)getFromNamespace(n,'LKT')
spec<-jsonlite::fromJSON(a[8],simplifyVector=FALSE)
stopifnot(is.character(spec$name),grepl('^[a-z][a-z0-9_-]*$',spec$name),
 spec$kind%in%c('mechanism','shared_control'),is.list(spec$overrides),
 length(spec$expected_parameter_count)==1L,spec$expected_parameter_count>0L)
# The general threshold remains 8:10. A score-seven paper requires an explicit,
# hash-bound user selection of this exact paper, mechanism and reviewed source.
user_selection<-NULL
if(spec$kind=='mechanism') {
 stopifnot(length(spec$score)==1L,spec$score%in%7:10)
 if(spec$score==7L) {
  stopifnot(is.character(spec$user_selection_file),length(spec$user_selection_file)==1L,
   is.character(spec$user_selection_sha256),length(spec$user_selection_sha256)==1L,
   grepl('^[a-f0-9]{64}$',spec$user_selection_sha256),
   is.character(spec$original_review_file),length(spec$original_review_file)==1L)
  selection_file<-normalizePath(spec$user_selection_file,winslash='/',mustWork=TRUE)
  stopifnot(identical(hash(selection_file),spec$user_selection_sha256))
  selected<-jsonlite::fromJSON(selection_file,simplifyVector=FALSE)
  entries<-Filter(function(x)identical(x$paper_id,spec$paper_id),selected$selected_papers)
  stopifnot(length(entries)==1L)
  entry<-entries[[1L]]
  stopifnot(identical(entry$original_score,7L),
   identical(entry$mechanism_id,spec$mechanism_family),
   identical(hash(spec$original_review_file),entry$review_sha256))
  user_selection<-list(file=selection_file,sha256=spec$user_selection_sha256,paper=entry)
 }
}
baseline<-readRDS(file.path(a[7],'study.rds'))
stopifnot(baseline$total_fits==15L,nrow(baseline$tasks)==15L,
 all(baseline$tasks$candidate=='constant'),setequal(baseline$tasks$seed,1:3),
 setequal(baseline$tasks$partition,paste0('cv',1:5)),
 all(table(baseline$tasks$partition,baseline$tasks$seed)==1L))
script_files<-c('scripts/pfa-gru-study.R','scripts/gru-encoder-study.R',
 'scripts/run-pfa-gru-batch.py','scripts/finalize-pfa-gru-study.py','scripts/verify-pfa-gru-metrics.py',
 'scripts/verify-pfa-exercise-attributes.R')
script_hashes<-setNames(vapply(file.path(root,script_files),hash,character(1)),script_files)
stopifnot(is.character(spec$runtime_provenance_file),length(spec$runtime_provenance_file)==1L)
runtime_file<-normalizePath(spec$runtime_provenance_file,winslash='/',mustWork=TRUE)
runtime_sha256<-hash(runtime_file);runtime_record<-jsonlite::fromJSON(runtime_file)
policy<-runtime_record$runtime_policy
stopifnot(identical(runtime_record$schema,2L),
 identical(runtime_record$execution_host,'Big Blue'),identical(runtime_record$scheduler,'slurm'),
 is.numeric(policy$maximum_concurrent_research_fits),length(policy$maximum_concurrent_research_fits)==1L,
 policy$maximum_concurrent_research_fits%in%1:15,
 is.numeric(policy$allocated_cpus),length(policy$allocated_cpus)==1L,
 policy$allocated_cpus==as.integer(policy$allocated_cpus),
 policy$allocated_cpus>=policy$maximum_concurrent_research_fits,
 identical(policy$torch_threads_per_fit,1L),is.list(policy$environment))
slurm_cpus<-Sys.getenv('SLURM_CPUS_PER_TASK',unset='')
stopifnot(grepl('^[0-9]+$',slurm_cpus),as.integer(slurm_cpus)>=policy$allocated_cpus,
 setequal(names(policy$environment),c('LC_ALL','LANG','LANGUAGE','R_TESTS',
  'OMP_NUM_THREADS','MKL_NUM_THREADS','OPENBLAS_NUM_THREADS')),
 all(vapply(policy$environment,function(x)is.character(x)&&length(x)==1L,logical(1))),
 all(nzchar(unlist(policy$environment[c('LC_ALL','LANG','LANGUAGE')]))),
 identical(policy$environment$R_TESTS,''),
 all(unlist(policy$environment[c('OMP_NUM_THREADS','MKL_NUM_THREADS','OPENBLAS_NUM_THREADS')])=='1'))
for(n in names(runtime_record$runtime_policy$environment))stopifnot(
 identical(Sys.getenv(n,unset=NA_character_),runtime_record$runtime_policy$environment[[n]]))
candidate<-study_require_release(a[5],a[6])
gate<-jsonlite::fromJSON(spec$readiness_file)
stopifnot(identical(gate$runtime_provenance_sha256,runtime_sha256),
 identical(runtime_record$candidate,candidate),identical(R.version.string,runtime_record$r_version),
 identical(R.version$platform,runtime_record$platform),identical(Sys.getlocale(),runtime_record$locale),
 identical(normalizePath(.libPaths(),winslash='/'),normalizePath(runtime_record$library_paths,winslash='/')))
for(n in names(runtime_record$package_versions))stopifnot(
 identical(as.character(utils::packageVersion(n)),runtime_record$package_versions[[n]]))

is_eakt<-identical(spec$paper_id,'pfa-90bae1222bc1') || !is.null(spec$overrides$exercise_attributes)
exercise_preflight<-function(settings,source_file) {
 if(is_eakt || !is.null(settings$exercise_attributes))pfa_verify_exercise_attributes(spec,settings,source_file) else NULL
}

replay<-function(fit,d) {
 runtime<-LKTGRURuntime(fit);s<-runtime$settings
 cleanup<-internal('lkt_gru_runtime')(s);on.exit(cleanup())
 ord<-internal('lkt_gru_order')(d,s);ordered<-d[ord,,drop=FALSE]
 encoded<-internal('lkt_gru_encode')(ordered,fit$model$preprocessing,fit$model$recipe,s)
 internal('lkt_gru_replay')(runtime$network,encoded,ordered,s)[order(ord)]
}
if(action=='plan') {
 stopifnot(!dir.exists(out))
 gate<-jsonlite::fromJSON(spec$readiness_file)
 stopifnot(isTRUE(gate$complete),isTRUE(gate$implementation_verified),
  isTRUE(gate$required_study_checks_verified),isTRUE(gate$control_decision_complete))
 if(spec$kind=='mechanism')stopifnot(isTRUE(gate$queue_order_verified))
 if(spec$kind=='shared_control')stopifnot(identical(gate$reuse_historical_controls,FALSE),length(spec$overrides)==0L)
 resolved_base<-do.call(GRUModel,baseline$settings$constant)$settings
 settings<-do.call(GRUModel,utils::modifyList(resolved_base,spec$overrides,keep.null=TRUE))$settings
 frozen_controls<-c('epochs','batch_size','sequence_length','optimizer','learning_rate','weight_decay',
  'optimizer_options','gradient_clip','gradient_clip_norm','positive_weight','weight_col',
  'validation_folds','validation_fraction','split_seed','early_stopping','patience','min_delta',
  'restore_best','scheduler','scheduler_options','input_dropout','gru_dropout','head_dropout',
  'encoder_dropout','retrieval_dropout','device','dtype','deterministic','num_threads',
  'hidden_size','num_layers','head_hidden_sizes','categorical_cols','numeric_cols','numeric_feature_cols',
  'time_col','time_feature_cols','time_missing_cols','numeric_transforms','use_lkt_features',
  'initial_model_file','additional_input_init')
 stopifnot(identical(settings[frozen_controls],resolved_base[frozen_controls]),
  settings$num_threads==runtime_record$runtime_policy$torch_threads_per_fit,
  settings$device==runtime_record$runtime_policy$device,settings$dtype==runtime_record$runtime_policy$dtype,
  identical(settings$deterministic,runtime_record$runtime_policy$deterministic))
 eakt_verification<-exercise_preflight(settings,file.path(a[7],'data/prepared_data.rds'))
 tasks<-baseline$tasks;tasks$candidate<-spec$name
 stopifnot(all(grepl('__constant__',tasks$run_id,fixed=TRUE)))
 tasks$run_id<-gsub('__constant__',paste0('__',spec$name,'__'),tasks$run_id,fixed=TRUE)
 dir.create(out,recursive=TRUE)
 for(n in c('data','partitions','models','training','reports'))dir.create(file.path(out,n))
 stopifnot(file.copy(file.path(a[7],'data/prepared_data.rds'),file.path(out,'data/prepared_data.rds')))
 for(part in unique(tasks$partition))stopifnot(file.copy(file.path(a[7],'partitions',paste0(part,'.rds')),
  file.path(out,'partitions',paste0(part,'.rds'))))
 p<-list(schema=2L,condition=spec$name,kind=spec$kind,spec=spec,candidate=candidate,source_root=root,
  user_selection=user_selection,exercise_attributes_verification=eakt_verification,
  runtime_provenance_file=runtime_file,runtime_provenance_sha256=runtime_sha256,
  total_fits=15L,tasks=tasks,settings=settings,baseline_recipe=baseline$baseline,
  original_baseline_protocol_sha256=hash(file.path(a[7],'study.rds')),
  candidate_file_sha256=hash(a[5]),spec_file_sha256=hash(a[8]),
  readiness_sha256=hash(spec$readiness_file),script_hashes=script_hashes,
  data_sha256=hash(file.path(out,'data/prepared_data.rds')),
  partition_hashes=setNames(vapply(unique(tasks$partition),function(part)
   hash(file.path(out,'partitions',paste0(part,'.rds'))),character(1)),unique(tasks$partition)),
  checkpoint_selection='Minimum validation log loss; the baseline architecture was selected separately by mean validation AUC.',
  interpretation='Exploratory comparison on repeatedly used Cloze folds. One mechanism only; no tuning grid or default promotion.',
  shared_upstream_preparation='Fixed outcome-independent text encoding is separate from the15 downstream research fits.',
  initialization='Fresh network per frozen seed; no historical warm start. The extra semantic projection and expanded recurrent matrix consume additional RNG draws; common weights are not claimed to be identical across architectures.')
 study_write_rds(p,file.path(out,'study.rds'));study_write_csv(tasks,file.path(out,'tasks.csv'))
 writeLines(capture.output(dput(p)),file.path(out,'study.txt'))
 jsonlite::write_json(list(schema=2L,condition=spec$name,kind=spec$kind,total_fits=15L,
  user_selection=user_selection,exercise_attributes_verification=eakt_verification,
  protocol_sha256=hash(file.path(out,'study.rds')),
  original_baseline_protocol_sha256=p$original_baseline_protocol_sha256,
  data_sha256=p$data_sha256,partition_hashes=as.list(p$partition_hashes),
  source_root=root,runtime_provenance_file=runtime_file,runtime_provenance_sha256=runtime_sha256,
  candidate_source_sha256=candidate$source_sha256,
  frozen_settings=settings[frozen_controls],readiness=gate,
  script_hashes=as.list(script_hashes),initialization=p$initialization,spec=spec),file.path(out,'protocol.json'),
  auto_unbox=TRUE,pretty=TRUE,null='null',digits=17)
 cat('FROZEN exactly15 fits:',spec$name,'\n');quit(save='no')
}
p<-readRDS(file.path(out,'study.rds'))
stopifnot(identical(p$schema,2L),identical(p$user_selection,user_selection),
 identical(p$candidate,candidate),identical(p$source_root,root),
 identical(p$runtime_provenance_file,runtime_file),identical(p$runtime_provenance_sha256,runtime_sha256),
 p$total_fits==15L,nrow(p$tasks)==15L,
 hash(a[5])==p$candidate_file_sha256,hash(a[8])==p$spec_file_sha256,
 hash(spec$readiness_file)==p$readiness_sha256,identical(script_hashes,p$script_hashes),
 hash(file.path(a[7],'study.rds'))==p$original_baseline_protocol_sha256,
 hash(file.path(out,'data/prepared_data.rds'))==p$data_sha256)
for(part in names(p$partition_hashes))stopifnot(hash(file.path(out,'partitions',paste0(part,'.rds')))==p$partition_hashes[[part]])
eakt_verification<-exercise_preflight(p$settings,file.path(out,'data/prepared_data.rds'))
stopifnot(identical(p$exercise_attributes_verification,eakt_verification))
d<-readRDS(file.path(out,'data/prepared_data.rds'))
fit_path<-function(task)file.path(out,'models',paste0(task$run_id,'.rds'))
if(action=='fit') {
 id<-as.integer(a[4]);stopifnot(length(id)==1L,!is.na(id),id>=0L,id<15L)
 task<-p$tasks[id+1L,,drop=FALSE];s<-p$settings;s$seed<-as.integer(task$seed)
 d$fold<-readRDS(file.path(out,'partitions',paste0(task$partition,'.rds')))
 for(indices in split(seq_len(nrow(d)),d$Anon.Student.Id))stopifnot(length(unique(d$fold[indices]))==1L)
 fingerprint<-digest::digest(list(study=hash(file.path(out,'study.rds')),task=task,settings=s),algo='sha256')
 path<-fit_path(task)
 if(!file.exists(path)) {
  r<-p$baseline_recipe;cat('START',task$run_id,'\n');flush.console()
  elapsed<-system.time(fit<-LKT(d,usefolds=1,components=r$components,features=r$features,
   fixedpars=r$fixedpars,interc=r$interc,interacts=r$interacts,connectors=r$connectors,
   maketimes=r$maketimes,model=do.call(GRUModel,s),verbose=TRUE))[['elapsed']]
  fit$newdata<-NULL;fit$nullmodel<-NULL
  study_write_rds(list(fingerprint=fingerprint,task=task,settings=s,fit=fit,elapsed=elapsed),path)
 }
 saved<-readRDS(path);fit<-saved$fit
 stopifnot(identical(saved$fingerprint,fingerprint),identical(saved$settings,s),
  identical(fit$fit_rows,which(d$fold==1L)),identical(fit$validation_rows,which(d$fold==2L)),
  identical(fit$evaluation_rows,which(d$fold==3L)),fit$parameter_count==spec$expected_parameter_count,
  all(is.finite(unlist(fit$model$weights))),
  fit$selected_epoch==fit$training_history$epoch[which.min(fit$training_history$validation_loss)])
 q<-replay(fit,d);difference<-max(abs(q-fit$prediction));stopifnot(is.finite(difference),difference<3e-6)
 study_write_csv(data.frame(partition=task$partition,condition=task$candidate,seed=task$seed,
  source_row=d$comparison_source_row,participant=d$Anon.Student.Id,role=d$fold,
  observed=d$CF..ansbin.,prediction=q),file.path(out,'reports',paste0(task$run_id,'-predictions.csv')))
 study_write_csv(fit$training_history,file.path(out,'training',paste0(task$run_id,'.csv')))
 jsonlite::write_json(list(complete=TRUE,run_id=task$run_id,fit_sha256=hash(path),
  maximum_replay_probability_difference=difference,expected_parameters=spec$expected_parameter_count),
  file.path(out,'reports',paste0(task$run_id,'-verification.json')),auto_unbox=TRUE,pretty=TRUE,digits=17)
 cat('DONE',task$run_id,'\n');quit(save='no')
}
stopifnot(length(list.files(file.path(out,'models'),pattern='[.]rds$'))==15L)
records<-predictions<-list()
for(i in 1:15) {
 task<-p$tasks[i,,drop=FALSE];saved<-readRDS(fit_path(task));fit<-saved$fit
 s<-p$settings;s$seed<-as.integer(task$seed)
 roles<-readRDS(file.path(out,'partitions',paste0(task$partition,'.rds')))
 stopifnot(identical(saved$settings,s),identical(fit$gru_settings,s),
  identical(fit$fit_rows,which(roles==1L)),identical(fit$validation_rows,which(roles==2L)),
  identical(fit$evaluation_rows,which(roles==3L)),fit$parameter_count==spec$expected_parameter_count)
 check<-jsonlite::fromJSON(file.path(out,'reports',paste0(task$run_id,'-verification.json')))
 stopifnot(isTRUE(check$complete),check$fit_sha256==hash(fit_path(task)))
 q<-read.csv(file.path(out,'reports',paste0(task$run_id,'-predictions.csv')))
 stopifnot(nrow(q)==nrow(d),identical(q$role,roles),
  identical(q$source_row,d$comparison_source_row),all(q$observed==d$CF..ansbin.),
  all(q$participant==d$Anon.Student.Id),all(is.finite(q$prediction)))
 m<-unlist(lapply(1:3,function(k) {
  y<-q$observed[roles==k];pr<-q$prediction[roles==k]
  value<-study_metrics(y,pr)[c('AUC','log_loss')]
  stopifnot(abs(value[1]-as.numeric(pROC::auc(y,pr,quiet=TRUE,direction='<')))<1e-12)
  value
 }),use.names=FALSE)
 names(m)<-c('training_AUC','training_log_loss','validation_AUC','validation_log_loss','heldout_AUC','heldout_log_loss')
 records[[i]]<-cbind(task,as.data.frame(as.list(m)),parameter_count=fit$parameter_count,
  elapsed_seconds=saved$elapsed,epochs_run=fit$trained_epochs,selected_epoch=fit$selected_epoch,
  maximum_replay_probability_difference=check$maximum_replay_probability_difference)
 predictions[[i]]<-q[q$role==3L,,drop=FALSE]
}
fits<-do.call(rbind,records);held<-data.table::as.data.table(do.call(rbind,predictions))
stopifnot(!anyDuplicated(held[,.(seed,source_row)]),all(held[,.N,by=seed]$N==nrow(d)))
pooled<-held[,as.list(study_metrics(observed,prediction)[c('AUC','log_loss')]),by=seed]
summary<-data.frame(condition=p$condition,training_AUC=mean(fits$training_AUC),
 validation_AUC=mean(fits$validation_AUC),heldout_AUC=mean(pooled$AUC),
 heldout_log_loss=mean(pooled$log_loss),parameter_count=unique(fits$parameter_count),
 fit_seconds=sum(fits$elapsed_seconds))
study_write_csv(fits,file.path(out,'reports/fits.csv'))
study_write_csv(as.data.frame(pooled),file.path(out,'reports/pooled-by-seed.csv'))
study_write_csv(summary,file.path(out,'reports/summary.csv'))
jsonlite::write_json(list(complete=FALSE,fits_complete=TRUE,fits=15L,phase_metric_pairs_checked=45L,
 independent_python_auc_pending=TRUE,paired_control_comparison_pending=TRUE,
 protocol_sha256=hash(file.path(out,'study.rds'))),file.path(out,'reports/verification.json'),
 auto_unbox=TRUE,pretty=TRUE)
print(summary);print(pooled)
