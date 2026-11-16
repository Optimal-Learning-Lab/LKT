# This study adds two conditions; the prior study helpers/artifacts stay frozen.
gate_study_parameters <- function(fit) {
 w<-fit$model$weights;kappa<-as.numeric(w[['gate_fatigue.kappa']]);rho<-exp(as.numeric(w[['gate_fatigue.log_rho']]))
 list(gate_fatigue_strength=kappa,gate_recovery_rate_per_minute=rho,gate_recovery_half_life_minutes=log(2)/rho,
      gate_half_life_at_full_write_events=if(kappa>0)log(2)/kappa else Inf)
}
gate_study_fit <- function(task,s,output,p,d) {
 plastic_study_fit(task,s,output,p,d)
 fit<-readRDS(file.path(output,'models',paste0(task$run_id,'.rds')))$fit
 study_write_csv(cbind(task,as.data.frame(gate_study_parameters(fit))),file.path(output,'reports',paste0(task$run_id,'-gate-parameters.csv')))
}
gate_study_report <- function(output,p,action) {
 stopifnot(length(list.files(file.path(output,'models'),pattern='[.]rds$'))==30L)
 d<-readRDS(file.path(output,'data/prepared_data.rds'));records<-predictions<-list()
 tasks<-rbind(p$controls,p$tasks)
 for(i in seq_len(nrow(tasks))) {
   task<-tasks[i,,drop=FALSE];id<-task$run_id;reused<-task$candidate%in%c('stock','constant')
   folder<-if(reused)file.path(output,'controls') else output
   saved<-readRDS(file.path(folder,'models',paste0(id,'.rds')));fit<-saved$fit
   roles<-readRDS(file.path(output,'partitions',paste0(task$partition,'.rds')))
   stopifnot(identical(fit$fit_rows,which(roles==1L)),identical(fit$validation_rows,which(roles==2L)),identical(fit$evaluation_rows,which(roles==3L)),
     fit$selected_epoch==fit$training_history$epoch[which.min(fit$training_history$validation_loss)])
   for(n in c('components','features','fixedpars','interc','maketimes','connectors'))stopifnot(identical(fit$model$recipe[[n]],p$baseline[[n]]))
   if(!reused) {
     s<-p$settings[[task$candidate]];s$seed<-as.integer(task$seed)
     # Compare task values, ignoring row labels inherited from a combined report.
     stopifnot(identical(saved$settings,s),identical(fit$gru_settings,s),fit$parameter_count==p$parameter_counts[[task$candidate]],
       identical(saved$fingerprint,digest::digest(list(study=study_hash(file.path(output,'study.rds')),task=saved$task,settings=s),algo='sha256')),
       isTRUE(all.equal(saved$task,task,check.attributes=FALSE)))
   }
   q<-read.csv(file.path(folder,'reports',paste0(id,'-predictions.csv')))
   stopifnot(!anyNA(q),!anyDuplicated(q$source_row),identical(q$source_row,d$comparison_source_row[roles==3L]),
     all(q$observed==d$CF..ansbin.[roles==3L]),all(q$participant==d$Anon.Student.Id[roles==3L]),max(abs(q$prediction-fit$prediction[roles==3L]))<3e-6)
   metrics<-unlist(lapply(1:3,function(k) {
     y<-d$CF..ansbin.[roles==k];v<-if(k==3L)q$prediction else fit$prediction[roles==k]
     m<-study_metrics(y,v)[c('AUC','log_loss')]
     stopifnot(abs(m[1]-as.numeric(pROC::auc(y,v,quiet=TRUE,direction='<')))<1e-12,abs(m[2]+mean(dbinom(y,1,v,log=TRUE)))<1e-12);m
   }),use.names=FALSE)
   names(metrics)<-c('training_AUC','training_log_loss','validation_AUC','validation_log_loss','test_AUC','test_log_loss')
   pars<-if(!reused)gate_study_parameters(fit) else setNames(as.list(rep(NA_real_,4)),c('gate_fatigue_strength','gate_recovery_rate_per_minute','gate_recovery_half_life_minutes','gate_half_life_at_full_write_events'))
   records[[i]]<-cbind(task,reused=reused,as.data.frame(plastic_study_parameters(fit)),as.data.frame(pars),as.data.frame(as.list(metrics)),
     selected_epoch=fit$selected_epoch,epochs_run=fit$trained_epochs,elapsed_seconds=saved$elapsed)
   predictions[[i]]<-q
 }
 fits<-data.table::as.data.table(do.call(rbind,records));pred<-data.table::as.data.table(do.call(rbind,predictions))
 stopifnot(!anyDuplicated(pred[,.(condition,seed,source_row)]),all(pred[,.N,by=.(condition,seed)]$N==nrow(d)))
 pooled<-pred[,as.list(study_metrics(observed,prediction)[c('AUC','log_loss')]),by=.(condition,seed)]
 summary<-pooled[,.(heldout_AUC=mean(AUC),heldout_log_loss=mean(log_loss)),by=condition]
 pairs<-list()
 for(pair in list(c('stock','gate'),c('constant','gate_constant'),c('gate','gate_constant'),c('stock','constant'))) {
   a<-fits[candidate==pair[1]];b<-fits[candidate==pair[2]]
   q<-merge(as.data.frame(a),as.data.frame(b),by=c('partition','seed'),suffixes=c('_baseline','_extension'))
   pairs[[length(pairs)+1L]]<-data.frame(comparison=paste(pair,collapse='_to_'),partition=q$partition,seed=q$seed,
     training_log_loss_improvement=q$training_log_loss_baseline-q$training_log_loss_extension,
     test_AUC_gain=q$test_AUC_extension-q$test_AUC_baseline,test_log_loss_improvement=q$test_log_loss_baseline-q$test_log_loss_extension)
 }
 tables<-list(fits=fits,pooled_by_seed=pooled,summary=summary,paired_contrasts=do.call(rbind,pairs))
 for(n in names(tables)) {
   path<-file.path(output,'reports',paste0(gsub('_','-',n),'.csv'));expected<-as.data.frame(tables[[n]])
   if(action=='report')study_write_csv(expected,path) else {
     observed<-as.data.frame(data.table::fread(path))
     for(k in names(expected)[vapply(expected,is.numeric,logical(1))])observed[[k]]<-as.numeric(observed[[k]])
     stopifnot(isTRUE(all.equal(observed,expected,tolerance=1e-12,check.attributes=FALSE)))
   }
 }
 if(action=='check')jsonlite::write_json(list(complete=TRUE,new_fits=30L,reused_fits=30L,phase_metric_pairs=180L,
   protocol_sha256=study_hash(file.path(output,'study.rds'))),file.path(output,'reports/verification.json'),auto_unbox=TRUE,pretty=TRUE)
 print(summary);invisible(tables)
}
