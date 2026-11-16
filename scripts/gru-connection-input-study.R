input_study_fit <- function(task,s,output,p,d) {
 plastic_study_fit(task,s,output,p,d)
 fit<-readRDS(file.path(output,'models',paste0(task$run_id,'.rds')))$fit
 study_write_csv(cbind(task,as.data.frame(connection_study_parameters(fit))),file.path(output,'reports',paste0(task$run_id,'-connection-parameters.csv')))
 w<-fit$model$weights;lam<-as.numeric(w[['gate_fatigue.lambda']]);scale<-as.numeric(w[['gate_fatigue.time_scale']]);name<-as.character(task$candidate)
 idx<-p$full_indices[[name]];stopifnot(length(lam)==length(idx),identical(scale,p$fixed_times[[name]]),digest::digest(scale,algo='sha256')==p$fixed_time_hashes[[name]])
 offset<-0L;full_offset<-0L;records<-list()
 for(matrix in names(w)[grepl('^gru[.].*weight_',names(w))]) {
  shape<-dim(w[[matrix]]);total<-prod(shape);selected<-idx[idx>full_offset & idx<=full_offset+total]
  n<-length(selected)
  if(n) {
   k<-selected-full_offset-1L
   records[[matrix]]<-data.frame(matrix=matrix,destination_row=k%/%shape[2]+1L,source_column=k%%shape[2]+1L,full_connection_index=selected,
    lambda=lam[offset+seq_len(n)],time_scale=scale[offset+seq_len(n)])
   stopifnot(grepl('weight_ih',matrix),all(k%/%shape[2]+1L>2L*shape[1]/3L));offset<-offset+n
  }
  full_offset<-full_offset+total
 }
 clocks<-do.call(rbind,records);stopifnot(offset==length(lam),identical(as.integer(clocks$full_connection_index),as.integer(idx)))
 clocks$recovery_half_life_minutes<-connection_study_parameters(fit)$recovery_half_life_minutes/scale
 study_write_csv(clocks,file.path(output,'reports',paste0(task$run_id,'-connection-clocks.csv')))
}
input_study_report <- function(output,p,data_for,reference) {
 stopifnot(length(list.files(file.path(output,'models'),pattern='[.]rds$'))==30L)
 for(n in names(p$prior_report_hashes))stopifnot(study_hash(file.path(reference,'reports',n))==p$prior_report_hashes[[n]])
 records<-predictions<-list()
 for(i in seq_len(nrow(p$tasks))) {
  task<-p$tasks[i,,drop=FALSE];saved<-readRDS(file.path(output,'models',paste0(task$run_id,'.rds')));fit<-saved$fit
  s<-p$settings[[task$candidate]];s$seed<-as.integer(task$seed);d<-data_for(task$partition);roles<-d$fold
  stopifnot(identical(fit$gru_settings,s),identical(saved$settings,s),identical(fit$fit_rows,which(roles==1L)),identical(fit$validation_rows,which(roles==2L)),
   identical(fit$evaluation_rows,which(roles==3L)),fit$parameter_count==p$parameter_counts[[task$candidate]],fit$selected_epoch==fit$training_history$epoch[which.min(fit$training_history$validation_loss)])
  q<-read.csv(file.path(output,'reports',paste0(task$run_id,'-predictions.csv')))
  stopifnot(identical(q$source_row,d$comparison_source_row[roles==3L]),all(q$observed==d$CF..ansbin.[roles==3L]),max(abs(q$prediction-fit$prediction[roles==3L]))<3e-6)
  metrics<-unlist(lapply(1:3,function(k)study_metrics(d$CF..ansbin.[roles==k],if(k==3L)q$prediction else fit$prediction[roles==k])[c('AUC','log_loss')]),use.names=FALSE)
  names(metrics)<-c('training_AUC','training_log_loss','validation_AUC','validation_log_loss','test_AUC','test_log_loss')
  records[[i]]<-cbind(task,as.data.frame(connection_study_parameters(fit)),as.data.frame(as.list(metrics)),selected_epoch=fit$selected_epoch)
  predictions[[i]]<-q
 }
 fits<-data.table::as.data.table(do.call(rbind,records));pred<-data.table::as.data.table(do.call(rbind,predictions))
 stopifnot(!anyDuplicated(pred[,.(condition,seed,source_row)]),all(pred[,.N,by=.(condition,seed)]$N==55122L))
 pooled<-pred[,as.list(study_metrics(observed,prediction)[c('AUC','log_loss')]),by=.(condition,seed)]
 baseline<-read.csv(file.path(reference,'reports/pooled-by-seed.csv'));baseline$condition<-'candidate_input_and_recurrent'
 all<-rbind(as.data.frame(pooled),baseline[c('condition','seed','AUC','log_loss')]);summary<-aggregate(cbind(AUC,log_loss)~condition,data=all,FUN=mean)
 study_write_csv(fits,file.path(output,'reports/fits.csv'));study_write_csv(pooled,file.path(output,'reports/pooled-by-seed.csv'));study_write_csv(summary,file.path(output,'reports/comparison-summary.csv'))
 old<-read.csv(file.path(reference,'reports/fits.csv'));paired<-merge(as.data.frame(fits),old,by=c('partition','seed'),suffixes=c('_input_only','_candidate_baseline'))
 stopifnot(nrow(paired)==30L);study_write_csv(paired,file.path(output,'reports/paired-with-candidate.csv'))
 jsonlite::write_json(list(complete=TRUE,new_fits=30L,arms=c(input_all=15L,input_h1=15L),connections=c(input_all=4128L,input_h1=2080L)),file.path(output,'reports/verification.json'),auto_unbox=TRUE,pretty=TRUE)
 print(summary)
}
