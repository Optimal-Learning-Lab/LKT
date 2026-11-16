# Matched signed error-magnitude output study. Recurrent fatigue helper is unchanged.
error_rate_study_parameters <- function(fit) {
 w<-fit$model$weights;eta<-as.numeric(w[['plasticity.eta']]);alpha<-as.numeric(w[['plasticity.alpha']]);cc<-as.numeric(w[['plasticity.c']])
 stopifnot(length(eta)==1L,length(alpha)==1L,length(cc)==1L,all(is.finite(c(eta,alpha,cc))),all(c(eta,alpha,cc)>=0))
 c(connection_study_parameters(fit),list(output_alpha=alpha,output_c=cc,error_threshold=if(alpha>0)cc/alpha else NA_real_,
   rate_change_error_zero=-cc,rate_change_error_one=alpha-cc))
}
error_rate_study_fit <- function(task,s,output,p,d) {
 connection_study_fit(task,s,output,p,d)
 fit<-readRDS(file.path(output,'models',paste0(task$run_id,'.rds')))$fit
 study_write_csv(cbind(task,as.data.frame(error_rate_study_parameters(fit))),file.path(output,'reports',paste0(task$run_id,'-output-parameters.csv')))
}
error_rate_study_report <- function(output,p,data_for) {
 stopifnot(length(list.files(file.path(output,'models'),pattern='[.]rds$'))==15L)
 records<-predictions<-list()
 for(i in seq_len(nrow(p$tasks))) {
   task<-p$tasks[i,,drop=FALSE];saved<-readRDS(file.path(output,'models',paste0(task$run_id,'.rds')));fit<-saved$fit
   s<-p$settings[[1]];s$seed<-as.integer(task$seed);d<-data_for(task$partition);roles<-d$fold
   stopifnot(identical(saved$settings,s),identical(fit$gru_settings,s),identical(fit$fit_rows,which(roles==1L)),
     identical(fit$validation_rows,which(roles==2L)),identical(fit$evaluation_rows,which(roles==3L)),
     fit$parameter_count==p$parameter_counts[[1]],fit$selected_epoch==fit$training_history$epoch[which.min(fit$training_history$validation_loss)])
   metrics<-unlist(lapply(1:3,function(k)study_metrics(d$CF..ansbin.[roles==k],fit$prediction[roles==k])[c('AUC','log_loss')]),use.names=FALSE)
   names(metrics)<-c('training_AUC','training_log_loss','validation_AUC','validation_log_loss','test_AUC','test_log_loss')
   records[[i]]<-cbind(task,as.data.frame(error_rate_study_parameters(fit)),as.data.frame(as.list(metrics)),selected_epoch=fit$selected_epoch)
   predictions[[i]]<-read.csv(file.path(output,'reports',paste0(task$run_id,'-predictions.csv')))
 }
 pred<-data.table::as.data.table(do.call(rbind,predictions));stopifnot(!anyDuplicated(pred[,.(seed,source_row)]),all(pred[,.N,by=seed]$N==55122L))
 pooled<-pred[,as.list(study_metrics(observed,prediction)[c('AUC','log_loss')]),by=seed]
 study_write_csv(do.call(rbind,records),file.path(output,'reports/fits.csv'));study_write_csv(pooled,file.path(output,'reports/pooled-by-seed.csv'))
 jsonlite::write_json(list(complete=TRUE,new_fits=15L,numeric_inputs=p$numeric_names,output_adaptation='evolving_student_rate',formula='v_new=v_old+eta_student*(y-p)*z; eta_student_new=eta_student+alpha*abs(y-p)-c',initial_baseline_eta=0,initial_alpha=0,initial_c=0,
   fatigue='per-connection lambda; recover then use weights then fatigue'),file.path(output,'reports/verification.json'),auto_unbox=TRUE,pretty=TRUE)
 print(pooled)
}
