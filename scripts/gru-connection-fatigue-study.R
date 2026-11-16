connection_study_parameters <- function(fit) {
 w<-fit$model$weights;lam<-as.numeric(w[['gate_fatigue.lambda']]);rho<-exp(as.numeric(w[['gate_fatigue.log_rho']]))
 list(output_eta=as.numeric(w[['plasticity.eta']]),recovery_rate_per_minute=rho,recovery_half_life_minutes=log(2)/rho,
   connection_count=length(lam),lambda_min=min(lam),lambda_median=median(lam),lambda_max=max(lam),lambda_fraction_below_one=mean(lam<1))
}
connection_study_fit <- function(task,s,output,p,d) {
 plastic_study_fit(task,s,output,p,d)
 fit<-readRDS(file.path(output,'models',paste0(task$run_id,'.rds')))$fit
 study_write_csv(cbind(task,as.data.frame(connection_study_parameters(fit))),file.path(output,'reports',paste0(task$run_id,'-connection-parameters.csv')))
 w<-fit$model$weights;lam<-as.numeric(w[['gate_fatigue.lambda']]);offset<-0L;records<-list()
 for(name in names(w)[grepl('^gru[.].*weight_',names(w))]) {
   shape<-dim(w[[name]]);n<-prod(shape);k<-seq_len(n)-1L
   records[[name]]<-data.frame(matrix=name,destination_row=k%/%shape[2]+1L,source_column=k%%shape[2]+1L,
     lambda=lam[offset+seq_len(n)])
   offset<-offset+n
 }
 stopifnot(offset==length(lam));study_write_csv(do.call(rbind,records),file.path(output,'reports',paste0(task$run_id,'-lambdas.csv')))
}
connection_study_report <- function(output,p,data_for) {
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
   records[[i]]<-cbind(task,as.data.frame(connection_study_parameters(fit)),as.data.frame(as.list(metrics)),selected_epoch=fit$selected_epoch)
   predictions[[i]]<-read.csv(file.path(output,'reports',paste0(task$run_id,'-predictions.csv')))
 }
 pred<-data.table::as.data.table(do.call(rbind,predictions));stopifnot(!anyDuplicated(pred[,.(seed,source_row)]),all(pred[,.N,by=seed]$N==55122L))
 pooled<-pred[,as.list(study_metrics(observed,prediction)[c('AUC','log_loss')]),by=seed]
 study_write_csv(do.call(rbind,records),file.path(output,'reports/fits.csv'));study_write_csv(pooled,file.path(output,'reports/pooled-by-seed.csv'))
 jsonlite::write_json(list(complete=TRUE,new_fits=15L,numeric_inputs=p$numeric_names,output_adaptation='constant',
   fatigue='per-connection lambda; recover then use weights then fatigue'),file.path(output,'reports/verification.json'),auto_unbox=TRUE,pretty=TRUE)
 print(pooled)
}
