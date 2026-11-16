item_skill_study_tables <- function(fit) {
 schema<-fit$model$preprocessing$plasticity_item_skill;w<-fit$model$weights
 skill<-as.numeric(w[['plasticity.skill_rate']]);raw<-as.numeric(w[['plasticity.log_item']]);parent<-schema$parent
 multiplier<-exp(raw-ave(raw,parent,FUN=mean));effective<-skill[parent]*multiplier
 stopifnot(length(skill)==length(schema$skills),length(raw)==length(schema$items),all(skill>=0),all(is.finite(multiplier)),all(multiplier>0))
 stopifnot(max(abs(tapply(log(multiplier),parent,mean)))<1e-6)
 list(skills=data.frame(skill=schema$skills,learning_rate=skill),
  items=data.frame(item=schema$items,skill=schema$skills[parent],raw_log_multiplier=raw,item_multiplier=multiplier,effective_learning_rate=effective))
}
item_skill_study_parameters <- function(fit) {
 t<-item_skill_study_tables(fit)
 list(item_count=nrow(t$items),skill_count=nrow(t$skills),skill_rate_min=min(t$skills$learning_rate),skill_rate_median=median(t$skills$learning_rate),skill_rate_max=max(t$skills$learning_rate),
  item_multiplier_min=min(t$items$item_multiplier),item_multiplier_median=median(t$items$item_multiplier),item_multiplier_max=max(t$items$item_multiplier),
  effective_rate_min=min(t$items$effective_learning_rate),effective_rate_median=median(t$items$effective_learning_rate),effective_rate_max=max(t$items$effective_learning_rate))
}
item_skill_study_fit <- function(task,s,output,p,d) {
 plastic_study_fit(task,s,output,p,d)
 fit<-readRDS(file.path(output,'models',paste0(task$run_id,'.rds')))$fit;t<-item_skill_study_tables(fit)
 stopifnot(nrow(t$items)==p$item_count,nrow(t$skills)==p$skill_count)
 for(n in names(t))study_write_csv(t[[n]],file.path(output,'reports',paste0(task$run_id,'-',n,'.csv')))
}
item_skill_study_report <- function(output,p,data_for) {
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
   records[[i]]<-cbind(task,as.data.frame(item_skill_study_parameters(fit)),as.data.frame(as.list(metrics)),selected_epoch=fit$selected_epoch)
   predictions[[i]]<-read.csv(file.path(output,'reports',paste0(task$run_id,'-predictions.csv')))
 }
 pred<-data.table::as.data.table(do.call(rbind,predictions));stopifnot(!anyDuplicated(pred[,.(seed,source_row)]),all(pred[,.N,by=seed]$N==55122L))
 pooled<-pred[,as.list(study_metrics(observed,prediction)[c('AUC','log_loss')]),by=seed]
 study_write_csv(do.call(rbind,records),file.path(output,'reports/fits.csv'));study_write_csv(pooled,file.path(output,'reports/pooled-by-seed.csv'))
 jsonlite::write_json(list(complete=TRUE,new_fits=15L,numeric_inputs=p$numeric_names,output_adaptation='item_skill',formula='v_new=v_old+skill_rate[k]*item_multiplier[j]*(y-p)*z',
   fatigue='disabled'),file.path(output,'reports/verification.json'),auto_unbox=TRUE,pretty=TRUE)
 print(pooled)
}
