connection_study_fit <- function(task,s,output,p,d) {
 plastic_study_fit(task,s,output,p,d)
 fit<-readRDS(file.path(output,'models',paste0(task$run_id,'.rds')))$fit
 study_write_csv(cbind(task,as.data.frame(connection_study_parameters(fit))),file.path(output,'reports',paste0(task$run_id,'-connection-parameters.csv')))
 w<-fit$model$weights;lam<-as.numeric(w[['gate_fatigue.lambda']]);scale<-as.numeric(w[['gate_fatigue.time_scale']]);name<-as.character(task$candidate)
 idx<-p$recurrent_full_indices;stopifnot(length(lam)==length(idx),identical(scale,p$fixed_time_scales),digest::digest(scale,algo='sha256')==p$fixed_time_scale_sha256)
 offset<-0L;full_offset<-0L;records<-list()
 for(matrix in names(w)[grepl('^gru[.].*weight_',names(w))]) {
  shape<-dim(w[[matrix]]);total<-prod(shape);selected<-idx[idx>full_offset & idx<=full_offset+total]
  n<-length(selected)
  if(n) {
   k<-selected-full_offset-1L
   records[[matrix]]<-data.frame(matrix=matrix,destination_row=k%/%shape[2]+1L,source_column=k%%shape[2]+1L,full_connection_index=selected,
    lambda=lam[offset+seq_len(n)],time_scale=scale[offset+seq_len(n)])
   stopifnot(grepl('weight_hh',matrix),all(k%/%shape[2]+1L>2L*shape[1]/3L));offset<-offset+n
  }
  full_offset<-full_offset+total
 }
 clocks<-do.call(rbind,records);stopifnot(offset==length(lam),identical(as.integer(clocks$full_connection_index),as.integer(idx)))
 clocks$recovery_half_life_minutes<-connection_study_parameters(fit)$recovery_half_life_minutes/scale
 study_write_csv(clocks,file.path(output,'reports',paste0(task$run_id,'-connection-clocks.csv')))
}
