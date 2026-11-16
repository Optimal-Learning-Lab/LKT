# Candidate-only reporting keeps exact physical matrix indices and clock mapping.
connection_study_fit <- function(task,s,output,p,d) {
 plastic_study_fit(task,s,output,p,d)
 fit<-readRDS(file.path(output,'models',paste0(task$run_id,'.rds')))$fit
 study_write_csv(cbind(task,as.data.frame(connection_study_parameters(fit))),file.path(output,'reports',paste0(task$run_id,'-connection-parameters.csv')))
 w<-fit$model$weights;lam<-as.numeric(w[['gate_fatigue.lambda']]);scale<-as.numeric(w[['gate_fatigue.time_scale']])
 stopifnot(length(lam)==length(p$fixed_time_scales),identical(scale,p$fixed_time_scales),
   digest::digest(scale,algo='sha256')==p$fixed_time_scale_sha256)
 offset<-0L;full_offset<-0L;records<-list()
 for(name in names(w)[grepl('^gru[.].*weight_',names(w))]) {
   shape<-dim(w[[name]]);rows<-shape[1]%/%3L;n<-rows*shape[2];k<-seq_len(n)-1L
   records[[name]]<-data.frame(matrix=name,destination_row=2L*rows+k%/%shape[2]+1L,source_column=k%%shape[2]+1L,
     full_connection_index=full_offset+2L*n+k+1L,lambda=lam[offset+seq_len(n)],time_scale=scale[offset+seq_len(n)])
   offset<-offset+n;full_offset<-full_offset+prod(shape)
 }
 clocks<-do.call(rbind,records);stopifnot(offset==length(lam),identical(as.integer(clocks$full_connection_index),as.integer(p$candidate_full_indices)))
 clocks$recovery_half_life_minutes<-connection_study_parameters(fit)$recovery_half_life_minutes/scale
 study_write_csv(clocks,file.path(output,'reports',paste0(task$run_id,'-connection-clocks.csv')))
 study_write_csv(clocks[c('matrix','destination_row','source_column','full_connection_index','lambda')],file.path(output,'reports',paste0(task$run_id,'-lambdas.csv')))
}
