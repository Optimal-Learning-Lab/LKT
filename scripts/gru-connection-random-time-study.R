# Extend the frozen common helper only to audit the fixed, nonlearned clock.
.connection_fit_without_clock_audit <- connection_study_fit
connection_study_fit <- function(task,s,output,p,d) {
 .connection_fit_without_clock_audit(task,s,output,p,d)
 fit<-readRDS(file.path(output,'models',paste0(task$run_id,'.rds')))$fit
 scale<-as.numeric(fit$model$weights[['gate_fatigue.time_scale']])
 stopifnot(identical(scale,p$fixed_time_scales),digest::digest(scale,algo='sha256')==p$fixed_time_scale_sha256)
 clocks<-read.csv(file.path(output,'reports',paste0(task$run_id,'-lambdas.csv')))
 stopifnot(nrow(clocks)==length(scale));clocks$time_scale<-scale
 clocks$recovery_half_life_minutes<-connection_study_parameters(fit)$recovery_half_life_minutes/scale
 study_write_csv(clocks,file.path(output,'reports',paste0(task$run_id,'-connection-clocks.csv')))
}
