# RESULTS_DIRECTORY OUTPUT_CSV. Likelihood-based summaries of saved predictions.
a<-commandArgs(TRUE);stopifnot(length(a)==2L)
d<-read.csv(file.path(a[1],'reports/predictions.csv'))
stopifnot(all(d$observed%in%0:1),all(is.finite(d$prediction)),
 all(d$prediction>0 & d$prediction<1),setequal(unique(d$role),1:3))
baseline<-mean(d$observed[d$role==1L]);stopifnot(baseline>0,baseline<1)
records<-lapply(1:3,function(k) {
 rows<-d[d$role==k,,drop=FALSE]
 ll_model<-sum(dbinom(rows$observed,1,rows$prediction,log=TRUE))
 ll_null<-sum(dbinom(rows$observed,1,baseline,log=TRUE))
 data.frame(phase=c('training','validation','heldout')[k],responses=nrow(rows),
  observed_correct_rate=mean(rows$observed),null_correct_probability=baseline,
  model_log_loss=-ll_model/nrow(rows),null_log_loss=-ll_null/nrow(rows),
  mcfadden_pseudo_r2=1-ll_model/ll_null)
})
r<-do.call(rbind,records)
write.csv(r,a[2],row.names=FALSE)
cat('McFadden pseudo-R2: 1 - logLik(model)/logLik(intercept-only).\n',
 'Null probability is estimated from training students and fixed for all partitions.\n',sep='')
print(r,digits=9,row.names=FALSE)
