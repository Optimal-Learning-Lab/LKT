# ROOT CANDIDATE_LIBRARY ORIGINAL_BUNDLE -- read-only historical prediction oracle.
a<-commandArgs(TRUE);stopifnot(length(a)==3L)
.libPaths(c(file.path(a[1],'.codex-r-lib'),.libPaths()));library(LKT,lib.loc=a[2])
b<-readRDS(a[3]);before<-tools::md5sum(a[3])
for(role in c('ground_truth','tracer')) {
  f<-b[[role]]$fit
  q<-LKTGRUStep(f,LKTGRUState(f),as.data.frame(f$newdata))$predictions
  delta<-max(abs(q-f$prediction))
  stopifnot(is.finite(delta),delta<4e-6)
  cat('PASS original pre-encoder',role,'saved prediction maximum difference',delta,'\n')
}
stopifnot(identical(before,tools::md5sum(a[3])))
