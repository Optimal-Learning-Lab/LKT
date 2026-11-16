# Meaningful numerical checks: participant weights, ties, and independent AUC.
source('scripts/report-gru-feature-ablation.R')
set.seed(20261106L)
y <- rep(c(0,1,1,0,1),6L);participant <- rep(1:6,each=5L)
for(prediction in list(rep(.5,30L),ifelse(y==1,.8,.2),ifelse(y==1,.2,.8),round(runif(30L),1))) {
  idx <- order(prediction);q <- prediction[idx]
  z <- list(participant=participant[idx],y=y[idx],ends=c(which(diff(q)!=0),length(q)))
  for(counts in list(rep(1L,6L),c(0L,2L,0L,3L,1L,0L),c(4L,0L,1L,0L,0L,1L))) {
    rows <- rep(seq_along(y),counts[participant])
    expected <- as.numeric(pROC::auc(y[rows],prediction[rows],levels=c(0,1),direction='<',quiet=TRUE))
    stopifnot(abs(feature_ablation_weighted_auc(z,counts)-expected)<1e-12)
  }
}
cat('PASS participant-weighted AUC: ties, perfect/reversed ranking, zero-weight participants and repeated whole histories.\n')
