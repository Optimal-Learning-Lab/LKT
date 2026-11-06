# Large-sample AUC must remain finite and agree with independent reference cases.
# These counts deliberately exceed the 32-bit limit for positive-negative pairs.
source('scripts/gru-encoder-study.R')
y <- rep(0:1,each=60000L)
stopifnot(study_metrics(y,ifelse(y==1,.9,.1))[['AUC']]==1,
  study_metrics(y,ifelse(y==1,.1,.9))[['AUC']]==0,
  study_metrics(y,rep(.5,length(y)))[['AUC']]==.5)
set.seed(20261105L);p <- round(runif(length(y),.01,.99),3)
expected <- as.numeric(pROC::auc(y,p,quiet=TRUE,direction='<'))
stopifnot(abs(study_metrics(y,p)[['AUC']]-expected)<1e-12)
cat('PASS large-sample AUC: perfect, reversed, tied and independent pROC cases.\n')
