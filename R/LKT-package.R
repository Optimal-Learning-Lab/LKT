#' LKT: Logistic Knowledge Tracing
#'
#' The LKT package builds logistic knowledge tracing models from educational
#' trial data. Its primary workflow is to compute learning-history features for
#' one or more components and fit a logistic model that predicts the next trial
#' outcome.
#'
#' @keywords internal
#' @importFrom crayon white
#' @importFrom data.table %between% := data.table dcast setDT
#' @importFrom HDInterval hdi
#' @importFrom LiblineaR LiblineaR
#' @importFrom Matrix sparse.model.matrix
#' @importFrom SparseM as.matrix.csr
#' @importFrom cluster pam
#' @importFrom glmnet glmnet
#' @importFrom graphics abline axis hist legend matplot mtext par plot
#' @importFrom grDevices dev.off pdf
#' @importFrom lme4 glmer
#' @importFrom methods new
#' @importFrom pROC auc roc
#' @importFrom stats aggregate as.formula ave binomial coef cor deviance glm lm logLik median nobs optim plogis predict qlogis quantile sd setNames terms
#' @importFrom utils browseURL head modifyList packageVersion
"_PACKAGE"

utils::globalVariables(".SD")
