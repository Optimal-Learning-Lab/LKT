\name{LKTOptimizeOnlineSimpleAdaptiveAlpha}
\alias{LKTOptimizeOnlineSimpleAdaptiveAlpha}
\title{Optimize only the online learning rates}
\usage{
LKTOptimizeOnlineSimpleAdaptiveAlpha(
  input,
  start = c(alpha_recency = 0.03, alpha_logsuc = 0.05),
  lower = c(0, 0),
  upper = rep(Inf, 2),
  control = list(maxit = 100),
  require_native = FALSE,
  use_gradient = TRUE
)
}
\arguments{
\item{input}{Output from \code{LKTOnlineSimpleAdaptiveInput()}.}
\item{start}{Optional two-parameter starting vector: recency alpha and logsuc alpha.}
\item{lower}{Lower bounds for L-BFGS-B.}
\item{upper}{Upper bounds for L-BFGS-B.}
\item{control}{Optimizer control list.}
\item{require_native}{Logical; stop if the compiled native evaluator is not loaded.}
\item{use_gradient}{Logical; pass the propagated alpha gradient to the optimizer.}
}
\value{
A list containing the optimizer result and fit details.
}
\description{
Optimizes the recency and logsuc online learning rates while holding the global
simple-adaptive coefficients fixed at their LibLinear optimum.
}
