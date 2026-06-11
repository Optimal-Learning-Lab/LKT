\name{LKTOptimizeOnlineSimpleAdaptiveDecayAlpha}
\alias{LKTOptimizeOnlineSimpleAdaptiveDecayAlpha}
\title{Optimize online beta and decay learning rates}
\usage{
LKTOptimizeOnlineSimpleAdaptiveDecayAlpha(
  input,
  start = c(alpha_recency = 0.03, alpha_logsuc = 0.05, alpha_decay = 0.01),
  lower = c(0, 0, 0),
  upper = rep(Inf, 3),
  control = list(maxit = 100),
  require_native = FALSE,
  use_gradient = FALSE,
  decay_lower = 1e-05,
  decay_upper = 0.99999
)
}
\arguments{
\item{input}{Output from \code{LKTOnlineSimpleAdaptiveInput()}.}
\item{start}{Starting vector for recency beta alpha, logsuc beta alpha, and recency-decay alpha.}
\item{lower}{Lower bounds for L-BFGS-B.}
\item{upper}{Upper bounds for L-BFGS-B.}
\item{control}{Optimizer control list.}
\item{require_native}{Logical; stop if the compiled native evaluator is not loaded.}
\item{use_gradient}{Reserved for future analytic-gradient support.}
\item{decay_lower}{Lower bound for the within-subject recency decay state.}
\item{decay_upper}{Upper bound for the within-subject recency decay state.}
}
\value{
A list containing the optimizer result and fit details.
}
\description{
Optimizes online learning rates while holding global coefficients and the
global starting recency decay fixed.
}
