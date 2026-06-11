\name{LKTOnlineSimpleAdaptiveDecayEval}
\alias{LKTOnlineSimpleAdaptiveDecayEval}
\title{Evaluate online simple-adaptive decay learning}
\usage{
LKTOnlineSimpleAdaptiveDecayEval(
  par,
  input,
  clip_epsilon = 1e-06,
  denom_epsilon = 1e-06,
  decay_lower = 1e-05,
  decay_upper = 0.99999,
  return_details = FALSE,
  require_native = FALSE
)
}
\arguments{
\item{par}{Numeric vector: intercept, recency beta, logsuc beta, logitdec beta, recency alpha, logsuc alpha, and recency-decay alpha.}
\item{input}{Output from \code{LKTOnlineSimpleAdaptiveInput()}.}
\item{clip_epsilon}{Probability clipping value.}
\item{denom_epsilon}{Denominator stabilizer for beta updates.}
\item{decay_lower}{Lower bound for the within-subject recency decay state.}
\item{decay_upper}{Upper bound for the within-subject recency decay state.}
\item{return_details}{Logical; return predictions and final subject states.}
\item{require_native}{Logical; stop if the compiled native evaluator is not loaded.}
}
\value{
Negative log likelihood, or details when requested.
}
\description{
Replays the online simple-adaptive model while adapting the recency decay
parameter within subject using \code{d <- d - alpha_decay * error}.
}
