\name{LKTGRUOptionalIdentities}
\alias{LKTGRUOptionalIdentities}
\title{Allow unmarked identities in a fitted adaptive GRU}
\usage{LKTGRUOptionalIdentities(modelob, columns)}
\arguments{
\item{modelob}{Fitted GRU already using per-student embedding adaptation.}
\item{columns}{Adaptive categorical inputs allowed to be absent or NA.}
}
\value{A derived fitted model with unchanged shared weights and a new continuation identity.}
\description{A missing optional label provides a neutral zero vector, never creates or updates
an identity entry and never matches a retrieval memory. Supplied unfamiliar labels remain
distinct and learn through the ordinary post-answer rule. A versioned identity-policy
extension records the parent model; existing states cannot continue under the new policy.
The original fit, its weights and its strict input policy are unchanged. Optional columns
cannot be required engineered-feature, timing or history-group inputs. No refitting occurs.}
