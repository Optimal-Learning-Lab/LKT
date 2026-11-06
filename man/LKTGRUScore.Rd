\name{LKTGRUScore}
\alias{LKTGRUScore}
\title{Score independent GRU candidates without observing them}
\usage{LKTGRUScore(modelob, state, data, runtime = NULL)}
\arguments{
\item{modelob}{Fitted GRU LKT model.}
\item{state}{Retained LKTGRUState.}
\item{data}{Candidate trial rows. Answers are ignored.}
\item{runtime}{Optional process-local LKTGRURuntime.}
}
\value{Numeric correctness probabilities in input order.}
\description{Each candidate branches from the same supplied state, including when rows share a history key. No hidden state, retained history, pending trial, or ordering metadata is committed. Use LKTGRUStep to observe the selected answered trial.}
