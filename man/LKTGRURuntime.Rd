\name{LKTGRURuntime}
\alias{LKTGRURuntime}
\title{Load a fitted GRU once for continued inference}
\usage{LKTGRURuntime(modelob)}
\arguments{\item{modelob}{Fitted GRU LKT model.}}
\value{A process-local LKTGRURuntime environment.}
\description{A runtime owns a loaded Torch network and belongs to its creating process. Save fitted models and LKTGRUState objects, never runtimes. Create a runtime independently in each worker. Construction and prediction restore caller random streams and Torch thread settings.}
