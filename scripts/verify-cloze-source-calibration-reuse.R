# Run real-data GRU preparation while making any attempted fit fail immediately.
# ROOT PROJECT REFERENCE CONTROL OUTPUT prepare --training-only STORE
a <- commandArgs(TRUE);stopifnot(length(a)==8L,a[6]=='prepare',a[7]=='--training-only')
root <- normalizePath(a[1],winslash='/',mustWork=TRUE)
source(file.path(root,'scripts/load-current-lkt.R'))
trace('fit_lkt_config',where=asNamespace('LKT'),print=FALSE,
  tracer=quote(stop('FORBIDDEN: fitting LKT during cached GRU preparation')))
trace('LKT',where=asNamespace('LKT'),print=FALSE,
  tracer=quote(stop('FORBIDDEN: fitting a model during cached GRU preparation')))
source(file.path(root,'scripts/gru-ground-truth-eight-input.R'))
