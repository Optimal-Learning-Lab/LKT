a<-commandArgs(TRUE);stopifnot(length(a)==1L)
x<-readRDS(a[1]);if(inherits(x,'PFSimulationBundle'))x<-x$tracer$fit
if(!is.null(x[['fit']]))x<-x[['fit']]
print(x$model$settings);print(names(x$model$weights))
