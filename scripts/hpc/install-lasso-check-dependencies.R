# DEPENDENCY_LIBRARY RECEIPT -- installation only, no model fitting.
a<-commandArgs(TRUE);stopifnot(length(a)==2L,R.version$major=='4',R.version$minor=='5.3')
lib<-normalizePath(a[1],winslash='/');.libPaths(c(lib,.Library))
packages<-c('fgui','rmarkdown','knitr','ggplot2')
missing<-packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing))install.packages(missing,lib=lib,repos='https://cloud.r-project.org',dependencies=NA,Ncpus=2L)
stopifnot(all(vapply(packages,requireNamespace,logical(1),quietly=TRUE)))
receipt<-list(R=R.version.string,library=lib,repository='https://cloud.r-project.org',
  package_versions=setNames(vapply(packages,function(n)as.character(utils::packageVersion(n)),character(1)),packages),
  installed=missing,session=capture.output(sessionInfo()))
saveRDS(receipt,a[2]);print(receipt$package_versions)
