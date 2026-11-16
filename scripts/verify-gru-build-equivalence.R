# Compare executable R syntax between immutable builds. Comments, docs and
# coverage can change without invalidating an already running calibration.
a<-commandArgs(TRUE);stopifnot(length(a)==3L)
left<-file.path(a[1],'LKT/R');right<-file.path(a[2],'LKT/R')
files<-sort(list.files(left,pattern='[.]R$'))
stopifnot(identical(files,sort(list.files(right,pattern='[.]R$'))))
same<-vapply(files,function(f)identical(parse(file.path(left,f),keep.source=FALSE),
  parse(file.path(right,f),keep.source=FALSE)),logical(1))
write.csv(data.frame(file=files,executable_R_identical=same),a[3],row.names=FALSE)
stopifnot(all(same));cat('PASS',length(files),'R source files have identical executable syntax\n')
