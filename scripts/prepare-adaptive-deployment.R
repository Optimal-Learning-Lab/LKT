# Generate an isolated verification deployment, never an alternate source tree.
# ROOT CANDIDATE_LIBRARY ARCHIVE OUTPUT [EXPLICIT_POLICYFACTORY_SOURCE_ROOT]
a <- commandArgs(TRUE);stopifnot(length(a)%in%c(4L,5L))
root<-normalizePath(a[1],winslash='/');lib<-normalizePath(a[2],winslash='/');archive<-normalizePath(a[3],winslash='/')
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
out<-a[4];stopifnot(!dir.exists(out));dir.create(out,recursive=TRUE)
version<-read.dcf(file.path(lib,'LKT/DESCRIPTION'))[1,'Version']
sha<-function(p)digest::digest(file=p,algo='sha256')
relative<-paste0('.r-libs/LKT-',version,'-',substr(sha(archive),1,12))
dir.create(file.path(out,relative),recursive=TRUE)
stopifnot(file.copy(file.path(lib,'LKT'),file.path(out,relative),recursive=TRUE))
shared<-file.path(out,'workflows/simulation/shared');dir.create(shared,recursive=TRUE)
pf<-if(length(a)==5L)normalizePath(a[5],winslash='/') else file.path(dirname(root),'PolicyFactory2')
stopifnot(file.copy(file.path(pf,'workflows/simulation/shared/lkt_release.R'),shared),file.copy(archive,out))
writeLines(c(paste0('lkt_version=',version),paste0('lkt_source_archive=',basename(archive)),
  paste0('lkt_source_sha256=',sha(archive)),paste0('lkt_manifest_sha256=',sha(file.path(lib,'LKT/research-source-manifest.json'))),
  paste0('lkt_library=',relative)),file.path(shared,'lkt-release.env'))
original<-list.files(file.path(lib,'LKT'),recursive=TRUE,full.names=TRUE)
target<-file.path(out,relative,'LKT',substring(original,nchar(file.path(lib,'LKT'))+2L))
stopifnot(identical(unname(vapply(original,sha,character(1))),unname(vapply(target,sha,character(1)))))
cat('PASS verified candidate deployment:',normalizePath(out,winslash='/'),'\n')
