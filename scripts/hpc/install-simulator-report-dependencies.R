# DEPENDENCY_LIBRARY RECEIPT_JSON [REPOSITORY] -- add missing SVG dependencies only.
a <- commandArgs(TRUE);stopifnot(length(a)%in%c(2L,3L))
lib <- normalizePath(a[1],winslash='/');.libPaths(c(lib,.Library))
repository <- if(length(a)==3L)a[3] else 'https://cloud.r-project.org'
binary <- grepl('^https://packagemanager[.]posit[.]co/cran/__linux__/noble/[0-9-]+$',repository)
descriptions <- list.files(lib,pattern='^DESCRIPTION$',recursive=TRUE,full.names=TRUE)
hash <- function(p)digest::digest(file=p,algo='sha256')
before <- vapply(descriptions,hash,character(1))
missing <- if(requireNamespace('svglite',quietly=TRUE))character() else 'svglite'
if(length(missing)) {
  if(binary) {
    stopifnot(R.version$platform=='x86_64-pc-linux-gnu',
      any(readLines('/etc/os-release')=='VERSION_ID="24.04"'))
    options(HTTPUserAgent=sprintf('R/%s R (%s)',getRversion(),
      paste(getRversion(),R.version$platform,R.version$arch,R.version$os)))
  } else if(system2('pkg-config',c('--exists','fontconfig','freetype2'))!=0L)
    stop('SVG reporting requires fontconfig and FreeType development libraries in the declared container.')
  install.packages(missing,lib=lib,repos=repository,dependencies=NA,Ncpus=2L)
}
stopifnot(requireNamespace('svglite',quietly=TRUE),
  identical(before,vapply(descriptions,hash,character(1))))
path <- tempfile(fileext='.svg');svglite::svglite(path,width=4,height=3)
plot(1:3,c(.2,.5,.8),type='l',main='SVG report dependency check');grDevices::dev.off()
stopifnot(file.info(path)$size>1000L,any(grepl('<svg',readLines(path,warn=FALSE),fixed=TRUE)))
unlink(path)
jsonlite::write_json(list(passed=TRUE,library=lib,repository=repository,binary_repository=binary,
  installed=missing,svglite_version=as.character(utils::packageVersion('svglite')),
  existing_package_descriptions_unchanged=TRUE,svg_device_rendered=TRUE,
  session=capture.output(sessionInfo())),a[2],auto_unbox=TRUE,pretty=TRUE)
cat('PASS SVG rendering dependency installed; existing packages unchanged\n')
