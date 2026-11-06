# Prepare the R library inside the verified container workspace. Requiring the expected
# R version and private path prevents accidental installation into a host library.
args <- commandArgs(TRUE)
stopifnot(length(args)==1L)
workspace <- normalizePath(args[1L],winslash='/',mustWork=TRUE)
private <- Sys.getenv('LKT_GRU_LIBRARY')
dir.create(private,recursive=TRUE,showWarnings=FALSE)
stopifnot(normalizePath(private,winslash='/',mustWork=TRUE)==file.path(workspace,'library-container'),
  getRversion()==package_version('4.5.3'),nzchar(Sys.getenv('LKT_GRU_CONTAINER_SHA')))
.libPaths(c(private,.libPaths()))
# Supply the declared build tool for nloptr, an LKT dependency. Its pinned version keeps
# the package build independent of the host machine's installed tools.
cmake <- file.path(workspace,'tools','cmake-3.31.8-linux-x86_64','bin','cmake')
stopifnot(file.exists(cmake),system2(cmake,'--version',stdout=TRUE)[1L]=='cmake version 3.31.8')
Sys.setenv(PATH=paste(dirname(cmake),Sys.getenv('PATH'),sep=.Platform$path.sep))
# Use the pinned CPU Torch repository with CRAN for its R dependencies. The container
# supplies the R and system-library ABI required by that Torch binary.
options(timeout=1200,repos=c(torch='https://torch-cdn.mlverse.org/packages/cpu/0.17.0/',
  CRAN='https://cloud.r-project.org'))
# The pinned container matches the R ABI and glibc required by this CPU binary.
if(!dir.exists(file.path(private,'torch')))install.packages('torch',lib=private,dependencies=NA,Ncpus=4L)
stopifnot(requireNamespace('torch',quietly=TRUE),as.character(packageVersion('torch'))=='0.17.0',
  normalizePath(system.file(package='torch'),winslash='/')==file.path(private,'torch'),torch::torch_is_installed())
# Read LKT's actual dependency declarations and add the grid runner's digest dependency.
# Only missing or insufficient dependencies need installation in this private library.
description <- read.dcf(file.path(workspace,'source','DESCRIPTION'))
requirements <- c(trimws(strsplit(paste(description[1,c('Depends','Imports')],collapse=','),',')[[1L]]),'digest')
needed <- character()
for(requirement in requirements) {
  name <- sub(' .*','',requirement)
  if(name=='R')next
  minimum <- if(grepl('(',requirement,fixed=TRUE))sub('.*>= *([^ )]+).*','\\1',requirement) else NULL
  present <- requireNamespace(name,quietly=TRUE)
  if(!present || (!is.null(minimum) && packageVersion(name)<package_version(minimum)))needed <- c(needed,name)
}
if(length(needed))install.packages(needed,lib=private,dependencies=NA,Ncpus=4L)
# Install the exact LKT archive included in the uploaded bundle. Loading it from the
# private path confirms that the search will use the intended GRU implementation.
archive <- file.path(workspace,'source','LKT_2.0.0.9009.tar.gz')
if(!dir.exists(file.path(private,'LKT')))install.packages(archive,lib=private,repos=NULL,type='source')
stopifnot(requireNamespace('LKT',quietly=TRUE),as.character(packageVersion('LKT'))=='2.0.0.9009',
  normalizePath(system.file(package='LKT'),winslash='/')==file.path(private,'LKT'))
# Record the installed runtime and package versions after successful loading. The
# finished search can then be interpreted with its actual execution environment, not
# just its requested settings.
saveRDS(list(R=R.version.string,platform=R.version$platform,library=private,
  container_sha256=Sys.getenv('LKT_GRU_CONTAINER_SHA'),
  cmake_version='3.31.8',cmake_archive_sha256='630615d8e98ac33eba7fbe472626dff5c899c85af3c024585ae109166a6909d0',
  packages=installed.packages()[,c('Package','Version','LibPath'),drop=FALSE],
  session=capture.output(sessionInfo())),file.path(workspace,'environment.rds'))
cat('Private LKT/Torch environment installed and loaded successfully.\n')
