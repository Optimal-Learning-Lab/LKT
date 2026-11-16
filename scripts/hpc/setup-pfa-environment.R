# Dependency/package installation only. Execute inside the selected Linux SIF
# and Slurm allocation through setup-pfa-environment.py; never on a login host.
a <- commandArgs(TRUE)
stopifnot(length(a)==2L,a[2L]%in%c('dependencies','package'))
config_file <- normalizePath(a[1L],mustWork=TRUE)
cfg <- read.dcf(config_file)
stopifnot(nrow(cfg)==1L)
field <- function(name) {
 stopifnot(name%in%colnames(cfg),length(cfg[1L,name])==1L,!is.na(cfg[1L,name]),nzchar(cfg[1L,name]))
 unname(cfg[1L,name])
}
sha <- function(path) {
 value <- system2('sha256sum',shQuote(normalizePath(path,mustWork=TRUE)),stdout=TRUE)
 stopifnot(is.null(attr(value,'status')),length(value)==1L,grepl('^[a-f0-9]{64} ',value))
 substr(value,1L,64L)
}
inside <- function(path,parent)identical(path,parent) || startsWith(path,paste0(parent,'/'))
workspace <- normalizePath(field('Workspace'),mustWork=TRUE)
private <- normalizePath(field('Private-Library'),mustWork=TRUE)
reports <- normalizePath(field('Reports-Directory'),mustWork=TRUE)
job <- Sys.getenv('SLURM_JOB_ID');cpus <- as.integer(field('Install-CPUs'))
stopifnot(field('Schema')=='1',getRversion()==package_version('4.5.3'),
 Sys.info()[['sysname']]=='Linux',grepl('^[0-9]+$',job),
 as.integer(Sys.getenv('SLURM_CPUS_PER_TASK'))>=cpus,cpus>=1L,
 inside(private,workspace),inside(reports,workspace),
 identical(sha(field('Container')),field('Container-SHA256')),
 identical(sha(field('Description')),field('Description-SHA256')),
 identical(sha(field('Setup-R-Script')),field('Setup-R-SHA256')),
 identical(Sys.getenv('LC_ALL'),field('Locale')),identical(Sys.getenv('LANG'),field('Locale')),
 identical(Sys.getenv('R_TESTS'),''),identical(Sys.getenv('CUDA'),'cpu'),
 identical(Sys.getenv('TORCH_INSTALL'),'0'),identical(Sys.getenv('TORCH_CUDATOOLKIT'),'FALSE'),
 identical(Sys.getenv('TORCH_VERIFY_LOAD'),'TRUE'),
 identical(normalizePath(field('Rscript-Executable'),mustWork=TRUE),
  normalizePath(file.path(R.home('bin'),'Rscript'),mustWork=TRUE)))
cat('PREFLIGHT',R.version.string,'Rscript',normalizePath(field('Rscript-Executable'),mustWork=TRUE),'\n')
# Exclude every host/site/user library. The image's base/recommended packages are
# explicitly allowed; all other installed requirements belong to this private lib.
.libPaths(c(private,.Library),include.site=FALSE)
stopifnot(identical(normalizePath(.libPaths(),mustWork=TRUE),c(private,normalizePath(.Library,mustWork=TRUE))))
cmake <- Sys.getenv('LKT_SETUP_CMAKE')
stopifnot(nzchar(cmake),system2(cmake,'--version',stdout=TRUE)[1L]=='cmake version 3.31.8',
 startsWith(normalizePath(tempdir(),mustWork=TRUE),paste0(Sys.getenv('LKT_SETUP_SCRATCH'),'/')))
Sys.setenv(PATH=paste(dirname(cmake),Sys.getenv('PATH'),sep=.Platform$path.sep))
options(timeout=1800,repos=c(CRAN=field('CRAN-Repository')),Ncpus=cpus,warn=2)
report_file <- file.path(reports,paste0(a[2L],'-',job,'.json'))
stopifnot(!file.exists(report_file))
# Parse actual Depends/Imports without executing any source code.
description <- read.dcf(field('Description'))
requirements <- function(desc) {
 fields <- intersect(c('Depends','Imports'),colnames(desc))
 tokens <- trimws(strsplit(paste(desc[1L,fields],collapse=','),',',fixed=TRUE)[[1L]])
 lapply(tokens[nzchar(tokens)],function(token) {
  m <- regmatches(token,regexec('^([A-Za-z][A-Za-z0-9.]*)[[:space:]]*(?:\\(([><=]+)[[:space:]]*([0-9][0-9A-Za-z.+-]*)\\))?$',token,perl=TRUE))[[1L]]
  if(!length(m))stop('Unsupported dependency declaration: ',token)
  list(package=m[2L],operator=m[3L],version=m[4L])
 })
}
req <- requirements(description)
satisfies <- function(version,requirement) {
 if(!nzchar(requirement$operator))return(TRUE)
 comparison <- utils::compareVersion(as.character(version),requirement$version)
 switch(requirement$operator,'>='=comparison>=0,'>'=comparison>0,'=='=comparison==0,
  '<='=comparison<=0,'<'=comparison<0,stop('Unsupported dependency comparator.'))
}
system_package <- function(name) {
 d <- tryCatch(utils::packageDescription(name,lib.loc=.Library),error=function(e)NULL)
 !is.null(d) && !is.null(d$Priority) && d$Priority%in%c('base','recommended')
}
verify_requirement <- function(r) {
 if(r$package=='R')return(stopifnot(satisfies(getRversion(),r)))
 lib <- if(system_package(r$package) && !dir.exists(file.path(private,r$package))).Library else private
 stopifnot(dir.exists(file.path(lib,r$package)),satisfies(utils::packageVersion(r$package,lib.loc=lib),r))
 invisible(TRUE)
}
torch_repo <- 'https://torch-cdn.mlverse.org/packages/cpu/0.17.0/'
torch_archive <- NULL;torch_archive_sha <- NULL;torch_metadata <- NULL
if(a[2L]=='dependencies') {
 stopifnot(length(list.files(private,all.files=TRUE,no..=TRUE))==0L)
 db <- available.packages(repos=field('CRAN-Repository'),type='source')
 pinned <- available.packages(repos=torch_repo,type='source')
 stopifnot('torch'%in%rownames(pinned),pinned['torch','Version']=='0.17.0')
 # Install the pinned package's R dependencies from CRAN BEFORE torch itself.
 td <- pinned['torch',,drop=FALSE]
 torch_dependencies <- unique(unlist(tools::package_dependencies('torch',db=td,
  which=c('Depends','Imports','LinkingTo'),recursive=FALSE),use.names=FALSE))
 direct <- unique(c(vapply(req,`[[`,character(1),'package'),'digest','jsonlite','coro',torch_dependencies))
 needed <- setdiff(direct,c('R','torch'))
 needed <- needed[!vapply(needed,system_package,logical(1))]
 stopifnot(all(needed%in%rownames(db)))
 # Inspect the actual pinned CPU bundle BEFORE spending time compiling CRAN
 # dependencies. The exported path API and bundled version are not assumed from
 # another platform's installation.
 downloaded <- download.packages('torch',destdir=tempdir(),repos=torch_repo,
  available=pinned,type='source')
 stopifnot(nrow(downloaded)==1L,downloaded[1L,1L]=='torch')
 torch_archive <- downloaded[1L,2L];torch_archive_sha <- sha(torch_archive)
 torch_metadata <- as.list(pinned['torch',])
 inspect <- tempfile('torch-bundle-');dir.create(inspect)
 members <- utils::untar(torch_archive,list=TRUE)
 stopifnot(sum(members=='torch/build-version')==1L,sum(members=='torch/NAMESPACE')==1L)
 utils::untar(torch_archive,files=c('torch/build-version','torch/NAMESPACE'),exdir=inspect)
 stopifnot(identical(trimws(readLines(file.path(inspect,'torch','build-version'),warn=FALSE)),'2.8.0+cpu'),
  any(grepl('^export[(]torch_install_path[)]$',readLines(file.path(inspect,'torch','NAMESPACE'),warn=FALSE))))
 if(length(needed))install.packages(needed,lib=private,repos=field('CRAN-Repository'),
  available=db,type='source',dependencies=NA,Ncpus=cpus,destdir=tempdir())
 # Never combine CRAN and the Torch repo for version selection and never resolve
 # dependencies while installing this explicit CPU archive.
 install.packages(torch_archive,lib=private,repos=NULL,type='source',dependencies=FALSE)
} else {
 receipt_file <- field('Dependency-Receipt')
 stopifnot(identical(sha(receipt_file),field('Dependency-Receipt-SHA256')))
 receipt <- jsonlite::fromJSON(receipt_file,simplifyVector=FALSE)
 stopifnot(isTRUE(receipt$complete),identical(receipt$phase,'dependencies'),
  identical(receipt$private_library,private),identical(receipt$container_sha256,field('Container-SHA256')),
  identical(receipt$description_sha256,field('Description-SHA256')))
 installed <- installed.packages(lib.loc=private)
 for(entry in receipt$private_packages)stopifnot(entry$Package%in%rownames(installed),
  identical(installed[entry$Package,'Version'],entry$Version))
 stopifnot(!dir.exists(file.path(private,'LKT')))
}
for(r in req)verify_requirement(r)
for(name in c('digest','jsonlite','coro','torch'))stopifnot(
 dir.exists(file.path(private,name)),requireNamespace(name,quietly=TRUE),
 identical(normalizePath(find.package(name),mustWork=TRUE),file.path(private,name)))
stopifnot(as.character(utils::packageVersion('torch'))=='0.17.0',torch::torch_is_installed(),
 identical(normalizePath(torch::torch_install_path(),mustWork=TRUE),file.path(private,'torch')))
# The public default status call respects the verified startup cache. Forcing
# recheck writes .torch_can_load inside the locked 0.17.0 namespace. Verify actual
# CPU execution independently through ordinary public tensor/autograd APIs.
probe <- torch::torch_tensor(c(1,2,3),device='cpu',dtype=torch::torch_float64(),requires_grad=TRUE)
probe_sum <- probe$pow(2)$sum()
stopifnot(identical(as.character(probe$device),'cpu'),probe_sum$item()==14)
probe_sum$backward()
probe_gradient <- as.numeric(torch::as_array(probe$grad))
stopifnot(identical(probe_gradient,c(2,4,6)))
libtorch_build <- trimws(readLines(file.path(private,'torch','build-version'),warn=FALSE))
stopifnot(identical(libtorch_build,'2.8.0+cpu'))
source <- NULL
if(a[2L]=='package') {
 candidate_file <- field('Candidate-JSON')
 stopifnot(identical(sha(candidate_file),field('Candidate-JSON-SHA256')))
 candidate <- jsonlite::fromJSON(candidate_file)
 stopifnot(is.character(candidate$source_archive),length(candidate$source_archive)==1L,
  is.character(candidate$source_sha256),length(candidate$source_sha256)==1L,
  grepl('^[a-f0-9]{64}$',candidate$source_sha256),
  is.character(candidate$version),length(candidate$version)==1L,
  is.character(candidate$manifest_sha256),length(candidate$manifest_sha256)==1L,
  grepl('^[a-f0-9]{64}$',candidate$manifest_sha256),
  identical(sha(candidate$source_archive),candidate$source_sha256),
  identical(normalizePath(candidate$library,mustWork=TRUE),private))
 unpack <- tempfile('candidate-description-');dir.create(unpack)
 members <- utils::untar(candidate$source_archive,list=TRUE)
 stopifnot(sum(members=='LKT/DESCRIPTION')==1L)
 utils::untar(candidate$source_archive,files='LKT/DESCRIPTION',exdir=unpack)
 archive_description <- read.dcf(file.path(unpack,'LKT','DESCRIPTION'))
 stopifnot(identical(unname(archive_description[1L,'Package']),'LKT'),
  identical(unname(archive_description[1L,'Version']),candidate$version),
  identical(requirements(archive_description),req))
 install.packages(candidate$source_archive,lib=private,repos=NULL,type='source',dependencies=FALSE)
 stopifnot(requireNamespace('LKT',quietly=TRUE),
  identical(normalizePath(find.package('LKT'),mustWork=TRUE),file.path(private,'LKT')),
  identical(as.character(utils::packageVersion('LKT')),candidate$version),
  identical(sha(system.file('research-source-manifest.json',package='LKT')),candidate$manifest_sha256))
 source <- candidate
}
packages <- installed.packages(lib.loc=private)[,c('Package','Version','LibPath'),drop=FALSE]
private_packages <- lapply(seq_len(nrow(packages)),function(i)as.list(packages[i,]))
record <- list(schema=1L,complete=TRUE,phase=a[2L],job_id=job,
 setup_script_sha256=sha(field('Setup-R-Script')),configuration_sha256=sha(config_file),
 r_version=R.version.string,platform=R.version$platform,locale=Sys.getlocale(),
 library_paths=.libPaths(),private_library=private,
 rscript_requested_path=field('Rscript-Executable'),
 rscript_executable_path=normalizePath(field('Rscript-Executable'),mustWork=TRUE),
 rscript_sha256=sha(file.path(R.home('bin'),'Rscript')),
 container_sha256=field('Container-SHA256'),description_sha256=field('Description-SHA256'),
 dependency_requirements=req,cmake_version='3.31.8',
 cmake_archive_sha256='630615d8e98ac33eba7fbe472626dff5c899c85af3c024585ae109166a6909d0',
 torch_package_version=as.character(utils::packageVersion('torch')),
 libtorch_bundled_build_version=libtorch_build,libtorch_version_evidence='Installed CPU bundle build-version metadata; same explicit bundle passed runtime load verification.',
 libtorch_load_verified=TRUE,
 cpu_tensor_verification=list(device=as.character(probe$device),dtype='float64',
  squared_sum=probe_sum$item(),gradient=probe_gradient),
 torch_install_path=torch::torch_install_path(),torch_repository=torch_repo,
 torch_archive_sha256=torch_archive_sha,torch_repository_metadata=torch_metadata,
 private_packages=private_packages,source=source,session_info=capture.output(sessionInfo()))
jsonlite::write_json(record,report_file,auto_unbox=TRUE,pretty=TRUE,null='null')
cat('VERIFIED',a[2L],report_file,'SHA256',sha(report_file),'\n')
