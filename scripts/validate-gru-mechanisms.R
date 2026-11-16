# ROOT LIBRARY LKT_VERSION R_VERSION MANIFEST_SHA256 OUTPUT SEMANTIC_CONFIG_OR_PENDING
# Execute once inside the selected Linux/Slurm container. This never submits jobs.
a<-commandArgs(TRUE);stopifnot(length(a)==7L)
root<-normalizePath(a[1],winslash='/',mustWork=TRUE)
lib<-normalizePath(a[2],winslash='/',mustWork=TRUE)
version<-a[3];r_version<-a[4];expected_manifest<-a[5];out<-a[6];semantic<-a[7]
stopifnot(.Platform$OS.type=='unix',nzchar(Sys.getenv('SLURM_JOB_ID')),
 grepl('^[1-9][0-9]*$',Sys.getenv('SLURM_CPUS_PER_TASK')),
 grepl('^[a-f0-9]{64}$',expected_manifest),as.character(getRversion())==r_version,!dir.exists(out))
for(n in c('OMP_NUM_THREADS','MKL_NUM_THREADS','OPENBLAS_NUM_THREADS'))stopifnot(Sys.getenv(n)=='1')
stopifnot(Sys.getenv('R_TESTS')=='',nzchar(Sys.getenv('LC_ALL')),nzchar(Sys.getenv('LANG')))
.libPaths(c(lib,.Library),include.site=FALSE)
Sys.setenv(R_LIBS=lib,R_LIBS_USER=lib,R_LIBS_SITE='',LKT_REQUIRE_TORCH_TESTS='true')
if(semantic=='PENDING')Sys.unsetenv('LKT_SEMANTIC_TEST_CONFIG') else {
 semantic<-normalizePath(semantic,winslash='/',mustWork=TRUE)
 Sys.setenv(LKT_SEMANTIC_TEST_CONFIG=semantic)
}
suppressPackageStartupMessages(library(LKT,lib.loc=lib))
stopifnot(as.character(utils::packageVersion('LKT'))==version,
 normalizePath(find.package('LKT'),winslash='/')==paste0(lib,'/LKT'),
 requireNamespace('digest',quietly=TRUE),requireNamespace('jsonlite',quietly=TRUE),
 requireNamespace('torch',quietly=TRUE),as.character(utils::packageVersion('torch'))=='0.17.0',torch::torch_is_installed())
hash<-function(file)digest::digest(file=file,algo='sha256',serialize=FALSE)
manifest_file<-system.file('research-source-manifest.json',package='LKT')
stopifnot(nzchar(manifest_file),hash(manifest_file)==expected_manifest)
manifest<-jsonlite::fromJSON(manifest_file,simplifyVector=FALSE)
stopifnot(identical(manifest$version,version))
rscript<-normalizePath(file.path(R.home('bin'),'Rscript'),winslash='/',mustWork=TRUE)
tests<-c('tests/gru-configuration.R','tests/gru-defaults.R','tests/gru-saved-architecture.R',
 'tests/gru-tensor-ownership.R','tests/gru-runtime.R','tests/gru-regression.R',
 'tests/gru-plasticity.R','tests/gru-retrieval-filter.R','tests/gru-latent-denoising.R',
 'tests/gru-bayesian-prototypes.R','tests/gru-exercise-attributes.R','scripts/test-pfa-exercise-attributes.R')
parameter_probe<-'scripts/verify-gru-mechanism-parameters.R'
required<-unique(c(tests,parameter_probe,'scripts/validate-gru-mechanisms.R',
 'scripts/verify-pfa-exercise-attributes.R','tests/fixtures/gru-minimal-settings.R',
 'tests/fixtures/gru-exercise-attributes-fixture.R','DESCRIPTION','NAMESPACE',
 vapply(Filter(function(x)startsWith(x$path,'R/'),manifest$files),`[[`,character(1),'path')))
if(semantic!='PENDING')required<-c(required,'tests/gru-semantic-input.R')
entries<-stats::setNames(vapply(manifest$files,`[[`,character(1),'sha256'),vapply(manifest$files,`[[`,character(1),'path'))
stopifnot(all(required%in%names(entries)))
actual<-stats::setNames(vapply(file.path(root,required),hash,character(1)),required)
stopifnot(identical(unname(actual),unname(entries[required])))
dir.create(out,recursive=TRUE);out<-normalizePath(out,winslash='/',mustWork=TRUE)
records<-list();started<-format(Sys.time(),tz='UTC',usetz=TRUE)
provenance<-list(schema=1L,started_utc=started,lkt_version=version,r_version=R.version.string,
 platform=R.version$platform,locale=Sys.getlocale(),library=lib,library_paths=.libPaths(),
 rscript=rscript,rscript_sha256=hash(rscript),installed_manifest_sha256=expected_manifest,
 host=as.list(Sys.info()),slurm_job_id=Sys.getenv('SLURM_JOB_ID'),
 slurm_cpus_per_task=Sys.getenv('SLURM_CPUS_PER_TASK'),
 environment=as.list(Sys.getenv(c('LC_ALL','LANG','LANGUAGE','R_TESTS','R_LIBS','R_LIBS_USER','R_LIBS_SITE',
 'OMP_NUM_THREADS','MKL_NUM_THREADS','OPENBLAS_NUM_THREADS','LKT_REQUIRE_TORCH_TESTS'))),
 package_versions=as.list(vapply(c('LKT','torch','jsonlite','digest'),function(n)as.character(utils::packageVersion(n)),character(1))),
 source_sha256=as.list(actual),semantic_config=if(semantic=='PENDING')NULL else semantic,
 semantic_config_sha256=if(semantic=='PENDING')NULL else hash(semantic))
write_summary<-function() {
 native<-Filter(function(x)x$group=='native',records)
 native_ok<-length(native)==13L && all(vapply(native,function(x)isTRUE(x$passed),logical(1)))
 sem<-Filter(function(x)x$group=='semantic',records)
 sem_ok<-length(sem)==1L && isTRUE(sem[[1L]]$passed)
 summary<-list(schema=1L,provenance=provenance,checks=records,
 native_required_checks_verified=native_ok,native_required_check_count=13L,
 semantic_status=if(semantic=='PENDING')'PENDING_EXPLICIT_CONFIG_NOT_SUPPLIED' else if(!length(sem))'NOT_RUN' else if(sem_ok)'PASS' else 'FAIL',
 eakt_study_annotations_verified=FALSE,eakt_study_preflight_positive_gate='PENDING_REAL_VALIDATED_ANNOTATIONS',
 complete=native_ok && sem_ok,requested_suite_passed=native_ok && (semantic=='PENDING' || sem_ok),
 finished_utc=format(Sys.time(),tz='UTC',usetz=TRUE))
 jsonlite::write_json(summary,file.path(out,'validation.json'),auto_unbox=TRUE,pretty=TRUE,null='null',digits=17)
 summary
}
run<-function(file,group,required_marker=NULL) {
 name<-sub('[.]R$','',basename(file));log<-file.path(out,paste0(name,'.log'))
 args<-c('--vanilla',shQuote(file.path(root,file)))
 if(startsWith(file,'scripts/'))args<-c(args,shQuote(root))
 time<-proc.time()[['elapsed']]
 status<-tryCatch(suppressWarnings(system2(rscript,args,stdout=log,stderr=log)),error=function(e){writeLines(conditionMessage(e),log);127L})
 passed<-identical(as.integer(status),0L)
 if(!is.null(required_marker))passed<-passed && any(grepl(required_marker,readLines(log,warn=FALSE),fixed=TRUE))
 records[[length(records)+1L]]<<-list(file=file,group=group,status=as.integer(status),passed=passed,
 elapsed_seconds=unname(proc.time()[['elapsed']]-time),log=log,log_sha256=hash(log))
 write_summary();cat(if(passed)'PASS' else 'FAIL',file,'log=',log,'\n');flush.console()
}
invisible(write_summary())
for(file in c(tests,parameter_probe))run(file,'native')
if(semantic!='PENDING')run('tests/gru-semantic-input.R','semantic','PASS semantic dimensions/counts, real local encoding/empty strings')
result<-write_summary()
cat('native_required_checks_verified=',result$native_required_checks_verified,
 'semantic_status=',result$semantic_status,'complete=',result$complete,
 'eakt_study_annotations_verified=FALSE\n',sep='')
quit(save='no',status=if(result$requested_suite_passed)0L else 1L)
