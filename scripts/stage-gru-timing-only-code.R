# Stage a source-only follow-up bundle; scientific inputs are reused remotely.
# ROOT ARTIFACT LOCAL_PLAN LOCAL_REFERENCE_STUDY_ROOT REMOTE_REFERENCE_STUDY_ROOT
args<-commandArgs(TRUE);stopifnot(length(args)==5L)
root<-normalizePath(args[1],winslash='/',mustWork=TRUE);artifact<-args[2];plan<-readRDS(file.path(args[3],'study.rds'));reference<-args[4]
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
hash<-function(f)digest::digest(file=f,algo='sha256')
bundle<-file.path(artifact,'code-only-bundle');stopifnot(!dir.exists(bundle));dir.create(bundle,recursive=TRUE)
files<-c('scripts/compare-gru-timing-only.R','scripts/report-gru-timing-only.R','scripts/gru-encoder-study.R','scripts/verify-gru-followup-inputs.py',paste0('scripts/hpc/',c('setup','run','report'),'-gru-timing-only.slurm'))
for(f in files){dir.create(dirname(file.path(bundle,f)),recursive=TRUE,showWarnings=FALSE)
 if(endsWith(f,'.slurm'))writeBin(charToRaw(paste0(paste(readLines(file.path(root,f)),collapse='\n'),'\n')),file.path(bundle,f)) else stopifnot(file.copy(file.path(root,f),file.path(bundle,f)))}
for(f in names(plan$script_hashes))stopifnot(hash(file.path(bundle,f))==plan$script_hashes[[f]])
candidate<-jsonlite::fromJSON(file.path(reference,'candidate/candidate.json'))
scientific<-setNames(plan$reference_input_hashes,paste0('results/',names(plan$reference_input_hashes)))
extra<-c('candidate/candidate.json',paste0('candidate/',candidate$source_archive))
all_hashes<-c(scientific,setNames(vapply(file.path(reference,extra),hash,character(1)),extra))
stopifnot(all_hashes[['candidate/candidate.json']]==plan$candidate_file_sha256,
 all_hashes[[paste0('candidate/',candidate$source_archive)]]==candidate$source_sha256)
for(f in names(all_hashes))stopifnot(hash(file.path(reference,f))==all_hashes[[f]])
spec<-list(reference_directory=args[5],reference_hashes=as.list(all_hashes),code_only_upload=TRUE)
writeBin(charToRaw(paste0(jsonlite::toJSON(spec,auto_unbox=TRUE,pretty=TRUE),'\n')),file.path(bundle,'remote-inputs.json'))
files<-list.files(bundle,recursive=TRUE,full.names=FALSE)
stopifnot(!any(grepl('[.](rds|rda|RData|csv|parquet)$',files,ignore.case=TRUE)))
checks<-paste(vapply(file.path(bundle,files),hash,character(1)),files,sep='  ')
writeBin(charToRaw(paste0(paste(checks,collapse='\n'),'\n')),file.path(bundle,'bundle.sha256'))
archive<-file.path(artifact,'gru-timing-only-code-only.tar.gz');stopifnot(!file.exists(archive))
stopifnot(system2('tar',c('-czf',shQuote(archive),'-C',shQuote(bundle),'.'))==0)
writeBin(charToRaw(paste0(hash(archive),'  ',basename(archive),'\n')),file.path(artifact,'code-only-upload.sha256'))
cat('PASS source-only bundle:',length(files),'files; SHA256',hash(archive),'\n')
