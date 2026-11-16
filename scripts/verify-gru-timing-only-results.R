# Pure numeric/contract verification of a completed timing-only study.
# ROOT RESULTS AUDIT_OUTPUT FROZEN_STUDY_ROOT PREFLIGHT_STUDY_RDS
args<-commandArgs(TRUE);stopifnot(length(args)==5L)
root<-normalizePath(args[1],winslash='/',mustWork=TRUE);results<-args[2];audit<-args[3];frozen<-args[4]
.libPaths(c(file.path(root,'.codex-r-lib'),.libPaths()))
source(file.path(root,'scripts/gru-encoder-study.R'))
p<-readRDS(file.path(results,'study.rds'));planned<-readRDS(args[5])
for(n in c('settings','baseline','partitions','partition_hashes','seeds','numeric_names','data_sha256','reference_input_hashes','reference_protocol_sha256','candidate'))stopifnot(identical(p[[n]],planned[[n]]))
for(n in names(p$script_hashes))stopifnot(study_hash(file.path(frozen,n))==p$script_hashes[[n]],study_hash(file.path(root,n))==p$script_hashes[[n]])
stopifnot(study_hash(file.path(results,'data/prepared_data.rds'))==p$data_sha256)
for(part in p$partitions)stopifnot(study_hash(file.path(results,'partitions',paste0(part,'.rds')))==p$partition_hashes[[part]])
for(f in names(p$reference_generated_hashes))stopifnot(study_hash(file.path(results,f))==p$reference_generated_hashes[[f]])
for(f in grep('^models/',names(p$reference_input_hashes),value=TRUE))stopifnot(study_hash(file.path(results,'reference',f))==p$reference_input_hashes[[f]])
source(file.path(root,'scripts/report-gru-timing-only.R'))
r<-timing_only_report(results,p,'check')
stopifnot(!dir.exists(audit));dir.create(audit,recursive=TRUE)
for(n in names(r))study_write_csv(r[[n]],file.path(audit,paste0(gsub('_','-',n),'.csv')))
v<-jsonlite::fromJSON(file.path(results,'reports/verification.json'))
stopifnot(v$new_fits==15L,v$saved_control_fits==30L,v$phase_AUCs_checked==135L,v$pooled_AUCs_checked==9L)
v$local_preflight_matched<-TRUE;v$frozen_sources_matched<-TRUE
v$audit_source_sha256<-study_hash(sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)))
jsonlite::write_json(v,file.path(audit,'verification.json'),auto_unbox=TRUE,pretty=TRUE)
cat('PASS local numeric/contract audit, preflight and frozen hashes: 45 checkpoints, 135 phase AUCs, nine pooled AUCs.\n')
