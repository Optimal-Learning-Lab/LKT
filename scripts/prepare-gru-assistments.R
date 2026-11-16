# Prepare raw ASSISTments 2017 original, unassisted, single-skill interactions.
# Args: maintained ROOT, RAW_CSV, fresh OUTPUT.
args <- commandArgs(TRUE);stopifnot(length(args)==3L)
root <- normalizePath(args[1],winslash='/',mustWork=TRUE);input <- args[2];output <- args[3]
source(file.path(root,'scripts/load-current-lkt.R'))
source(file.path(root,'scripts/gru-encoder-study.R'))
suppressPackageStartupMessages(library(data.table))
stopifnot(!dir.exists(output))
required <- c('studentId','skill','problemId','action_num','startTime','endTime','correct',
  'original','hint','hintCount','scaffold','frIsHelpRequest')
stopifnot(all(required %in% names(fread(input,nrows=0))))
x <- fread(input,select=required);x[,source_row:=seq_len(.N)]
stopifnot(nrow(x)==942816L,uniqueN(x$studentId)==1709L,
  !anyNA(x),all(x$correct %in% 0:1),all(is.finite(x$startTime)),all(is.finite(x$endTime)))
setorder(x,studentId,startTime,action_num,source_row)
trace <- list()
record <- function(d,stage)trace[[length(trace)+1L]] <<- data.frame(stage=stage,
  responses=nrow(d),students=uniqueN(d$studentId),items=uniqueN(d$problemId),skills=uniqueN(d$skill))
record(x,'raw actions')
first <- x[,.SD[1L],by=.(studentId,problemId)]
bad <- first[original!=1L|scaffold!=0L|hint!=0L|hintCount!=0L|frIsHelpRequest!=0L,.(studentId,problemId)]
y <- x[original==1L & scaffold==0L & hint==0L & hintCount==0L & frIsHelpRequest==0L & nzchar(trimws(skill))]
record(y,'unassisted original responses')
y <- y[!bad,on=.(studentId,problemId)];record(y,'exclude initially assisted student-item pairs')
# Define item eligibility on the cleaned records, not on excluded scaffold/hint actions.
single <- y[,.(skills=uniqueN(skill)),by=problemId][skills==1L,problemId]
y <- y[problemId %in% single];record(y,'single observed skill per cleaned item')
iteration <- 0L
repeat {
  before <- nrow(y);iteration <- iteration+1L
  items <- y[,.(n=uniqueN(studentId)),by=problemId][n>=100L,problemId]
  y <- y[problemId %in% items]
  students <- y[,.(n=uniqueN(problemId)),by=studentId][n>=10L,studentId]
  y <- y[studentId %in% students];record(y,paste('threshold fixed-point',iteration))
  if(nrow(y)==before)break
}
stopifnot(nrow(y)>0L,all(y$endTime>=y$startTime),
  all(y[,.(n=uniqueN(studentId)),by=problemId]$n>=100L),
  all(y[,.(n=uniqueN(problemId)),by=studentId]$n>=10L))
# Original problems, including unscored assisted encounters, contribute exposures.
exposure <- x[original==1L,.(Anon.Student.Id=as.character(studentId),
  KC..Default.=as.character(problemId),CF..Time.=as.numeric(startTime),source_row)]
timing <- LKTGRUTimeFeatures(as.data.frame(exposure))
idx <- match(y$source_row,timing$source_row);stopifnot(!anyNA(idx))
d <- data.frame(Anon.Student.Id=as.character(y$studentId),KC..Default.=as.character(y$problemId),
  KC..Cluster.=as.character(y$skill),CF..Time.=as.numeric(y$startTime),CF..ansbin.=as.numeric(y$correct),
  Duration..sec.=as.numeric(y$endTime-y$startTime),comparison_source_row=y$source_row,
  gru_item_age_seconds=timing$gru_item_age_seconds[idx],
  gru_mean_prior_spacing_seconds=timing$gru_mean_prior_spacing_seconds[idx])
d$CF..reltime. <- getFromNamespace('practiceTime','LKT')(d)
d$Duration..sec. <- NULL
stopifnot(!anyDuplicated(d$comparison_source_row),all(is.finite(d$CF..reltime.)))
dir.create(output,recursive=TRUE)
study_write_rds(d,file.path(output,'prepared_data.rds'))
study_write_csv(do.call(rbind,trace),file.path(output,'filtering-stages.csv'))
jsonlite::write_json(list(dataset='ASSISTments 2017',raw_sha256=study_hash(input),
  raw_source='https://base.ustc.edu.cn/data/ASSISTment/anonymized_full_release_competition_dataset.zip',
  source_index='https://github.com/bigdata-ustc/EduData/blob/master/EduData/DataSet/download_data/download_data.py',
  publisher='https://sites.google.com/view/assistmentsdatamining/dataset',
  raw_rows=nrow(x),responses=nrow(d),students=uniqueN(d$Anon.Student.Id),
  items=uniqueN(d$KC..Default.),skills=uniqueN(d$KC..Cluster.),prepared_sha256=study_hash(file.path(output,'prepared_data.rds')),
  time='startTime is UNIX seconds, available before the current response; cumulative prior duration is shifted.',
  cleaning='Original unassisted responses; exclude initially assisted pairs; one skill per item among cleaned records; iterative >=100 distinct students/item and >=10 distinct items/student.',
  caveat='Explicit procedural approximation, not recovery of UNVaMP preprocessing. Its reported counts are 172200 responses, 1561 students, 548 items and 78 skills.',
  timing_exposures='All original-problem rows in the full raw log, including assisted encounters, before scored-row filtering.'),
  file.path(output,'provenance.json'),pretty=TRUE,auto_unbox=TRUE)
print(do.call(rbind,trace));cat('PASS ASSISTments prepared-data, causal timing and fixed-point thresholds.\n')
