# Prepare the current Cloze benchmark and verify its identities and filtering without
# fitting models. A new output folder keeps this audit separate from completed runs.
args <- commandArgs(TRUE)
stopifnot(length(args)==2L)
root <- normalizePath(args[1L],winslash='/',mustWork=TRUE)
output <- args[2L]
if(dir.exists(output) && length(list.files(output,all.files=TRUE,no..=TRUE)))
  stop('Use a new output directory for this audit.')
dir.create(output,recursive=TRUE,showWarnings=FALSE)
source(file.path(root,'scripts/load-current-lkt.R'))
suppressPackageStartupMessages({library(LKT);library(data.table)})
source(file.path(root,'scripts/gru-benchmark-data.R'))

# Exercise a cascade that requires several filtering passes. Repeated responses must
# not inflate distinct-person or distinct-item counts; only A and B should survive.
fixture <- data.table(Anon.Student.Id=c('A','A','B','B','C','C','D','D','C'),
  KC..Default.=c('i1','i2','i1','i2','i2','i3','i3','i4','i2'),KC..Cluster.='skill',
  comparison_source_row=1:9)
check <- filter_gru_cloze_data(fixture,2L,2L)
stopifnot(setequal(check$data$Anon.Student.Id,c('A','B')),nrow(check$data)==4L,
  max(check$stages$iteration)>=3L,nrow(check$removed)==5L,
  setequal(check$removed$source_row,5:9))

# Prepare the real data through the same function used by the training drivers.
# Reconstruct the inclusion decisions independently from unique identity pairs.
prepared <- prepare_gru_benchmark_data('largerawsample',root,output,workflow=NULL)
d <- prepared$data;original <- readRDS(prepared$data_file)
expected <- which(original$Outcome %in% c('CORRECT','INCORRECT'))
repeat {
  before <- expected
  pairs <- unique(original[expected,c('Anon.Student.Id','KC..Default.')])
  students <- names(which(table(pairs$Anon.Student.Id)>=10L))
  expected <- expected[original$Anon.Student.Id[expected] %in% students]
  pairs <- unique(original[expected,c('Anon.Student.Id','KC..Default.')])
  items <- names(which(table(pairs$KC..Default.)>=100L))
  expected <- expected[original$KC..Default.[expected] %in% items]
  if(identical(before,expected))break
}
stopifnot(identical(sort(d$comparison_source_row),expected),
  all(d$KC..Default.==original$KC..Default.[d$comparison_source_row]),
  all(d$KC..Cluster.==original$KC..Cluster.[d$comparison_source_row]),
  all(d$CF..ansbin.==as.integer(original$Outcome[d$comparison_source_row]=='CORRECT')),
  uniqueN(d$KC..Default.)==144L,uniqueN(d$KC..Cluster.)==36L,
  uniqueN(unique(d[,.(item=KC..Default.,skill=KC..Cluster.)])$item)==144L)

# Check the actual neural preprocessing and network, not just column names. Two
# separate embedding tables must be present, with training-only vocabularies of 144
# items and 36 skills, and both identities must survive the encoding step.
settings <- do.call(GRUModel,c(prepared$families$raw,
  list(embedding_dims=16L,hidden_size=32L,num_layers=3L,order_col='CF..Time.')))$settings
recipe <- list(subject_col='Anon.Student.Id')
# LKT's model interface passes an ordinary data frame to these backend functions.
model_data <- as.data.frame(d)
prep <- getFromNamespace('lkt_gru_preprocess','LKT')(model_data,d$fold==1L,recipe,settings)
encoded <- getFromNamespace('lkt_gru_encode','LKT')(model_data,prep,recipe,settings)
net <- getFromNamespace('lkt_gru_network','LKT')(prep,settings)
weights <- getFromNamespace('lkt_gru_weights','LKT')(net)
stopifnot(identical(names(prep$vocabularies),c('KC..Default.','KC..Cluster.')),
  identical(unname(lengths(prep$vocabularies)),c(144L,36L)),
  identical(dim(weights[['embeddings.0.weight']]),c(145L,16L)),
  identical(dim(weights[['embeddings.1.weight']]),c(37L,16L)),
  ncol(encoded$categorical)==2L,
  all(prep$vocabularies[[1L]][encoded$categorical[,1L]-1L]==d$KC..Default.),
  all(prep$vocabularies[[2L]][encoded$categorical[,2L]-1L]==d$KC..Cluster.))

# Run a short causal forward pass and backpropagate its loss without taking an
# optimizer step. Both embedding tables must participate in the computation, while
# changing the final answer must not change that answer's own prediction.
rows <- 1:4
numeric <- torch::torch_tensor(array(getFromNamespace('lkt_gru_numeric_rows','LKT')(encoded,rows),
  dim=c(1L,length(rows),length(prep$numeric_names))),dtype=torch::torch_float32())
categorical <- torch::torch_tensor(array(encoded$categorical[rows,,drop=FALSE],
  dim=c(1L,length(rows),2L)),dtype=torch::torch_long())
answers <- torch::torch_tensor(matrix(d$CF..ansbin.[rows],nrow=1L),dtype=torch::torch_float32())
net$eval();forward <- net(numeric,categorical,answers)
changed <- answers$clone();changed[1L,length(rows)] <- 1-answers[1L,length(rows)]
other <- net(numeric,categorical,changed)
stopifnot(max(abs(torch::as_array(forward$logits)-torch::as_array(other$logits)))==0)
loss <- torch::nnf_binary_cross_entropy_with_logits(forward$logits,answers)
loss$backward()
stopifnot(is.finite(loss$item()),
  any(abs(torch::as_array(net$embeddings[[1L]]$weight$grad))>0),
  any(abs(torch::as_array(net$embeddings[[2L]]$weight$grad))>0))

# Save the prepared table and its protocol so the audit is reviewable and every
# retained response can be traced back to the immutable original export.
saveRDS(as.data.frame(d),file.path(output,'prepared_data.rds'))
write.csv(prepared$partition,file.path(output,'participant_split.csv'),row.names=FALSE)
write.csv(prepared$counts,file.path(output,'split_counts.csv'),row.names=FALSE)
manifest <- list(preprocessing=prepared$preprocessing,families=prepared$families,
  split_seed=1L,split_fractions=c(.7,.15,.15),split_unit='whole participant; no stratification',
  input_data_sha256=digest::digest(prepared$data_file,algo='sha256',file=TRUE),
  preprocessing_source_sha256=digest::digest(file.path(root,'scripts/gru-benchmark-data.R'),algo='sha256',file=TRUE),
  status='preprocessing and encoding verified; no models trained')
saveRDS(manifest,file.path(output,'audit_protocol.rds'))
summary <- c('PASS: iterative cascade and duplicate-count checks.',
  'PASS: independent reconstruction of retained source rows and labels.',
  'PASS: 144 item and 36 skill identities preserved; every item maps to one skill.',
  'PASS: training vocabularies and both actual GRU embedding tables verified.',
  'PASS: both embeddings receive prediction-loss gradients; current-answer causality verified.',
  paste('Retained scored responses:',nrow(d)),paste('Retained participants:',uniqueN(d$Anon.Student.Id)),
  paste('Responses removed by thresholds:',sum(original$Outcome %in% c('CORRECT','INCORRECT'))-nrow(d)),
  paste('Minimum distinct items per participant:',min(prepared$data[,.(n=uniqueN(KC..Default.)),by=Anon.Student.Id]$n)),
  paste('Minimum distinct participants per item:',min(prepared$data[,.(n=uniqueN(Anon.Student.Id)),by=KC..Default.]$n)),
  'Split unchanged: 70/15/15 whole participants, seed 1. This is not the authors exact split.',
  'Published Cloze response count: 45115; inclusion difference remains unresolved.',
  'No model fitting, hyperparameter search, or cluster submission performed.')
writeLines(summary,file.path(output,'verification.txt'));cat(paste(summary,collapse='\n'),'\n')
