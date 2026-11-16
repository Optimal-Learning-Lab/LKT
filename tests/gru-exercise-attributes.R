library(LKT)
args <- commandArgs(FALSE);file_arg <- args[grepl('^--file=',args)]
here <- if(length(file_arg))dirname(normalizePath(sub('^--file=','',file_arg[1]),winslash='/')) else getwd()
source(file.path(here,'fixtures/gru-minimal-settings.R'))
source(file.path(here,'fixtures/gru-exercise-attributes-fixture.R'))
f <- function(n)getFromNamespace(n,'LKT')
close <- function(a,b,tol=1e-9)stopifnot(isTRUE(all.equal(a,b,tolerance=tol,check.attributes=FALSE)))
reject <- function(expr)stopifnot(inherits(tryCatch(force(expr),error=identity),'error'))
if(!requireNamespace('jsonlite',quietly=TRUE) || !requireNamespace('digest',quietly=TRUE))stop('EAKT artifact tests require jsonlite and digest.')
directory <- tempfile('synthetic-eakt-');fixture <- test_gru_exercise_fixture(directory)
z <- fixture$controls
settings <- test_gru_model(categorical_cols=c('item','skill'),embedding_dims=16L,zero_init_embedding_cols='item',
 exercise_attributes=z,numeric_cols='z',hidden_size=3L,num_layers=3L,head_hidden_sizes=4L,
 input_dropout=.1,gru_dropout=.1,head_dropout=.2,epochs=1L,sequence_length=2L,batch_size=2L,
 validation_fraction=0,early_stopping=FALSE,dtype='float64')$settings
close(f('lkt_gru_gui_decode')(as.list(f('lkt_gru_gui_encode')(settings))),settings,0)
saved <- f('lkt_gru_exercise_load')(settings)
stopifnot(length(saved$item_ids)==4L,length(saved$ability_ids)==2L,length(unique(vapply(saved$catalog,`[[`,character(1),'question')))==2L)
# Every mutation is rebound to its new external hashes when appropriate. Semantic
# validation must reject it independently of simple stale-file-hash rejection.
mutate <- function(artifact=fixture$artifact,provenance=fixture$provenance,controls=z) {
 a <- charToRaw(enc2utf8(fixture$json(artifact)));controls$artifact_sha256 <- f('lkt_gru_exercise_hash')(a)
 provenance$artifact_sha256 <- controls$artifact_sha256
 p <- charToRaw(enc2utf8(fixture$json(provenance)));controls$provenance_sha256 <- f('lkt_gru_exercise_hash')(p)
 f('lkt_gru_exercise_parse')(a,p,controls)
}
a <- fixture$artifact;a$model <- 'substitute-model';reject(mutate(artifact=a))
a <- fixture$artifact;a$items[[2]]$item_id <- a$items[[1]]$item_id;reject(mutate(artifact=a))
a <- fixture$artifact;a$items[[1]]$ability_ids <- list();reject(mutate(artifact=a))
a <- fixture$artifact;a$items[[1]]$ability_ids <- list('unknown');reject(mutate(artifact=a))
a <- fixture$artifact;a$items[[1]]$difficulty_rank <- 5;reject(mutate(artifact=a))
a <- fixture$artifact;a$items[[1]]$expected_seconds <- 101;reject(mutate(artifact=a))
a <- fixture$artifact;a$content_validation_complete <- FALSE;reject(mutate(artifact=a))
a <- fixture$artifact;a$items <- a$items[-1L];reject(mutate(artifact=a))
p <- fixture$provenance;p$prompts$attributes$text <- 'changed';reject(mutate(provenance=p))
p <- fixture$provenance;p$raw_outputs[[1]]$output_sha256 <- paste(rep('0',64),collapse='');reject(mutate(provenance=p))
p <- fixture$provenance;p$raw_outputs[[1]]$model <- 'substitute-model';reject(mutate(provenance=p))
p <- fixture$provenance;p$catalog[[1]]$participant <- 'forbidden';reject(mutate(provenance=p))
p <- fixture$provenance;p$reviews[[1]]$accepted_answer_match <- 'wrong';reject(mutate(provenance=p))
p <- fixture$provenance;p$raw_outputs <- p$raw_outputs[-1L];reject(mutate(provenance=p))
# Other legitimate study catalogs use their declared counts/hashes, rather than
# being forced to claim Cloze identities or a synthetic purpose. This relabelled
# toy exists only inside this test; the PFA study preflight rejects it separately.
other <- z;other$purpose <- 'study'
other_provenance <- fixture$provenance;other_provenance$purpose <- 'study'
other_saved <- mutate(provenance=other_provenance,controls=other)
stopifnot(identical(other_saved$item_ids,saved$item_ids))
invisible(GRUModel(exercise_attributes=other,categorical_cols=c('item','skill'),zero_init_embedding_cols='item'))
# The provenance purpose must still agree with the declared binding.
bad <- settings;bad$exercise_attributes$purpose <- 'study';reject(f('lkt_gru_exercise_load')(bad))
bad <- settings;bad$latent_denoising <- TRUE;reject(do.call(GRUModel,bad))
bad <- settings;bad$plasticity <- 'bayesian_prototypes';bad$plasticity_time_col <- 'clock';reject(do.call(GRUModel,bad))
bad <- settings;bad$exercise_attributes$artifact_sha256 <- paste(rep('0',64),collapse='');reject(f('lkt_gru_exercise_load')(bad))
bad_saved <- saved;bad_saved$ability_mask[2,1] <- 0;reject(f('lkt_gru_exercise_saved')(bad_saved,settings))
cat('PASS strict artifact/model/prompts/ranks/coverage/answer/provenance hashes, duplicate-text identity and independent-condition validation.\n')
if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed()) {
 if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch required')
} else {
arrayof <- function(x)torch::as_array(x$detach()$cpu())
tensor <- function(x,long=FALSE)torch::torch_tensor(x,dtype=if(long)torch::torch_long() else torch::torch_float64())
attribute <- f('lkt_gru_exercise_module')(saved)$to(dtype=torch::torch_float64())
padding <- f('lkt_gru_exercise_padding')(tensor(matrix(0,5L,16L)))$to(dtype=torch::torch_float64())
ids <- tensor(matrix(c(1L,2L,3L,5L),1L),TRUE)
net_stub <- list(exercise_attributes=attribute,embeddings=list(padding))
vector <- function()f('lkt_gru_exercise_vector')(net_stub,ids,1L,settings)
stopifnot(all(arrayof(vector())==0))
vector()$sum()$backward()
cat('DIAG EAKT retained padding gradient:',paste(format(as.numeric(arrayof(padding$weight$grad)),digits=17),collapse=','),'\n')
stopifnot(attribute$projection$weight$grad$abs()$sum()$item()>0,all(arrayof(attribute$difficulty$weight$grad)==0),
 all(arrayof(attribute$time$weight$grad)==0),all(arrayof(attribute$ability$weight$grad)==0),all(arrayof(padding$weight$grad)==0))
optimizer <- torch::optim_sgd(attribute$parameters,lr=.01);optimizer$step();optimizer$zero_grad();padding$zero_grad()
vector()$pow(2)$sum()$backward()
stopifnot(attribute$difficulty$weight$grad$abs()$sum()$item()>0,attribute$time$weight$grad$abs()$sum()$item()>0,
 attribute$ability$weight$grad$abs()$sum()$item()>0,all(arrayof(padding$weight$grad)==0),all(arrayof(vector())[,1,]==0))
# Independent mean pooling and affine equations, including multiple ability labels.
torch::with_no_grad({attribute$projection$weight$fill_(.1);attribute$projection$bias$fill_(.2)})
observed <- arrayof(vector())
for(t in 2:4) {
 item <- as.integer(arrayof(ids)[1,t])-1L
 a <- fixture$artifact$items[[item]]
 ab <- match(unlist(a$ability_ids),saved$ability_ids)
 expected <- .2+.1*sum(c(arrayof(attribute$difficulty$weight)[a$difficulty_rank,],
  colMeans(arrayof(attribute$ability$weight)[ab,,drop=FALSE]),arrayof(attribute$time$weight)[a$time_rank,]))
 close(observed[1,t,],rep(expected,16L))
}
# Data include both distinct recorded answer strings for item001. Duplicated
# question texts retain separate IDs and separate metadata rows.
d <- data.frame(Anon.Student.Id=rep(c('s1','s2'),each=4L),item=rep(saved$item_ids,2L),
 skill=rep(saved$kc_ids,2L),CF..ansbin.=c(1,0,1,0,0,1,0,1),z=seq(-1,1,length.out=8),
 question=rep(vapply(saved$catalog,`[[`,character(1),'question'),2L),answers=rep('yes|y',8L))
d$answers[5L] <- 'YES'
recipe <- list(components='skill',features='lineafm',fixedpars=numeric(),interacts=NA_character_,
 connectors=character(),interc=FALSE,maketimes=FALSE,subject_col='Anon.Student.Id')
cleanup <- f('lkt_gru_runtime')(settings)
prep <- f('lkt_gru_preprocess')(d,rep(TRUE,nrow(d)),recipe,settings)
encoded <- f('lkt_gru_encode')(d,prep,recipe,settings)
groups <- split(seq_len(nrow(d)),f('lkt_gru_keys')(d,settings$history_group_cols))
batch <- f('lkt_gru_batch')(encoded,d,groups,lapply(groups,seq_along),settings)
stock <- settings;stock$exercise_attributes <- NULL
stockprep <- prep;stockprep$exercise_attributes <- NULL
run <- function(net,s)f('lkt_gru_forward')(net,batch$numeric,batch$categorical,batch$answers,NULL,NULL,batch$retrieval_ids,s,batch$decay_gaps,!is.na(batch$rows))
torch::torch_manual_seed(172);net <- f('lkt_gru_network')(prep,settings)
torch::torch_manual_seed(172);baseline <- f('lkt_gru_network')(stockprep,stock)
w <- f('lkt_gru_weights')(net);bw <- f('lkt_gru_weights')(baseline)
common <- setdiff(names(bw),'embeddings.0.weight');close(w[common],bw[common],0)
stopifnot(identical(dim(w[['embeddings.0.weight']]),c(1L,16L)))
count <- function(net)sum(vapply(net$parameters,function(p)as.numeric(p$numel()),numeric(1)))
stopifnot(count(net)==count(baseline)-4L*16L+80L+8L*2L+400L)
net$eval();baseline$eval();stopifnot(identical(arrayof(run(net,settings)$logits),arrayof(run(baseline,stock)$logits)))
net$train();baseline$train();torch::torch_manual_seed(101);a <- run(net,settings);ra <- torch::torch_rand(5)
torch::torch_manual_seed(101);b <- run(baseline,stock);rb <- torch::torch_rand(5)
stopifnot(identical(arrayof(a$logits),arrayof(b$logits)));close(arrayof(ra),arrayof(rb),0)
# Disabled mode is exactly the historical network, with no attribute parameters,
# allocation or random draws. Construct it twice to test the opt-out contract.
torch::torch_manual_seed(71);off1 <- f('lkt_gru_network')(stockprep,stock);r1 <- torch::torch_rand(5)
torch::torch_manual_seed(71);off2 <- f('lkt_gru_network')(stockprep,stock);r2 <- torch::torch_rand(5)
stopifnot(!any(grepl('exercise_attributes',names(f('lkt_gru_weights')(off1)))))
close(f('lkt_gru_weights')(off1),f('lkt_gru_weights')(off2),0);close(arrayof(r1),arrayof(r2),0)
# A known-catalog item absent from fitting participants still has attributes.
newprep <- f('lkt_gru_preprocess')(d,d$item!='item004',recipe,settings)
stopifnot('item004'%in%newprep$vocabularies$item)
f('lkt_gru_encode')(d[d$item=='item004',],newprep,recipe,settings)
unknown <- d[1,,drop=FALSE];unknown$item <- 'unknown';reject(f('lkt_gru_encode')(unknown,prep,recipe,settings))
wrong <- d[1,,drop=FALSE];wrong$skill <- 'kc002';reject(f('lkt_gru_encode')(wrong,prep,recipe,settings))
missing <- d[1,,drop=FALSE];missing$skill <- NULL;reject(f('lkt_gru_encode')(missing,prep,recipe,settings))
cleanup()
fit <- LKT(d,components='skill',features='lineafm',model=do.call(GRUModel,settings),verbose=FALSE)
runtime <- LKTGRURuntime(fit);empty <- LKTGRUState(fit);full <- LKTGRUStep(fit,empty,d,runtime)
initial <- LKTGRUStep(fit,empty,d[1:2,],runtime);snapshot <- serialize(initial$state,NULL)
score <- LKTGRUScore(fit,initial$state,d[3:4,],runtime)
close(score,rev(LKTGRUScore(fit,initial$state,d[4:3,],runtime)))
stopifnot(identical(snapshot,serialize(initial$state,NULL)))
# Shared network and metadata cannot change during scoring.
weights <- serialize(f('lkt_gru_weights')(runtime$network),NULL)
path <- tempfile();saveRDS(list(fit=fit,state=initial$state),path);roundtrip <- readRDS(path);unlink(path)
# Saved inference consumes embedded raw artifacts, never the original paths.
unlink(c(z$artifact_file,z$provenance_file))
close(LKTGRUStep(roundtrip$fit,roundtrip$state,d[3:4,])$predictions,full$predictions[3:4])
stopifnot(identical(weights,serialize(f('lkt_gru_weights')(runtime$network),NULL)))
bad <- fit;bad$model$preprocessing$exercise_attributes$ability_mask[2,1] <- 0;reject(LKTGRURuntime(bad))
cat('PASS attribute equations/gradients/padding, common initialization/dropout parity, opt-out RNG, known-catalog new questions, strict unknown failure and portable saved inference.\n')
# Full frozen architecture parameter accounting uses synthetic144/36 content only.
large <- test_gru_exercise_fixture(file.path(directory,'large'),144L,36L)
s <- GRUModel(categorical_cols=c('item','skill'),zero_init_embedding_cols='item',exercise_attributes=large$controls,
 plasticity='constant',plasticity_time_col='clock',dtype='float64')$settings
attrs <- f('lkt_gru_exercise_load')(s)
p <- list(vocabularies=list(item=attrs$item_ids,skill=sort(unique(attrs$kc_ids),method='radix')),embedding_dims=c(16L,16L),
 numeric_names=c('time:a','time:b','time:available_b','lkt:a','lkt:b'),center=rep(0,5L),scale=rep(1,5L),engineered=NULL,exercise_attributes=attrs)
base_settings <- s;base_settings$exercise_attributes <- NULL;base_prep <- p;base_prep$exercise_attributes <- NULL
base <- f('lkt_gru_network')(base_prep,base_settings);active <- f('lkt_gru_network')(p,s)
stopifnot(count(base)==43042L,count(active)==41218L+8L*length(attrs$ability_ids))
cat('PASS full baseline43042 and EAKT41218+8B parameter counts, retaining only the original padding row.\n')
}
unlink(directory,recursive=TRUE)
