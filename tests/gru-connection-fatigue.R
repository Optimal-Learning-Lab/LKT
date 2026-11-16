library(LKT)
args <- commandArgs(FALSE);file_arg <- args[grepl('^--file=',args)]
here <- if(length(file_arg))dirname(normalizePath(sub('^--file=','',file_arg[1]),winslash='/')) else getwd()
source(file.path(here,'fixtures/gru-minimal-settings.R'))
f <- function(n)getFromNamespace(n,'LKT')
close <- function(a,b,tol=1e-9)stopifnot(isTRUE(all.equal(a,b,tolerance=tol,check.attributes=FALSE)))
if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed()) {
 if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch required')
} else {
tensor<-function(x)torch::torch_tensor(x,dtype=torch::torch_float64())
recurrent<-torch::nn_gru(1L,1L,batch_first=TRUE)$to(dtype=torch::torch_float64())
mod<-f('lkt_gru_connection_module')(recurrent)$to(dtype=torch::torch_float64())
weights<-list(tensor(matrix(c(0,0,1),3,1)),tensor(matrix(c(0,0,.5),3,1)),tensor(rep(0,3)),tensor(rep(0,3)))
run<-function(mask=c(TRUE,TRUE,TRUE),dt=c(0,0,2),lam=mod$lambda)f('lkt_gru_connection_scan')()(
 tensor(array(1,c(1,3,1))),tensor(array(0,c(1,1,1))),tensor(matrix(1,1,6)),tensor(matrix(dt,1,3)),
 torch::torch_tensor(matrix(mask,1,3),dtype=torch::torch_bool()),lam,mod$log_rho,weights,
 torch::jit_scalar(1L),torch::jit_scalar(TRUE),torch::jit_scalar(0.),torch::jit_scalar(FALSE),torch::jit_scalar(FALSE))
torch::with_no_grad({mod$lambda$copy_(tensor(c(.9,.8,.7,.6,.5,.4)))})
# Independent numeric R recurrence, with separate lambda for every matrix entry.
a<-rep(1,6);h<-0;posts<-numeric();lam<-as.numeric(torch::as_array(mod$lambda))
for(dt in c(0,0,2)) {
 recovered<-1-(1-a)*exp(-log(2)*dt)
 ip<-c(0,0,1)*recovered[1:3];hp<-c(0,0,.5)*recovered[4:6]*h
 r<-plogis(ip[1]+hp[1]);z<-plogis(ip[2]+hp[2]);n<-tanh(ip[3]+r*hp[3]);h<-n+z*(h-n)
 a<-lam*recovered;posts<-c(posts,h)
}
q<-run();close(as.numeric(torch::as_array(q[[1]])),posts);close(as.numeric(torch::as_array(q[[4]])),a)
# Padding freezes both hidden and connection states, including the recovery clock.
pad<-run(c(TRUE,FALSE,FALSE));close(as.numeric(torch::as_array(pad[[4]])),lam)
# Different rest lengths recover from the same fatigued state.
stopifnot(run(dt=c(0,0,20))[[4]][1,3]$item()>run(dt=c(0,0,0))[[4]][1,3]$item())
objective<-function() {q<-run();q[[1]]$pow(2)$sum()+q[[4]]$pow(2)$sum()}
objective()$backward();eps<-1e-6
for(j in 1:6) {
 grad<-mod$lambda$grad[j]$item();v<-mod$lambda[j]$item()
 torch::with_no_grad({mod$lambda[j]$fill_(v+eps)});up<-objective()$item()
 torch::with_no_grad({mod$lambda[j]$fill_(v-eps)});down<-objective()$item()
 torch::with_no_grad({mod$lambda[j]$fill_(v)})
 close(grad,(up-down)/(2*eps),1e-6)
}
v<-mod$log_rho$item();grad<-mod$log_rho$grad$item()
torch::with_no_grad({mod$log_rho$fill_(v+eps)});up<-objective()$item()
torch::with_no_grad({mod$log_rho$fill_(v-eps)});down<-objective()$item()
torch::with_no_grad({mod$log_rho$fill_(v)});close(grad,(up-down)/(2*eps),1e-6)
# Useful derivative at lambda=1: ordinary optimization learns away from no fatigue.
torch::with_no_grad({mod$lambda$fill_(1)});mod$zero_grad();objective()$backward()
stopifnot(any(torch::as_array(mod$lambda$grad)>1e-8))
opt<-torch::optim_adamw(mod$parameters,lr=.001,weight_decay=0);opt$step()
f('lkt_gru_project_gate')(list(gate_fatigue=mod),list(gate_fatigue=TRUE,gate_fatigue_mode='connection'))
stopifnot(any(torch::as_array(mod$lambda)<1),all(torch::as_array(mod$lambda)>=0),all(torch::as_array(mod$lambda)<=1))
cat('PASS scalar reference, per-connection differences, recovery, padding, all lambda/rho gradients and learning away from one.\n')
set.seed(23)
d<-data.frame(Anon.Student.Id=rep(paste0('s',1:4),c(7,3,6,5)),KC..Default.=rep(c('a','b','a'),7),
 CF..ansbin.=rbinom(21,1,.6),z=seq(-1,1,length.out=21),clock=unlist(lapply(c(7,3,6,5),function(n)seq_len(n)*20)))
recipe<-list(components='KC..Default.',features='lineafm',fixedpars=numeric(),interacts=NA_character_,
 connectors=character(),interc=FALSE,maketimes=FALSE,subject_col='Anon.Student.Id')
for(retrieval in 0:1)for(mode in c('constant')) {
  s<-test_gru_model(plasticity=mode,plasticity_time_col=if(mode=='none')NULL else 'clock',gate_fatigue=TRUE,gate_fatigue_time_col='clock',gate_fatigue_mode='connection',hidden_size=3L,num_layers=3L,gru_dropout=.1,
    head_hidden_sizes=4L,numeric_cols='z',embedding_dims=2L,input_dropout=.1,head_dropout=.2,epochs=2L,sequence_length=2L,batch_size=4L,
    validation_fraction=0,early_stopping=FALSE,dtype='float64',retrieval_heads=retrieval,
    retrieval_filter_cols=if(retrieval)'KC..Default.' else character(),retrieval_size=3L,retrieval_window=4L,
    feature_encoder=if(retrieval)'mlp' else 'none',encoder_hidden_sizes=3L,encoder_output_size=2L)$settings
  cleanup<-f('lkt_gru_runtime')(s)
  prep<-f('lkt_gru_preprocess')(d,rep(TRUE,nrow(d)),recipe,s)
  encoded<-f('lkt_gru_encode')(d,prep,recipe,s)
  groups<-split(seq_len(nrow(d)),f('lkt_gru_keys')(d,s$history_group_cols))
  batch<-f('lkt_gru_batch')(encoded,d,groups,lapply(groups,seq_along),s)
  net<-f('lkt_gru_network')(prep,s);net$eval()
  run<-function(net,s,b=batch,context=NULL,hidden=NULL)f('lkt_gru_forward')(net,b$numeric,b$categorical,
    b$answers,hidden,context,b$retrieval_ids,s,b$decay_gaps,!is.na(b$rows))
  stock<-s;stock$plasticity<-'none';stock$plasticity_time_col<-NULL;stock$gate_fatigue<-FALSE;stock$gate_fatigue_time_col<-NULL
  baseline<-f('lkt_gru_network')(prep,stock);baseline$eval()
  w<-f('lkt_gru_weights')(net);f('lkt_gru_load_weights')(baseline,w[!grepl('^(plasticity|gate_fatigue)[.]',names(w))],stock)
  close(torch::as_array(run(net,s)$logits)[!is.na(batch$rows)],torch::as_array(run(baseline,stock)$logits)[!is.na(batch$rows)],1e-10)
  # Preserve dropout RNG stream and all stock outputs in training mode too.
  net$train();baseline$train();torch::torch_manual_seed(101);a<-run(net,s)
  torch::torch_manual_seed(101);b<-run(baseline,stock)
  close(torch::as_array(a$logits)[!is.na(batch$rows)],torch::as_array(b$logits)[!is.na(batch$rows)],1e-10)
  net$eval();torch::with_no_grad({net$gate_fatigue$lambda$fill_(.9);if(mode!='none')net$plasticity$eta$fill_(.05)})
  a<-run(net,s);changed<-batch;changed$answers<-1-batch$answers
  b<-run(net,s,changed)
  close(torch::as_array(a$logits)[,1],torch::as_array(b$logits)[,1])
  stopifnot(max(abs(torch::as_array(a$hidden$h)-torch::as_array(b$hidden$h)))>1e-8)
  # Chunk continuity and finished-student state isolation.
  first<-f('lkt_gru_batch')(encoded,d,groups,lapply(groups,function(x)1:2),s)
  second<-f('lkt_gru_batch')(encoded,d,groups,lapply(groups,function(x)seq.int(3,length(x))),s)
  p<-run(net,s,first);q<-run(net,s,second,p$context,p$hidden)
  joined<-torch::torch_cat(list(p$logits,q$logits),dim=2)
  close(torch::as_array(joined)[!is.na(batch$rows)],torch::as_array(a$logits)[!is.na(batch$rows)])
  cleanup()
  fit<-LKT(d,components='KC..Default.',features='lineafm',model=do.call(GRUModel,s),verbose=FALSE)
  # Exercise nonzero state regardless of whether this tiny optimizer projects eta to 0.
  fit$model$weights[['gate_fatigue.lambda']][]<-.9
  if(mode!='none')fit$model$weights[['plasticity.eta']][]<-.05
  runtime<-LKTGRURuntime(fit);empty<-LKTGRUState(fit)
  frozen<-serialize(f('lkt_gru_weights')(runtime$network),NULL)
  full<-LKTGRUStep(fit,empty,d,runtime)
  enc<-f('lkt_gru_encode')(d,fit$model$preprocessing,fit$model$recipe,s)
  close(full$predictions,f('lkt_gru_replay')(runtime$network,enc,d,s),2e-7)
  initial<-LKTGRUStep(fit,empty,d[1:3,],runtime);snapshot<-serialize(initial$state,NULL)
  score<-LKTGRUScore(fit,initial$state,d[4:6,],runtime)
  close(score,rev(LKTGRUScore(fit,initial$state,d[6:4,],runtime)))
  stopifnot(identical(snapshot,serialize(initial$state,NULL)))
  pending<-d[4,,drop=FALSE];pending$CF..ansbin.<-NA_real_
  p<-LKTGRUStep(fit,initial$state,pending,runtime)
  close(LKTGRUStep(fit,p$state,d[4,,drop=FALSE],runtime),LKTGRUStep(fit,initial$state,d[4,,drop=FALSE],runtime))
  path<-tempfile();saveRDS(list(fit=fit,state=initial$state),path);saved<-readRDS(path);unlink(path)
  close(LKTGRUStep(saved$fit,saved$state,d[4:7,])$predictions,full$predictions[4:7])
  stopifnot(identical(frozen,serialize(f('lkt_gru_weights')(runtime$network),NULL)))
}
cat('PASS stock evaluation/training within float64 tolerance, causal feedback, chunks, fitting, retrieval, portable state, immutable scoring and fixed shared weights.\n')
}
