library(LKT)
args <- commandArgs(FALSE);file_arg <- args[grepl('^--file=',args)]
here <- if(length(file_arg))dirname(normalizePath(sub('^--file=','',file_arg[1]),winslash='/')) else getwd()
source(file.path(here,'fixtures/gru-minimal-settings.R'))
f <- function(n)getFromNamespace(n,'LKT')
close <- function(a,b,tol=1e-9)stopifnot(isTRUE(all.equal(a,b,tolerance=tol,check.attributes=FALSE)))
for(mode in c('none','constant')) {
 s<-GRUModel(gate_fatigue=TRUE,gate_fatigue_time_col='clock',plasticity=mode,
   plasticity_time_col=if(mode=='none')NULL else 'clock')$settings
 stopifnot(isTRUE(all.equal(f('lkt_gru_gui_decode')(as.list(f('lkt_gru_gui_encode')(s))),s)))
}
stopifnot(inherits(try(GRUModel(gate_fatigue=TRUE),silent=TRUE),'try-error'))
if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed()) {
 if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch required')
} else {
tensor<-function(x)torch::torch_tensor(x,dtype=torch::torch_float64())
mod<-f('lkt_gru_gate_module')()$to(dtype=torch::torch_float64())
# Zero weights, fixed candidate .8 and write gate .5: independent scalar recursion.
weights<-list(tensor(matrix(0,3,1)),tensor(matrix(0,3,1)),tensor(c(0,0,atanh(.8))),tensor(rep(0,3)))
scan<-function(h=0,a=1,gaps=c(0,0,2),mask=c(TRUE,TRUE,TRUE))f('lkt_gru_gate_scan')()(
 tensor(array(0,c(1,3,1))),tensor(array(h,c(1,1,1))),tensor(array(a,c(1,1,1))),
 tensor(matrix(gaps,1)),torch::torch_tensor(matrix(mask,1),dtype=torch::torch_bool()),
 mod$kappa,mod$log_rho,weights,torch::jit_scalar(1L),torch::jit_scalar(TRUE),torch::jit_scalar(0.),torch::jit_scalar(FALSE),torch::jit_scalar(FALSE))
torch::with_no_grad({mod$kappa$fill_(.4)})
h<-0;a<-1;hp<-numeric();prior<-numeric()
for(dt in c(0,0,2)) {
 ar<-1-(1-a)*exp(-log(2)*dt);prior<-c(prior,h)
 h<-h+ar*.5*(.8-h);a<-ar*exp(-.4*.5);hp<-c(hp,h)
}
q<-scan();close(as.numeric(torch::as_array(q[[1]])),hp);close(q[[3]]$item(),h);close(q[[4]]$item(),a)
# Activity fatigue persists with zero unit-strength change (candidate equals h).
steady<-scan(.8);close(steady[[3]]$item(),.8);close(steady[[4]]$item(),a);stopifnot(a<1,a>0)
# Longer rest recovers more; padding does not update either state.
stopifnot(scan(gaps=c(0,0,20))[[4]]$item()>scan(gaps=c(0,0,0))[[4]]$item())
padded<-scan(mask=c(TRUE,FALSE,FALSE));close(padded[[3]]$item(),.4);close(padded[[4]]$item(),exp(-.2))
# Reverse-mode gradients agree with independent central differences for both scalars.
objective<-function()scan()[[1]]$pow(2)$sum()
objective()$backward()
for(p in mod$parameters) {
 grad<-p$grad$item();v<-p$item();eps<-1e-6
 torch::with_no_grad({p$fill_(v+eps)});up<-objective()$item()
 torch::with_no_grad({p$fill_(v-eps)});down<-objective()$item()
 torch::with_no_grad({p$fill_(v)})
 close(grad,(up-down)/(2*eps),1e-6)
}
# The constrained kappa has a useful derivative at zero; ordinary Adam learns it.
torch::with_no_grad({mod$kappa$fill_(0)});mod$zero_grad();objective()$backward()
stopifnot(mod$kappa$grad$item()<0)
opt<-torch::optim_adamw(mod$parameters,lr=.001,weight_decay=0);opt$step()
f('lkt_gru_project_gate')(list(gate_fatigue=mod),list(gate_fatigue=TRUE));stopifnot(mod$kappa$item()>0)
cat('PASS independent scalar fatigue/recovery, zero-change activity, padding, gradients and learning away from zero.\n')
# Every supported recurrent weight layout, with and without biases.
for(all_inputs in c(FALSE,TRUE))for(bias in c(FALSE,TRUE)) {
 s<-test_gru_model(gate_fatigue=TRUE,gate_fatigue_time_col='clock',hidden_size=3L,num_layers=2L,
   input_to_all_layers=all_inputs,gru_bias=bias,gru_dropout=.2,dtype='float64')$settings
 recurrent<-f('lkt_gru_recurrent')(2L,s)$to(dtype=torch::torch_float64())
 net<-list(gru=recurrent,gate_fatigue=f('lkt_gru_gate_module')()$to(dtype=torch::torch_float64()),training=TRUE)
 x<-tensor(array(seq(-1,1,length.out=40),c(2,10,2)));state<-f('lkt_sequence_initial')(s,2L,x)
 mask<-torch::torch_tensor(matrix(TRUE,2,10),dtype=torch::torch_bool());dt<-tensor(matrix(1,2,10))
 torch::torch_manual_seed(75);stock<-recurrent(x,state$h)
 torch::torch_manual_seed(75);q<-f('lkt_gru_gate_transition')(net,x,state,s,dt,mask)
 cat('layout',all_inputs,bias,'difference',max(abs(torch::as_array(q[[1]])-torch::as_array(stock[[1]]))),'\n')
 close(torch::as_array(q[[1]]),torch::as_array(stock[[1]]),1e-10)
 close(torch::as_array(q[[2]]$h),torch::as_array(stock[[2]]),1e-10)
}
# Zero initialization of added layer inputs loads a recurrent submodule, not a
# complete network; it must not require gate/output parameters in that submodule.
s<-test_gru_model(gate_fatigue=TRUE,gate_fatigue_time_col='clock',plasticity='constant',
 plasticity_time_col='clock',hidden_size=3L,num_layers=2L,input_to_all_layers=TRUE,
 additional_input_init='zero',dtype='float64')$settings
stopifnot(length(f('lkt_gru_recurrent')(2L,s)$parameters)==8L)
cat('PASS multilevel gate layouts, biases, zero expansion and matched dropout.\n')
set.seed(23)
d<-data.frame(Anon.Student.Id=rep(paste0('s',1:4),c(7,3,6,5)),KC..Default.=rep(c('a','b','a'),7),
 CF..ansbin.=rbinom(21,1,.6),z=seq(-1,1,length.out=21),clock=unlist(lapply(c(7,3,6,5),function(n)seq_len(n)*20)))
recipe<-list(components='KC..Default.',features='lineafm',fixedpars=numeric(),interacts=NA_character_,
 connectors=character(),interc=FALSE,maketimes=FALSE,subject_col='Anon.Student.Id')
for(retrieval in 0:1)for(mode in c('none','constant')) {
  s<-test_gru_model(plasticity=mode,plasticity_time_col=if(mode=='none')NULL else 'clock',gate_fatigue=TRUE,gate_fatigue_time_col='clock',hidden_size=3L,num_layers=3L,gru_dropout=.1,
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
  net$eval();torch::with_no_grad({net$gate_fatigue$kappa$fill_(.4);if(mode!='none')net$plasticity$eta$fill_(.05)})
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
  fit$model$weights[['gate_fatigue.kappa']][]<-.4
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
