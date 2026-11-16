library(LKT)
args<-commandArgs(FALSE);file_arg<-args[grepl('^--file=',args)]
here<-if(length(file_arg))dirname(normalizePath(sub('^--file=','',file_arg[1]),winslash='/')) else getwd()
source(file.path(here,'fixtures/gru-minimal-settings.R'))
f<-function(n)getFromNamespace(n,'LKT')
close<-function(a,b,tol=1e-9)stopifnot(isTRUE(all.equal(a,b,tolerance=tol,check.attributes=FALSE)))
if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed()) {
 if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch required')
} else {
tensor<-function(x)torch::torch_tensor(x,dtype=torch::torch_float64())
mod<-f('lkt_gru_plasticity_module')('error_rate')$to(dtype=torch::torch_float64())
close(mod$eta$item(),0);close(mod$alpha$item(),0);close(mod$c$item(),0);stopifnot(length(mod$parameters)==3L)
s<-GRUModel(plasticity='error_rate',plasticity_time_col='clock')$settings
stopifnot(isTRUE(all.equal(f('lkt_gru_gui_decode')(as.list(f('lkt_gru_gui_encode')(s))),s)))
set.seed(71);base<-tensor(matrix(rnorm(10),2,5));z<-tensor(array(rnorm(30),c(2,5,3)));y<-tensor(matrix(rep(c(0,1),5),2,5))
valid<-torch::torch_tensor(rbind(rep(TRUE,5),c(TRUE,TRUE,TRUE,FALSE,FALSE)),dtype=torch::torch_bool())
zero<-tensor(matrix(0,2,3))
reference<-function() {
 v<-zero;rates<-zero+mod$eta;logits<-list()
 for(t in 1:5) {
  mask<-valid[,t,drop=FALSE];features<-torch::torch_where(mask,z[,t,,drop=FALSE]$squeeze(2),zero)
  logit<-base[,t,drop=FALSE]+(v*features)$sum(dim=2,keepdim=TRUE)
  error<-y[,t,drop=FALSE]-torch::torch_sigmoid(logit)
  v<-torch::torch_where(mask,v+rates*error*features,v)
  rates<-torch::torch_where(mask,rates+mod$alpha*error$abs()-mod$c,rates);logits[[t]]<-logit
 }
 list(torch::torch_cat(logits,dim=2),v,rates)
}
run<-function()f('lkt_gru_error_rate_scan')()(base,z,y,valid,zero,zero+mod$eta,mod$alpha,mod$c)
objective<-function(q)q[[1]]$pow(2)$sum()+q[[2]]$pow(2)$sum()+q[[3]]$pow(2)$sum()
torch::with_no_grad({mod$eta$fill_(.2);mod$alpha$fill_(.3);mod$c$fill_(.1)})
a<-run();objective(a)$backward();grad<-lapply(mod$parameters,function(p)p$grad$item());mod$zero_grad()
b<-reference();for(j in 1:3)close(torch::as_array(a[[j]]),torch::as_array(b[[j]]))
objective(b)$backward();close(grad,lapply(mod$parameters,function(p)p$grad$item()))
for(p in mod$parameters) {
 g<-p$grad$item();value<-p$item();eps<-1e-6
 torch::with_no_grad({p$fill_(value+eps)});up<-objective(run())$item()
 torch::with_no_grad({p$fill_(value-eps)});down<-objective(run())$item()
 torch::with_no_grad({p$fill_(value)});close(g,(up-down)/(2*eps),1e-6)
}
# The learned parameters are shared; cumulative eta is isolated by student.
a<-run();errors<-abs(torch::as_array(y)-plogis(torch::as_array(a[[1]])))
expected<-mod$eta$item()+mod$alpha$item()*rowSums(errors*torch::as_array(valid))-mod$c$item()*rowSums(torch::as_array(valid))
close(torch::as_array(a[[3]]),matrix(expected,2,3))
# alpha=c=0 exactly recovers the old constant-rate scan, including saved rates.
torch::with_no_grad({mod$alpha$fill_(0);mod$c$fill_(0)})
a<-run();b<-f('lkt_gru_plasticity_scan')()(base,z,y,base*0,valid,zero,zero+mod$eta,mod$eta,mod$eta*0,mod$eta*0,torch::jit_scalar(FALSE))
for(j in 1:3)close(torch::as_array(a[[j]]),torch::as_array(b[[j]]),0)
# Hand calculation proves delayed rate application and permits negative eta.
q<-f('lkt_gru_error_rate_scan')()(tensor(matrix(0,1,3)),tensor(array(1,c(1,3,1))),tensor(matrix(1,1,3)),
 torch::torch_tensor(matrix(TRUE,1,3),dtype=torch::torch_bool()),tensor(matrix(0,1,1)),tensor(matrix(.2,1,1)),tensor(.1),tensor(.3))
close(torch::as_array(q[[1]]),matrix(c(0,.1,.1-.05*(1-plogis(.1))),1,3))
stopifnot(q[[3]]$item()<0,q[[4]][1,1]$item()>0,q[[4]][1,3]$item()>0)
torch::with_no_grad({mod$eta$fill_(0)});mod$zero_grad();a<-run()
stopifnot(identical(torch::as_array(a[[1]]),torch::as_array(base)))
objective(a)$backward();stopifnot(all(vapply(mod$parameters,function(p)abs(p$grad$item())>1e-8,logical(1))))
torch::with_no_grad({mod$eta$fill_(-1);mod$alpha$fill_(-1);mod$c$fill_(-1)})
f('lkt_gru_project_plasticity')(list(plasticity=mod),s)
for(p in mod$parameters)close(p$item(),0)
cat('PASS compiled/reference values, all three parameter gradients, finite differences, cumulative student rates, padding, constant parity and signed delayed updates.\n')
# Each added parameter can learn from its zero boundary under the actual BCE
# objective and matched AdamW optimizer, on tasks that favor that parameter.
for(target in c('alpha','c')) {
 m<-f('lkt_gru_plasticity_module')('error_rate')$to(dtype=torch::torch_float64())
 answers<-tensor(matrix(if(target=='alpha')c(1,1,1) else c(1,0,1),1,3))
 zeros<-tensor(matrix(0,1,1))
 q<-f('lkt_gru_error_rate_scan')()(tensor(matrix(0,1,3)),tensor(array(1,c(1,3,1))),answers,
  torch::torch_tensor(matrix(TRUE,1,3),dtype=torch::torch_bool()),zeros,zeros+m$eta,m$alpha,m$c)
 loss<-torch::nnf_binary_cross_entropy_with_logits(q[[1]],answers);loss$backward()
 stopifnot(m[[target]]$grad$item()<0)
 optimizer<-torch::optim_adamw(m$parameters,lr=.001,weight_decay=1e-4);optimizer$step()
 f('lkt_gru_project_plasticity')(list(plasticity=m),s)
 stopifnot(m[[target]]$item()>0)
}
cat('PASS alpha and c each learn away from zero under BCE and matched AdamW.\n')
set.seed(23)
d<-data.frame(Anon.Student.Id=rep(paste0('s',1:4),c(7,3,6,5)),KC..Default.=rep(c('a','b','a'),7),
 CF..ansbin.=rbinom(21,1,.6),z=seq(-1,1,length.out=21),clock=unlist(lapply(c(7,3,6,5),function(n)seq_len(n)*20)))
recipe<-list(components='KC..Default.',features='lineafm',fixedpars=numeric(),interacts=NA_character_,
 connectors=character(),interc=FALSE,maketimes=FALSE,subject_col='Anon.Student.Id')
for(scope in c('recurrent_candidate'))for(retrieval in 0:1)for(mode in c('error_rate')) {
  s<-test_gru_model(plasticity=mode,plasticity_time_col=if(mode=='none')NULL else 'clock',gate_fatigue=TRUE,gate_fatigue_time_col='clock',gate_fatigue_mode='connection',connection_fatigue_scope=scope,connection_time_scale_range=c(.1,10),connection_time_scale_seed=20261108L,hidden_size=3L,num_layers=3L,gru_dropout=.1,
    head_hidden_sizes=4L,numeric_cols='z',embedding_dims=2L,input_dropout=.1,head_dropout=.2,epochs=2L,sequence_length=2L,batch_size=4L,
    validation_fraction=0,early_stopping=FALSE,dtype='float64',retrieval_heads=retrieval,
    retrieval_filter_cols=if(retrieval)'KC..Default.' else character(),retrieval_size=3L,retrieval_window=4L,
    feature_encoder=if(retrieval)'mlp' else 'none',encoder_hidden_sizes=3L,encoder_output_size=2L)$settings
  cleanup<-f('lkt_gru_runtime')(s)
  prep<-f('lkt_gru_preprocess')(d,rep(TRUE,nrow(d)),recipe,s)
  encoded<-f('lkt_gru_encode')(d,prep,recipe,s)
  groups<-split(seq_len(nrow(d)),f('lkt_gru_keys')(d,s$history_group_cols))
  batch<-f('lkt_gru_batch')(encoded,d,groups,lapply(groups,seq_along),s)
  net<-f('lkt_gru_network')(prep,s);net$eval();torch::with_no_grad({net$plasticity$eta$fill_(0);net$plasticity$alpha$fill_(0);net$plasticity$c$fill_(0)})
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
  net$eval();torch::with_no_grad({net$gate_fatigue$lambda$fill_(.9);if(mode!='none')net$plasticity$eta$fill_(.05);net$plasticity$alpha$fill_(.1);net$plasticity$c$fill_(.01)})
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
