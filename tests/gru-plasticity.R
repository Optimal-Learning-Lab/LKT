library(LKT)
args <- commandArgs(FALSE);file_arg <- args[grepl('^--file=',args)]
here <- if(length(file_arg))dirname(normalizePath(sub('^--file=','',file_arg[1]),winslash='/')) else getwd()
source(file.path(here,'fixtures/gru-minimal-settings.R'))
f <- function(n)getFromNamespace(n,'LKT')
close <- function(a,b,tol=1e-9)stopifnot(isTRUE(all.equal(a,b,tolerance=tol,check.attributes=FALSE)))
for(mode in c('none','constant','fatigue')) {
  s<-GRUModel(plasticity=mode,plasticity_time_col=if(mode=='none')NULL else 'clock')$settings
  stopifnot(isTRUE(all.equal(f('lkt_gru_gui_decode')(as.list(f('lkt_gru_gui_encode')(s))),s)))
}
if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed()) {
  if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch required')
} else {
tensor <- function(x)torch::torch_tensor(x,dtype=torch::torch_float64())
# Independent R/Torch equations: compiled values AND gradients must agree.
ref <- function(base,z,y,dt,valid,v,e,eta,kappa,lr,fatigue) {
  logits<-list()
  for(t in seq_len(z$size(2))) {
    mask<-valid[,t,drop=FALSE];gap<-dt[,t,drop=FALSE]
    before<-if(fatigue)eta+(e-eta)*torch::torch_exp(-torch::torch_exp(lr)*gap) else e*0+eta
    features<-torch::torch_where(mask,z[,t,,drop=FALSE]$squeeze(2),v*0)
    logit<-base[,t,drop=FALSE]+(v*features)$sum(dim=2,keepdim=TRUE)
    delta<-before*(y[,t,drop=FALSE]-torch::torch_sigmoid(logit))*features
    after<-if(fatigue)before*(1-kappa*delta$abs()) else before
    v<-torch::torch_where(mask,v+delta,v);e<-torch::torch_where(mask,after,e)
    logits[[t]]<-logit
  }
  list(torch::torch_cat(logits,dim=2),v,e)
}
set.seed(4);base<-tensor(matrix(rnorm(10),2,5));z<-tensor(array(rnorm(30),c(2,5,3)))
y<-tensor(matrix(rep(c(0,1),5),2,5));dt<-tensor(matrix(runif(10),2,5))
valid<-torch::torch_tensor(rbind(rep(TRUE,5),c(TRUE,TRUE,TRUE,FALSE,FALSE)),dtype=torch::torch_bool())
for(mode in c('constant','fatigue')) {
  mod<-f('lkt_gru_plasticity_module')(mode)$to(dtype=torch::torch_float64())
  torch::with_no_grad({mod$eta$fill_(.2);if(mode=='fatigue')mod$kappa$fill_(12)})
  run<-function(compiled=TRUE) {
    zero<-tensor(matrix(0,2,3));fun<-if(compiled)f('lkt_gru_plasticity_scan')() else ref
    fun(base,z,y,dt,valid,zero,zero+mod$eta,mod$eta,
      if(mode=='fatigue')mod$kappa else mod$eta*0,
      if(mode=='fatigue')mod$log_rho else mod$eta*0,
      if(compiled)torch::jit_scalar(mode=='fatigue') else mode=='fatigue')
  }
  objective<-function(q)q[[1]]$pow(2)$sum()+q[[2]]$pow(2)$sum()+q[[3]]$pow(2)$sum()
  compiled<-run();objective(compiled)$backward()
  grads<-lapply(mod$parameters,function(p)p$grad$item());mod$zero_grad()
  other<-run(FALSE);for(j in 1:3)close(torch::as_array(compiled[[j]]),torch::as_array(other[[j]]))
  objective(other)$backward();close(grads,lapply(mod$parameters,function(p)p$grad$item()),1e-8)
  for(p in mod$parameters) {
    analytical<-p$grad$item();value<-p$item();eps<-1e-6
    torch::with_no_grad({p$fill_(value+eps)});up<-objective(run())$item()
    torch::with_no_grad({p$fill_(value-eps)});down<-objective(run())$item()
    torch::with_no_grad({p$fill_(value)})
    close(analytical,(up-down)/(2*eps),1e-6)
  }
  torch::with_no_grad({mod$eta$fill_(0)});mod$zero_grad();zero<-run()
  stopifnot(identical(torch::as_array(zero[[1]]),torch::as_array(base)))
  objective(zero)$backward();stopifnot(abs(mod$eta$grad$item())>1e-8)
}
cat('PASS independent compiled values/gradients, finite differences, signed fatigue, padding and zero-rate derivative.\n')
# A two-response task with a useful positive update learns directly from eta=0.
mod<-f('lkt_gru_plasticity_module')('constant')$to(dtype=torch::torch_float64())
base2<-tensor(matrix(0,1,2));z2<-tensor(array(1,c(1,2,1)));eta<-mod$eta
q<-f('lkt_gru_plasticity_scan')()(base2,z2,tensor(matrix(1,1,2)),base2,
  torch::torch_tensor(matrix(TRUE,1,2),dtype=torch::torch_bool()),tensor(matrix(0,1,1)),eta$reshape(c(1,1)),
  eta,eta*0,eta*0,torch::jit_scalar(FALSE))
loss<-torch::nnf_binary_cross_entropy_with_logits(q[[1]],tensor(matrix(1,1,2)))
loss$backward();stopifnot(eta$grad$item()<0)
opt<-torch::optim_adamw(mod$parameters,lr=.001,weight_decay=0);opt$step()
f('lkt_gru_project_plasticity')(list(plasticity=mod),list(plasticity='constant'))
stopifnot(eta$item()>0)
cat('PASS optimizer learns away from exact zero.\n')

set.seed(23)
d<-data.frame(Anon.Student.Id=rep(paste0('s',1:4),c(7,3,6,5)),KC..Default.=rep(c('a','b','a'),7),
 CF..ansbin.=rbinom(21,1,.6),z=seq(-1,1,length.out=21),clock=unlist(lapply(c(7,3,6,5),function(n)seq_len(n)*20)))
recipe<-list(components='KC..Default.',features='lineafm',fixedpars=numeric(),interacts=NA_character_,
 connectors=character(),interc=FALSE,maketimes=FALSE,subject_col='Anon.Student.Id')
for(retrieval in 0:1)for(mode in c('constant','fatigue')) {
  s<-test_gru_model(plasticity=mode,plasticity_time_col='clock',hidden_size=3L,num_layers=1L,
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
  stock<-s;stock$plasticity<-'none';stock$plasticity_time_col<-NULL
  baseline<-f('lkt_gru_network')(prep,stock);baseline$eval()
  w<-f('lkt_gru_weights')(net);f('lkt_gru_load_weights')(baseline,w[!grepl('^plasticity[.]',names(w))],stock)
  stopifnot(identical(torch::as_array(run(net,s)$logits),torch::as_array(run(baseline,stock)$logits)))
  # Preserve dropout RNG stream and all stock outputs in training mode too.
  net$train();baseline$train();torch::torch_manual_seed(101);a<-run(net,s)
  torch::torch_manual_seed(101);b<-run(baseline,stock)
  stopifnot(identical(torch::as_array(a$logits),torch::as_array(b$logits)))
  net$eval();torch::with_no_grad({net$plasticity$eta$fill_(.05)})
  a<-run(net,s);changed<-batch;changed$answers<-1-batch$answers
  b<-run(net,s,changed)
  close(torch::as_array(a$logits)[,1],torch::as_array(b$logits)[,1])
  stopifnot(max(abs(torch::as_array(a$logits)[,2]-torch::as_array(b$logits)[,2]))>1e-8)
  # Chunk continuity and finished-student state isolation.
  first<-f('lkt_gru_batch')(encoded,d,groups,lapply(groups,function(x)1:2),s)
  second<-f('lkt_gru_batch')(encoded,d,groups,lapply(groups,function(x)seq.int(3,length(x))),s)
  p<-run(net,s,first);q<-run(net,s,second,p$context,p$hidden)
  joined<-torch::torch_cat(list(p$logits,q$logits),dim=2)
  close(torch::as_array(joined)[!is.na(batch$rows)],torch::as_array(a$logits)[!is.na(batch$rows)])
  cleanup()
  fit<-LKT(d,components='KC..Default.',features='lineafm',model=do.call(GRUModel,s),verbose=FALSE)
  # Exercise nonzero state regardless of whether this tiny optimizer projects eta to 0.
  fit$model$weights[['plasticity.eta']][]<-.05
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
cat('PASS exact stock evaluation/training, causal feedback, chunks, fitting, retrieval, portable state, immutable scoring and fixed shared weights.\n')
}
