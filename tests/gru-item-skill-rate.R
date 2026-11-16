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
schema<-list(parent=c(1L,1L,2L,2L),items=letters[1:4],skills=c('A','B'))
m<-f('lkt_gru_plasticity_module')('item_skill',schema)$to(dtype=torch::torch_float64())
stopifnot(all(torch::as_array(m$skill_rate)==0),all(torch::as_array(m$log_item)==0))
set.seed(84);base<-tensor(matrix(rnorm(10),2,5));z<-tensor(array(rnorm(30),c(2,5,3)));y<-tensor(matrix(rep(c(0,1),5),2,5))
valid<-torch::torch_tensor(rbind(rep(TRUE,5),c(TRUE,TRUE,TRUE,FALSE,FALSE)),dtype=torch::torch_bool())
ids<-array(2L,c(2,5,2));ids[,,1]<-matrix(c(2,3,4,5,2,4,3,5,2,3),2,5);ids[,,2]<-ifelse(ids[,,1]<4,2L,3L)
categories<-torch::torch_tensor(ids,dtype=torch::torch_long());zero<-tensor(matrix(0,2,3))
run<-function()f('lkt_gru_item_skill_scan')()(base,z,y,valid,zero,zero,categories,m$skill_rate,m$log_item,m$item_parent,m$counts)
objective<-function(q)q[[1]]$pow(2)$sum()+q[[2]]$pow(2)$sum()+q[[3]]$pow(2)$sum()
stopifnot(identical(torch::as_array(run()[[1]]),torch::as_array(base)))
torch::with_no_grad({m$skill_rate$fill_(.2)})
a<-run();b<-f('lkt_gru_plasticity_scan')()(base,z,y,base*0,valid,zero,zero+.2,tensor(.2),tensor(0),tensor(0),torch::jit_scalar(FALSE))
for(j in 1:3)close(torch::as_array(a[[j]]),torch::as_array(b[[j]]),0)
torch::with_no_grad({m$skill_rate[2]$fill_(.1);m$log_item$copy_(tensor(c(.2,-.2,.1,-.1)))})
reference<-function() {
 v<-zero;rates<-zero;ls<-list()
 centered<-m$log_item-torch::torch_stack(list(m$log_item[1:2]$mean(),m$log_item[1:2]$mean(),m$log_item[3:4]$mean(),m$log_item[3:4]$mean()))
 for(t in 1:5) {
  mask<-valid[,t,drop=FALSE];features<-torch::torch_where(mask,z[,t,,drop=FALSE]$squeeze(2),zero)
  logit<-base[,t,drop=FALSE]+(v*features)$sum(dim=2,keepdim=TRUE)
  rate<-(m$skill_rate[ids[,t,2]-1L]*torch::torch_exp(centered[ids[,t,1]-1L]))$unsqueeze(2)
  v<-torch::torch_where(mask,v+rate*(y[,t,drop=FALSE]-torch::torch_sigmoid(logit))*features,v)
  rates<-torch::torch_where(mask,rate$expand_as(rates),rates);ls[[t]]<-logit
 }
 list(torch::torch_cat(ls,dim=2),v,rates)
}
a<-run();objective(a)$backward();g<-lapply(m$parameters,function(x)torch::as_array(x$grad));m$zero_grad()
b<-reference();for(j in 1:3)close(torch::as_array(a[[j]]),torch::as_array(b[[j]]))
objective(b)$backward();close(g,lapply(m$parameters,function(x)torch::as_array(x$grad)))
for(p in m$parameters)for(j in seq_len(p$numel())) {
 value<-p[j]$item();grad<-p$grad[j]$item();eps<-1e-6
 torch::with_no_grad({p[j]$fill_(value+eps)});up<-objective(run())$item()
 torch::with_no_grad({p[j]$fill_(value-eps)});down<-objective(run())$item()
 torch::with_no_grad({p[j]$fill_(value)});close(grad,(up-down)/(2*eps),1e-6)
}
# Common offsets within one skill cancel exactly in normalized item multipliers.
a<-run();torch::with_no_grad({m$log_item[1:2]$add_(.7)})
for(j in 1:3)close(torch::as_array(a[[j]]),torch::as_array(run()[[j]]))
cat('PASS initial stock parity, nonzero constant parity, independent values/gradients, finite differences, normalization and padding.\n')
set.seed(23)
d<-data.frame(Anon.Student.Id=rep(paste0('s',1:4),c(7,3,6,5)),KC..Default.=rep(c('a','b','a'),7),
 CF..ansbin.=rbinom(21,1,.6),z=seq(-1,1,length.out=21),clock=unlist(lapply(c(7,3,6,5),function(n)seq_len(n)*20)))
d$skill<-ifelse(d$KC..Default.=='a','s1','s2')
recipe<-list(components='KC..Default.',features='lineafm',fixedpars=numeric(),interacts=NA_character_,
 connectors=character(),interc=FALSE,maketimes=FALSE,subject_col='Anon.Student.Id')
for(retrieval in 0:1)for(mode in c('item_skill')) {
  s<-test_gru_model(categorical_cols=c('KC..Default.','skill'),plasticity=mode,plasticity_time_col='clock',hidden_size=3L,num_layers=1L,
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
  net$eval();torch::with_no_grad({net$plasticity$skill_rate$fill_(.05)})
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
  fit$model$weights[['plasticity.skill_rate']][]<-.05
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
