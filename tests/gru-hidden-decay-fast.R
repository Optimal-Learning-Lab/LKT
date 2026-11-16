library(LKT)
args <- commandArgs(FALSE);file_arg <- args[grepl('^--file=',args)]
here <- if(length(file_arg))dirname(normalizePath(sub('^--file=','',file_arg[1L]),winslash='/')) else getwd()
source(file.path(here,'fixtures/gru-minimal-settings.R'))
ns <- asNamespace('LKT');f <- function(n)get(n,ns)
close <- function(a,b,tolerance=2e-7)stopifnot(isTRUE(all.equal(a,b,tolerance=tolerance,check.attributes=FALSE)))
if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed()) {
  if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required')
} else {
tensor <- function(x)torch::torch_tensor(x,dtype=torch::torch_float64())
# Independent uncompiled reference, using the existing GRU module and explicit
# eventwise equations. Production amplitude models always use the compiled scan.
reference <- function(net,interaction,state,s,days,valid) {
  posts <- priors <- vector('list',interaction$size(2))
  for(t in seq_len(interaction$size(2))) {
    mask <- valid[,t,drop=FALSE]$reshape(c(1L,interaction$size(1),1L))
    dt <- torch::torch_where(valid[,t,drop=FALSE],days[,t,drop=FALSE],days[,t,drop=FALSE]*0)$reshape(c(1L,interaction$size(1),1L))
    faded <- torch::torch_exp(-torch::nnf_softplus(net$hidden_decay$c)*dt)*state$dk
    rate <- net$hidden_decay$a*torch::torch_exp(net$hidden_decay$b*faded)
    before <- torch::torch_exp(-rate*dt)*state$h
    priors[[t]] <- before[s$num_layers,,,drop=FALSE]$permute(c(2,1,3))
    result <- f('lkt_sequence_module')(net,s)(interaction[,t,,drop=FALSE],before)
    next_dk <- faded+(before-state$h)+(result[[2]]-before)
    state <- list(h=torch::torch_where(mask,result[[2]],state$h),dk=torch::torch_where(mask,next_dk,state$dk))
    posts[[t]] <- state$h[s$num_layers,,,drop=FALSE]$permute(c(2,1,3))
  }
  list(torch::torch_cat(posts,dim=2),state,prior=torch::torch_cat(priors,dim=2))
}
set.seed(23)
d <- data.frame(Anon.Student.Id=rep(paste0('s',1:4),c(7,3,6,5)),
  KC..Default.=rep(c('a','b','a'),7),CF..ansbin.=rbinom(21,1,.6),z=seq(-1,1,length.out=21))
d$clock <- unlist(lapply(c(7,3,6,5),function(n)seq_len(n)*86400))
recipe <- list(components='KC..Default.',features='lineafm',fixedpars=numeric(),interacts=NA_character_,
  connectors=character(),interc=FALSE,maketimes=FALSE,subject_col='Anon.Student.Id')
for(all_inputs in c(FALSE,TRUE))for(retrieval in 0:1)for(bias in c(FALSE,TRUE)) {
  s <- test_gru_model(hidden_decay=TRUE,hidden_decay_time_col='clock',hidden_size=3L,num_layers=2L,
    head_hidden_sizes=4L,numeric_cols='z',embedding_dims=2L,epochs=1L,validation_fraction=0,
    early_stopping=FALSE,dtype='float64',input_to_all_layers=all_inputs,gru_bias=bias,retrieval_heads=retrieval,
    retrieval_filter_cols=if(retrieval)'KC..Default.' else character(),retrieval_size=3L,
    retrieval_window=4L,feature_encoder=if(retrieval)'mlp' else 'none',encoder_hidden_sizes=3L,encoder_output_size=2L)$settings
  cleanup <- f('lkt_gru_runtime')(s)
  prep <- f('lkt_gru_preprocess')(d,rep(TRUE,nrow(d)),recipe,s)
  encoded <- f('lkt_gru_encode')(d,prep,recipe,s)
  groups <- split(seq_len(nrow(d)),f('lkt_gru_keys')(d,s$history_group_cols))
  batch <- f('lkt_gru_batch')(encoded,d,groups,lapply(groups,seq_along),s)
  net <- f('lkt_gru_network')(prep,s);net$eval()
  run <- function()f('lkt_gru_forward')(net,batch$numeric,batch$categorical,batch$answers,
    NULL,NULL,batch$retrieval_ids,s,batch$decay_gaps,!is.na(batch$rows))
  torch::with_no_grad({net$hidden_decay$a$fill_(.2);net$hidden_decay$b$fill_(-.3);net$hidden_decay$c$fill_(.1)})
  objective <- function(o)o$logits$pow(2)$sum(dtype=torch::torch_float64())+
    o$hidden$h$pow(2)$sum(dtype=torch::torch_float64())+o$hidden$dk$pow(2)$sum(dtype=torch::torch_float64())
  compiled <- run();objective(compiled)$backward()
  gradients <- lapply(net$named_parameters(),function(x)if(is.null(x$grad))NULL else torch::as_array(x$grad))
  net$zero_grad()
  original <- f('lkt_gru_compiled_transition')
  assignInNamespace('lkt_gru_compiled_transition',reference,ns='LKT')
  reference_out <- tryCatch(run(),finally=assignInNamespace('lkt_gru_compiled_transition',original,ns='LKT'))
  close(torch::as_array(compiled$logits),torch::as_array(reference_out$logits),1e-10)
  for(n in c('h','dk'))close(torch::as_array(compiled$hidden[[n]]),torch::as_array(reference_out$hidden[[n]]),1e-10)
  objective(reference_out)$backward()
  for(n in names(gradients)) {
    g <- net$named_parameters()[[n]]$grad
    close(gradients[[n]],if(is.null(g))NULL else torch::as_array(g),1e-8)
  }
  net$zero_grad()
  # Independent finite differences on all three scalar controls at an interior point.
  for(n in c('a','b','c')) {
    parameter <- net$hidden_decay[[n]];value <- parameter$item();eps <- 1e-5
    net$zero_grad();objective(run())$backward();analytical <- parameter$grad$item()
    torch::with_no_grad({parameter$fill_(value+eps)});up <- objective(run())$item()
    torch::with_no_grad({parameter$fill_(value-eps)});down <- objective(run())$item()
    torch::with_no_grad({parameter$fill_(value)})
    close(analytical,(up-down)/(2*eps),2e-6)
  }
  # Exact null amplitude recovers stock even with nonzero history controls.
  torch::with_no_grad({net$hidden_decay$a$fill_(0)})
  zero <- run();stock <- s;stock$hidden_decay <- FALSE
  stock_net <- f('lkt_gru_network')(prep,stock)
  weights <- f('lkt_gru_weights')(net);weights <- weights[!grepl('^hidden_decay[.]',names(weights))]
  f('lkt_gru_load_weights')(stock_net,weights,stock);stock_net$eval()
  baseline <- f('lkt_gru_forward')(stock_net,batch$numeric,batch$categorical,batch$answers,NULL,NULL,batch$retrieval_ids,stock)
  keep <- !is.na(batch$rows)
  close(torch::as_array(zero$logits)[keep],torch::as_array(baseline$logits)[keep],1e-10)
  net$zero_grad()
  objective(run())$backward();stopifnot(is.finite(net$hidden_decay$a$grad$item()),abs(net$hidden_decay$a$grad$item())>1e-8)
  cleanup()
}
cat('PASS compiled/reference values and gradients, finite differences, padding and zero-decay stock equivalence for ordinary/all-layer GRUs with/without retrieval\n')

module <- f('lkt_gru_hidden_decay_module')('amplitude')$to(dtype=torch::torch_float64())
state <- list(h=tensor(array(1,c(1,1,2))),dk=tensor(array(.2,c(1,1,2))))
loss <- module(state,tensor(array(1,c(1,1,1))))$h$sum(dtype=torch::torch_float64());loss$backward()
stopifnot(module$a$grad$item()<0)
opt <- torch::optim_adamw(list(module$a),lr=.001,weight_decay=0);opt$step()
fake_net <- list(hidden_decay=module)
f('lkt_gru_project_hidden_decay')(fake_net,list(hidden_decay=TRUE,hidden_decay_parameterization='amplitude'))
stopifnot(module$a$item()>0)
torch::with_no_grad({module$a$fill_(-.1)})
f('lkt_gru_project_hidden_decay')(fake_net,list(hidden_decay=TRUE,hidden_decay_parameterization='amplitude'))
stopifnot(module$a$item()==0)
cat('PASS exact zero baseline, nonzero boundary gradient, learning away from zero and nonnegative optimizer projection\n')

for(mode in c('none','deterministic','gaussian'))for(retrieval in 0:1) {
  model <- test_gru_model(hidden_decay=TRUE,hidden_decay_time_col='clock',hidden_size=3L,num_layers=2L,
    head_hidden_sizes=4L,numeric_cols='z',embedding_dims=2L,epochs=2L,sequence_length=2L,batch_size=4L,
    validation_fraction=0,early_stopping=FALSE,inference_mode=mode,latent_size=2L,inference_hidden_sizes=3L,
    retrieval_heads=retrieval,retrieval_filter_cols=if(retrieval)'KC..Default.' else character(),
    retrieval_size=3L,retrieval_window=4L,feature_encoder=if(retrieval)'mlp' else 'none',encoder_hidden_sizes=3L,encoder_output_size=2L)
  fit <- LKT(d,components='KC..Default.',features='lineafm',model=model,verbose=FALSE)
  stopifnot(fit$model$settings$hidden_decay_parameterization=='amplitude',fit$model$weights[['hidden_decay.a']]>=0)
  runtime <- LKTGRURuntime(fit);empty <- LKTGRUState(fit)
  before <- serialize(f('lkt_gru_weights')(runtime$network),NULL)
  full <- LKTGRUStep(fit,empty,d,runtime)
  close(full$predictions,fit$prediction,3e-6)
  first <- LKTGRUStep(fit,empty,d[1:3,],runtime)
  rest <- LKTGRUStep(fit,first$state,d[4:7,],runtime)
  close(c(first$predictions,rest$predictions),full$predictions[1:7],3e-6)
  saved_state <- serialize(first$state,NULL)
  candidates <- d[c(4,5,6),]
  q <- LKTGRUScore(fit,first$state,candidates,runtime)
  close(q,rev(LKTGRUScore(fit,first$state,candidates[3:1,],runtime)),3e-6)
  stopifnot(identical(saved_state,serialize(first$state,NULL)))
  row <- d[4,,drop=FALSE];row$CF..ansbin. <- NA_real_
  pending <- LKTGRUStep(fit,first$state,row,runtime)
  finished <- LKTGRUStep(fit,pending$state,d[4,,drop=FALSE],runtime)
  direct <- LKTGRUStep(fit,first$state,d[4,,drop=FALSE],runtime)
  close(finished,direct,3e-6)
  path <- tempfile(fileext='.rds');saveRDS(list(fit=fit,state=first$state),path)
  saved <- readRDS(path);unlink(path)
  close(LKTGRUStep(saved$fit,saved$state,d[4:7,]),rest,3e-6)
  stopifnot(identical(before,serialize(f('lkt_gru_weights')(runtime$network),NULL)))
}
cat('PASS compiled fitting, all inference modes, retrieval, chunked/step replay, immutable candidates, pending completion and portable model/state loading\n')
}
