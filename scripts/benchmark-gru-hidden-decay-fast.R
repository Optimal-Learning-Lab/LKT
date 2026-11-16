# ROOT OUTPUT; compares the same transition and gradients in compiled and R scans.
a <- commandArgs(TRUE);stopifnot(length(a)==2L,!file.exists(a[2L]))
.libPaths(c(file.path(a[1L],'.codex-r-lib'),.libPaths()));library(LKT)
ns <- asNamespace('LKT');f <- function(n)get(n,ns)
source(file.path(a[1L],'tests/fixtures/gru-minimal-settings.R'))
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
s <- test_gru_model(hidden_decay=TRUE,hidden_decay_time_col='clock',hidden_size=32L,num_layers=3L,
  numeric_cols='z',embedding_dims=16L,head_hidden_sizes=128L)$settings
cleanup <- f('lkt_gru_runtime')(s);on.exit(cleanup())
# Transition timing excludes encoding/head/retrieval equally in both scans.
net <- torch::nn_module(initialize=function(){self$gru<-torch::nn_gru(64L,32L,3L,batch_first=TRUE,dropout=0)
  self$hidden_decay<-f('lkt_gru_hidden_decay_module')('amplitude')})()
net$eval();s$gru_dropout<-0
x <- torch::torch_randn(c(32,100,64));state <- list(h=torch::torch_randn(c(3,32,32)),dk=torch::torch_randn(c(3,32,32)))
days <- torch::torch_rand(c(32,100));valid <- torch::torch_ones(c(32,100),dtype=torch::torch_bool())
torch::with_no_grad({net$hidden_decay$a$fill_(.2);net$hidden_decay$b$fill_(-.3)})
run <- function(fn) {net$zero_grad();result<-fn(net,x,state,s,days,valid)
  (result[[1]]$pow(2)$mean()+result[[2]]$dk$pow(2)$mean())$backward();result}
compiled <- run(f('lkt_gru_compiled_transition'));plain<-run(reference)
stopifnot(max(abs(torch::as_array(compiled[[1]])-torch::as_array(plain[[1]])))<3e-6)
rows<-lapply(1:3,function(i)data.frame(repetition=i,compiled_seconds=system.time(run(f('lkt_gru_compiled_transition')))[['elapsed']],
  R_scan_seconds=system.time(run(reference))[['elapsed']]))
rows<-do.call(rbind,rows);write.csv(rows,a[2L],row.names=FALSE);print(rows)
cat('Transition-only median speedup',median(rows$R_scan_seconds)/median(rows$compiled_seconds),'\n')
