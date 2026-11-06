library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed()) {
  if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required')
} else {
ns <- function(n)getFromNamespace(n,'LKT')
close <- function(a,b,tolerance=3e-6)stopifnot(isTRUE(all.equal(a,b,tolerance=tolerance,check.attributes=FALSE)))
fixture_path <- if(file.exists('tests/fixtures/sequence-mamba.json'))'tests/fixtures/sequence-mamba.json' else 'fixtures/sequence-mamba.json'
f <- jsonlite::fromJSON(fixture_path,simplifyVector=FALSE)
arr <- function(v)array(unlist(v$values),unlist(v$shape))
tensor <- function(v)torch::torch_tensor(arr(v),dtype=torch::torch_float64())
s <- test_sequence_model('mamba',hidden_size=2L,num_layers=1L,dtype='float64',
  unit_options=list(d_state=2L,d_conv=2L,expand=1L))$settings
net <- ns('lkt_mamba_network')(3L,s)$to(dtype=torch::torch_float64());net$eval()
ns('lkt_gru_load_weights')(net,lapply(f$weights,arr),s)
input <- tensor(f$input)$requires_grad_(TRUE)
initial <- list(read=torch::torch_zeros(c(2,1,2),dtype=torch::torch_float64()),
  layers=list(list(conv=tensor(f$initial_conv),ssm=tensor(f$initial_ssm))))
out <- net(input,initial)
close(torch::as_array(out[[1L]]),arr(f$output),1e-10)
state <- initial
for(t in 1:5) {
  step <- net(input[,t,,drop=FALSE],state);state <- step[[2L]]
  close(torch::as_array(state$layers[[1]]$ssm),arr(f$ssm_steps[[t]]),1e-10)
  close(torch::as_array(state$layers[[1]]$conv),arr(f$conv_steps[[t]]),1e-10)
  close(torch::as_array(step[[1]]),arr(f$output)[,t,,drop=FALSE],1e-10)
}
(out[[1L]]$pow(2)$sum()/2)$backward()
close(torch::as_array(input$grad),arr(f$gradients$input),1e-7)
for(n in names(net$named_parameters())) {
  actual <- torch::as_array(net$named_parameters()[[n]]$grad);expected <- arr(f$gradients[[n]])
  if(max(abs(actual-expected))>1e-7)stop('Finite-difference gradient differs: ',n)
}
# Full and stepwise inference must agree for odd, even and long histories.
for(length in c(1L,2L,3L,4L,31L,100L,101L)) {
  x <- torch::torch_randn(c(2,length,3),dtype=torch::torch_float64())
  full <- net(x,initial);state <- initial;pieces <- vector('list',length)
  for(t in seq_len(length)) {
    one <- net(x[,t,,drop=FALSE],state);pieces[[t]] <- one[[1]];state <- one[[2]]
  }
  close(torch::as_array(full[[1]]),torch::as_array(torch::torch_cat(pieces,dim=2)),1e-10)
  close(torch::as_array(full[[2]]$layers[[1]]$ssm),torch::as_array(state$layers[[1]]$ssm),1e-10)
}
# Heterogeneous future-unit state: two leaves with different batch axes.
heterogeneous <- list(h=torch::torch_ones(c(2,3,4)),c=torch::torch_ones(c(3,4)))
selected <- ns('lkt_sequence_state_map')(heterogeneous,s,'subset',c(1L,3L),layout=list(h=2L,c=1L))
stopifnot(identical(selected$h$size(),c(2L,2L,4L)),identical(selected$c$size(),c(2L,4L)))
cat('PASS independent Mamba output, intermediate state, finite-difference gradients and heterogeneous state layout\n')
}
