library(LKT)
args <- commandArgs(FALSE);file_arg <- args[grepl('^--file=',args)]
here <- if(length(file_arg))dirname(normalizePath(sub('^--file=','',file_arg[1]),winslash='/')) else getwd()
source(file.path(here,'fixtures/gru-minimal-settings.R'))
f <- function(n)getFromNamespace(n,'LKT')
close <- function(a,b,tol=1e-9)stopifnot(isTRUE(all.equal(a,b,tolerance=tol,check.attributes=FALSE)))
reject <- function(expr)stopifnot(inherits(tryCatch(force(expr),error=identity),'error'))
# This entire regression is intended for the authorized Big Blue environment.
if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed()) {
  if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch required')
} else {
tensor <- function(x,grad=FALSE)torch::torch_tensor(x,dtype=torch::torch_float64(),requires_grad=grad)
arrayof <- function(x)torch::as_array(x$detach()$cpu())
scan <- f('lkt_gru_bayesian_prototype_scan')()
mod <- f('lkt_gru_bayesian_prototypes_module')()$to(dtype=torch::torch_float64())
close(mod$eta$item(),0,0);close(arrayof(mod$log_rate_multipliers),seq(log(.25),log(4),length.out=20),0)
stopifnot(length(mod$parameters)==2L)
s <- GRUModel(plasticity='bayesian_prototypes',plasticity_time_col='clock')$settings
close(f('lkt_gru_gui_decode')(as.list(f('lkt_gru_gui_encode')(s))),s,0)
f('lkt_gru_validate_bayesian_prototype_weights')(list('plasticity.eta'=0,'plasticity.log_rate_multipliers'=rep(0,20)))
for(bad in list(list('plasticity.eta'=-1,'plasticity.log_rate_multipliers'=rep(0,20)),
 list('plasticity.eta'=0,'plasticity.log_rate_multipliers'=rep(0,19)),
 list('plasticity.eta'=0,'plasticity.log_rate_multipliers'=c(Inf,rep(0,19)))))
 reject(f('lkt_gru_validate_bayesian_prototype_weights')(bad))
set.seed(41)
raw_base <- matrix(rnorm(10),2,5);raw_z <- array(rnorm(30),c(2,5,3))
y <- tensor(matrix(rep(c(0,1),5),2,5))
valid <- torch::torch_tensor(rbind(rep(TRUE,5),c(TRUE,TRUE,TRUE,FALSE,FALSE)),dtype=torch::torch_bool())
zero <- tensor(array(0,c(2,20,3)));prior <- tensor(matrix(-log(20),2,20))
# Independent probability-space reference at moderate logits. It expresses the
# stated marginalization and Bayes rule directly, rather than copying the scan.
reference <- function(base,z,eta,d) {
 v <- zero;w <- torch::torch_exp(prior);logits <- list()
 for(t in 1:5) {
  mask <- valid[,t,drop=FALSE]
  features <- torch::torch_where(mask,z[,t,,drop=FALSE]$squeeze(2),z[,t,,drop=FALSE]$squeeze(2)*0)
  comp <- base[,t,drop=FALSE]+(v*features$unsqueeze(2))$sum(dim=3)
  p <- torch::torch_sigmoid(comp);mix <- (w*p)$sum(dim=2,keepdim=TRUE)
  logits[[t]] <- torch::torch_log(mix)-torch::torch_log1p(-mix)
  answer <- y[,t,drop=FALSE]
  after <- w*(answer*p+(1-answer)*(1-p));after <- after/after$sum(dim=2,keepdim=TRUE)
  w <- torch::torch_where(mask,after,w)
  delta <- (eta*torch::torch_exp(d))$reshape(c(1,20,1))*(answer-p)$unsqueeze(3)*features$unsqueeze(2)
  v <- torch::torch_where(mask$unsqueeze(3),v+delta,v)
 }
 list(torch::torch_cat(logits,dim=2),v,torch::torch_log(w))
}
original_objective <- function(q)q[[1]]$pow(2)$sum()+.03*q[[2]]$pow(2)$sum()+.001*q[[3]]$pow(2)$sum()
# Explicit reduction dtype and constants keep finite differences in float64.
# Bare R scalars interacting with scalar tensors can change arithmetic dtype.
objective <- function(q)q[[1]]$pow(2)$sum(dtype=torch::torch_float64())+
 tensor(.03)*q[[2]]$pow(2)$sum(dtype=torch::torch_float64())+
 tensor(.001)*q[[3]]$pow(2)$sum(dtype=torch::torch_float64())
run <- function(reference_mode=FALSE) {
 b <- tensor(raw_base,TRUE);z <- tensor(raw_z,TRUE);eta <- tensor(.13,TRUE)
 d <- tensor(seq(log(.25),log(4),length.out=20),TRUE)
 q <- if(reference_mode)reference(b,z,eta,d) else scan(b,z,y,valid,zero,prior,eta,d)
 objective(q)$backward()
 list(value=lapply(q,arrayof),gradient=lapply(list(b,z,eta,d),function(p)arrayof(p$grad)))
}
a <- run();b <- run(TRUE);close(a,b,2e-8)
# Finite difference for a rate coordinate includes prediction, posterior and
# component-adaptation paths through every subsequent observed event.
d <- tensor(seq(log(.25),log(4),length.out=20),TRUE);eta <- tensor(.13,TRUE)
q <- scan(tensor(raw_base),tensor(raw_z),y,valid,zero,prior,eta,d);objective(q)$backward()
analytical <- as.numeric(arrayof(d$grad))[7];eps <- 1e-6;values <- arrayof(d)
up <- values;up[7] <- up[7]+eps;down <- values;down[7] <- down[7]-eps
lossat <- function(ds)as.numeric(arrayof(objective(scan(tensor(raw_base),tensor(raw_z),y,valid,zero,prior,eta,tensor(ds)))))
original <- original_objective(q);precise <- objective(q)
stopifnot(precise$element_size()==8L)
cat(sprintf('DIAG Bayes q bytes=%s original term bytes=%s original objective bytes=%d item=%.17g array=%.17g fixed bytes=%d value=%.17g\n',
 paste(vapply(q,function(x)x$element_size(),numeric(1)),collapse=','),
 paste(vapply(q,function(x)x$pow(2)$sum()$element_size(),numeric(1)),collapse=','),
 original$element_size(),original$item(),as.numeric(arrayof(original)),precise$element_size(),as.numeric(arrayof(precise))))
cat(sprintf('DIAG Bayes coordinate7 analytic=%.17g reference_autodiff=%.17g objective=%.17g\n',analytical,as.numeric(b$gradient[[4]])[7],lossat(values)))
for(step in c(1e-3,1e-4,1e-5,1e-6,1e-7)) {
 diagnostic_up <- values;diagnostic_down <- values
 diagnostic_up[7] <- diagnostic_up[7]+step;diagnostic_down[7] <- diagnostic_down[7]-step
 positive <- lossat(diagnostic_up);negative <- lossat(diagnostic_down)
 numerical <- (positive-negative)/(2*step)
 cat(sprintf('DIAG Bayes eps=%.1e up=%.17g down=%.17g fd=%.17g abs_error=%.17g relative_error=%.17g\n',
  step,positive,negative,numerical,abs(numerical-analytical),abs(numerical-analytical)/abs(analytical)))
}
close(analytical,(lossat(up)-lossat(down))/(2*eps),1e-6)
# Unit multipliers recover constant-output adaptation values and all shared
# gradients; no special all-equal branch exists in production code.
parity <- function(prototypes) {
 b <- tensor(raw_base,TRUE);z <- tensor(raw_z,TRUE);eta <- tensor(.17,TRUE)
 q <- if(prototypes)scan(b,z,y,valid,zero,prior,eta,tensor(rep(0,20))) else
  f('lkt_gru_plasticity_scan')()(b,z,y,b*0,valid,tensor(matrix(0,2,3)),tensor(matrix(0,2,3)),eta,eta*0,eta*0,torch::jit_scalar(FALSE))
 loss <- torch::nnf_binary_cross_entropy_with_logits(q[[1]],y);loss$backward()
 list(logits=arrayof(q[[1]]),gradient=lapply(list(b,z,eta),function(p)arrayof(p$grad)))
}
close(parity(TRUE),parity(FALSE),1e-10)
q <- scan(tensor(raw_base),tensor(raw_z),y,valid,zero,prior,tensor(0),mod$log_rate_multipliers)
stopifnot(identical(arrayof(q[[1]]),raw_base))
# Every student's own state matches a separately processed history; trailing
# padding never changes its posterior/corrections. Chunk boundaries preserve both.
a <- scan(tensor(raw_base),tensor(raw_z),y,valid,zero,prior,tensor(.13),mod$log_rate_multipliers)
for(i in 1:2) {
 n <- if(i==1)5L else 3L
 b <- scan(tensor(raw_base[i,1:n,drop=FALSE]),tensor(raw_z[i,1:n,,drop=FALSE]),y[i,1:n,drop=FALSE],
  valid[i,1:n,drop=FALSE],zero[i,,,drop=FALSE],prior[i,,drop=FALSE],tensor(.13),mod$log_rate_multipliers)
 close(arrayof(a[[1]])[i,1:n],as.numeric(arrayof(b[[1]])))
 close(arrayof(a[[2]])[i,,,drop=FALSE],arrayof(b[[2]]));close(arrayof(a[[3]])[i,,drop=FALSE],arrayof(b[[3]]))
}
p <- scan(tensor(raw_base[,1:2]),tensor(raw_z[,1:2,,drop=FALSE]),y[,1:2],valid[,1:2],zero,prior,tensor(.13),mod$log_rate_multipliers)
q <- scan(tensor(raw_base[,3:5]),tensor(raw_z[,3:5,,drop=FALSE]),y[,3:5],valid[,3:5],p[[2]],p[[3]],tensor(.13),mod$log_rate_multipliers)
close(arrayof(a[[1]]),arrayof(torch::torch_cat(list(p[[1]],q[[1]]),dim=2)))
close(arrayof(a[[2]]),arrayof(q[[2]]));close(arrayof(a[[3]]),arrayof(q[[3]]))
# Finite log weights can differ by thousands even when exp(logw) underflows.
# Unknown outcomes never update state, and no posterior reset is permitted.
long <- 2000L
extreme_prior <- tensor(matrix(seq(-20000,0,length.out=20),1,20))
extreme_v <- tensor(array(seq(-1000,1000,length.out=20),c(1,20,1)))
q <- scan(tensor(matrix(0,1,long)),tensor(array(1,c(1,long,1))),tensor(matrix(1,1,long)),
 torch::torch_tensor(matrix(TRUE,1,long),dtype=torch::torch_bool()),extreme_v,extreme_prior,tensor(0),tensor(rep(0,20)))
stopifnot(all(is.finite(arrayof(q[[1]]))),all(is.finite(arrayof(q[[3]]))))
close(torch::torch_logsumexp(q[[3]],dim=2)$item(),0,1e-10)
stopifnot(diff(range(arrayof(q[[3]])))>10000)
q <- scan(tensor(matrix(0,1,1)),tensor(array(1,c(1,1,1))),tensor(matrix(NA_real_,1,1)),
 torch::torch_tensor(matrix(TRUE,1,1),dtype=torch::torch_bool()),extreme_v,extreme_prior,tensor(.2),tensor(rep(0,20)))
stopifnot(identical(arrayof(q[[2]]),arrayof(extreme_v)),identical(arrayof(q[[3]]),arrayof(extreme_prior)))
cat('PASS reference values/gradients, finite difference, constant-rate parity, causal isolation/chunks/padding, log-domain extreme posterior and unknown feedback.\n')

# End-to-end fit/replay/continuation and exact disabled RNG stream.
set.seed(23)
d <- data.frame(Anon.Student.Id=rep(paste0('s',1:3),c(6,4,5)),KC..Default.=rep(c('a','b','a'),5),
 CF..ansbin.=rbinom(15,1,.6),z=seq(-1,1,length.out=15),clock=unlist(lapply(c(6,4,5),function(n)seq_len(n)*20)))
settings <- test_gru_model(plasticity='bayesian_prototypes',plasticity_time_col='clock',hidden_size=3L,num_layers=3L,
 head_hidden_sizes=4L,numeric_cols='z',embedding_dims=2L,input_dropout=.1,gru_dropout=.1,head_dropout=.2,
 epochs=1L,sequence_length=2L,batch_size=3L,validation_fraction=0,early_stopping=FALSE,dtype='float64',
 retrieval_heads=1L,retrieval_filter_cols='KC..Default.',retrieval_size=3L,retrieval_window=4L)$settings
recipe <- list(components='KC..Default.',features='lineafm',fixedpars=numeric(),interacts=NA_character_,
 connectors=character(),interc=FALSE,maketimes=FALSE,subject_col='Anon.Student.Id')
cleanup <- f('lkt_gru_runtime')(settings)
prep <- f('lkt_gru_preprocess')(d,rep(TRUE,nrow(d)),recipe,settings)
encoded <- f('lkt_gru_encode')(d,prep,recipe,settings)
groups <- split(seq_len(nrow(d)),f('lkt_gru_keys')(d,settings$history_group_cols))
batch <- f('lkt_gru_batch')(encoded,d,groups,lapply(groups,seq_along),settings)
runnet <- function(net,s)f('lkt_gru_forward')(net,batch$numeric,batch$categorical,batch$answers,NULL,NULL,batch$retrieval_ids,s,batch$decay_gaps,!is.na(batch$rows))
stock <- settings;stock$plasticity <- 'none';stock$plasticity_time_col <- NULL
constant <- settings;constant$plasticity <- 'constant'
torch::torch_manual_seed(172);net <- f('lkt_gru_network')(prep,settings);rng_new <- torch::torch_rand(5)
torch::torch_manual_seed(172);baseline <- f('lkt_gru_network')(prep,stock);rng_stock <- torch::torch_rand(5)
close(arrayof(rng_new),arrayof(rng_stock),0)
w <- f('lkt_gru_weights')(net);close(w[!grepl('^plasticity[.]',names(w))],f('lkt_gru_weights')(baseline),0)
# eta=0 keeps baseline predictions and dropout RNG; disabled path is unchanged.
net$train();baseline$train();torch::torch_manual_seed(101);a <- runnet(net,settings);ra <- torch::torch_rand(5)
torch::torch_manual_seed(101);b <- runnet(baseline,stock);rb <- torch::torch_rand(5)
stopifnot(identical(arrayof(a$logits),arrayof(b$logits)));close(arrayof(ra),arrayof(rb),0)
# Whole-network constant parity includes shared recurrent/retrieval/head gradients.
constant_net <- f('lkt_gru_network')(prep,constant)
f('lkt_gru_load_weights')(constant_net,w[names(w)!='plasticity.log_rate_multipliers'],constant)
torch::with_no_grad({net$plasticity$log_rate_multipliers$fill_(0);net$plasticity$eta$fill_(.07);constant_net$plasticity$eta$fill_(.07)})
net$eval();constant_net$eval();net$zero_grad();constant_net$zero_grad()
a <- runnet(net,settings);b <- runnet(constant_net,constant);close(arrayof(a$logits),arrayof(b$logits),1e-10)
loss <- function(q) (torch::nnf_binary_cross_entropy_with_logits(q$logits,batch$answers,reduction='none')*batch$mask)$sum()
loss(a)$backward();loss(b)$backward()
for(n in names(constant_net$named_parameters()))close(arrayof(net$named_parameters()[[n]]$grad),arrayof(constant_net$named_parameters()[[n]]$grad),1e-9)
cleanup()
fit <- LKT(d,components='KC..Default.',features='lineafm',model=do.call(GRUModel,settings),verbose=FALSE)
fit$model$weights[['plasticity.eta']][] <- .07
runtime <- LKTGRURuntime(fit);empty <- LKTGRUState(fit)
frozen <- serialize(f('lkt_gru_weights')(runtime$network),NULL)
full <- LKTGRUStep(fit,empty,d,runtime)
enc <- f('lkt_gru_encode')(d,fit$model$preprocessing,fit$model$recipe,settings)
close(full$predictions,f('lkt_gru_replay')(runtime$network,enc,d,settings),2e-7)
initial <- LKTGRUStep(fit,empty,d[1:3,],runtime);snapshot <- serialize(initial$state,NULL)
score <- LKTGRUScore(fit,initial$state,d[4:5,],runtime)
close(score,rev(LKTGRUScore(fit,initial$state,d[5:4,],runtime)))
stopifnot(identical(snapshot,serialize(initial$state,NULL)))
pending <- d[4,,drop=FALSE];pending$CF..ansbin. <- NA_real_
p <- LKTGRUStep(fit,initial$state,pending,runtime)
stopifnot(identical(serialize(initial$state$inference_context,NULL),serialize(p$state$inference_context,NULL)))
close(LKTGRUStep(fit,p$state,d[4,,drop=FALSE],runtime),LKTGRUStep(fit,initial$state,d[4,,drop=FALSE],runtime))
path <- tempfile();saveRDS(list(fit=fit,state=initial$state),path);saved <- readRDS(path);unlink(path)
close(LKTGRUStep(saved$fit,saved$state,d[4:6,])$predictions,full$predictions[4:6])
stopifnot(identical(frozen,serialize(f('lkt_gru_weights')(runtime$network),NULL)))
# Saved posterior and correction dimensions are validated rather than reset.
bad <- initial$state;key <- names(bad$inference_context)[1L]
bad$inference_context[[key]]$prototype_log_weights <- array(0,c(1,1,19))
reject(LKTGRUStep(fit,bad,d[4,,drop=FALSE],runtime))
cat('PASS configuration, fixed shared initialization/RNG, whole-network constant gradients, fit/replay, saved continuation, immutable candidate scoring and strict state validation.\n')
}

