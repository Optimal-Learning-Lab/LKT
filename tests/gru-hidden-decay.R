library(LKT)
args <- commandArgs(FALSE)
file_arg <- args[grepl('^--file=',args)]
here <- if(length(file_arg))dirname(normalizePath(sub('^--file=','',file_arg[1L]),winslash='/')) else getwd()
source(file.path(here,'fixtures/gru-minimal-settings.R'))
ns <- asNamespace('LKT');f <- function(n)get(n,ns)
close <- function(a,b,tolerance=3e-6)
  stopifnot(isTRUE(all.equal(a,b,tolerance=tolerance,check.attributes=FALSE)))
fail <- function(expr,pattern) {
  e <- tryCatch({force(expr);NULL},error=identity)
  stopifnot(inherits(e,'error'),grepl(pattern,conditionMessage(e),fixed=TRUE))
}
fail(GRUModel(hidden_decay=TRUE),'explicit hidden_decay_time_col')
fail(GRUModel(hidden_decay=NA),'TRUE or FALSE')
fail(GRUModel(hidden_decay=TRUE,hidden_decay_time_col=character()),'hidden_decay_time_col')
fail(GRUModel(hidden_decay=TRUE,hidden_decay_time_col='CF..ansbin.'),'Responses')
fail(test_sequence_model('mamba',hidden_decay=TRUE,hidden_decay_time_col='clock'),'only for the GRU')
settings <- test_gru_model(hidden_decay=TRUE,hidden_decay_time_col='clock')$settings
close(f('lkt_gru_gui_decode')(as.list(f('lkt_gru_gui_encode')(settings))),settings)
cfg <- lkt_model_config(data.frame(component='KC..Default.',feature='lineafm'),
  backend='GRU',gru_options=settings)
stopifnot(identical(eval(lkt_config_call(cfg)$model)$settings,settings))
cat('PASS hidden decay explicit configuration, GUI roundtrip and architecture validation\n')

if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed()) {
  if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required')
} else {
tensor <- function(x)torch::torch_tensor(x,dtype=torch::torch_float64())
module <- f('lkt_gru_hidden_decay_module')()$to(dtype=torch::torch_float64())
torch::with_no_grad({module$a$fill_(.1);module$b$fill_(.6);module$c$fill_(-.2)})
h <- array(c(.5,-.5,0),c(1L,1L,3L));D <- array(c(.2,-.3,.1),dim(h))
state <- list(h=tensor(h),dk=tensor(D));days <- tensor(array(2,c(1L,1L,1L)))
softplus <- function(x)log1p(exp(x))
F <- exp(-softplus(-.2)*2)*D;r <- softplus(.1+.6*F)
H <- exp(-r*2)*h;K <- F+(H-h)
out <- module(state,days)
close(torch::as_array(out$h),H,1e-12);close(torch::as_array(out$dk),K,1e-12)
stopifnot(all(r>0),all(abs(H)<=abs(h)),all(sign(H)==sign(h)))
zero <- module(state,days*0)
close(torch::as_array(zero$h),h,1e-12);close(torch::as_array(zero$dk),D,1e-12)
# Decay changes can cancel either sign of old DK, rather than accumulate magnitude.
torch::with_no_grad({module$a$fill_(0);module$b$fill_(0);module$c$fill_(0)})
cancel <- module(list(h=tensor(array(c(1,-1),c(1,1,2))),
  dk=tensor(array(c(1,-1),c(1,1,2)))),tensor(array(1,c(1,1,1))))
close(torch::as_array(cancel$dk),array(0,c(1,1,2)),1e-12)
# Old contributions fade even when h=0; no magnitude or sign switch is introduced.
fade <- module(list(h=state$h*0,dk=state$dk),days)
close(torch::as_array(fade$dk),D/4,1e-12)
torch::with_no_grad({module$a$fill_(.1);module$b$fill_(.6);module$c$fill_(-.2)})
# R/Torch scalar arithmetic can promote a zero-dimensional double reduction to
# the default float32 dtype. Weight before reduction to keep this objective double.
objective <- function() {
  o <- module(state,days)
  (o$h+.2*o$dk)$sum(dtype=torch::torch_float64())
}
module$zero_grad();objective()$backward()
for(n in c('a','b','c')) {
  p <- module[[n]];v <- p$item();analytical <- p$grad$item();eps <- 1e-5
  torch::with_no_grad({p$fill_(v+eps)});up <- objective()$item()
  torch::with_no_grad({p$fill_(v-eps)});down <- objective()$item()
  torch::with_no_grad({p$fill_(v)})
  stopifnot(is.finite(analytical),abs(analytical)>1e-8)
  close(analytical,(up-down)/(2*eps),1e-7)
}
cat('PASS signed hidden/DK equations, cancellation, zero gaps, fading and finite-difference gradients\n')

set.seed(123)
d <- data.frame(Anon.Student.Id=rep(paste0('s',1:4),c(7,3,6,5)),
  KC..Default.=rep(c('a','b','a'),7),KC..Cluster.=rep(c('k1','k2','k1'),7),
  CF..ansbin.=rbinom(21,1,.6),ability=seq(-1,1,length.out=21))
d$clock <- unlist(lapply(c(7,3,6,5),function(n)cumsum(c(0,rep(c(86400,0,172800),length.out=n-1L)))))
fit_model <- function(data=d,mode='none',retrieval=0L,engineered=FALSE,all_layers=FALSE,unit='GRU') {
  options <- list(hidden_decay=TRUE,hidden_decay_time_col='clock',hidden_decay_parameterization='softplus',hidden_size=3L,
    num_layers=2L,head_hidden_sizes=4L,embedding_dims=2L,epochs=2L,batch_size=4L,
    sequence_length=2L,numeric_cols='ability',validation_fraction=0,early_stopping=FALSE,
    inference_mode=mode,latent_size=2L,inference_hidden_sizes=3L,
    temporal_kl_beta=if(mode=='gaussian').01 else 0,
    use_lkt_features=engineered,retrieval_heads=retrieval,retrieval_size=3L,
    retrieval_window=4L,retrieval_filter_cols=if(retrieval)'KC..Default.' else character(),
    feature_encoder=if(retrieval)'mlp' else 'none',encoder_hidden_sizes=3L,encoder_output_size=2L,
    input_to_all_layers=all_layers,seed=19L)
  adapter <- if(unit=='GRU')do.call(test_gru_model,options) else {
    options$input_to_all_layers <- NULL
    do.call(test_sequence_model,c(list(unit='gru'),options))
  }
  LKT(data,features='lineafm',components='KC..Default.',model=adapter,verbose=FALSE)
}
for(mode in c('none','deterministic','gaussian'))for(retrieval in 0:1) {
  fit <- fit_model(mode=mode,retrieval=retrieval,engineered=retrieval==1L,
    all_layers=retrieval==1L)
  s <- fit$model$settings;runtime <- LKTGRURuntime(fit);empty <- LKTGRUState(fit)
  weights <- serialize(fit$model$weights,NULL)
  runtime_weights <- serialize(f('lkt_gru_weights')(runtime$network),NULL)
  full <- LKTGRUStep(fit,empty,d,runtime)
  close(full$predictions,fit$prediction)
  names_decay <- paste0('hidden_decay.',c('a','b','c'))
  stopifnot(all(names_decay %in% names(fit$model$weights)),
    all(vapply(fit$model$weights[names_decay],function(x)all(is.finite(x)) && any(x!=0),logical(1))),
    !'time:log1p_gap_seconds'%in%fit$model$preprocessing$numeric_names,
    !any(grepl('clock',fit$model$preprocessing$numeric_names,fixed=TRUE)))
  # Histories evaluated alone match padded batches. Their DK starts independently.
  for(idx in split(seq_len(nrow(d)),d$Anon.Student.Id)) {
    solo <- LKTGRUStep(fit,empty,d[idx,],runtime)
    close(solo$predictions,full$predictions[idx])
    key <- f('lkt_gru_keys')(d[idx[1L],,drop=FALSE],s$history_group_cols)
    close(solo$state$hidden[[key]],full$state$hidden[[key]])
  }
  first <- LKTGRUStep(fit,empty,d[1:3,],runtime)
  tail <- LKTGRUStep(fit,first$state,d[4:7,],runtime)
  close(c(first$predictions,tail$predictions),full$predictions[1:7])
  key <- f('lkt_gru_keys')(d[1,,drop=FALSE],s$history_group_cols)
  close(tail$state$hidden[[key]],full$state$hidden[[key]])
  # Tie at rows 2 and 3: no fading/decay, so DK change equals GRU activation change.
  two <- LKTGRUStep(fit,empty,d[1:2,],runtime)
  three <- LKTGRUStep(fit,two$state,d[3,,drop=FALSE],runtime)
  close(three$state$hidden[[key]]$dk-two$state$hidden[[key]]$dk,
    three$state$hidden[[key]]$h-two$state$hidden[[key]]$h)
  before <- serialize(first$state,NULL)
  broken <- first$state;broken$last_decay_time <- NULL
  fail(LKTGRUStep(fit,broken,d[4,,drop=FALSE],runtime),'committed timestamp')
  broken <- first$state;broken$last_decay_time[[key]] <- Inf
  fail(LKTGRUStep(fit,broken,d[4,,drop=FALSE],runtime),'finite numeric scalars')
  candidates <- d[c(4,5,6),]
  p <- LKTGRUScore(fit,first$state,candidates,runtime)
  close(p,rev(LKTGRUScore(fit,first$state,candidates[3:1,],runtime)))
  stopifnot(identical(before,serialize(first$state,NULL)))
  pending_row <- d[4,,drop=FALSE];pending_row$CF..ansbin. <- NA_real_
  pending <- LKTGRUStep(fit,first$state,pending_row,runtime)
  close(pending$predictions,p[1])
  close(pending$state$hidden,first$state$hidden)
  stopifnot(identical(pending$state$last_decay_time,first$state$last_decay_time))
  completed <- LKTGRUStep(fit,pending$state,d[4,,drop=FALSE],runtime)
  direct <- LKTGRUStep(fit,first$state,d[4,,drop=FALSE],runtime)
  close(completed,direct)
  changed <- d[4:7,];changed$CF..ansbin.[1L] <- 1-changed$CF..ansbin.[1L]
  causal <- LKTGRUStep(fit,first$state,changed,runtime)
  close(causal$predictions[1L],tail$predictions[1L])
  close(predict_lkt(fit,d[4:7,],state=first$state)$predictions,tail$predictions)
  path <- tempfile(fileext='.rds');saveRDS(list(fit=fit,state=first$state),path)
  saved <- readRDS(path);unlink(path)
  close(LKTGRUStep(saved$fit,saved$state,d[4:7,]),tail)
  stopifnot(identical(weights,serialize(fit$model$weights,NULL)),
    identical(runtime_weights,serialize(f('lkt_gru_weights')(runtime$network),NULL)))
}
seq_fit <- fit_model(unit='Sequence')
close(LKTSequenceStep(seq_fit,LKTSequenceState(seq_fit),d)$predictions,seq_fit$prediction)
cat('PASS training of all three parameters, all inference modes, retrieval, student isolation, causal/pending/immutable scoring and portable continuation\n')

# Independently vary replay chunk length, not training, to verify identical paths.
for(chunk in c(1L,3L,100L)) {
  replay_settings <- s;replay_settings$sequence_length <- chunk
  encoded <- f('lkt_gru_encode')(d,fit$model$preprocessing,fit$model$recipe,s)
  close(f('lkt_gru_replay')(runtime$network,encoded,d,replay_settings),full$predictions)
}
# Compare padded scan's final h/DK with per-history scans, including finished rows.
groups <- split(seq_len(nrow(d)),f('lkt_gru_keys')(d,s$history_group_cols))
b <- f('lkt_gru_batch')(encoded,d,groups,lapply(groups,seq_along),s)
torch::with_no_grad({
  scanned <- f('lkt_gru_forward')(runtime$network,b$numeric,b$categorical,b$answers,
    NULL,NULL,b$retrieval_ids,s,b$decay_gaps,!is.na(b$rows))
  for(j in seq_along(groups)) {
    packed <- f('lkt_sequence_state_map')(f('lkt_sequence_state_map')(scanned$hidden,s,'subset',j),s,'pack')
    close(packed,full$state$hidden[[names(groups)[j]]])
  }
})
bad <- d;bad$clock[2] <- -1
fail(fit_model(bad),'nonnegative gaps')
for(value in c(NA_real_,Inf,NaN)) {
  bad <- d;bad$clock[2] <- value
  fail(fit_model(bad),'finite numeric or POSIXct')
}
bad <- d;bad$clock <- NULL
fail(fit_model(bad),'clock')
bad <- d[4,,drop=FALSE];bad$clock <- -1
fail(LKTGRUStep(fit,first$state,bad,runtime),'nonnegative gaps')
# Clock conversion agrees for numeric days and POSIXct seconds.
gaps <- f('lkt_gru_hidden_decay_gaps')(d,s)
days_data <- d;days_data$clock <- days_data$clock/86400
days_settings <- s;days_settings$time_units <- 'days'
close(f('lkt_gru_hidden_decay_gaps')(days_data,days_settings),gaps)
posix <- d;posix$clock <- as.POSIXct(posix$clock,origin='2020-01-01',tz='UTC')
close(f('lkt_gru_hidden_decay_gaps')(posix,s),gaps)
cat('PASS chunk equivalence, padding-state exclusion and timestamp errors/unit conversion\n')

# Interleaved item histories have their own DK and clocks within the same student.
group_options <- test_gru_model(hidden_decay=TRUE,hidden_decay_time_col='clock',
  history_group_cols=c('Anon.Student.Id','KC..Default.'),hidden_size=3L,
  head_hidden_sizes=4L,epochs=1L,sequence_length=2L,batch_size=3L,
  validation_fraction=0,early_stopping=FALSE)
group_fit <- LKT(d,features='lineafm',components='KC..Default.',model=group_options,verbose=FALSE)
group_full <- LKTGRUStep(group_fit,LKTGRUState(group_fit),d)
close(group_full$predictions,group_fit$prediction)
group_keys <- f('lkt_gru_keys')(d,group_fit$model$settings$history_group_cols)
for(idx in split(seq_len(nrow(d)),group_keys)) {
  solo <- LKTGRUStep(group_fit,LKTGRUState(group_fit),d[idx,])
  close(solo$predictions,group_full$predictions[idx])
  close(solo$state$hidden[[group_keys[idx[1L]]]],group_full$state$hidden[[group_keys[idx[1L]]]])
}
cat('PASS interleaved history keys retain separate DK and elapsed-time clocks\n')

# Training rows alone determine fitted parameters, including hidden-decay scalars.
holdout <- d;holdout$fold <- ifelse(holdout$Anon.Student.Id=='s4',2,1)
adapter <- test_gru_model(hidden_decay=TRUE,hidden_decay_time_col='clock',hidden_size=3L,
  head_hidden_sizes=4L,epochs=2L,sequence_length=2L,batch_size=3L,seed=21L,
  validation_fraction=0,early_stopping=FALSE)
fit_holdout <- function(x)LKT(x,features='lineafm',components='KC..Default.',
  model=adapter,usefolds=1,verbose=FALSE)
a <- fit_holdout(holdout)
holdout$CF..ansbin.[holdout$fold==2] <- 1-holdout$CF..ansbin.[holdout$fold==2]
b <- fit_holdout(holdout)
stopifnot(identical(a$model$weights,b$model$weights))
cat('PASS held-out responses do not train shared or hidden-decay parameters\n')
}
