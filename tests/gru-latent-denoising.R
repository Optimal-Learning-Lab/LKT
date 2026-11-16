# Focused mechanism tests. Execute against the verified package on Big Blue;
# absence of the real torch runtime is a failure, never a skipped substitute.
library(LKT)
stopifnot(requireNamespace('torch',quietly=TRUE),torch::torch_is_installed())
f <- function(n)getFromNamespace(n,'LKT')
close <- function(a,b,tol=1e-10)stopifnot(isTRUE(all.equal(a,b,tolerance=tol,check.attributes=FALSE)))
error <- function(expr,pattern) {
 e <- tryCatch({force(expr);NULL},error=identity)
 stopifnot(inherits(e,'error'),grepl(pattern,conditionMessage(e)))
}
stopifnot(identical(GRUModel()$settings$latent_denoising,FALSE))
error(GRUModel(latent_denoising=NA),'TRUE or FALSE')
error(GRUModel(denoising_noise_std=.1),'noise SD')
error(GRUModel(denoising_loss_weight=.2),'loss weight')
error(GRUModel(latent_denoising=TRUE,hidden_size=16L),'32-wide')
error(SequenceModel(unit='mamba',latent_denoising=TRUE),'only for the GRU')
opts <- list(latent_denoising=TRUE,numeric_cols=paste0('n',1:5),use_lkt_features=FALSE,
 time_feature_cols=character(),time_missing_cols=character(),order_col='clock',
 plasticity='constant',plasticity_time_col='clock',input_dropout=.1,gru_dropout=.1,
 head_dropout=.1,epochs=2L,sequence_length=2L,batch_size=2L,validation_fraction=0,
 early_stopping=FALSE,dtype='float64',seed=71L)
s <- do.call(GRUModel,opts)$settings
close(f('lkt_gru_gui_decode')(as.list(f('lkt_gru_gui_encode')(s))),s)
config <- lkt_model_config(data.frame(component='KC..Default.',feature='intercept'),
 backend='GRU',gru_options=opts)
stopifnot(identical(config$gru_options,s))
legacy <- s;legacy[c('latent_denoising','denoising_noise_std','denoising_loss_weight')] <- NULL
stopifnot(!f('lkt_gru_saved_controls')(legacy)$latent_denoising)
incomplete <- s;incomplete$denoising_loss_weight <- NULL
error(f('lkt_gru_saved_controls')(incomplete),'incomplete')

d <- data.frame(Anon.Student.Id=rep(c('a','b'),c(5L,3L)),clock=c(1:5,1:3),
 KC..Default.=rep(c('q1','q2'),4),KC..Cluster.=rep(c('k1','k2'),4),
 CF..ansbin.=c(1,0,1,0,1,0,1,0),stringsAsFactors=FALSE)
for(j in 1:5)d[[paste0('n',j)]] <- seq_len(nrow(d))/10+j
recipe <- list(components='KC..Default.',features='intercept',fixedpars=numeric(),
 interacts=NA_character_,connectors=character(),interc=FALSE,maketimes=FALSE,subject_col='Anon.Student.Id')
cleanup <- f('lkt_gru_runtime')(s)
prep <- f('lkt_gru_preprocess')(d,rep(TRUE,nrow(d)),recipe,s)
enc <- f('lkt_gru_encode')(d,prep,recipe,s)
groups <- split(seq_len(nrow(d)),d$Anon.Student.Id)
b <- f('lkt_gru_batch')(enc,d,groups,lapply(groups,seq_along),s)
valid <- !is.na(b$rows)
plain <- s;plain$latent_denoising <- FALSE
zero <- s;zero$denoising_loss_weight <- 0
build <- function(settings) {
 torch::torch_manual_seed(119L)
 f('lkt_gru_network')(prep,settings)
}
base <- build(plain);base_rng <- torch::torch_get_rng_state()
net <- build(s);aux_rng <- torch::torch_get_rng_state()
stopifnot(identical(torch::as_array(base_rng),torch::as_array(aux_rng)))
weights <- f('lkt_gru_weights')(base)
close(weights,f('lkt_gru_weights')(net)[names(weights)],0)
count <- function(n)sum(vapply(n$parameters,function(p)as.numeric(p$numel()),numeric(1)))
stopifnot(count(net)-count(base)==2112)
# Authentic item/KC cardinalities reproduce the study's exact count.
full <- d[rep(seq_len(nrow(d)),length.out=144),];full$KC..Default. <- paste0('q',1:144)
full$KC..Cluster. <- paste0('k',rep(1:36,4))
fp <- f('lkt_gru_preprocess')(full,rep(TRUE,144),recipe,s)
stopifnot(count(f('lkt_gru_network')(fp,plain))==43042,
 count(f('lkt_gru_network')(fp,s))==45154)
run <- function(n,settings,batch=b,hidden=NULL,context=NULL)f('lkt_gru_forward')(
 n,batch$numeric,batch$categorical,batch$answers,hidden,context,batch$retrieval_ids,
 settings,batch$decay_gaps,!is.na(batch$rows))
bce <- function(out,batch=b)(torch::nnf_binary_cross_entropy_with_logits(out$logits,
 batch$answers,reduction='none')*batch$mask*batch$weights)$sum()/(batch$mask*batch$weights)$sum()
# A zero coefficient preserves common gradients and stochastic forward draws.
n0 <- build(zero)
for(n in list(base,n0)) {n$train();torch::with_no_grad({n$plasticity$eta$fill_(.05)})}
torch::torch_manual_seed(222L);a <- run(base,plain);loss_a <- bce(a);loss_a$backward()
torch::torch_manual_seed(222L);z <- run(n0,zero);loss_z <- bce(z);loss_z$backward()
close(torch::as_array(a$logits),torch::as_array(z$logits),0)
close(loss_a$item(),loss_z$item(),0)
stopifnot(is.null(z$denoising_state),is.null(f('lkt_gru_denoising_mse')(n0,z,valid,zero)))
pa <- base$named_parameters();pz <- n0$named_parameters()
for(k in names(pa)) {
 if(is.null(pa[[k]]$grad))stopifnot(is.null(pz[[k]]$grad))
 else close(torch::as_array(pa[[k]]$grad),torch::as_array(pz[[k]]$grad),0)
}
# Inspect the causal latent before retrieval replacement. Current feedback cannot
# affect its own reconstruction target, while preceding feedback can affect later h.
net$eval();a <- run(net,s);changed <- b;changed$answers <- 1-b$answers
stopifnot(is.null(a$denoising_state),is.null(f('lkt_gru_denoising_mse')(net,a,valid,s)))
net$train();torch::torch_manual_seed(333L);a <- run(net,s)
torch::torch_manual_seed(333L);other <- run(net,s,changed)
close(torch::as_array(a$denoising_state)[,1,],torch::as_array(other$denoising_state)[,1,],0)
stopifnot(max(abs(torch::as_array(a$denoising_state)[,2,]-
 torch::as_array(other$denoising_state)[,2,]))>1e-10)
# Padding consumes no noise and has no effect on the valid-event reduction.
padded <- a;padded$denoising_state <- a$denoising_state$clone()
padded$denoising_state[2,4:5,] <- 1e6
torch::torch_manual_seed(444L);mse <- f('lkt_gru_denoising_mse')(net,a,valid,s)
rng_after <- torch::torch_get_rng_state()
torch::torch_manual_seed(444L);padded_mse <- f('lkt_gru_denoising_mse')(net,padded,valid,s)
close(mse$item(),padded_mse$item(),0)
stopifnot(identical(torch::as_array(rng_after),torch::as_array(torch::torch_get_rng_state())))
# Confirm joint target gradients, rather than silently detaching clean h.
h <- a$denoising_state$detach()$clone()$requires_grad_(TRUE)
leaf <- list(denoising_state=h);net$zero_grad()
torch::torch_manual_seed(555L);joint <- f('lkt_gru_denoising_mse')(net,leaf,valid,s)
joint$backward();joint_grad <- torch::as_array(h$grad)
h2 <- h$detach()$clone()$requires_grad_(TRUE)
clean <- h2$reshape(c(-1L,32L))[which(as.vector(t(valid))),,drop=FALSE]
torch::torch_manual_seed(555L);noise <- torch::torch_randn_like(clean)*.20
noisy <- clean+noise;reconstructed <- noisy+net$latent_denoiser(noisy)
stopped <- (reconstructed-clean$detach())$pow(2)$mean()
close(stopped$item(),joint$item(),0);stopped$backward()
stopifnot(max(abs(joint_grad-torch::as_array(h2$grad)))>1e-10,
 all(is.finite(joint_grad)),all(joint_grad[2,4:5,]==0),
 net$latent_denoiser[[1]]$weight$grad$abs()$sum()$item()>0)
# Auxiliary gradients reach the shared recurrent network, but its output is never
# routed into logits, retrieval memory, hidden updates or online plasticity.
net$zero_grad();torch::torch_manual_seed(666L);a <- run(net,s)
f('lkt_gru_denoising_mse')(net,a,valid,s)$backward()
stopifnot(net$gru$weight_hh_l1$grad$abs()$sum()$item()>0)
net$eval();before <- run(net,s);rng <- torch::torch_get_rng_state()
torch::with_no_grad({net$latent_denoiser[[1]]$weight$fill_(30)})
after <- run(net,s)
close(torch::as_array(before$logits),torch::as_array(after$logits),0)
close(torch::as_array(before$hidden),torch::as_array(after$hidden),0)
for(k in names(before$context)) {
 if(is.character(before$context[[k]]))stopifnot(identical(before$context[[k]],after$context[[k]]))
 else close(torch::as_array(before$context[[k]]),torch::as_array(after$context[[k]]),0)
}
stopifnot(identical(torch::as_array(rng),torch::as_array(torch::torch_get_rng_state())))
# Also cover the plain fused GRU branch, without retrieval/encoders.
raw <- s;raw$feature_encoder <- 'none';raw$retrieval_heads <- 0L
raw$retrieval_filter_cols <- character();raw$encoder_output_sizes <- integer()
raw$numeric_encoder <- 'separate';raw$response_encoding <- 'mlp'
raw$retrieval_value_mode <- 'projected'
raw <- do.call(GRUModel,raw)$settings
nr <- f('lkt_gru_network')(prep,raw);nr$train()
stopifnot(!is.null(run(nr,raw)$denoising_state))
# The trainer's detached chunk boundary cannot backpropagate into prior history.
old_hidden <- f('lkt_sequence_initial')(raw,2L,b$numeric)$requires_grad_(TRUE)
detached <- f('lkt_sequence_state_map')(old_hidden,raw,'detach')
nr$zero_grad();next_chunk <- run(nr,raw,hidden=detached)
f('lkt_gru_denoising_mse')(nr,next_chunk,valid,raw)$backward()
stopifnot(!detached$requires_grad,torch::is_undefined_tensor(old_hidden$grad))
cleanup()
# End-to-end zero-coefficient fitting also preserves dropout streams, optimization
# and the existing weights; the auxiliary tensors remain outside the objective.
fit_base <- LKT(d,components='KC..Default.',features='intercept',fixedpars=numeric(),
 model=do.call(GRUModel,plain),verbose=FALSE)
fit_zero <- LKT(d,components='KC..Default.',features='intercept',fixedpars=numeric(),
 model=do.call(GRUModel,zero),verbose=FALSE)
close(fit_base$prediction,fit_zero$prediction,0)
close(fit_base$model$weights,fit_zero$model$weights[names(fit_base$model$weights)],0)
cols <- setdiff(names(fit_base$training_history),'elapsed_seconds')
close(fit_base$training_history[cols],fit_zero$training_history[cols],0)
# Public fitting, checkpoint fields and portable replay.
fit <- LKT(d,components='KC..Default.',features='intercept',fixedpars=numeric(),
 model=do.call(GRUModel,s),verbose=FALSE)
stopifnot(fit$training_only_parameter_count==2112L,
 fit$parameter_count-fit$prediction_parameter_count==2112,
 all(is.finite(fit$training_history$training_denoising_mse)),
 any(fit$training_history$training_denoising_mse>0))
path <- tempfile(fileext='.rds');saveRDS(fit,path);saved <- readRDS(path);unlink(path)
rt <- LKTGRURuntime(saved);initial <- LKTGRUState(saved)
step <- LKTGRUStep(saved,initial,d,runtime=rt)
close(step$predictions,saved$prediction,1e-9)
part <- LKTGRUStep(saved,initial,d[1:2,],runtime=rt)
continued <- LKTGRUStep(saved,part$state,d[3:5,],runtime=rt)
close(c(part$predictions,continued$predictions),saved$prediction[1:5],1e-9)
state_bytes <- serialize(part$state,NULL)
LKTGRUScore(saved,part$state,d[3:4,],runtime=rt)
stopifnot(identical(state_bytes,serialize(part$state,NULL)))
bad <- saved;bad$model$weights[['latent_denoiser.0.weight']][1,1] <- NA_real_
error(LKTGRURuntime(bad),'denoising weights')
cat('PASS denoising counts/config, exact zero loss/prediction/gradient/RNG parity, causal valid-state masking, joint target gradients, shared gradients, inference isolation, fitting and saved continuation.\n')
