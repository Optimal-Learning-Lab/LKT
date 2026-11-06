library(LKT)
# Historical small-model cases specify their raw architecture explicitly.
fixture_args <- commandArgs(FALSE)
fixture_file <- fixture_args[grepl('^--file=',fixture_args)]
fixture_dir <- if(length(fixture_file))dirname(normalizePath(sub('^--file=','',fixture_file[1L]),winslash='/')) else getwd()
source(file.path(fixture_dir,'fixtures/gru-minimal-settings.R'))
if(!requireNamespace('torch',quietly=TRUE) || !torch::torch_is_installed()) {
  if(identical(Sys.getenv('LKT_REQUIRE_TORCH_TESTS'),'true'))stop('Torch runtime required')
} else {
# Test-only adapter: recurrent hidden state uses batch axis 2, while a second
# evolving memory uses axis 1. Both affect predictions. Replace only adapter
# entry points; fitting, chunking, retrieval, inference and continuation are real.
exercise <- function() {
  ns <- asNamespace('LKT')
  names <- c('lkt_sequence_build','lkt_sequence_initial','lkt_sequence_read','lkt_sequence_layout')
  originals <- mget(names,ns,inherits=FALSE)
  on.exit(for(n in names)assignInNamespace(n,originals[[n]],ns='LKT'),add=TRUE)
  close <- function(a,b)stopifnot(isTRUE(all.equal(a,b,tolerance=3e-6,check.attributes=FALSE)))
  read <- function(state,s)state$h[s$num_layers,,,drop=FALSE]$permute(c(2,1,3))+state$c$unsqueeze(2)
  initial <- function(s,batch,like)list(
    h=torch::torch_zeros(c(s$num_layers,batch,s$hidden_size),dtype=like$dtype,device=like$device),
    c=torch::torch_zeros(c(batch,1L),dtype=like$dtype,device=like$device))
  build <- function(input_size,s) {
    self <- NULL
    definition <- torch::nn_module(
      initialize=function()self$inner <- torch::nn_gru(input_size,s$hidden_size,
        num_layers=s$num_layers,batch_first=TRUE),
      forward=function(input,state=NULL) {
        if(is.null(state))state <- initial(s,input$size(1),input)
        outputs <- vector('list',input$size(2))
        for(t in seq_len(input$size(2))) {
          current <- input[,t,,drop=FALSE]
          state <- list(h=self$inner(current,state$h)[[2]],
            c=.7*state$c+current$mean(dim=3)$reshape(c(input$size(1),1L)))
          outputs[[t]] <- read(state,s)
        }
        list(torch::torch_cat(outputs,dim=2),state)
      })
    definition()
  }
  replacements <- list(build,initial,read,function(s)list(h=2L,c=1L))
  for(i in seq_along(names))assignInNamespace(names[i],replacements[[i]],ns='LKT')
  set.seed(17)
  d <- data.frame(Anon.Student.Id=rep(paste0('s',1:5),c(7,3,6,4,5)),
    KC..Default.=rep(c('a','b','c','a','b'),5),CF..ansbin.=rbinom(25,1,.6))
  for(mode in c('none','deterministic','gaussian')) {
    fit <- LKT(d,features='lineafm',components='KC..Default.',verbose=FALSE,
      model=test_sequence_model('gru',epochs=1L,hidden_size=3L,num_layers=2L,
        batch_size=5L,sequence_length=2L,embedding_dims=2L,head_hidden_sizes=4L,
        inference_mode=mode,latent_size=2L,inference_hidden_sizes=3L,
        feature_encoder='mlp',encoder_hidden_sizes=3L,encoder_output_size=2L,
        retrieval_heads=1L,retrieval_size=3L,retrieval_window=4L,
        validation_fraction=0,early_stopping=FALSE))
    stopifnot(all(is.finite(fit$prediction)))
    runtime <- LKTSequenceRuntime(fit);empty <- LKTSequenceState(fit)
    full <- LKTSequenceStep(fit,empty,d,runtime)
    close(full$predictions,fit$prediction)
    first <- LKTSequenceStep(fit,empty,d[1:3,],runtime)
    remainder <- LKTSequenceStep(fit,first$state,d[4:7,],runtime)
    close(c(first$predictions,remainder$predictions),fit$prediction[1:7])
    stopifnot(length(dim(first$state$hidden[[1]]$h))==3L,
      identical(dim(first$state$hidden[[1]]$c),c(1L,1L)))
    before <- serialize(first$state,NULL)
    candidates <- d[c(4,5,6),]
    p <- LKTSequenceScore(fit,first$state,candidates,runtime)
    close(p,rev(LKTSequenceScore(fit,first$state,candidates[3:1,],runtime)))
    stopifnot(identical(before,serialize(first$state,NULL)))
    file <- tempfile(fileext='.rds');on.exit(unlink(file),add=TRUE)
    saveRDS(list(fit=fit,state=first$state),file);saved <- readRDS(file)
    close(LKTSequenceStep(saved$fit,saved$state,d[4:7,])$predictions,remainder$predictions)
    # All leaf operations retain layout and storage independence.
    settings <- fit$model$settings
    packed <- first$state$hidden[[1]]
    state <- get('lkt_sequence_state_map',ns)(packed,settings,'unpack')
    detached <- get('lkt_sequence_state_map',ns)(state,settings,'detach')
    cloned <- get('lkt_sequence_state_map',ns)(detached,settings,'clone')
    torch::with_no_grad({cloned$c$add_(1)})
    close(torch::as_array(state$c),packed$c)
    stopifnot(max(abs(torch::as_array(cloned$c)-packed$c))>.9)
  }
}
exercise()
cat('PASS heterogeneous-state adapter through shared fitting, active batches, chunk detach, retrieval/inference, immutable scoring and serialized continuation\n')
}
