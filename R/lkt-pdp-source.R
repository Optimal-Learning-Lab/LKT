# Literal dynamics from demo pdp auto13.R (2004), isolated from its optimizer,
# globals and data loading. Negative learning rates are intentional here: no clamp.
lkt_pdp_source_model <- function(decays, stp_rate=.010214728,
    base_rate=.007472300, recovery=.005629399, fatigue=.188602174*1000,
    consolidation=.032804697, heat=.35, recall_noise=.4, recall_threshold=1) {
  p <- list(stp_rate=stp_rate,base_rate=base_rate,recovery=recovery,
    fatigue=fatigue,consolidation=consolidation,heat=heat,
    recall_noise=recall_noise,recall_threshold=recall_threshold)
  if(!all(vapply(p,function(x)is.numeric(x) && length(x)==1L && is.finite(x),logical(1))) ||
     any(unlist(p[1:5])<0) || recovery>1 || consolidation>1 || heat<=0 || recall_noise<=0)
    stop('Invalid source PDP parameters.',call.=FALSE)
  if(!is.matrix(decays) || !is.numeric(decays) || nrow(decays)<2L ||
     nrow(decays)!=ncol(decays) || nrow(decays)%%2L!=0L ||
     any(!is.finite(decays)) || any(decays<0 | decays>.8))
    stop('Decays must be an even-dimensional square matrix in [0, .8].',call.=FALSE)
  structure(c(p,list(decays=decays)),class='lkt_pdp_source_model')
}

lkt_pdp_source_state <- function(model) {
  if(!inherits(model,'lkt_pdp_source_model'))stop('Expected a source PDP model.',call.=FALSE)
  z <- model$decays*0
  structure(list(short=z,target=z,long=z,rate=z+model$base_rate),class='lkt_pdp_source_state')
}

lkt_pdp_source_validate <- function(model,state,input=NULL) {
  if(!inherits(model,'lkt_pdp_source_model') || !inherits(state,'lkt_pdp_source_state'))
    stop('Expected source PDP model and state.',call.=FALSE)
  for(n in c('short','target','long','rate'))
    if(!is.matrix(state[[n]]) || !identical(dim(state[[n]]),dim(model$decays)) ||
       any(!is.finite(state[[n]])))stop('Nonfinite or incompatible connection state.',call.=FALSE)
  if(!is.null(input) && (!is.numeric(input) || length(input)!=nrow(model$decays) ||
     any(!is.finite(input)) || any(!input %in% c(-1,0,1))))
    stop('Input must be an unnormalized ternary vector with one value per unit.',call.=FALSE)
  invisible(NULL)
}

lkt_pdp_source_recall <- function(model,state,input) {
  lkt_pdp_source_validate(model,state,input)
  n <- length(input);hidden <- seq.int(n/2+1L,n)
  cue <- input;cue[hidden] <- 0
  out <- 2/(1+exp(-as.numeric(cue%*%(state$short+state$long))/model$heat))-1
  strength <- sum(out[hidden]*input[hidden])
  log_strength <- if(strength<=0)-10 else log(strength)
  list(strength=strength,probability=1/(1+exp((model$recall_threshold-log_strength)/model$recall_noise)))
}

lkt_pdp_source_step <- function(model,state,input,phase=c('training','retention')) {
  phase <- match.arg(phase);lkt_pdp_source_validate(model,state,input)
  prediction <- as.numeric(2/(1+exp(-(input%*%(state$short+state$long))/model$heat))-1)
  # Preserve error outer input, even though forward propagation is input %*% W.
  direction <- outer(input-prediction,input)
  ds <- model$stp_rate*direction;dt <- state$rate*direction
  after <- state;after$short <- state$short+ds;after$target <- state$target+dt
  if(phase=='training') {
    after$long <- state$long+(after$target-state$long)*model$consolidation
    after$short <- after$short-after$short*model$decays
    recovered <- state$rate+(model$base_rate-state$rate)*model$recovery
    after$rate <- recovered-recovered*abs(dt)*model$fatigue
  } else {
    fatigued <- state$rate-state$rate*abs(dt)*model$fatigue
    after$rate <- fatigued+(model$base_rate-fatigued)*(1-exp(-model$recovery*40))
    after$short <- after$short*exp(-model$decays*40)
    after$long <- state$long+(after$target-state$long)*(1-exp(-model$consolidation*40))
  }
  # Surface divergence; never substitute clipped, reset or reconstructed states.
  lkt_pdp_source_validate(model,after)
  list(state=after,prediction=prediction,direction=direction,
    short_change=ds,target_change=dt)
}

# Exact zero-input iterations of the selected original phase. Training ticks
# are discrete; one retention block represents the original 40 time units.
lkt_pdp_source_idle <- function(model,state,ticks,phase=c('training','retention')) {
  phase <- match.arg(phase);lkt_pdp_source_validate(model,state)
  if(!is.numeric(ticks) || length(ticks)!=1L || !is.finite(ticks) || ticks<0 || ticks!=floor(ticks))
    stop('Idle ticks must be a nonnegative integer.',call.=FALSE)
  if(phase=='training') {
    a <- (1-model$decays)^ticks;b <- (1-model$recovery)^ticks
    c <- (1-model$consolidation)^ticks
  } else {
    a <- exp(-model$decays*40*ticks);b <- exp(-model$recovery*40*ticks)
    c <- exp(-model$consolidation*40*ticks)
  }
  state$short <- state$short*a
  state$rate <- model$base_rate+(state$rate-model$base_rate)*b
  state$long <- state$target+(state$long-state$target)*c
  lkt_pdp_source_validate(model,state)
  state
}
