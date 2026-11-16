# Controlled PDP connection dynamics for mechanistic research. Independent of the
# GRU adapters. The paper specifies the mechanisms verbally; the fatigue equation
# here is an explicit reconstruction, not a claimed recovery of the original code.
lkt_pdp_controlled_model <- function(stp_learning_rate=.3,ltp_learning_rate=.1,
    fatigue_sensitivity=8,recovery_half_life_seconds=300,
    stp_decay_rates_per_second=matrix(.038/10,1L,1L)) {
  scalar <- function(x,lower=0) is.numeric(x) && length(x)==1L && is.finite(x) && x>=lower
  if(!scalar(stp_learning_rate) || !scalar(ltp_learning_rate) ||
     stp_learning_rate+ltp_learning_rate>1 || !scalar(fatigue_sensitivity))
    stop('Learning rates must be nonnegative, sum to at most one, and fatigue must be nonnegative.',call.=FALSE)
  if(!is.numeric(recovery_half_life_seconds) || length(recovery_half_life_seconds)!=1L ||
     is.na(recovery_half_life_seconds) || recovery_half_life_seconds<=0)
    stop('Recovery half-life must be positive seconds or Inf (no recovery).',call.=FALSE)
  rates <- stp_decay_rates_per_second
  if(!is.matrix(rates) || !is.numeric(rates) || !all(is.finite(rates)) ||
     !all(rates>=0) || any(dim(rates)<1L))
    stop('STP decay rates must be a nonempty matrix of finite nonnegative rates per second.',call.=FALSE)
  structure(list(stp_learning_rate=stp_learning_rate,ltp_learning_rate=ltp_learning_rate,
    fatigue_sensitivity=fatigue_sensitivity,recovery_half_life_seconds=recovery_half_life_seconds,
    recovery_rate_per_second=log(2)/recovery_half_life_seconds,stp_decay_rates_per_second=rates),
    class='lkt_pdp_controlled_model')
}

lkt_pdp_controlled_state <- function(model,time_seconds=0) {
  if(!inherits(model,'lkt_pdp_controlled_model'))stop('Expected a controlled PDP model.',call.=FALSE)
  if(!is.numeric(time_seconds) || length(time_seconds)!=1L || !is.finite(time_seconds))
    stop('State time must be one finite number of seconds.',call.=FALSE)
  shape <- dim(model$stp_decay_rates_per_second)
  structure(list(short=matrix(0,shape[1L],shape[2L]),long=matrix(0,shape[1L],shape[2L]),
    available_rate=matrix(model$ltp_learning_rate,shape[1L],shape[2L]),time_seconds=time_seconds),
    class='lkt_pdp_controlled_state')
}

lkt_pdp_controlled_advance <- function(model,state,time_seconds) {
  if(!inherits(model,'lkt_pdp_controlled_model') || !inherits(state,'lkt_pdp_controlled_state'))
    stop('Expected controlled PDP model and state.',call.=FALSE)
  shape <- dim(model$stp_decay_rates_per_second)
  for(n in c('short','long','available_rate'))
    if(!is.matrix(state[[n]]) || !identical(dim(state[[n]]),shape) || !all(is.finite(state[[n]])))
      stop('Connection state has incompatible dimensions or nonfinite values.',call.=FALSE)
  if(any(state$available_rate<0) || any(state$available_rate>model$ltp_learning_rate))
    stop('Available learning rates must lie between zero and the base LTP learning rate.',call.=FALSE)
  if(!is.numeric(state$time_seconds) || length(state$time_seconds)!=1L || !is.finite(state$time_seconds) ||
     !is.numeric(time_seconds) || length(time_seconds)!=1L || !is.finite(time_seconds) || time_seconds<state$time_seconds)
    stop('Time must be finite and cannot precede committed state time.',call.=FALSE)
  elapsed <- time_seconds-state$time_seconds
  state$short <- state$short*exp(-model$stp_decay_rates_per_second*elapsed)
  # Stable exact exponential recovery, including zero elapsed time and rho=0.
  state$available_rate <- state$available_rate+(model$ltp_learning_rate-state$available_rate)*
    (-expm1(-model$recovery_rate_per_second*elapsed))
  state$time_seconds <- time_seconds
  state
}

lkt_pdp_controlled_input <- function(model,input) {
  if(!is.numeric(input) || length(input)!=nrow(model$stp_decay_rates_per_second) || !all(is.finite(input)) ||
     abs(sum(input^2)-1)>1e-10)
    stop('Input must be a finite unit-norm cue vector with one entry per input unit.',call.=FALSE)
  invisible(NULL)
}

# Scoring branches in time without modifying the caller's connection state.
lkt_pdp_controlled_predict <- function(model,state,input,time_seconds=state$time_seconds) {
  current <- lkt_pdp_controlled_advance(model,state,time_seconds)
  lkt_pdp_controlled_input(model,input)
  as.numeric(input%*%(current$short+current$long))
}

lkt_pdp_controlled_practice <- function(model,state,input,target,time_seconds) {
  before <- lkt_pdp_controlled_advance(model,state,time_seconds)
  lkt_pdp_controlled_input(model,input)
  if(!is.numeric(target) || length(target)!=ncol(model$stp_decay_rates_per_second) || !all(is.finite(target)))
    stop('Target must have one finite entry per output unit.',call.=FALSE)
  prediction <- as.numeric(input%*%(before$short+before$long))
  error <- target-prediction
  direction <- outer(input,error)
  short_change <- model$stp_learning_rate*direction
  long_change <- before$available_rate*direction
  after <- before
  after$short <- before$short+short_change
  after$long <- before$long+long_change
  # Both signs remain in the weights. Magnitude consumes learning capacity, so
  # a sign reversal cannot increase available capacity above its resting value.
  # Symmetric fatigue for depression is a reconstruction assumption.
  after$available_rate <- before$available_rate*exp(-model$fatigue_sensitivity*abs(long_change))
  list(prediction=prediction,error=error,before=before,state=after,
    short_change=short_change,long_change=long_change,
    post_prediction=as.numeric(input%*%(after$short+after$long)))
}
