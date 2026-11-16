library(LKT)
f <- function(n)getFromNamespace(paste0('lkt_pdp_source_',n),'LKT')
model <- f('model');state <- f('state');step <- f('step');idle <- f('idle');recall <- f('recall')
eq <- function(a,b)stopifnot(isTRUE(all.equal(unname(as.numeric(a)),unname(as.numeric(b)),tolerance=1e-12)))
m <- model(matrix(.060921298,2,2));z <- state(m);x <- c(1,1)
first <- step(m,z,x);q <- first$state
eq(q$short,.010214728*(1-.060921298)+matrix(0,2,2))
eq(q$target,m$base_rate+matrix(0,2,2))
eq(q$long,m$base_rate*m$consolidation+matrix(0,2,2))
eq(q$rate,-.0030583527967651+matrix(0,2,2))
stopifnot(all(q$rate<0),all(step(m,q,x)$target_change<0),all(z$target==0))
cat('PASS first-event arithmetic and negative-rate reversal.\n')

# Independent non-symmetric hand calculation distinguishes transposed updates
# and use of old vs recovered rates, new vs old target, and phase ordering.
z$short[] <- c(.1,-.2,.3,-.1);z$target[] <- c(.03,.02,-.04,.01)
z$long[] <- c(.01,.03,-.02,.04);z$rate[] <- c(-.003,.002,.004,-.001)
x <- c(1,-1);out <- 2/(1+exp(-drop(x%*%(z$short+z$long))/.35))-1
D <- (matrix(x,2,1)-matrix(out,2,1))%*%x
stopifnot(max(abs(D-t(D)))>1e-3)
for(phase in c('training','retention')) {
  p <- step(m,z,x,phase);target <- z$target+z$rate*D;S <- z$short+m$stp_rate*D
  if(phase=='training') {
    E <- z$rate+(m$base_rate-z$rate)*m$recovery
    E <- E-E*abs(z$rate*D)*m$fatigue
    L <- z$long+(target-z$long)*m$consolidation;S <- S-S*m$decays
  } else {
    E <- z$rate-z$rate*abs(z$rate*D)*m$fatigue
    E <- E+(m$base_rate-E)*(1-exp(-40*m$recovery))
    L <- z$long+(target-z$long)*(1-exp(-40*m$consolidation));S <- S*exp(-40*m$decays)
  }
  eq(p$state$short,S);eq(p$state$target,target);eq(p$state$long,L);eq(p$state$rate,E)
  for(n in c(0,1,17)) {
    repeated <- z
    if(n>0)for(i in seq_len(n))repeated <- step(m,repeated,c(0,0),phase)$state
    closed <- idle(m,z,n,phase)
    for(key in names(z))eq(repeated[[key]],closed[[key]])
  }
}
cat('PASS asymmetric orientation, phase order and zero-input closed forms.\n')

recovered <- idle(m,q,100,'training')
stopifnot(all(recovered$rate>q$rate),all(recovered$rate<m$base_rate))
eq(recovered$target,q$target)
# Recall is a read-only partial-cue probe, including the original nonpositive rule.
saved <- serialize(q,NULL);recall(m,q,c(1,1));stopifnot(identical(saved,serialize(q,NULL)))
eq(recall(m,state(m),c(1,1))$probability,1/(1+exp(11/.4)))
bad <- function(expr)stopifnot(inherits(tryCatch(expr,error=identity),'error'))
bad(idle(m,q,-1));bad(step(m,q,c(.5,1)));bad(model(matrix(1,2,2)))
q$rate[1] <- Inf;bad(step(m,q,c(1,1)))
cat('PASS recovery, state isolation, recall and invalid/divergent-state rejection.\n')
