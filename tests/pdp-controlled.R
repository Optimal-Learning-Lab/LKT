library(LKT)
f <- function(n)getFromNamespace(paste0('lkt_pdp_controlled_',n),'LKT')
model <- f('model');state <- f('state');advance <- f('advance');practice <- f('practice');predict <- f('predict')
close <- function(a,b,tol=1e-12) {
  if(is.numeric(a) && is.numeric(b)){a <- as.numeric(a);b <- as.numeric(b)}
  stopifnot(isTRUE(all.equal(a,b,tolerance=tol,check.attributes=FALSE)))
}
fails <- function(expr)stopifnot(inherits(tryCatch(force(expr),error=identity),'error'))
m <- model();zero <- state(m);saved <- serialize(zero,NULL)
p <- practice(m,zero,1,1,0)
close(p$prediction,0);close(p$short_change,.3);close(p$long_change,.1)
close(p$state$available_rate,.1*exp(-.8));close(p$post_prediction,.4)
stopifnot(identical(saved,serialize(zero,NULL)))
cat('PASS independent first-practice delta updates, multiplicative fatigue and immutable initial state\n')

r <- advance(m,p$state,300)
close(r$available_rate,(p$state$available_rate+.1)/2)
close(r$short,.3*exp(-.0038*300));close(r$long,p$state$long)
close(advance(m,advance(m,p$state,100),300),r)
close(advance(m,p$state,0),p$state)
close(advance(m,p$state,1e6)$available_rate,.1)
close(advance(m,p$state,1e6)$long,.1)
saved <- serialize(p$state,NULL);predict(m,p$state,1,1e6)
stopifnot(identical(saved,serialize(p$state,NULL)))
cat('PASS analytic STP decay, exponential half-recovery, no LTP decay, time subdivision and immutable delayed scoring\n')

# In a no-STP connection, feedback compensation makes the pre-fatigue update
# constant: (actual LTP increment)/(prediction error) = available learning rate.
run <- function(m,gap,exposure_count=4L,target=1) {
  z <- state(m);events <- list()
  for(i in seq_len(exposure_count)) {
    event <- practice(m,z,1,target,(i-1)*gap);z <- event$state;events[[i]] <- event
  }
  list(state=z,events=events)
}
plain <- model(stp_learning_rate=0,fatigue_sensitivity=0)
fat <- model(stp_learning_rate=0)
fixed_time <- run(fat,0)
eta <- vapply(fixed_time$events,function(x)x$before$available_rate[1],numeric(1))
stopifnot(all(diff(eta)<0))
for(e in fixed_time$events)close(e$long_change[1]/e$error[1],e$before$available_rate[1])
a <- run(fat,20);b <- run(fat,300)
stopifnot(b$state$long>a$state$long)
for(gap in c(0,20,300,3600)) {
  # Independent two-practice closed form, with STP absent.
  expected_rate <- .1+(.1*exp(-.8)-.1)*exp(-log(2)*gap/300)
  two <- run(fat,gap,exposure_count=2L)
  close(two$state$long,.1+.9*expected_rate)
}
close(run(plain,20)$state$long,run(plain,300)$state$long)
unrecovered <- model(stp_learning_rate=0,recovery_half_life_seconds=Inf)
close(run(unrecovered,20)$state$long,run(unrecovered,300)$state$long)
# Error-driven STP competition can also create a spacing benefit without fatigue.
stp_only <- model(fatigue_sensitivity=0)
stopifnot(run(stp_only,300)$state$long>run(stp_only,20)$state$long)
full_a <- run(m,20);full_b <- run(m,300)
stopifnot(predict(m,full_b$state,1,full_b$state$time_seconds+86400)>
  predict(m,full_a$state,1,full_a$state$time_seconds+86400))
negative <- run(m,300,target=-1)
close(negative$state$long,-full_b$state$long);close(negative$state$short,-full_b$state$short)
close(negative$state$available_rate,full_b$state$available_rate)
reversal <- practice(m,p$state,1,-1,0)
stopifnot(reversal$long_change[1]<0,reversal$state$available_rate[1]<p$state$available_rate[1])
scaled <- model(recovery_half_life_seconds=3000,stp_decay_rates_per_second=matrix(.0038/10,1,1))
scaled_result <- run(scaled,3000)
for(n in c('short','long','available_rate'))close(scaled_result$state[[n]],full_b$state[[n]])
disabled <- model(stp_learning_rate=0,ltp_learning_rate=0)
close(run(disabled,300)$state$long,0);close(run(disabled,300)$state$short,0)
cat('PASS repeated-practice fatigue, fatigue-specific spacing, no-fatigue/no-recovery controls, STP competition and signed symmetry\n')

# Different connections have separate strengths and capacities. No learner state
# lives in model parameters. All weights and capacities survive serialization.
mm <- model(stp_decay_rates_per_second=matrix(c(.001,.002,.003,.004),2,2))
z <- state(mm);q <- practice(mm,z,c(1,0),c(1,0),0)
close(q$state$available_rate[2,],rep(.1,2));close(q$state$available_rate[1,2],.1)
close(q$state$long[2,],c(0,0));stopifnot(q$state$available_rate[1,1]<.1)
close(state(mm),z)
reloaded <- unserialize(serialize(list(model=mm,state=q$state),NULL))
close(predict(mm,q$state,c(1,0),100),predict(reloaded$model,reloaded$state,c(1,0),100))
for(dt in c(0,1,20,300,3600,1e6)) {
  v <- advance(mm,q$state,dt)
  stopifnot(all(v$available_rate>=0),all(v$available_rate<=mm$ltp_learning_rate))
}
fails(advance(m,p$state,-1));fails(practice(m,zero,2,1,0));fails(model(fatigue_sensitivity=-1))
fails(model(stp_learning_rate=.95,ltp_learning_rate=.1));fails(predict(mm,z,c(1,0),NA_real_))
cat('PASS connection/learner isolation, heterogeneous rates, capacity bounds, portable state and invalid-input rejection\n')
