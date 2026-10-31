library(LKT)
library(data.table)
fail <- function(expr) stopifnot(inherits(try(force(expr), silent = TRUE), "try-error"))
near <- function(a, b, tol = 1e-10) stopifnot(isTRUE(all.equal(a, b, tolerance = tol, check.attributes = FALSE)))
mu <- c(a = -0.4, b = 0.8)
v <- matrix(c(1, .6, .6, 2), 2, dimnames = list(names(mu), names(mu)))
set.seed(178)
rng <- .Random.seed
s <- LKTBayesianState(mu, v, 20000, 41)
stopifnot(identical(rng, .Random.seed))
near(s$mean, mu, .03)
near(s$covariance, v, .04)
near(s$particles, LKTBayesianState(mu, v[c(2,1), c(2,1)], 20000, 41)$particles)
rm(.Random.seed, envir = .GlobalEnv)
invisible(LKTBayesianState(mu, v, 20))
stopifnot(!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))

# Same particles give exact sequential/batch likelihood and moment equivalence.
s <- LKTBayesianState(mu, v, 128, 42)
x <- rbind(c(a=1,b=.3), c(a=1,b=1.1), c(a=1,b=-.5))
y <- c(1,0,1)
initial <- s
expected <- rep(1/128,128)
for (i in seq_along(y)) {
  p <- plogis(as.numeric(initial$particles %*% x[i,]))
  step <- LKTBayesianStep(s, x[i,], y[i], ess_action = "ignore")
  near(step$prediction, sum(expected*p))
  expected <- expected * if (y[i]) p else (1-p)
  expected <- expected/sum(expected)
  near(exp(step$state$log_weights), expected)
  s <- step$state
}
batch_p <- plogis(initial$particles %*% t(x))
batch <- apply(sweep(batch_p,2,y,function(p,y) ifelse(y==1,p,1-p)),1,prod)
batch <- batch/sum(batch)
near(exp(s$log_weights),batch)
near(s$mean,colSums(initial$particles*batch))
center <- sweep(initial$particles,2,colSums(initial$particles*batch),"-")
near(s$covariance,crossprod(center,center*batch))
near(LKTBayesianStep(initial,c(b=.3,a=1))$prediction,
     LKTBayesianStep(initial,c(a=1,b=.3))$prediction)
stopifnot(identical(LKTBayesianStep(initial,c(a=1,b=.3))$state$updates,0L))

# Independent numerical integration reference for a one-dimensional posterior.
one <- LKTBayesianState(c(a=0),matrix(1,1,dimnames=list("a","a")),50000,3)
posterior <- LKTBayesianStep(one,c(a=1),1,ess_action="ignore")$state
reference <- integrate(function(z) z*plogis(z)*dnorm(z),-10,10)$value/.5
stopifnot(abs(posterior$mean-reference)<.015)
uncertain <- LKTBayesianState(c(a=2),matrix(9,1,dimnames=list("a","a")),20000,7)
stopifnot(abs(LKTBayesianStep(uncertain,c(a=1))$prediction-plogis(uncertain$mean))>.08)

# Extreme logits remain finite; low ESS is diagnosed without altering draws.
extreme <- LKTBayesianStep(initial,c(a=1e4,b=-1e4),1,ess_action="ignore")
stopifnot(is.finite(extreme$log_score),all(is.finite(extreme$state$log_weights)),
          extreme$prediction>0,extreme$prediction<1)
collapsed <- initial
collapsed$particles[,] <- -1000
collapsed$particles[1,] <- 1000
fail(LKTBayesianStep(collapsed,c(a=1,b=0),1,ess_threshold=.5,ess_action="error"))
warns <- 0L
first <- withCallingHandlers(LKTBayesianStep(collapsed,c(a=1,b=0),1,
    ess_threshold=.5),warning=function(w){warns<<-warns+1L;invokeRestart("muffleWarning")})
second <- withCallingHandlers(LKTBayesianStep(first$state,c(a=1,b=0),1,
    ess_threshold=.5),warning=function(w){warns<<-warns+1L;invokeRestart("muffleWarning")})
stopifnot(warns==1L,first$low_ess,second$low_ess,
          identical(second$state$particles,collapsed$particles))
fail(LKTBayesianState(mu, v*NA, 10))
fail(LKTBayesianState(mu, matrix(1,2,2,dimnames=dimnames(v)),10))
bad <- v; bad[1,2] <- .1
fail(LKTBayesianState(mu,bad,10))
bad <- v; dimnames(bad) <- list(c("a","c"),c("a","c"))
fail(LKTBayesianState(mu,bad,10))
fail(LKTBayesianState(mu,v,1))
fail(LKTBayesianState(mu,v,10,NA_real_))
fail(LKTBayesianStep(initial,c(a=NA_real_,b=0),1))
fail(LKTBayesianStep(initial,c(a=1,b=0),NA_real_))
fail(LKTBayesianStep(initial,c(a=1,b=0),2))

# Full LKT adapter: pre-response predictions and causal history features.
d <- data.table(Anon.Student.Id=rep(c("s1","s2"),each=8),
                KC..Default.="k",CF..ansbin.=rep(c(0,1,1,0,1,0,0,1),2),
                CF..Time.=rep(seq(1000,1070,10),2))
mean_all <- c("(Intercept)"=-.5,"logsucKC..Default."=.6)
cov_all <- diag(c(.3,.2)); dimnames(cov_all)<-list(names(mean_all),names(mean_all))
args <- list(components="KC..Default.",features="logsuc",interc=TRUE,verbose=FALSE,
             model="online_bayesian", model_options=list(prior_mean=mean_all,
               prior_covariance=cov_all,n_particles=128,seed=11,store_trace=TRUE,
               ess_action="ignore"))
fit <- do.call(LKT,c(list(data=copy(d)),args))
near(fit$prediction[1:8],fit$prediction[9:16])
near(fit$model$final_states$s1$mean,fit$model$final_states$s2$mean)
stopifnot(length(fit$model$trace)==16,
          fit$model$final_states$s1$updates==8L)
single <- do.call(LKT,c(list(data=copy(d[Anon.Student.Id=="s2"])),args))
near(single$prediction,fit$prediction[9:16])
changed <- copy(d); changed[1,CF..ansbin.:=1]
different <- do.call(LKT,c(list(data=changed),args))
near(different$prediction[1],fit$prediction[1])
near(different$prediction[9:16],fit$prediction[9:16])
changed <- copy(d); changed[8,CF..ansbin.:=0]
future <- do.call(LKT,c(list(data=changed),args))
near(future$prediction[1:8],fit$prediction[1:8])
near(predict_lkt(fit,fit$newdata)$predictions,fit$prediction)
near(predict_lkt(fit,fit$newdata,return_stats=TRUE)$LL,fit$loglike)
fail(predict_lkt(fit,changed))
resumed <- unserialize(serialize(fit$model$final_states$s1,NULL))
near(LKTBayesianStep(resumed,c("(Intercept)"=1,"logsucKC..Default."=.4),1,
                     ess_action="ignore")$state$log_weights,
     LKTBayesianStep(fit$model$final_states$s1,c("(Intercept)"=1,"logsucKC..Default."=.4),1,
                     ess_action="ignore")$state$log_weights)
# Fixed nonlinear feature parameters are reused, with no evaluation-data fit.
nonlinear_args <- args
nonlinear_args$features <- "logitdec"
nonlinear_args$fixedpars <- .9
nonlinear_args$model_options$prior_mean <- c("(Intercept)"=-.5,"logitdecKC..Default."=.6)
nonlinear_args$model_options$prior_covariance <- cov_all
dimnames(nonlinear_args$model_options$prior_covariance) <-
  list(names(nonlinear_args$model_options$prior_mean),names(nonlinear_args$model_options$prior_mean))
nonlinear <- do.call(LKT,c(list(data=copy(d)),nonlinear_args))
near(nonlinear$prediction[1:8],nonlinear$prediction[9:16])

# Fixed coefficients remain exact; reordered names do not change predictions.
subset_args <- args
subset_args$model <- OnlineBayesianModel(mean_all,cov_all[2,2,drop=FALSE],
   beta_alpha_terms="logsucKC..Default.",n_particles=128,seed=11,ess_action="ignore")
subset_args$model_options <- list()
partial <- do.call(LKT,c(list(data=copy(d)),subset_args))
stopifnot(identical(colnames(partial$model$initial_state$particles),"logsucKC..Default."))
reordered <- args
reordered$model_options$prior_mean <- mean_all[2:1]
reordered$model_options$prior_covariance <- cov_all[2:1,2:1]
# Force canonical adapted order to compare identical Gaussian draws.
reordered$model_options$beta_alpha_terms <- names(mean_all)
near(do.call(LKT,c(list(data=copy(d)),reordered))$prediction,fit$prediction)
badargs <- args; badargs$model_options$nonlinear_alpha_terms <- "recency|KC..Default.|para"
fail(do.call(LKT,c(list(data=copy(d)),badargs)))
badargs <- args; badargs$features <- "recency"
fail(do.call(LKT,c(list(data=copy(d)),badargs)))
badargs$fixedpars <- 1 # LKT integer references denote optimized/tied parameters.
fail(do.call(LKT,c(list(data=copy(d)),badargs)))
changed <- copy(d); changed[1,CF..Time.:=2000]
fail(do.call(LKT,c(list(data=changed),args)))
badargs <- args; badargs$usefolds <- 1
fail(do.call(LKT,c(list(data=copy(d)),badargs)))
badargs <- args; badargs$model_options$prior_mean <- NULL
fail(do.call(LKT,c(list(data=copy(d)),badargs)))
badargs <- args; badargs$features <- "logsuc@"
fail(do.call(LKT,c(list(data=copy(d)),badargs)))
badargs <- args; badargs$nosolve <- TRUE
fail(do.call(LKT,c(list(data=copy(d)),badargs)))

# Existing alpha integration and compiled/R evaluators remain equivalent.
alpha <- LKT(copy(d),components="KC..Default.",features="logsuc",interc=TRUE,
             verbose=FALSE,model=OnlineCalibrationModel(optimize_alpha=FALSE,alpha_start=0))
static <- LKT(copy(d),components="KC..Default.",features="logsuc",interc=TRUE,verbose=FALSE)
near(alpha$prediction,static$prediction,1e-5)
cat("Bayesian online regression checks passed.\n")
