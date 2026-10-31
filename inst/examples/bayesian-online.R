# Synthetic example: training subjects estimate the starting mean; completely
# different subjects supply the online likelihoods. No private data is used.
library(LKT)
set.seed(57)
make_subjects <- function(ids) {
  data.table::as.data.table(do.call(rbind,lapply(ids,function(id) {
    beta <- c(-.6,.8)+rnorm(2,sd=c(.4,.2))
    success <- 0L
    y <- numeric(20)
    for (trial in seq_along(y)) {
      y[trial] <- rbinom(1,1,plogis(beta[1]+beta[2]*log1p(success)))
      success <- success+y[trial]
    }
    data.frame(Anon.Student.Id=id,KC..Default.="k",CF..ansbin.=y,
               CF..Time.=seq(1000,1190,10))
  })))
}
training <- make_subjects(paste0("train",1:20))
evaluation <- make_subjects(paste0("new",1:3))
stopifnot(!any(evaluation$Anon.Student.Id %in% training$Anon.Student.Id))
population <- LKT(training,components="KC..Default.",features="logsuc",
                  interc=TRUE,verbose=FALSE)
prior_mean <- setNames(population$coefs[,"coefficient"],rownames(population$coefs))
# Known generating BETWEEN-PERSON variances, for illustration. These are not
# standard errors of population$coefs. Real applications must supply a justified
# individual prior covariance from independent training subjects/domain evidence.
prior_covariance <- diag(c(.4^2,.2^2))
dimnames(prior_covariance) <- list(names(prior_mean),names(prior_mean))
fit <- LKT(evaluation,components="KC..Default.",features="logsuc",interc=TRUE,
  verbose=FALSE,model=OnlineBayesianModel(prior_mean,prior_covariance,
    n_particles=4096,seed=91,store_trace=TRUE))
print(head(fit$prediction))
print(lapply(fit$model$final_states,function(s) list(mean=s$mean,ess=s$ess)))
stopifnot(all(is.finite(fit$prediction)),length(fit$model$final_states)==3L)
