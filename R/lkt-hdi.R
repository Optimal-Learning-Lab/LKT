# Bootstrap and credibility interval helpers for LKT.
# These functions build on LKT() and are sourced after LKTfunctions.R.

boot_fn <- function(dat, n_students, comps, feats, ints = NA, fixeds, conns) {
  dat_ss = smallSet(dat, n_students)

  mod = LKT(setDT(dat_ss), interc = TRUE,
            components = comps, interacts = ints,
            features = feats, connectors = conns,
            fixedpars = fixeds,
            seedpars = c(NA), verbose = FALSE)
  return(mod$coefs)
}

get_hdi <- function(par_reps,cred_mass=.95){

  coef_hdi <- data.frame(
    "coef_name" = colnames(par_reps),
    "lower" = rep(NA,dim(par_reps)[2]),
    "upper" = rep(NA,dim(par_reps)[2]),
    "includes_zero" = rep(NA,dim(par_reps)[2]),
    "credMass" = cred_mass
  )
  intervals = apply(par_reps,MARGIN=2,FUN = hdi,credMass = cred_mass)
  coef_hdi$lower = intervals[1,]
  coef_hdi$upper = intervals[2,]
  coef_hdi$includes_zero = rep(0,length(intervals[1,])) %between% list(intervals[1,],intervals[2,])

  return(coef_hdi)
}


#' Sample a subset of students
#'
#' @description Legacy exported helper that samples up to `nSub` students and returns all of their rows. It is retained because `LKT_HDI()` and older examples use it.
#'
#' @param data Data frame or data table containing `Anon.Student.Id`.
#' @param nSub Number of students to sample.
#' @return A data frame containing all rows for the sampled students.
#' @examples
#' data(samplelkt)
#' subset_data <- smallSet(samplelkt, 2)
#' length(unique(subset_data$Anon.Student.Id))
#' @export
smallSet <- function(data, nSub) {
  totsub <- length(unique(data$Anon.Student.Id))
  datasub <- unique(data$Anon.Student.Id)
  smallSub <- datasub[sample(1:totsub)[1:nSub]]

  smallIdx <- which(data$Anon.Student.Id %in% smallSub)
  smalldata <- data[smallIdx, ]
  smalldata <- droplevels(smalldata)
  return(smalldata)
}


#' Estimate coefficient HDIs by bootstrap resampling
#'
#' @description Fits a full LKT model and repeated student-level bootstrap samples, then summarizes coefficient intervals with `HDInterval`. This helper is computation-heavy and is retained as a legacy public interpretation aid.
#'
#' @param dat Data frame or data table containing trial data for `LKT()`.
#' @param n_boot Number of bootstrap subsamples to fit.
#' @param n_students Number of students per bootstrap subsample.
#' @param comps Character vector of components passed to `LKT()`.
#' @param feats Character vector of features passed to `LKT()`.
#' @param conns Character vector of formula connectors passed to `LKT()`.
#' @param ints Optional interactions passed to `LKT()`.
#' @param fixeds Fixed nonlinear parameters passed to `LKT()`.
#' @param get_hdi Logical compatibility argument. The current implementation always returns `coef_hdi`.
#' @param cred_mass Credibility mass used for HDI width.
#' @return A list containing bootstrap coefficient replicates (`par_reps`), the full-data model (`mod_full`), and coefficient intervals (`coef_hdi`).
#' @examples
#' \donttest{
#' data(samplelkt)
#' hdi_result <- LKT_HDI(
#'   samplelkt,
#'   n_boot = 2,
#'   n_students = 2,
#'   comps = "KC..Default.",
#'   feats = "intercept",
#'   fixeds = numeric(0)
#' )
#' names(hdi_result)
#' }
#' @export
LKT_HDI <- function(dat, n_boot, n_students, comps, feats,conns = rep("+",max(1,length(comps)-1)), ints = NA, fixeds, get_hdi = TRUE, cred_mass = .95) {

  # First fit full to get all features to get all predictor names
  mod_full = LKT(setDT(dat), interc = TRUE,
                 interacts = ints, connectors = conns,
                 components = comps,
                 features = feats,
                 fixedpars = fixeds,
                 seedpars = c(NA), verbose = FALSE)

  par_reps = matrix(nrow = n_boot, ncol = length(mod_full$coefs))
  colnames(par_reps) <- rownames(mod_full$coefs)

  for(i in 1:n_boot) {
    # First trial, return the names and make the matrix
    temp = boot_fn(dat, n_students, comps, feats, ints, fixeds, conns)
    idx = match(rownames(temp), colnames(par_reps))
    par_reps[i, idx] = as.numeric(temp)
    if(i == 1) {
      cat("0%")
    } else {
      cat(paste("...", round((i / n_boot) * 100), "%", sep = ""))
    }
    if(i == n_boot) {
      cat("\n")
    }
  }
  return(list("par_reps" = par_reps, "mod_full" = mod_full, coef_hdi = get_hdi(par_reps, cred_mass = cred_mass)))
}
