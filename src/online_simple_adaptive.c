#include <R.h>
#include <Rinternals.h>
#include <R_ext/Boolean.h>
#include <math.h>

static double clamp_probability(double p, double eps) {
  if (p < eps) {
    return eps;
  }
  if (p > 1.0 - eps) {
    return 1.0 - eps;
  }
  return p;
}

static double sigmoid_stable(double eta) {
  if (eta >= 0.0) {
    double z = exp(-eta);
    return 1.0 / (1.0 + z);
  }
  double z = exp(eta);
  return z / (1.0 + z);
}

SEXP C_lkt_online_simple_adaptive_eval(SEXP par_,
                                       SEXP y_,
                                       SEXP x_recency_,
                                       SEXP x_logsuc_,
                                       SEXP x_logitdec_,
                                       SEXP subject_start_,
                                       SEXP subject_end_,
                                       SEXP clip_epsilon_,
                                       SEXP denom_epsilon_,
                                       SEXP return_details_) {
  const double *par = REAL(par_);
  const double beta_intercept = par[0];
  const double beta_recency_start = par[1];
  const double beta_logsuc_start = par[2];
  const double beta_logitdec = par[3];
  const double alpha_recency = par[4];
  const double alpha_logsuc = par[5];

  const double *y = REAL(y_);
  const double *x_recency = REAL(x_recency_);
  const double *x_logsuc = REAL(x_logsuc_);
  const double *x_logitdec = REAL(x_logitdec_);
  const int *subject_start = INTEGER(subject_start_);
  const int *subject_end = INTEGER(subject_end_);
  const int n_subject = LENGTH(subject_start_);
  const int n_obs = LENGTH(y_);
  const double clip_epsilon = REAL(clip_epsilon_)[0];
  const double denom_epsilon = REAL(denom_epsilon_)[0];
  const Rboolean return_details = asLogical(return_details_) == TRUE;

  SEXP pred_ = R_NilValue;
  SEXP final_recency_ = R_NilValue;
  SEXP final_logsuc_ = R_NilValue;
  double *pred = NULL;
  double *final_recency = NULL;
  double *final_logsuc = NULL;
  int protect_count = 0;

  if (return_details) {
    PROTECT(pred_ = allocVector(REALSXP, n_obs));
    protect_count++;
    PROTECT(final_recency_ = allocVector(REALSXP, n_subject));
    protect_count++;
    PROTECT(final_logsuc_ = allocVector(REALSXP, n_subject));
    protect_count++;
    pred = REAL(pred_);
    final_recency = REAL(final_recency_);
    final_logsuc = REAL(final_logsuc_);
  }

  double loglike = 0.0;

  for (int s = 0; s < n_subject; s++) {
    double beta_recency = beta_recency_start;
    double beta_logsuc = beta_logsuc_start;
    const int start = subject_start[s] - 1;
    const int end = subject_end[s] - 1;

    for (int i = start; i <= end; i++) {
      double eta = beta_intercept +
        beta_recency * x_recency[i] +
        beta_logsuc * x_logsuc[i] +
        beta_logitdec * x_logitdec[i];
      double p = clamp_probability(sigmoid_stable(eta), clip_epsilon);
      double err = y[i] - p;

      if (return_details) {
        pred[i] = p;
      }

      if (y[i] == 1.0) {
        loglike += log(p);
      } else {
        loglike += log(1.0 - p);
      }

      if (x_recency[i] != 0.0 && alpha_recency != 0.0) {
        double denom = x_recency[i] * x_recency[i] + denom_epsilon;
        beta_recency += alpha_recency * x_recency[i] * err / denom;
      }
      if (x_logsuc[i] != 0.0 && alpha_logsuc != 0.0) {
        double denom = x_logsuc[i] * x_logsuc[i] + denom_epsilon;
        beta_logsuc += alpha_logsuc * x_logsuc[i] * err / denom;
      }
    }

    if (return_details) {
      final_recency[s] = beta_recency;
      final_logsuc[s] = beta_logsuc;
    }
  }

  if (!return_details) {
    SEXP result;
    PROTECT(result = allocVector(REALSXP, 1));
    REAL(result)[0] = -loglike;
    UNPROTECT(1);
    return result;
  }

  SEXP result;
  SEXP nll_;
  PROTECT(result = allocVector(VECSXP, 4));
  protect_count++;
  PROTECT(nll_ = ScalarReal(-loglike));
  protect_count++;
  SET_VECTOR_ELT(result, 0, nll_);
  SET_VECTOR_ELT(result, 1, pred_);
  SET_VECTOR_ELT(result, 2, final_recency_);
  SET_VECTOR_ELT(result, 3, final_logsuc_);

  SEXP names;
  PROTECT(names = allocVector(STRSXP, 4));
  protect_count++;
  SET_STRING_ELT(names, 0, mkChar("nll"));
  SET_STRING_ELT(names, 1, mkChar("pred"));
  SET_STRING_ELT(names, 2, mkChar("final_recency"));
  SET_STRING_ELT(names, 3, mkChar("final_logsuc"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(protect_count);
  return result;
}

SEXP C_lkt_online_simple_adaptive_gradient(SEXP par_,
                                           SEXP y_,
                                           SEXP x_recency_,
                                           SEXP x_logsuc_,
                                           SEXP x_logitdec_,
                                           SEXP subject_start_,
                                           SEXP subject_end_,
                                           SEXP clip_epsilon_,
                                           SEXP denom_epsilon_) {
  const double *par = REAL(par_);
  const double beta_intercept = par[0];
  const double beta_recency_start = par[1];
  const double beta_logsuc_start = par[2];
  const double beta_logitdec = par[3];
  const double alpha_recency = par[4];
  const double alpha_logsuc = par[5];

  const double *y = REAL(y_);
  const double *x_recency = REAL(x_recency_);
  const double *x_logsuc = REAL(x_logsuc_);
  const double *x_logitdec = REAL(x_logitdec_);
  const int *subject_start = INTEGER(subject_start_);
  const int *subject_end = INTEGER(subject_end_);
  const int n_subject = LENGTH(subject_start_);
  const double clip_epsilon = REAL(clip_epsilon_)[0];
  const double denom_epsilon = REAL(denom_epsilon_)[0];

  SEXP gradient_;
  PROTECT(gradient_ = allocVector(REALSXP, 6));
  double *gradient = REAL(gradient_);
  for (int k = 0; k < 6; k++) {
    gradient[k] = 0.0;
  }

  for (int s = 0; s < n_subject; s++) {
    double beta_recency = beta_recency_start;
    double beta_logsuc = beta_logsuc_start;
    double d_beta_recency[6] = {0.0, 1.0, 0.0, 0.0, 0.0, 0.0};
    double d_beta_logsuc[6] = {0.0, 0.0, 1.0, 0.0, 0.0, 0.0};
    const int start = subject_start[s] - 1;
    const int end = subject_end[s] - 1;

    for (int i = start; i <= end; i++) {
      const double xr = x_recency[i];
      const double xl = x_logsuc[i];
      const double xg = x_logitdec[i];
      const double eta = beta_intercept +
        beta_recency * xr +
        beta_logsuc * xl +
        beta_logitdec * xg;
      const double raw_p = sigmoid_stable(eta);
      const double p = clamp_probability(raw_p, clip_epsilon);
      double d_eta[6];
      double d_p[6];

      for (int k = 0; k < 6; k++) {
        d_eta[k] = xr * d_beta_recency[k] + xl * d_beta_logsuc[k];
      }
      d_eta[0] += 1.0;
      d_eta[3] += xg;

      if (raw_p > clip_epsilon && raw_p < 1.0 - clip_epsilon) {
        const double p_slope = raw_p * (1.0 - raw_p);
        for (int k = 0; k < 6; k++) {
          d_p[k] = p_slope * d_eta[k];
        }
      } else {
        for (int k = 0; k < 6; k++) {
          d_p[k] = 0.0;
        }
      }

      for (int k = 0; k < 6; k++) {
        gradient[k] += (p - y[i]) * d_eta[k];
      }

      const double err = y[i] - p;
      if (xr != 0.0) {
        const double denom = xr * xr + denom_epsilon;
        if (alpha_recency != 0.0) {
          for (int k = 0; k < 6; k++) {
            d_beta_recency[k] += alpha_recency * xr * (-d_p[k]) / denom;
          }
        }
        d_beta_recency[4] += xr * err / denom;
        beta_recency += alpha_recency * xr * err / denom;
      }

      if (xl != 0.0) {
        const double denom = xl * xl + denom_epsilon;
        if (alpha_logsuc != 0.0) {
          for (int k = 0; k < 6; k++) {
            d_beta_logsuc[k] += alpha_logsuc * xl * (-d_p[k]) / denom;
          }
        }
        d_beta_logsuc[5] += xl * err / denom;
        beta_logsuc += alpha_logsuc * xl * err / denom;
      }
    }
  }

  UNPROTECT(1);
  return gradient_;
}

SEXP C_lkt_online_simple_adaptive_value_gradient(SEXP par_,
                                                 SEXP y_,
                                                 SEXP x_recency_,
                                                 SEXP x_logsuc_,
                                                 SEXP x_logitdec_,
                                                 SEXP subject_start_,
                                                 SEXP subject_end_,
                                                 SEXP clip_epsilon_,
                                                 SEXP denom_epsilon_) {
  const double *par = REAL(par_);
  const double beta_intercept = par[0];
  const double beta_recency_start = par[1];
  const double beta_logsuc_start = par[2];
  const double beta_logitdec = par[3];
  const double alpha_recency = par[4];
  const double alpha_logsuc = par[5];

  const double *y = REAL(y_);
  const double *x_recency = REAL(x_recency_);
  const double *x_logsuc = REAL(x_logsuc_);
  const double *x_logitdec = REAL(x_logitdec_);
  const int *subject_start = INTEGER(subject_start_);
  const int *subject_end = INTEGER(subject_end_);
  const int n_subject = LENGTH(subject_start_);
  const double clip_epsilon = REAL(clip_epsilon_)[0];
  const double denom_epsilon = REAL(denom_epsilon_)[0];

  SEXP result_;
  SEXP nll_;
  SEXP gradient_;
  SEXP names_;
  PROTECT(result_ = allocVector(VECSXP, 2));
  PROTECT(nll_ = allocVector(REALSXP, 1));
  PROTECT(gradient_ = allocVector(REALSXP, 6));
  PROTECT(names_ = allocVector(STRSXP, 2));

  double *gradient = REAL(gradient_);
  for (int k = 0; k < 6; k++) {
    gradient[k] = 0.0;
  }

  double loglike = 0.0;

  for (int s = 0; s < n_subject; s++) {
    double beta_recency = beta_recency_start;
    double beta_logsuc = beta_logsuc_start;
    double d_beta_recency[6] = {0.0, 1.0, 0.0, 0.0, 0.0, 0.0};
    double d_beta_logsuc[6] = {0.0, 0.0, 1.0, 0.0, 0.0, 0.0};
    const int start = subject_start[s] - 1;
    const int end = subject_end[s] - 1;

    for (int i = start; i <= end; i++) {
      const double xr = x_recency[i];
      const double xl = x_logsuc[i];
      const double xg = x_logitdec[i];
      const double eta = beta_intercept +
        beta_recency * xr +
        beta_logsuc * xl +
        beta_logitdec * xg;
      const double raw_p = sigmoid_stable(eta);
      const double p = clamp_probability(raw_p, clip_epsilon);
      double d_eta[6];
      double d_p[6];

      if (y[i] == 1.0) {
        loglike += log(p);
      } else {
        loglike += log(1.0 - p);
      }

      for (int k = 0; k < 6; k++) {
        d_eta[k] = xr * d_beta_recency[k] + xl * d_beta_logsuc[k];
      }
      d_eta[0] += 1.0;
      d_eta[3] += xg;

      if (raw_p > clip_epsilon && raw_p < 1.0 - clip_epsilon) {
        const double p_slope = raw_p * (1.0 - raw_p);
        for (int k = 0; k < 6; k++) {
          d_p[k] = p_slope * d_eta[k];
        }
      } else {
        for (int k = 0; k < 6; k++) {
          d_p[k] = 0.0;
        }
      }

      for (int k = 0; k < 6; k++) {
        gradient[k] += (p - y[i]) * d_eta[k];
      }

      const double err = y[i] - p;
      if (xr != 0.0) {
        const double denom = xr * xr + denom_epsilon;
        if (alpha_recency != 0.0) {
          for (int k = 0; k < 6; k++) {
            d_beta_recency[k] += alpha_recency * xr * (-d_p[k]) / denom;
          }
        }
        d_beta_recency[4] += xr * err / denom;
        beta_recency += alpha_recency * xr * err / denom;
      }

      if (xl != 0.0) {
        const double denom = xl * xl + denom_epsilon;
        if (alpha_logsuc != 0.0) {
          for (int k = 0; k < 6; k++) {
            d_beta_logsuc[k] += alpha_logsuc * xl * (-d_p[k]) / denom;
          }
        }
        d_beta_logsuc[5] += xl * err / denom;
        beta_logsuc += alpha_logsuc * xl * err / denom;
      }
    }
  }

  REAL(nll_)[0] = -loglike;
  SET_VECTOR_ELT(result_, 0, nll_);
  SET_VECTOR_ELT(result_, 1, gradient_);
  SET_STRING_ELT(names_, 0, mkChar("nll"));
  SET_STRING_ELT(names_, 1, mkChar("gradient"));
  setAttrib(result_, R_NamesSymbol, names_);

  UNPROTECT(4);
  return result_;
}

SEXP C_lkt_online_simple_adaptive_decay_eval(SEXP par_,
                                             SEXP y_,
                                             SEXP x_spacing_,
                                             SEXP x_logsuc_,
                                             SEXP x_logitdec_,
                                             SEXP subject_start_,
                                             SEXP subject_end_,
                                             SEXP recency_decay_start_,
                                             SEXP decay_lower_,
                                             SEXP decay_upper_,
                                             SEXP clip_epsilon_,
                                             SEXP denom_epsilon_,
                                             SEXP return_details_) {
  const double *par = REAL(par_);
  const double beta_intercept = par[0];
  const double beta_recency_start = par[1];
  const double beta_logsuc_start = par[2];
  const double beta_logitdec = par[3];
  const double alpha_recency = par[4];
  const double alpha_logsuc = par[5];
  const double alpha_decay = par[6];

  const double *y = REAL(y_);
  const double *x_spacing = REAL(x_spacing_);
  const double *x_logsuc = REAL(x_logsuc_);
  const double *x_logitdec = REAL(x_logitdec_);
  const int *subject_start = INTEGER(subject_start_);
  const int *subject_end = INTEGER(subject_end_);
  const int n_subject = LENGTH(subject_start_);
  const int n_obs = LENGTH(y_);
  const double recency_decay_start = REAL(recency_decay_start_)[0];
  const double decay_lower = REAL(decay_lower_)[0];
  const double decay_upper = REAL(decay_upper_)[0];
  const double clip_epsilon = REAL(clip_epsilon_)[0];
  const double denom_epsilon = REAL(denom_epsilon_)[0];
  const Rboolean return_details = asLogical(return_details_) == TRUE;

  SEXP pred_ = R_NilValue;
  SEXP final_recency_ = R_NilValue;
  SEXP final_logsuc_ = R_NilValue;
  SEXP final_decay_ = R_NilValue;
  double *pred = NULL;
  double *final_recency = NULL;
  double *final_logsuc = NULL;
  double *final_decay = NULL;
  int protect_count = 0;

  if (return_details) {
    PROTECT(pred_ = allocVector(REALSXP, n_obs));
    protect_count++;
    PROTECT(final_recency_ = allocVector(REALSXP, n_subject));
    protect_count++;
    PROTECT(final_logsuc_ = allocVector(REALSXP, n_subject));
    protect_count++;
    PROTECT(final_decay_ = allocVector(REALSXP, n_subject));
    protect_count++;
    pred = REAL(pred_);
    final_recency = REAL(final_recency_);
    final_logsuc = REAL(final_logsuc_);
    final_decay = REAL(final_decay_);
  }

  double loglike = 0.0;

  for (int s = 0; s < n_subject; s++) {
    double beta_recency = beta_recency_start;
    double beta_logsuc = beta_logsuc_start;
    double recency_decay = recency_decay_start;
    const int start = subject_start[s] - 1;
    const int end = subject_end[s] - 1;

    for (int i = start; i <= end; i++) {
      const double spacing = x_spacing[i];
      const double xr = spacing > 0.0 ? pow(spacing, -recency_decay) : 0.0;
      const double xl = x_logsuc[i];
      const double xg = x_logitdec[i];
      const double eta = beta_intercept +
        beta_recency * xr +
        beta_logsuc * xl +
        beta_logitdec * xg;
      const double p = clamp_probability(sigmoid_stable(eta), clip_epsilon);
      const double err = y[i] - p;

      if (return_details) {
        pred[i] = p;
      }

      if (y[i] == 1.0) {
        loglike += log(p);
      } else {
        loglike += log(1.0 - p);
      }

      if (xr != 0.0 && alpha_recency != 0.0) {
        const double denom = xr * xr + denom_epsilon;
        beta_recency += alpha_recency * xr * err / denom;
      }
      if (xl != 0.0 && alpha_logsuc != 0.0) {
        const double denom = xl * xl + denom_epsilon;
        beta_logsuc += alpha_logsuc * xl * err / denom;
      }
      if (alpha_decay != 0.0) {
        recency_decay -= alpha_decay * err;
        if (recency_decay < decay_lower) {
          recency_decay = decay_lower;
        } else if (recency_decay > decay_upper) {
          recency_decay = decay_upper;
        }
      }
    }

    if (return_details) {
      final_recency[s] = beta_recency;
      final_logsuc[s] = beta_logsuc;
      final_decay[s] = recency_decay;
    }
  }

  if (!return_details) {
    SEXP result;
    PROTECT(result = allocVector(REALSXP, 1));
    REAL(result)[0] = -loglike;
    UNPROTECT(1);
    return result;
  }

  SEXP result;
  SEXP nll_;
  SEXP names;
  PROTECT(result = allocVector(VECSXP, 5));
  protect_count++;
  PROTECT(nll_ = ScalarReal(-loglike));
  protect_count++;
  SET_VECTOR_ELT(result, 0, nll_);
  SET_VECTOR_ELT(result, 1, pred_);
  SET_VECTOR_ELT(result, 2, final_recency_);
  SET_VECTOR_ELT(result, 3, final_logsuc_);
  SET_VECTOR_ELT(result, 4, final_decay_);

  PROTECT(names = allocVector(STRSXP, 5));
  protect_count++;
  SET_STRING_ELT(names, 0, mkChar("nll"));
  SET_STRING_ELT(names, 1, mkChar("pred"));
  SET_STRING_ELT(names, 2, mkChar("final_recency"));
  SET_STRING_ELT(names, 3, mkChar("final_logsuc"));
  SET_STRING_ELT(names, 4, mkChar("final_decay"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(protect_count);
  return result;
}

SEXP C_lkt_online_adaptive_beta_eval(SEXP alpha_,
                                     SEXP y_,
                                     SEXP eta_base_,
                                     SEXP x_beta_,
                                     SEXP beta_start_,
                                     SEXP subject_start_,
                                     SEXP subject_end_,
                                     SEXP clip_epsilon_,
                                     SEXP denom_epsilon_,
                                     SEXP return_details_) {
  const double *alpha = REAL(alpha_);
  const double *y = REAL(y_);
  const double *eta_base = REAL(eta_base_);
  const double *x_beta = REAL(x_beta_);
  const double *beta_start = REAL(beta_start_);
  const int *subject_start = INTEGER(subject_start_);
  const int *subject_end = INTEGER(subject_end_);
  const int n_subject = LENGTH(subject_start_);
  const int n_obs = LENGTH(y_);
  const int n_beta = LENGTH(alpha_);
  const double clip_epsilon = REAL(clip_epsilon_)[0];
  const double denom_epsilon = REAL(denom_epsilon_)[0];
  const Rboolean return_details = asLogical(return_details_) == TRUE;

  SEXP pred_ = R_NilValue;
  SEXP final_beta_ = R_NilValue;
  double *pred = NULL;
  double *final_beta = NULL;
  int protect_count = 0;

  if (return_details) {
    PROTECT(pred_ = allocVector(REALSXP, n_obs));
    protect_count++;
    PROTECT(final_beta_ = allocMatrix(REALSXP, n_subject, n_beta));
    protect_count++;
    pred = REAL(pred_);
    final_beta = REAL(final_beta_);
  }

  double *delta = (double *) R_alloc((size_t) n_beta, sizeof(double));
  double loglike = 0.0;

  for (int s = 0; s < n_subject; s++) {
    for (int j = 0; j < n_beta; j++) {
      delta[j] = 0.0;
    }
    const int start = subject_start[s] - 1;
    const int end = subject_end[s] - 1;

    for (int i = start; i <= end; i++) {
      double eta = eta_base[i];
      for (int j = 0; j < n_beta; j++) {
        eta += delta[j] * x_beta[i + n_obs * j];
      }
      const double p = clamp_probability(sigmoid_stable(eta), clip_epsilon);
      const double err = y[i] - p;

      if (return_details) {
        pred[i] = p;
      }

      if (y[i] == 1.0) {
        loglike += log(p);
      } else {
        loglike += log(1.0 - p);
      }

      for (int j = 0; j < n_beta; j++) {
        const double x = x_beta[i + n_obs * j];
        const double denom = x * x + denom_epsilon;
        delta[j] += alpha[j] * x * err / denom;
      }
    }

    if (return_details) {
      for (int j = 0; j < n_beta; j++) {
        final_beta[s + n_subject * j] = beta_start[j] + delta[j];
      }
    }
  }

  if (!return_details) {
    SEXP result;
    PROTECT(result = allocVector(REALSXP, 1));
    REAL(result)[0] = -loglike;
    UNPROTECT(1);
    return result;
  }

  SEXP result;
  SEXP nll_;
  SEXP names;
  PROTECT(result = allocVector(VECSXP, 3));
  protect_count++;
  PROTECT(nll_ = ScalarReal(-loglike));
  protect_count++;
  SET_VECTOR_ELT(result, 0, nll_);
  SET_VECTOR_ELT(result, 1, pred_);
  SET_VECTOR_ELT(result, 2, final_beta_);

  PROTECT(names = allocVector(STRSXP, 3));
  protect_count++;
  SET_STRING_ELT(names, 0, mkChar("nll"));
  SET_STRING_ELT(names, 1, mkChar("pred"));
  SET_STRING_ELT(names, 2, mkChar("final_beta"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(protect_count);
  return result;
}

SEXP C_lkt_online_adaptive_recency_decay_eval(SEXP alpha_,
                                              SEXP y_,
                                              SEXP eta_base_,
                                              SEXP x_beta_,
                                              SEXP beta_start_,
                                              SEXP x_spacing_,
                                              SEXP x_recency_original_,
                                              SEXP recency_coefficient_,
                                              SEXP subject_start_,
                                              SEXP subject_end_,
                                              SEXP recency_decay_start_,
                                              SEXP decay_lower_,
                                              SEXP decay_upper_,
                                              SEXP clip_epsilon_,
                                              SEXP denom_epsilon_,
                                              SEXP return_details_) {
  const double *alpha = REAL(alpha_);
  const double *y = REAL(y_);
  const double *eta_base = REAL(eta_base_);
  const double *x_beta = REAL(x_beta_);
  const double *beta_start = REAL(beta_start_);
  const double *x_spacing = REAL(x_spacing_);
  const double *x_recency_original = REAL(x_recency_original_);
  const double recency_coefficient = REAL(recency_coefficient_)[0];
  const int *subject_start = INTEGER(subject_start_);
  const int *subject_end = INTEGER(subject_end_);
  const int n_subject = LENGTH(subject_start_);
  const int n_obs = LENGTH(y_);
  const int n_alpha = LENGTH(alpha_);
  const int n_beta = LENGTH(beta_start_);
  const double recency_decay_start = REAL(recency_decay_start_)[0];
  const double decay_lower = REAL(decay_lower_)[0];
  const double decay_upper = REAL(decay_upper_)[0];
  const double clip_epsilon = REAL(clip_epsilon_)[0];
  const double denom_epsilon = REAL(denom_epsilon_)[0];
  const Rboolean return_details = asLogical(return_details_) == TRUE;

  if (n_alpha != n_beta + 1) {
    error("alpha length must equal selected beta count plus one decay alpha.");
  }

  SEXP pred_ = R_NilValue;
  SEXP final_beta_ = R_NilValue;
  SEXP final_decay_ = R_NilValue;
  double *pred = NULL;
  double *final_beta = NULL;
  double *final_decay = NULL;
  int protect_count = 0;

  if (return_details) {
    PROTECT(pred_ = allocVector(REALSXP, n_obs));
    protect_count++;
    PROTECT(final_beta_ = allocMatrix(REALSXP, n_subject, n_beta));
    protect_count++;
    PROTECT(final_decay_ = allocVector(REALSXP, n_subject));
    protect_count++;
    pred = REAL(pred_);
    final_beta = REAL(final_beta_);
    final_decay = REAL(final_decay_);
  }

  double *delta = (double *) R_alloc((size_t) n_beta, sizeof(double));
  double loglike = 0.0;

  for (int s = 0; s < n_subject; s++) {
    for (int j = 0; j < n_beta; j++) {
      delta[j] = 0.0;
    }
    double recency_decay = recency_decay_start;
    const int start = subject_start[s] - 1;
    const int end = subject_end[s] - 1;

    for (int i = start; i <= end; i++) {
      const double spacing = x_spacing[i];
      const double recency_current =
        spacing > 0.0 ? pow(spacing, -recency_decay) : 0.0;
      double eta = eta_base[i] +
        recency_coefficient * (recency_current - x_recency_original[i]);

      for (int j = 0; j < n_beta; j++) {
        eta += delta[j] * x_beta[i + n_obs * j];
      }

      const double p = clamp_probability(sigmoid_stable(eta), clip_epsilon);
      const double err = y[i] - p;

      if (return_details) {
        pred[i] = p;
      }

      if (y[i] == 1.0) {
        loglike += log(p);
      } else {
        loglike += log(1.0 - p);
      }

      for (int j = 0; j < n_beta; j++) {
        const double x = x_beta[i + n_obs * j];
        const double denom = x * x + denom_epsilon;
        delta[j] += alpha[j] * x * err / denom;
      }

      recency_decay -= alpha[n_beta] * err;
      if (recency_decay < decay_lower) {
        recency_decay = decay_lower;
      } else if (recency_decay > decay_upper) {
        recency_decay = decay_upper;
      }
    }

    if (return_details) {
      for (int j = 0; j < n_beta; j++) {
        final_beta[s + n_subject * j] = beta_start[j] + delta[j];
      }
      final_decay[s] = recency_decay;
    }
  }

  if (!return_details) {
    SEXP result;
    PROTECT(result = allocVector(REALSXP, 1));
    REAL(result)[0] = -loglike;
    UNPROTECT(1);
    return result;
  }

  SEXP result;
  SEXP nll_;
  SEXP names;
  PROTECT(result = allocVector(VECSXP, 4));
  protect_count++;
  PROTECT(nll_ = ScalarReal(-loglike));
  protect_count++;
  SET_VECTOR_ELT(result, 0, nll_);
  SET_VECTOR_ELT(result, 1, pred_);
  SET_VECTOR_ELT(result, 2, final_beta_);
  SET_VECTOR_ELT(result, 3, final_decay_);

  PROTECT(names = allocVector(STRSXP, 4));
  protect_count++;
  SET_STRING_ELT(names, 0, mkChar("nll"));
  SET_STRING_ELT(names, 1, mkChar("pred"));
  SET_STRING_ELT(names, 2, mkChar("final_beta"));
  SET_STRING_ELT(names, 3, mkChar("final_decay"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(protect_count);
  return result;
}

static double lkt_logitdec_previous_window(const double *y,
                                           int start,
                                           int current,
                                           double d) {
  const int last = current - 1;
  if (last < start) {
    return 0.0;
  }
  int first = last - 59;
  if (first < start) {
    first = start;
  }
  const int w = last - first + 1;
  double corv = pow(d, (double) w);
  double incorv = pow(d, (double) w);
  for (int k = 0; k < w; k++) {
    const int idx = first + k;
    const double exponent = (double) (w - 1 - k);
    const double weight = pow(d, exponent);
    corv += y[idx] * weight;
    incorv += fabs(y[idx] - 1.0) * weight;
  }
  return log(corv / incorv);
}

static double lkt_ppes_tw_current(const double *tn,
                                  int start,
                                  int current,
                                  double d) {
  if (current <= start) {
    return 1.0;
  }
  const double current_tn = tn[current];
  double weighted_sum = 0.0;
  double weight_total = 0.0;
  for (int k = start; k < current; k++) {
    const double lag = current_tn - tn[k];
    if (lag <= 0.0) {
      continue;
    }
    const double weight = pow(lag, -d);
    if (R_FINITE(weight)) {
      weighted_sum += weight * lag;
      weight_total += weight;
    }
  }
  if (weight_total <= 0.0 || !R_FINITE(weighted_sum)) {
    return 1.0;
  }
  return weighted_sum / weight_total;
}

SEXP C_lkt_online_adaptive_feature_group_eval(SEXP alpha_,
                                              SEXP y_,
                                              SEXP eta_base_,
                                              SEXP x_beta_,
                                              SEXP beta_start_,
                                              SEXP group_type_,
                                              SEXP group_aux_,
                                              SEXP group_original_,
                                              SEXP group_coefficient_,
                                              SEXP group_start_,
                                              SEXP group_active_,
                                              SEXP subject_start_,
                                              SEXP subject_end_,
                                              SEXP nonlinear_lower_,
                                              SEXP nonlinear_upper_,
                                              SEXP clip_epsilon_,
                                              SEXP denom_epsilon_,
                                              SEXP return_details_) {
  const double *alpha = REAL(alpha_);
  const double *y = REAL(y_);
  const double *eta_base = REAL(eta_base_);
  const double *x_beta = REAL(x_beta_);
  const double *beta_start = REAL(beta_start_);
  const int *group_type = INTEGER(group_type_);
  const double *group_aux = REAL(group_aux_);
  const double *group_original = REAL(group_original_);
  const double *group_coefficient = REAL(group_coefficient_);
  const double *group_start = REAL(group_start_);
  const int *group_active = INTEGER(group_active_);
  const int *subject_start = INTEGER(subject_start_);
  const int *subject_end = INTEGER(subject_end_);
  const int n_subject = LENGTH(subject_start_);
  const int n_obs = LENGTH(y_);
  const int n_beta = LENGTH(beta_start_);
  const int n_group = LENGTH(group_type_);
  const int n_alpha = LENGTH(alpha_);
  const double nonlinear_lower = REAL(nonlinear_lower_)[0];
  const double nonlinear_upper = REAL(nonlinear_upper_)[0];
  const double clip_epsilon = REAL(clip_epsilon_)[0];
  const double denom_epsilon = REAL(denom_epsilon_)[0];
  const Rboolean return_details = asLogical(return_details_) == TRUE;

  int n_active = 0;
  for (int g = 0; g < n_group; g++) {
    for (int slot = 0; slot < 5; slot++) {
      if (group_active[slot + 5 * g]) {
        n_active++;
      }
    }
  }
  if (n_alpha != n_beta + n_active) {
    error("alpha length must equal selected beta count plus active nonlinear slots.");
  }

  SEXP pred_ = R_NilValue;
  SEXP final_beta_ = R_NilValue;
  SEXP final_nonlinear_ = R_NilValue;
  double *pred = NULL;
  double *final_beta = NULL;
  double *final_nonlinear = NULL;
  int protect_count = 0;

  if (return_details) {
    PROTECT(pred_ = allocVector(REALSXP, n_obs));
    protect_count++;
    PROTECT(final_beta_ = allocMatrix(REALSXP, n_subject, n_beta));
    protect_count++;
    PROTECT(final_nonlinear_ = allocMatrix(REALSXP, n_subject, n_active));
    protect_count++;
    pred = REAL(pred_);
    final_beta = REAL(final_beta_);
    final_nonlinear = REAL(final_nonlinear_);
  }

  double *delta = (double *) R_alloc((size_t) n_beta, sizeof(double));
  double *state = (double *) R_alloc((size_t) (5 * n_group), sizeof(double));
  double loglike = 0.0;

  for (int s = 0; s < n_subject; s++) {
    for (int j = 0; j < n_beta; j++) {
      delta[j] = 0.0;
    }
    for (int g = 0; g < n_group; g++) {
      for (int slot = 0; slot < 5; slot++) {
        state[slot + 5 * g] = group_start[slot + 5 * g];
      }
    }

    const int start = subject_start[s] - 1;
    const int end = subject_end[s] - 1;

    for (int i = start; i <= end; i++) {
      double eta = eta_base[i];

      for (int j = 0; j < n_beta; j++) {
        eta += delta[j] * x_beta[i + n_obs * j];
      }

      for (int g = 0; g < n_group; g++) {
        double current_x = 0.0;
        if (group_type[g] == 1) {
          const double spacing = group_aux[i + n_obs * (3 * g)];
          current_x = spacing > 0.0 ? pow(spacing, -state[5 * g]) : 0.0;
        } else if (group_type[g] == 2) {
          current_x = lkt_logitdec_previous_window(
            y, start, i, state[5 * g]);
        } else if (group_type[g] == 3) {
          const double cor = group_aux[i + n_obs * (3 * g)];
          const double *tn = group_aux + n_obs * (3 * g + 1);
          const double space = group_aux[i + n_obs * (3 * g + 2)];
          const double para = state[5 * g];
          const double parb = state[1 + 5 * g];
          const double parc = state[2 + 5 * g];
          const double pard = state[3 + 5 * g];
          const double tw = lkt_ppes_tw_current(tn, start, i, pard);
          current_x = pow(cor, para) * pow(tw, -(parb + parc * space));
        } else {
          error("unsupported compiled nonlinear adaptive feature type.");
        }
        eta += group_coefficient[g] *
          (current_x - group_original[i + n_obs * g]);
      }

      const double p = clamp_probability(sigmoid_stable(eta), clip_epsilon);
      const double err = y[i] - p;

      if (return_details) {
        pred[i] = p;
      }

      if (y[i] == 1.0) {
        loglike += log(p);
      } else {
        loglike += log(1.0 - p);
      }

      for (int j = 0; j < n_beta; j++) {
        const double x = x_beta[i + n_obs * j];
        const double denom = x * x + denom_epsilon;
        delta[j] += alpha[j] * x * err / denom;
      }

      int alpha_pos = n_beta;
      for (int g = 0; g < n_group; g++) {
        for (int slot = 0; slot < 5; slot++) {
          if (group_active[slot + 5 * g]) {
            state[slot + 5 * g] -= alpha[alpha_pos] * err;
            if (state[slot + 5 * g] < nonlinear_lower) {
              state[slot + 5 * g] = nonlinear_lower;
            } else if (state[slot + 5 * g] > nonlinear_upper) {
              state[slot + 5 * g] = nonlinear_upper;
            }
            alpha_pos++;
          }
        }
      }
    }

    if (return_details) {
      for (int j = 0; j < n_beta; j++) {
        final_beta[s + n_subject * j] = beta_start[j] + delta[j];
      }
      int out_col = 0;
      for (int g = 0; g < n_group; g++) {
        for (int slot = 0; slot < 5; slot++) {
          if (group_active[slot + 5 * g]) {
            final_nonlinear[s + n_subject * out_col] = state[slot + 5 * g];
            out_col++;
          }
        }
      }
    }
  }

  if (!return_details) {
    SEXP result;
    PROTECT(result = allocVector(REALSXP, 1));
    REAL(result)[0] = -loglike;
    UNPROTECT(1);
    return result;
  }

  SEXP result;
  SEXP nll_;
  SEXP names;
  PROTECT(result = allocVector(VECSXP, 4));
  protect_count++;
  PROTECT(nll_ = ScalarReal(-loglike));
  protect_count++;
  SET_VECTOR_ELT(result, 0, nll_);
  SET_VECTOR_ELT(result, 1, pred_);
  SET_VECTOR_ELT(result, 2, final_beta_);
  SET_VECTOR_ELT(result, 3, final_nonlinear_);

  PROTECT(names = allocVector(STRSXP, 4));
  protect_count++;
  SET_STRING_ELT(names, 0, mkChar("nll"));
  SET_STRING_ELT(names, 1, mkChar("pred"));
  SET_STRING_ELT(names, 2, mkChar("final_beta"));
  SET_STRING_ELT(names, 3, mkChar("final_nonlinear"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(protect_count);
  return result;
}

SEXP C_lkt_online_adaptive_feature_decay_eval(SEXP alpha_,
                                              SEXP y_,
                                              SEXP eta_base_,
                                              SEXP x_beta_,
                                              SEXP beta_start_,
                                              SEXP nonlinear_type_,
                                              SEXP nonlinear_aux_,
                                              SEXP nonlinear_original_,
                                              SEXP nonlinear_coefficient_,
                                              SEXP nonlinear_start_,
                                              SEXP subject_start_,
                                              SEXP subject_end_,
                                              SEXP nonlinear_lower_,
                                              SEXP nonlinear_upper_,
                                              SEXP clip_epsilon_,
                                              SEXP denom_epsilon_,
                                              SEXP return_details_) {
  const double *alpha = REAL(alpha_);
  const double *y = REAL(y_);
  const double *eta_base = REAL(eta_base_);
  const double *x_beta = REAL(x_beta_);
  const double *beta_start = REAL(beta_start_);
  const int *nonlinear_type = INTEGER(nonlinear_type_);
  const double *nonlinear_aux = REAL(nonlinear_aux_);
  const double *nonlinear_original = REAL(nonlinear_original_);
  const double *nonlinear_coefficient = REAL(nonlinear_coefficient_);
  const double *nonlinear_start = REAL(nonlinear_start_);
  const int *subject_start = INTEGER(subject_start_);
  const int *subject_end = INTEGER(subject_end_);
  const int n_subject = LENGTH(subject_start_);
  const int n_obs = LENGTH(y_);
  const int n_beta = LENGTH(beta_start_);
  const int n_nonlinear = LENGTH(nonlinear_type_);
  const int n_alpha = LENGTH(alpha_);
  const double nonlinear_lower = REAL(nonlinear_lower_)[0];
  const double nonlinear_upper = REAL(nonlinear_upper_)[0];
  const double clip_epsilon = REAL(clip_epsilon_)[0];
  const double denom_epsilon = REAL(denom_epsilon_)[0];
  const Rboolean return_details = asLogical(return_details_) == TRUE;

  if (n_alpha != n_beta + n_nonlinear) {
    error("alpha length must equal selected beta count plus nonlinear count.");
  }

  SEXP pred_ = R_NilValue;
  SEXP final_beta_ = R_NilValue;
  SEXP final_nonlinear_ = R_NilValue;
  double *pred = NULL;
  double *final_beta = NULL;
  double *final_nonlinear = NULL;
  int protect_count = 0;

  if (return_details) {
    PROTECT(pred_ = allocVector(REALSXP, n_obs));
    protect_count++;
    PROTECT(final_beta_ = allocMatrix(REALSXP, n_subject, n_beta));
    protect_count++;
    PROTECT(final_nonlinear_ = allocMatrix(REALSXP, n_subject, n_nonlinear));
    protect_count++;
    pred = REAL(pred_);
    final_beta = REAL(final_beta_);
    final_nonlinear = REAL(final_nonlinear_);
  }

  double *delta = (double *) R_alloc((size_t) n_beta, sizeof(double));
  double *nonlinear_state =
    (double *) R_alloc((size_t) n_nonlinear, sizeof(double));
  double loglike = 0.0;

  for (int s = 0; s < n_subject; s++) {
    for (int j = 0; j < n_beta; j++) {
      delta[j] = 0.0;
    }
    for (int j = 0; j < n_nonlinear; j++) {
      nonlinear_state[j] = nonlinear_start[j];
    }

    const int start = subject_start[s] - 1;
    const int end = subject_end[s] - 1;

    for (int i = start; i <= end; i++) {
      double eta = eta_base[i];

      for (int j = 0; j < n_beta; j++) {
        eta += delta[j] * x_beta[i + n_obs * j];
      }

      for (int j = 0; j < n_nonlinear; j++) {
        double current_x = 0.0;
        if (nonlinear_type[j] == 1) {
          const double spacing = nonlinear_aux[i + n_obs * j];
          current_x = spacing > 0.0 ? pow(spacing, -nonlinear_state[j]) : 0.0;
        } else if (nonlinear_type[j] == 2) {
          current_x = lkt_logitdec_previous_window(
            y, start, i, nonlinear_state[j]);
        } else {
          error("unsupported compiled nonlinear adaptive feature type.");
        }
        eta += nonlinear_coefficient[j] *
          (current_x - nonlinear_original[i + n_obs * j]);
      }

      const double p = clamp_probability(sigmoid_stable(eta), clip_epsilon);
      const double err = y[i] - p;

      if (return_details) {
        pred[i] = p;
      }

      if (y[i] == 1.0) {
        loglike += log(p);
      } else {
        loglike += log(1.0 - p);
      }

      for (int j = 0; j < n_beta; j++) {
        const double x = x_beta[i + n_obs * j];
        const double denom = x * x + denom_epsilon;
        delta[j] += alpha[j] * x * err / denom;
      }

      for (int j = 0; j < n_nonlinear; j++) {
        nonlinear_state[j] -= alpha[n_beta + j] * err;
        if (nonlinear_state[j] < nonlinear_lower) {
          nonlinear_state[j] = nonlinear_lower;
        } else if (nonlinear_state[j] > nonlinear_upper) {
          nonlinear_state[j] = nonlinear_upper;
        }
      }
    }

    if (return_details) {
      for (int j = 0; j < n_beta; j++) {
        final_beta[s + n_subject * j] = beta_start[j] + delta[j];
      }
      for (int j = 0; j < n_nonlinear; j++) {
        final_nonlinear[s + n_subject * j] = nonlinear_state[j];
      }
    }
  }

  if (!return_details) {
    SEXP result;
    PROTECT(result = allocVector(REALSXP, 1));
    REAL(result)[0] = -loglike;
    UNPROTECT(1);
    return result;
  }

  SEXP result;
  SEXP nll_;
  SEXP names;
  PROTECT(result = allocVector(VECSXP, 4));
  protect_count++;
  PROTECT(nll_ = ScalarReal(-loglike));
  protect_count++;
  SET_VECTOR_ELT(result, 0, nll_);
  SET_VECTOR_ELT(result, 1, pred_);
  SET_VECTOR_ELT(result, 2, final_beta_);
  SET_VECTOR_ELT(result, 3, final_nonlinear_);

  PROTECT(names = allocVector(STRSXP, 4));
  protect_count++;
  SET_STRING_ELT(names, 0, mkChar("nll"));
  SET_STRING_ELT(names, 1, mkChar("pred"));
  SET_STRING_ELT(names, 2, mkChar("final_beta"));
  SET_STRING_ELT(names, 3, mkChar("final_nonlinear"));
  setAttrib(result, R_NamesSymbol, names);

  UNPROTECT(protect_count);
  return result;
}
