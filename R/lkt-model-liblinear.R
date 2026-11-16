#' Define the default LibLinear LKT model
#'
#' @description Creates the default LKT model adapter backed by `LiblineaR`. This model uses the shared LKT feature input and returns the common `LKTModelFit()` shape.
#'
#' @return A model adapter suitable for `LKT(model = ...)`.
#' @examples
#' adapter <- LibLinearModel()
#' adapter$name
#' @export
LibLinearModel <- function() {
  LKTCustomModel(
    name = "LibLinear",
    fit = function(request, verbose = FALSE) {
      lkt_fit_liblinear(
        design_matrix = request$design_matrix,
        data = request$data,
        usefolds = request$usefolds,
        bias = request$options$bias,
        cost = request$options$cost,
        epsilon = request$options$epsilon,
        type = request$options$type,
        verbose = verbose)
    }
  )
}

lkt_fit_liblinear <- function(design_matrix, data, usefolds, bias, cost,
                              epsilon, type, verbose = FALSE) {
  predictset <- design_matrix$sparse
  predictset2 <- design_matrix$csr
  pred <- data$pred
  success <- FALSE
  keep <- !is.na(data$CF..ansbin.)
  if (!is.na(usefolds)[1]) keep <- keep & data$fold %in% usefolds
  if (!any(keep)) stop("No scored responses selected for fitting.", call. = FALSE)

  while (!success) {
    if (is.na(usefolds)[1]) {
      temp <- LiblineaR(predictset2[keep, ], data$CF..ansbin.[keep],
                        bias = bias,
                        cost = cost, epsilon = epsilon, type = type)
    } else {
      temp <- LiblineaR(predictset2[keep, ], data$CF..ansbin.[keep],
                        bias = bias,
                        cost = cost, epsilon = epsilon, type = type)
    }
    if (temp$ClassNames[1] == 0) {
      temp$W <- temp$W * (-1)
    }
    modelvs <- data.frame(temp$W)
    colnames(modelvs) <- colnames(predictset)
    modelvs <- t(modelvs)
    colnames(modelvs) <- "coefficient"

    if (is.na(usefolds)[1]) {
      pred <- pmin(pmax(
        predict(temp, predictset2, proba = TRUE)$probabilities[, 1],
        .00001
      ), .99999)
      success <- sum(is.nan(pred)) == 0 && sum(is.na(pred)) == 0
    } else {
      pred <- pmin(pmax(
        predict(temp, predictset2, proba = TRUE)$probabilities[, 1],
        .00001
      ), .99999)
      success <- sum(is.nan(pred)) == 0 && sum(is.na(pred)) == 0

      if (verbose) {
        print(summary(pred[keep]))
      }
    }
  }

  if (is.na(usefolds)[1]) {
    fitstat <- sum(log(ifelse(data$CF..ansbin.[keep] == 1, pred[keep], 1 - pred[keep])))
  } else {
    fitstat <- sum(log(ifelse(data$CF..ansbin.[keep] == 1,
                              pred[keep],
                              1 - pred[keep])))
  }

  LKTModelFit(
    model = temp,
    pred = pred,
    modelvs = modelvs,
    fitstat = fitstat,
    predictset = predictset,
    predictset2 = predictset2
  )
}
