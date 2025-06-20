#' @title Extract numeric value from string
#' @description Extracts the first numeric value from a string.
#' @param name A string containing a numeric value.
#' @return A numeric value.
#' @examples
#' get_quantile_value("quantiles = 0.001")
#' @export
get_quantile_value <- function(name) {
  value <- as.numeric(regmatches(name, regexpr("[0-9.]+", name)))
  return(value)
}

#' @title Find the closest quantile index
#' @description Finds the closest quantile value (from names) to a given value in a vector.
#' @param x A named numeric vector (names should contain quantile info).
#' @param y A value to match.
#' @return The quantile value (numeric) closest to y.
#' @examples
#' find_closest_quantile_index(setNames(1:5, paste0("quantiles = ", seq(0.1, 0.5, 0.1))), 3.5)
#' @export
find_closest_quantile_index <- function(x, y) {
  if (is.null(names(x))) stop("x must have names indicating quantiles")
  index <- which(x == y)
  if (length(index) >= 1) {
    value <- get_quantile_value(names(x)[index])
    return(value)
  } else {
    closest_index <- which.min(abs(x - y))
    value <- get_quantile_value(names(x)[closest_index])
    return(value)
  }
}

#' @title Get rank for response
#' @description Finds the appropriate rank for a response value in quantile predictions.
#' @param response A vector of response values.
#' @param outMatrix A matrix of quantile predictions.
#' @param median_outMatrix A vector of median predictions.
#' @param rmse_ RMSE value for the predictions.
#' @return A vector of ranks.
#' @export
get_right_rank <- function(response, outMatrix, median_outMatrix, rmse_) {
  vapply(seq_along(response), function(i) {
    rank_ <- find_closest_quantile_index(outMatrix[i, ], response[i])
    if (length(rank_) > 1) {
      diff <- response[i] - median_outMatrix[i]
      if (abs(diff) > 3 * rmse_ && diff < 0) {
        return(min(rank_))
      } else if (abs(diff) > 3 * rmse_ && diff > 0) {
        return(max(rank_))
      } else {
        return(mean(rank_))
      }
    } else {
      return(rank_)
    }
  }, numeric(1))
}

#' @title Outlier detection using quantile random forest
#' @description Detects outliers in a dataset using quantile random forests.
#' @param data A data frame.
#' @param quantiles_type Quantile grid type: 1000, 400, or 40. Default is 1000.
#' @param threshold Outlier threshold (0-1). Default is 0.025.
#' @param impute Whether to impute missing values. Default TRUE.
#' @param verbose Verbosity level. Default 1.
#' @param weight Whether to use weighted threshold. Default FALSE.
#' @param ... Additional arguments passed to ranger.
#' @return An object of class "outqrf" with outlier info and model stats.
#' @examples
#' iris_with_outliers <- generateOutliers(iris, p=0.05)
#' qrf = outqrf(iris_with_outliers)
#' qrf$outliers
#' evaluateOutliers(iris, iris_with_outliers, qrf$outliers)
#' @export
outqrf <- function(data,
                   quantiles_type = 1000,
                   threshold = 0.025,
                   impute = TRUE,
                   verbose = 1,
                   weight = FALSE,
                   ...) {
  # Input checks
  if (!is.data.frame(data)) data <- as.data.frame(data)
  if (!is.numeric(threshold) || threshold < 0 || threshold > 1) stop("Threshold should be a numeric value between 0 and 1.")
  if (!(quantiles_type %in% c(1000, 400, 40))) stop("quantiles_type should be one of 1000, 400, 40")
  if (!requireNamespace("ranger", quietly = TRUE)) stop("Package 'ranger' is required.")
  if (!requireNamespace("missRanger", quietly = TRUE)) stop("Package 'missRanger' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")

  # Impute missing values if needed
  if (anyNA(data)) {
    if (impute) {
      data <- missRanger::missRanger(data, pmm.k = 3, num.trees = 500, data_only = TRUE, verbose = 0)
    } else {
      stop("Missing values detected. Please impute them first!")
    }
  }

  threshold_low <- threshold
  threshold_high <- 1 - threshold
  numeric_features <- names(data)[sapply(data, is.numeric)]
  rmse <- numeric()
  oob.error <- numeric()
  r.squared <- numeric()
  outliers_list <- list()
  outMatrices <- list()

  quantiles <- switch(
    as.character(quantiles_type),
    "1000" = seq(0.001, 0.999, 0.001),
    "400"  = seq(0.0025, 0.9975, 0.0025),
    "40"   = seq(0.025, 0.975, 0.025)
  )

  if (verbose) {
    cat("\nOutlier identification by quantile random forests\n")
    cat("\n  Variables to check:\t\t", paste(numeric_features, collapse = ", "))
    cat("\n  Variables used to check:\t", paste(names(data), collapse = ", "))
    cat("\n\n  Checking: ")
  }

  for (v in numeric_features) {
    if (verbose) cat(v, " ")
    covariables <- setdiff(names(data), v)
    qrf <- ranger::ranger(
      formula = stats::reformulate(covariables, response = v),
      data = data,
      quantreg = TRUE,
      ...
    )
    pred <- predict(qrf, data[covariables], type = "quantiles", quantiles = quantiles)
    oob.error <- c(oob.error, qrf$prediction.error)
    r.squared <- c(r.squared, qrf$r.squared)
    outMatrix <- pred$predictions
    outMatrices[[v]] <- outMatrix
    median_outMatrix <- outMatrix[, ceiling(ncol(outMatrix) / 2)]
    response <- data[[v]]
    diffs <- response - median_outMatrix
    rmse_ <- sqrt(mean(diffs^2))
    rmse <- c(rmse, rmse_)
    rank_value <- get_right_rank(response, outMatrix, median_outMatrix, rmse_)
    outlier <- data.frame(
      row = seq_len(nrow(data)),
      col = v,
      observed = response,
      predicted = median_outMatrix,
      rank = rank_value
    )
    if (weight) {
      outlier <- dplyr::filter(outlier, rank <= threshold_low * qrf$r.squared | rank >= 1 - threshold_low * qrf$r.squared)
    } else {
      outlier <- dplyr::filter(outlier, rank <= threshold_low | rank >= threshold_high)
    }
    outliers_list[[v]] <- outlier
  }

  outliers <- dplyr::bind_rows(outliers_list)
  names(rmse) <- numeric_features
  names(oob.error) <- numeric_features
  names(r.squared) <- numeric_features

  result <- list(
    Data = data,
    outliers = outliers,
    n_outliers = table(outliers$col),
    threshold = threshold,
    rmse = rmse,
    oob.error = oob.error,
    r.squared = r.squared,
    outMatrices = outMatrices,
    quantiles_type = quantiles_type
  )
  class(result) <- "outqrf"
  return(result)
}
