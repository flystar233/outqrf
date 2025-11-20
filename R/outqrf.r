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

#' @title Get quantile values from vector
#' @description Efficiently extracts quantile values from named vector
#' @param x A named vector with quantile information in names
#' @return A numeric vector of quantile values
#' @keywords internal
get_quantile_values_vectorized <- function(x) {
  if (is.null(names(x))) return(NULL)
  # Pre-extract all quantile values at once to avoid repeated string parsing
  quantile_values <- as.numeric(regmatches(names(x), regexpr("[0-9.]+", names(x))))
  return(quantile_values)
}

#' @title Find the closest quantile index (LEGACY)
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

#' @title Find closest quantile index (optimized)
#' @description Efficiently finds the closest quantile using binary search
#' @param sorted_predictions Sorted vector of predictions
#' @param quantile_values Pre-computed quantile values corresponding to predictions
#' @param response_value The response value to find quantile for
#' @return The quantile value closest to the response
#' @keywords internal
find_closest_quantile_optimized <- function(sorted_predictions, quantile_values, response_value) {
  if (length(sorted_predictions) == 0) return(0.5)
  
  # Use findInterval for efficient binary search
  interval_idx <- findInterval(response_value, sorted_predictions, rightmost.closed = TRUE)
  
  # Handle edge cases
  if (interval_idx == 0) {
    return(quantile_values[1])
  } else if (interval_idx >= length(sorted_predictions)) {
    return(quantile_values[length(quantile_values)])
  } else {
    # Find the closest between left and right boundaries
    left_dist <- abs(response_value - sorted_predictions[interval_idx])
    right_dist <- abs(response_value - sorted_predictions[interval_idx + 1])
    
    if (left_dist <= right_dist) {
      return(quantile_values[interval_idx])
    } else {
      return(quantile_values[interval_idx + 1])
    }
  }
}

#' @title Get rank for response (LEGACY)
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

#' @title Get rank for response (optimized)
#' @description Vectorized computation of ranks for response values
#' @param response A vector of response values
#' @param outMatrix A matrix of quantile predictions
#' @param median_outMatrix A vector of median predictions
#' @param rmse_ RMSE value for the predictions
#' @param quantile_values Pre-computed quantile values
#' @return A vector of ranks
#' @keywords internal
get_right_rank_optimized <- function(response, outMatrix, median_outMatrix, rmse_, quantile_values) {
  n <- length(response)
  ranks <- numeric(n)
  
  # Vectorized difference calculation
  diffs <- response - median_outMatrix
  extreme_threshold <- 3 * rmse_
  
  # Pre-allocate for efficiency
  extreme_negative <- abs(diffs) > extreme_threshold & diffs < 0
  extreme_positive <- abs(diffs) > extreme_threshold & diffs > 0
  
  # Process each row efficiently
  for (i in seq_len(n)) {
    pred_row <- outMatrix[i, ]
    
    # Find multiple matches efficiently
    exact_matches <- which(pred_row == response[i])
    
    if (length(exact_matches) > 1) {
      # Handle multiple matches
      match_ranks <- quantile_values[exact_matches]
      
      if (extreme_negative[i]) {
        ranks[i] <- min(match_ranks)
      } else if (extreme_positive[i]) {
        ranks[i] <- max(match_ranks)
      } else {
        ranks[i] <- mean(match_ranks)
      }
    } else if (length(exact_matches) == 1) {
      ranks[i] <- quantile_values[exact_matches]
    } else {
      # Use optimized closest search
      sorted_idx <- order(pred_row)
      sorted_preds <- pred_row[sorted_idx]
      sorted_quantiles <- quantile_values[sorted_idx]
      
      ranks[i] <- find_closest_quantile_optimized(sorted_preds, sorted_quantiles, response[i])
    }
  }
  
  return(ranks)
}

#' @title Outlier detection using quantile random forest
#' @description Detects outliers in a dataset using quantile random forests.
#' @param data A data frame.
#' @param quantiles_type Quantile grid type: 1000, 400, or 40. Default is 1000.
#' @param threshold Outlier threshold (0-1). Default is 0.025.
#' @param impute Whether to impute missing values. Default TRUE.
#' @param verbose Verbosity level. Default 1.
#' @param weight Whether to use weighted threshold. Default FALSE.
#' @param use_optimized Whether to use optimized algorithms. Default TRUE.
#' @param parallel Whether to use parallel processing. Default FALSE.
#' @param n_cores Number of cores for parallel processing. Default is detectCores()-1.
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
                   use_optimized = TRUE,
                   parallel = FALSE,
                   n_cores = NULL,
                   ...) {
  # Input checks
  if (!is.data.frame(data)) data <- as.data.frame(data)
  if (!is.numeric(threshold) || threshold < 0 || threshold > 1) stop("Threshold should be a numeric value between 0 and 1.")
  if (!(quantiles_type %in% c(1000, 400, 40))) stop("quantiles_type should be one of 1000, 400, 40")
  if (!requireNamespace("ranger", quietly = TRUE)) stop("Package 'ranger' is required.")
  if (!requireNamespace("missRanger", quietly = TRUE)) stop("Package 'missRanger' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  
  # Parallel processing setup
  if (parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      warning("Package 'parallel' not available. Using sequential processing.")
      parallel <- FALSE
    } else {
      if (is.null(n_cores)) {
        n_cores <- max(1, parallel::detectCores() - 1)
      }
      if (verbose) cat("\nUsing parallel processing with", n_cores, "cores\n")
    }
  }

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
    cat("\nOutlier identification by quantile random forests", 
        if(use_optimized) "(OPTIMIZED)" else "(LEGACY)", "\n")
    cat("\n  Variables to check:\t\t", paste(numeric_features, collapse = ", "))
    cat("\n  Variables used to check:\t", paste(names(data), collapse = ", "))
    cat("\n\n  Checking: ")
  }

  # Function to process a single variable
  process_variable <- function(v) {
    if (verbose && !parallel) cat(v, " ")
    
    covariables <- setdiff(names(data), v)
    qrf <- ranger::ranger(
      formula = stats::reformulate(covariables, response = v),
      data = data,
      quantreg = TRUE,
      ...
    )
    
    pred <- predict(qrf, data[covariables], type = "quantiles", quantiles = quantiles)
    outMatrix <- pred$predictions
    median_outMatrix <- outMatrix[, ceiling(ncol(outMatrix) / 2)]
    response <- data[[v]]
    diffs <- response - median_outMatrix
    rmse_ <- sqrt(mean(diffs^2))
    
    # Use optimized or legacy rank calculation
    if (use_optimized) {
      rank_value <- get_right_rank_optimized(response, outMatrix, median_outMatrix, rmse_, quantiles)
    } else {
      rank_value <- get_right_rank(response, outMatrix, median_outMatrix, rmse_)
    }
    
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
    
    return(list(
      outlier = outlier,
      outMatrix = outMatrix,
      oob.error = qrf$prediction.error,
      r.squared = qrf$r.squared,
      rmse = rmse_
    ))
  }

  # Process variables (parallel or sequential)
  if (parallel && length(numeric_features) > 1) {
    if (verbose) cat("\n  Processing variables in parallel...\n")
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    # Export necessary objects to cluster
    parallel::clusterExport(cl, c("data", "quantiles", "threshold_low", "threshold_high", 
                                  "weight", "use_optimized", "get_right_rank", 
                                  "get_right_rank_optimized"), envir = environment())
    parallel::clusterEvalQ(cl, {
      library(ranger)
      library(dplyr)
    })
    
    results <- parallel::parLapply(cl, numeric_features, process_variable)
    names(results) <- numeric_features
  } else {
    # Sequential processing
    results <- lapply(numeric_features, process_variable)
    names(results) <- numeric_features
  }

  # Extract results
  for (v in numeric_features) {
    outliers_list[[v]] <- results[[v]]$outlier
    outMatrices[[v]] <- results[[v]]$outMatrix
    oob.error <- c(oob.error, results[[v]]$oob.error)
    r.squared <- c(r.squared, results[[v]]$r.squared)
    rmse <- c(rmse, results[[v]]$rmse)
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
