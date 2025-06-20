#' @title Evaluate Outliers
#' @description
#' This function evaluates the performance of the outlier detection algorithm.
#' @param original_data A data frame containing the original data.
#' @param anomaly_data A data frame containing the anomaly data.
#' @param anomaly_result A data frame containing the predicted anomalies (columns: row, col).
#' @return A named numeric vector containing the evaluation metrics.
#' @examples
#' anomaly_data <- generateOutliers(iris, p = 0.05, sd_factor = 5, seed = 123)
#' qrf <- outqrf(anomaly_data)
#' evaluateOutliers(iris, anomaly_data, qrf$outliers)
#' @export
evaluateOutliers <- function(original_data, anomaly_data, anomaly_result) {
  # 参数校验
  stopifnot(is.data.frame(original_data), is.data.frame(anomaly_data), is.data.frame(anomaly_result))
  
  numeric_features <- names(original_data)[sapply(original_data, is.numeric)]
  if (length(numeric_features) == 0) stop("No numeric features found in original_data")
  
  original_data <- original_data[, numeric_features, drop = FALSE]
  anomaly_data  <- anomaly_data[, numeric_features, drop = FALSE]
  
  # 计算实际异常点（被修改的单元格）
  diff <- original_data != anomaly_data
  actual_indices <- which(diff, arr.ind = TRUE)
  if (nrow(actual_indices) == 0) {
    warning("No actual outliers found.")
    return(c(
      Actual = 0, Predicted = nrow(anomaly_result), Cover = 0, Coverage = NA, Efficiency = NA
    ))
  }
  actual_df <- data.frame(row = actual_indices[, 1], col = colnames(diff)[actual_indices[, 2]])
  
  # 预测异常点
  if (!all(c("row", "col") %in% colnames(anomaly_result))) {
    # 尝试自动适配
    colnames(anomaly_result)[1:2] <- c("row", "col")
  }
  predict_df <- anomaly_result[, c("row", "col")]
  predict_df$row <- as.integer(predict_df$row)
  predict_df$col <- as.character(predict_df$col)
  
  # 交集
  intersection <- merge(predict_df, actual_df, by = c("row", "col"))
  
  n_actual <- nrow(actual_df)
  n_pred   <- nrow(predict_df)
  n_cover  <- nrow(intersection)
  coverage <- if (n_actual > 0) n_cover / n_actual else NA
  efficiency <- if (n_pred > 0) n_cover / n_pred else NA
  
  result <- c(
    Actual = n_actual,
    Predicted = n_pred,
    Cover = n_cover,
    Coverage = round(coverage, 4),
    Efficiency = round(efficiency, 4)
  )
  return(result)
}
