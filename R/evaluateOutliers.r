#' @title Evaluate Outliers
#' @description
#' This function evaluates the performance of the outlier detection algorithm.
#' 
#' @param original_data A data frame containing the original data.
#' @param anomaly_data A data frame containing the anomaly data.
#' @param anomaly_result A data frame containing the predicted anomalies.
#' @return A data frame containing the evaluation metrics.
#' @examples 
#' evaluateOutliers(original_data,anomaly_data,qrf$outliers)
#' @export
evaluateOutliers<- function(original_data, anomaly_data, anomaly_result){
    numeric_features <- names(original_data)[sapply(original_data,is.numeric)]
    original_data <- original_data[,numeric_features]
    anomaly_data <- anomaly_data[,numeric_features]
    diff <- original_data - anomaly_data
    actual_indices <- as.data.frame(which(diff != 0, arr.ind=TRUE))
    actual_indices$col <- colnames(diff)[actual_indices$col]
    predict_indices<- anomaly_result[,1:2] 

    intersection <- merge(predict_indices,actual_indices)
    Coverage <- nrow(intersection)/nrow(actual_indices)
    Efficiency <- nrow(intersection)/nrow(predict_indices)
    result <- c('Coverage'=nrow(intersection)/nrow(actual_indices),'Efficiency'=nrow(intersection)/nrow(predict_indices))
    return(result)

}