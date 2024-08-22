#' @title get numberic value from string
#' 
#' @description
#' This function extracts the numeric value from a string.
#' 
#' @param name a string
#' 
#' @return a numeric value
#' 
#' @examples
#' get_quantily_value("quantiles = 0.001")
#' @export
get_quantily_value <- function(name){
    str<- gsub("[^0-9.]", "", name)
    value <- as.numeric(str)
    return(value)
}

#' @title find the closest index
#' 
#' @description
#' This function finds the closest index to a given value in a vector.
#' 
#' @param x a vector
#' @param y a value
#' 
#' @return the index of the closest value in the vector
#' 
#' @examples
#' find_max_index(c(1, 2, 3, 4, 5), 3.5)
#' @export
find_max_index <- function(x, y) {
    index <- which(x == y)
    if (length(index) >= 1) {
        index_name <- names(x)[index]
        value<-get_quantily_value(index_name)
        return(value)
    } 
    else {
        closest_index <- which.min(abs(x - y))
        closest_index_name <- names(x)[closest_index]
        value <- get_quantily_value(closest_index_name)
        return(value)
    }
}

#' @title find outliers
#' 
#' @description
#' This function finds outliers in a dataset using quantile random forests.
#' 
#' @param data a data frame
#' @param quantiles a vector of quantiles
#' @param threshold a threshold for outlier detection
#' @param verbose a boolean value indicating whether to print verbose output
#' @param ... additional arguments passed to the ranger function
#' 
#' @return a data frame of outliers
#' 
#' @examples 
#' outqrf(iris)
#' @export
outqrf <-function(data,
                    quantiles=seq(from = 0.001, to = 0.999, by = 0.001),
                    threshold =0.025,
                    verbose = 1,
                    ...){
    data <- as.data.frame(data)
    numeric_features <- names(data)[sapply(data,is.numeric)]
    threshold_low<-threshold
    threshold_high<-1-threshold
    rmse <-c()
    outliers <- data.frame()

    if (verbose) {
    cat("\nOutlier identification by quantiles random forests\n")
    cat("\n  Variables to check:\t\t")
    cat(numeric_features, sep = ", ")
    cat("\n  Variables used to check:\t")
    cat(numeric_features, sep = ", ")
    cat("\n\n  Checking: ")
    }

    for (v in numeric_features){
        if (verbose) {
            cat(v, " ")
        }
        covariables <- setdiff(numeric_features, v)
        qrf <- ranger::ranger(
            formula = stats::reformulate(covariables, response = v),
            data = data,
            quantreg = TRUE,
            ...)
        pred <- predict(qrf, data[,covariables], type = "quantiles",quantiles=quantiles)
        outMatrix <- pred$predictions
        median_outMatrix <- outMatrix[,(length(quantiles)+1)/2]

        response<- data[,v]
        diffs = response - median_outMatrix
        rmse_ <- sqrt(sum(diffs*diffs)/(length(diffs)-1))
        rmse <- c(rmse,rmse_)
        rank_value <-c()
        median_values <-c()
        for (i in 1:length(response)){
            median_values <- c(median_values,median_outMatrix[i])
            rank_<- find_max_index(outMatrix[i,],response[i])
            if (length(rank_)>1){
                diff = response[i] -median_outMatrix[i]
                if (abs(diff)>3*rmse_ & diff<0 ){
                    min_value <- min(rank_)
                    rank_value<-c(rank_value,min_value)
                } else if (abs(diff)>3*rmse_ & diff>0) {
                    max_value <- max(rank_)
                    rank_value<-c(rank_value,max_value)
                }else {
                    mean_value <- mean(rank_)
                    rank_value<-c(rank_value,mean_value)
                }       
            }else {
                rank_value<-c(rank_value,rank_)
                }
        
        }
        outlier <- data.frame(row = row.names(data),col = v,observed = response, predicted = median_values,rank = rank_value)
        outlier<- outlier|>dplyr::filter(rank<=threshold_low| rank>=threshold_high)
        outliers <- rbind(outliers,outlier)
    }
    names(rmse) <- numeric_features
    list(
    Data = data,
    outliers = outliers,
    n_outliers = table(outliers$col),
    threshold = threshold,
    rmse = rmse
    )

}
