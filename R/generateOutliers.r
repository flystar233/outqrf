#' @title Adds Outliers
#' @param data data.frame.
#' @param p Proportion of outliers to add to data.
#' @param sd_factor Each outlier is generated by shifting the original value by a
#'   realization of a normal random variable with `sd_factor` times
#'   the original sample standard deviation.
#' @param seed An integer seed.
#' @returns data with some outliers.
#' @export
#' @examples
#' generateOutliers(iris, p = 0.05, sd_factor = 5, seed = 123)
generateOutliers <- function(data, p = 0.05, sd_factor = 5, seed = NULL){
    if (p <= 0 || p > 1|| sd_factor <= 0) {
        stop("p and sd_factor must be between 0 and 1")
    }
    if (is.null(seed)) {
    set.seed(123)
    } else {
    set.seed(seed)
    }
    data<- as.data.frame(data)
    numeric_features <- names(data)[sapply(data,is.numeric)]

    max_decimal_places <- function(vec) { #/
    # Function to find the maximum number of decimal places in a vector
    #
    vec_str <- as.character(vec)
    split_vec <- strsplit(vec_str, "\\.")
    decimal_places <- sapply(split_vec, function(x) ifelse(length(x) > 1, nchar(x[2]), 0))
    max_decimal_places <- max(decimal_places)
    return(max_decimal_places)
    }

    for (features in numeric_features){
        n <- length(data[,features])
        m <- round(p * n)
        round_n <- max_decimal_places(data[,features])
        data[sample(n,m),features] <- round(data[m,features] + sd_factor  * sample(c(-1, 1), m, replace = TRUE) * rnorm(m,sd(data[[features]])),round_n)
    }
    return(data)
}
