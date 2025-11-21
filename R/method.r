#' @title Plots outqrf
#' @description
#' This function can plot paired boxplot of an "outqrf" object.
#' It helps us to better observe the relationship between the original and predicted values
#' @param x An object of class "outqrf".
#' @param data Original data frame used for outlier detection. Required.
#' @param ... Other parameters passed to ggplot2/ggpubr functions.
#' @returns A ggplot2 object
#' @export
#' @examples
#' irisWithOutliers <- generateOutliers(iris, seed = 2024)
#' qrf <- outqrf(irisWithOutliers)
#' plot(qrf, data = irisWithOutliers)
plot.outqrf <- function(x, data, ...) {
  # Input validation
  if (missing(data) || is.null(data)) {
    stop("Please provide the original data frame used for outlier detection.")
  }
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  # Extract numeric features
  numeric_features <- names(data)[vapply(data, is.numeric, logical(1))]
  if (length(numeric_features) == 0) {
    stop("No numeric features found in data.")
  }
  
  # Check if features match
  if (!all(names(x$outMatrices) %in% numeric_features)) {
    warning("Some features in outMatrices are not found in data. Only matching features will be plotted.")
    numeric_features <- intersect(numeric_features, names(x$outMatrices))
  }
  
  # Prepare predicted values
  predicted_df <- as.data.frame(x$outMatrices[numeric_features])
  predicted_df$tag <- "predicted"
  
  # Prepare observed values
  observed_df <- data[, numeric_features, drop = FALSE]
  observed_df$tag <- "observed"
  
  # Combine and reshape data
  plot_data <- rbind(predicted_df, observed_df)
  plot_data_long <- tidyr::pivot_longer(
    plot_data,
    cols = -tag,
    names_to = "features",
    values_to = "value"
  )
  
  # Create plot
  p <- ggpubr::ggpaired(
    plot_data_long,
    x = "tag",
    y = "value",
    fill = "tag",
    palette = "jco",
    line.color = "grey",
    line.size = 0.8,
    width = 0.4,
    short.panel.labs = FALSE
  ) +
    ggpubr::stat_compare_means(
      label = "p.format",
      paired = TRUE
    ) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::facet_wrap(~features, scales = "free")
  
  return(p)
}
