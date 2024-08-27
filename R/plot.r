#' @title Plots outqrf
#' @description
#' This function can plot paired boxplot of an "outqrf" object.
#' It helps us to better observe the relationship between the original and predicted values
#' @param qrf An object of class "outqrf".
#' @returns A ggplot2 object
#' @export
#' @examples
#' irisWithOutliers <- generateOutliers(iris, seed = 2024)
#' qrf <- outqrf(irisWithOutliers)
#' plot(qrf)
plot.outqrf<- function(qrf) {
    result_df <- data.frame()
    data <- qrf$Data
    for (i in seq_along(qrf$outMatrixs)) {
        temp_df <- as.data.frame(qrf$outMatrixs[[i]][,500])
        if (nrow(result_df) == 0) {
          result_df <- temp_df
        } else {
          result_df <- cbind(result_df, temp_df)
        }
    }
    names(result_df) = names(qrf$outMatrixs)
    result_df <- mutate(result_df,tag = "predicted")
    numeric_features <- names(data)[sapply(data,is.numeric)]
    data <- data[numeric_features]
    data <- mutate(data,tag = "observed")
    plot_in <-rbind(result_df,data)
    plot_in_longer<- plot_in|>pivot_longer(!tag,names_to ="features",values_to ="value" )
    p<- ggpubr::ggpaired(plot_in_longer, x="tag", y="value",
             fill="tag", palette = "jco",
             line.color = "grey", line.size =0.8, width = 0.4,short.panel.labs = FALSE)+
             stat_compare_means(label = "p.format", paired = TRUE)+theme(legend.position = "none")+facet_wrap(~features, scales = "free")
    return(p)
}