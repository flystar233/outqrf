# {outqrf}
## Overview
**outqrf** is an R package used for outlier detection. Each numeric variable is regressed onto all other variables using a quantile random forest (qrf).
Next, we will compute the rank of the observed values in the predicted results' quantiles.If the rank of the observed value exceeds the threshold, 
the observed value is considered an outlier.

Since the same predicted value might be distributed across multiple quantiles in the predicted quantile results, 
this affects our location finding for the observed value. Therefore, we also used a method similar to the [outForest](https://github.com/mayer79/outForest) package to compare the observed value 
with the 50% quantile value again to determine the final quantile result.

## Installation
```r
# Development version
devtools::install_github("flystar233/outqrf")
```

## Usage
We first generate a data set with about 5% outliers values in each numeric column.
```
library(outqrf)
#Generate data with outliers in numeric columns
irisWithOutliers <- generateOutliers(iris, p = 0.05,seed =2024)
# Find outliers by quantile random forest regressions
out <- outqrf(irisWithOutliers)
out$outliers
#    row          col    observed predicted  rank
# 1   32 Sepal.Length  14.9308229       5.4 0.999
# 2   35 Sepal.Length  -1.8135664       4.6 0.001
# 3   84 Sepal.Length  11.4849203       6.3 0.999
# 4  129 Sepal.Length  -5.6021049       6.2 0.001
# 5   49  Sepal.Width  10.7927619       3.7 0.999
# 6   90  Sepal.Width  -0.7648333       2.4 0.001
# 7  131  Sepal.Width  -2.1389311       2.7 0.001
# 8  137  Sepal.Width  11.4992802       3.2 0.999
# 9   36 Petal.Length  12.8033669       1.6 0.999
# 10  73 Petal.Length -17.1905846       4.4 0.001
# 11 107 Petal.Length  13.6672827       5.6 0.999
# 12 123 Petal.Length  -8.9717894       5.1 0.001
# 13 140 Petal.Length  13.5214560       5.7 0.999
# 14  10  Petal.Width -11.8406790       0.2 0.001
# 15  14  Petal.Width  -6.3030372       0.2 0.003
# 16  34  Petal.Width   7.5843853       0.4 0.999
# 17  66  Petal.Width   6.9828746       2.0 0.993
# 18 113  Petal.Width  -6.0696862       1.5 0.001


```
