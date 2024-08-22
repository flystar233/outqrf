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
