# {outqrf}
## Overview
**outqrf** is an R package used for outlier detection. Each numeric variable is regressed onto all other variables using a quantile random forest (QRF).
We use [ranger](https://github.com/imbs-hl/ranger) to perform the fitting and prediction of quantile regression forests (QRF).
Next, we will compute the rank of the observed values in the predicted results' quantiles. If the rank of the observed value exceeds the threshold, 
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
#Generate data with outliers in numeric columns
irisWithOutliers <- generateOutliers(iris, p = 0.05,seed =2024)
# Find outliers by quantile random forest regressions
out <- outqrf(irisWithOutliers,quantiles_type=400)
out$outliers
 row          col observed predicted        rank
1   37 Sepal.Length      9.8      5.10 1.000000000
2   45 Sepal.Length      7.4      5.10 1.000000000
3  105 Sepal.Length      4.2      6.80 0.002506266
4  109 Sepal.Length     -1.7      6.40 0.000000000
5  122 Sepal.Length     13.6      6.40 1.000000000
6  131 Sepal.Length      7.4      5.80 0.987468672
7  145 Sepal.Length     10.4      6.70 1.000000000
8   20  Sepal.Width      4.5      3.50 1.000000000
9   42  Sepal.Width      2.3      3.20 0.000000000
10  87  Sepal.Width      2.0      2.95 0.000000000
11 106  Sepal.Width      5.6      3.00 1.000000000
12 116  Sepal.Width      6.3      3.00 1.000000000
13   5 Petal.Length     14.8      1.50 1.000000000
14  49 Petal.Length     -2.4      1.50 0.000000000
15  73 Petal.Length     -7.0      4.60 0.000000000
16  98 Petal.Length      2.7      4.40 0.002506266
17 108 Petal.Length     14.5      5.30 1.000000000
18 131 Petal.Length      3.4      6.40 0.000000000
19 134 Petal.Length     12.6      5.10 0.989974937
20  16  Petal.Width      1.9      0.20 1.000000000
21  27  Petal.Width      2.8      0.20 1.000000000
22  31  Petal.Width     -9.5      0.20 0.000000000
23  59  Petal.Width      0.8      1.50 0.000000000
24  66  Petal.Width      6.5      1.40 1.000000000
25 101  Petal.Width      2.5      1.80 0.989974937
26 105  Petal.Width      3.2      1.80 1.000000000
27 113  Petal.Width      0.9      2.10 0.000000000
28 130  Petal.Width      1.6      2.10 0.002506266
29 134  Petal.Width      1.5      2.00 0.017543860
30 135  Petal.Width      1.4      2.30 0.012531328

```

## Evaluation on iris (Small Dataset)
First, let's simply detect outliers in the data using a box plot.
```
# find outliers use boxplot
# 32
irisWithOutliers <- outqrf::generateOutliers(iris, p = 0.05,seed =2024)
boxplot_num = 0
for (i in names(irisWithOutliers)[sapply(irisWithOutliers,is.numeric)]){
  q1 <- quantile(irisWithOutliers[,i], 0.25)
  q3 <- quantile(irisWithOutliers[,i], 0.75)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  num <- sum(irisWithOutliers[,i]<lower_bound|irisWithOutliers[,i]>upper_bound)
  boxplot_num<-boxplot_num+num
}
boxplot_num
# 28
```
![](https://github.com/user-attachments/assets/0a453eb9-3901-4c46-a4f4-ee86c386a701)

Then, use outqtf and outForest respectively to detect outliers.
```
qrf <- outqrf(irisWithOutliers,quantiles_type=400)
rf <- outForest(irisWithOutliers)

evaluateOutliers(iris,irisWithOutliers,qrf$outliers)
#Actual  Predicted      Cover   Coverage Efficiency 
# 32.00      30.00      24.00       0.75       0.8 
evaluateOutliers(iris,irisWithOutliers,rf$outliers)
#Actual  Predicted      Cover   Coverage Efficiency
# 32.00      19.00      19.00       0.59       1.00 
```
We can even display the original values and outliers of the data using a paired box plot.
```
plot(qrf)
```
![](https://github.com/user-attachments/assets/073f4e4d-3c80-459a-af50-40988d769899)

## Evaluation on diamonds (Big Dataset)
```
data <- diamonds|>select(price,carat,cut,color,clarity)
data2 <- outqrf::generateOutliers(data, p = 0.001,seed =2024)
qrf <- outqrf(data2,num.threads=8,quantiles_type=400)
# The process can be slow because it needs to predict the value at 400|1000 quantiles for each observation. 
rf <- outForest(data2)
evaluateOutliers(data,data2,qrf$outliers)
#Actual  Predicted      Cover   Coverage Efficiency 
#108.00     336.00     103.00       0.95       0.31 
evaluateOutliers(data,data2,rf$outliers)
#Actual  Predicted      Cover   Coverage Efficiency 
#108.00     687.00     104.00       0.96       0.15
```

