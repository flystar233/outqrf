X <- data.frame(a = 1:100, b = 1:100)
X[1L, "a"] <- 1000
X_NA <- X
X_NA[2L, "a"] <- NA

test_that("outqrf(iris) Generate results as expected", {
    qrf <- outqrf(iris)
    expect_true(nrow(qrf$outliers)>0)
})

test_that("outqrf(iris,quantiles_type=400) Generate results as expected", {
    qrf <- outqrf(iris)
    expect_true(nrow(qrf$outliers)>0)
})

test_that("outqrf(iris,quantiles_type=400,verbose=0) silent", {
    expect_silent(qrf<- outqrf(iris,quantiles_type=400,verbose=0))
})

test_that("outqrf(X_NA,quantiles_type=400,impute=TRUE) impute normally", {
    qrf <- outqrf(X_NA,quantiles_type=400,impute=TRUE)
    expect_true(nrow(qrf$outliers)>0)
})


