library(testthat)
usethis::use_testthat()
library(glmnet)

context("Test the output of homework 2")

test_that ("Your function passes a test with iris data.", {
  X <- as.matrix(iris[,-c(3,5)])
  Y <- iris[[3]]
  n <- nrow(X)

  sd_y <- sqrt(var(Y)*(n-1)/n)

  fit_glmnet <- glmnet(X,Y, alpha=0, standardize = FALSE, intercept = FALSE, thresh = 1e-20)
  beta <- as.vector(coef(fit_glmnet, s = sd_y*10/n, exact = TRUE,x=X,y=Y))[-1]

  fit_ridge_regression <- lm_ridge(X = X, y = Y, lambdas= 10)

  expect_equivalent(beta, fit_ridge_regression$coefficients,
                    tolerance = 1e-5)

})
test_that("Your function passes a test with a randomly generated data frame.",{
  set.seed(123)
  n    <- 1000
  p   <-  100

  X   <- matrix(rnorm(n*p,0,1),n,p)
  beta <- rnorm(p,0,1)
  Y    <- X%*%beta+rnorm(n,0,0.5)

  sd_y <- sqrt(var(Y)*(n-1)/n)

  fit_glmnet <- glmnet(X,Y, alpha=0, standardize = FALSE, intercept = FALSE, thresh = 1e-20)
  beta2 <- as.vector(coef(fit_glmnet, s = sd_y*10/n, exact = TRUE,x=X,y=Y))[-1]

  fit_ridge_regression <- lm_ridge(X = X, y = Y, lambdas= 10)

  expect_equivalent(beta2, fit_ridge_regression$coefficients,
                    tolerance = 1e-5)
})
test_that ("Your cross validation function passes a test with iris data.", {
  y <- as.numeric(iris[,5])
  X <- iris[y!=1, 1:4]
  y <- y[y!=1]-2

  n_sample = NROW(X)

  w = .6
  X_train = X[0:(w * n_sample),]
  y_train = y[0:(w * n_sample)]
  X_test = X[((w * n_sample)+1):n_sample,]
  y_test = y[((w * n_sample)+1):n_sample]

  set.seed(0)
  fit_model_lambda <- cv.glmnet(as.matrix(X_train), as.factor(y_train), nfolds = 10, alpha=0, family="binomial", type.measure="class")
  best_lambda  <- fit_model_lambda$lambda.1se

  lambdaval=20
  test_model_lambda<- cv_ridge(X, y, nfolds=10, lambdas=lambdaval)

  expect_equivalent(best_lambda, test_model_lambda$coefficients,
                    tolerance = 1e-5)
})
