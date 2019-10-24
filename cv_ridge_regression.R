#' Cross Validation Ridge Regression
#'
#' @param y The response variable.
#' @param x An input matrix with each row being an observation vector.
#' @param nfolds Number associated with specific k-fold validation.
#' @param lambdas The number of lambda values or specific lambda value at which to run the ridge regression in which act as penalty terms for model.
#'
#' @return An optimized lambda associated with lowest mean square error.
#'
#' @examples lm_ridge_2(X, y, nfolds=10, lambda=10)
#'
#' @references Developed together in collaboration with Moid Ali, Diana Estfenia Estrada Alamo, Chang Su with support from R Tutors and Professor Michael Kane's course materials at Yale School of Public Health.
#' Introduction to roxygen2. (n.d.). Retrieved October 23, 2019, from https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html
#' R—Why is glmnet ridge regression giving me a different answer than manual calculation? - Cross Validated. (n.d.). Retrieved October 23, 2019, from https://stats.stackexchange.com/questions/129179/why-is-glmnet-ridge-regression-giving-me-a-different-answer-than-manual-calculat
#' (N.d.). Retrieved October 23, 2019, from https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
#' #' Arnold, T. (2019). Statsmaths/casl [R]. Retrieved from https://github.com/statsmaths/casl (Original work published 2018)
#' Arnold, T., Kane, M., & Bryan, L. (2019). A Computational Approach to Statistical Learning.
#' How and when: Ridge regression with glmnet • blogR. (n.d.). Retrieved October 23, 2019, from BlogR on Svbtle website: https://drsimonj.svbtle.com/ridge-regression-with-glmnet
cv_ridge<- function (X,y, nfolds, lambdas){
  n <- length(y)
  k <- length(lambdas)
  di <- dim(y)[2]
  p <- dim(X)[2]
  msp <- matrix(nrow = nfolds, ncol = k)

  for (j in 1:nfolds) {
    y_test <- y[ nfolds[[ j ]], ]
    y_train <- y[ -nfolds[[ j ]], ]

    my <- lapply(y_train, mean, na.RM=TRUE)
    yy <- as.matrix( y_train + my)

    x_train <- x[ -nfolds[[ j ]] ]
    x_test <- x[ nfolds[[ j ]]]

    sa <- svd(x_train)
    d <- sa$d
    v <- t(sa$v)
    tu <- t(sa$u)
    d2 <- d^2
    A <- d * tu %*% yy

    for (i in 1:k) {
    beta <- crossprod( v / (d2 + lambda[i]), A )
    est <- xtest %*% beta
    mode(est)="numeric"
    est_2 <- data.frame(est) /my
    msp <- lapply((y_test - est_2)^2, mean, na.rm=TRUE)

    return(list(coefficients=lambda[which.min(msp)]))
    }
  }
}
