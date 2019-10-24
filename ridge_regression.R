#' Ridge Regression
#'
#' @param X An input matrix with each row being an observation vector.
#' @param y The response variable.
#' @param lambdas The number of lambda values or specific lambda value at which to run the ridge regression in which act as penalty terms for model.
#'
#' @return A ridge beta coefficient corresponding to ridge regression testing.
#'
#' @examples linear_model(X, y, lambdas=10)
#'
#' @author Tyler D. Harvey
#'
#' @references Developed together in collaboration with Moid Ali, Diana Estfenia Estrada Alamo, Chang Su with support from R Tutors and Professor Michael Kane's course materials at Yale School of Public Health.
#' Introduction to roxygen2. (n.d.). Retrieved October 23, 2019, from https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html
#' R—Why is glmnet ridge regression giving me a different answer than manual calculation? - Cross Validated. (n.d.). Retrieved October 23, 2019, from https://stats.stackexchange.com/questions/129179/why-is-glmnet-ridge-regression-giving-me-a-different-answer-than-manual-calculat
#' (N.d.). Retrieved October 23, 2019, from https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
#' #' Arnold, T. (2019). Statsmaths/casl [R]. Retrieved from https://github.com/statsmaths/casl (Original work published 2018)
#' Arnold, T., Kane, M., & Bryan, L. (2019). A Computational Approach to Statistical Learning.
#' How and when: Ridge regression with glmnet • blogR. (n.d.). Retrieved October 23, 2019, from BlogR on Svbtle website: https://drsimonj.svbtle.com/ridge-regression-with-glmnet
lm_ridge<-
  function(X, y, lambdas)
  {
    svd_obj <- svd(X)
    U <- svd_obj$u
    V <- svd_obj$v
    svals <- svd_obj$d
    k <- length(lambdas)

    ridge_beta <- matrix(NA_real_, nrow = k, ncol = ncol(X))
    for (j in seq_len(k))
    {
      D <- diag(svals / (svals^2 + lambdas[j]))
      ridge_beta[j,] <- V %*% D %*% t(U) %*% y
    }
    return(list(coefficients=ridge_beta))
  }
