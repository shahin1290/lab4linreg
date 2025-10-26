#' Ridge regression
#'
#' Performs ridge regression using a formula interface.See
#' \url{http://math.stackexchange.com/questions/299481/qr-factorization-for-ridge-regression}
#' for details.
#'
#' @param formula formula Linear regression formula.
#' @param data data.frame Input data.
#' @param lambda numeric A non-negative regularization parameter
#'
#' @returns ridgereg object containing the results.
#' @importFrom methods new
#' @importFrom stats model.frame model.matrix model.response terms delete.response
#' @export
#'
#' @examples
#' data(iris)
#' ridge_model <- ridgereg(Sepal.Length ~ Petal.Length + Petal.Width, data = iris, lambda = 0.5)
#' print(ridge_model)
ridgereg <- function(formula, data, lambda) {

  # Create the design matrix and response vector from the formula
  mf <- model.frame(formula = formula, data = data)
  X <- model.matrix(attr(mf, "terms"), mf)
  y <- model.response(mf)

  # Perform ridge regression using QR decomposition
  p <- ncol(X)
  X_aug <- rbind(X, sqrt(lambda) * diag(p))
  y_aug <- c(y, rep(0, p))

  qr_decomp <- qr(X_aug)
  beta <- backsolve(qr.R(qr_decomp), crossprod(qr.Q(qr_decomp), y_aug))

  # Assign names to the coefficient vector
  names(beta) <- colnames(X)

  # Construct the ridgereg object
  result <- list(
    call = match.call(),
    formula = formula,
    coefficients = beta,
    lambda = lambda
  )

  # Set the class of the returned object
  class(result) <- "ridgereg"

  return(result)
}

#' @exportS3Method
print.ridgereg <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}

#' Generates predictions from a fitted ridgereg model.
#'
#' @param object A fitted object of class 'ridgereg'.
#' @param newdata A data frame for prediction. If missing, fitted values are returned.
#'
#' @exportS3Method
predict.ridgereg <- function(object, newdata) {

  # If newdata is missing, return the fitted values on the original data
  if (missing(newdata)) {
    # Re-evaluate the data used for the model fit
    original_data <- eval(object$call$data)
    # Recreate the original design matrix
    X <- model.matrix(object$formula, original_data)
  } else {
    # If newdata is provided:
    # 1. Get the terms from the original formula
    full_terms <- terms(object$formula)

    # 2. Remove the response variable to get only predictor terms
    predictor_terms <- delete.response(full_terms)

    # 3. Build the design matrix from newdata using only predictor terms
    # This prevents the function from searching for the response variable
    X <- model.matrix(predictor_terms, data = newdata)
  }

  # Calculate the predicted values using the stored coefficients
  y_hat <- X %*% object$coefficients

  return(as.vector(y_hat))
}

#' @exportS3Method
coef.ridgereg <- function(object, ...) {
  return(object$coefficients)
}
