#' Predict from a Simple Linear Regression Model
#'
#' Predict new responses given the regression model and new input values.
#'
#' @param object An object of class "mylin".
#' @param newdata A data frame or matrix of new input values.
#' @param ... Additional arguments.
#' @return A vector of predicted values.
#' @export
predict.mylin <- function(object, newdata, ...) {
  # Extract the coefficients
  coef <- object$coefficients
  # Create the model matrix for the new data
  mm <- model.frame(object$terms, newdata)
  x <- model.matrix(object$terms, mm)
  # Compute predictions 
  predict <- x %*% coef
  predict
}