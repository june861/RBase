#' Predict via Linear Regression Model
#'
#' @description train a linear model
#' @param model describle model formula
#' @param newdata build from data.frame
#' @return return a list consist of coefficiences and formula
#' @export
predict <- function(model, newdata) {
  x_new <- cbind(1, newdata)
  return(x_new %*% model$coefficients)
}