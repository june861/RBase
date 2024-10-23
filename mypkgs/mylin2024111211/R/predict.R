#' Predict via Linear Regression Model
#'
#' @description train a linear model
#' @param model describle model formula
#' @param newdata build from data.frame
#' @return return a list consist of coefficiences and formula
#' @export
predict <- function(model, newdata) {
  x_new <- cbind(1, newdata)
  # conver x_new to numerical
  x_new_mat <- as.matrix(x_new)
  return(x_new_mat %*% model$coefficients)
}