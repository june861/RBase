#' Linear Regression Model
#'
#' @description train a linear model
#' @param formula describle model formula
#' @param data data build from data.frame
#' @return return a list consist of coefficiences and formula
#' @export
lin_reg <- function(formula, data) {
  x <- model.matrix(formula, data)[, -1]
  y <- data[[as.character(formula[[2]])]]
  coef <- solve(t(x) %*% x) %*% t(x) %*% y
  list(coefficients = coef, formula = formula)
}

#' Linear Regression Model
#'
#' @description save model
#' @param mode the linear model you want to save
#' @param filename the save model file name
#' @return none
#' @export
save_model <- function(model, filename) {
  save(model, file = filename)
}
