#' Linear Regression Model
#'
#' @description train a linear model
#' @param model the model you want to display
#' @return return a list consist of coefficiences and formula
#' @export
print.lm <- function(model) {
  cat("Coefficients:\n")
  print(model$coefficients)
}