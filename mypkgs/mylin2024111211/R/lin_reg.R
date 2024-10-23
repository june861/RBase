#' Linear Regression Model
#'
#' @description train a linear model
#' @param formula describle model formula
#' @param data data build from data.frame
#' @return return a list consist of coefficiences and formula
#' @export
lin_reg <- function(formula, data) {
  m <- model.matrix(formula, data)
  y <- model.response(model.frame(formula, data))
  # Solve for coefficients using least squares
  coef <- solve(crossprod(m), crossprod(m, y))
  # Create a list that holds the model information
  model <- list(coefficients = coef,
                residuals = y - m %*% coef,
                call = match.call(),
                terms = terms(formula),
                rank = length(coef))
  class(model) <- "mylin"
  return(model)
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
