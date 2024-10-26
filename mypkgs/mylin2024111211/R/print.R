#' Print a Simple Linear Regression Model Summary
#'
#' Prints a summary of a simple linear regression model.
#'
#' @param x An object of class "mylin".
#' @param ... Additional arguments.
#' @export
print.mylin <- function(x, ...) {
  cat("\nCall:\n", deparse(x$call), "\n\n")
  cat("Coefficients:\n")
  print.default(format(x$coefficients), print.gap = 2L, quote = FALSE)
  cat("\n")
  invisible(x)
}