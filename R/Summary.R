#' Importance of variables based on partial chi-square statistic
#'
#' Calculates partial chi-square (Wald chi-square for individual
#' coefficients) from assess class objects. The
#' importance is the partial chi-square minus its degrees
#' of freedom based on the regression coefficients (Harrell, 2015).
#' A higher chi-square indicates a larger effect by the predictors.
#' Therefore, the rank of the chi-square can indicate which predictors
#' can contribute more in explaining the variation in the outcome variable.
#'
#' @param model an assess class object or models with lm or glm class.
#'
#' @return an object with summary statistics.
#' @export
#'
#'
#' @examples
#' # OLS regression
#' summary(assess(mpg ~ hp + wt + cyl, data=mtcars, regression= "ols")$model)
#'
#' @importFrom stats lm glm
summary <- function(model) {
  if (!any(class(model) %in% c("glm","lm","coxph"))) {stop("Error: Expecting 'lm', 'glm', or 'coxph' class regression model." )}
  #Make a data frame
  summary_model <- summary(model)
  class(summary_model) <- c("summary", "ham", "summary.coxph", "summary.lm", "summary.glm")
  return(summary_model)
}
