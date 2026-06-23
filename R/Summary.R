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
Summary <- function(model) {
  if (!any(class(model) %in% c("glm","lm","coxph", "ham", "assess"))) {stop("Error: Expecting 'assess', 'lm', 'glm', or 'coxph' class regression model." )}
  #Make a data frame
  summary_model <- summary(model)

  #Get regression model type
  if (any(class(summary_model) == "summary.lm") == TRUE) {
    reg_type <- "ols"
  }
  if (any(class(summary_model) == "summary.glm") == TRUE && summary_model$family$family == "binomial") {
    reg_type <- "logistic"
  }
  if (any(class(summary_model) == "summary.glm") == TRUE && summary_model$family$family == "poisson") {
    reg_type <- "poisson"
  }
  if (any(class(summary_model) == "summary.coxph") == TRUE) {
    reg_type <- "cox"
  }
  if (any(class(summary_model) %in% c("summary.coxph", "summary.lm", "summary.glm")) == FALSE) {
    reg_type <- "other"
  }
  #Create summary classes
  if (reg_type == "ols") {
    class(summary_model) <- c("Summary", "Summary.ols", "ham", "summary.lm")
  }
  if (reg_type == "logistic") {
    class(summary_model) <- c("Summary", "Summary.logistic", "ham", "summary.glm")
  }
  if (reg_type == "poisson") {
    class(summary_model) <- c("Summary", "Summary.poisson", "ham", "summary.glm")
  }
  if (reg_type == "cox") {
    class(summary_model) <- c("Summary", "Summary.coxph", "ham", "summary.coxph")
  }
  if (reg_type == "other") {
    class(summary_model) <- c("Summary", "Summary.other", "ham", "summary.glm")
  }
  return(summary_model)
}
