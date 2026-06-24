#' Creates a Summary object from the standard summary object, for graphing purposes
#'
#' Produces a Summary object for regression models based on summary(model). The purpose
#' is to later pass the object to the plot function to produce a summary of the
#' predictor variable's coefficients. The main difference between Summary() and summary()
#' is that the ham function adds new classes helpful for creating the plot and Summary()
#' has a capital 'S'.
#'
#' @param model an assess class object or models with 'lm', 'glm', or 'coxph' class.
#' This includes the differences-in-differences and interrupted time series models.
#' For assess objects, make sure to select the correct element, for example,
#' model= my_regression$model for ordinary least squares, logistic, or Poisson models.
#' For 'ITS' or 'DID' models, use model= its_model$ITS or model= did_model$DID. If
#' using lm(), glm() or even coxph(), the model object name is acceptable (e.g.,
#' model= my_glm_model).
#'
#' @return an object with summary statistics and additional classes.
#' @export
#'
#' @seealso [plot.Summary()] for a plot of the 'Summary' class object.
#'
#' @examples
#' # OLS regression, identical output as 'lower case' summary(model)
#' Summary(assess(mpg ~ hp + wt + cyl, data=mtcars, regression= "ols")$model)
#'
#' #Creates new classes useful for to plot the summary, e.g., plot(Summary(model))
#' class(Summary(assess(mpg ~ hp + wt + cyl, data=mtcars, regression= "ols")$model))
#'
#' @importFrom stats lm glm
Summary <- function(model) {
  if (!any(class(model) %in% c("glm","lm","coxph", "assess"))) {stop("Error: Expecting 'assess', 'lm', 'glm', or 'coxph' class regression model." )}

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
