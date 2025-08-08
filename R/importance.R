#' Importance of variables based on partial chi-square statistic
#'
#' Calculates partial chi-square from assess class objects. The
#' importance metric is the partial chi-square minus its degrees
#' of freedom based on the regression coefficients (Harrell, 2015).
#' A higher chi-square indicates a larger effect by the predictors.
#' Therefore, the rank of the chi-square can indicate which predictors
#' are more important in explaining the variation in the outcome variable.
#'
#' @param model an assess class object or models with lm or glm class.
#'
#' @return a data.frame object with partial X^2 summary statistics.
#' @export
#'
#' @references
#' Harrell, F. E., Jr. (2016). Regression Modeling Strategies. Springer
#' International Publishing. ISBN: 978-3-319-19424-0.
#'
#' @examples
#' # OLS regression
#' importance(assess(mpg ~ hp + wt + cyl, data=mtcars, regression= "ols")$model)
#'
#' # logistic regression
#' importance(assess(vs~mpg+wt+hp, data=mtcars, regression= "logistic")$model)
#'
#' @importFrom stats pchisq
importance <- function(model) {
  if (!any(class(model) %in% c("glm","lm"))) {stop("Error: Expecting 'lm' or 'glm' class regression model." )}
  # Compute the Wald chi squared statistic for a subset of model terms.
  partx2 <- function(model, xvar) {
    #Get predictor variables
    model_terms <- function(model, xvar) {
      terms <- names(coef(model))
      terms[grepl(xvar, terms, perl = TRUE)]
    }
    quad_form <- function(V, x) {
      x %*% solve(V, x)
    }
    #Get variance-covariance matrix
    terms <- model_terms(model, xvar)
    b <- coef(model)
    V <- vcov(model)
    idx <- names(b) %in% terms
    #Calculate X2
    Chi.Sq <- quad_form(V[idx, idx], b[idx])
    DF <- sum(idx)
    #Make data frame of results
    data.frame(
      xvar, Chi.Sq, DF,
      p.value = pchisq(Chi.Sq, DF, lower.tail = FALSE)
    )
  }
  #Get variable list
  x_col_names <- names(model$coefficients)
  # Remove intercept
  xvar <- x_col_names[!x_col_names %in% c("(Intercept)", "Intercept")]
  # list of partial X2 output
  partx2_list <- list()
  # for loop partial X^2 function
  for (i in 1:length(xvar)) {
    partx2_list[[i]] <- partx2(model, xvar[i])
  }
  #Make a data frame
  partx2_df <- do.call(rbind.data.frame, partx2_list)
  colnames(partx2_df)[which(colnames(partx2_df) =="xvar")] <- "X"
  colnames(partx2_df)[which(colnames(partx2_df) =="DF")] <- "d.f."
  class(partx2_df) <- c("importance", "ham","data.frame")
  return(partx2_df)
}
