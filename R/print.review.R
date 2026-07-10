#' Print Model Review Results
#'
#' Formats 'review' class results to display summary statistics. These include
#' the coefficient's point estimate, lower and upper confidence intervals, and
#' the p-value. Excludes intercept coefficient as does review().
#'
#' @param x review class object of a regression model.
#' @param digits minimal number of significant digits. Defaults to 3.
#' @param ... Additional arguments.
#'
#' @return formatted review results in a table.
#'
#' @examples
#' # OLS regression
#' print(review(assess(mpg ~ wt+hp+am, data=mtcars, regression="ols")$model) )
#'
#' @importFrom methods show
#'
#' @export
#'
#' @seealso [review()] for the 'review' class object.
#'
print.review <- function(x, digits=NULL, ...) {
  #Stop if not a review class object
  if (any(class(x) == 'review') ==FALSE) {stop("Error: Expecting 'review' class object." )}

  #Get just the estimates
  if(!is.null(digits)) {
    dig_val <-  digits
  } else {
    dig_val <-  3
  }
  #Get model estimates
  model_summary <- signif(x$Estimates, dig_val)

  ## Print function ##
    cat("=================================================")
    cat("\n                  Model Review")
    cat("\n=================================================\n")
    print.data.frame(model_summary)
    cat("=================================================")
    invisible(model_summary)
  }
