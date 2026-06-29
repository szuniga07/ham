#' Print Model Summary Results
#'
#' Formats 'Summary' class results to display summary statistics. These include
#' the coefficient's point estimate, lower and upper confidence intervals, and
#' the p-value. Excludes intercept coefficient as does Summary().
#'
#' @param x Summary class object of a regression model.
#' @param digits minimal number of significant digits. Defaults to 3.
#' @param ... Additional arguments.
#'
#' @return formatted Summary estimate results.
#'
#' @examples
#' # OLS regression
#' print(Summary(assess(mpg ~ wt+hp+am, data=mtcars, regression="ols")$model) )
#'
#' @importFrom methods show
#'
#' @export
#'
#' @seealso [Summary()] for the 'Summary' class object.
#'
print.Summary <- function(x, digits=NULL, ...) {
  #Stop if not a Summary class object
  if (any(class(x) == 'Summary') ==FALSE) {stop("Error: Expecting 'Summary' class object." )}

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
    cat("\n                  Model Summary")
    cat("\n=================================================\n")
    print.data.frame(model_summary)
    cat("=================================================")
    invisible(model_summary)
  }
