#' Plot of variable importance ranked by partial chi-square statistic
#'
#' Plots an importance class object. Produces a dot chart that places the predictor
#' variable with the highest partial chi-square (Wald chi-square for individual
#' coefficients) at the bottom. It is a metric of the partial chi-square
#' minus its degrees of freedom (Harrell, 2015). Predictor variables
#' with significant p-values at the 0.05 alpha are highlighted red. Consider
#' graphical parameters of mar=c(4.2, 2, 3.5, 3) and oma = c(0, 0, 0, 3).
#'
#' @param x importance object.
#' @param y not currently used.
#' @param ... additional arguments.
#'
#' @return plot of variable importance, significant variables highlighted in red.
#'
#' @importFrom graphics axis dotchart
#' @export
#' @references
#' Harrell, F. E., Jr. (2016). Regression Modeling Strategies. Springer
#' International Publishing. ISBN: 978-3-319-19424-0.
#'
#' @examples
#' # OLS regression
#' plot(importance(assess(mpg ~ hp + wt + cyl, data=mtcars, regression= "ols")$model))
#'
#' # logistic regression
#' plot(importance(assess(vs~mpg+wt+hp, data=mtcars, regression= "logistic")$model))
plot.importance <- function(x, y, ...) {
  object <- x
  # Plotting objects
  chi2_df <- object[, "Chi.Sq"]
  a_o <- order(chi2_df, decreasing = T)
  a_num <- chi2_df[a_o ]
  p_vals <- object[a_o, "p.value"]
  chi2_labels <- object[, "X"][a_o]

  # Create the dot chart
  graphics::dotchart(a_num, labels = chi2_labels, main = "Variable Importance", pt.cex= 2,
           col = ifelse(p_vals < .05, "red", "black"), pch = 19, xlab="X^2 - d.f.")
  # Add labels to the right margin
  graphics::axis(4, at=1:(nrow(object)), tick= FALSE, las=1, cex.axis= 1.1,
       labels= paste("p=" , sprintf("%.4f", p_vals)) )
}
