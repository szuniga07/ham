#' Plot of variable importance sorted by partial chi-square statistic
#'
#' @param x importance object.
#' @param y not currently used.
#' @param ... additional arguments.
#'
#' @return plot of variable importance, significant variables highlighted in red.
#'
#' @importFrom graphics axis dotchart par
#' @export
#'
#' @examples plot(importance(assess(mpg ~ hp + wt, data=mtcars, regression= "ols")$model))
plot.importance <- function(x, y, ...) {
  object <- x
  # Plotting objects
  chi2_df <- object[, "Chi.Sq"]
  a_o <- order(chi2_df, decreasing = T)
  a_num <- chi2_df[a_o ]
  p_vals <- object[a_o, "p.value"]
  chi2_labels <- object[, "X"][a_o]

  # Create the dot chart
  graphics::par(mar=c(4.2, 2, 3.5, 3))
  graphics::par(oma = c(0, 0, 0, 3))
  graphics::dotchart(a_num, labels = chi2_labels, main = "Variable Importance", pt.cex= 2,
           col = ifelse(p_vals < .05, "red", "black"), pch = 19, xlab="X^2 - d.f.")
  # Add labels to the right margin
  graphics::axis(4, at=1:(nrow(object)), tick= FALSE, las=1, cex.axis= 1.1,
       labels= paste("p=" , sprintf("%.4f", p_vals)) )
  graphics::par(mar= c(5.1, 4.1, 4.1, 2.1))
  graphics::par(oma = c(0, 0, 0, 0))
}
