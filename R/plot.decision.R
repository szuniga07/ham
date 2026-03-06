#' Decision Curve Analysis plots and regression mode classification graphs on sensitivity and specificity
#'
#' Graph X-bar charts, p-charts, and u-charts. This includes
#' producing means center lines, 3-sigma upper and lower control limits. Users can also calculate
#' values before and after an intervention to see if a change in the control process happened. Values are
#' returned in a data frame.
#'
#' @param x decision object.
#' @param y type of plot to display. Select either 'nb', 'is', or 'cl' for a decision curve
#' analysis 'net benefit' and 'interventions saved', or a model classification (e.g., sensitivity
#' and specificity) according to a selected threshold. Net benefit and interventions saved display
#' results for specific quantiles found between the 1st and 99th percentiles. Default is 'nb'.
#' @param main the main title of the plot.
#' @param xlab a character vector label for the x-axis.
#' @param ylab a character vector label for the y-axis.
#' @param xlim specify plot's x-axis limits with a 2 element numeric vector.
#' @param ylim specify plot's y-axis limits with a 2 element numeric vector.
#' @param lwd select the line width.
#' @param bcol a multiple element character vector of length == 2  to specify the bar, band, or block colors that
#' is the shading of the true and false classification regions of the plot (e.g., true-positive and false-negative).
#' The first color represents 'true' and the second color is for 'negative'. Default is null, if none selected, the
#' colors are c('green', 'red') and 'blue' for the threshold line.
#' @param lcol a single or multiple element character vector to specify the line color(s).
#' When y='nb', select up to 3 colors in this order for model, 'all treated', and 'none treated' line colors.
#' When y='is', select 1 color for the single line. And when y='cl', select 1 color to represent the Bayesian estimates and observed values are present, the first colors are Bayesian estimates
#' while the last colors are observed values. When multiple lines are needed, single item lines
#' precede multiple use lines. For example, a single comparison value line will be assigned the first lcol
#' while both rope lines will be given the same color of the second lcol when y='post'. Defaults to 'gray'
#' if nothing selected.
#' @param add.legend add a legend by selecting the location as "bottomright", "bottom", "bottomleft",
#' "left", "topleft", "top", "topright", "right", "center". Default is no legend produced if nothing is selected.
#' @param legend a character vector of length >= 1 to appear when y='check', y='multi', and sometimes y='target'.
#' Legends to represent hierarchical estimates and observed values.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default of 1.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex.
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex.
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex.
#' @param cex.text The magnification to be used for the iname text added into the plot relative to the current setting of 1.
#' @param cex.legend The magnification to be used for the legend added into the plot relative to the current setting of 1.
#' @param ... additional arguments.
#'
#' @return plot of Shewhart control charts: X-bar charts, p-charts, and u-charts with 3-sigma control limits.
#' @importFrom graphics lines plot abline points text arrows par
#' @importFrom methods is
#' @importFrom stats lm
#' @export
#' @references
#' Vickers, A. & Elkin, E. (2006). Decision Curve Analysis: A Novel Method for Evaluating Prediction Models.
#' Society for Medical Decision Making, 26, 6, 565-574. https://doi.org/10.1177/0272989X06295361
#'
#' @examples
#' ## Hospital LOS and readmissions ##

plot.decision <- function(x, y=NULL, main=NULL, xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, lwd=NULL,
                       bcol=NULL, lcol=NULL, add.legend=NULL, legend=NULL, cex=1, cex.lab=NULL, cex.axis=NULL, cex.main=NULL,
                       cex.text=NULL, cex.legend=NULL, ...) {

  if (any(class(x) == "decision") == FALSE) {stop("Error: Expecting control class object." )}

  # set default values
  if(!is.null(lcol)) {
    lcol <- lcol
  } else {
    lcol <- c("blue", "black", "gray")
  }

  if(!is.null(bcol)) {
    bcol <- bcol
  } else {
    bcol <- c("green", "red")
  }

##############
## Graphing ##
##############
fncYhatClassPlt <- function(ClassDF, AUC, Brks=NULL, RegType, aspectRatio=NULL,
                            TBar=NULL, FBar=NULL, ThreshCol=NULL)  {
  xlimMin <- ClassDF$senspcXmin
  xlimMax <- ClassDF$senspcXmax
  #Sensitivity and 1-specificity
  Sens.Value <- switch(RegType,
                       "ols"   = round(ClassDF$propAbovMY1, 3),
                       "logistic" = round(ClassDF$propAbovMY1, 3),
                       "Proportion Y Logistic" = round(ClassDF$propAbovMY1, 3),
                       "Ordinal Logistic"  = round(ClassDF$propAbovMY1, 3),
                       "Poisson"  = round(ClassDF$propAbovMY1, 3),
                       "Quantile" = round(ClassDF$propAbovMY1, 3),
                       "Cox PH"   = round(ClassDF$propAbovMY1, 3),
                       "Cox PH with censoring"  = round(ClassDF$propAbovMY1, 3),
                       "AFT"  = round(ClassDF$fls_Neg, 3) ,
                       "AFT with censoring"     = round(ClassDF$fls_Neg, 3),
                       "Generalized Least Squares" = round(ClassDF$propAbovMY1, 3) )
  Spec.Value <- switch(RegType,
                       "ols"   = round(ClassDF$propAbovMY0, 3),
                       "logistic" = round(ClassDF$propAbovMY0, 3),
                       "Proportion Y Logistic" = round(ClassDF$propAbovMY0, 3),
                       "Ordinal Logistic"  = round(ClassDF$propAbovMY0, 3),
                       "Poisson"  = round(ClassDF$propAbovMY0, 3),
                       "Quantile" = round(ClassDF$propAbovMY0, 3),
                       "Cox PH"   = round(ClassDF$propAbovMY0, 3),
                       "Cox PH with censoring"  = round(ClassDF$propAbovMY0, 3),
                       "AFT"  = round(ClassDF$specifity, 3),
                       "AFT with censoring"     = round(ClassDF$specifity, 3),
                       "Generalized Least Squares" = round(ClassDF$propAbovMY0, 3) )
  #Bar colors
  Bar.Colors1 <- switch(RegType,
                        "ols"   = c(FBar, TBar),
                        "logistic" = c(FBar, TBar),
                        "Proportion Y Logistic" = c(FBar, TBar),
                        "Ordinal Logistic"  = c(FBar, TBar),
                        "Poisson"  = c(FBar, TBar),
                        "Quantile" = c(FBar, TBar),
                        "Cox PH"   = c(FBar, TBar),
                        "Cox PH with censoring"  = c(FBar, TBar),
                        "AFT"  = c(TBar, FBar),
                        "AFT with censoring"     = c(TBar, FBar),
                        "Generalized Least Squares" = c(FBar, TBar) )
  Bar.Colors2 <- switch(RegType,
                        "ols"   = c(TBar, FBar),
                        "logistic" = c(TBar, FBar),
                        "Proportion Y Logistic" = c(TBar, FBar),
                        "Ordinal Logistic"  = c(TBar, FBar),
                        "Poisson"  = c(TBar, FBar),
                        "Quantile" = c(TBar, FBar),
                        "Cox PH"   = c(TBar, FBar),
                        "Cox PH with censoring"  = c(TBar, FBar),
                        "AFT"  = c(FBar, TBar),
                        "AFT with censoring"     = c(FBar, TBar),
                        "Generalized Least Squares" = c(TBar, FBar) )
  #AUC value
  AUC_val <- switch(RegType,
                    "ols"   = round(AUC, 3),
                    "logistic" = round(AUC, 3),
                    "Proportion Y Logistic" = round(AUC, 3),
                    "Ordinal Logistic"  = round(AUC, 3),
                    "Poisson"  = round(AUC, 3),
                    "Quantile" = round(AUC, 3),
                    "Cox PH"   = round(AUC, 3),
                    "Cox PH with censoring"  = round(AUC, 3),
                    "AFT"  = round(1-AUC, 3),
                    "AFT with censoring"     = round(1-AUC, 3),
                    "Generalized Least Squares" = round(AUC, 3) )

  # 2. Density
  den1 <- density(ClassDF$pm1, kernel = c("gaussian"), bw = "SJ", na.rm=TRUE)
  den2 <- density(ClassDF$pm2, kernel = c("gaussian"), bw = "SJ", na.rm=TRUE)
  max_den <- max(den2$y)
  den2$y <- den2$y - max_den
  ## Create graph ##
  plot(c(den1$x, den2$x), c(den1$y, den2$y), type="n")
  lines(den1$x, den1$y)
  lines(den2$x, den2$y)
  abline(h=0)

  # 4. Fill the first half (below threshold)
  #   a. Identify x-values below the median
  x.left1 <- den1$x[den1$x <= ClassDF$threshLev]
  x.left2 <- den2$x[den2$x > ClassDF$threshLev]
  #   b. Identify corresponding y-values
  y.left1 <- den1$y[den1$x <= ClassDF$threshLev]
  y.left2 <- den2$y[den2$x > ClassDF$threshLev]
  #   c. Create the polygon points: add the median x, 0 to close the shape
  polygon(c(x.left1, ClassDF$threshLev), c(y.left1, 0), col = "tomato", border = NA)
  polygon(c(x.left2, ClassDF$threshLev), c(y.left2, 0- max_den), col = "tomato", border = NA)
  # 5. Fill the right side (above the threshold)
  #   a. Identify x-values above the threshold
  x.right1 <- den1$x[den1$x >= ClassDF$threshLev]
  x.right2 <- den2$x[den2$x < ClassDF$threshLev]
  #   b. Identify corresponding y-values
  y.right1 <- den1$y[den1$x >= ClassDF$threshLev]
  y.right2 <- den2$y[den2$x < ClassDF$threshLev]
  #   c. Create the polygon space
  polygon(c(x.right1, ClassDF$threshLev), c(y.right1, 0), col = "springgreen", border = NA)
  polygon(c(x.right2, ClassDF$threshLev), c(y.right2, 0- max_den), col = "springgreen", border = NA)
  # 6. Redraw the density line over the filled areas for clarity
  lines(den1, lwd = 2)
  lines(den2, lwd = 2)
  # 7. Add a vertical line at the median for the threshold
  abline(v = ClassDF$threshLev, col = "darkgray", lty = 2, lwd = 2)
  # 8. Add a legend
  legend("topright", c("Below Median", "Above Median"), fill = c("tomato", "springgreen"),
         border = NA, bty="n", inset=c(0, .05))
}

#####################################
## Function to plot decision curve ##
#####################################
fncDcsnCrvPlt <- function(ThreshQntl, CType, xlim1=NULL, xlim2=NULL, ylim1=NULL, ylim2=NULL,
                          Legend.Loc=NULL, LCol=NULL, LSize=NULL) {
  if(CType == "nb") {
    plot(ThreshQntl$Threshold.Level, ThreshQntl$Net.Benefit,type="n", xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2),
         main="Decision curve plot of net benefit by threshold levels", xlab="Threshold", ylab="Net Benefit",
         cex.main=2, cex.lab=2)
    lines(ThreshQntl$Threshold.Level, ThreshQntl$Net.Benefit,cex=2, lwd=LSize, lty=1, col=LCol[1])
    lines(ThreshQntl$Threshold.Level, ThreshQntl$All.Treated,cex=2, lwd=LSize, lty=2, col=LCol[2])
    abline(h=0, col=LCol[3], lwd=LSize)
    legend(x=Legend.Loc, legend=c("Model", "All treated", "None treated"),
           col=LCol, lty=c(1,2,1),
           lwd=1, cex=1, bty="n", inset=c(0, .05))
  }
  if(CType == "is") {
    par(mar= c(5.1, 4.6, 4.1, 1.6))
    plot(ThreshQntl$Threshold.Level, ThreshQntl$Interventions.Saved,
         lwd=LSize, type="l", col=LCol[1], axes=F, xlim=c(xlim1,xlim2), ylim=c(ylim1,ylim2),
         main="Decision curve plot of interventions avoided by threshold levels", xlab="Threshold",
         cex.main=2, cex.lab=2,
         ylab="Interventions avoided per 100 persons")
    axis(1)
    axis(2, at= seq(0,1, .1), labels= seq(0,1, .1)*100)
    box()
  }
}

###############
## Run plots ##
###############

# Decision curve analysis #
# net benefit
if(y=="nb") {
  fncDcsnCrvPlt(ThreshQntl=x$DCA, CType="nb", Legend.Loc="topleft", LCol= lcol)
}
# interventions saved
if(y=="is") {
  fncDcsnCrvPlt(ThreshQntl=x$DCA, CType="is", Legend.Loc="topleft", LCol= lcol)
}

# Classification sensitivity and specificity
if(y=="cl") {
fncYhatClassPlt(ClassDF=x$Classification, AUC=x$AUC, RegType= x$type,
                TBar=bcol[1], FBar=bcol[2], ThreshCol=lcol[1])
}

}





