#' Decision Curve Analysis plots and regression model classification graphs on sensitivity and specificity
#'
#' Graph decide class object's model classification results as well as Decision Curve analysis output for
#' net benefit and interventions saved.
#'
#' @param x decide object.
#' @param y type of plot to display. Select either 'nb', 'is', or 'cl' for a decision curve
#' analysis 'net benefit' and 'interventions saved', or a model classification (e.g., sensitivity
#' and specificity) according to a selected threshold. Net benefit and interventions saved display
#' results for specific percentiles found between the 1st and 99th percentiles. Default is 'nb'.
#' @param main the main title of the plot.
#' @param xlab a character vector label for the x-axis.
#' @param ylab a character vector label for the y-axis.
#' @param xlim specify plot's x-axis limits with a 2 element numeric vector.
#' @param ylim specify plot's y-axis limits with a 2 element numeric vector. When y='is', the y-axis
#' labels are in integers (e.g., 1 to 100) but the actual scale is in decimals (0.0 to 1.0), please
#' note when making y-axis limits.
#' @param lwd select the line width.
#' @param bcol a multiple element character vector of length == 2  to specify the bar, band, or block colors that
#' are the shading of the true and false classification regions of the plot (e.g., true-positive and false-negative).
#' When y='cl', the first color represents 'true' and the second color is for 'negative'. Default is null, if none
#' selected, the colors are c('green', 'red').
#' @param lcol a single or multiple element character vector to specify the line color(s).
#' When y='nb', select up to 3 colors in this order for model, 'net benefit', 'all treated', and 'none treated' line colors.
#' When y='is', select 1 color for the interventions saved line. And when y='cl', select 1 color to represent the selected threshold.
#' @param add.legend add a legend by selecting the location as "bottomright", "bottom", "bottomleft",
#' "left", "topleft", "top", "topright", "right", "center". Default is no legend produced if nothing is selected.
#' @param legend a character vector of length >= 2 to appear when y='nb' and y='cl' with legend description.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default of 1.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex.
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex.
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex.
#' @param cex.legend The magnification to be used for the legend added into the plot relative to the current setting of 1.
#' @param round.c an integer indicating the number of decimal places when rounding numbers y='multi' and y='target'. Default is 2.
#' @param ... additional arguments.
#'
#' @return plots of model classification, net benefit, and interventions saved.
#' @importFrom graphics lines plot abline points text arrows
#' @importFrom methods is
#' @importFrom stats lm
#' @export
#' @references
#' Vickers, A. & Elkin, E. (2006). Decision Curve Analysis: A Novel Method for Evaluating Prediction Models.
#' Society for Medical Decision Making, 26, 6, 565-574. https://doi.org/10.1177/0272989X06295361
#'
#' @examples
#' ## Predicting car engine shape type, v or straight  ##
#' # run the model
#' car_m1 <- assess(formula=vs ~ hp + am, data=mtcars, regression="logistic")
#' # create a decide object, enter the model name and a threshold on the logit scale
#' d1 <- decide(x=car_m1, threshold= -0.767)
#' #Plot the classification results
#' plot(x=d1, y= "cl")
#'
#' #Plot the net benefit
#' plot(x=d1, y= "nb")
#'
#' #Plot the interventions saved
#' plot(x=d1, y= "is")

plot.decide <- function(x, y=NULL, main=NULL, xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, lwd=NULL,
                       bcol=NULL, lcol=NULL, add.legend=NULL, legend=NULL, cex=1, cex.lab=NULL, cex.axis=NULL, cex.main=NULL,
                       cex.legend=NULL, round.c=2, ...) {

  if (any(class(x) == "decide") == FALSE) {stop("Error: Expecting decide class object." )}

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
  #######
  # CEX #
  #######
  #Make CEX for legend
  if(!is.null(cex.legend)) {
    cex.legend <- cex.legend
  } else {
    cex.legend <- 1
  }
  #cex.axis
  if(!is.null(cex.axis)) {
    cex.axis <- cex.axis
  } else {
    cex.axis <- 1
  }
  #cex.lab
  if(!is.null(cex.lab)) {
    cex.lab <- cex.lab
  } else {
    cex.lab <- 1
  }
  #cex.main
  if(!is.null(cex.main)) {
    cex.main <- cex.main
  } else {
    cex.main <- 1
  }
  #cex
  if(!is.null(cex)) {
    cex <- cex
  } else {
    cex <- 1
  }

##############
## Graphing ##
##############
  # Labels #
  if(y == "cl") {
    #Create AUC
    AUC <-  x$AUC
  if(!is.null(main)) {
    main <- main
  } else {
    main <- paste0("Classification: ", "AUC= ", round(AUC, round.c), ", ", "Sensitivity= ", round(x$Classification$propAbovMY1, round.c), ", ", "Specifity= ", round(x$Classification$specifity, round.c), ".")
  }
    }

fncYhatClassPlt <- function(x, Brks=NULL, RegType=NULL, xlab=NULL, ylab=NULL,
                            aspectRatio=NULL, TBar=NULL, FBar=NULL, xlim=NULL, ylim=NULL,
                            ThreshCol=NULL, add.legend=NULL, legend=NULL, main=NULL,
                            cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                            cex.axis=cex.axis, cex.legend=cex.legend,
                            lwd=lwd, round.c=round.c)  {
  ClassDF <- x$Classification
  AUC <-  round(x$AUC, round.c)
  thresh_lev <-  round(x$Classification$threshLev, round.c)
  xlimMin <- ClassDF$senspcXmin
  xlimMax <- ClassDF$senspcXmax
  if(!is.null(xlab)) {
    xlab <- xlab
  } else {
    xlab <- paste0("Top: When outcome= 1. ", "Bottom: When outcome= 0. ", "True predictions in ", TBar, ", false predictions in ", FBar, ".")
  }
  if(!is.null(ylab)) {
    ylab <- ylab
  } else {
    ylab <- paste0("Threshold= ", ClassDF$threshLev, " as ", ThreshCol, " line")

  }
  if(!is.null(legend)) {
    legend <- legend
  } else {
    legend <- c("True predictions", "False predictions")
  }
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
  plot(c(den1$x, den2$x), c(den1$y, den2$y), type="n", axes=FALSE,
       main=main, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim,
       cex.lab= cex.lab, cex= cex, cex.main=cex.main)
  lines(den1$x, den1$y, lwd=lwd)
  lines(den2$x, den2$y, lwd=lwd)
  abline(h=0)
  axis(1, cex.axis=cex.axis)
  box()

  # 4. Fill the first half (below threshold)
  #   a. Identify x-values below the median
  x.left1 <- den1$x[den1$x <= ClassDF$threshLev]
  x.left2 <- den2$x[den2$x > ClassDF$threshLev]
  #   b. Identify corresponding y-values
  y.left1 <- den1$y[den1$x <= ClassDF$threshLev]
  y.left2 <- den2$y[den2$x > ClassDF$threshLev]
  #   c. Create the polygon points: add the median x, 0 to close the shape
  polygon(c(x.left1, ClassDF$threshLev), c(y.left1, 0), col = FBar, border = FBar)
  polygon(c(x.left2, ClassDF$threshLev), c(y.left2, 0- max_den), col = FBar, border = FBar)
  # 5. Fill the right side (above the threshold)
  #   a. Identify x-values above the threshold
  x.right1 <- den1$x[den1$x >= ClassDF$threshLev]
  x.right2 <- den2$x[den2$x < ClassDF$threshLev]
  #   b. Identify corresponding y-values
  y.right1 <- den1$y[den1$x >= ClassDF$threshLev]
  y.right2 <- den2$y[den2$x < ClassDF$threshLev]
  #   c. Create the polygon space
  polygon(c(x.right1, ClassDF$threshLev), c(y.right1, 0), col = TBar, border = TBar)
  polygon(c(x.right2, ClassDF$threshLev), c(y.right2, 0- max_den), col = TBar, border = TBar)
  # 6. Redraw the density line over the filled areas for clarity
  lines(den1)
  lines(den2)
  # 7. Add a vertical line at the median for the threshold
  abline(v = ClassDF$threshLev, col = ThreshCol, lty = 2, lwd = lwd)
  # 8. Add a legend
  #Add legend
  if(!is.null(add.legend) ) {
  legend(add.legend, legend=legend, fill = c(TBar, FBar ), lty=NA,
         border = NA, bty="n", inset=c(0, .05), cex=cex.legend, lwd=lwd)
    }
}
#####################################
## Function to plot decision curve ##
#####################################
if(y == "nb") {
if(!is.null(main)) {
  main <- main
} else {
  main <- "Decision curve analysis of net benefit by threshold levels"
}
if(!is.null(xlab)) {
  xlab <- xlab
} else {
  xlab <- "Threshold: Probability"
}
if(!is.null(ylab)) {
  ylab <- ylab
} else {
  ylab <- "Net Benefit"
}
  if(!is.null(legend)) {
    legend <- legend
  } else {
    legend <- c("Model", "All treated", "None treated")
  }
  #Give default ylims if doing a net benefit
  if (!is.null(ylim)) {
    ylim <- ylim
  } else {
    ylim <- c(-0.02, max(x$DCA$Net.Benefit)*1.05 )
  }
}

if(y == "is") {
  if(!is.null(main)) {
    main <- main
  } else {
    main <- "Decision curve analysis of interventions saved by threshold levels"
  }
  if(!is.null(xlab)) {
    xlab <- xlab
  } else {
    xlab <- "Threshold: Probability"
  }
  if(!is.null(ylab)) {
    ylab <- ylab
  } else {
    ylab <- "Interventions avoided per 100 persons"
  }
}

fncDcsnCrvPlt <- function(ThreshQntl, CType, xlim=NULL, ylim=NULL, main=NULL,
                          Legend.Loc=NULL, LCol=NULL, LSize=NULL, xlab=NULL, ylab=NULL,
                          cex=1, ...) {
  if(CType == "nb") {
    plot(ThreshQntl$Threshold.Level, ThreshQntl$Net.Benefit,type="n", xlim=xlim, ylim=ylim,
         main=main, xlab=xlab, ylab=ylab, cex.main=cex.main, cex.lab=cex.lab)
    lines(ThreshQntl$Threshold.Level, ThreshQntl$Net.Benefit, cex=cex, lwd=lwd, lty=1, col=LCol[1])
    lines(ThreshQntl$Threshold.Level, ThreshQntl$All.Treated, cex=cex, lwd=lwd, lty=2, col=LCol[2])
    abline(h=0, col=LCol[3], lwd=LSize)
    if(!is.null(Legend.Loc) ) {
    legend(x=Legend.Loc, legend=legend, col=LCol, lty=c(1,2,1), lwd=lwd, cex=cex.legend,
           bty="n", inset=c(0, .05))
    }
  }
  if(CType == "is") {
    plot(ThreshQntl$Threshold.Level, ThreshQntl$Interventions.Saved,
         lwd=LSize, type="l", col=LCol[1], axes=F, xlim=xlim, ylim=ylim,
         main=main, xlab=xlab, ylab=ylab, cex.main=cex.main, cex.lab=cex.lab)
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
  fncDcsnCrvPlt(ThreshQntl=x$DCA, CType="nb",  LCol= lcol, main=main,
                xlim=xlim, ylim=ylim,
                xlab=xlab, ylab=ylab, LSize=lwd, Legend.Loc=add.legend)
}
# interventions saved
if(y=="is") {
  fncDcsnCrvPlt(ThreshQntl=x$DCA, CType="is",  LCol= lcol, main=main,
                xlim=xlim, ylim=ylim,
                xlab=xlab, ylab=ylab, LSize=lwd, Legend.Loc=add.legend)
}

# Classification sensitivity and specificity
if(y=="cl") {
fncYhatClassPlt(x=x, RegType= x$type, main=main, TBar=bcol[1], FBar=bcol[2],
                ThreshCol=lcol[1], cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                cex.axis=cex.axis, add.legend=add.legend, legend=legend, cex.legend=cex.legend,
                lwd=lwd, xlab=xlab, ylab=ylab, xlim=xlim, ylim=ylim)
}

}


