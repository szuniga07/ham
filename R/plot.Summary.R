#' Plot of a regression model's coefficient summary
#'
#' Plots a Summary class object of a regression model (Summary() with a capital 'S'). Prints a chart of
#' the predictor's coefficient estimates with 95% confidence intervals (Harrell, 2015). Excludes the
#' intercept (if present). Horizontal line segments represent the predictor variable coefficients and
#' the vertical line is at 0 or 1 (e.g., odds ratio, incidence rate ratio, hazard ratio and other GLMs at 1)
#' to show if the proper reference is outside of the "normal range" or what represents no significant
#' difference. The Summary plot can be used on assess objects as well as models created from the base
#' package lm() and glm() as well as coxph() from the survival package. There are various graphing
#' options and sorting by coefficient names, values, p-value or the model formula.
#'
#' @param x Summary object from assess() model or lm(), glm(), and coxph() models. For assess objects, use
#' Summary(my_regression$model) unless using ITS or DID models (my_did_model$DID). For model objects from
#' lm(), glm(), or coxph(), use Summary(my_model).
#' @param y not currently used.
#' @param main overall title for the plot, default is NULL which then lists 'Summary' for OLS regression or the type
#' of Summary statistic such as 'Odds Ratio' (logistic), 'Incidence Rate Ratio' (Poisson), or 'Hazard Ratio' (Cox).
#' @param sub a character vector for the subtitle of the plot, default is NULL which then lists the outcome name.
#' @param sort specify how confidence intervals are sorted. Options are 'alpha' (alphabetical coefficient names),
#' 'coef' (coefficient values), 'p' (p-values), or 'enter' (how variables were entered in the model).
#' Default is 'enter'. See decreasing argument to adjust how the selection is then sorted.
#' @param decreasing logical TRUE or FALSE that indicates how confidence intervals are sorted, in decreasing order or not.
#' Defaults to FALSE.
#' @param abbrv the minimum length of the character length for the coefficient name abbreviations. Default is 7.
#' @param xlim specify plot's x-axis limits using a numeric vector with length equal to 2.
#' @param ylim specify plot's y-axis limits using a numeric vector with length equal to 2.
#' @param xlab a vector label for the x-axis. Default is a blank space. Caution: It will overlap the 'sub' argument if sub != "".
#' @param ylab a vector label for the y-axis. Default is a blank space.
#' @param lwd select the line width. Default is 1.
#' @param color the color to be used for the horizontal lines. Default is 'blue'.
#' @param pcol select the point color for the point estimates. Default is 'red'.
#' @param pt.cex the cex to be applied to plotting symbols, default is 1.
#' @param pch the plotting character or symbol to be used, default is 17.
#' @param tgt specify a value on the x-axis of where to add a target line to indicate when confidence intervals fall outside
#' of a normal range or value that indicates no difference.
#' @param tcol select a color for the vertical target line. Default is 'black'.
#' @param cex a numerical value giving the amount by which plotting text and symbols should be magnified relative to the default of 1.
#' @param cex.axis the magnification to be used for axis annotation relative to the current setting of cex. Default is 1.
#' @param cex.lab the magnification to be used for x and y labels relative to the current setting of cex. Default is 1.
#' @param cex.main the magnification to be used for main titles relative to the current setting of cex. Default is 1.
#' @param cex.sub the magnification to be used for subtitles relative to the current setting of cex. Default is 1.
#' @param print logical TRUE or FALSE that indicates whether to print point estimates and confidence intervals. Defaults to FALSE.
#' @param round.c an integer indicating the number of decimal places to be used for rounding values in the printed point estimates
#' and confidence intervals. Default is 4.
#' @param ... additional arguments.
#'
#' @return plot of the Summary of regression coefficients, the display can be modified in various ways.
#'
#' @importFrom graphics axis
#' @export
#' @references
#' Harrell, F. E., Jr. (2015). Regression Modeling Strategies, Second Edition.
#' Springer International Publishing. ISBN: 978-3-319-19424-0.
#'
#' @examples
#' # OLS regression--most basic format with Base R lm()
#' m01 <- lm(mpg ~ wt+hp+am, data=mtcars)
#' # note it uses Summary() with a captialized 'S'
#' plot(x=Summary(m01))
#'
#' # Using the assess function, notice 'm02$model' object below
#' m02 <- assess(formula=mpg ~ wt+hp+am, data=mtcars, regression="ols")
#' # let's see the impactful 83.5 hp increase in the 1st to 3rd quartiles
#' plot(x=Summary(m02$model, increase=c(hp= 83.5)))
#'
#' #Logistic model
#' m03 <- glm(vs ~ wt+hp+am, data=mtcars, family="binomial")
#' # Display options, sorted plot, and printing the 95% CIs to review
#' plot(x=Summary(m03, increase=c(wt=1.1375, hp= 83.5) ), color="aquamarine", lwd=4,
#' pt.cex= 2, tcol="magenta", sort="coef", cex.axis=2, cex.main=2, xlim=c(-.5, 3),
#' print=TRUE, round.c=6)
#'
#' #Poisson model with an offset using assess()
#' m04 <- assess(formula=HAI ~  Month+ offset(log(PatientDays)),
#' data=infections, regression="poisson")
#' #Because 1 year is a meaningful period in program evaluation, Month=12
#' plot(x=Summary(m04$model, increase=c(Month=12)), xlim=c(0.6, 1.01),
#' lwd=7, color="cyan", pcol="salmon", pt.cex=2)
#'
#' #NOT RUN: Cox Proportional Hazards model
#' #library(survival)
#' #m04 <- coxph(Surv(time, status) ~ age+sex+ph.karno, data=cancer)
#' #plot(x=Summary(m04, increase=c(age=13, ph.karno=15)))
#'
#' # This also works for ITS and DID causal models through ham
#' im22 <- assess(formula=los ~ ., data=hosprog, intervention = "program",
#' int.time="month", interrupt= c(5, 9), its="two")
#' # The intervention group had the biggest change between the baseline and month 5
#' plot(Summary(im22$ITS), sort="coef", decreasing=TRUE, color="red", pcol="green")
#'
plot.Summary <- function(x, y=NULL, main=NULL, sub=NULL, sort=NULL,
                         decreasing=NULL, abbrv=NULL, xlim=NULL, ylim=NULL, xlab=NULL,
                         ylab=NULL, lwd=NULL, color=NULL, pcol=NULL, pt.cex=NULL, pch=NULL,
                         tgt=NULL, tcol=NULL, cex=NULL, cex.axis=NULL, cex.lab=NULL,
                         cex.main=NULL, cex.sub=NULL, print=NULL, round.c=NULL, ...) {
  #Checks
  if (any(class(x) == 'Summary') ==FALSE) {stop("Error: Expecting 'Summary' class object." )}
  if(!is.null(sort)) {
    if (!sort %in% c('alpha','coef','p','enter')) {stop("Error: Expecting 'sort' using one of these options: 'alpha', 'coef', 'p', 'enter' ." )}
  }
  if(!is.null(decreasing)) {
    if (is.logical(decreasing) == FALSE) {stop("Error: Expecting 'decreasing' is a logical (TRUE or FALSE) class object." )}
  }
  if(!is.null(abbrv)) {
    if (is.numeric(abbrv) == FALSE) {stop("Error: Expecting 'abbrv' is a numeric class object." )}
  }
  if(!is.null(print)) {
    if (is.logical(print) == FALSE) {stop("Error: Expecting 'print' is a logical (TRUE or FALSE) class object." )}
  }

  ##############################################################################
  #                                Graph                                       #
  ##############################################################################
  plot_Summary <- function(adf, alpha_num=NULL, main=NULL, xlab=NULL, ylab=NULL,
                           lwd=NULL, Lcol=NULL, Pcol=NULL, tgt=NULL, roundVal=NULL,
                           xlim=NULL, ylim=NULL, abbrv=NULL, tcol=NULL, cex=NULL,
                           cex.axis=NULL, cex.lab=NULL, cex.main=NULL, cex.sub=NULL,
                          pt.cex=NULL, pch=NULL, print=NULL,sub=NULL,decreasing=NULL) {
    if(!is.null(cex)) {
      cex <- cex
    } else {
      cex <- 1
    }
    if(!is.null(cex.axis)) {
      cex.axis <- cex.axis
    } else {
      cex.axis <- 1
    }
    if(!is.null(cex.lab)) {
      cex.lab <- cex.lab
    } else {
      cex.lab <- 1
    }
    if(!is.null(cex.main)) {
      cex.main <- cex.main
    } else {
      cex.main <- 1
    }
    if(!is.null(cex.sub)) {
      cex.sub <- cex.sub
    } else {
      cex.sub <- 1
    }
    #Line width
    if(!is.null(lwd)) {
      lwd <- lwd
    } else {
      lwd <- 1
    }
    if(!is.null(cex)) {
      pt.cex <- pt.cex
    } else {
      pt.cex <- 1
    }
    #The regression type
    RegType <- adf$Regression
    #Order of point estimates and confidence intervals
    if(!is.null(alpha_num) ) {
      alpha_num <- alpha_num
    } else {
      alpha_num <- 'enter'
    }
    #sorted point estimates and confidence intervals
    sort_order <- switch(alpha_num,
                  'alpha'  = order(rownames(adf$Estimates)),
                  'coef'   = order(adf$Estimates[, "PointEst"]),
                  'p'      = order(adf$Estimates[, "P"]),
                  'enter'  = 1:nrow(adf$Estimates) )

    #Set decreasing argument
    if(!is.null(decreasing)) {
      decreasing <- decreasing
    } else {
      decreasing <- FALSE
    }
    #Get get correct sort order
    if(decreasing == FALSE) {
      sort_order <- rev(sort_order)
    }
    if(decreasing == TRUE) {
      sort_order <- sort_order
    }
    #Get ordered point estimates and confidence intervals
    adf_est <- adf$Estimates[sort_order, ]
    #Abbreviated row names
    if(!is.null(abbrv)) {
      abbrv <- abbrv
    } else {
      abbrv <- 7
    }
    rownames(adf_est) <- abbreviate(rownames(adf_est), abbrv)
    #Set up print
    if(!is.null(print) ) {
      print <- print
    } else {
      print <- FALSE
    }
    #Rounding decimals for print output
    if(!is.null(round.c)) {
      round.c <- round.c
    } else {
      round.c <- 4
    }
    #Print output
    if(print == TRUE) {
      print(round(adf$Estimates[rev(sort_order), ], round.c))
    }

    #Default title
    nom <- switch(RegType,
                  "ols"   = "Summary",
                  "logistic" = "Odds Ratio",
                  "poisson"  = "Incidence Rate Ratio",
                  "cox" = "Hazard Ratio" )
    #Main title
    if(!is.null(main) ) {
      main_ttl <- main
    } else {
      main_ttl <- nom
    }
    #Sub title
    if(!is.null(sub) ) {
      sub_ttl <- sub
    } else {
      sub_ttl <- adf$Outcome
    }
    # Create x and y labels
    if (!is.null(xlab)) {
      xlab <- xlab
    } else {
      xlab <- ""
    }
    if (!is.null(ylab)) {
      ylab <- ylab
    } else {
      ylab <- ""
    }
    #Printing range of x-axis
    rng <- seq(min(adf_est[, "Lower"]), max(adf_est[, "Upper"]),
               length.out=nrow(adf_est))
    plot(rng, 1:nrow(adf_est), type="n", ylab=ylab,
         xlab= xlab, axes=F,  cex.lab=cex.lab, xlim=xlim, ylim=ylim)
    title(main=main_ttl, sub=sub_ttl, cex.main=cex.main, cex.sub=cex.sub, line=2.75)
    #Target line
    if(!is.null(tgt)) {
      tgt <- tgt
    } else {
      if(RegType == "ols") {
        tgt <- 0
      } else {
        tgt <- 1
      }
    }
    #Vertical target line color
    if(!is.null(tcol)) {
      tcol <- tcol
    } else {
      tcol <- "black"
    }
    abline(v= tgt, lty=3, col=tcol, lwd=2)
    #Create values for the location of the y-axis labels
    y.at <- 1:nrow(adf_est)
    axis(side=2, at=y.at, labels=rownames(adf_est), cex.axis=cex.axis,
         las=1, line=-1.75, tick= FALSE)
    axis(3, cex.axis=cex.axis, padj=.5, line=.5)
    ## Add confidence lines ##
    if (!is.null(Lcol)) {
      Lcol <- Lcol
    } else {
      Lcol <- "blue"
    }
    for (i in 1:nrow(adf_est)) {
      segments(x0 = adf_est[i, "Lower"], y0 = i,
               x1 = adf_est[i, "Upper"], y1 = i,
               col = Lcol, lwd = lwd, lty=1)
    }
    ## Add Point Estimates ##
    if (!is.null(pch)) {
      pch <- pch
    } else {
      pch <- 17
    }
    if (!is.null(Pcol)) {
      Pcol <- Pcol
    } else {
      Pcol <- "red"
    }
    for (i in 1:nrow(adf_est)) {
      points(adf_est[, 'PointEst'][i],i, pch=pch, col=Pcol, bg=Pcol, cex=pt.cex )
    }

  }

  ################
  # Create graph #
  ################
  #Run graph
  plot_Summary(adf=x, alpha_num=sort, main=main, xlab=xlab,
               ylab=ylab, lwd=lwd, Lcol=color, Pcol=pcol, tgt=tgt, roundVal=round.c, xlim=xlim,
               ylim=ylim, abbrv=abbrv, tcol=tcol, cex=cex, cex.axis=cex.axis,
               cex.lab=cex.lab, cex.main=cex.main, cex.sub=cex.sub,
               pt.cex=pt.cex, pch=pch, print=print, sub=sub, decreasing=decreasing )
}
