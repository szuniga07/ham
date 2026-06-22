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
#' @param increase a named list object of the coefficient name and associated increase in the predictor. For example,
#' in the model y ~ a + b1 + b2, increase= list(b1=10), will produce point estimates and confidence intervals adjusted
#' to a 10-unit increase in the coefficient b1. This is most helpful when the continuous predictor has many levels and
#' the interpretation of the standard 1-unit increase is less informative than a larger change in X. For example,
#' discussing a difference in 10 or 20 years may be more helpful in describing the impact on 30-day mortality.
#' @param main overall title for the plot, default is NULL which then lists the outcome name for OLS regression or
#' 'Odds Ratio' (logistic), 'Incidence Rate Ratio' (Poisson), or 'Hazard Ratio' (Cox).
#' @param sort specify how confidence intervals are sorted. Options are 'alpha' (alphabetical coefficient names),
#' 'coef' (coefficient values), 'p' (p-values), or 'enter' (how variables were entered in the model).
#' Default is 'enter'. See decreasing argument to adjust how the selection is then sorted.
#' @param decreasing logical TRUE or FALSE that indicates how confidence intervals are sorted. Defaults to FALSE.
#' @param abbrv the minimum length of the coefficient name abbreviations. Default is 5.
#' @param xlim specify plot's x-axis limits with a 2 value vector.
#' @param ylim specify plot's y-axis limits with a 2 value vector.
#' @param xlab a vector label for the x-axis.
#' @param ylab a vector label for the y-axis.
#' @param lwd select the line width. Default is 1.
#' @param color the color(s) to be used for the horizontal lines. Default is 'gray'.
#' @param scol the color to be used for lines when there are significant results. Default is 'red'.
#' @param pcol select the point color for the point estimates. Default is 'blue'.
#' @param pt.cex the cex to be applied to plotting symbols, default is 2.
#' @param pch the plotting character or symbol to be used, default is 17.
#' @param tgt specify 1 or more values on the x-axis of where to add a target line when y='group'. Or 1 or more values on the y-axis of where to add a
#' target line when y='time' or 'roll'. Default is NULL.
#' @param tcol select a color for the target line. Default is 'black'.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default of 1.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex. Default is 1.
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex. Default is 1.
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex. Default is 1.
#' @param print logical TRUE or FALSE that indicates whether to print point estimates and confidence intervals. Defaults to FALSE.
#' @param round.c an integer indicating the number of decimal places to be used for rounding values in the printed point estimates and confidence intervals. Default is 3.
#' @param ... additional arguments.
#'
#' @return plot of variable importance, significant variables highlighted in red.
#'
#' @importFrom graphics axis dotchart
#' @export
#' @references
#' Harrell, F. E., Jr. (2016). Regression Modeling Strategies, Second Edition.
#' Springer International Publishing. ISBN: 978-3-319-19424-0.
#'
#' @examples
#' # OLS regression
#'
#' # logistic regression
plot.summary <- function(x, y=NULL, increase=NULL, main=NULL, sort=NULL, decreasing=NULL, abbrv=NULL,
                         xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL, lwd=NULL, color=NULL,
                         scol=NULL, pcol=NULL, pt.cex=NULL, pch=NULL, tgt=NULL,
                         tcol=NULL, cex=NULL, cex.axis=NULL, cex.lab=NULL, cex.main=NULL,
                         print=NULL, round.c=NULL, ...) {

  ##############################################################################
  #                                Coefficients                                #
  ##############################################################################
  fncCoef <- function(x, y=NULL) {
    #Get regression model type
    if (any(class(x) == "summary.lm") == TRUE) {
      reg_type <- "ols"
    }
    if (any(class(x) == "summary.glm") == TRUE && x$family$family == "binomial") {
      reg_type <- "logistic"
    }
    if (any(class(x) == "summary.glm") == TRUE && x$family$family == "poisson") {
      reg_type <- "poisson"
    }
    if (any(class(x) == "summary.coxph") == TRUE) {
      reg_type <- "cox"
    }
    if (all(class(x) %in% c("summary.coxph", "summary.lm", "summary.glm")) == FALSE) {
      reg_type <- "other"
    }
    #Get degrees of freedom
    if (reg_type == "ols") {
      degrees_of_freedom <- max(x$df)
    } else {
      degrees_of_freedom <- 1
    }
    #Get critical values for distributions
    if (reg_type == "ols") {
      critical_value <- qt(p=.975, df=degrees_of_freedom)
    } else {
      critical_value <- qnorm(p=.975)
    }
    #Get intercept column name
    intercept_row <- grep("Intercept", row.names(x[["coefficients"]]) )
    #Get the number of coefficients
    names_coefs <- rownames(x[["coefficients"]])
    #Create multiplier values
    multi_vals <- rep(1, length(names_coefs))
    #create multiplier values based on coefficients
    names(multi_vals) <- names_coefs
    #Identify which standard values of 1 will get changed
    if(!is.null(y)) {
      match1 <- match(names(y), names(multi_vals))
      multi_vals[names(y)]
      multi_vals[match1] <- y
    }
    # Get point estimates and 95% confidence intervals #
    if (reg_type == "ols") {
      if(!is.null(y)) {
        point_estimate <- x[["coefficients"]][, "Estimate"] * multi_vals
        lower_ci <- (x[["coefficients"]][, "Estimate"] * multi_vals - ((x[["coefficients"]][, "Std. Error"]* multi_vals) * critical_value))
        upper_ci <- (x[["coefficients"]][, "Estimate"] * multi_vals + ((x[["coefficients"]][, "Std. Error"]* multi_vals) * critical_value))
        pvalue <- x[["coefficients"]][, grep("Pr", colnames(x[["coefficients"]]))]
        ci_data_frame <- data.frame(point_estimate, lower_ci, upper_ci, pvalue)
      } else {
        point_estimate <- x[["coefficients"]][, "Estimate"]
        lower_ci <- x[["coefficients"]][, "Estimate"] - ((x[["coefficients"]][, "Std. Error"]* multi_vals) * critical_value)
        upper_ci <- x[["coefficients"]][, "Estimate"] + ((x[["coefficients"]][, "Std. Error"]* multi_vals) * critical_value)
        pvalue <- x[["coefficients"]][, grep("Pr", colnames(x[["coefficients"]]))]
        ci_data_frame <- data.frame(point_estimate, lower_ci, upper_ci, pvalue)
      }
    }
    if (reg_type %in% c("logistic","poisson","other")) {
      if(!is.null(y)) {
        point_estimate <- exp(x[["coefficients"]][, "Estimate"] * multi_vals)
        lower_ci <- exp((x[["coefficients"]][, "Estimate"] * multi_vals - ((x[["coefficients"]][, "Std. Error"]* multi_vals) * critical_value)))
        upper_ci <- exp((x[["coefficients"]][, "Estimate"] * multi_vals + ((x[["coefficients"]][, "Std. Error"]* multi_vals) * critical_value)))
        pvalue <- x[["coefficients"]][, grep("Pr", colnames(x[["coefficients"]]))]
        ci_data_frame <- data.frame(point_estimate, lower_ci, upper_ci, pvalue)
      } else {
        point_estimate <- exp(x[["coefficients"]][, "Estimate"] )
        lower_ci <- exp(x[["coefficients"]][, "Estimate"] - ((x[["coefficients"]][, "Std. Error"]* multi_vals) * critical_value))
        upper_ci <- exp(x[["coefficients"]][, "Estimate"] + ((x[["coefficients"]][, "Std. Error"]* multi_vals) * critical_value))
        pvalue <- x[["coefficients"]][, grep("Pr", colnames(x[["coefficients"]]))]
        ci_data_frame <- data.frame(point_estimate, lower_ci, upper_ci, pvalue)
      }
    }
    if (reg_type == "cox") {
      if(!is.null(y)) {
        point_estimate <- exp(x[["coefficients"]][, "coef"] * multi_vals)
        lower_ci <- exp((x[["coefficients"]][, "coef"] * multi_vals - ((x[["coefficients"]][, "se(coef)"]* multi_vals) * critical_value)))
        upper_ci <- exp((x[["coefficients"]][, "coef"] * multi_vals + ((x[["coefficients"]][, "se(coef)"]* multi_vals) * critical_value)))
        pvalue <- x[["coefficients"]][, grep("Pr", colnames(x[["coefficients"]]))]
        ci_data_frame <- data.frame(point_estimate, lower_ci, upper_ci, pvalue)
      } else {
        point_estimate <- exp(x[["coefficients"]][, "coef"] )
        lower_ci <- exp(x[["coefficients"]][, "coef"] - ((x[["coefficients"]][, "se(coef)"]* multi_vals) * critical_value))
        upper_ci <- exp(x[["coefficients"]][, "coef"] + ((x[["coefficients"]][, "se(coef)"]* multi_vals) * critical_value))
        pvalue <- x[["coefficients"]][, grep("Pr", colnames(x[["coefficients"]]))]
        ci_data_frame <- data.frame(point_estimate, lower_ci, upper_ci, pvalue)
      }
    }
    #Drop intercept row
    if (reg_type != "cox") {
      ci_data_frame <- ci_data_frame[-intercept_row, ]
    }
    #Change column names
    colnames(ci_data_frame) <- c("PointEst","Lower","Upper", "P")
    #Return object
    return(list("Estimates"=ci_data_frame, "Regression"=reg_type, "Outcome"=x[["call"]][[2]][[2]]))
  }

  ##############################################################################
  #                                Graph                                       #
  ##############################################################################
  plot_summary <- function(adf, alpha_num=NULL, main=NULL, xlab=NULL, ylab=NULL,
                           lwd=NULL, Lcol=NULL, Pcol=NULL, tgt=NULL, Cbar=NULL,
                          roundVal=NULL, xlim=NULL, ylim=NULL, abbrv=NULL, tcol=NULL,
                          cex=NULL, cex.axis=NULL, cex.lab=NULL, cex.main=NULL,
                          scol=NULL, pt.cex=NULL, pch=NULL, print=NULL, decreasing=NULL) {
    RegType <- adf$Regression
    #Default title
    nom <- switch(RegType,
                  "ols"   = adf$Outcome,
                  "logistic" = "Odds Ratio",
                  "poisson"  = "Incidence Rate Ratio",
                  "cox" = "Hazard Ratio" )
    #Main title
    if(!is.null(main) ) {
      main_ttl <- main
    } else {
      main_ttl <- nom
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

    rng <- seq(min(adf$Estimates[, c("Lower","Upper")]), max(adf$Estimates[, c("Lower","Upper")]),
               length.out=nrow(adf$Estimates))
    plot(rng, 1:nrow(adf$Estimates), type="n", ylab=ylab,
         xlab= xlab, axes=F,  cex.lab=cex.lab, xlim=xlim, ylim=ylim)
    title(main_ttl, cex.main = cex.main)
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
    abline(v= tgt, lty=3)
    #Create values for the location of the y-axis labels
    y.at <- 1:nrow(adf$Estimates)
    axis(side=2, at=y.at, labels=rownames(adf$Estimates), cex.axis=cex.axis)
    axis(3, cex.axis=cex.axis)

    ## Add confidence lines ##
    for (i in 1:nrow(adf$Estimates)) {
      segments(x0 = adf$Estimates[i, "Lower"], y0 = i,
               x1 = adf$Estimates[i, "Upper"], y1 = i,
               col = color, lwd = lwd, lty=1)
    }
  }

  ################
  # Create graph #
  ################
  #Create point estimate and confidence interval data
  pecidf <- fncCoef(x=x, y=y)
  #Run graph
  plot_summary(adf=pecidf, alpha_num=sort, main=main, xlab=xlab, ylab=ylab,
               lwd=lwd, Lcol=color, Pcol=pcol, tgt=tgt, roundVal=round.c, xlim=xlim,
               ylim=ylim, abbrv=abbrv, tcol=tcol, cex=cex, cex.axis=cex.axis,
               cex.lab=cex.lab, cex.main=cex.main,
               scol=scol, pt.cex=pt.cex, pch=pch, print=print, decreasing=decreasing )

  #This closes out original plot.importance
}
