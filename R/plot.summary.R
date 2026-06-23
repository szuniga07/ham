#' Plot of a regression model's coefficient summary
#'
#' Plots a summary class object of a regression model. Produces a chart of the predictor's
#' coefficient estimates with 95% confidence intervals (Harrell, 2015). Excludes the intercept
#' (if present). Horizontal line segments represent the predictor variable coefficients and
#' vertical lines are at 0 or 1 (e.g., odds ratio, incidence rate ratio, hazard ratio and other GLMs)
#' to show if the proper reference is outside of the "normal range" or what represents no significant
#' difference. The summary plot can be used on assess objects as well as models created from the base
#' package lm() and glm() as well as coxph() from the survival package. There are various graphing
#' options and sorting by coefficient name, value, p-value or the model formula.
#'
#' @param x summary object from assess() model or lm(), glm(), and coxph() models. For assess objects, use
#' summary(x$model). For model objects from lm(), glm(), or coxph(), use summary(x).
#' @param y not currently used.
#' @param coefs an expression defining a subset of the predictor coefficient rows (i.e., not the intercept) to view in
#' the summary plot. The default is NULL, thereby using all predictor coefficients. Specify, for example, 1:2 to
#' view just the summary of the first 2 coefficients. It is recommended to use print=TRUE to confirm the coefficient
#' names because they will follow that same order and not necessarily the order of the coefficients listed in the
#' original regression model.
#' @param increase a named numeric vector object of the coefficient name and associated increase in the predictor. For example,
#' in the model y ~ a + b1 + b2, increase= c(b1= 10), will produce point estimates and confidence intervals adjusted
#' to a 10-unit increase in the coefficient b1. This is most helpful when the continuous predictor has many levels and
#' the interpretation of the standard 1-unit increase is less informative than a larger change in X. For example,
#' discussing a difference in 10 or 20 years may be more helpful in describing the impact on 30-day mortality.
#' @param main overall title for the plot, default is NULL which then lists 'Summary'  for OLS regression or the type
#' of summary statistic such as 'Odds Ratio' (logistic), 'Incidence Rate Ratio' (Poisson), or 'Hazard Ratio' (Cox).
#' @param sub a character vector for the subtitle of the plot, default is NULL which then lists the outcome name.
#' @param sort specify how confidence intervals are sorted. Options are 'alpha' (alphabetical coefficient names),
#' 'coef' (coefficient values), 'p' (p-values), or 'enter' (how variables were entered in the model).
#' Default is 'enter'. See decreasing argument to adjust how the selection is then sorted.
#' @param decreasing logical TRUE or FALSE that indicates how confidence intervals are sorted, in decreasing order or not.
#' Defaults to FALSE.
#' @param abbrv the minimum length of the character length for the coefficient name abbreviations. Default is 7.
#' @param xlim specify plot's x-axis limits with a 2 value numeric vector.
#' @param ylim specify plot's y-axis limits with a 2 value numeric vector.
#' @param xlab a vector label for the x-axis. Default is blank space. Caution: It will overlap the 'sub' argument if sub != "".
#' @param ylab a vector label for the y-axis. Default is blank space.
#' @param lwd select the line width. Default is 1.
#' @param color the color to be used for the horizontal lines. Default is 'blue'.
#' @param pcol select the point color for the point estimates. Default is 'red'.
#' @param pt.cex the cex to be applied to plotting symbols, default is 1.
#' @param pch the plotting character or symbol to be used, default is 17.
#' @param tgt specify 1 or more values on the x-axis of where to add a target line when y='group'. Or 1 or more values on the y-axis of where to add a
#' target line when y='time' or 'roll'. Default is NULL.
#' @param tcol select a color for the vertical target line. Default is 'black'.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default of 1.
#' @param cex.axis the magnification to be used for axis annotation relative to the current setting of cex. Default is 1.
#' @param cex.lab the magnification to be used for x and y labels relative to the current setting of cex. Default is 1.
#' @param cex.main the magnification to be used for main titles relative to the current setting of cex. Default is 1.
#' @param cex.sub the magnification to be used for subtitles relative to the current setting of cex. Default is 1.
#' @param print logical TRUE or FALSE that indicates whether to print point estimates and confidence intervals. Defaults to FALSE.
#' @param round.c an integer indicating the number of decimal places to be used for rounding values in the printed point estimates and confidence intervals. Default is 4.
#' @param ... additional arguments.
#'
#' @return plot of the summary of regression coefficients, the display can be modified in various ways.
#'
#' @importFrom graphics axis
#' @export
#' @references
#' Harrell, F. E., Jr. (2015). Regression Modeling Strategies, Second Edition.
#' Springer International Publishing. ISBN: 978-3-319-19424-0.
#'
#' @examples
#' # OLS regression--most basic format
#'
#' # logistic regression
plot.summary <- function(x, y=NULL, coefs=NULL, increase=NULL, main=NULL, sub=NULL, sort=NULL,
                         decreasing=NULL, abbrv=NULL, xlim=NULL, ylim=NULL, xlab=NULL,
                         ylab=NULL, lwd=NULL, color=NULL, pcol=NULL, pt.cex=NULL, pch=NULL,
                         tgt=NULL, tcol=NULL, cex=NULL, cex.axis=NULL, cex.lab=NULL,
                         cex.main=NULL, cex.sub=NULL, print=NULL, round.c=NULL, ...) {

  ##############################################################################
  #                                Coefficients                                #
  ##############################################################################
  fncCoef <- function(x, y=NULL, coefs=NULL) {
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
    #Identify all rows to use for subsets
    all_rows <- nrow(ci_data_frame)
    if(!is.null(coefs)) {
      coefs <- coefs
    } else {
      coefs <- 1:all_rows
    }
    #Create subset data if needed
    if(!is.null(coefs)) {
      ci_data_frame <-   ci_data_frame[coefs, ]
    }
    #Get outcome
    if(reg_type == "cox") {
      outcome_y <- x[["call"]][[2]][[2]]
    } else {
      outcome_y <- x$terms[[2]]
    }
    #Return object
    return(list("Estimates"=ci_data_frame, "Regression"=reg_type, "Outcome"= outcome_y))
  }

  ##############################################################################
  #                                Graph                                       #
  ##############################################################################
  plot_summary <- function(adf, alpha_num=NULL, main=NULL, xlab=NULL, ylab=NULL,
                           lwd=NULL, Lcol=NULL, Pcol=NULL, tgt=NULL,
                          roundVal=NULL, xlim=NULL, ylim=NULL, abbrv=NULL, tcol=NULL,
                          cex=NULL, cex.axis=NULL, cex.lab=NULL, cex.main=NULL, cex.sub=NULL,
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
#      print(round(adf_est[sort_order, ], round.c))
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
  #Create point estimate and confidence interval data
  pecidf <- fncCoef(x=x, y=increase, coefs=coefs)
  #Run graph
  plot_summary(adf=pecidf, alpha_num=sort, main=main, xlab=xlab,
               ylab=ylab, lwd=lwd, Lcol=color, Pcol=pcol, tgt=tgt, roundVal=round.c, xlim=xlim,
               ylim=ylim, abbrv=abbrv, tcol=tcol, cex=cex, cex.axis=cex.axis,
               cex.lab=cex.lab, cex.main=cex.main, cex.sub=cex.sub,
               pt.cex=pt.cex, pch=pch, print=print, sub=sub, decreasing=decreasing )
}
