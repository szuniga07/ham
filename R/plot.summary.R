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
#' @param main overall title for the plot, default is 'Variable Importance'.
#' @param cex the character size to be used. Setting cex to a value smaller than 1
#' can be a useful way of avoiding label overlap. This sets the actual size,
#' not a multiple of par('cex').
#' @param pt.cex the cex to be applied to plotting symbols, default is 2.
#' @param pch the plotting character or symbol to be used, default is 19.
#' @param color the color to be used for points and labels when there are
#' significant results. Default is 'red'.
#' @param lcolor the color(s) to be used for the horizontal lines. Default is 'gray'.
#' @param ... additional arguments.
#'
#' @return plot of variable importance, significant variables highlighted in red.
#'
#' @importFrom graphics axis dotchart utils
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
plot.summary <- function(x, y, main=NULL, cex=NULL, pt.cex=NULL, pch=NULL,
                            color=NULL, lcolor=NULL, ...) {
  # Make default values #
  if (!is.null(color)) {  #label color
    color <- color
  } else {
    color <- "red"
  }
  if (!is.null(lcolor)) {  #line color
    lcolor <- lcolor
  } else {
    lcolor <- "gray"
  }
  if (!is.null(main)) {
    main <- main
  } else {
    main <- "Variable Importance"
  }
  if (!is.null(cex)) {
    cex <- cex
  } else {
    cex <- 1
  }
  if (!is.null(pt.cex)) {
    pt.cex <- pt.cex
  } else {
    pt.cex <- 2
  }
  if (!is.null(pch)) {
    pch <- pch
  } else {
    pch <- 19
  }
# main arguments #
object <- x
  # Plotting objects
  chi2_df <- object[, "Chi.Sq"]
  a_o <- order(chi2_df, decreasing = T)
  a_num <- chi2_df[a_o ]
  p_vals <- object[a_o, "p.value"]
  chi2_labels <- object[, "X"][a_o]

  # Create the dot chart
  graphics::dotchart(a_num, labels = chi2_labels, main = main,
           col = ifelse(p_vals < .05, color, "black"), ,
           xlab="X^2 - d.f.", lcolor=lcolor,
           cex=cex, pt.cex=pt.cex, pch = pch)
  # Add labels to the right margin
  graphics::axis(4, at=1:(nrow(object)), tick= FALSE, las=1, cex.axis= 1.1,
       labels= paste("p=" , sprintf("%.4f", p_vals)), line=-.7 )

  ##############################################################################
  #                                NEW                                         #
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
    return(list("Estimates"=ci_data_frame, "Regression"=reg_type))
  }



  #This closes out original plot.importance
}
