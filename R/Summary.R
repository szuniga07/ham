#' Creates a Summary object of regression model results and for graphing purposes
#'
#' Produces a Summary object of regression models based on summary(model). The purpose
#' is to later pass the object to the plot function to produce a summary of the
#' predictor variable's coefficients. In other words, it excludes the intercept. The main
#' difference between Summary() and summary() is that the ham function can create the effects from increased predictor values and
#' it has a capital letter 'S' in Summary().
#'
#' @param model an assess class object or models with 'lm', 'glm', or 'coxph' class.
#' This includes the differences-in-differences and interrupted time series models.
#' For assess objects, make sure to select the correct model list element, for example,
#' model= my_regression$model for ordinary least squares, logistic, or Poisson models.
#' For 'ITS' or 'DID' models, use model= its_model$ITS or model= did_model$DID. If
#' using Base R's lm(), glm() or even coxph(), use the model object name (e.g., model= my_glm_model).
#' @param coefs an expression defining a subset of the predictor coefficient rows (i.e., not the intercept) to view in
#' the Summary plot. The default is NULL, thereby using all predictor coefficients. Specify, for example, 1:2 to
#' view just the Summary of the first 2 coefficients. It is recommended to use print=TRUE to confirm the coefficient
#' names because they will follow that same order and not necessarily the order of the coefficients listed in the
#' original regression model.
#' @param increase a named numeric vector object of the coefficient name and associated increase in the predictor.
#' For example, in the model y ~ a + x1 + x2, increase= c(x1= 10), will produce point estimates and confidence
#' intervals adjusted to a 10-unit increase in the variable x1. This is most helpful when the continuous predictor
#' has many levels and the interpretation of the standard 1-unit increase is less informative than a larger change
#' in X. For example, discussing a difference in 10 or 20 years may be more helpful in describing the impact on
#' 30-day mortality.
#'
#' @return a list object with regression model summary statistics as a data.frame and additional information.
#' @export
#'
#' @seealso [plot.Summary()] for a plot and printed output of the 'Summary' class object.
#'
#' @examples
#' # OLS regression
#' Summary(assess(mpg ~ hp + wt + cyl, data=mtcars, regression= "ols")$model)
#'
#' # Creates new classes useful for plots, e.g., plot(Summary(model))
#' class(Summary(assess(mpg ~ hp + wt + cyl, data=mtcars, regression= "ols")$model))
#'
#' # Works with Base R
#' Summary(lm(mpg ~ hp + wt + cyl, data=mtcars))
#'
#' # Effect after increasing hp and excluding am's coefficient
#' m03 <- glm(vs ~ wt+hp+am, data=mtcars, family="binomial")
#' plot(x=Summary(m03, increase=c(wt=1.1375, hp= 83.5), coefs= c(1, 3) ))
#'
#' @importFrom stats lm glm
Summary <- function(model, increase=NULL, coefs=NULL) {
  if (!any(class(model) %in% c("glm","lm","coxph", "assess"))) {stop("Error: Expecting 'assess', 'lm', 'glm', or 'coxph' class regression model." )}

  if(!is.null(coefs)) {
    if (is.numeric(coefs) == FALSE) {
      stop("Error: Expecting 'coefs' is a numeric class object." )
    }
  }
  if(!is.null(increase)) {
    if (is.numeric(increase) == FALSE) {stop("Error: Expecting 'increase' is a numeric class object." )}
  }
  if(!is.null(increase)) {
    if (is.null(names(increase))) {stop("Error: Expecting 'increase' is a named object." )}
  }

  #Make a data frame
  summary_model <- summary(model)

  #Get regression model type
  if (any(class(summary_model) == "summary.lm") == TRUE) {
    reg_type <- "ols"
  }
  if (any(class(summary_model) == "summary.glm") == TRUE && summary_model$family$family == "binomial") {
    reg_type <- "logistic"
  }
  if (any(class(summary_model) == "summary.glm") == TRUE && summary_model$family$family == "poisson") {
    reg_type <- "poisson"
  }
  if (any(class(summary_model) == "summary.coxph") == TRUE) {
    reg_type <- "cox"
  }
  if (any(class(summary_model) %in% c("summary.coxph", "summary.lm", "summary.glm")) == FALSE) {
    reg_type <- "other"
  }

  ##############################################################################
  #                                Coefficients                                #
  ##############################################################################
  fncCoef <- function(x, y=NULL, coefs=NULL) {

    #Get regression model type
    if (any(class(x) == "Summary.ols") == TRUE) {
      reg_type <- "ols"
    }
    if (any(class(x) == "Summary.logistic") == TRUE) {
      reg_type <- "logistic"
    }
    if (any(class(x) == "Summary.poisson") == TRUE) {
      reg_type <- "poisson"
    }
    if (any(class(x) == "Summary.coxph") == TRUE) {
      reg_type <- "cox"
    }
    if (any(class(x) == "Summary.other") == TRUE) {
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

  ################
  # Create data  #
  ################
  #Create point estimate and confidence interval data
  pecidf <- fncCoef(x=summary_model, y=increase, coefs=coefs)

#  return class
  class(pecidf) <- c("Summary", "ham", "list")
  return(pecidf)
}
