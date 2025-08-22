#' Prediction plot of treatment and control groups for DID and ITS models
#'
#' Provides partial prediction plots for treatment and control groups from difference-in-difference (DID)
#' and interrupted time series (ITS) models. The graph will produce lines for treatment/intervention and
#' control groups to gain understanding through a visual representation of the regression coefficients.
#' The treatment/intervention group is represented with a blue line, the control group is represented with
#' a red line, and the counterfactual line, when available, is a dashed line.
#'
#' @param x assess object. Either difference-in-difference or interrupted time series model with no covariate adjustment.
#' @param y type of model, specify either 'DID' (difference-in-difference) or 'ITS' (interrupted time series). Will not accept other models.
#' @param xlim specify plot's x-axis limits with a 2 value vector.
#' @param ylim specify plot's y-axis limits with a 2 value vector.
#' @param add.legend add a legend by selecting the location as "bottomright", "bottom", "bottomleft",
#' "left", "topleft", "top", "topright", "right", "center". No legend if nothing selected.
#' @param ... additional arguments.
#'
#' @return plot of partial predictions for treatment and control groups.
#' @importFrom graphics lines plot legend abline segments
#' @export
#'
#' @examples
#' am2 <- assess(formula= los ~ ., data=hosprog, intervention = "program",
#' topcode =NULL, int.time="month", regression="none", treatment= 5,
#' interrupt=c(5,9), did="many", its="two", newdata=TRUE, propensity=NULL)
#' plot(am2, "DID", add.legend="bottomleft", ylim=c(2, 8))  #DID model
#' plot(am2, "ITS", add.legend="top", ylim=c(2, 8))         #ITS model
plot.assess <- function(x, y, xlim=NULL, ylim=NULL, add.legend=NULL, ...) {
  if(any(is.null(c(x, y)) == TRUE)) {
    stop("Error: Expecting both an x and y argument.")
  }
  if (any(class(x) == "assess") == FALSE) {stop("Error: Expecting assess class object." )}
  if (!y %in% c("DID", "ITS")) {stop("Error: Expecting y='DID' or y='ITS'." )}

  # Get assess objects

  # Get aggregated values
  aggr_mns <-  x[["study"]][["group_means"]]
  #Correct for time increments that don't begin at 1
  aggr_mns[, "time.2.backup.var"] <- aggr_mns[, 1]
  aggr_mns[, 1] <- as.numeric(ordered(aggr_mns[, 1]))
  int_start_y <- aggr_mns[which(aggr_mns[, 1] == min(aggr_mns[, 1]) &
                                  aggr_mns[, 2] == max(aggr_mns[, 2])), 3]
  # formula type
  if(y == "DID") {
    formula_type <- "DID_formula"
  }
  if(y == "ITS") {
    formula_type <- "ITS_formula"
  }
  # outcome variable for printing
  yvar <- all.vars(as.formula(x[["formula"]][[formula_type]]))[[1]]
  xvar <- colnames(aggr_mns)[1]

  # causal models
  if(y == "DID") {
    cmodel <- x[["DID"]]
  }
  if(y == "ITS") {
    cmodel <- x[["ITS"]]
  }

  # model type
  if(y == "DID") {
    model_type <- x[["analysis_type"]][["did_type"]]
  }
  if(y == "ITS") {
    model_type <- x[["analysis_type"]][["itsa_type"]]
  }

  #Get main title
  if(y == "DID") {
    main_title <- "Differences-in-Differences"
  }
  if(y == "ITS") {
    main_title <- "Interrupted Time Series"
  }

  #############
  ## DID Two ##
  #############
  if (model_type == "two") {
    model_vars <- all.vars(x[["formula"]][["DID_formula"]])[-1] #all model X
    #Model X means
    mod_var_mn <- colMeans(cmodel[["model"]])[-which(names(colMeans(cmodel[["model"]]))==yvar)]
    main_vars <- c("Post.All", "Int.Var",  "DID") # main causal model terms
    cov_names <- setdiff(names(mod_var_mn), main_vars) # additional covariates
    cov_mn <- mod_var_mn[which(names(mod_var_mn) %in% cov_names)] # covariate means
    #coefficient * X mean
    cov_vals <- vector()
    for( i in cov_names) {
      cov_vals[i] <- cov_mn[i] * coef(cmodel)[i]
    }
    # add this to adjust the intercept for additional covariates
    if(length(cov_names) !=0) {
      B0_adjust <- sum(cov_vals)
    } else {
      B0_adjust <- 0
    }
    #Fitted values
    c0 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*0 + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0 #control group time 0
    c1 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*1 + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0 #control group time 1
    t0 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*0 + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*0   #intervention group time 0
    t1 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*1 + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*1   #intervention group time 1
    #Counterfactual for treated
    cft1 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*1 + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*0   #intervention group time 1
    #Average treatment effect on the treated
    atet1 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*1 + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*1   #intervention group time 1

    plot(0:1, range(c(cmodel[["fitted.values"]], t0)), type="n",
         main=main_title, xlab= xvar, ylab=yvar, xlim=xlim, ylim=ylim)
    #Intervention line
    abline(v=.5, col="gray", lty=3, lwd=3)
    lines(0:1, c(t0, t1), type="l", col="blue", lwd=3)
    lines(0:1, c(c0, c1), type="l", col="red", lwd=3)
    #Counter factual lines
    lines(c(0, 1), c(mean(t0, cft1), cft1), col="blue", lty=2, lwd=3)
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Control","Counterfactual", "Treated"),
             lty= c(1,1,2,3),
             lwd=1, col=c("blue","red","blue","gray"), bty="n", cex=1)
    }
  }

  ##############
  ## DID Many ##
  ##############
  if (model_type == "many") {
    model_vars <- all.vars(x[["formula"]][["DID_formula"]])[-1] #all model X
    #Model X means
    mod_var_mn <- colMeans(cmodel[["model"]])[-which(names(colMeans(cmodel[["model"]]))==yvar)]
    main_vars <- c("Period","DID","DID.Trend") # main causal model terms
    cov_names <- setdiff(names(mod_var_mn), main_vars) # additional covariates
    cov_mn <- mod_var_mn[which(names(mod_var_mn) %in% cov_names)] # covariate means
    #coefficient * X mean
    cov_vals <- vector()
    for( i in cov_names) {
      cov_vals[i] <- cov_mn[i] * coef(cmodel)[i]
    }
    # add this to adjust the intercept for additional covariates
    if(length(cov_names) !=0) {
      B0_adjust <- sum(cov_vals)
    } else {
      B0_adjust <- 0
    }

    treat_start <- x[["study"]][["treatment"]] # treatment start
    treat_start <- which(aggr_mns[, "time.2.backup.var"] == treat_start )[1]
    max_time <- max(cmodel[["model"]][["Period"]]) # max study time
    #Fitted values
    c0 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*1 + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0 #control group time 0
    c1 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*max_time + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0 #control group time 1
    t0 <- int_start_y + coef(cmodel)[[2]]*0 + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0   #intervention group time 0
    t1 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*max_time + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*max_time   #intervention group time 1
    #Counterfactual for treated
    # use int_start_y here
    cft1 <- int_start_y + coef(cmodel)[[2]]*max_time + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0   #intervention group time 1
    #Average treatment effect on the treated
    atet0 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*treat_start + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*treat_start   #intervention group time 1
    atet1 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*max_time + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*max_time   #intervention group time 1

    plot(range(aggr_mns[, 1]), range(c(cmodel[["fitted.values"]], t0)), type="n",
         main=main_title, xlab= xvar, ylab=yvar, xlim=xlim, ylim=ylim)
    #Intervention line
    abline(v=treat_start, col="gray", lty=3, lwd=3)
    lines(c(1, max_time), c(c0, c1), type="l", col="red", lwd=3)
    #Counter factual line
    lines(c(1, max_time), c(t0, cft1), type="l", col="blue", lty=2, lwd=3)
    #ATET line
    segments(x0 = treat_start, y0 = atet0, x1 = max_time, y1 = atet1, col = "blue", lwd = 3, lty=1)
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Control","Counterfactual", "Treated"),
             lty= c(1,1,2,3),
             lwd=1, col=c("blue","red","blue","gray"), bty="n", cex=1)
    }
  }

  ##########
  ## sgst ##
  ##########
  if (model_type == "sgst") {
    model_vars <- all.vars(x[["formula"]][["DID_formula"]])[-1] #all model X
    #Model X means
    mod_var_mn <- colMeans(cmodel[["model"]])[-which(names(colMeans(cmodel[["model"]]))==yvar)]
    main_vars <- x[["ITS.Names"]] # main causal model terms
    cov_names <- setdiff(names(mod_var_mn), main_vars) # additional covariates
    cov_mn <- mod_var_mn[which(names(mod_var_mn) %in% cov_names)] # covariate means
    #coefficient * X mean
    cov_vals <- vector()
    for( i in cov_names) {
      cov_vals[i] <- cov_mn[i] * coef(cmodel)[i]
    }
    # add this to adjust the intercept for additional covariates
    if(length(cov_names) !=0) {
      B0_adjust <- sum(cov_vals)
    } else {
      B0_adjust <- 0
    }

    interrupt_1 <- x[["study"]][["interrupt"]]
    interrupt_1 <- which(aggr_mns[, "time.2.backup.var"] == interrupt_1 )[1]
    max_time <- max(cmodel$model$ITS.Time)
    time_per1 <- c(1, interrupt_1-1)
    time_per2 <- c(interrupt_1, max_time)
    #Period 1's (pre-intervention) start and stop values
    t00 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*0 + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0
    t01 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per1[2] + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0
    #Period 2's (post-intervention) start and stop values
    t10 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[1] + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*0
    t11 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[2] + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*(time_per2[2]-interrupt_1)
    #Counterfactual for treated in period 2
    cft10 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[1] + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0
    cft11 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[2] + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0

    plot(range(aggr_mns[, 1]), range(c(cmodel[["fitted.values"]], t00)), type="n",
         main=main_title, xlab= xvar, ylab=yvar, xlim=xlim, ylim=ylim)
    #Intervention line
    abline(v=interrupt_1, col="gray", lty=3, lwd=3)
    lines(time_per1, c(t00, t01), lty=1, col="blue", lwd=3)
    # counterfactual
    segments(x0 = interrupt_1, y0 = cft10, x1 = time_per2[2], y1 = cft11, col = "blue", lwd = 3, lty=2)
    # intervention line
    segments(x0 = interrupt_1, y0 = t10, x1 = time_per2[2], y1 = t11, col = "blue", lwd = 3, lty=1)
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Counterfactual", "Treated"),
             lty= c(1,2,3),
             lwd=1, col=c("blue","blue","gray"), bty="n", cex=1)
    }
  }

  ##########
  ## sgmt ##
  ##########
  if (model_type == "sgmt") {
    model_vars <- all.vars(x[["formula"]][["DID_formula"]])[-1] #all model X
    #Model X means
    mod_var_mn <- colMeans(cmodel[["model"]])[-which(names(colMeans(cmodel[["model"]]))==yvar)]
    main_vars <- x[["ITS.Names"]] # main causal model terms
    cov_names <- setdiff(names(mod_var_mn), main_vars) # additional covariates
    cov_mn <- mod_var_mn[which(names(mod_var_mn) %in% cov_names)] # covariate means
    #coefficient * X mean
    cov_vals <- vector()
    for( i in cov_names) {
      cov_vals[i] <- cov_mn[i] * coef(cmodel)[i]
    }
    # add this to adjust the intercept for additional covariates
    if(length(cov_names) !=0) {
      B0_adjust <- sum(cov_vals)
    } else {
      B0_adjust <- 0
    }

    interrupt_1 <- x[["study"]][["interrupt"]][1]
    interrupt_1 <- which(aggr_mns[, "time.2.backup.var"] == interrupt_1 )[1]
    interrupt_2 <- x[["study"]][["interrupt"]][2]
    interrupt_2 <- which(aggr_mns[, "time.2.backup.var"] == interrupt_2 )[1]
    max_time <- max(cmodel$model$ITS.Time)
    time_per1 <- c(1, interrupt_1-1)
    time_per2 <- c(interrupt_1, interrupt_2-1)
    time_per3 <- c(interrupt_2, max_time)
    #Period 1's (pre-intervention) start and stop values
    t00 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*0 + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0 + coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0
    t01 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per1[2] + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0 + coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0
    #Period 2's (post-intervention) start and stop values
    t10 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[1] + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*0 + coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0
    t11 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[2] + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*(time_per2[2]-interrupt_1) + coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0
    #Period 3's (post-intervention) start and stop values
    t20 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per3[1] + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*(time_per2[2]-interrupt_1) + coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*0
    t21 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per3[2] + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*(time_per3[2]-interrupt_1) + coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*(time_per3[2]-interrupt_2)
    #Counterfactual for treated in period 2
    cft10 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[1] + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0 + coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0
    cft11 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[2] + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0  + coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0
    #Counterfactual for treated in period 3
    cft20 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per3[1] + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0 + coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0
    cft21 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per3[2] + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0  + coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0

    plot(range(aggr_mns[, 1]), range(c(cmodel[["fitted.values"]], t00)), type="n",
         main=main_title, xlab= xvar, ylab=yvar, xlim=xlim, ylim=ylim)
    #Intervention line
    abline(v= interrupt_1, col="gray", lty=3, lwd=3)
    abline(v= interrupt_2, col="gray", lty=3, lwd=3)
    lines(time_per1, c(t00, t01), lty=1, col="blue", lwd=3)
    # counterfactual in period 2
    segments(x0 = interrupt_1, y0 = cft10, x1 = time_per2[2], y1 = cft11, col = "blue", lwd = 3, lty=2)
    # counterfactual in period 3
    segments(x0 = interrupt_2, y0 = cft20, x1 = time_per3[2], y1 = cft21, col = "blue", lwd = 3, lty=2)
    # intervention line period 2
    segments(x0 = interrupt_1, y0 = t10, x1 = time_per2[2], y1 = t11, col = "blue", lwd = 3, lty=1)
    # intervention line period 3
    segments(x0 = interrupt_2, y0 = t20, x1 = time_per3[2], y1 = t21, col = "blue", lwd = 3, lty=1)
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Counterfactual", "Treated"),
             lty= c(1,2,3),
             lwd=1, col=c("blue","blue","gray"), bty="n", cex=1)
    }
  }

  ##########
  ## mgst ##
  ##########
  if (model_type == "mgst") {
    model_vars <- all.vars(x[["formula"]][["DID_formula"]])[-1] #all model X
    #Model X means
    mod_var_mn <- colMeans(cmodel[["model"]])[-which(names(colMeans(cmodel[["model"]]))==yvar)]
    main_vars <- x[["ITS.Names"]] # main causal model terms
    cov_names <- setdiff(names(mod_var_mn), main_vars) # additional covariates
    cov_mn <- mod_var_mn[which(names(mod_var_mn) %in% cov_names)] # covariate means
    #coefficient * X mean
    cov_vals <- vector()
    for( i in cov_names) {
      cov_vals[i] <- cov_mn[i] * coef(cmodel)[i]
    }
    # add this to adjust the intercept for additional covariates
    if(length(cov_names) !=0) {
      B0_adjust <- sum(cov_vals)
    } else {
      B0_adjust <- 0
    }

    interrupt_1 <- x[["study"]][["interrupt"]][1]
    interrupt_1 <- which(aggr_mns[, "time.2.backup.var"] == interrupt_1 )[1]
    max_time <- max(cmodel$model$ITS.Time)
    time_per1 <- c(1, interrupt_1-1)
    time_per2 <- c(interrupt_1, max_time)

    ## Control group
    #Period 1's (pre-intervention) start and stop values
    c00 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per1[1] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0
    c01 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per1[2] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0
    #Period 2's (post-intervention) start and stop values
    c10 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[1] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0
    c11 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[2] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*(time_per2[2]-interrupt_1) + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0

    ## Intervention group
    #Period 1's (pre-intervention) start and stop values
    t00 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per1[1] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*time_per1[1] +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0
    t01 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per1[2] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*time_per1[2] +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0
    #Period 2's (post-intervention) start and stop values
    t10 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[1] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*time_per2[1] +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*1 + coef(cmodel)[[8]]*0
    t11 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[2] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*time_per2[2] +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*(time_per2[2]-interrupt_1) + coef(cmodel)[[7]]*1 + coef(cmodel)[[8]]*(time_per2[2]-interrupt_1)

    plot(range(aggr_mns[, 1]), range(c(cmodel[["fitted.values"]], t00)), type="n",
         main=main_title, xlab= xvar, ylab=yvar, xlim=xlim, ylim=ylim)
    #Intervention line
    abline(v= interrupt_1, col="gray", lty=3, lwd=3)
    # control period 1
    lines(time_per1, c(c00, c01), lty=1, col="red", lwd=3)
    # control line period 2
    segments(x0 = interrupt_1, y0 = c10, x1 = time_per2[2], y1 = c11, col = "red", lwd = 3, lty=1)
    # intervention period 1
    lines(time_per1, c(t00, t01), lty=1, col="blue", lwd=3)
    # intervention line period 2
    segments(x0 = interrupt_1, y0 = t10, x1 = time_per2[2], y1 = t11, col = "blue", lwd = 3, lty=1)
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Control", "Treated"),
             lty= c(1,1,3),
             lwd=1, col=c("blue","red","gray"), bty="n", cex=1)
    }
  }

  ##########
  ## mgmt ##
  ##########
  if (model_type == "mgmt") {
    model_vars <- all.vars(x[["formula"]][["DID_formula"]])[-1] #all model X
    #Model X means
    mod_var_mn <- colMeans(cmodel[["model"]])[-which(names(colMeans(cmodel[["model"]]))==yvar)]
    main_vars <- x[["ITS.Names"]] # main causal model terms
    cov_names <- setdiff(names(mod_var_mn), main_vars) # additional covariates
    cov_mn <- mod_var_mn[which(names(mod_var_mn) %in% cov_names)] # covariate means
    #coefficient * X mean
    cov_vals <- vector()
    for( i in cov_names) {
      cov_vals[i] <- cov_mn[i] * coef(cmodel)[i]
    }
    # add this to adjust the intercept for additional covariates
    if(length(cov_names) !=0) {
      B0_adjust <- sum(cov_vals)
    } else {
      B0_adjust <- 0
    }

    interrupt_1 <- x[["study"]][["interrupt"]][1]
    interrupt_1 <- which(aggr_mns[, "time.2.backup.var"] == interrupt_1 )[1]
    interrupt_2 <- x[["study"]][["interrupt"]][2]
    interrupt_2 <- which(aggr_mns[, "time.2.backup.var"] == interrupt_2 )[1]
    max_time <- max(cmodel$model$ITS.Time)
    time_per1 <- c(1, interrupt_1-1)
    time_per2 <- c(interrupt_1, interrupt_2-1)
    time_per3 <- c(interrupt_2, max_time)

    ## Control group
    #Period 1's (pre-intervention) start and stop values
    c00 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per1[1] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0 +
      coef(cmodel)[[9]]*0 + coef(cmodel)[[10]]*0 + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0
    c01 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per1[2] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0 +
      coef(cmodel)[[9]]*0 + coef(cmodel)[[10]]*0 + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0
    #Period 2's (post-intervention) start and stop values
    c10 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[1] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0 +
      coef(cmodel)[[9]]*0 + coef(cmodel)[[10]]*0 + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0
    c11 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[2] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*(time_per2[2]-interrupt_1) + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0 +
      coef(cmodel)[[9]]*0 + coef(cmodel)[[10]]*0 + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0
    #Period 3's (post-intervention) start and stop values
    c20 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per3[1] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*(time_per3[1]-interrupt_1) + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0 +
      coef(cmodel)[[9]]*1 + coef(cmodel)[[10]]*0 + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0
    c21 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per3[2] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*(time_per3[2]-interrupt_1) + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0 +
      coef(cmodel)[[9]]*1 + coef(cmodel)[[10]]*(time_per3[2]-interrupt_2) + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0

    ## Intervention group
    #Period 1's (pre-intervention) start and stop values
    t00 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per1[1] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*time_per1[1] +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0 +
      coef(cmodel)[[9]]*0 + coef(cmodel)[[10]]*0 + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0
    t01 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per1[2] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*time_per1[2] +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0 +
      coef(cmodel)[[9]]*0 + coef(cmodel)[[10]]*0 + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0
    #Period 2's (post-intervention) start and stop values
    t10 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[1] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*time_per2[1] +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*1 + coef(cmodel)[[8]]*0 +
      coef(cmodel)[[9]]*0 + coef(cmodel)[[10]]*0 + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0
    t11 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[2] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*time_per2[2] +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*(time_per2[2]-interrupt_1) + coef(cmodel)[[7]]*1 + coef(cmodel)[[8]]*(time_per2[2]-interrupt_1) +
      coef(cmodel)[[9]]*0 + coef(cmodel)[[10]]*0 + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0
    #Period 3's (post-intervention) start and stop values
    t20 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per3[1] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*time_per3[1] +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*(time_per3[1]-interrupt_1) + coef(cmodel)[[7]]*1 + coef(cmodel)[[8]]*(time_per3[1]-interrupt_1) +
      coef(cmodel)[[9]]*1 + coef(cmodel)[[10]]*0 + coef(cmodel)[[11]]*1 + coef(cmodel)[[12]]*0
    t21 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per3[2] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*time_per3[2] +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*(time_per3[2]-interrupt_1) + coef(cmodel)[[7]]*1 + coef(cmodel)[[8]]*(time_per3[2]-interrupt_1) +
      coef(cmodel)[[9]]*1 + coef(cmodel)[[10]]*(time_per3[2]-interrupt_2) + coef(cmodel)[[11]]*1 + coef(cmodel)[[12]]*(time_per3[2]-interrupt_2)

    plot(range(aggr_mns[, 1]), range(c(cmodel[["fitted.values"]], t00)), type="n",
         main=main_title, xlab= xvar, ylab=yvar, xlim=xlim, ylim=ylim)
    #Intervention line
    abline(v= interrupt_1, col="gray", lty=3, lwd=3)
    abline(v= interrupt_2, col="gray", lty=3, lwd=3)
    # control period 1
    lines(time_per1, c(c00, c01), lty=1, col="red", lwd=3)
    # control line period 2
    segments(x0 = interrupt_1, y0 = c10, x1 = time_per2[2], y1 = c11, col = "red", lwd = 3, lty=1)
    # control line period 3
    segments(x0 = interrupt_2, y0 = c20, x1 = time_per3[2], y1 = c21, col = "red", lwd = 3, lty=1)
    # intervention period 1
    lines(time_per1, c(t00, t01), lty=1, col="blue", lwd=3)
    # intervention line period 2
    segments(x0 = interrupt_1, y0 = t10, x1 = time_per2[2], y1 = t11, col = "blue", lwd = 3, lty=1)
    # intervention line period 3
    segments(x0 = interrupt_2, y0 = t20, x1 = time_per3[2], y1 = t21, col = "blue", lwd = 3, lty=1)
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Control", "Treated"),
             lty= c(1,1,3),
             lwd=1, col=c("blue","red","gray"), bty="n", cex=1)
    }
  }
}

