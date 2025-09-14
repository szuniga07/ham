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
#' @param main the main title of the plot.
#' @param col specify intervention and control group colors in a vector. Defaults are c("blue", "red") or "blue" for single-group Interrupted Time Series models.
#' @param lwd select the line width.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default of 1.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex.
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex.
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex.
#' @param arrow logical TRUE or FALSE that indicates whether arrows and
#' coefficient names should be added to visualize effects. Default is FALSE.
#' @param xshift shifts one or two of some if the overlapping intervention associated arrows
#' along the x-axis for a better view. Vector values of at least length 1 or 2 can be positive
#' or negative. And xshift should be specified in the order of the coefficients. Only 1 or 2
#' of the furthest right, vertical lines for the intervention group is shifted (i.e., not left).
#' One line is shifted when there is 1 treatment/interruption period and 2 shifts for 2 periods.
#' (e.g., "DID" before "DID.Trend" for DID models with argument did="many").
#' @param add.legend add a legend by selecting the location as "bottomright", "bottom", "bottomleft",
#' "left", "topleft", "top", "topright", "right", "center". No legend if nothing selected.
#' @param ... additional arguments.
#'
#' @return plot of partial predictions for treatment and control groups.
#' @importFrom graphics lines plot legend abline segments arrows points text
#' @importFrom utils head tail
#' @export
#'
#' @examples
#' am2 <- assess(formula= los ~ ., data=hosprog, intervention = "program",
#' topcode =NULL, int.time="month", regression="none", treatment= 5,
#' interrupt=c(5,9), did="two", its="two", newdata=TRUE, propensity=NULL)
#' plot(am2, "DID", add.legend="bottomleft", ylim=c(2, 8))  #DID model
#' plot(am2, "ITS", add.legend="top", ylim=c(2, 8))         #ITS model
#' plot(am2, "DID", add.legend="topleft", main="DID study", col=c("dodgerblue","magenta"),
#' ylim=c(2, 8), lwd=6, cex=2, cex.axis=2, cex.lab=1.5, cex.main=3, arrow=TRUE, xshift=0.02)
plot.assess <- function(x, y, xlim=NULL, ylim=NULL, main=NULL, col=NULL, lwd=NULL,
                        cex=NULL, cex.axis=NULL, cex.lab=NULL, cex.main=NULL,
                        arrow=FALSE, xshift=NULL, add.legend=NULL, ...) {
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
  if(is.null(main)) {
  if(y == "DID") {
    main_title <- "Differences-in-Differences"
  }
  if(y == "ITS") {
    main_title <- "Interrupted Time Series"
  }
  } else{
    main_title <- main
  }
  #Line colors
  if(is.null(col)) {
    lcol <- c("blue", "red")
    } else {
      lcol <- col
      }
  # Cex settings cex, cex.axis, cex.lab, cex.main
  if(!is.null(cex)) {
    cex <- cex
  } else {
    cex <- NULL
    }
  if(!is.null(cex.axis)) {
    cex.axis <- cex.axis
    }
  if(!is.null(cex.lab)) {
    cex.lab <- cex.lab
    }
  if(!is.null(cex.main)) {
    cex.main <- cex.main
    }
  #Line width
  if(is.null(lwd)) {
    lwidth <- 3
  } else {
    lwidth <- lwd
  }
  #Indicate if I want to add arrows with coefficient names
  #Make CEX for intercept point
  if(!is.null(cex)) {
    arwCEX <- cex
  } else {
    arwCEX <- 1
  }
  #This will shift over the post.all arrow, left or right so it doesn't
  #collide with the counterfactual line
  if(!is.null(xshift)) {
    axshift <- xshift
  } else {
    axshift <- 0
  }
  #This gets the sign of the coefficients to assign the arrow codes
  #cmodel will work for DID and ITS models
  mdlcoefsign <- sign(coef(cmodel))[-1]
  #Change to arrow code, this leaves 0=0
  arrow_code <- mdlcoefsign
  arrow_code[arrow_code == 1] <- 2
  arrow_code[arrow_code == -1] <- 1

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
         main=main_title, xlab= xvar, ylab=yvar, xlim=xlim, ylim=ylim,
         cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main)
    #Intervention line
    abline(v=.5, col="gray", lty=3, lwd=lwidth)
    lines(0:1, c(t0, t1), type="l", col=lcol[1], lwd=lwidth)
    lines(0:1, c(c0, c1), type="l", col=lcol[2], lwd=lwidth)
    #Counter factual lines
    lines(c(0, 1), c(mean(t0, cft1), cft1), col=lcol[1], lty=2, lwd=lwidth)
    # Arrows and coefficient names #
    if (arrow == TRUE) {
      # c0 Intercept
      points(0, c0, col=lcol[2], cex=arwCEX)  # intercept: control group pre-test
      # c1 effect
      arrows(x0 = 1, y0 = c0, x1 = 1, y1 = c1, code=arrow_code[1], angle=25,
             length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # t0 effect
      arrows(x0 = 0, y0 = c0, x1 = 0, y1 = t0, code=arrow_code[2], angle=25,
             length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # t1 effect
      arrows(x0 = 1 + axshift, y0 = t1, x1 = 1 + axshift, y1 = cft1, code=arrow_code[3],
             angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # Add in coefficient names
      text(0, c0, labels ="Intercept", pos=4)  # intercept: control group pre-test
      text(1, c1, labels ="Post.All", pos=2)  # control group post-test
      text(0, t0, labels ="Int.Var", pos=4)  # intervention group pre-test
      text(1, t1, labels ="DID", pos=2)  # intervention group post-test
    }
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Control","Counterfactual", "Treated"),
             lty= c(1,1,2,3),
             lwd=1, col=c(lcol[1],lcol[2],lcol[1],"gray"), bty="n", cex=1)
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
    c0.5 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*treat_start + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0 #control group at time of the treatment start
    t0 <- int_start_y + coef(cmodel)[[2]]*1 + coef(cmodel)[[3]]*0 +
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
         main=main_title, xlab= xvar, ylab=yvar, xlim=xlim, ylim=ylim,
         cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main)
    #Intervention line
    abline(v=treat_start, col="gray", lty=3, lwd=lwidth)
    lines(c(1, max_time), c(c0, c1), type="l", col=lcol[2], lwd=lwidth)
    #Counter factual line
    lines(c(1, max_time), c(t0, cft1), type="l", col=lcol[1], lty=2, lwd=lwidth)
    #ATET line
    segments(x0 = treat_start, y0 = atet0, x1 = max_time, y1 = atet1, col = lcol[1], lwd = lwidth, lty=1)

    # Arrows and coefficient names #
    if (arrow == TRUE) {
      #Add a 0 to axshift if length == 1
      if (length(axshift) == 1) {
        axshift <- c(axshift, 0)
      }
      # c0 Intercept
      points(1, c0, col=lcol[2], cex=arwCEX)  # intercept: control group pre-test
      # c1 effect
      arrows(x0 = max_time, y0 = c0, x1 = max_time, y1 = c1, code=arrow_code[1],
             angle=25, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # DID effect
      arrows(x0 = treat_start + axshift[1], y0 = c0.5, x1 = treat_start + head(axshift, 1),
             y1 = atet0, code=arrow_code[2], angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # DID.Trend effect
      arrows(x0 = max_time + axshift[2], y0 = atet1, x1 = max_time + tail(axshift, 1), y1 = atet0,
             code=arrow_code[3], angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # Add in coefficient names
      text(1, c0, labels ="Intercept", pos=4)
      text(max_time, c1, labels ="Period", pos=2)
      text(treat_start, t0, labels ="DID", pos= ((mdlcoefsign[2]-3)*-1) -1) # how to place text
      text(max_time, atet0, labels ="DID.Trend", pos=2)
    }
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Control","Counterfactual", "Treated"),
             lty= c(1,1,2,3),
             lwd=1, col=c(lcol[1],lcol[2],lcol[1],"gray"), bty="n", cex=1)
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
    t00 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*1 + coef(cmodel)[[3]]*0 +
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
         main=main_title, xlab= xvar, ylab=yvar, xlim=xlim, ylim=ylim,
         cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main)
    #Intervention line
    abline(v=interrupt_1, col="gray", lty=3, lwd=lwidth)
    lines(time_per1, c(t00, t01), lty=1, col=lcol[1], lwd=lwidth)
    # counterfactual
    segments(x0 = interrupt_1, y0 = cft10, x1 = time_per2[2], y1 = cft11, col = lcol[1], lwd = lwidth, lty=2)
    # intervention line
    segments(x0 = interrupt_1, y0 = t10, x1 = time_per2[2], y1 = t11, col = lcol[1], lwd = lwidth, lty=1)
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Counterfactual", "Treated"),
             lty= c(1,2,3),
             lwd=1, col=c(lcol[1],lcol[1],"gray"), bty="n", cex=1)
    }
    # Arrows and coefficient names #
    if (arrow == TRUE) {
      ################
      ## 1st Period ##
      ################
      timemidp1 <- mean(time_per1)
      timeqtrp1 <- (timemidp1/2) + 0.5  #start point of arrow, add 0.5 b/c no 0 in X
      #ITS y-axis
      ## Time 1, Pre-intervention ##
      #What to do if the treatment group has the highest pre-intervention scores
      #ITS.int variable name position
      posIntercept <- 1
      #treatment group
      if(t00 >= t01) {
        time1thi <- t00
      } else {
        time1thi <- t01
      }
      ################
      ## 2nd Period ##
      ################
      timemidp2 <- mean(time_per2)
      timeqtrp2 <- mean(c(time_per2[1], timemidp2)) #start point of 2nd arrow
      #ITS y-axis
      ## Time 1, Pre-intervention ##
      #What to do if the treatment group has the highest pre-intervention scores
      #treatment group
      if(t10 >= t11) {
        time2thi <- t10
      } else {
        time2thi <- t11
      }
      ## Period 1 ##
      # Intercept
      points(1, t00, col=lcol[1], cex=arwCEX)  # intercept: control group pre-test
      # ITS.Time
      arrows(x0 = timeqtrp1, y0 = time1thi, x1 = timemidp1, y1 = time1thi, code=2,
             angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## Period 2 ##
      # post1
      arrows(x0 = time_per2[1] + axshift[1], y0 = cft10, x1 = time_per2[1] + axshift[1], y1 = t10, code=2,
             angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # txp1
      arrows(x0 = timeqtrp2, y0 = time2thi, x1 = timemidp2, y1 = time2thi, code=2,
             angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)

      # Add in coefficient names
      # Period 1
      # Intercept
      text(1, t00, labels ="Intercept", pos=posIntercept)
      # ITS.Time
      text(timemidp1, time1thi, labels ="ITS.Time", pos=4)
      # Period 2
      # post1
      text(time_per2[1] + axshift[1], mean(c(t10, cft10)), labels ="post1", pos=2)
      # txp1
      text(timemidp2, time2thi, labels ="txp1", pos=4)
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
    t00 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*1 + coef(cmodel)[[3]]*0 +
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
         main=main_title, xlab= xvar, ylab=yvar, xlim=xlim, ylim=ylim,
         cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main)
    #Intervention line
    abline(v= interrupt_1, col="gray", lty=3, lwd=lwidth)
    abline(v= interrupt_2, col="gray", lty=3, lwd=lwidth)
    lines(time_per1, c(t00, t01), lty=1, col=lcol[1], lwd=lwidth)
    # counterfactual in period 2
    segments(x0 = interrupt_1, y0 = cft10, x1 = time_per2[2], y1 = cft11, col = lcol[1], lwd = lwidth, lty=2)
    # counterfactual in period 3
    segments(x0 = interrupt_2, y0 = cft20, x1 = time_per3[2], y1 = cft21, col = lcol[1], lwd = lwidth, lty=2)
    # intervention line period 2
    segments(x0 = interrupt_1, y0 = t10, x1 = time_per2[2], y1 = t11, col = lcol[1], lwd = lwidth, lty=1)
    # intervention line period 3
    segments(x0 = interrupt_2, y0 = t20, x1 = time_per3[2], y1 = t21, col = lcol[1], lwd = lwidth, lty=1)
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Counterfactual", "Treated"),
             lty= c(1,2,3),
             lwd=1, col=c(lcol[1],lcol[1],"gray"), bty="n", cex=1)
    }
    # Arrows and coefficient names #
    if (arrow == TRUE) {
      #Add a 0 to axshift if length == 1
      if (length(axshift) == 1) {
        axshift <- c(axshift, 0)
      }
      ################
      ## 1st Period ##
      ################
      timemidp1 <- mean(time_per1)
      timeqtrp1 <- (timemidp1/2) + 0.5  #start point of arrow, add 0.5 b/c no 0 in X
      #ITS y-axis
      ## Time 1, Pre-intervention ##
      #What to do if the treatment group has the highest pre-intervention scores
      #ITS.int variable name position
      posIntercept <- 1
      #treatment group
      if(t00 >= t01) {
        time1thi <- t00
      } else {
        time1thi <- t01
      }
      ################
      ## 2nd Period ##
      ################
      timemidp2 <- mean(time_per2)
      timeqtrp2 <- mean(c(time_per2[1], timemidp2)) #start point of 2nd arrow
      #ITS y-axis
      ## Time 1, Pre-intervention ##
      #What to do if the treatment group has the highest pre-intervention scores
      #treatment group
      if(t10 >= t11) {
        time2thi <- t10
      } else {
        time2thi <- t11
      }
      ################
      ## 3rd Period ##
      ################
      timemidp3 <- mean(time_per3)
      timeqtrp3 <- mean(c(time_per3[1], timemidp3)) #start point of 3rd arrow
      #ITS y-axis
      ## Time 1, Pre-intervention ##
      #What to do if the treatment group has the highest pre-intervention scores
      #treatment group
      if(t10 >= t11) {
        time3thi <- t20
      } else {
        time3thi <- t21
      }
      ## Period 1 ##
      # Intercept
      points(1, t00, col=lcol[1], cex=arwCEX)  # intercept: control group pre-test
      # ITS.Time
      arrows(x0 = timeqtrp1, y0 = time1thi, x1 = timemidp1, y1 = time1thi, code=2,
             angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## Period 2 ##
      # post1
      arrows(x0 = time_per2[1] + axshift[1], y0 = cft10, x1 = time_per2[1] + axshift[1],
             y1 = t10, code=2, angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # txp1
      arrows(x0 = timeqtrp2, y0 = time2thi, x1 = timemidp2, y1 = time2thi, code=2,
             angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## Period 3 ##
      # post2
      arrows(x0 = time_per3[1] + axshift[2], y0 = cft20, x1 = time_per3[1] + axshift[2],
             y1 = t20, code=2, angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # txp2
      arrows(x0 = timeqtrp3, y0 = time3thi, x1 = timemidp3, y1 = time3thi, code=2,
             angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)

      # Add in coefficient names
      # Period 1
      # Intercept
      text(1, t00, labels ="Intercept", pos=posIntercept)
      # ITS.Time
      text(timemidp1, time1thi, labels ="ITS.Time", pos=4)
      # Period 2
      # post1
      text(time_per2[1] + axshift[1], mean(c(t10, cft10)), labels ="post1", pos=2)
      # txp1
      text(timemidp2, time2thi, labels ="txp1", pos=4)
      # Period 3
      # post2
      text(time_per3[1] + axshift[2], mean(c(t20, cft20)), labels ="post2", pos=2)
      # txp2
      text(timemidp3, time3thi, labels ="txp2", pos=4)
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
    c00 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per1[1] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0
    c01 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per1[2] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0
    #Period 2's (post-intervention) start and stop values
    c10 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[1] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0
    c11 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[2] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*(time_per2[2]-interrupt_1) + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0
    # Control group counterfactual for post1
    cfc10 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[1] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0
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
         main=main_title, xlab= xvar, ylab=yvar, xlim=xlim, ylim=ylim,
         cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main)
    #Intervention line
    abline(v= interrupt_1, col="gray", lty=3, lwd=lwidth)
    # control period 1
    lines(time_per1, c(c00, c01), lty=1, col=lcol[2], lwd=lwidth)
    # control line period 2
    segments(x0 = interrupt_1, y0 = c10, x1 = time_per2[2], y1 = c11, col = lcol[2], lwd = lwidth, lty=1)
    # intervention period 1
    lines(time_per1, c(t00, t01), lty=1, col=lcol[1], lwd=lwidth)
    # intervention line period 2
    segments(x0 = interrupt_1, y0 = t10, x1 = time_per2[2], y1 = t11, col = lcol[1], lwd = lwidth, lty=1)
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Control", "Treated"),
             lty= c(1,1,3),
             lwd=1, col=c(lcol[1],lcol[2],"gray"), bty="n", cex=1)
    }
    # Arrows and coefficient names #
    if (arrow == TRUE) {
      ################
      ## 1st Period ##
      ################
      timemidp1 <- mean(time_per1)
      timeqtrp1 <- (timemidp1/2) + 0.5  #start point of arrow, add 0.5 b/c no 0 in X
      #ITS y-axis
      ## Time 1, Pre-intervention ##
      #What to do if the treatment group has the highest pre-intervention scores
      if(t00 >= c00) {
        #ITS.int variable name position
        posITS.int <- 3
        posIntercept <- 1
        #treatment group
        if(t00 >= t01) {
          time1thi <- t00
        } else {
          time1thi <- t01
        }
        # control
        if(c00 >= c01) {
          time1chi <- c01
        } else {
          time1chi <- c00
        }
      }
      #What to do if the control group has the highest pre-intervention scores
      if(c00 >= t00) {
        #ITS.int variable name position
        posIntercept <- 3
        posITS.int <- 1
        #treatment group
        if(t00 >= t01) {
          time1thi <- t01
        } else {
          time1thi <- t00
        }
        # control
        if(c00 >= c01) {
          time1chi <- c00
        } else {
          time1chi <- c01
        }
      }
      ################
      ## 2nd Period ##
      ################
      timemidp2 <- mean(time_per2)
      timeqtrp2 <- mean(c(time_per2[1], timemidp2)) #start point of 2nd arrow
      #ITS y-axis
      ## Time 1, Pre-intervention ##
      #What to do if the treatment group has the highest pre-intervention scores
      if(t10 >= c10) {
        #treatment group
        if(t10 >= t11) {
          time2thi <- t10
        } else {
          time2thi <- t11
        }
        # control
        if(c10 >= c11) {
          time2chi <- c11
        } else {
          time2chi <- c10
        }
      }
      #What to do if the control group has the highest pre-intervention scores
      if(c10 >= t10) {
        #treatment group
        if(t10 >= t11) {
          time2thi <- t11
        } else {
          time2thi <- t10
        }
        # control
        if(c10 >= c11) {
          time2chi <- c10
        } else {
          time2chi <- c11
        }
      }
      ## Period 1 ##
      # Intercept
      points(1, c00, col=lcol[2], cex=arwCEX)  # intercept: control group pre-test
      # ITS.Time
      arrows(x0 = timeqtrp1, y0 = time1chi, x1 = timemidp1, y1 = time1chi, code=2,
             angle=25, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # ITS.Int
      arrows(x0 = 1, y0 = c00, x1 = 1, y1 = t00, code=arrow_code[2],  #need arrow, vertical line
             angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # txi
      arrows(x0 = timeqtrp1, y0 = time1thi, x1 = timemidp1, y1 = time1thi, code=2,
             angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## Period 2 ##
      # post1
      arrows(x0 = time_per2[1], y0 = cfc10, x1 = time_per2[1], y1 = c10, code=2,
             angle=25, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # txp1
      arrows(x0 = timeqtrp2, y0 = time2chi, x1 = timemidp2, y1 = time2chi, code=2,
             angle=25, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # ixp1
      arrows(x0 = time_per2[1] + axshift[1], y0 = t10, x1 = time_per2[1] + axshift[1],
             y1 = c10, code=3, angle=25, length=.25, col = lcol[1],
             lwd = arwCEX, lty=3)
      # txip1
      arrows(x0 = timeqtrp2, y0 = time2thi, x1 = timemidp2, y1 = time2thi,
             code=2, angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)

      # Add in coefficient names
      # Period 1
      # Intercept
      text(1, c00, labels ="Intercept", pos=posIntercept)
      # ITS.Time
      text(timemidp1, time1chi, labels ="ITS.Time", pos=4)
      # ITS.Int
      text(1, t00, labels ="ITS.Int", pos=posITS.int)
      # txi
      text(timemidp1, time1thi, labels ="txi", pos=4)
      # Period 2
      # post1
      text(time_per2[1], mean(c(c10, cfc10)), labels ="post1", pos=2)
      # txp1
      text(timemidp2, time2chi, labels ="txp1", pos=4)
      # ixp1
      text(time_per2[1] + axshift[1], mean(c(c10, t10)), labels ="ixp1", pos=4)
      # txip1
      text(timemidp2, time2thi, labels ="txip1", pos=4)
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
    c00 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per1[1] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
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
    # Control group counterfactual for post1
    cfc10 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[1] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0 +
      coef(cmodel)[[9]]*0 + coef(cmodel)[[10]]*0 + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0
    #Period 3's (post-intervention) start and stop values
    c20 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per3[1] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*(time_per3[1]-interrupt_1) + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0 +
      coef(cmodel)[[9]]*1 + coef(cmodel)[[10]]*0 + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0
    c21 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per3[2] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*(time_per3[2]-interrupt_1) + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0 +
      coef(cmodel)[[9]]*1 + coef(cmodel)[[10]]*(time_per3[2]-interrupt_2) + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0
    # Control group counterfactual for post2
    cfc20 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per3[1] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
      coef(cmodel)[[5]]*1 + coef(cmodel)[[6]]*(time_per3[1]-interrupt_1) + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0 +
      coef(cmodel)[[9]]*0 + coef(cmodel)[[10]]*0 + coef(cmodel)[[11]]*0 + coef(cmodel)[[12]]*0

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
         main=main_title, xlab= xvar, ylab=yvar, xlim=xlim, ylim=ylim,
         cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main)
    #Intervention line
    abline(v= interrupt_1, col="gray", lty=3, lwd=lwidth)
    abline(v= interrupt_2, col="gray", lty=3, lwd=lwidth)
    # control period 1
    lines(time_per1, c(c00, c01), lty=1, col=lcol[2], lwd=lwidth)
    # control line period 2
    segments(x0 = interrupt_1, y0 = c10, x1 = time_per2[2], y1 = c11, col = lcol[2], lwd = lwidth, lty=1)
    # control line period 3
    segments(x0 = interrupt_2, y0 = c20, x1 = time_per3[2], y1 = c21, col = lcol[2], lwd = lwidth, lty=1)
    # intervention period 1
    lines(time_per1, c(t00, t01), lty=1, col=lcol[1], lwd=lwidth)
    # intervention line period 2
    segments(x0 = interrupt_1, y0 = t10, x1 = time_per2[2], y1 = t11, col = lcol[1], lwd = lwidth, lty=1)
    # intervention line period 3
    segments(x0 = interrupt_2, y0 = t20, x1 = time_per3[2], y1 = t21, col = lcol[1], lwd = lwidth, lty=1)
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Control", "Treated"),
             lty= c(1,1,3),
             lwd=1, col=c(lcol[1],lcol[2],"gray"), bty="n", cex=1)
    }
    # Arrows and coefficient names #
    if (arrow == TRUE) {
      #Add a 0 to axshift if length == 1
      if (length(axshift) == 1) {
        axshift <- c(axshift, 0)
      }
      ################
      ## 1st Period ##
      ################
      timemidp1 <- mean(time_per1)
      timeqtrp1 <- (timemidp1/2) + 0.5  #start point of arrow, add 0.5 b/c no 0 in X
      #ITS y-axis
      ## Time 1, Pre-intervention ##
      #What to do if the treatment group has the highest pre-intervention scores
      if(t00 >= c00) {
        #ITS.int variable name position
        posITS.int <- 3
        posIntercept <- 1
        #treatment group
        if(t00 >= t01) {
          time1thi <- t00
        } else {
          time1thi <- t01
        }
        # control
        if(c00 >= c01) {
          time1chi <- c01
        } else {
          time1chi <- c00
        }
      }
      #What to do if the control group has the highest pre-intervention scores
      if(c00 >= t00) {
        #ITS.int variable name position
        posIntercept <- 3
        posITS.int <- 1
        #treatment group
        if(t00 >= t01) {
          time1thi <- t01
        } else {
          time1thi <- t00
        }
        # control
        if(c00 >= c01) {
          time1chi <- c00
        } else {
          time1chi <- c01
        }
      }
      ################
      ## 2nd Period ##
      ################
      timemidp2 <- mean(time_per2)
      timeqtrp2 <- mean(c(time_per2[1], timemidp2)) #start point of 2nd arrow
      #ITS y-axis
      ## Time 1, Pre-intervention ##
      #What to do if the treatment group has the highest pre-intervention scores
      if(t10 >= c10) {
        #treatment group
        if(t10 >= t11) {
          time2thi <- t10
        } else {
          time2thi <- t11
        }
        # control
        if(c10 >= c11) {
          time2chi <- c11
        } else {
          time2chi <- c10
        }
      }
      #What to do if the control group has the highest pre-intervention scores
      if(c10 >= t10) {
        #treatment group
        if(t10 >= t11) {
          time2thi <- t11
        } else {
          time2thi <- t10
        }
        # control
        if(c10 >= c11) {
          time2chi <- c10
        } else {
          time2chi <- c11
        }
      }
      ################
      ## 3rd Period ##
      ################
      timemidp3 <- mean(time_per3)
      timeqtrp3 <- mean(c(time_per3[1], timemidp3)) #start point of 3rd arrow
      #ITS y-axis
      ## Time 1, Pre-intervention ##
      #What to do if the treatment group has the highest pre-intervention scores
      if(t20 >= c20) {
        #treatment group
        if(t10 >= t11) {
          time3thi <- t20
        } else {
          time3thi <- t21
        }
        # control
        if(c20 >= c21) {
          time3chi <- c21
        } else {
          time3chi <- c20
        }
      }
      #What to do if the control group has the highest pre-intervention scores
      if(c20 >= t20) {
        #treatment group
        if(t20 >= t21) {
          time3thi <- t21
        } else {
          time3thi <- t20
        }
        # control
        if(c20 >= c21) {
          time3chi <- c20
        } else {
          time3chi <- c21
        }
      }
      ## Period 1 ##
      # Intercept
      points(1, c00, col=lcol[2], cex=arwCEX)  # intercept: control group pre-test
      # ITS.Time
      arrows(x0 = timeqtrp1, y0 = time1chi, x1 = timemidp1, y1 = time1chi, code=2,
             angle=25, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # ITS.Int
      arrows(x0 = 1, y0 = c00, x1 = 1, y1 = t00, code=arrow_code[2],  #need arrow, vertical line
             angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # txi
      arrows(x0 = timeqtrp1, y0 = time1thi, x1 = timemidp1, y1 = time1thi, code=2,
             angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## Period 2 ##
      # post1
      arrows(x0 = time_per2[1], y0 = cfc10, x1 = time_per2[1], y1 = c10, code=2,
             angle=25, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # txp1
      arrows(x0 = timeqtrp2, y0 = time2chi, x1 = timemidp2, y1 = time2chi, code=2,
             angle=25, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # ixp1
      arrows(x0 = time_per2[1] + axshift[1], y0 = t10, x1 = time_per2[1] + axshift[1],
             y1 = c10, code=3, angle=25, length=.25, col = lcol[1],
             lwd = arwCEX, lty=3)
      # txip1
      arrows(x0 = timeqtrp2, y0 = time2thi, x1 = timemidp2, y1 = time2thi,
             code=2, angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## Period 3 ##
      # post2
      arrows(x0 = time_per3[1], y0 = cfc20, x1 = time_per3[1], y1 = c20, code=2,
             angle=25, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # txp2
      arrows(x0 = timeqtrp3, y0 = time3chi, x1 = timemidp3, y1 = time3chi, code=2,
             angle=25, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # ixp2
      arrows(x0 = time_per3[1] + axshift[2], y0 = t20, x1 = time_per3[1] + axshift[2],
             y1 = c20, code=3, angle=25, length=.25, col = lcol[1],
             lwd = arwCEX, lty=3)
      # txip2
      arrows(x0 = timeqtrp3, y0 = time3thi, x1 = timemidp3, y1 = time3thi,
             code=2, angle=25, length=.25, col = lcol[1], lwd = arwCEX, lty=3)

      # Add in coefficient names
      # Period 1
      # Intercept
      text(1, c00, labels ="Intercept", pos=posIntercept)
      # ITS.Time
      text(timemidp1, time1chi, labels ="ITS.Time", pos=4)
      # ITS.Int
      text(1, t00, labels ="ITS.Int", pos=posITS.int)
      # txi
      text(timemidp1, time1thi, labels ="txi", pos=4)
      # Period 2
      # post1
      text(time_per2[1], mean(c(c10, cfc10)), labels ="post1", pos=2)
      # txp1
      text(timemidp2, time2chi, labels ="txp1", pos=4)
      # ixp1
      text(time_per2[1] + axshift[1], mean(c(c10, t10)), labels ="ixp1", pos=4)
      # txip1
      text(timemidp2, time2thi, labels ="txip1", pos=4)
      # Period 3
      # post2
      text(time_per3[1], mean(c(c20, cfc20)), labels ="post2", pos=2)
      # txp2
      text(timemidp3, time3chi, labels ="txp2", pos=4)
      # ixp2
      text(time_per3[1] + axshift[2], mean(c(c20, t20)), labels ="ixp2", pos=4)
      # txip2
      text(timemidp3, time3thi, labels ="txip2", pos=4)
    }
  }
}

