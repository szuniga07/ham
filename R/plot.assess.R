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
#' @param cex.text The magnification to be used for the text added into the plot relative to the current setting of 1.
#' @param cex.legend The magnification to be used for the legend added into the plot relative to the current setting of 1.
#' @param arrow logical TRUE or FALSE that indicates whether arrows and
#' coefficient names should be added to visualize effects. Default is FALSE.
#' @param xshift shifts one or two of some if the overlapping intervention associated arrows
#' along the x-axis for a better view. Vector values of at least length 1 or 2 can be positive
#' or negative. And xshift should be specified in the order of the coefficients. Only 1 or 2
#' of the furthest right, vertical lines for the intervention group is shifted (i.e., not left).
#' One line is shifted when there is 1 treatment/interruption period and 2 shifts for 2 periods.
#' (e.g., "DID" before "DID.Trend" for DID models with argument did="many").
#' @param coefs if 'arrow' is TRUE, logical TRUE or FALSE that indicates whether coefficient values
#' and p-value significance symbols ('+' p<0.10; '*' p<0.05; '**' p<0.01; '***' p<0.001) should be
#' added to the plot. Default is FALSE.
#' @param round.c if 'arrow' and 'coefs' are TRUE, an integer indicating the number of decimal places
#' to be used for rounding coefficient values.
#' @param pos.text if 'arrow' and 'coefs' are TRUE, a list of named integer value(s) between 1 to 4 indicating
#' the position of the text added into the plot. List name(s) should use existing generic time references
#' (e.g., "post1" and "post2").
#' @param add.legend add a legend by selecting the location as "bottomright", "bottom", "bottomleft",
#' "left", "topleft", "top", "topright", "right", "center". No legend if nothing selected.
#' @param ... additional arguments.
#'
#' @return plot of partial predictions for treatment and control groups.
#' @importFrom graphics lines plot legend abline segments arrows points text
#' @importFrom utils head tail
#' @importFrom methods is
#' @export
#'
#' @examples
#' am2 <- assess(formula= los ~ ., data=hosprog, intervention = "program",
#' topcode =NULL, int.time="month", regression="none", treatment= 5,
#' interrupt=c(5,9), did="two", its="two", newdata=TRUE, propensity=NULL)
#' plot(am2, "DID", add.legend="bottomleft", ylim=c(2, 8))  #DID model, basic plot
#' plot(am2, "ITS", add.legend="top", ylim=c(2, 8))         #ITS model, basic plot
#' plot(am2, "DID", add.legend="topleft", main="DID study", col=c("dodgerblue","magenta"),
#' ylim=c(2, 8), lwd=6, cex=3, cex.axis=2, cex.lab=1.5, cex.main=3, cex.text=2,
#' arrow=TRUE, xshift=0.02, coefs=TRUE, round.c=2 )
#' plot(am2, "ITS", add.legend="top", xlim=c(-.5, 13), ylim=c(2, 8), main="ITS study",
#' col=c("cyan","hotpink"), lwd=7, arrow=TRUE, xshift=c(.5, 1.5), cex=2, cex.axis=2, cex.lab=2,
#' cex.main=3, cex.text=1.2, cex.legend=1.25, coefs=TRUE, round.c=1, pos.text= list("txp1"=3,
#' "post2"=4))
plot.assess <- function(x, y, xlim=NULL, ylim=NULL, main=NULL, col=NULL, lwd=NULL,
                        cex=NULL, cex.axis=NULL, cex.lab=NULL, cex.main=NULL, cex.text=NULL,
                        cex.legend=NULL, arrow=FALSE, xshift=NULL, coefs=FALSE, round.c=NULL,
                        pos.text=NULL, add.legend=NULL, ...) {
  if(any(is.null(c(x, y)) == TRUE)) {
    stop("Error: Expecting both an x and y argument.")
  }
  if (any(class(x) == "assess") == FALSE) {stop("Error: Expecting assess class object." )}
  if (!y %in% c("DID", "ITS")) {stop("Error: Expecting y='DID' or y='ITS'." )}
  if(!is.null(pos.text)) {
    if(is(pos.text, "list") == FALSE) {
      stop("Error: Expecting a list for pos.text")
    }
  }

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

  #Model summary p-values
  model_summary_p <- summary(cmodel)$coefficients[, "Pr(>|t|)"]
  model_summary <- model_summary_p #Need this when indexing, above becomes a character
  for(i in 1:length(model_summary_p)) {
    model_summary_p[i][model_summary[i] < 0.10] <- "+"
    model_summary_p[i][model_summary[i] < 0.05] <- "*"
    model_summary_p[i][model_summary[i] < 0.01] <- "**"
    model_summary_p[i][model_summary[i] < 0.001] <- "***"
    model_summary_p[i][model_summary[i] >= 0.10] <- ""
  }

  # model type
  if(y == "DID") {
    model_type <- x[["analysis_type"]][["did_type"]]
  }
  if(y == "ITS") {
    model_type <- x[["analysis_type"]][["itsa_type"]]
  }

  #Check for correct pos.text variable names
  if (model_type == "two") {
    if (!is.null(pos.text)) {
      if (all(names(pos.text) %in% c("Intercept",'Post.All', 'Int.Var', 'DID')) == FALSE) {
        stop("Error: Expecting pos.text variable names as 'Intercept', 'Period','Post.All', 'Int.Var', and/or 'DID'.")
      }
    }
  }
  if (model_type == "many") {
    if (!is.null(pos.text)) {
      if (all(names(pos.text) %in% c("Intercept","Period","DID","DID.Trend")) == FALSE) {
        stop("Error: Expecting pos.text variable names as 'Intercept', 'Period', 'DID', and/or 'DID.Trend'.")
      }
    }
  }
  if (model_type == "sgst") {
    if (!is.null(pos.text)) {
      if (all(names(pos.text) %in% c('Intercept','ITS.Time', 'post1', 'txp1')) == FALSE) {
        stop("Error: Expecting pos.text variable names as 'Intercept', 'ITS.Time', 'post1', and/or 'txp1'.")
      }
    }
  }
  if (model_type == "sgmt") {
    if (!is.null(pos.text)) {
      if (all(names(pos.text) %in% c('Intercept','ITS.Time', 'post1', 'txp1', 'post2','txp2')) == FALSE) {
        stop("Error: Expecting pos.text variable names as 'Intercept', 'ITS.Time', 'post1', 'txp1', 'post2', and/or 'txp2'.")
      }
    }
  }
  if (model_type == "mgst") {
    if (!is.null(pos.text)) {
      if (all(names(pos.text) %in% c('Intercept', 'ITS.Time', 'ITS.Int', 'txi', 'post1', 'txp1', 'ixp1', 'txip1')) == FALSE) {
        stop("Error: Expecting pos.text variable names as 'Intercept', 'ITS.Time', 'ITS.Int', 'txi', 'post1', 'txp1', 'ixp1', and/or 'txip1'.")
      }
    }
  }
  if (model_type == "mgmt") {
    if (!is.null(pos.text)) {
      if (all(names(pos.text) %in% c('Intercept', 'ITS.Time', 'ITS.Int', 'txi', 'post1', 'txp1', 'ixp1', 'txip1', 'post2', 'txp2', 'ixp2', 'txip2')) == FALSE) {
        stop("Error: Expecting pos.text variable names as 'Intercept', 'ITS.Time', 'ITS.Int', 'txi', 'post1', 'txp1', 'ixp1', 'txip1', 'post2', 'txp2', 'ixp2', and/or 'txip2'.")
      }
    }
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
  #Make CEX for text added into the plot (i.e, text())
  if(!is.null(cex)) {
    textCEX <- cex.text
  } else {
    textCEX <- 1
  }
  #Make default round= 2 for text added into the plot (i.e, text())
  if(!is.null(round.c)) {
    round.c <- round.c
  } else {
    round.c <- 2
  }
  #Make CEX for legend
  if(!is.null(cex.legend)) {
    cex.legend <- cex.legend
  } else {
    cex.legend <- 1
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
      # c1 effect Post.All
      arrows(x0 = 1, y0 = c0, x1 = 1, y1 = c1, code=2, #code=arrow_code[1],
             angle=30, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # t0 effect
      arrows(x0 = 0, y0 = c0, x1 = 0, y1 = t0, code=2, #code=arrow_code[2],
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # t1 effect
      arrows(x0 = 1 + axshift, y0 = cft1, x1 = 1 + axshift, y1 = t1, code=2, #code=arrow_code[3],
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## text positions ##
      postwo <- list("Intercept"=4, "Post.All"=2, "Int.Var"=4,  "DID"=2)
      #User submitted cpos
      makechange <- pos.text
      # identifies which elements to change in original positions
      newposlist <- vector()
      if(!is.null(pos.text)) {
        for (i in 1:length(names(makechange))) {
          newposlist[i] <- grep(paste0("^", names(makechange)[i], "$"), names(postwo))
        }
      }
      # changes values
      if(!is.null(pos.text)) {
        for (i in 1:length(newposlist)) {
          postwo[[newposlist[i]]] <- makechange[[i]]
        }
      } else {
        postwo <- postwo
      }

      # Add in coefficient names
      # intercept: control group pre-test
      text(0, c0, labels = if(coefs == TRUE) paste0("Intercept= ", round(coef(cmodel)[1], round.c), model_summary_p[1]) else "Intercept", pos=postwo[[1]], cex=textCEX)
      # control group post-test
      text(1, c1, labels =if(coefs == TRUE) paste0("Post.All= ", round(coef(cmodel)[2], round.c), model_summary_p[2]) else "Post.All", pos=postwo[[2]], cex=textCEX)
      # intervention group pre-test
      text(0, t0, labels =if(coefs == TRUE) paste0("Int.Var= ", round(coef(cmodel)[3], round.c), model_summary_p[3]) else "Int.Var", pos=postwo[[3]], cex=textCEX)
      # intervention group post-test
      text(1, t1, labels =if(coefs == TRUE) paste0("DID= ", round(coef(cmodel)[4], round.c), model_summary_p[4]) else "DID", pos=postwo[[4]], cex=textCEX)
    }
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Control","Counterfactual", "Treated"),
             lty= c(1,1,2,3),
             lwd=cex.legend, col=c(lcol[1],lcol[2],lcol[1],"gray"), bty="n", cex=cex.legend)
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
      # c1 effect Period
      arrows(x0 = max_time, y0 = c0, x1 = max_time, y1 = c1, code=2, #code=arrow_code[1],
             angle=30, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # DID effect
      arrows(x0 = treat_start + axshift[1], y0 = c0.5, x1 = treat_start + axshift[1], y1 = atet0,
             code=2, angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3) #code=arrow_code[2]
      # DID.Trend effect
      arrows(x0 = max_time + axshift[2], y0 = atet0, x1 = max_time + axshift[2],
             y1 = atet1, code=2, #code=arrow_code[3],
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)

      ## text positions ##
      posmany <- list("Intercept"=4, "Period"=2, "DID"=((mdlcoefsign[2]-3)*-1) -1,"DID.Trend"= 2)
#User submitted cpos
makechange <- pos.text
# identifies which elements to change in original positions
newposlist <- vector()
if(!is.null(pos.text)) {
  for (i in 1:length(names(makechange))) {
    newposlist[i] <- grep(paste0("^", names(makechange)[i], "$"), names(posmany))
  }
}
# changes values
if(!is.null(pos.text)) {
  for (i in 1:length(newposlist)) {
    posmany[[newposlist[i]]] <- makechange[[i]]
  }
} else {
  posmany <- posmany
}

      # Add in coefficient names
      text(1, c0, labels = if(coefs == TRUE) paste0("Intercept= ", round(coef(cmodel)[1], round.c), model_summary_p[1]) else "Intercept", pos=posmany[[1]], cex=textCEX)
      text(max_time, c1, labels =if(coefs == TRUE) paste0("Period= ", round(coef(cmodel)[2], round.c), model_summary_p[2]) else "Period", pos=posmany[[2]], cex=textCEX)
      text(treat_start, t0, labels =if(coefs == TRUE) paste0("DID= ", round(coef(cmodel)[3], round.c), model_summary_p[3]) else "DID",
           pos= posmany[[3]], cex=textCEX) # how to place text
      text(max_time, atet0, labels =if(coefs == TRUE) paste0("DID.Trend= ", round(coef(cmodel)[4], round.c), model_summary_p[4]) else "DID.Trend", pos=posmany[[4]], cex=textCEX)
    }
    if (!is.null(add.legend)) {
      legend(x=add.legend, legend=c("Intervention", "Control","Counterfactual", "Treated"),
             lty= c(1,1,2,3),
             lwd=cex.legend, col=c(lcol[1],lcol[2],lcol[1],"gray"), bty="n", cex=cex.legend)
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
             lwd=cex.legend, col=c(lcol[1],lcol[1],"gray"), bty="n", cex=cex.legend)
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
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## Period 2 ##
      # post1
      arrows(x0 = time_per2[1] + axshift[1], y0 = cft10, x1 = time_per2[1] + axshift[1], y1 = t10, code=2,
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # txp1
      arrows(x0 = timeqtrp2, y0 = time2thi, x1 = timemidp2, y1 = time2thi, code=2,
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)

      ## text positions ##
      possgst <- list("Intercept"=posIntercept, "ITS.Time"=4, "post1"=2, "txp1"=4)
      #User submitted cpos
      makechange <- pos.text
      # identifies which elements to change in original positions
      newposlist <- vector()
      if(!is.null(pos.text)) {
        for (i in 1:length(names(makechange))) {
          newposlist[i] <- grep(paste0("^", names(makechange)[i], "$"), names(possgst))
        }
      }
      # changes values
      if(!is.null(pos.text)) {
        for (i in 1:length(newposlist)) {
          possgst[[newposlist[i]]] <- makechange[[i]]
        }
      } else {
        possgst <- possgst
      }

      # Add in coefficient names
      # Period 1
      # Intercept
      text(1, t00, labels = if(coefs == TRUE) paste0("Intercept= ", round(coef(cmodel)[1], round.c), model_summary_p[1]) else "Intercept",
           pos=possgst[[1]], cex=textCEX)
      # ITS.Time
      text(timemidp1, time1thi, labels =if(coefs == TRUE) paste0("ITS.Time= ", round(coef(cmodel)[2], round.c), model_summary_p[2]) else "ITS.Time",
           pos=possgst[[2]], cex=textCEX)
      # Period 2
      # post1
      text(time_per2[1] + axshift[1], mean(c(t10, cft10)), labels =if(coefs == TRUE) paste0("post1= ", round(coef(cmodel)[3], round.c), model_summary_p[3]) else "post1",
           pos=possgst[[3]], cex=textCEX)
      # txp1
      text(timemidp2, time2thi, labels =if(coefs == TRUE) paste0("txp1= ", round(coef(cmodel)[4], round.c), model_summary_p[4]) else "txp1",
           pos=possgst[[4]], cex=textCEX)

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
             lwd=cex.legend, col=c(lcol[1],lcol[1],"gray"), bty="n", cex=cex.legend)
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
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## Period 2 ##
      # post1
      arrows(x0 = time_per2[1] + axshift[1], y0 = cft10, x1 = time_per2[1] + axshift[1],
             y1 = t10, code=2, angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # txp1
      arrows(x0 = timeqtrp2, y0 = time2thi, x1 = timemidp2, y1 = time2thi, code=2,
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## Period 3 ##
      # post2
      arrows(x0 = time_per3[1] + axshift[2], y0 = cft20, x1 = time_per3[1] + axshift[2],
             y1 = t20, code=2, angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # txp2
      arrows(x0 = timeqtrp3, y0 = time3thi, x1 = timemidp3, y1 = time3thi, code=2,
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)

      ## text positions ##
      possgmt <- list("Intercept"=posIntercept, "ITS.Time"=4, "post1"=2,
                      "txp1"=4, "post2"=2, "txp2"=4)
      #User submitted cpos
      makechange <- pos.text
      # identifies which elements to change in original positions
      newposlist <- vector()
      if(!is.null(pos.text)) {
        for (i in 1:length(names(makechange))) {
          newposlist[i] <- grep(paste0("^", names(makechange)[i], "$"), names(possgmt))
        }
      }
      # changes values
      if(!is.null(pos.text)) {
        for (i in 1:length(newposlist)) {
          possgmt[[newposlist[i]]] <- makechange[[i]]
        }
      } else {
        possgmt <- possgmt
      }

      # Add in coefficient names
      # Period 1
      # Intercept
      text(1, t00, labels = if(coefs == TRUE) paste0("Intercept= ", round(coef(cmodel)[1], round.c), model_summary_p[1]) else "Intercept",
           pos=possgmt[[1]], cex=textCEX)
      # ITS.Time
      text(timemidp1, time1thi, labels =if(coefs == TRUE) paste0("ITS.Time= ", round(coef(cmodel)[2], round.c), model_summary_p[2]) else "ITS.Time",
           pos=possgmt[[2]], cex=textCEX)
      # Period 2
      # post1
      text(time_per2[1] + axshift[1], mean(c(t10, cft10)), labels =if(coefs == TRUE) paste0("post1= ", round(coef(cmodel)[3], round.c), model_summary_p[3]) else "post1",
           pos=possgmt[[3]], cex=textCEX)
      # txp1
      text(timemidp2, time2thi, labels =if(coefs == TRUE) paste0("txp1= ", round(coef(cmodel)[4], round.c), model_summary_p[4]) else "txp1",
           pos=possgmt[[4]], cex=textCEX)
      # Period 3
      # post2
      text(time_per3[1] + axshift[2], mean(c(t20, cft20)),
           labels =if(coefs == TRUE) paste0("post2= ", round(coef(cmodel)[5], round.c), model_summary_p[5]) else "post2",
           pos=possgmt[[5]], cex=textCEX)
      # txp2
      text(timemidp3, time3thi, labels =if(coefs == TRUE) paste0("txp2= ", round(coef(cmodel)[6], round.c), model_summary_p[6]) else "txp2",
           pos=possgmt[[6]], cex=textCEX)
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
             lwd=cex.legend, col=c(lcol[1],lcol[2],"gray"), bty="n", cex=cex.legend)
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
             angle=30, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # ITS.Int
      arrows(x0 = 1, y0 = c00, x1 = 1, y1 = t00, code=2, #code=arrow_code[2],  #need arrow, vertical line
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # txi
      arrows(x0 = timeqtrp1, y0 = time1thi, x1 = timemidp1, y1 = time1thi, code=2,
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## Period 2 ##
      # post1
      arrows(x0 = time_per2[1], y0 = cfc10, x1 = time_per2[1], y1 = c10, code=2,
             angle=30, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # txp1
      arrows(x0 = timeqtrp2, y0 = time2chi, x1 = timemidp2, y1 = time2chi, code=2,
             angle=30, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # ixp1
      arrows(x0 = time_per2[1] + axshift[1], y0 = t10, x1 = time_per2[1] + axshift[1],
             y1 = c10, code=3, angle=30, length=.25, col = lcol[1],
             lwd = arwCEX, lty=3)
      # txip1
      arrows(x0 = timeqtrp2, y0 = time2thi, x1 = timemidp2, y1 = time2thi,
             code=2, angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)

      ## text positions ##
      posmgst <- list("Intercept"=posIntercept, "ITS.Time"=4, "ITS.Int"=posITS.int, "txi"=4,
                      "post1"=4, "txp1"=4, "ixp1"=4, "txip1"=4)
      #User submitted cpos
      makechange <- pos.text
      # identifies which elements to change in original positions
      newposlist <- vector()
      if(!is.null(pos.text)) {
        for (i in 1:length(names(makechange))) {
          newposlist[i] <- grep(paste0("^", names(makechange)[i], "$"), names(posmgst))
        }
      }
      # changes values
      if(!is.null(pos.text)) {
        for (i in 1:length(newposlist)) {
          posmgst[[newposlist[i]]] <- makechange[[i]]
        }
      } else {
        posmgst <- posmgst
      }

      # Add in coefficient names
      # Period 1
      # Intercept
      text(1, c00, labels =if(coefs == TRUE) paste0("Intercept= ", round(coef(cmodel)[1], round.c), model_summary_p[1]) else "Intercept",
           pos=posmgst[[1]], cex=textCEX)
      # ITS.Time
      text(timemidp1, time1chi, labels =if(coefs == TRUE) paste0("ITS.Time= ", round(coef(cmodel)[2], round.c), model_summary_p[2]) else "ITS.Time",
           pos=posmgst[[2]], cex=textCEX)
      # ITS.Int
      text(1, t00, labels =if(coefs == TRUE) paste0("ITS.Int= ", round(coef(cmodel)[3], round.c), model_summary_p[3]) else "ITS.Int",
           pos=posmgst[[3]], cex=textCEX)
      # txi
      text(timemidp1, time1thi, labels =if(coefs == TRUE) paste0("txi= ", round(coef(cmodel)[4], round.c), model_summary_p[4]) else "txi",
           pos=posmgst[[4]], cex=textCEX)
      # Period 2
      # post1
      text(time_per2[1], mean(c(c10, cfc10)), labels =if(coefs == TRUE) paste0("post1= ", round(coef(cmodel)[5], round.c), model_summary_p[5]) else "post1",
           pos=posmgst[[5]], cex=textCEX)
      # txp1
      text(timemidp2, time2chi, labels =if(coefs == TRUE) paste0("txp1= ", round(coef(cmodel)[6], round.c), model_summary_p[6]) else "txp1",
           pos=posmgst[[6]], cex=textCEX)
      # ixp1
      text(time_per2[1] + axshift[1], mean(c(c10, t10)), labels =if(coefs == TRUE) paste0("ixp1= ", round(coef(cmodel)[7], round.c), model_summary_p[7]) else "ixp1",
           pos=posmgst[[7]], cex=textCEX)
      # txip1
      text(timemidp2, time2thi, labels =if(coefs == TRUE) paste0("txip1= ", round(coef(cmodel)[8], round.c), model_summary_p[8]) else "txip1",
           pos=posmgst[[8]], cex=textCEX)
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
             lwd=cex.legend, col=c(lcol[1],lcol[2],"gray"), bty="n", cex=cex.legend)
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
             angle=30, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # ITS.Int
      arrows(x0 = 1, y0 = c00, x1 = 1, y1 = t00, code=2, #code=arrow_code[2],  #need arrow, vertical line
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # txi
      arrows(x0 = timeqtrp1, y0 = time1thi, x1 = timemidp1, y1 = time1thi, code=2,
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## Period 2 ##
      # post1
      arrows(x0 = time_per2[1], y0 = cfc10, x1 = time_per2[1], y1 = c10, code=2,
             angle=30, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # txp1
      arrows(x0 = timeqtrp2, y0 = time2chi, x1 = timemidp2, y1 = time2chi, code=2,
             angle=30, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # ixp1
      arrows(x0 = time_per2[1] + axshift[1], y0 = t10, x1 = time_per2[1] + axshift[1],
             y1 = c10, code=3, angle=30, length=.25, col = lcol[1],
             lwd = arwCEX, lty=3)
      # txip1
      arrows(x0 = timeqtrp2, y0 = time2thi, x1 = timemidp2, y1 = time2thi,
             code=2, angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## Period 3 ##
      # post2
      arrows(x0 = time_per3[1], y0 = cfc20, x1 = time_per3[1], y1 = c20, code=2,
             angle=30, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # txp2
      arrows(x0 = timeqtrp3, y0 = time3chi, x1 = timemidp3, y1 = time3chi, code=2,
             angle=30, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      # ixp2
      arrows(x0 = time_per3[1] + axshift[2], y0 = t20, x1 = time_per3[1] + axshift[2],
             y1 = c20, code=3, angle=30, length=.25, col = lcol[1],
             lwd = arwCEX, lty=3)
      # txip2
      arrows(x0 = timeqtrp3, y0 = time3thi, x1 = timemidp3, y1 = time3thi,
             code=2, angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      ## text positions ##
      posmgmt <- list("Intercept"=posIntercept, "ITS.Time"=4, "ITS.Int"=posITS.int, "txi"=4,
                      "post1"=2, "txp1"=4,"ixp1"=4, "txip1"=4, "post2"=2,"txp2"=4,"ixp2"=4, "txip2"=4)
      #User submitted cpos
      makechange <- pos.text
      # identifies which elements to change in original positions
      newposlist <- vector()
      if(!is.null(pos.text)) {
        for (i in 1:length(names(makechange))) {
          newposlist[i] <- grep(paste0("^", names(makechange)[i], "$"), names(posmgmt))
        }
      }
      # changes values
      if(!is.null(pos.text)) {
        for (i in 1:length(newposlist)) {
          posmgmt[[newposlist[i]]] <- makechange[[i]]
        }
      } else {
        posmgmt <- posmgmt
      }

      # Add in coefficient names
      # Period 1
      # Intercept
      text(1, c00, labels =if(coefs == TRUE) paste0("Intercept= ", round(coef(cmodel)[1], round.c), model_summary_p[1]) else "Intercept",
           pos=posmgmt[[1]], cex=textCEX)
      # ITS.Time
      text(timemidp1, time1chi, labels =if(coefs == TRUE) paste0("ITS.Time= ", round(coef(cmodel)[2], round.c), model_summary_p[2]) else "ITS.Time",
           pos=posmgmt[[2]], cex=textCEX)
      # ITS.Int
      text(1, t00, labels =if(coefs == TRUE) paste0("ITS.Int= ", round(coef(cmodel)[3], round.c), model_summary_p[3]) else "ITS.Int",
           pos=posmgmt[[3]], cex=textCEX)
      # txi
      text(timemidp1, time1thi, labels =if(coefs == TRUE) paste0("txi= ", round(coef(cmodel)[4], round.c), model_summary_p[4]) else "txi",
           pos=posmgmt[[4]], cex=textCEX)
      # Period 2
      # post1
      text(time_per2[1], mean(c(c10, cfc10)), labels =if(coefs == TRUE) paste0("post1= ", round(coef(cmodel)[5], round.c), model_summary_p[5]) else "post1",
           pos=posmgmt[[5]], cex=textCEX)
      # txp1
      text(timemidp2, time2chi, labels =if(coefs == TRUE) paste0("txp1= ", round(coef(cmodel)[6], round.c), model_summary_p[6]) else "txp1",
           pos=posmgmt[[6]], cex=textCEX)
      # ixp1
      text(time_per2[1] + axshift[1], mean(c(c10, t10)), labels =if(coefs == TRUE) paste0("ixp1= ", round(coef(cmodel)[7], round.c), model_summary_p[7]) else "ixp1",
           pos=posmgmt[[7]], cex=textCEX)
      # txip1
      text(timemidp2, time2thi, labels =if(coefs == TRUE) paste0("txip1= ", round(coef(cmodel)[8], round.c), model_summary_p[8]) else "txip1",
           pos=posmgmt[[8]], cex=textCEX)
      # Period 3
      # post2
      text(time_per3[1], mean(c(c20, cfc20)), labels =if(coefs == TRUE) paste0("post2= ", round(coef(cmodel)[9], round.c), model_summary_p[9]) else "post2",
           pos=posmgmt[[9]], cex=textCEX)
      # txp2
      text(timemidp3, time3chi, labels =if(coefs == TRUE) paste0("txp2= ", round(coef(cmodel)[10], round.c), model_summary_p[10]) else "txp2",
           pos=posmgmt[[10]], cex=textCEX)
      # ixp2
      text(time_per3[1] + axshift[2], mean(c(c20, t20)), labels =if(coefs == TRUE) paste0("ixp2= ", round(coef(cmodel)[11], round.c), model_summary_p[11]) else "ixp2",
           pos=posmgmt[[11]], cex=textCEX)
      # txip2
      text(timemidp3, time3thi, labels =if(coefs == TRUE) paste0("txip2= ", round(coef(cmodel)[12], round.c), model_summary_p[12]) else "txip2",
           pos=posmgmt[[12]], cex=textCEX)
    }
  }
}

