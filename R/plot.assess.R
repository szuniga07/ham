#' Prediction plot of treatment and control groups for DID and ITS models
#'
#' Provides partial prediction plots for treatment and control groups from difference-in-difference (DID)
#' and interrupted time series (ITS) models. The graph will produce lines for treatment/intervention and
#' control groups to gain understanding through a visual representation of the regression coefficients.
#' By default, the treatment/intervention group is represented with a blue line, the control group is represented with
#' a red line, and the counterfactual line, when available, is a dashed line. There are many options to change the plot.
#'
#' @param x assess object. Either difference-in-difference or interrupted time series model with no covariate adjustment.
#' @param y type of model, specify either 'DID' (difference-in-difference) or 'ITS' (interrupted time series). Will not accept other models.
#' @param xlim specify plot's x-axis limits with a 2 value vector.
#' @param ylim specify plot's y-axis limits with a 2 value vector.
#' @param xlab a vector label for the x-axis.
#' @param ylab a vector label for the y-axis.
#' @param main the main title of the plot.
#' @param lwd select the line width.
#' @param col specify intervention and control group colors in a vector. Defaults to, if nothing selected, c("blue", "red") or "blue" for single-group Interrupted Time Series models.
#' @param tcol specify treatment or interruption line color as a single character vector. Defaults to "gray" if nothing selected.
#' @param tgt specify 1 or more values on the x-axis of where to add a target line when y='group'. Or 1 or more values on the y-axis of where to add a target line when y='time' or 'roll'. Default is NULL.
#' @param tgtcol select a color for the target line. Default is 'gray'.
#' @param cfact logical TRUE or FALSE that indicates whether a counterfactual line should be included. Defaults to FALSE.
#' @param conf.int logical TRUE or FALSE that indicates whether a 95% confidence interval bands should be included. Defaults to FALSE.
#' @param adj.alpha factor modifying the opacity alpha of the confidence interval bands, in the range of 0 to 1. Default is NULL; if conf.int=TRUE, defaults to 0.4.
#' @param add.means adds group means by time period based on model data. Default is FALSE
#' @param add.legend add a legend by selecting the location as "bottomright", "bottom", "bottomleft",
#' "left", "topleft", "top", "topright", "right", "center". No legend if nothing selected.
#' @param legend a character vector of length >= 1 to appear in the DID and ITS model legends to represent the Intervention and Control groups.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default of 1.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex.
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex.
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex.
#' @param cex.text The magnification to be used for the text added into the plot relative to the current setting of 1.
#' @param cex.legend The magnification to be used for the legend added into the plot relative to the current setting of 1.
#' @param name logical TRUE or FALSE that indicates whether coefficient names
#' should be added to the plot. Default is FALSE. It is overridden if coefs = TRUE.
#' @param coefs logical TRUE or FALSE that indicates whether coefficient names, values,
#' and p-value significance symbols ('+' p<0.10; '*' p<0.05; '**' p<0.01; '***' p<0.001) should be
#' added to the plot. Default is FALSE. coefs = TRUE overrides name = FALSE.
#' @param round.c an integer indicating the number of decimal places
#' to be used for rounding coefficient values.
#' @param pos.text a list of named integer value(s) between 1 to 4 indicating
#' the position of the text added into the plot. List name(s) should use coefficient variable names.
#' @param arrow logical TRUE or FALSE that indicates whether arrows and
#' coefficient names should be added to visualize effects. Default is FALSE.
#' @param xshift shifts one or two of some of the overlapping intervention associated arrows
#' along the x-axis for a better view. Vector values of at least length 1 or 2 can be positive
#' or negative. And xshift should be specified in the order of the coefficients. Only 1 or 2
#' of the furthest right, vertical lines for the intervention group is shifted (i.e., not left).
#' One line is shifted when there is 1 treatment/interruption period and 2 shifts for 2 periods.
#' (e.g., "DID" before "DID.Trend" for DID models with argument did="many").
#' @param y.axis a vector of data or unique values that makes up y-axis values to replace the intervention time
#' variable values. This will be most helpful if you prefer current calendar years instead of
#' values starting at 1 (e.g., y.axis= data$Year for 1900-1999, not 1-100). Must have equal
#' lengths for unique y.axis values and unique replaced values.
#' @param ... additional arguments.
#'
#' @return plot of partial predictions for treatment and control groups.
#' @importFrom graphics lines plot legend abline segments arrows points text polygon
#' @importFrom grDevices adjustcolor
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
#' col=c("cyan","hotpink"), tcol="springgreen", lwd=7, cex=2, cex.axis=2, cex.lab=2,
#' cex.main=3, cex.text=1.2, cex.legend=1.25, name=FALSE, coefs=TRUE, round.c=1,
#' pos.text= list("txp5"=3, "post9"=4), arrow=TRUE, xshift=c(.5, 1.5),
#' cfact=TRUE, conf.int=TRUE, adj.alpha=0.2)
#' # ITS: USA's unemployment rate with a single group, 10 interruption model
#' key_time <- c(5, 14, 17, 29, 42, 59, 69, 73, 80,92)# key interruption periods
#' u110 <- assess(formula=rate ~ ., data=unemployment, intervention="usa",# model
#'                int.time="year", its="one", interrupt= key_time)
#' # Graph the results, note shift in intercept and slope during 1933's New Deal
#' plot(u110, "ITS", add.means = TRUE, coefs=TRUE, conf.int=TRUE, adj.alpha = .2,
#'      lwd=2, col="slategray", tcol= "orange", main="US unemployment rate",
#'      xlab="Years (1929-2024)", ylab="Proportion of labor market",
#'      cex.main=2, cex.axis = 1.5, cex.lab = 1.5, cex=2, cex.text = 1.25,
#'      pos.text=list("ITS.Time"=4, "post42"=1,"txp42"=3,"txp92"=3), y.axis=unemployment$Year)
#' for(i in 1:length(key_time)) {
#'   text(key_time[i], .22-(.01*i), cex=1.25, labels =
#'          paste0(unemployment[ key_time[i], "Year"], ": ", unemployment[ key_time[i], "event"]))
#' }
plot.assess <- function(x, y, xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL, main=NULL, lwd=NULL, col=NULL, tcol=NULL,
                        cfact=FALSE, conf.int=FALSE, adj.alpha=NULL, add.means=FALSE, add.legend=NULL, legend=NULL,
                        tgt=NULL, tgtcol="gray", cex=1, cex.axis=NULL, cex.lab=NULL, cex.main=NULL, cex.text=NULL,
                        cex.legend=NULL, name=FALSE, coefs=FALSE, round.c=NULL,
                        pos.text=NULL, arrow=FALSE, xshift=NULL, y.axis=NULL, ...) {
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
  #y.axis having equal lengths with current time variable
  if(!is.null(y.axis)) {
    if(!is.null(y.axis)) {
      if (length(unique(x$study$group_means[,1])) != length(unique(y.axis))) {stop("Error: Expecting equal lengths for int.time and y.axis." )}
    }
  }
  # Get assess objects
if(y == "DID") {
  aggr_mns <-  x[["study"]][["group_means_did"]]   #model data
  aggr_mns_real <-  x[["study"]][["group_means"]]  #actual data
  }
if(y == "ITS") {
  aggr_mns <-  x[["study"]][["group_means_its"]]
    }
    #Correct for time increments that don't begin at 1
  aggr_mns[, "time.2.backup.var"] <- aggr_mns[, 1]
  aggr_mns[, 1] <- as.numeric(ordered(aggr_mns[, 1]))
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
  # Create x and y labels
  if (!is.null(xlab)) {
    xlab <- xlab
  } else {
    xlab <- xvar
  }
  if (!is.null(ylab)) {
    ylab <- ylab
  } else {
    ylab <- yvar
  }

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
        stop("Error: Expecting pos.text variable names and values for 'Intercept', 'Period','Post.All', 'Int.Var', and/or 'DID'.")
      }
    }
  }
  if (model_type == "many") {
    if (!is.null(pos.text)) {
      if (all(names(pos.text) %in% c("Intercept","Period","DID","DID.Trend")) == FALSE) {
        stop("Error: Expecting pos.text variable names and values for 'Intercept', 'Period', 'DID', and/or 'DID.Trend'.")
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
  #Treatment or Intervention Line colors
  if(is.null(tcol)) {
    ticol <- c("gray")
  } else {
    ticol <- tcol
  }
  # Cex settings cex, cex.axis, cex.lab, cex.main
  if(!is.null(cex)) {
    cex <- cex
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
  #polygon() alpha f color
  if(!is.null(adj.alpha)) {
    adj.alpha <- adj.alpha
  } else {
    adj.alpha <- 0.4
  }
  # Legend names
  if(!is.null(legend)) {
    legend <- legend
  } else {
    legend <- c("Intervention", "Control")
  }
  #List of legend locations
  leg_locate <- c("bottomright", "bottom", "bottomleft", "left",
                  "topleft", "top", "topright", "right", "center")
  #This gets the sign of the coefficients to assign the arrow codes
  #cmodel will work for DID and ITS models
  mdlcoefsign <- sign(coef(cmodel))[-1]
  #Change to arrow code, this leaves 0=0
  arrow_code <- mdlcoefsign
  arrow_code[arrow_code == 1] <- 2
  arrow_code[arrow_code == -1] <- 1
  # predSE function to get predicted standard errors for confidence bands #
  predSE <- function(Model, Data) {
    #Get model data
    tmoddf <- Model$model
    #Create list with TRUE/FALSE on identical rows I am looking for
    tdup <- list()
    #Function to identify which are identical for just 1 row of data
    dupFnc <- function(xdf, ydf) {
      i <- 1
      tcnt_ls <- vector(length = 1)
      while (i < nrow(xdf)) {
        tcnt_ls[1] <- i+1
        i <- i + 1
        if (all(xdf[i, 2:(ncol(ydf) +1)] == ydf) == TRUE) {
          break
        }
      }
      return(tcnt_ls)
    }
    #Narrow down to TRUE only
    true_tlep <- dupFnc(xdf=Model$model, ydf=Data)
    #Return the first true row standard error, first needed with aggregated data
    pred_SE <- predict(Model, newdata=tmoddf[true_tlep[1], -1], se.fit=TRUE)[["se.fit"]]
    return(pred_SE)
  }
  #Indicates we will have axes
  if(!is.null(y.axis)) {
    axes <- FALSE
  } else {
    axes <- TRUE
  }
  #Indicates y axis "at" values
  if(axes== FALSE) {
    y.at <- sort(unique(x$study$group_means[, 1]))
  } else {
    y.at <- NULL
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
         main=main_title, xlab= xlab, ylab=ylab, xlim=xlim, ylim=ylim,
         cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main, axes=axes)
    #Add in confidence bands
    if(conf.int==TRUE) {
      # temp data frame #
      tmpdf00 <- data.frame(Post.All = 0, Int.Var=0, DID=0)
      tmpdf01 <- data.frame(Post.All = 1, Int.Var=0, DID=0)
      tmpdf10 <- data.frame(Post.All = 0, Int.Var=1, DID=0)
      tmpdf11 <- data.frame(Post.All = 1, Int.Var=1, DID=1)
      # Margin of Errors #
      #Control
      moe00 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf00)
      moe01 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf01)
      #treatment
      moe10 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf10)
      moe11 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf11)
      #Control
      polygon(c(0:1, rev(0:1)), c(c0+moe00, c1+moe01, c1-moe01, c0-moe00),
              col= adjustcolor(lcol[2], alpha.f = adj.alpha),
              border=adjustcolor(lcol[2], alpha.f = adj.alpha))
      #treatment
      polygon(c(0:1, rev(0:1)), c(t0+moe10, t1+moe11, t1-moe11, t0-moe10),
              col=adjustcolor(lcol[1], alpha.f = adj.alpha),
              border=adjustcolor(lcol[1], alpha.f = adj.alpha))
    }
    #Intervention line
    abline(v=.5, col= ticol, lty=3, lwd=lwidth)
    lines(0:1, c(t0, t1), type="l", col=lcol[1], lwd=lwidth)
    lines(0:1, c(c0, c1), type="l", col=lcol[2], lwd=lwidth)
    #Counter factual lines
    if(cfact==TRUE) {
      lines(c(0, 1), c(mean(t0, cft1), cft1), col=lcol[1], lty=2, lwd=lwidth)
    }
    # Arrows and coefficient names #
    if(arrow == TRUE) {
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
    }
    ## Group means by time ##
    #Add in group means based on the model data
    if(add.means == TRUE) {
      did_grps <- x$study$group_means_did
      points(did_grps[did_grps[, 2]==0, 1], did_grps[did_grps[, 2]==0, 3],
             col=lcol[2], pch=20, cex=cex)
      points(did_grps[did_grps[, 2]==1, 1], did_grps[did_grps[, 2]==1, 3],
             col=lcol[1], pch=20, cex=cex)
    }
      ## text positions ##
    if(any(c(coefs, name) == TRUE)) {
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
      if (add.means == TRUE) {
        legend(x=add.legend, legend= if(cfact==TRUE) c(legend[1], legend[2],"Counterfactual", "Treated", "Means") else
        c(legend[1], legend[2],"Treated", "Means"), lty= if(cfact==TRUE) c(1,1,2,3, NA) else c(1,1,3, NA),
        lwd=cex.legend, bty="n", cex=cex.legend, pch= if(cfact==TRUE) c(NA,NA,NA,NA,20) else c(NA,NA,NA, 20),
        col= if(cfact==TRUE) c(lcol[1],lcol[2],lcol[1], ticol, "black") else c(lcol[1],lcol[2], ticol, "black"))
      }
      if (add.means == FALSE) {
        legend(x=add.legend, legend= if(cfact==TRUE) c(legend[1], legend[2],"Counterfactual", "Treated") else
        c(legend[1], legend[2],"Treated"), lty= if(cfact==TRUE) c(1,1,2,3) else c(1,1,3),
             lwd=cex.legend, bty="n", cex=cex.legend,
        col= if(cfact==TRUE) c(lcol[1],lcol[2],lcol[1], ticol) else c(lcol[1],lcol[2], ticol))
      }
    }
    #Target line
    if(!is.null(tgt)) {
      for (i in 1:length(tgt)) {
        abline(h=tgt[i], lwd=lwd, col= tgtcol[1], lty=1)
      }
    }
    #Add y-axis value to graph
    if(axes== FALSE) {
      axis(side=1, at=y.at, labels=y.axis, cex.axis=cex.axis)
      axis(2, cex.axis=cex.axis)
      box()
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
    #For the issue of no pre-intervention values for DID==1, get Y value at treatment
      int_start_y <- aggr_mns_real[which(aggr_mns_real[, 1] == min(aggr_mns_real[, 1]) &
                                           aggr_mns_real[, 2] == max(aggr_mns_real[, 2])), 3]
      #Trying out counterfactual at start of treatment period
      #int_start_y <- aggr_mns[which(aggr_mns[, 1] == treat_start &
      #                              aggr_mns[, 2] == max(aggr_mns[, 2])), 3]
    c0 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*1 + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0 #control group time 0
    c1 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*max_time + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0 #control group time 1
    c0.5 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*treat_start + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0 #control group at time of the treatment start
    t0 <- int_start_y + coef(cmodel)[[2]]*1 + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0   #intervention group time 0
    #t0.5 <- int_start_y
    t1 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*max_time + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*max_time   #intervention group time 1
    #Counterfactual for treated
    # use int_start_y here
    cft1 <- int_start_y + coef(cmodel)[[2]]*max_time + coef(cmodel)[[3]]*0 +
      coef(cmodel)[[4]]*0   #intervention group time max
    #Average treatment effect on the treated
    atet0 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*treat_start + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*treat_start   #intervention group time 1
    atet1 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*max_time + coef(cmodel)[[3]]*1 +
      coef(cmodel)[[4]]*max_time   #intervention group time 1

    plot(range(aggr_mns[, 1]), range(c(cmodel[["fitted.values"]], t0)), type="n",
         main=main_title, xlab= xlab, ylab=ylab, xlim=xlim, ylim=ylim,
         cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main, axes=axes)
    #Add in confidence bands
    if(conf.int==TRUE) {
      # temp data frame #
      tmpdf00 <- data.frame(Period = 1, DID=0, DID.Trend=0)
      tmpdf01 <- data.frame(Period = max_time, DID=0, DID.Trend=0)
      tmpdf10 <- data.frame(Period = treat_start, DID=1, DID.Trend=treat_start)
      tmpdf11 <- data.frame(Period = max_time, DID=1, DID.Trend=max_time)
      # Margin of Errors #
      #Control
      moe00 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf00)
      moe01 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf01)
      #treatment
      moe10 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf10)
      moe11 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf11)
      #Control
      polygon(c(c(1,max_time), c(max_time,1)), c(c0+moe00, c1+moe01, c1-moe01, c0-moe00),
              col= adjustcolor(lcol[2], alpha.f = adj.alpha),
              border=adjustcolor(lcol[2], alpha.f = adj.alpha))
      #treatment
      polygon(c(c(treat_start,max_time), c(max_time,treat_start)), c(atet0+moe10, t1+moe11, t1-moe11, atet0-moe10),
              col=adjustcolor(lcol[1], alpha.f = adj.alpha),
              border=adjustcolor(lcol[1], alpha.f = adj.alpha))
    }
    #Intervention line
    abline(v=treat_start, col= ticol, lty=3, lwd=lwidth)
    lines(c(1, max_time), c(c0, c1), type="l", col=lcol[2], lwd=lwidth)
    #Counter factual line
    if(cfact==TRUE) {
      lines(c(1, max_time), c(t0, cft1), type="l", col=lcol[1], lty=2, lwd=lwidth)
#      segments(x0 = treat_start, y0 = t0.5, x1 = max_time, y1 = cft1, col = lcol[1], lwd = lwidth, lty=2)
    }
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
    }
    ## Group means by time ##
    #Add in group means based on the model data
    if(add.means == TRUE) {
      did_grps <- x$study$group_means_did
      points(did_grps[did_grps[, 2]==0, 1], did_grps[did_grps[, 2]==0, 3],
             col=lcol[2], pch=20, cex=cex)
      points(did_grps[did_grps[, 2]==1, 1], did_grps[did_grps[, 2]==1, 3],
             col=lcol[1], pch=20, cex=cex)
    }
    if(any(c(coefs, name) == TRUE)) {
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
      if (add.means == TRUE) {
        legend(x=add.legend, legend= if(cfact==TRUE) c(legend[1], legend[2],"Counterfactual", "Treated", "Means") else
          c(legend[1], legend[2], "Treated", "Means"), lty= if(cfact==TRUE) c(1,1,2,3, NA) else c(1,1,3, NA),
          lwd=cex.legend, col= if(cfact==TRUE) c(lcol[1],lcol[2],lcol[1], ticol, "black") else c(lcol[1],lcol[2], ticol, "black"),
          bty="n", cex=cex.legend, pch= if(cfact==TRUE) c(NA,NA,NA,NA,20) else c(NA,NA,NA, 20))
      }
      if (add.means == FALSE) {
      legend(x=add.legend, legend= if(cfact==TRUE) c(legend[1], legend[2],"Counterfactual", "Treated") else
        c(legend[1], legend[2], "Treated"), lty= if(cfact==TRUE) c(1,1,2,3) else c(1,1,3), lwd=cex.legend,
        col= if(cfact==TRUE) c(lcol[1],lcol[2],lcol[1], ticol) else c(lcol[1],lcol[2], ticol), bty="n",
        cex=cex.legend)
      }
    }
    #Target line
    if(!is.null(tgt)) {
      for (i in 1:length(tgt)) {
        abline(h=tgt[i], lwd=lwd, col= tgtcol[1], lty=1)
      }
    }
    #Add y-axis value to graph
    if(axes== FALSE) {
      axis(side=1, at=y.at, labels=y.axis, cex.axis=cex.axis)
      axis(2, cex.axis=cex.axis)
      box()
    }
  }

  ##############################################################################
  #      Function to get fitted and counterfactual values from mgmt model      #
  ##############################################################################
  fitfnc <- function(x, a, B0_adjust, d, e, conf.int) {
    #x= coefficients, y= binary values, z= trend values,
    #a=interruptions, b= # of groups, B0_adjust, d= max_time, e= model_type
    #Get the number of groups
    if(e == "sgmt") {
      b <- 1
    }
    if(e == "mgmt") {
      b <- 2
    }
    # Number of coefficients during the baseline period
    num_baseline_coef <- ifelse(b == 2, 3, 1)
    total_coefs <- length(x)
    # Formula for the number of expected coefficients
    expect_coefs <- ((b * 2) * length(a)) + num_baseline_coef + 1 #1=intercept
    # How many coefficients were added to the standard models #
    add_coefs <- total_coefs - expect_coefs
    #Time points that effect intervention and control group expected vals + counterfactuals
    treat_fit_time <- c(1:d)
    #Get coefficient names
    tmpdf_names <- names(x)[1:expect_coefs]
    #Control group starting/stopping fitted and counterfactual values
    #Create NA values for the controls and treatments to place in the return list
    cp1 <- NA
    cp2 <- NA
    cfcp1 <- NA
    cfcp2 <- NA
    controls_df <- NA
    ctmpdf <- NA
    tmpdf <- NA
    tp1 <- NA
    tp2 <- NA
    cftp1 <- NA
    cftp2 <- NA
    time_per <- NA

    #########################
    # Specific coefficients #
    #########################
    # Control group binary values
    control_fit_binary <- seq(1, expect_coefs, by=4)
    # Control group trend lines
    control_fit_trend <- seq(2, expect_coefs, by=4)
    # treatment group trend lines
    treat_fit_trend <- (1:expect_coefs)[1:expect_coefs %% 2 == 0]
    # treatment group binary values
    #Just subtracting 1 to get odds
    treat_fit_binary <- treat_fit_trend - 1
    # Get start and stop time periods for each segment
    time_per1 <- c(1, a)
    time_per2 <- c(a - 1, d)
    #Make a data frame for confidence bands
    time_per <- data.frame(time_per1, time_per2)
    #Get the total amount of periods
    num_pers <- 1:length(time_per1)
    ## Controls data frame ##
    if (b == 2) {
      controls_df <- cbind(control_fit_binary, control_fit_trend, num_pers,
                           time_per1, time_per2, intrup_wt= c(0, a))
    } else {
      controls_df <- NA
    }
    #################
    # Fitted Values #
    #################

    ## Treated data frame ##
    treated_df <- cbind(treat_fit_binary, treat_fit_trend,
                        num_pers=rep(num_pers, each=length(treat_fit_binary)/max(num_pers) ),
                        time_per1= rep(time_per1, each=length(treat_fit_binary)/max(num_pers)),
                        time_per2= rep(time_per2, each=length(treat_fit_binary)/max(num_pers)),
                        intrup_wt= rep(c(0, a), each=length(treat_fit_binary)/max(num_pers)))
    #Treatment group starting fitted values
    tinc1 <- treated_df[, "time_per1"]
    tp1 <- vector(mode = "numeric", length = length(tinc1))
    for (i in 1:length(treated_df[, "num_pers"])) {
      tp1[[i]] <- sum(mod_coefs[treated_df[,"treat_fit_binary"]][1:i]) +  # binary predictors
        sum(mod_coefs[treated_df[, "treat_fit_trend"]] *       # trend predictors
              pmax(tinc1[i] - treated_df[, "intrup_wt"], 0)) +   # X values
              B0_adjust
    }
    #Treatment group stopping fitted values
    tinc2 <- treated_df[, "time_per2"]
    tp2 <- vector(mode = "numeric", length = length(tinc2))
    for (i in 1:length(treated_df[, "num_pers"])) {
      tp2[[i]] <- sum(mod_coefs[treated_df[,"treat_fit_binary"]][1:i]) +  # binary predictors
        sum(mod_coefs[treated_df[, "treat_fit_trend"]] *       # trend predictors
              pmax(tinc2[i] - treated_df[, "intrup_wt"], 0)) +   # X values
              B0_adjust
    }
    # Control group starting fitted values
    if(e == "mgmt") {
      cinc1 <- controls_df[, "time_per1"]
      cp1 <- vector(mode = "numeric", length = length(cinc1))
      for (i in controls_df[, "num_pers"]) {
        cp1[[i]] <- sum(mod_coefs[controls_df[,"control_fit_binary"]][1:i]) +  # binary predictors
          sum(mod_coefs[controls_df[,"control_fit_trend"]] *       # trend predictors
                pmax(cinc1[i] - controls_df[, "intrup_wt"], 0)) +          # X values
                B0_adjust
      }
      # Control group stopping fitted values
      cinc2 <- controls_df[, "time_per2"]
      cp2 <- vector(mode = "numeric", length = length(cinc2))
      for (i in controls_df[, "num_pers"]) {
        cp2[[i]] <- sum(mod_coefs[controls_df[,"control_fit_binary"]][1:i]) +  # binary predictors
          sum(mod_coefs[controls_df[,"control_fit_trend"]] *       # trend predictors
                pmax(cinc2[i] - controls_df[, "intrup_wt"], 0)) +          # X values
                B0_adjust
      }
    }
    #####################
    ## Counterfactuals ##
    #####################
    #Treatment group starting counterfactual values
    l_time <- length(treated_df[, "time_per1"]) - b  #length of new data frame
    treated_cf <- treated_df[1:l_time, ]
    treated_cf_time_per1 <- tail(treated_df[, "time_per1"], l_time)
    treated_cf_time_per2 <- tail(treated_df[, "time_per2"], l_time)
    treated_cf[, "time_per1"] <- treated_cf_time_per1
    treated_cf[, "time_per2"] <- treated_cf_time_per2
    cftinc1 <- treated_cf[, "time_per1"]
    cftp1 <- vector(mode = "numeric", length = length(cftinc1))
    for (i in 1:length(treated_cf[, "num_pers"])) {
      cftp1[[i]] <- sum(mod_coefs[treated_cf[,"treat_fit_binary"]][1:i]) +  # binary predictors
        sum(mod_coefs[treated_cf[, "treat_fit_trend"]][1:i] *       # trend predictors
              pmax(cftinc1[i] - treated_cf[, "intrup_wt"][1:i], 0)) +   # X values
              B0_adjust
    }
    #Treatment group ending counterfactual values
    cftinc2 <- treated_cf[, "time_per2"]
    cftp2 <- vector(mode = "numeric", length = length(cftinc2))
    for (i in 1:length(treated_cf[, "num_pers"])) {
      cftp2[[i]] <- sum(mod_coefs[treated_cf[,"treat_fit_binary"]][1:i]) +  # binary predictors
        sum(mod_coefs[treated_cf[, "treat_fit_trend"]][1:i] *       # trend predictors
              pmax(cftinc2[i] - treated_cf[, "intrup_wt"][1:i], 0)) +   # X values
              B0_adjust
    }

    #Control group starting/stopping counterfactual values
    if (e == "mgmt") {
      cl_time <- length(controls_df[, "time_per1"]) - (b/2)  #length of new data frame
      controls_cf <- controls_df[1:cl_time, ]
      controls_cf_time_per1 <- tail(controls_df[, "time_per1"], cl_time)
      controls_cf_time_per2 <- tail(controls_df[, "time_per2"], cl_time)
      controls_cf[, "time_per1"] <- controls_cf_time_per1
      controls_cf[, "time_per2"] <- controls_cf_time_per2
      cfcinc1 <- controls_cf[, "time_per1"]
      cfcp1 <- vector(mode = "numeric", length = length(cfcinc1))
      for (i in 1:length(controls_cf[, "num_pers"])) {
        cfcp1[[i]] <- sum(mod_coefs[controls_cf[,"control_fit_binary"]][1:i]) +  # binary predictors
          sum(mod_coefs[controls_cf[, "control_fit_trend"]][1:i] *       # trend predictors
                pmax(cfcinc1[i] - controls_cf[, "intrup_wt"][1:i], 0)) +   # X values
                B0_adjust
      }
    }

    #Control group ending counterfactual values
    if (e == "mgmt") {
      cfcinc2 <- controls_cf[, "time_per2"]
      cfcp2 <- vector(mode = "numeric", length = length(cfcinc2))
      for (i in 1:length(controls_cf[, "num_pers"])) {
        cfcp2[[i]] <- sum(mod_coefs[controls_cf[,"control_fit_binary"]][1:i]) +  # binary predictors
          sum(mod_coefs[controls_cf[, "control_fit_trend"]][1:i] *       # trend predictors
                pmax(cfcinc2[i] - controls_cf[, "intrup_wt"][1:i], 0)) +   # X values
                B0_adjust
      }
    }

    # Relevant treatment time trends indexed only for mgmt, doesn't apply to sgmt or controls
    if(b== 2) {
      tp1 <- tp1[1:length(tp1) %% 2 == 0]  # Gets even elements only
      tp2 <- tp2[1:length(tp2) %% 2 == 0]
      cftp1 <- cftp1[1:length(cftp1) %% 2 == 0]
      cftp2 <- cftp2[1:length(cftp2) %% 2 == 0]
    }
    #########################################
    ## Data for confidence band std errors ##
    #########################################
    if(conf.int==TRUE) {
      if (e == "mgmt") {
        #Get number of periods as a vector to fit in longer data column
        num_pers_long <- rep(num_pers, each=2)
        #Key time points
        time_points <- sort(c(1, a, a - 1, d))
        #interruption weights
        intrup_wt <- rep(c(0, a), each=length(treat_fit_binary)/max(num_pers))
        #Number of columns per each period
        num_cols_per <- expect_coefs/ max(num_pers)
        #Number of rows needed to create each period
        rows_needed <- max(num_pers) * 2
        cells_needed <- rows_needed * (expect_coefs )
        #Treatment group binary predictors
        tmpdf <- data.frame(matrix(rep(0, cells_needed), nrow=rows_needed))
        colnames(tmpdf) <- tmpdf_names
        #Control group index of variables to convert to 0 from treatment data
        ctl_0_col1 <- seq(3, expect_coefs, by=4)
        ctl_0_col2 <- seq(4, expect_coefs, by=4)
        #Create values for binary predictors
        for(i in 1:rows_needed) {
          tmpdf[i, treat_fit_binary[which(treat_fit_binary < (num_pers_long[i]*num_cols_per))]] <- 1
        }
        # Give values to time point columns
        tmpdf$ITS.Time <- time_points
        tmpdf$txi <- time_points

        #Create values for trend predictors
        for(i in 1:(rows_needed-2)) {
          tmpdf[3:nrow(tmpdf), treat_fit_trend[-(1:2)][i] ]  <-
            pmax(tmpdf[3:nrow(tmpdf), "ITS.Time" ] - intrup_wt[-(1:2)][i], 0)
        }
        ## Control group data ##
        ctmpdf <- tmpdf
        ctmpdf[, c(ctl_0_col1, ctl_0_col2)]  <- 0

        # Remove unneccessary intercept #
        tmpdf <- tmpdf[, -1]
        ctmpdf <- ctmpdf[, -1]

      }
      if (e == "sgmt") {
        #Get number of periods as a vector to fit in longer data column
        num_pers_long <- rep(num_pers, each=2)
        #Key time points
        time_points <- sort(c(1, a, a - 1, d))
        #interruption weights
        intrup_wt <- rep(c(0, a), each=length(treat_fit_binary)/max(num_pers))
        #Number of columns per each period
        num_cols_per <- expect_coefs/ max(num_pers)
        #Number of rows needed to create each period
        rows_needed <- max(num_pers) * 2
        cells_needed <- rows_needed * (expect_coefs )
        #Treatment group binary predictors
        tmpdf <- data.frame(matrix(rep(0, cells_needed), nrow=rows_needed))
        colnames(tmpdf) <- tmpdf_names
        #Create values for binary predictors
        for(i in 1:rows_needed) {
          tmpdf[i, treat_fit_binary[which(treat_fit_binary < (num_pers_long[i]*num_cols_per))]] <- 1
        }
        # Give values to time point columns
        tmpdf$ITS.Time <- time_points

        #Create values for trend predictors
        for(i in 1:(rows_needed-2)) {
          tmpdf[3:nrow(tmpdf), rep(treat_fit_trend, each=2)[-(1:2)][i] ]  <-
            pmax(tmpdf[3:nrow(tmpdf), "ITS.Time" ] - rep(intrup_wt, each=2)[-(1:2)][i], 0)
        }
        # Remove unneccessary intercept #
        tmpdf <- tmpdf[, -1]

      }
    }
    return(list( treated_df=treated_df,  tmpdf=tmpdf, tp1=tp1, tp2=tp2,
                 cftp1=cftp1, cftp2=cftp2,                    # treatment
                 controls_df=controls_df, ctmpdf=ctmpdf, cp1=cp1, cp2=cp2,
                 cfcp1=cfcp1, cfcp2=cfcp2, time_per=time_per)  # control values
    )
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
         main=main_title, xlab= xlab, ylab=ylab, xlim=xlim, ylim=ylim,
         cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main, axes=axes)
    #Add in confidence bands
    tmpdf_names <- names(coef(cmodel))[1:4][-1]
    if(conf.int==TRUE) {
      tmpdf00 <- data.frame(ITS.Time = 1, post1=0, txp1=0)
      tmpdf01 <- data.frame(ITS.Time = time_per1[2], post1=0, txp1=0)
      tmpdf10 <- data.frame(ITS.Time = time_per2[1], post1=1, txp1=0)
      tmpdf11 <- data.frame(ITS.Time = time_per2[2], post1=1, txp1= (time_per2[2]-interrupt_1))
      colnames(tmpdf00) <- tmpdf_names
      colnames(tmpdf01) <- tmpdf_names
      colnames(tmpdf10) <- tmpdf_names
      colnames(tmpdf11) <- tmpdf_names

      # Margin of Errors #
      #period 1
      moe00 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf00)
      moe01 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf01)
      #period 2
      moe10 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf10)
      moe11 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf11)
      ## treatment ##
      #period 1
      polygon(c(c(1, time_per1[2]), rev(c(1, time_per1[2]))),
              c(t00+moe00, t01+moe01, t01-moe01, t00-moe00),
              col=adjustcolor(lcol[1], alpha.f = adj.alpha),
              border=adjustcolor(lcol[1], alpha.f = adj.alpha))
      #period 2
      polygon(c(c(time_per2[1], time_per2[2]), rev(c(time_per2[1], time_per2[2]))),
              c(t10+moe10, t11+moe11, t11-moe11, t10-moe10),
              col=adjustcolor(lcol[1], alpha.f = adj.alpha),
              border=adjustcolor(lcol[1], alpha.f = adj.alpha))
    }
    #Intervention line
    abline(v=interrupt_1, col= ticol, lty=3, lwd=lwidth)
    lines(time_per1, c(t00, t01), lty=1, col=lcol[1], lwd=lwidth)
    # counterfactual
    if(cfact==TRUE) {
      segments(x0 = interrupt_1, y0 = cft10, x1 = time_per2[2], y1 = cft11, col = lcol[1], lwd = lwidth, lty=2)
    }
    # intervention line
    segments(x0 = interrupt_1, y0 = t10, x1 = time_per2[2], y1 = t11, col = lcol[1], lwd = lwidth, lty=1)
    if (!is.null(add.legend)) {
      if (add.means == TRUE) {
        legend(x=add.legend, legend= if(cfact==TRUE) c(legend[1],  "Counterfactual", "Treated", "Means") else
          c(legend[1],  "Treated", "Means"), lty= if(cfact==TRUE) c(1,2,3, NA) else c(1,3, NA), lwd=cex.legend,
          col= if(cfact==TRUE) c(lcol[1],lcol[1], ticol, "black") else c(lcol[1], ticol, "black"), bty="n",
          cex=cex.legend, pch= if(cfact==TRUE) c(NA,NA,NA,20) else c(NA,NA, 20))
      }
      if (add.means == FALSE) {
        legend(x=add.legend, legend= if(cfact==TRUE) c(legend[1],  "Counterfactual", "Treated") else
          c(legend[1],  "Treated"), lty= if(cfact==TRUE) c(1,2,3) else c(1,3), lwd=cex.legend,
          col= if(cfact==TRUE) c(lcol[1],lcol[1], ticol) else c(lcol[1], ticol), bty="n", cex=cex.legend)
      }
    }
    # Arrows and coefficient names #
    if(any(c(arrow,coefs, name) == TRUE)) {
      ################
      ## 1st Period ##
      ################
      timemidp1 <- mean(time_per1)
      timeqtrp1 <- (timemidp1/2) + 0.5  #start point of arrow, add 0.5 b/c no 0 in X
      #ITS y-axis
      ## Time 1, Pre-intervention ##
      #What to do if the treatment group has the highest pre-intervention scores
      #ITS.int variable name position
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
    }
      ## Period 1 ##
      if (arrow == TRUE) {
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
      }
    ## Group means by time ##
    #Add in group means based on the model data
    if(add.means == TRUE) {
      its_grps <- x$study$group_means_its
      points(its_grps[, 1], its_grps[, 2],
             col=lcol[1], pch=20, cex=cex)
    }
    ## text positions ##
      if(any(c(coefs, name) == TRUE)) {
        possgst <- list("Intercept"=1, "ITS.Time"=4, "post1"=2, "txp1"=4)
        names(possgst)[-1] <- tmpdf_names
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
      text(time_per2[1] + axshift[1], mean(c(t10, cft10)), labels =if(coefs == TRUE) paste0(tmpdf_names[2], "= ", round(coef(cmodel)[3], round.c), model_summary_p[3]) else tmpdf_names[2],
           pos=possgst[[3]], cex=textCEX)
      # txp1
      text(timemidp2, time2thi, labels =if(coefs == TRUE) paste0(tmpdf_names[3],"= ", round(coef(cmodel)[4], round.c), model_summary_p[4]) else tmpdf_names[3],
           pos=possgst[[4]], cex=textCEX)
      }
    #Target line
    if(!is.null(tgt)) {
      for (i in 1:length(tgt)) {
        abline(h=tgt[i], lwd=lwd, col= tgtcol[1], lty=1)
      }
    }
    #Add y-axis value to graph
    if(axes== FALSE) {
      axis(side=1, at=y.at, labels=y.axis, cex.axis=cex.axis)
      axis(2, cex.axis=cex.axis)
      box()
    }
  }

  ##########
  ## sgmt ##
  ##########
  if (model_type == "sgmt") {
    #Model X means
    mod_var_mn <- colMeans(cmodel[["model"]])[-which(names(colMeans(cmodel[["model"]]))==yvar)]
    main_vars <- x[["ITS.Names"]] # main causal model terms
    #Full list of variables for later coefficient names and arrows
    mainvars2 <- c("Intercept", main_vars)
    #Get ITS models coefficient names
    tmpdf_names <- names(coef(cmodel))[1:(length(main_vars)+1)][-1]
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

    #Get the interruption times
    interrupt_all <- x[["study"]][["interrupt"]]
    interrupt_all <- which(aggr_mns[, "time.2.backup.var"] %in% interrupt_all )[1:(length(c(interrupt_all)))]
    #Get maximum time from the model
    max_time <- max(cmodel$model$ITS.Time)
    #Get model coefficient values
    mod_coefs <- stats::coef( cmodel )

    ## Run function for fitted values and counterfactuals ##
    its_fit_mgmt <- fitfnc(x=mod_coefs, a=interrupt_all, B0_adjust=B0_adjust,
                           d=max_time, e=model_type, conf.int=conf.int)

    ## Margins of errors ##
    if(conf.int==TRUE) {
      #Get standard errors based on fitted values of key time points: treatment and controls
      moe <- vector(mode="numeric", length=nrow(its_fit_mgmt$tmpdf))
      #    cmoe <- vector(mode="numeric", length=nrow(its_fit_mgmt$ctmpdf))
      for(i in 1:nrow(its_fit_mgmt$tmpdf)) {
        moe[i] <- qt(.975, nrow(cmodel$model)) * predSE(cmodel , its_fit_mgmt$tmpdf[i, ])
        #      cmoe[i] <- qt(.975, nrow(cmodel$model)) * predSE(cmodel , its_fit_mgmt$ctmpdf[i, ])
      }
      #Make into a matrix for later indexing
      moe <- data.frame(matrix(moe, ncol=2, byrow=T))
      colnames(moe) <- c("start", "stop")
      #   cmoe <- data.frame(matrix(cmoe, ncol=2, byrow=T))
      #    colnames(cmoe) <- c("start", "stop")
    }
    #Get time periods
    time_per <- its_fit_mgmt$time_per

    plot(range(aggr_mns[, 1]), range(c(cmodel[["fitted.values"]], its_fit_mgmt$tp1[1])),
         type="n", main=main_title, xlab= xlab, ylab=ylab, xlim=xlim, ylim=ylim,
         cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main, axes=axes)

    #Add in confidence bands
    if(conf.int==TRUE) {
      ######################
      ## Confidence Bands ##
      ######################
      ## treatment ##
      for (i in 1:nrow(time_per)) {
        polygon(c(c(time_per[i, 1], time_per[i, 2]), rev(c(time_per[i, 1], time_per[i, 2]))),
                c(its_fit_mgmt$tp1[i]+ moe[i, "start"], its_fit_mgmt$tp2[i]+moe[i, "stop"],
                  its_fit_mgmt$tp2[i]-moe[i, "stop"], its_fit_mgmt$tp1[i]- moe[i, "start"]),
                col=adjustcolor(lcol[1], alpha.f = adj.alpha),
                border=adjustcolor(lcol[1], alpha.f = adj.alpha))
      }
    }
    # Intervention treatment period lines #
    for (i in 1:length(interrupt_all)) {
      abline(v= interrupt_all[i], col= ticol, lty=3, lwd=lwidth)
    }
    ##################
    ## Fitted lines ##
    ##################
    ## treatment group fitted lines ##
    for (i in 1:nrow(time_per)) {
      segments(x0 = time_per[i, 1], y0 = its_fit_mgmt$tp1[i], x1 = time_per[i, 2],
               y1 = its_fit_mgmt$tp2[i],
               col = lcol[1], lwd = lwidth, lty=1)
    }
    ##########################
    ## Counterfactual lines ##
    ##########################
    ## treatment group counterfactual lines ##
    if (cfact == TRUE) {
      for (i in 1:(length(its_fit_mgmt$cftp1))) {
        segments(x0 = time_per[i+1, 1], y0 = its_fit_mgmt$cftp1[i],
                 x1 = time_per[i+1, 2], y1 = its_fit_mgmt$cftp2[i],
                 col = lcol[1], lwd = lwidth, lty=2)
      }
    }

    ############
    ## Legend ##
    ############
    if (!is.null(add.legend)) {
      if (add.means == TRUE) {
        legend(x=add.legend, legend= if(cfact==TRUE) c(legend[1], "Counterfactual", "Treated", "Means") else
          c(legend[1],  "Treated", "Means"), lty= if(cfact==TRUE) c(1,2,3, NA) else c(1,3, NA), lwd=cex.legend,
          col= if(cfact==TRUE) c(lcol[1],lcol[1], ticol, "black") else c(lcol[1], ticol, "black"), bty="n",
          cex=cex.legend, pch= if(cfact==TRUE) c(NA,NA,NA,20) else c(NA,NA, 20))
      }
      if (add.means == FALSE) {
        legend(x=add.legend, legend= if(cfact==TRUE) c(legend[1], "Counterfactual", "Treated") else
          c(legend[1],  "Treated"), lty= if(cfact==TRUE) c(1,2,3) else c(1,3), lwd=cex.legend,
          col= if(cfact==TRUE) c(lcol[1],lcol[1], ticol) else c(lcol[1], ticol), bty="n", cex=cex.legend)
      }
    }
    ################################
    # Arrows and coefficient names #
    ################################
    if(any(c(arrow,coefs, name) == TRUE)) {
      #Add a 0 to axshift if length == 1
      if (length(axshift) == 1) {
        axshift <- c(axshift, rep(0, length(c(interrupt_all))-1))
      }
    }
    #Stop it if there aren't enough xshifts
    if(!is.null(xshift)) {
    if(length(xshift) != length(interrupt_all)) {
      stop("Error: Expecting length of xshift to be equal to the number of ITS interruptions.")
    }
    }
    ##################
    ## Slope arrows ##
    ##################
    if (arrow == TRUE) {
      # Treatment group #
      for (i in 1:nrow(time_per)) {
        arrows(x0 = (mean(c(time_per[i, 1], time_per[i, 2])) - 0.5),
               y0 = mean(c(its_fit_mgmt$tp1[i], its_fit_mgmt$tp2[i])),
               x1 = (mean(c(time_per[i, 1], time_per[i, 2])) + 0.5),
               y1 = mean(c(its_fit_mgmt$tp1[i], its_fit_mgmt$tp2[i])), code=2,
               angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      }
      ######################
      ## Intercept arrows ##
      ######################
      # Intercept
      points(1, its_fit_mgmt$tp1[1], col=lcol[2], cex=arwCEX)  # intercept: control group pre-test
      # post #
      for(i in 2:length(its_fit_mgmt$tp1)) {
        arrows(x0 = time_per[[i, 1]] + axshift[i-1], y0 = its_fit_mgmt$tp1[i],
               x1 = time_per[[i, 1]] + axshift[i-1], y1 = its_fit_mgmt$cftp1[i-1], code=1,
               angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      }
    }
    ## Group means by time ##
    #Add in group means based on the model data
    if(add.means == TRUE) {
      its_grps <- x$study$group_means_its
      points(its_grps[, 1], its_grps[, 2],
             col=lcol[1], pch=20, cex=cex)
    }
    ####################
    ## text positions ##
    ####################
    if(any(c(coefs, name) == TRUE)) {
      posmgmt <- rep(list(1), length(tmpdf_names) +1)
      #Change the values to 4 so the default graph is easier to read
      posmgmt[1] <- list(4)
      names(posmgmt)[1] <- "Intercept"
      names(posmgmt)[-1] <- tmpdf_names
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
      ############################
      # Add in coefficient names #
      ############################
      # Intercept
      text(1, its_fit_mgmt$tp1[1], pos=posmgmt[[1]], cex=textCEX,
           labels =if(coefs == TRUE) paste0("Intercept= ", round(mod_coefs[1], round.c),
                                            model_summary_p[1]) else "Intercept")
      # post variables #
      for(i in 2:length(its_fit_mgmt$tp1)) {
        text(time_per[[i, 1]] + axshift[i-1], mean(c(its_fit_mgmt$tp1[i], its_fit_mgmt$cftp1[i-1])),
             labels =if(coefs == TRUE) paste0(mainvars2[seq(3, (length(mainvars2)), by=2)][i-1], "= ",
                                              round(mod_coefs[seq(3, (length(mainvars2)), by=2)][i-1], round.c),
                                              model_summary_p[seq(3, (length(mainvars2)), by=2)][i-1]) else mainvars2[seq(3, (length(mainvars2)), by=2)][i-1],
             pos=posmgmt[seq(3, (length(mainvars2)), by=2)][i-1], cex=textCEX)
      }
      ## Slope text ##
      # Treatment group #
      for(i in 1:length(its_fit_mgmt$tp1)) {
        text(mean(c(time_per[i, 1], time_per[i, 2])),
             mean(c(its_fit_mgmt$tp1[i], its_fit_mgmt$tp2[i])),
             labels =if(coefs == TRUE) paste0(mainvars2[ seq(2, length(mainvars2), by=2)][i], "= ",
                                              round(mod_coefs[seq(2, length(mainvars2), by=2)][i], round.c),
                                              model_summary_p[seq(2, length(mainvars2), by=2)][i]) else
                                                mainvars2[ seq(2, length(mainvars2), by=2)][i],
             pos=posmgmt[ seq(2, length(mainvars2), by=2) ][i], cex=textCEX)
      }
    }
    #Target line
    if(!is.null(tgt)) {
      for (i in 1:length(tgt)) {
        abline(h=tgt[i], lwd=lwd, col= tgtcol[1], lty=1)
      }
    }
    #Add y-axis value to graph
    if(axes== FALSE) {
      axis(side=1, at=y.at, labels=y.axis, cex.axis=cex.axis)
      axis(2, cex.axis=cex.axis)
      box()
    }
  } #End of sgmt

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
    cfc11 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[2] + coef(cmodel)[[3]]*0 + coef(cmodel)[[4]]*0 +
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
    # Treatment group counterfactual for post1
    cft10 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[1] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*time_per2[1] +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0
    cft11 <- coef(cmodel)[[1]] + B0_adjust + coef(cmodel)[[2]]*time_per2[2] + coef(cmodel)[[3]]*1 + coef(cmodel)[[4]]*time_per2[2] +
      coef(cmodel)[[5]]*0 + coef(cmodel)[[6]]*0 + coef(cmodel)[[7]]*0 + coef(cmodel)[[8]]*0
    #ITS.int variable name position
    if(t00 >= c00) {
      posITS.int <- 3
      posIntercept <- 1
    }
    if(c00 > t00) {
      posITS.int <- 1
      posIntercept <- 3
    }

    plot(range(aggr_mns[, 1]), range(c(cmodel[["fitted.values"]], t00)), type="n",
         main=main_title, xlab= xlab, ylab=ylab, xlim=xlim, ylim=ylim,
         cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main, axes=axes)
    #Add in confidence bands
    tmpdf_names <- names(coef(cmodel))[1:8][-1]
    if(conf.int==TRUE) {
      #Treatment group
      tmpdf00 <- data.frame(ITS.Time=time_per1[1], ITS.Int=1, txi=time_per1[1], post1=0, txp1=0, ixp1=0, txip1=0)
      tmpdf01 <- data.frame(ITS.Time=time_per1[2], ITS.Int=1, txi=time_per1[2], post1=0, txp1=0, ixp1=0, txip1=0)
      tmpdf10 <- data.frame(ITS.Time=time_per2[1], ITS.Int=1, txi=time_per2[1], post1=1, txp1=0, ixp1=1, txip1=0)
      tmpdf11 <- data.frame(ITS.Time=time_per2[2], ITS.Int=1, txi=time_per2[2], post1=1, txp1=(time_per2[2]-interrupt_1), ixp1=1, txip1=(time_per2[2]-interrupt_1))
      colnames(tmpdf00) <- tmpdf_names
      colnames(tmpdf01) <- tmpdf_names
      colnames(tmpdf10) <- tmpdf_names
      colnames(tmpdf11) <- tmpdf_names
      #Control group
      ctmpdf00 <- data.frame(ITS.Time=time_per1[1], ITS.Int=0, txi=0, post1=0, txp1=0, ixp1=0, txip1=0)
      ctmpdf01 <- data.frame(ITS.Time=time_per1[2], ITS.Int=0, txi=0, post1=0, txp1=0, ixp1=0, txip1=0)
      ctmpdf10 <- data.frame(ITS.Time=time_per2[1], ITS.Int=0, txi=0, post1=1, txp1=0, ixp1=0, txip1=0)
      ctmpdf11 <- data.frame(ITS.Time=time_per2[2], ITS.Int=0, txi=0, post1=1, txp1=(time_per2[2]-interrupt_1), ixp1=0, txip1=0)
      colnames(ctmpdf00) <- tmpdf_names
      colnames(ctmpdf01) <- tmpdf_names
      colnames(ctmpdf10) <- tmpdf_names
      colnames(ctmpdf11) <- tmpdf_names

      # Margin of Errors #
      # Treatment
      #period 1
      moe00 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf00)
      moe01 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf01)
      #period 2
      moe10 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf10)
      moe11 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, tmpdf11)
      # Control group
      #period 1
      cmoe00 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, ctmpdf00)
      cmoe01 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, ctmpdf01)
      #period 2
      cmoe10 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, ctmpdf10)
      cmoe11 <- qt(.975, nrow(cmodel$model)) * predSE(cmodel, ctmpdf11)
      # Control group
      #period 1
      polygon(c(c(1, time_per1[2]), rev(c(1, time_per1[2]))),
              c(c00+cmoe00, c01+cmoe01, c01-cmoe01, c00-cmoe00),
              col=adjustcolor(lcol[2], alpha.f = adj.alpha),
              border=adjustcolor(lcol[2], alpha.f = adj.alpha))
      #period 2
      polygon(c(c(time_per2[1], time_per2[2]), rev(c(time_per2[1], time_per2[2]))),
              c(c10+cmoe10, c11+cmoe11, c11-cmoe11, c10-cmoe10),
              col=adjustcolor(lcol[2], alpha.f = adj.alpha),
              border=adjustcolor(lcol[2], alpha.f = adj.alpha))
      ## treatment ##
      #period 1
      polygon(c(c(1, time_per1[2]), rev(c(1, time_per1[2]))),
              c(t00+moe00, t01+moe01, t01-moe01, t00-moe00),
              col=adjustcolor(lcol[1], alpha.f = adj.alpha),
              border=adjustcolor(lcol[1], alpha.f = adj.alpha))
      #period 2
      polygon(c(c(time_per2[1], time_per2[2]), rev(c(time_per2[1], time_per2[2]))),
              c(t10+moe10, t11+moe11, t11-moe11, t10-moe10),
              col=adjustcolor(lcol[1], alpha.f = adj.alpha),
              border=adjustcolor(lcol[1], alpha.f = adj.alpha))
    }
    #Intervention line
    abline(v= interrupt_1, col= ticol, lty=3, lwd=lwidth)
    # Intervention counterfactual
    if(cfact==TRUE) {
      segments(x0 = interrupt_1, y0 = cft10, x1 = time_per2[2], y1 = cft11, col = lcol[1], lwd = lwidth, lty=2)
    }
    # control period 1
    lines(time_per1, c(c00, c01), lty=1, col=lcol[2], lwd=lwidth)
    # control counterfactual
    if(cfact==TRUE) {
      segments(x0 = interrupt_1, y0 = cfc10, x1 = time_per2[2], y1 = cfc11, col = lcol[2], lwd = lwidth, lty=2)
    }
    # control line period 2
    segments(x0 = interrupt_1, y0 = c10, x1 = time_per2[2], y1 = c11, col = lcol[2], lwd = lwidth, lty=1)
    # intervention period 1
    lines(time_per1, c(t00, t01), lty=1, col=lcol[1], lwd=lwidth)
    # intervention line period 2
    segments(x0 = interrupt_1, y0 = t10, x1 = time_per2[2], y1 = t11, col = lcol[1], lwd = lwidth, lty=1)
    ## Legend ##
    if (!is.null(add.legend)) {
      if (add.means == TRUE) {
        legend(x=add.legend, legend= if(cfact==TRUE) c(legend[1], legend[2],"Counterfactual","Counterfactual", "Treated", "Means") else
          c(legend[1], legend[2], "Treated", "Means"), lty= if(cfact==TRUE) c(1,1,2,2,3, NA) else c(1,1,3, NA), lwd=cex.legend,
          col= if(cfact==TRUE) c(lcol[1],lcol[2],lcol[1],lcol[2], ticol, "black") else c(lcol[1], lcol[2],ticol, "black"), bty="n",
          cex=cex.legend, pch= if(cfact==TRUE) c(NA,NA,NA,NA,NA,20) else c(NA,NA,NA, 20))
      }
      if (add.means == FALSE) {
        legend(x=add.legend, legend= if(cfact==TRUE) c(legend[1], legend[2],"Counterfactual","Counterfactual", "Treated") else
          c(legend[1], legend[2], "Treated"), lty= if(cfact==TRUE) c(1,1,2,2,3) else c(1,1,3), lwd=cex.legend,
          col= if(cfact==TRUE) c(lcol[1],lcol[2],lcol[1],lcol[2], ticol) else c(lcol[1],lcol[2], ticol), bty="n", cex=cex.legend)
      }
    }
    # Arrows and coefficient names #
    if(any(c(arrow,coefs, name) == TRUE)) {
      ################
      ## 1st Period ##
      ################
      timemidp1 <- mean(time_per1)
      timeqtrp1 <- (timemidp1/2) + 0.5  #start point of arrow, add 0.5 b/c no 0 in X
      #ITS y-axis
      ## Time 1, Pre-intervention ##
      #What to do if the treatment group has the highest pre-intervention scores
      if(t00 >= c00) {
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
    }
      ## Period 1 ##
    if (arrow == TRUE) {
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
    }
    ## Group means by time ##
    #Add in group means based on the model data
    if(add.means == TRUE) {
      its_grps <- x$study$group_means_its
      points(its_grps[its_grps[, 2]==0, 1], its_grps[its_grps[, 2]==0, 3],
             col=lcol[2], pch=20, cex=cex)
      points(its_grps[its_grps[, 2]==1, 1], its_grps[its_grps[, 2]==1, 3],
             col=lcol[1], pch=20, cex=cex)
    }
      ## text positions ##
    if(any(c(coefs, name) == TRUE)) {
      posmgst <- list("Intercept"=posIntercept, "ITS.Time"=4, "ITS.Int"=posITS.int, "txi"=4,
                      "post1"=4, "txp1"=4, "ixp1"=4, "txip1"=4)
      names(posmgst)[-1] <- tmpdf_names
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
      text(time_per2[1], mean(c(c10, cfc10)), labels =if(coefs == TRUE) paste0(tmpdf_names[4], "= ", round(coef(cmodel)[5], round.c), model_summary_p[5]) else tmpdf_names[4],
           pos=posmgst[[5]], cex=textCEX)
      # txp1
      text(timemidp2, time2chi, labels =if(coefs == TRUE) paste0(tmpdf_names[5], "= ", round(coef(cmodel)[6], round.c), model_summary_p[6]) else tmpdf_names[5],
           pos=posmgst[[6]], cex=textCEX)
      # ixp1
      text(time_per2[1] + axshift[1], mean(c(c10, t10)), labels =if(coefs == TRUE) paste0(tmpdf_names[6], "= ", round(coef(cmodel)[7], round.c), model_summary_p[7]) else tmpdf_names[6],
           pos=posmgst[[7]], cex=textCEX)
      # txip1
      text(timemidp2, time2thi, labels =if(coefs == TRUE) paste0(tmpdf_names[7], "= ", round(coef(cmodel)[8], round.c), model_summary_p[8]) else tmpdf_names[7],
           pos=posmgst[[8]], cex=textCEX)
    }
    #Target line
    if(!is.null(tgt)) {
      for (i in 1:length(tgt)) {
        abline(h=tgt[i], lwd=lwd, col= tgtcol[1], lty=1)
      }
    }
    #Add y-axis value to graph
    if(axes== FALSE) {
      axis(side=1, at=y.at, labels=y.axis, cex.axis=cex.axis)
      axis(2, cex.axis=cex.axis)
      box()
    }
  }
  ##########
  ## mgmt ##
  ##########
  if (model_type == "mgmt") {
    #Model X means
    mod_var_mn <- colMeans(cmodel[["model"]])[-which(names(colMeans(cmodel[["model"]]))==yvar)]
    main_vars <- x[["ITS.Names"]] # main causal model terms
    #Full list of variables for later coefficient names and arrows
    mainvars2 <- c("Intercept", main_vars)
    #Get ITS models coefficient names
    tmpdf_names <- names(coef(cmodel))[1:(length(main_vars)+1)][-1]
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

    #Get the interruption times
    interrupt_all <- x[["study"]][["interrupt"]]
    interrupt_all <- which(aggr_mns[, "time.2.backup.var"] %in% interrupt_all )[1:(length(c(interrupt_all)))]
    #Get maximum time from the model
    max_time <- max(cmodel$model$ITS.Time)
    #Get model coefficient values
    mod_coefs <- stats::coef( cmodel )

    ## Run function for fitted values and counterfactuals ##
    its_fit_mgmt <- fitfnc(x=mod_coefs, a=interrupt_all, B0_adjust=B0_adjust,
                           d=max_time, e=model_type, conf.int=conf.int)

    ## Margins of errors ##
    if(conf.int==TRUE) {
      #Get standard errors based on fitted values of key time points: treatment and controls
      moe <- vector(mode="numeric", length=nrow(its_fit_mgmt$tmpdf))
      cmoe <- vector(mode="numeric", length=nrow(its_fit_mgmt$ctmpdf))
      for(i in 1:nrow(its_fit_mgmt$tmpdf)) {
        moe[i] <- qt(.975, nrow(cmodel$model)) * predSE(cmodel , its_fit_mgmt$tmpdf[i, ])
        cmoe[i] <- qt(.975, nrow(cmodel$model)) * predSE(cmodel , its_fit_mgmt$ctmpdf[i, ])
      }
      #Make into a matrix for later indexing
      moe <- data.frame(matrix(moe, ncol=2, byrow=T))
      colnames(moe) <- c("start", "stop")
      cmoe <- data.frame(matrix(cmoe, ncol=2, byrow=T))
      colnames(cmoe) <- c("start", "stop")
    }
    #Get time periods
    time_per <- its_fit_mgmt$time_per

    plot(range(aggr_mns[, 1]), range(c(cmodel[["fitted.values"]], its_fit_mgmt$tp1[1])),
         type="n", main=main_title, xlab= xlab, ylab=ylab, xlim=xlim, ylim=ylim,
         cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main, axes=axes)

    #Add in confidence bands
    if(conf.int==TRUE) {
      ######################
      ## Confidence Bands ##
      ######################
      ## controls ##
      for (i in 1:nrow(time_per)) {
        polygon(c(c(time_per[i, 1], time_per[i, 2]), rev(c(time_per[i, 1], time_per[i, 2]))),
                c(its_fit_mgmt$cp1[i]+cmoe[i, "start"], its_fit_mgmt$cp2[i]+cmoe[i, "stop"],
                  its_fit_mgmt$cp2[i]-cmoe[i, "stop"], its_fit_mgmt$cp1[i]-cmoe[i, "start"]),
                col=adjustcolor(lcol[2], alpha.f = adj.alpha),
                border=adjustcolor(lcol[2], alpha.f = adj.alpha))
      }
      ## treatment ##
      for (i in 1:nrow(time_per)) {
        polygon(c(c(time_per[i, 1], time_per[i, 2]), rev(c(time_per[i, 1], time_per[i, 2]))),
                c(its_fit_mgmt$tp1[i]+ moe[i, "start"], its_fit_mgmt$tp2[i]+moe[i, "stop"],
                  its_fit_mgmt$tp2[i]-moe[i, "stop"], its_fit_mgmt$tp1[i]- moe[i, "start"]),
                col=adjustcolor(lcol[1], alpha.f = adj.alpha),
                border=adjustcolor(lcol[1], alpha.f = adj.alpha))
      }
    }
    # Intervention treatment period lines #
    for (i in 1:length(interrupt_all)) {
      abline(v= interrupt_all[i], col= ticol, lty=3, lwd=lwidth)
    }
    ##################
    ## Fitted lines ##
    ##################
    ## control group fitted lines ##
    for (i in 1:nrow(time_per)) {
      segments(x0 = time_per[i, 1], y0 = its_fit_mgmt$cp1[i], x1 = time_per[i, 2],
               y1 = its_fit_mgmt$cp2[i],
               col = lcol[2], lwd = lwidth, lty=1)
    }
    ## treatment group fitted lines ##
    for (i in 1:nrow(time_per)) {
      segments(x0 = time_per[i, 1], y0 = its_fit_mgmt$tp1[i], x1 = time_per[i, 2],
               y1 = its_fit_mgmt$tp2[i],
               col = lcol[1], lwd = lwidth, lty=1)
    }
    ##########################
    ## Counterfactual lines ##
    ##########################
    ## control group counterfactual lines ##
    if (cfact == TRUE) {
    for (i in 1:(length(its_fit_mgmt$cfcp1))) {
      segments(x0 = time_per[i+1, 1], y0 = its_fit_mgmt$cfcp1[i],
               x1 = time_per[i+1, 2], y1 = its_fit_mgmt$cfcp2[i],
               col = lcol[2], lwd = lwidth, lty=2)
      }
    }
    ## treatment group counterfactual lines ##
    if (cfact == TRUE) {
      for (i in 1:(length(its_fit_mgmt$cftp1))) {
      segments(x0 = time_per[i+1, 1], y0 = its_fit_mgmt$cftp1[i],
               x1 = time_per[i+1, 2], y1 = its_fit_mgmt$cftp2[i],
               col = lcol[1], lwd = lwidth, lty=2)
      }
    }

    ############
    ## Legend ##
    ############
    if (!is.null(add.legend)) {
      if (add.means == TRUE) {
        legend(x=add.legend, legend= if(cfact==TRUE) c(legend[1], legend[2],"Counterfactual","Counterfactual", "Treated", "Means") else
          c(legend[1], legend[2], "Treated", "Means"), lty= if(cfact==TRUE) c(1,1,2,2,3, NA) else c(1,1,3, NA), lwd=cex.legend,
          col= if(cfact==TRUE) c(lcol[1],lcol[2],lcol[1],lcol[2], ticol, "black") else c(lcol[1], lcol[2],ticol, "black"), bty="n",
          cex=cex.legend, pch= if(cfact==TRUE) c(NA,NA,NA,NA,NA,20) else c(NA,NA,NA, 20))
      }
      if (add.means == FALSE) {
        legend(x=add.legend, legend= if(cfact==TRUE) c(legend[1], legend[2],"Counterfactual","Counterfactual", "Treated") else
          c(legend[1], legend[2], "Treated"), lty= if(cfact==TRUE) c(1,1,2,2,3) else c(1,1,3), lwd=cex.legend,
          col= if(cfact==TRUE) c(lcol[1],lcol[2],lcol[1],lcol[2], ticol) else c(lcol[1], lcol[2],ticol), bty="n", cex=cex.legend)
      }
    }
    ################################
    # Arrows and coefficient names #
    ################################
    if(any(c(arrow,coefs, name) == TRUE)) {
      #Add a 0 to axshift if length == 1
      if (length(axshift) == 1) {
        axshift <- c(axshift, rep(0, length(c(interrupt_all))-1))
      }
      #Stop it if there aren't enough xshifts
      if(!is.null(xshift)) {
        if(length(xshift) != length(interrupt_all)) {
          stop("Error: Expecting length of xshift to be equal to the number of ITS interruptions.")
        }
      }
    }
    ##################
    ## Slope arrows ##
    ##################
    if (arrow == TRUE) {
      # Control group #
      for (i in 1:nrow(time_per)) {
        arrows(x0 = (mean(c(time_per[i, 1], time_per[i, 2])) - 0.5),
               y0 = mean(c(its_fit_mgmt$cp1[i], its_fit_mgmt$cp2[i])),
               x1 = (mean(c(time_per[i, 1], time_per[i, 2])) + 0.5),
               y1 = mean(c(its_fit_mgmt$cp1[i], its_fit_mgmt$cp2[i])), code=2,
               angle=30, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      }
      # Treatment group #
      for (i in 1:nrow(time_per)) {
        arrows(x0 = (mean(c(time_per[i, 1], time_per[i, 2])) - 0.5),
               y0 = mean(c(its_fit_mgmt$tp1[i], its_fit_mgmt$tp2[i])),
               x1 = (mean(c(time_per[i, 1], time_per[i, 2])) + 0.5),
               y1 = mean(c(its_fit_mgmt$tp1[i], its_fit_mgmt$tp2[i])), code=2,
               angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      }
      ######################
      ## Intercept arrows ##
      ######################
      # Intercept
      points(1, its_fit_mgmt$cp1[1], col=lcol[2], cex=arwCEX)  # intercept: control group pre-test
      # ITS.Int #
      arrows(x0 = 1, y0 = its_fit_mgmt$cp1[1], x1 = 1, y1 = its_fit_mgmt$tp1[1],
             code=2, #code=arrow_code[2],  #need arrow, vertical line
             angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      # post variables #
      for(i in 2:length(its_fit_mgmt$tp1)) { #using tp1 to make easier for "sgmt"
        arrows(x0 = time_per[[i, 1]], y0 = its_fit_mgmt$cfcp1[i-1],
               x1 = time_per[[i, 1]], y1 = its_fit_mgmt$cp1[i], code=2,
               angle=30, length=.25, col = lcol[2], lwd = arwCEX, lty=3)
      }
      # ixp1 # start at time 2
      for(i in 2:length(its_fit_mgmt$tp1)) {
        arrows(x0 = time_per[[i, 1]] + axshift[i-1], y0 = its_fit_mgmt$tp1[i],
               x1 = time_per[[i, 1]] + axshift[i-1], y1 = its_fit_mgmt$cp1[i], code=3,
               angle=30, length=.25, col = lcol[1], lwd = arwCEX, lty=3)
      }
    }
    ## Group means by time ##
    #Add in group means based on the model data
    if(add.means == TRUE) {
      its_grps <- x$study$group_means_its
      points(its_grps[its_grps[, 2]==0, 1], its_grps[its_grps[, 2]==0, 3],
             col=lcol[2], pch=20, cex=cex)
      points(its_grps[its_grps[, 2]==1, 1], its_grps[its_grps[, 2]==1, 3],
             col=lcol[1], pch=20, cex=cex)
    }
    ####################
    ## text positions ##
    ####################
    if(any(c(coefs, name) == TRUE)) {
      posmgmt <- rep(list(1), length(tmpdf_names) +1)
      #Change the values to 4 so the default graph is easier to read
      posmgmt[1:4] <- list(4,4,4,4)
      names(posmgmt)[1] <- "Intercept"
      names(posmgmt)[-1] <- tmpdf_names
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
      ############################
      # Add in coefficient names #
      ############################
      # Intercept
      text(1, its_fit_mgmt$cp1[1], pos=posmgmt[[1]], cex=textCEX,
           labels =if(coefs == TRUE) paste0("Intercept= ", round(mod_coefs[1], round.c),
                                            model_summary_p[1]) else "Intercept")
      # ITS.Int #
      text(1, its_fit_mgmt$tp1[1], pos=posmgmt[[3]], cex=textCEX,
           labels =if(coefs == TRUE) paste0("ITS.Int= ", round(mod_coefs[3], round.c),
                                            model_summary_p[3]) else "ITS.Int")
      # post variables #
      for(i in 2:length(its_fit_mgmt$tp1)) {
        text(time_per[[i, 1]], mean(c(its_fit_mgmt$cp1[i], its_fit_mgmt$cfcp1[i-1])),
             labels =if(coefs == TRUE) paste0(mainvars2[seq(1, (length(mainvars2)), by=4)][i], "= ",
                                              round(mod_coefs[seq(1, (length(mainvars2)), by=4)][i], round.c),
                                              model_summary_p[seq(1, (length(mainvars2)), by=4)][i]) else mainvars2[seq(1, (length(mainvars2)), by=4)][i],
             pos=posmgmt[seq(1, (length(mainvars2)), by=4)][i], cex=textCEX)
      }
      # ixp variables #
      for(i in 2:length(its_fit_mgmt$tp1)) {
        text(time_per[[i, 1]] + axshift[i-1], mean(c(its_fit_mgmt$tp1[i], its_fit_mgmt$cp1[i])),
             labels =if(coefs == TRUE) paste0(mainvars2[seq(7, (length(mainvars2)), by=4)][i-1], "= ",
                                              round(mod_coefs[seq(7, (length(mainvars2)), by=4)][i-1], round.c),
                                              model_summary_p[seq(7, (length(mainvars2)), by=4)][i-1]) else mainvars2[seq(7, (length(mainvars2)), by=4)][i-1],
             pos=posmgmt[seq(7, (length(mainvars2)), by=4)][i-1], cex=textCEX)
      }
      ## Slope text ##
      # Control group #
      for(i in 1:length(its_fit_mgmt$cp1)) {
        text(mean(c(time_per[i, 1], time_per[i, 2])),
             mean(c(its_fit_mgmt$cp1[i], its_fit_mgmt$cp2[i])),
             labels =if(coefs == TRUE) paste0(mainvars2[ seq(2, length(mainvars2), by=4)][i], "= ",
                                              round(mod_coefs[seq(2, length(mainvars2), by=4)][i], round.c),
                                              model_summary_p[seq(2, length(mainvars2), by=4)][i]) else
                                                mainvars2[ seq(2, length(mainvars2), by=4)][i],
             pos=posmgmt[ seq(2, length(mainvars2), by=4) ][i], cex=textCEX)
      }
      # Treatment group #
      for(i in 1:length(its_fit_mgmt$tp1)) {
        text(mean(c(time_per[i, 1], time_per[i, 2])),
             mean(c(its_fit_mgmt$tp1[i], its_fit_mgmt$tp2[i])),
             labels =if(coefs == TRUE) paste0(mainvars2[ seq(4, length(mainvars2), by=4)][i], "= ",
                                              round(mod_coefs[seq(4, length(mainvars2), by=4)][i], round.c),
                                              model_summary_p[seq(4, length(mainvars2), by=4)][i]) else
                                                mainvars2[ seq(4, length(mainvars2), by=4)][i],
             pos=posmgmt[ seq(4, length(mainvars2), by=4) ][i], cex=textCEX)
      }
    }
    #Target line
    if(!is.null(tgt)) {
      for (i in 1:length(tgt)) {
        abline(h=tgt[i], lwd=lwd, col= tgtcol[1], lty=1)
      }
    }
    #Add y-axis value to graph
    if(axes== FALSE) {
      axis(side=1, at=y.at, labels=y.axis, cex.axis=cex.axis)
      axis(2, cex.axis=cex.axis)
      box()
    }
  } #End of mgmt

}

