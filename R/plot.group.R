#' Confidence interval graphs for group class objects
#'
#' @param x group object.
#' @param y type of confidence interval object, specify either 'group', 'time', or 'roll'.
#' @param order specify confidence interval object order as 'alpha' or 'numeric' for alphabetical or numerical ordering in the 'group' graph.
#' @param gcol pick confidence interval line colors for groups in the 'group' graph. Default is 'blue'.
#' @param gbar logical TRUE or FALSE that indicates whether group lines have confidence bars for trend over time results. Default is FALSE.
#' @param pcol select point color for 'group' only confidence intervals. Default is 'red'.
#' @param overall logical TRUE or FALSE that indicates whether to include the overall sample confidence intervals (i.e., not each group). Default is FALSE.
#' @param ocol indicate the optional overall line color. Default is 'gray' when overall=TRUE.
#' @param obar logical TRUE or FALSE that indicates whether to add overall confidence band. Default is FALSE.
#' @param tgt specify 1 or more values on the x-axis of where to add a target line. Default is NULL.
#' @param tcol select a color for the target line. Default is 'gray'.
#' @param tpline add one or time point vertical line(s) using x-axis values when y='time' or y='roll'. Default is NULL.
#' @param tpcol specify a color for the time point line, tpline. Default is NULL.
#' @param xlim specify plot's x-axis limits with a 2 value vector.
#' @param ylim specify plot's y-axis limits with a 2 value vector.
#' @param main the main title of the plot.
#' @param lwd select the line width. Default is 1.
#' @param adj.alpha factor modifying the opacity alpha of the confidence interval bars, in the range of 0 to 1. Default is 0.4.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default of 1.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex. Default is 1.
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex. Default is 1.
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex. Default is 1.
#' @param cex.text The magnification to be used for the text added into the plot relative to the current setting of 1.
#' @param round.c an integer indicating the number of decimal places. Default is 2.
#' to be used for rounding coefficient values.
#' @param name logical TRUE or FALSE that indicates whether group names
#' should be added to the 'time' or 'roll' plots. Default is FALSE.
#' @param abbrv the minimum length of the abbreviations. Default is 5.
#' @param ... additional arguments.
#'
#' @return plot of group level confidence intervals, including estimates over time periods.
#' @importFrom graphics box title
#' @export
#'
#' @examples
#' #Simple graph for confidence intervals using the t-distribution
#' gr1 <- group(x="program", y="los", z="month", dataf=hosprog, dist="t",
#' increment=3, rolling=6)
#' # Group level confidence intervals
#' plot(x=gr1, y="group", order="numeric", lwd=4, gcol= "blue", pcol="red",
#' overall=TRUE, obar=TRUE, ocol="gray", tcol="green", tgt=4.5,
#' cex=1, cex.axis=1, cex.lab=1, cex.text=2,
#' cex.main=1.25, name=TRUE, adj.alpha=.2)
#' #Trend plots over time in the 3 month increments (i.e., quarters)
#' plot(x=gr1, y="time", lwd=4, gcol=c("red", "blue"), gbar=TRUE, overall=TRUE,
#'   obar=TRUE, ocol="gray", tcol="green", tgt=4, tpline=3,
#'   tpcol="yellow", name=TRUE, cex.axis=1, cex.lab=1, cex.text=2,
#'   cex.main=1.25, adj.alpha=.3)
#' #Plot for rolling 6-month averages
#' plot(x=gr1, y="roll", lwd=4, gcol=c("red", "blue"), gbar=TRUE, overall=TRUE,
#'   obar=TRUE, ocol="gray", tcol="green", tgt=4, tpline=c(4,6),
#'   tpcol="yellow", name=TRUE, cex.axis=1, cex.lab=1, cex.text=2,
#'   cex.main=1.25, adj.alpha=.3)


plot.group <- function(x, y="group", order="alpha", gcol="blue", gbar=FALSE, pcol="red", overall=FALSE,
                       ocol="gray", obar=FALSE, tgt=NULL, tcol="gray", tpline=NULL,
                       tpcol="gray", xlim=NULL, ylim=NULL, main=NULL, lwd=1, adj.alpha=0.4,
                       cex=1, cex.axis=1, cex.lab=1, cex.main=1, cex.text=1, round.c=2,
                       name=FALSE, abbrv=5, ...) {
  if(any(is.null(c(x, y)) == TRUE)) {
    stop("Error: Expecting both an x and y argument.")
  }
  if (any(class(x) == "group") == FALSE) {stop("Error: Expecting group class object." )}
  if (!y %in% c("group", "time", "roll")) {stop("Error: Expecting y='group', y='time', or y='roll'." )}

  ##########################################
  # Plot function for confidence intervals #
  ##########################################
  plot_ci_fnc <- function(x, alpha_num, main, lwd, Lcol, Pcol, tgt, Cbar,
                          roundVal, adj.alpha, xlim, ylim, abbrv, ocol, tcol,
                          cex, cex.axis, cex.lab, cex.main) {
    # Get analysis info #
    xcivar <- x[["Variables"]][["x"]]
    ycivar <- x[["Variables"]][["y"]]
    ciconf_lev <- x[["CI"]]
  # Get data frames #
    #Overall
    ydf <- x[["Group.CI"]][["adf_all"]]
    #Main group data
    cidf <- x[["Group.CI"]]
    #Get data in alphabetical or numerical order
    if (alpha_num=="alpha") {
      adf <- cidf$adf_alpha
    }
    if (alpha_num=="numeric") {
      adf <- cidf$adf_numeric
    }
      mainYmn <- ydf$PointEst
    #Main title
    if(!is.null(main) ) {
      main_ttl <- main
    } else {
      main_ttl <- paste0(ciconf_lev * 100, "% ", "Confidence Intervals of ", ycivar, " by ", xcivar)
    }
    rng <- seq(min(adf[, 3:4]), max(adf[, 3:4]),length.out=nrow(adf))
    plot(rng, 1:nrow(adf), type="n", ylab="",
         xlab= paste0("Value (vertical line = overall mean of ", round(mainYmn, roundVal), ", ", ciconf_lev * 100, "% ", "CI",
                      " [", round(cidf[["adf_all"]][,"Lower"], roundVal), ", ", round(cidf[["adf_all"]][,"Upper"], roundVal),"]",")"),
         #main=main_ttl,
         axes=F,  cex.lab=cex.lab, xlim=xlim, ylim=ylim)
    title(main_ttl, cex.main = cex.main)
    for (i in 1:nrow(adf)) {
      lines(c(adf[,'Lower'][i], adf[,'Upper'][i]), c(i,i), lwd=lwd, col=Lcol)
      points(adf[,'PointEst'][i],i, pch=24, col=Pcol, bg=Pcol, cex=cex)
    }
    #Mean line
    if(overall == TRUE) {
      abline(v=mainYmn, lwd=lwd, col= ocol, lty=3)
    }
    #Target line
    if(!is.null(tgt)) {
      for (i in 1:length(tgt)) {
        abline(v=tgt[i], lwd=lwd, col= tcol, lty=1)
        }
      }
  axis(1)
    axis(2,at=1:nrow(adf),labels= abbreviate(rownames(adf), minlength=abbrv), las=1, cex.axis=cex.axis )
    axis(4,at=1:nrow(adf),labels=round(adf[, "PointEst"], roundVal), las=1, cex.axis= cex.axis)
    ## Add confidence bar ##
    #Create x and y data
    cidf[["adf_all"]][,"Lower"]
    Cbar_x <- c(rep(cidf[["adf_all"]][,"Lower"], nrow(adf)), rep(cidf[["adf_all"]][,"Upper"], nrow(adf)))
    Cbar_y <- c(1:nrow(adf), nrow(adf):1)
    #Create shading
    if(Cbar== TRUE) {
      polygon(Cbar_x, Cbar_y, col= adjustcolor(ocol, alpha.f = adj.alpha), border= ocol )
    }
    box()
  }
  ############################################################
  ##            Function to create the time plot            ##
  ############################################################
  plot_fci_fnc <- function(x, y, xlim, ylim, lwd, LCol, cibands, ocibands,
                           Tot.Line, Tot.Color, Tgt.Color, Tgt.Line,
                           Time.Pt.Line, Tpt.Color, cex, cex.axis, cex.lab,
                           cex.main, cex.text, name, abbrv, adj.alpha) {

    # Get analysis info #
    xcivar <- x[["Variables"]][["x"]]
    ycivar <- x[["Variables"]][["y"]]
    zcivar <- x[["Variables"]][["z"]]
    Conf.Intrv <- x[["CI"]]
    # Get data frames #
    #Time data
    if(y== "time") {
      cidf <- x[["Time.CI"]][[1]]     #by time
      cidf_tot <- x[["Time.CI"]][[2]] #Overall
    }
    #Rolling data
    if(y== "roll") {
      cidf <- x[["Roll.CI"]][[1]]     #by time
      cidf_tot <- x[["Roll.CI"]][[2]] #Overall
      colnames(cidf)[which(colnames(cidf) %in% c("Group", "Start"))] <- c("x_lev","z_lev")
      colnames(cidf_tot)[which(colnames(cidf_tot) == "Start")] <- "z_lev"
    }
    #Unique levels
    ctrs <- sort(unique(cidf[["x_lev"]]))
    #Time periods
    times <- sort(unique(cidf[["z_lev"]]))
    #Get confidence intervals for graphing
    min_ci <- cidf[["Lower"]]
    max_ci <- cidf[["Upper"]]
    # Get even and odd numbered groups #
    #Number of centers
    ctrs_nums <- 1:length(ctrs)
    # Even numbers
    even_groups <- ctrs_nums[ctrs_nums %% 2 == 0]
    # Odd numbers
    odd_groups <- ctrs_nums[ctrs_nums %% 2 != 0]

    #Make text out of the confidence level
    ConINT <- paste0(as.character(Conf.Intrv*100), "%")
    #Main title
    if(cibands == TRUE) {
      Main.Title <- paste0( ycivar, " trajectories per ", xcivar,  " by ",
                            zcivar, " with ", ConINT, " confidence bands")
    } else {
      Main.Title <- paste0( ycivar, " trajectories per ", xcivar,  " by ", zcivar)
    }
    #Set up colors
    my_clr <- rep_len(LCol, length.out= length(ctrs))
    plot(times, seq(min(min_ci, na.rm=T), max(max_ci, na.rm=T),
                    length.out=length(times)), type="n",cex.lab=cex.lab,
         axes=F, ylab=ycivar, xlab=zcivar, xlim=xlim, ylim=ylim )
    title(Main.Title, cex.main = cex.main)
    axis(1, las=1, cex.axis=cex.axis )
    axis(2, las=3, cex.axis=cex.axis )
    box()
    #Confidence bands for group lines
    if(cibands == TRUE) {
      for (i in 1:length(ctrs)) {
        xx_t <- c( cidf[cidf$x_lev == ctrs[i], "z_lev"], rev(cidf[cidf$x_lev == ctrs[i], "z_lev"]))
        yy_t <- c(cidf[cidf$x_lev == ctrs[i], "Lower"], rev(cidf[cidf$x_lev == ctrs[i], "Upper"]))
        polygon(xx_t, yy_t,
                col = adjustcolor(my_clr[i], alpha.f = adj.alpha),
                border=adjustcolor(my_clr[i], alpha.f = adj.alpha))
        lines(cidf[cidf$x_lev == ctrs[i], "z_lev"] , cidf[cidf$x_lev == ctrs[i], "PointEst"],
              col=my_clr[i], lwd=lwd)
      }
    }
    if(cibands == FALSE) {
      for (i in 1:length(ctrs)) {
        lines(cidf[cidf$x_lev == ctrs[i], "z_lev"] , cidf[cidf$x_lev == ctrs[i], "PointEst"],
              lwd=lwd, col=my_clr[i])
      }
    }
    #Add text names
    if(name == TRUE) {
      #Odd
      for (i in odd_groups) {
        text(x=cidf[cidf$x_lev == ctrs[i], "z_lev"][which(times == min(cidf[cidf$x_lev == ctrs[i], "z_lev"], na.rm=T))],
             y=cidf[cidf$x_lev == ctrs[i], "PointEst"][which(times == min(cidf[cidf$x_lev == ctrs[i], "z_lev"], na.rm=T))],
             labels= abbreviate(ctrs[i], minlength=abbrv), cex= cex.text, col=my_clr[i])
      }
      #Evens
      for (i in even_groups) {
        text(x=cidf[cidf$x_lev == ctrs[i], "z_lev"][which(times == max(cidf[cidf$x_lev == ctrs[i], "z_lev"], na.rm=T))],
             y=cidf[cidf$x_lev == ctrs[i], "PointEst"][which(times == max(cidf[cidf$x_lev == ctrs[i], "z_lev"], na.rm=T))],
             labels= abbreviate(ctrs[i], minlength=abbrv), cex= cex.text, col=my_clr[i])
      }
    }
    ## Overall total lines ##
    #Confidence bands
    if (Tot.Line == TRUE ) {
      if (ocibands == TRUE ) {
        xx_t <- c( cidf_tot[, "z_lev"], rev(cidf_tot[, "z_lev"]))
        yy_t <- c(cidf_tot[, "Lower"], rev(cidf_tot[, "Upper"]))
        polygon(xx_t, yy_t,
                col = adjustcolor(Tot.Color, alpha.f = adj.alpha),
                border=adjustcolor(Tot.Color, alpha.f = adj.alpha))
        lines(cidf_tot[, "z_lev"] , cidf_tot[, "PointEst"], col=Tot.Color, lwd=lwd)
      }
    }
    #Add overall lines
    if (Tot.Line == TRUE ) {
      if (ocibands == FALSE ) {
        lines(cidf_tot[, "z_lev"] , cidf_tot[, "PointEst"], col=Tot.Color, lwd=lwd)
      }
    }
    #Add overall text names
    if(Tot.Line == TRUE) {
      if(name == TRUE) {
      text(x=cidf_tot[1, "z_lev"], y=cidf_tot[1, "PointEst"],
           labels= "All", cex= cex.text, col=Tot.Color)
      }
    }
    #Add target line
    if(!is.null(Tgt.Line)) {
    for (i in 1:length(Tgt.Line)) {
      abline(h= as.numeric(eval(parse(text=Tgt.Line[i] )) ),
             col=Tgt.Color, lty=3, lwd=lwd)
    }
    }
    #Add time point line
    if(!is.null(Time.Pt.Line)) {
    for (i in 1:length(Time.Pt.Line)) {
      abline(v= as.numeric(eval(parse(text=Time.Pt.Line[i] )) ),
             col=Tpt.Color, lty=1, lwd=lwd)
    }
    }
  }

  # Run the functions above #
  switch(y,
         "group"   = plot_ci_fnc(x=x, alpha_num=order, main=main, lwd=lwd, Lcol=gcol, Pcol=pcol, tgt=tgt, Cbar=obar,
                                 roundVal=round.c, adj.alpha=adj.alpha, xlim=xlim, ylim=ylim, abbrv=abbrv, ocol=ocol,
                                 tcol=tcol, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main),
         "time"   = plot_fci_fnc(x=x, y=y, xlim=xlim, ylim=ylim, lwd=lwd, LCol=gcol,
                                 cibands=gbar, ocibands=obar, Tot.Line=overall, Tot.Color=ocol, Tgt.Color=tcol,
                                 Tgt.Line=tgt, Time.Pt.Line=tpline, Tpt.Color=tpcol, cex=cex, cex.axis=cex.axis,
                                 cex.lab=cex.lab, cex.main=cex.main, cex.text=cex.text, name=name,
                                 abbrv=abbrv, adj.alpha=adj.alpha),
         "roll"   = plot_fci_fnc(x=x, y=y, xlim=xlim, ylim=ylim, lwd=lwd, LCol=gcol,
                                 cibands=gbar, ocibands=obar, Tot.Line=overall, Tot.Color=ocol, Tgt.Color=tcol,
                                 Tgt.Line=tgt, Time.Pt.Line=tpline, Tpt.Color=tpcol, cex=cex, cex.axis=cex.axis,
                                 cex.lab=cex.lab, cex.main=cex.main, cex.text=cex.text, name=name,
                                 abbrv=abbrv, adj.alpha=adj.alpha) )
}
