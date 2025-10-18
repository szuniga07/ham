#' Confidence interval graphs for group class objects
#'
#' @param x group object.
#' @param y type of confidence interval object, specify either 'group', 'time', or 'roll'.
#' @param order specify confidence interval object order as 'alpha' or 'numeric' for alphabetical or numerical ordering.
#' @param gcol pick confidence interval line colors for groups. Default is 'blue'.
#' @param gbar logical TRUE or FALSE that indicates whether group lines have confidence bars for trend over time results. Default is FALSE.
#' @param pcol select point color for 'group' only confidence intervals. Default is 'red'.
#' @param overall logical TRUE or FALSE that indicates whether to include the overall sample confidence intervals (i.e., no grouping). Default is FALSE.
#' @param ocol indicate the optional overall line color. Default is 'gray' when overall=TRUE.
#' @param obar logical TRUE or FALSE that indicates whether to add overall confidence band. Default is FALSE.
#' @param tgt specify a value on the x-axis of where to add a target line. Default is NULL.
#' @param tcol select a color for the target line. Default is 'gray'.
#' @param tpline add time line (time)
#' @param tpcol specify a value on the y-axis of where to add a time point line. Select when y="time" or y="roll". Default is NULL.
#' @param xlim specify plot's x-axis limits with a 2 value vector.
#' @param ylim specify plot's y-axis limits with a 2 value vector.
#' @param main the main title of the plot.
#' @param lwd select the line width.
#' @param adj.alpha factor modifying the opacity alpha of the confidence interval bars, in the range of 0 to 1. Default is 0.4.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default of 1.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex.
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex.
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex.
#' @param round.c an integer indicating the number of decimal places
#' to be used for rounding coefficient values.
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
#' plot(x=gr1, y="group")
#' #Improve the graph display using various options
#' plot(x=gr1, y="group", order="numeric", gcol="blue", gbar=TRUE, pcol="red",
#' overall=TRUE, ocol="gray", obar=TRUE, tgt=4.5, tcol="green", tpline=NULL,
#' tpcol="gray", xlim=c(4,5), ylim=c(.9,2.1), main=NULL, lwd=3, adj.alpha=0.2,
#' cex=2, cex.axis=1.25, cex.lab=1.25, cex.main=1.5, round.c=1, abbrv=5)

plot.group <- function(x, y="group", order="alpha", gcol="blue", gbar=FALSE, pcol="red", overall=FALSE,
                       ocol="gray", obar=FALSE, tgt=NULL, tcol="gray", tpline=NULL,
                       tpcol="gray", xlim=NULL, ylim=NULL, main=NULL, lwd=1, adj.alpha=0.4,
                       cex=1, cex.axis=1, cex.lab=1, cex.main=1, round.c=2, abbrv=5, ...) {
  if(any(is.null(c(x, y)) == TRUE)) {
    stop("Error: Expecting both an x and y argument.")
  }
  if (any(class(x) == "group") == FALSE) {stop("Error: Expecting group class object." )}
  if (!y %in% c("group", "time", "roll")) {stop("Error: Expecting y='group', y='time', or y='roll'." )}

  ##########################################
  # Plot function for confidence intervals #
  ##########################################
  plot_ci_fnc <- function(x, alpha_num, main, Lcol, Pcol, tgt, Cbar,
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
    rng <- seq(min(adf), max(adf),length.out=nrow(adf))
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
      abline(v=tgt, lwd=lwd, col= tcol, lty=1)
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

  #Run the function above
  if(y== "group") {
    plot_ci_fnc(x=x, alpha_num=order, main=main, Lcol=gcol, Pcol=pcol, tgt=tgt, Cbar=obar,
                roundVal=round.c, adj.alpha=adj.alpha, xlim=xlim, ylim=ylim, abbrv=abbrv, ocol=ocol,
                tcol=tcol, cex=cex, cex.axis=cex.axis, cex.lab=cex.lab, cex.main=cex.main)
  }

}
