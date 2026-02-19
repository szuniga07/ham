#' Shewhart control charts
#'
#' Graph X-bar charts, p-charts, and u-charts. This includes
#' producing means center lines, 3-sigma upper and lower control limits. Users can also calculate
#' values before and after an intervention to see if a change in the control process happened. Values are
#' returned in a data frame.
#'
#' @param x control object.
#' @param y not currently used, default is NULL.
#' @param xlim specify plot's x-axis limits with a 2 element numeric vector.
#' @param ylim specify plot's y-axis limits with a 2 element numeric vector.
#' @param xlab a character vector label for the x-axis.
#' @param ylab a character vector label for the y-axis.
#' @param main the main title of the plot.
#' @param lwd select the line width.
#' @param col a 2 element character vector to specify the 1) center line and 2) both control limit colors. Defaults to, if nothing selected, c("blue", "red").
#' @param iname intervention name text as a single character vector. Defaults to "Intervention" if nothing is selected.
#' @param icol specify intervention line color as a single character vector. Defaults to "black" if nothing is selected.
#' @param trend a logical vector that adds an ordinary least squares trend line (i.e., simple linear regression line) when trend=TRUE. Default is FALSE.
#' @param trcol select a color for the trend line. Default is 'gray'.
#' @param tgt specify 1 or more values on the y-axis of where to add one or more horizontal target lines. Default is NULL.
#' @param tgtcol select one or multiple colors for one or multiple target lines. Default is 'gray'.
#' @param tpline add one or more time point vertical lines using x-axis values. Default is NULL (i.e., no lines).
#' @param tpcol specify a color for the time point line, tpline. Default is NULL.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default of 1.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex.
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex.
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex.
#' @param cex.text The magnification to be used for the iname text added into the plot relative to the current setting of 1.
#' @param x.axis a vector of unique character or numeric values that makes up x-axis values to
#' replace the time variable values. This will be most helpful if you prefer current calendar
#' months/years instead of values starting at 1 (e.g., x.axis= sort(unique(data$Year)) for 1900-1999,
#' not 1-100). Must have equal lengths for unique x.axis values and replaced values (i.e., nrow(x)). Default is NULL.
#' @param y.axis a vector of unique character or numeric values that makes up y-axis values to replace
#' the outcome variable values. This will be most helpful if your outcome needs to be converted such as rate per
#' 1,000 patient days (e.g., y.axis= seq(min(x$HAI)*1000, max(x$HAI)*1000, length.out=nrow(x))). Must have equal
#' lengths for unique y.axis values and replaced values (i.e., nrow(x)). Default is NULL.
#' @param round.c an integer indicating the number of decimal places when rounding numbers such as for y.axis.
#' Default is 2.
#' @param ... additional arguments.
#'
#' @return plot of Shewhart control charts: X-bar charts, p-charts, and u-charts with 3-sigma control limits.
#' @importFrom graphics lines plot abline points text arrows
#' @importFrom utils head tail
#' @importFrom methods is
#' @importFrom stats lm
#' @export
#'
#' @examples
#' ## Hospital LOS and readmissions ##
#' # X-bar chart
#' spc_x <- control(x="los", time="month", data=hosprog, type="x", n.equal=TRUE)
#' # Basic X-bar chart
#' plot(spc_x)
#'
#' # p-chart, using only the numerator (i.e., y=NULL). Specify unequal sample sizes
#' spc_p <- control(x="rdm30", time="month", data=hosprog, type="p", n.equal=FALSE)
#' # p-chart, adding target and time point lines
#' plot(spc_p, tgt=c(0,.25), tgtcol="green", ylim=c(0,0.4), tpline=c(4,8),
#' tpcol= c("yellow","black"))
#'
#' # u-chart for infection rates with an intervention
#' spc_u <- control(x="HAI", y="PatientDays", time="Month", data=infections,
#' type="u", n.equal=FALSE, intervention=22)
#' # u-chart with trend lines, various graphing options, x.axis start at 2nd year
#' # and y.axis changed to show HAIs per 1,000 patient days
#' plot(spc_u, main="u-Chart: HAI per 1,000 Patient Days Pre/Post Intervention",
#' col=c("green","dodgerblue"), trend=TRUE, trcol="red", x.axis=c((1:41+12)), round.c=1,
#' y.axis=seq(min(spc_u$HAI)*1000, max(spc_u$HAI)*1000, length.out=nrow(spc_u)),
#' xlab="Months (starting at year 2)", icol="gray", lwd=2, cex=2,
#' cex.axis=1.1, cex.main=1.25, cex.text=1.25)

plot.control <- function(x, y=NULL, xlim=NULL, ylim=NULL, xlab=NULL, ylab=NULL, main=NULL, lwd=NULL, col=NULL, iname=NULL,
                         icol="black", trend=FALSE, trcol="gray", tgt=NULL, tgtcol="gray", tpline=NULL, tpcol=NULL, cex=1,
                         cex.lab=NULL, cex.axis=NULL, cex.main=NULL, cex.text=NULL, x.axis=NULL, y.axis=NULL, round.c=NULL, ...) {
  if (any(class(x) == "control") == FALSE) {stop("Error: Expecting control class object." )}
  #x.axis having equal lengths with current time variable
  if(!is.null(x.axis)) {
    if (nrow(x) != length(unique(x.axis))) {stop("Error: Expecting equal lengths for number of rows for x and length of x.axis." )}
  }
  #x.axis having equal lengths with current time variable
  if(!is.null(y.axis)) {
    if (nrow(x) != length(unique(y.axis))) {stop("Error: Expecting equal lengths for number of rows for x and length of y.axis." )}
  }
  #chart type
  chart_type <- x[1, "type"]

  # outcome variable for printing
  yvar <- colnames(x)[2]
  xvar <- colnames(x)[1]
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

  #Get main title
  if(is.null(main)) {
    if(chart_type == "x") {
      main_title <- "X-bar Chart"
    }
    if(chart_type == "p") {
      main_title <- "p-Chart"
    }
    if(chart_type == "u") {
      main_title <- "u-Chart"
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
  # Cex settings cex, cex.axis, cex.main
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
  #Make CEX for text added into the plot (i.e, text())
  if(!is.null(cex)) {
    textCEX <- cex.text
  } else {
    textCEX <- 1
  }
  #Indicates x axis "at" values
  if(!is.null(x.axis)) {
    x.at <- sort(unique(x[, 1]))
  } else {
    x.at <- NULL
  }
  #Indicates y axis "at" values
  if(!is.null(x.axis)) {
    y.at <- seq(min(x[, 2], na.rm=T), max(x[, 2], na.rm=T), length.out=nrow(x))
  } else {
    y.at <- NULL
  }
  #Get intervention time if there is one
  has_intervention <- ifelse("intervention" %in% colnames(x), TRUE, FALSE )
  if(has_intervention == TRUE) {
    intervention_time <- x[1, "intervention"][1]
  } else {
    intervention_time <- NULL
  }
  intervention_y_vals <- c(x$UCL[which(x[, 1] == intervention_time)], x$UCL[which(x[, 1] == intervention_time-1)])
  #Intervention text name
  if(!is.null(iname)) {
    iname_txt <- iname
  } else {
    iname_txt <- "Intervention"
  }
  #######################
  ## Trend line arrows ##
  #######################
  #Get model formula
  mod_fmla <- as.formula(paste(paste0(colnames(x)[2], "~"),
                     paste(colnames(x)[1])))
  #Double models if there is an intervention
  spcmodel <- vector(mode="list", length=2)
  x_start <- vector(mode="list", length=2)
  x_end <- vector(mode="list", length=2)
  y_start <- vector(mode="list", length=2)
  y_end <- vector(mode="list", length=2)
  if(has_intervention == TRUE) {
    for(i in 1:2) {
      spcmodel[[i]] <- lm(mod_fmla, data = x[x$post == c(0,1)[i], ])
      x_start[[i]] <- min(x[x$post == c(0,1)[i], 1])
      names(x_start)[[i]] <- colnames(x)[1]
      x_end[[i]] <- max(x[x$post == c(0,1)[i], 1])
      names(x_end)[[i]] <- colnames(x)[1]
      y_start[[i]] <- predict(spcmodel[[i]], newdata = x_start[i])
      y_end[[i]] <- predict(spcmodel[[i]], newdata = x_end[i])
    }
  } else {
    spcmodel[[1]] <- lm(mod_fmla, data = x)
    x_start[[1]] <- min(x[, 1])
    names(x_start)[[1]] <- colnames(x)[1]
    x_end[[1]] <- max(x[, 1])
    names(x_end)[[1]] <- colnames(x)[1]
    y_start[[1]] <- predict(spcmodel[[1]], newdata = x_start[1])
    y_end[[1]] <- predict(spcmodel[[1]], newdata = x_end[1])
  }

  #############################
  ## Shewhart control charts ##
  #############################
  plot(rep(x[, 1], 3), c(x[, 2],x[,"LCL"],x[,"UCL"]), type="n", main=main_title,
       xlab= xlab, ylab=ylab, xlim=xlim, ylim=ylim, cex=cex, cex.axis=cex.axis,
       cex.lab=cex.lab, cex.main=cex.main, axes=FALSE)
  #Control limit lines
  lines(x[, 1], x$UCL, lty=2, col=lcol[2], lwd=lwd, type="s")
  lines(x[, 1], x$LCL, lty=2, col=lcol[2], lwd=lwd, type="s")
  #center line
  lines(x[, 1], x$Mean, col=lcol[1], lwd=lwd)
  #Just the lines of the "lines and points" portion
  lines(x[, 1], x[, 2], type="c", col= "black", lwd=lwd)
  # overlay with colored points
  points(x[, 1], x[, 2], cex=cex,
         pch= ifelse(x[, 2] < x$LCL | x[, 2] > x$UCL, 15, 19),
         col= ifelse(x[, 2] < x$LCL | x[, 2] > x$UCL, lcol[2], "black")  )
  #Add intervention line
  if(has_intervention == TRUE) {
    abline(v=intervention_time, lty=1, lwd=lwd, col=icol)
    text(intervention_time, intervention_y_vals[which.max(intervention_y_vals)],
         labels=iname_txt, pos=3, cex=textCEX)
  }
  #Add target line
  if(!is.null(tgt)) {
    for (i in 1:length(tgt)) {
      abline(h= as.numeric(eval(parse(text=tgt[i] )) ),
             col=rep(tgtcol, length(tgt))[i], lty=3, lwd=lwd)
    }
  }
  #Add time point line
  if(!is.null(tpline)) {
    for (i in 1:length(tpline)) {
      abline(v= as.numeric(eval(parse(text=tpline[i] )) ),
             col=rep(tpcol, length(tpline))[i], lty=1, lwd=lwd)
    }
  }
  # Add the line with an arrow
  if(trend == TRUE) {
    if(has_intervention == TRUE) {
    for(i in 1:2) {
      arrows(x0 = x_start[[i]], y0 = y_start[[i]],
             x1 = x_end[[i]], y1 = y_end[[i]],
             col = trcol,       # Color of the line/arrow
             lwd = lwd,         # Line width
             length = 0.25,   # Length of the arrowhead sides (in inches)
             angle = 30)      # Angle of the arrowhead sides
    }
  } else {
    arrows(x0 = x_start[[1]], y0 = y_start[[1]],
           x1 = x_end[[1]], y1 = y_end[[1]],
           col = trcol,       # Color of the line/arrow
           lwd = lwd,         # Line width
           length = 0.25,   # Length of the arrowhead sides (in inches)
           angle = 30)      # Angle of the arrowhead sides
  }
  }

  #Add x-axis value to graph
  if(!is.null(x.axis)) {
    axis(side=1, at=x.at, labels=x.axis, cex.axis=cex.axis)
  } else {
    axis(1)
  }
  ## Add y-axis value to graph ##
  #Determine if y.axis is numeric or not
  yaxis_num <- ifelse(class(y.axis) == "numeric", TRUE, FALSE)
  if(!is.null(y.axis)) {
    if(yaxis_num == TRUE) {
      axis(side=2, at=y.at, labels= round(y.axis, round.c), cex.axis=cex.axis)
    } else {
      axis(side=2, at=y.at, labels=y.axis, cex.axis=cex.axis)
    }
  } else {
    axis(2)
  }
  if(chart_type == "x") {
    axis(4, cex.axis=cex.axis, at= c(tail(x$LCL, 1), tail(x$Mean, 1), tail(x$UCL, 1)),
         labels= c("LCL", expression(bar(X)), "UCL"), las=1, tick = F, line=-.6)
    }
    if(chart_type == "p") {
      axis(4, cex.axis=cex.axis, at= c(tail(x$LCL, 1), tail(x$Mean, 1), tail(x$UCL, 1)),
           labels= c("LCL", expression(bar(p)), "UCL"), las=1, tick = F, line=-.6)
    }
    if(chart_type == "u") {
      axis(4, cex.axis=cex.axis, at= c(tail(x$LCL, 1), tail(x$Mean, 1), tail(x$UCL, 1)),
           labels= c("LCL", expression(bar(u)), "UCL"), las=1, tick = F, line=-.6)
    }
    box()

} #end of function


