#' Statistics for Shewhart control charts
#'
#' Calculate statistics that can be used to produce X-bar charts, p-charts, and u-charts. This includes
#' producing means for center lines, 3-sigma upper and lower control limits. Users can also calculate
#' values before and after an intervention to see if a change in the control process happened. Values are
#' returned in a data frame.
#'
#' @param x character outcome variable in X bar charts or numerator variable name for u-charts or p-charts (when p-chart data is aggregated).
#' @param y character variable name for u-charts or p-charts (when p-chart data is aggregated). When y is present, it becomes the
#' denominator for a rate calculated as x/y. Default is NULL.
#' @param time time variable name.
#' @param data name of data frame object.
#' @param type indicate what type of control chart is needed. Options for the X-bar, p-, and u-charts should be listed as
#' 'x', 'p', and 'u'. Default is the 'x' chart.
#' @param n.equal whether there are or we assume equal subgroup (sample) sizes. If n.equal=TRUE, control limits are calculated using the overall
#' mean n value. If n.equal=FALSE, control limits are based on each subgroup's sample size. Default is TRUE.
#' @param intervention a single numeric value for the time when an intervention begins (e.g., intervention=25;
#' intervention begins on the 25th day). Separate means and control limits are calculated pre- and post-intervention.
#' Default is NULL.
#' @param subset an expression defining a subset of the observations to use in the grouping. The default
#' is NULL, thereby using all observations. Specify, for example, data$hospital == "NY" or c(1:100,200:300) respectively to
#' use just those observations.
#'
#' @return data frame of control chart statistics for X-bar charts, p-charts, and u-charts. Includes means,
#' standard deviations, and 3-sigma upper and lower control limit values.
#' @importFrom stats sd
#' @export
#' @references
#' Ryan, T. (2011). Statistical Methods for Quality Improvement, Third Edition.
#' New Jersey: John Wiley & Sons, Inc. ISBN: 978-0-470-59074-4.

#' @examples
#' ## Hospital LOS and readmissions ##
#' # X-bar chart statistics
#' spc_x <- control(x="los", time="month", data=hosprog, type="x", n.equal=TRUE)
#' print(spc_x) # get data frame output
#'
#' # X-bar chart statistics not assuming equal sample sizes, subsetting for females
#' spc_x <- control(x="los", time="month", data=hosprog, type="x", n.equal=FALSE,
#'   subset=hosprog$female==1)
#' print(spc_x) # get data frame output
#'
#' # p-chart statistics, using only the numerator (i.e., y=NULL). Specify unequal sample sizes
#' spc_p <- control(x="rdm30", time="month", data=hosprog, type="p", n.equal=FALSE)
#' print(spc_p) # get data frame output
#'
#' # u-chart for infection rates with an intervention at the 22nd month
#' spc_u <- control(x="HAI", y="PatientDays", time="Month", data=infections,
#' type="u", n.equal=FALSE, intervention=22)
#' print(spc_u) # get data frame output


control <- function(x, y=NULL, time, data, type="x", subset=NULL,
                    n.equal=TRUE, intervention=NULL) {

  if( !type %in% c("x","u","p")) {
    stop("Error: Expecting only 'x', 'p', or 'u' for X-charts, p-charts, or u-charts.")
  }
  if(type == "x" ) {
    if(!is.null(y)) {
    stop("Error: Expecting that y argument is null for X-charts.")
      }
    }
  if(type %in% c("u") ) {
    if(is.null(y)) {
      stop("Error: Expecting that y argument has a character vector variable name listed for u-charts.")
    }
  }
  if(min(data[, x], na.rm=TRUE) < 0) {
    stop("Error: Expecting x values that are not negative, i.e., x >= 0.")
  }
  #Too many interventions
  if(!is.null(intervention)) {
    if(length(intervention) != 1) {
      stop("Error: Expecting only 1 intervention value.")
    }
  }
  #Can't use "intervention" or "Mean", "SD", "post" as  variable name
    if(any(c(x,time) %in% c("intervention","Mean", "SD", "post"))) {
      stop("Error: Can't use data with these column names: intervention, Mean, SD, post .")
    }
  # Identify all rows to use for subsets
  all_rows <- nrow(data)
  if(!is.null(subset)) {
    subset <- subset
  } else {
    subset <- 1:all_rows
  }
  # Create subset data if needed
  if(!is.null(subset)) {
    data <- eval(substitute(data[subset , ], list(subset =subset))  )
  }
  ## Get the specific type of analysis to do ##
  # whether they use a rate or not
  if(!is.null(y)) {
    rate_type <- "d"
  } else {
    rate_type <- ""
  }
  #whether there is an intervention or not
  if(!is.null(intervention)) {
    int_type <- "i"
  } else {
    int_type <- ""
  }
#Create the control analysis type
  contype <- paste0(type, rate_type, int_type)

  #####################################
  ## Function to get spc C4 constant ##
  #####################################
  spc4 <- function(n) {
    if(any(n <= 1) ) {
      stop("Error: Expecting n >= 2.")
    }
    # Uses gamma function
    numerator <- gamma(n / 2)
    denominator <- gamma((n - 1) / 2)
    c4 <- sqrt(2 / (n - 1)) * (numerator / denominator)
    return(c4)
  }
  #################################
  ## Continuous Outcome function ##
  #################################
  #Doesn't use y argument
  contSPC <- function(x, time, data, type, n.equal) {
    #By each X level
    if(type == "x") {
      agr_m <- aggregate(data[, x], list(data[, time]), FUN="mean", na.rm=T)
      agr_sd <- aggregate(data[, x], list(data[, time]), FUN="sd", na.rm=T)
      agr_n <- aggregate(data[ complete.cases(data[, x]) , x], list(data[ complete.cases(data[, x]) , time]), FUN="length")
      agr_df <- data.frame(time=agr_m[, 1], agr_m=agr_m[, 2], agr_sd=agr_sd[, 2], agr_n=agr_n[, 2])
      agr_df$c4 <- spc4(agr_df$agr_n)
    }
    if(type == "p") {
      agr_m <- aggregate(data[, x], list(data[, time]), FUN="mean", na.rm=T)
      agr_sum <- aggregate(data[, x], list(data[, time]), FUN="sum", na.rm=T)
      agr_n <- aggregate(data[ complete.cases(data[, x]) , x], list(data[ complete.cases(data[, x]) , time]), FUN="length")
      agr_df <- data.frame(time=agr_m[, 1], agr_m=agr_m[, 2], agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
    }
    #Calculates control limits--By Level
    # X-bar chart
    if(type == "x") {
      agr_df$SD <- mean(agr_df$agr_sd)
      if(n.equal == TRUE) {
        agr_df$N <- mean(agr_df$agr_n)
      }
      if(n.equal == FALSE) {
        agr_df$N <- agr_df$agr_n
      }
      agr_df$MOC <- 3 * ((agr_df$SD/agr_df$c4) / sqrt(agr_df$N))
      agr_df$Mean <- mean(agr_df$agr_m)
      agr_df$LCL <- agr_df$Mean - agr_df$MOC
      agr_df$LCL <- ifelse(agr_df$LCL <= 0, 0, agr_df$LCL)
      agr_df$UCL <- agr_df$Mean + agr_df$MOC
      #Add in variable on type of control analysis
      agr_df$type <- type
    }
    # p-bar chart
    if(type == "p") {
      if(n.equal == TRUE) {
        agr_df$N <- mean(agr_df$agr_n)
      }
      if(n.equal == FALSE) {
        agr_df$N <- agr_df$agr_n
      }
      agr_df$Mean <- sum(agr_df$agr_sum)/sum(agr_df$N)
      agr_df$SD <- sqrt((agr_df$Mean*(1-agr_df$Mean))/agr_df$N )
      agr_df$MOC <- 3 * agr_df$SD
      agr_df$LCL <- agr_df$Mean - agr_df$MOC
      agr_df$LCL <- ifelse(agr_df$LCL <= 0, 0, agr_df$LCL)
      agr_df$UCL <- agr_df$Mean + agr_df$MOC
      #sort columns
      agr_df <- agr_df[, c("time", "agr_m", "agr_sum", "agr_n","SD","N","MOC","Mean","LCL","UCL")]
      #Add in variable on type of control analysis
      agr_df$type <- type
    }
    return(agr_df)
  }
  ###############################
  ## contSPC for interventions ##
  ###############################
  contSPCd <- function(x, time, data, type, n.equal, intervention) {
    #Baseline data
    baseline <- contSPC(x=x, time=time, data=data[data[, time] < intervention, ],
                        type=type, n.equal=n.equal)
    baseline$intervention <- intervention
    baseline$post <- 0

    #Post-intervention data
    Post <- contSPC(x=x, time=time, data=data[data[, time] >= intervention, ],
                        type=type, n.equal=n.equal)
    Post$intervention <- intervention
    Post$post <- 1
    #Merge data back together
    agr_df <- rbind(baseline, Post)
    return(agr_df)
  }

  ############################
  ## Rate outcomes function ##
  ############################
  #Uses y argument
  rateSPC <- function(x, y, time, data, type, n.equal) {
    #By each X level
    if(type == "u") {
      agr_sum <- data[, x]
      agr_n <- data[, y]
      agr_m <- agr_sum/agr_n
      agr_df <- data.frame(time=data[, time], agr_m=agr_m, agr_sum=agr_sum, agr_n=agr_n)
    }
    if(type == "p") {
      agr_sum <- data[, x]
      agr_n <- data[, y]
      agr_m <- agr_sum/agr_n
      agr_df <- data.frame(time=data[, time], agr_m=agr_m, agr_sum=agr_sum, agr_n=agr_n)
    }
    #Calculates control limits--By Level
    # u-bar chart
    if(type == "u") {
      if(n.equal == TRUE) {
        agr_df$N <- mean(agr_df$agr_n)
      }
      if(n.equal == FALSE) {
        agr_df$N <- agr_df$agr_n
      }
      agr_df$Mean <- sum(agr_df$agr_sum)/sum(agr_df$N)
      agr_df$SD <- sqrt(agr_df$Mean/agr_df$N)
      agr_df$MOC <- 3 * agr_df$SD
      agr_df$LCL <- agr_df$Mean - agr_df$MOC
      agr_df$LCL <- ifelse(agr_df$LCL <= 0, 0, agr_df$LCL)
      agr_df$UCL <- agr_df$Mean + agr_df$MOC
      #sort columns
      agr_df <- agr_df[, c("time", "agr_m", "agr_sum", "agr_n","SD","N","MOC","Mean","LCL","UCL")]
      #Add in variable on type of control analysis
      agr_df$type <- type
    }
    # p-bar chart
    if(type == "p") {
      if(n.equal == TRUE) {
        agr_df$N <- mean(agr_df$agr_n)
      }
      if(n.equal == FALSE) {
        agr_df$N <- agr_df$agr_n
      }
      agr_df$Mean <- sum(agr_df$agr_sum)/sum(agr_df$N)
      agr_df$SD <- sqrt((agr_df$Mean*(1-agr_df$Mean))/agr_df$N )
      agr_df$MOC <- 3 * agr_df$SD
      agr_df$LCL <- agr_df$Mean - agr_df$MOC
      agr_df$LCL <- ifelse(agr_df$LCL <= 0, 0, agr_df$LCL)
      agr_df$UCL <- agr_df$Mean + agr_df$MOC
      #sort columns
      agr_df <- agr_df[, c("time", "agr_m", "agr_sum", "agr_n","SD","N","MOC","Mean","LCL","UCL")]
      #Add in variable on type of control analysis
      agr_df$type <- type
    }
    return(agr_df)
  }
  ###############################
  ## rateSPC for interventions ##
  ###############################
  rateSPCd <- function(x, y, time, data, type, n.equal, intervention) {
    #Baseline data
    baseline <- rateSPC(x=x, y=y, time=time, data=data[data[, time] < intervention, ],
                        type=type, n.equal=n.equal)
    baseline$intervention <- intervention
    baseline$post <- 0

    #Post-intervention data
    Post <- rateSPC(x=x, y=y, time=time, data=data[data[, time] >= intervention, ],
                    type=type, n.equal=n.equal)
    Post$intervention <- intervention
    Post$post <- 1
    #Merge data back together
    agr_df <- rbind(baseline, Post)
    return(agr_df)
  }
  #######################
  ## Run SPC functions ##
  #######################
  spc <- switch(contype,
           "x" = contSPC(x=x, time=time, data=data, type=type, n.equal=n.equal),
           "xi" =  contSPCd(x=x, time=time, data=data, type=type,
                            n.equal=n.equal, intervention=intervention),
           "p" = contSPC(x=x, time=time, data=data, type=type, n.equal=n.equal),
           "pi" =  contSPCd(x=x, time=time, data=data, type=type,
                            n.equal=n.equal, intervention=intervention),
           "pd" = rateSPC(x=x, y=y, time=time, data=data, type=type, n.equal=n.equal),
           "pdi" =  rateSPCd(x=x, y=y, time=time, data=data, type=type,
                             n.equal=n.equal, intervention=intervention),
           "ud" = rateSPC(x=x, y=y, time=time, data=data, type=type, n.equal=n.equal),
           "udi" =  rateSPCd(x=x, y=y, time=time, data=data, type=type,
                             n.equal=n.equal, intervention=intervention)
    )
  #change column names time and agr_m to actual names
  colnames(spc)[which(colnames(spc) %in% c("time","agr_m"))] <- c(time, x )
  # Assign ham classes
  class(spc) <- c("control","ham", "data.frame")
  #return spc
  return(spc=spc)
}



