
#' Group level confidence intervals and between-group variation
#'
#' @param x group predictor variable name.
#' @param y outcome variable name.
#' @param z time period variable name.
#' @param data name of data frame object.
#' @param dist indicate the distribution used for confidence intervals. Options
#' for the t, binomial, and exact Poisson distributions. Options are 't', 'b', and 'p'.
#' Default is the 't'.
#' @param conf.int select the confidence interval level. Default is 0.95.
#' @param increment specify the increment in time periods. Selecting 3 if data uses
#' the month as the unit of time will give confidence intervals, each based on 3 months.
#' Default is 1.
#' @param rolling indicate the number of time periods for the 'rolling average'.
#' The rolling average consists of >1 time periods but subsequent point estimate
#' increase by a unit of 1. For example, the common 12-month rolling average is
#' based on months 1-12 of data, followed by the next estimate using months 2-13,
#' 3-14, and so on until the last month in the data has been reached. Default is NULL.
#' @param quarts logical TRUE or FALSE that indicates whether to convert continuous x
#' into 4 groups based on quartiles of x. Default is FALSE.
#' @param cluster logical TRUE or FALSE to generate measures of between-group variation
#' such as the Intra-Class Correlation, Median Odds Ratio, or Design Effect. Default is FALSE.
#' Uses binary outcome formula (between-group variance/(between-group variance + (3.14^2/3)) for ICC
#' in Rabe-Hesketh which may be more appropriate for multilevel models. ICC, MOR, DE may be less
#' reliable for binomial and Poisson distributions, use caution.
#' @param subset an expression defining a subset of the observations to use in the grouping. The default
#' is NULL, thereby using all observations. Specify, for example, data$hospital == "NY" or c(1:100,200:300) respectively to
#' use just those observations.
#' @param asis a logical vector that indicates if data will be processed as having only 1 unique observation per 'x' and 'z' combination
#' (i.e., this is intended for use with aggregated data). Default is FALSE. This will allow the plot function to graph single observation data for groups
#' over time. Only the t distribution is used for the overall trend line and confidence band (works in conjunction with 'ocol' and 'oband').
#' @param dataf not currently used, please use 'data' instead.
#'
#' @return list of confidence intervals for outcomes by groups, over time,
#' and clustering measures. Some values returned in alphabetical and numerical order based on the group.
#' @importFrom stats aov complete.cases na.omit poisson.test qnorm quantile sd binom.test
#' @export
#' @references
#' Merlo, J. (2006). A brief conceptual tutorial of multilevel analysis in
#' social epidemiology: using measures of clustering in multilevel logistic
#' regression to investigate contextual phenomena. Journal of Epidemiological Health, 60, 4,
#' 290-297. https://doi.org/10.1136/jech.2004.029454.
#'
#' Muthen, B. & Satorra, A. (1995). Complex Sample Data in Structural Equation Modeling.
#' Sociological Methodology, 25, 267-316. https://doi.org/10.2307/271070.
#'
#' Rabe-Hesketh, S. & Skrondal, A. (2008). Multilevel and Longitudinal Modeling Using Stata,
#' Second Edition. ISBN: 978-1-59718-040-5.
#'
#' @examples
#' #default t distribution results
#' group(x="program", y="los", data=hosprog)
#' #Rounding LOS to integers
#' hp2 <- hosprog; hp2$los2 <- round(hp2$los, 0)
#' #Exact Poisson confidence intervals
#' group(x="program", y="los2", data=hp2, dist="p")
#' #Rolling 6-months of data
#' group(x="program", y="los", z="month", data=hosprog, dist="t", rolling=6)
#' #Data returned separately for rolling 6-months of data and 3-month increments (e.g., quarters)
#' group(x="program", y="los", z="month", data=hosprog, dist="t", increment=3, rolling=6)
#' #Quartile groups for continuous risk score and returned clustering info
#' group(x="risk", y="los", data=hosprog, quarts=TRUE, cluster=TRUE)
#' #Binomial distribution with less conservative 90% confidence intervals
#' group(x="risk", y="rdm30", data=hosprog, quarts=TRUE, dist="b", conf.int=0.90)

group <- function(x, y, z=NULL, data, dist="t", conf.int=0.95, increment=1,
                  rolling=NULL, quarts=FALSE, cluster=FALSE, subset=NULL, asis=FALSE, dataf=NULL ) {

  if(conf.int <= 0 || conf.int >= 1 ) {
    stop("Error: Expecting confidence interval level within 0 to 1.")
  }
  if(any(c(increment, rolling) < 1)) {
    stop("Error: Expecting increment or rolling whole numbers >= 1.")
  }
  if( !dist %in% c("b","t","p")) {
    stop("Error: Expecting only 'b', 't', or 'p' for binomial, t, or Poisson distributions.")
  }
  if( dist %in% c("b","p")) {
    if( asis == TRUE) {
    stop("Error: Expecting only the argument dist= 't' when asis= TRUE.")
    }
  }
  if(all(c(cluster, asis)  == TRUE)) {
    stop("Error: No clustering results produced when asis= TRUE and cluster= TRUE.")
  }
  if(!is.null(dataf)) {
    warning("Warning: Please use data argument instead of dataf argument.")
  }
  #Replace data with dataf
  if(is.null(data)) {
    if(!is.null(dataf)) {
      data <- dataf
    }
  }
  #Identify all rows to use for subsets
  all_rows <- nrow(data)
  if(!is.null(subset)) {
    subset <- subset
  } else {
    subset <- 1:all_rows
  }
  #Create subset data if needed
  if(!is.null(subset)) {
    data <-   eval(substitute(data[subset , ], list(subset =subset))  )
  }
  #Make "Increment" object equal to increment
  Increment <- increment

  #Create quartiles of a continuous variable and replace in output
  fncQuarts <- function(data, x) {
    x_labels <- c()
    for(i in 1:4) {
      x_labels[i] <- paste0("Q", i)
    }
    quints_ls <- quantile(data[, x], na.rm=TRUE)
    X <- cut(data[, x], breaks= quints_ls, labels=x_labels,
             right=TRUE, include.lowest=TRUE)
    return(list(X=X, Quartiles=quints_ls))
  }
  if(quarts==TRUE) {
    X.quartile <- fncQuarts(data=data, x=x)[[1]]
  }  else {
    X.quartile <- NULL
  }
  if(quarts==TRUE) {
    Quartiles <- fncQuarts(data=data, x=x)[[2]]
  }  else {
    Quartiles <- NULL
  }

  #Add quartile variable
  if(quarts==TRUE) {
    data <- data.frame(data[, -which(colnames(data) == x)], X.quartile)
    colnames(data)[which(colnames(data) =="X.quartile")] <- x
  }

  #Continuous outcomes
  tconf <- function(x, y, data, conf_lev) {
    #Aggregates outcome by factor
    all_m <- mean(data[, y], na.rm=T)
    all_sd <- sd(data[, y], na.rm=T)
    all_n <- length(na.omit(data[, y]))
    #By each X level
    agr_m <- aggregate(data[, y], list(data[, x]), FUN="mean", na.rm=T)
    agr_sd <- aggregate(data[, y], list(data[, x]), FUN="sd", na.rm=T)
    agr_n <- aggregate(data[ complete.cases(data[, y]) , y], list(data[ complete.cases(data[, y]) , x]), FUN="length")
    agr_df <- data.frame(x_lev=agr_m[, 1], agr_m=agr_m[, 2], agr_sd=agr_sd[, 2], agr_n=agr_n[, 2])
    #Calculates confidence intervals--Overall
    all_MOE <- qt((conf_lev/2)+.5, df=all_n - 1) * all_sd/sqrt(all_n)
    all_Lower <- all_m - all_MOE
    all_Upper <- all_m + all_MOE
    adf_all <- data.frame(cbind(PointEst=all_m, Lower=all_Lower, Upper=all_Upper))
    #Calculates confidence intervals--By Level
    if (asis == TRUE) {
      MOE <- NA
      Lower <- agr_df$agr_m
      Upper <- agr_df$agr_m
    }
    if (asis == FALSE) {
    MOE <- qt((conf_lev/2)+.5, df=agr_df$agr_n - 1) * agr_df$agr_sd/sqrt(agr_df$agr_n)
    Lower <- agr_df$agr_m - MOE
    Upper <- agr_df$agr_m + MOE
    }
    adf_alpha <- data.frame(Group=agr_df$x_lev, PointEst=agr_df$agr_m, Lower=Lower, Upper=Upper)
    rownames(adf_alpha) <- agr_df$x_lev
    #  alpha_o <- order(rownames(adf_alpha), decreasing = T)
    alpha_o <- order(agr_df$x_lev, decreasing = T)
    adf_alpha <- adf_alpha[alpha_o, ]
    adf_o <- order(adf_alpha[, "PointEst"], decreasing = T)
    adf_numeric <- adf_alpha[adf_o, ]
    return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric, adf_all= adf_all) )
  }
  ###################
  # Binary outcomes #
  ###################
  #Binomial confidence intervals
  binci <- function(x, n, alpha=0.05) {
    conf_level <- 1 - alpha
    #Make a data frame from the aggregated values
    tbindf <- data.frame(x, n)
    tbindf[, 2] <- tbindf[, 2] - tbindf[, 1]
    #Binomial tests
    tbintest <- apply(tbindf, 1, binom.test, conf.level = conf_level)
    ci_df <- as.data.frame(cbind(sapply(tbintest, "[[", "estimate"),
                                 matrix(sapply(tbintest, "[[", "conf.int"), ncol=2, byrow=T)))
    #Create colnames
    colnames(ci_df) <- c("PointEst","Lower","Upper")
    rownames(ci_df) <- NULL
    return(ci_df)
  }

  bconf <- function(x, y, data, conf_lev) {
    #Aggregates outcome by factor
    agr_sum <- aggregate(data[, y], list(data[, x]),  FUN="sum", na.rm=T)
    agr_n <- aggregate(data[, y], list(data[, x]), FUN="length")
    agr_df <- data.frame(x_lev=agr_sum[, 1], agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
    #Calculates confidence intervals
    adf_alpha <- binci(x=agr_df[,2], n=agr_df[,3], alpha=1 - conf_lev)
    adf_alpha <- data.frame(Group=agr_df$x_lev, adf_alpha)

    adf_all <- binci(x=sum(agr_df[,2], na.rm=TRUE), n=sum(agr_df[,3], na.rm=TRUE), alpha=1 - conf_lev)

    rownames(adf_alpha) <- agr_df$x_lev
    #  alpha_o <- order(rownames(adf_alpha), decreasing = T)
    alpha_o <- order(agr_df$x_lev, decreasing = T)
    adf_alpha <- adf_alpha[alpha_o, ]
    adf_o <- order(adf_alpha[, "PointEst"], decreasing = T)
    adf_numeric <- adf_alpha[adf_o, ]
    return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric, adf_all=adf_all) )
  }
  #################
  # Exact Poisson #
  #################
  pconf <- function(x, y, data, conf_lev) {
    #Aggregate outcomes for all
    all_sum <- sum(data[, y], na.rm=T)
    all_n <- length(na.omit(data[, y]))
    adf_all <- unlist(poisson.test(x=all_sum, T=all_n, conf.level= conf_lev)[c("estimate","conf.int")])
    adf_all <- data.frame(matrix(adf_all, ncol=3))
    colnames(adf_all) <- c("PointEst", "Lower", "Upper")
    #Aggregates outcome by factor
    agr_sum <- aggregate(data[, y], list(data[, x]), FUN="sum", na.rm=T)
    agr_n <- aggregate(data[, y], list(data[, x]), FUN="length")
    agr_df <- data.frame(x_lev=agr_sum[, 1], agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
    #Calculates confidence intervals
    adf_alpha <- matrix(ncol= 3, nrow= nrow(agr_df), byrow = TRUE)
    for (i in 1:nrow(agr_df)) {
      adf_alpha[i, ] <- unlist(poisson.test(x=agr_df[i,2], T=agr_df[i,3], conf.level= conf_lev)[c("estimate","conf.int")])
    }
    adf_alpha <- data.frame(Group=agr_df$x_lev, adf_alpha)
    colnames(adf_alpha) <- c("Group", "PointEst", "Lower", "Upper")
    rownames(adf_alpha) <- agr_df$x_lev
    #  alpha_o <- order(rownames(adf_alpha), decreasing = T)
    alpha_o <- order(agr_df$x_lev, decreasing = T)
    adf_alpha <- adf_alpha[alpha_o, ]
    adf_o <- order(adf_alpha[, "PointEst"], decreasing = T)
    adf_numeric <- adf_alpha[adf_o, ]
    return(list(adf_alpha=adf_alpha, adf_numeric=adf_numeric, adf_all=adf_all) )
  }

  conf <- function(x, y, data, conf_lev, dist) {
    switch(dist,
           "t" =  tconf(x, y, data, conf_lev),
           "b" =  bconf(x, y, data, conf_lev),
           "p" =  pconf(x, y, data, conf_lev)
    )
  }

  #######################################
  # Graphs for trajectories by time     #
  #######################################
  #Binomial
  fbconf <- function(x, xlev=NULL, y, z, data, conf_lev, Increment=1) {
    #Aggregates outcome by factor
    if( is.null(xlev)) {
      data <- data
    } else {
      data <- data[ data[, x] %in% xlev,  ]
    }
    #Calculates confidence intervals for single units or in increments
    if(Increment == 1) {
      agr_sum <- aggregate(data[, y], list(data[, x] , data[, z]), FUN="sum", na.rm=TRUE)
      agr_n <- aggregate(data[, y], list(data[, x] , data[, z]), FUN="length")
    } else {
      agr_sum <- aggregate(data[, y], list(data[, x] , ceiling(data[, z]/Increment) ), FUN="sum", na.rm=TRUE)
      agr_n <- aggregate(data[, y], list(data[, x] , ceiling(data[, z]/Increment) ), FUN="length")
    }
    agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=as.integer(c(agr_sum[, 2])), agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
    agr_df <- cbind(agr_df, binci(x=agr_df[,3], n=agr_df[,4], alpha=1 - conf_lev))
    rownames(agr_df) <- 1:nrow(agr_df)
    return(agr_df)
  }

  #Continuous outcomes
  ftconf <- function(x, xlev=NULL, y, z, data, conf_lev, Increment=1, asis) {
    Increment <- Increment
    #Aggregates outcome by factor
    if( is.null(xlev)) {
      data <- data
    } else {
      data <- data[ data[, x] %in% xlev,  ]
    }
    #Confidence interval data for increments
    if(Increment == 1) {
      agr_m <- aggregate(data[, y], list(data[, x] , data[, z]), FUN="mean", na.rm=TRUE)
      agr_sd <- aggregate(data[, y], list(data[, x] , data[, z]), FUN="sd", na.rm=TRUE)
      agr_n <- aggregate(data[, y], list(data[, x] , data[, z]), FUN="length")
      agr_df <- data.frame(x_lev=agr_m[, 1], z_lev=as.integer(c(agr_m[, 2])) , agr_m=agr_m[, 3], agr_sd=agr_sd[, 3], agr_n=agr_n[, 3])
    } else {
      agr_m <- aggregate(data[, y], list(data[, x] , ceiling(data[, z]/Increment) ), FUN="mean", na.rm=TRUE)
      agr_sd <- aggregate(data[, y], list(data[, x] , ceiling(data[, z]/Increment) ), FUN="sd", na.rm=TRUE)
      agr_n <- aggregate(data[, y], list(data[, x] , ceiling(data[, z]/Increment) ), FUN="length")
      agr_df <- data.frame(x_lev=agr_m[, 1], z_lev=as.integer(c(agr_m[, 2])), agr_m=agr_m[, 3], agr_sd=agr_sd[, 3], agr_n=agr_n[, 3])
    }
    #Calculates confidence intervals
    if (asis == TRUE) {
      MOE <- NA
      Lower <- agr_df$agr_m
      Upper <- agr_df$agr_m
    }
    if (asis == FALSE) {
      MOE <- qt((conf_lev/2)+.5, df=agr_df$agr_n - 1) * agr_df$agr_sd/sqrt(agr_df$agr_n)
      Lower <- agr_df$agr_m - MOE
      Upper <- agr_df$agr_m + MOE
    }
    adf_alpha <- data.frame(cbind(PointEst=agr_df$agr_m, Lower=Lower, Upper=Upper))
    agr_df <- cbind(agr_df, adf_alpha)
    return(agr_df )
  }

  #Exact Poisson
  fpconf <- function(x, xlev=NULL, y, z, data, conf_lev, Increment=1) {
    #Aggregates outcome by factor
    if( is.null(xlev)) {
      data <- data
    } else {
      data <- data[ data[, x] %in% xlev,  ]
    }
    #Confidence interval data for increments
    if(Increment == 1) {
      agr_sum <- aggregate(data[, y] ~ data[, x]+ data[, z], FUN="sum", na.rm=TRUE)
      agr_n <- aggregate(data[, y] ~ data[, x]+ data[, z], FUN="length")
      agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=as.integer(c(agr_sum[, 2])), agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
    } else {
      agr_sum <- aggregate(data[, y] ~ data[, x]+ ceiling(data[, z]/Increment), FUN="sum", na.rm=TRUE)
      agr_n <- aggregate(data[, y] ~ data[, x]+ ceiling(data[, z]/Increment), FUN="length")
      agr_df <- data.frame(x_lev=agr_sum[, 1], z_lev=as.integer(c(agr_sum[, 2])), agr_sum=agr_sum[, 3], agr_n=agr_n[, 3])
    }
    #Calculates confidence intervals
    adf_alpha <- matrix(ncol= 3, nrow= nrow(agr_df), byrow = TRUE)
    for (i in 1:nrow(agr_df)) {
      adf_alpha[i, ] <- unlist(poisson.test(x=agr_df[i,3], T=agr_df[i,4], conf.level= conf_lev)[c("estimate","conf.int")])
    }
    adf_alpha <- data.frame(adf_alpha)
    colnames(adf_alpha) <- c("PointEst", "Lower", "Upper")
    agr_df <- cbind(agr_df, adf_alpha)
    return(agr_df=agr_df )
  }

  fconf <- function(x, xlev=NULL, y, z, data, conf_lev,
                    Increment=1, dist) {
    switch(dist,
           "t" =  ftconf(x, xlev, y, z, data, conf_lev, Increment, asis),
           "b" =  fbconf(x, xlev, y, z, data, conf_lev, Increment),
           "p" =  fpconf(x, xlev, y, z, data, conf_lev, Increment)
    )
  }
  ################################################################################
  #      Function to get overall trend confidence intervals, no grouping         #
  ################################################################################
  #Binomial
  ftotBconf <- function(y, z, data, conf_lev, Increment) {
    #Calculates confidence intervals for single units or in increments
    if(Increment == 1) {
      agr_sum <- aggregate(data[, y], list(data[, z]), FUN="sum", na.rm=TRUE)
      agr_n <- aggregate(data[, y], list(data[, z]), FUN="length")
    } else {
      agr_sum <- aggregate(data[, y], list(ceiling(data[, z]/Increment) ), FUN="sum", na.rm=TRUE)
      agr_n <- aggregate(data[, y], list(ceiling(data[, z]/Increment) ), FUN="length")
    }
    agr_df <- data.frame(z_lev=as.integer(c(agr_sum[, 1])), agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
    agr_df <- cbind(agr_df, binci(x=agr_df[,2], n=agr_df[,3], alpha=1 - conf_lev))
    rownames(agr_df) <- 1:nrow(agr_df)
    return(agr_df)
  }

  #Continuous outcomes
  ftotTconf <- function(y, z, data, conf_lev, Increment, asis) {
    #Confidence interval data for increments
    if(Increment == 1) {
      agr_m <- aggregate(data[, y], list( data[, z]), FUN="mean", na.rm=TRUE)
      agr_sd <- aggregate(data[, y], list(data[, z]), FUN="sd", na.rm=TRUE)
      agr_n <- aggregate(data[, y], list(data[, z]), FUN="length")
      agr_df <- data.frame(z_lev=as.integer(c(agr_m[, 1])), agr_m=agr_m[, 2], agr_sd=agr_sd[, 2], agr_n=agr_n[, 2])
    } else {
      agr_m <- aggregate(data[, y], list(ceiling(data[, z]/Increment) ), FUN="mean", na.rm=TRUE)
      agr_sd <- aggregate(data[, y], list(ceiling(data[, z]/Increment) ), FUN="sd", na.rm=TRUE)
      agr_n <- aggregate(data[, y], list(ceiling(data[, z]/Increment) ), FUN="length")
      agr_df <- data.frame(z_lev= as.integer(c(agr_m[, 1])), agr_m=agr_m[, 2], agr_sd=agr_sd[, 2], agr_n=agr_n[, 2])
    }
    #Calculates confidence intervals
      MOE <- qt((conf_lev/2)+.5, df=agr_df$agr_n - 1) * agr_df$agr_sd/sqrt(agr_df$agr_n)
    Lower <- agr_df$agr_m - MOE
    Upper <- agr_df$agr_m + MOE
    adf_alpha <- data.frame(cbind(PointEst=agr_df$agr_m, Lower=Lower, Upper=Upper))
    agr_df <- cbind(agr_df, adf_alpha)
    return(agr_df )
  }

  #Exact Poisson
  ftotPconf <- function(y, z, data, conf_lev, Increment) {
    #Confidence interval data for increments
    if(Increment == 1) {
      agr_sum <- aggregate(data[, y] ~ data[, z], FUN="sum", na.rm=TRUE)
      agr_n <- aggregate(data[, y] ~ data[, z], FUN="length")
      agr_df <- data.frame(z_lev= as.integer(c(agr_sum[, 1])), agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
    } else {
      agr_sum <- aggregate(data[, y] ~ ceiling(data[, z]/Increment), FUN="sum", na.rm=TRUE)
      agr_n <- aggregate(data[, y] ~ ceiling(data[, z]/Increment), FUN="length")
      agr_df <- data.frame(z_lev= as.integer(c(agr_sum[, 1])), agr_sum=agr_sum[, 2], agr_n=agr_n[, 2])
    }
    #Calculates confidence intervals
    adf_alpha <- matrix(ncol= 3, nrow= nrow(agr_df), byrow = TRUE)
    for (i in 1:nrow(agr_df)) {
      adf_alpha[i, ] <- unlist(poisson.test(x=agr_df[i,2], T=agr_df[i, 3], conf.level= conf_lev)[c("estimate","conf.int")])
    }
    adf_alpha <- data.frame(adf_alpha)
    colnames(adf_alpha) <- c("PointEst", "Lower", "Upper")
    agr_df <- cbind(agr_df, adf_alpha)
    return(agr_df=agr_df )
  }

  ftotconf <- function(y, z, data, conf_lev, Increment, dist) {
    switch(dist,                #"var" and can be used anywhere in server.r.
           "t" =  ftotTconf(y, z, data, conf_lev, Increment, asis),
           "b" =  ftotBconf(y, z, data, conf_lev, Increment),
           "p" =  ftotPconf(y, z, data, conf_lev, Increment)
    )
  }
  # Run the function above #
  if(!is.null(z)) {
    time_confint_all <- ftotconf(y=y, z=z, data=data, conf_lev=conf.int,
                                 Increment=Increment, dist=dist)
  } else {
    time_confint_all <- NULL
  }

  # Rolling time #
  fncRollTime <- function(data, x, y, z, Increment, dist="t", conf_lev=.95) {
    #Get summary of values
    Time.Period.Length <- length(unique(data[, z]))
    Increment.Length <- Time.Period.Length - (Increment - 1)
    Time.Period.Values <- unique(data[, z])
    oTPV <- order(Time.Period.Values)
    Time.Period.Values <- Time.Period.Values[oTPV]
    #Get time periods for aggregating rates
    i <- 1
    Time.Periods <- list()
    while( i <= Increment.Length) {
      Time.Periods[[i]] <- Time.Period.Values[i:(i + (Increment -1))]
      i = i + 1
    }
    #Main title: Get first and last time points
    Time.Start.Label <- lapply(Time.Periods, `[[`, 1)
    Time.Stop.Label <- lapply(Time.Periods, `[[`, Increment)
    #Aggregate values
    AggrY <- list()
    for (i in 1:length(Time.Periods )) {
      switch(dist,
             "t"   = AggrY[[i]] <- tconf(x=x, y=y, data=data[data[, z] %in% Time.Periods[[i]], ],
                                         conf_lev=conf_lev),
             "b"   = AggrY[[i]] <- bconf(x=x, y=y, data=data[data[, z] %in% Time.Periods[[i]], ],
                                         conf_lev=conf_lev),
             "p"   = AggrY[[i]] <- pconf(x=x, y=y, data=data[data[, z] %in% Time.Periods[[i]], ],
                                         conf_lev=conf_lev)
      )
    }
    #Add in other values to each of the 3 lists
    for (i in 1:length(AggrY )) {
      AggrY[[i]][[1]]$Rolling <- Increment
      AggrY[[i]][[2]]$Rolling <- Increment
      AggrY[[i]][[3]]$Rolling <- Increment
    }
    for(j in 1:length(Time.Periods )) {
      for (i in 1:3) {
        AggrY[[j]][[i]]$Start <- Time.Start.Label[[j]]
        AggrY[[j]][[i]]$Stop <- Time.Stop.Label[[j]]
      }
    }
    roll_alpha <- do.call(rbind.data.frame, lapply(AggrY, `[[`, 1))
    roll_all <- do.call(rbind.data.frame, lapply(AggrY, `[[`, 3))
    return(list("Rolling"=roll_alpha, "Rolling.All"=roll_all))
  }
  # Run the function above #
  if(!is.null(rolling)) {
    roll_confint <- fncRollTime(data=data, x=x, y=y, z=z, Increment=rolling,
                                dist=dist, conf_lev=conf.int)
  } else {
    roll_confint <- NULL
  }

  #Group level clustering ICC, MOR, Design effects
  fncIccMorDe <- function(x, y, data, dist="t") {
    # one-way ANOVA
    anova_model <- aov(data[, y] ~ data[, x])
    # ANOVA table
    anova_results <- summary(anova_model)
    # Mean squares
    ms_bw <- anova_results[[1]]$"Mean Sq"[1]
    ms_wn <- anova_results[[1]]$"Mean Sq"[2]
    # number of observations per group
    k <- length(unique(data[, x]))
    #Average number per cluster
    kn <- mean(table(data[, x]))

    # Intraclass correlation #
    if(dist %in% c("t","p")) {
      ICC <- (ms_bw - ms_wn) / (ms_bw + (kn - 1) * ms_wn)
    }
    if(dist == "b") {
      ICC <- ms_bw/ (ms_bw + (3.14^2/3))
    }
    # Median Odds Ratio (MOR) #
    if(dist == "b") {
      MOR <- exp(sqrt(2 * ICC) * qnorm(0.75))
    } else {
      MOR <- NA
    }
    # Design effects (>= 2, don't ignore clustering) #
    DE <- 1 + (kn - 1) * ICC
    return(c("Intraclass.Correlation.Coefficient"=ICC, "Median.Odds.Ratio"=MOR,
             "Dessign.Effect"=DE))
  }
  #Run the function above
  if(cluster==TRUE) {
    cluster_res <- fncIccMorDe(x=x, y=y, data=data, dist=dist)
  } else {
    cluster_res <- NULL
  }

  main_confint <- conf(x=x, y=y, data=data, conf_lev=conf.int, dist=dist)
  if(!is.null(z)) {
    time_confint <- fconf(x=x, xlev=NULL, y=y, z=z, data, conf_lev=conf.int,
                          Increment=Increment, dist=dist)
  } else {
    time_confint <- NULL
  }
  # return list
  xyz <- list(Group.CI=main_confint, Time.CI=list(Group=time_confint, All=time_confint_all),
       Roll.CI=roll_confint, Clustering=cluster_res, Quartiles=Quartiles,
       Variables=c(x=x, y=y, z=z), CI=conf.int)
  class(xyz) <- c("group","ham", "list")

  return(xyz)
}


