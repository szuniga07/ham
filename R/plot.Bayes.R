#' Bayesian plots for various analyses
#'
#' Graph X-bar charts, p-charts, and u-charts. This includes
#' producing means center lines, 3-sigma upper and lower control limits. Users can also calculate
#' values before and after an intervention to see if a change in the control process happened. Values are
#' returned in a data frame.
#'
#' @param x Bayes object.
#' @param y character vector for the type of plot to graph. Select 'post', 'dxa', 'dxd', 'dxg', 'dxt', 'check',
#' 'multi', or 'target' for posterior summary, diagnostics (4 'dx' plots produced: autocorrelation factor,
#' density plots on chain convergence, Gelman-Rubin statistic, and traceplot), posterior predictive check,
#' multilevel or hierarchical model summary, or target summary plots. Default is 'post'.
#' @param ctype character vector of length == 1 that indicates posterior predictive check type when y='check'.
#' Posterior predictive checks allow us to see how well our estimates match the observed data. These checks are
#' available for Bayesian estimation of outcomes and regression polynomial trend line using various distributions in the
#' likelihood function. Select 'n', 'ln', 'sn', 'w', 'g', 't', 't1', 'taov', 'ol', 'oq','oc', 'hol', 'hoq', 'hoc',
#' 'hlol', 'hloq', 'hloc', 'odid', 'logl', 'logq', 'logc', 'hlogl', 'hlogq', 'hlogc' for these respective options:
#' 'Normal', 'Log-normal', 'Skew-normal', 'Weibull', 'Gamma', 't', 't: 1 group', 't: ANOVA', 'OLS: Linear',
#' 'OLS: Quadratic', 'OLS: Cubic', 'Hierarchical OLS: Linear', 'Hierarchical OLS: Quadratic', 'Hierarchical OLS: Cubic',
#' 'Hierarchical Log OLS: Linear', 'Hierarchical Log OLS: Quadratic', 'Hierarchical Log OLS: Cubic', 'OLS: DID',
#' 'Logistic: Linear', 'Logistic: Quadratic', 'Logistic: Cubic', 'Hierarchical Logistic: Linear',
#' 'Hierarchical Logistic: Quadratic', and 'Hierarchical Logistic: Cubic'. The first 8 selections are for Bayesian
#' estimation of outcomes and the remaining options were developed to assess regression trend lines. Default is NULL.
#' @param parameter a character vector of length >= 1 or a 2 element list with the name(s) of parameter in MCMC chains to produce
#' summary statistics. Use a 1 element vector to get posterior estimates of a single parameter. Use a 2 or more element vector
#' to estimate the average joint effects of multiple parameters (e.g., average infection rate for interventions A and B when
#' parameter= c('IntA', 'IntB')). Use a 2 element list to perform mathematical calculations of multiple parameters (see 'math' below).
#' For example, use parameter=list('hospital_A', 'hospital_Z') if you want to estimate the difference between the hospital's outcomes.
#' Use parameter= list(c('hospital_A','hospital_B'), ('hospital_Y','hospital_Z')) to estimate how different the combined hospitals A
#' and B values are from the combined Hospital Y and Z values. When y='check', use either a multiple element character vector that represents
#' center, spread, and additional distribution parameters in order (e.g., mean, sd, nu from a t-distribution) or regression parameters in order (e.g., intercept, B1).
#' When y='multi', use a multiple element character vector to list the parameter names of the hierarchy, in order of the
#' nesting (e.g., responses, person, organization).
#' @param center character vector that selects the type of central tendency to use when reporting parameter values.
#' Choices include: 'mean', 'median', and 'mode'. Default is 'mode'.
#' @param mass numeric vector the specifies the credible mass used in the Highest Density Interval (HDI). Default is 0.95.
#' @param compare numeric vector with one comparison value to determine how much of the distribution is above or below
#' the comparison value. Default is NULL.
#' @param rope numeric vector with two values that define the Region of Practical Equivalence (ROPE).
#' Test hypotheses by setting low and high values to determine if the Highest Density Interval (HDI)
#' is within or without the ROPE. Parameter value declared not credible if the entire ROPE lies
#' outside the HDI of the parameter’s posterior (i.e., we reject the null hypothesis). For example,
#' the ROPE of a coin is set to 0.45 to 0.55 but the posterior 95% HDI is 0.61 - 0.69 so we reject
#' the null hypothesis value of 0.50. We can accept the null hypothesis if the entire 95% HDI falls with the ROPE. Default is NULL.
#' @param data object name for the observed data when y='check' or y='multi'.
#' @param dv character vector of length == 1 for the dependent variable name in the observed data frame
#' when y='check' or y='multi'.
#' @param iv character vector of length >= 1 for the independent variable name(s) in the observed data frame
#' when y='check' or y='multi'.
#' @param group character list of length == 2 for 1) the grouping variable name and 2) specific group(s) in the
#' observed data frame. This is primarily used for multilevel or hierarchical models when y='check' or y='multi'
#' that the hierarchies are based on (e.g., hospitals nested within health systems).
#' @param add.data character vector of length == 1 to determine the type of observed data added to the plot
#' when y='check'. Select 'a', 'u', 'al', 'ul', 'dg', 'n' for these options: 'All', 'Unit','All: Lines',
#' 'Unit: Lines','DID: Groups', 'none'. Default is 'n' for none.
#' @param main the main title of the plot.
#' @param xlab a character vector label for the x-axis.
#' @param ylab a character vector label for the y-axis.
#' @param xlim specify plot's x-axis limits with a 2 element numeric vector.
#' @param ylim specify plot's y-axis limits with a 2 element numeric vector.
#' @param vlim two element vector to specify limits for minimum and maximum values used to extrapolate posterior
#' lines along the x-axis. For example, when drawing a log-normal distribution, we may want to have our
#' posterior lines fit within a narrower range while having our graph's x-axis limits extend past those lines.
#' If so, our value limits (vlim) help us keep our posterior predictive check lines within desired limits.
#' Default is NULL.
#' @param curve select a curve to display instead of a histogram when y='post'. Default is FALSE.
#' @param lwd select the line width.
#' @param breaks number of breaks in a histogram. Default is 15.
#' @param bcol a single or multiple element character vector to specify the bar or band color(s).
#' When Bayesian estimates and observed values are present, the first colors are Bayesian estimates
#' while the last colors are observed values. Defaults to, if nothing selected, 'gray'.
#' @param lcol a single or multiple element character vector to specify the line color(s).
#' When Bayesian estimates and observed values are present, the first colors are Bayesian estimates
#' while the last colors are observed values. When multiple lines are needed, single use lines
#' precede multiple use lines. For example, a single comparison value line will be assigned the first lcol
#' while both rope lines will be given the same color of the second lcol when y='post'. Defaults to 'gray'
#' if nothing selected.
#' @param pcol a single or multiple element character vector to specify the point color(s).
#' When Bayesian estimates and observed values are present, the first colors are Bayesian estimates
#' while the last colors are observed values. Defaults to, if nothing selected, 'gray'.
#' @param xpt a numeric vector of single or multiple values that indicate placement of points (+) on the
#' x-axis when y='check'. This is intended for the graphs with predictive checks on Bayesian estimation
#' (i.e., not trend lines). Default is NULL.
#' @param tgt specify 1 or more values on the y-axis of where to add one or more horizontal target lines. Default is NULL.
#' @param tgtcol select one or multiple colors for one or multiple target lines. Default is 'gray'.
#' @param tpline add one or more time point vertical lines using x-axis values. Default is NULL (i.e., no lines).
#' @param tpcol specify a color for the time point line, tpline. Default is NULL.
#' @param pline a numeric vector of length == 1 for the number of random posterior predictive check
#' lines when y='check'. Default is 20.
#' @param add.legend add a legend by selecting the location as "bottomright", "bottom", "bottomleft",
#' "left", "topleft", "top", "topright", "right", "center". No legend if nothing selected.
#' @param legend a character vector of length >= 1 to appear when y='check' or y='multi'. Legends to represent
#' hierarchical estimates and observed values.
#' @param cex A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default of 1.
#' @param cex.axis The magnification to be used for axis annotation relative to the current setting of cex.
#' @param cex.lab The magnification to be used for x and y labels relative to the current setting of cex.
#' @param cex.main The magnification to be used for main titles relative to the current setting of cex.
#' @param cex.text The magnification to be used for the iname text added into the plot relative to the current setting of 1.
#' @param cex.legend The magnification to be used for the legend added into the plot relative to the current setting of 1.
#' @param HDItext numeric vector of length == 1 that can be a negative or positive value. Identifies placement of HDI text near credible interval
#' when y='post'. Values are relative to the x-axis values. Default is 0.7.
#' @param math mathematics function performed between multiple parameters when y='post'. Available functions are: 'add', 'subtract', 'multiply',
#' 'divide', or 'n' for none (i,e., no functions). Indicate parameters with parameter argument.
#' For example, when math='subtract', use parameter=list('hospital_A', 'hospital_Z') if you want to
#' estimate the difference between the hospital's outcomes. Use parameter=list(c('hospital_A','hospital_B'),
#' ('hospital_Y','hospital_Z')) to estimate how different the combined hospitals A and B values are from the
#' combined hospitals Y and Z. Additionally, compute statistics like the coefficient of variation when math='divide'
#' and parameter= list('Standard_Deviation', 'Mean'). Default is 'n' for no math function.
#' @param es one element vector that indicates which type likelihood distribution is relevant in calculating Jacob Cohen's
#' effect sizes between 2 parameters when y='post'. Options are 'beta' and 'n' for the beta distribution for binary outcomes
#' and none (i.e., no distribution, hence no effect size calculated). For example, to get the posterior distribution summary
#' for the difference between the intervention and control groups on 30-day readmissions or not, use es='beta' when y='post',
#' math='subtract', and parameter=list('intMean', 'ctlMean'). Default is 'n' which indicates not to calculate the effect size.
#' @param round.c an integer indicating the number of decimal places when rounding numbers such as for y.axis.
#' Default is 2.
#' @param ... additional arguments.
#'
#' @return plot of Shewhart control charts: X-bar charts, p-charts, and u-charts with 3-sigma control limits.
#' @importFrom graphics lines plot abline points text arrows hist layout matplot mtext plot.new
#' @importFrom utils head tail
#' @importFrom methods is
#' @importFrom stats lm acf cor dgamma dlnorm dnorm dt dweibull pbeta pgamma plnorm pnorm pweibull qbeta qgamma qlnorm qweibull runif
#' @export
#' @references
#' Kruschke, J. (2014). Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and
#' Stan, Second Edition. New York: Academic Press. ISBN: 9780124058880
#'
#' @examples
#' ## Hospital LOS and readmissions ##


plot.Bayes <- function(x, y=NULL, ctype="n", parameter=NULL, center="mode", mass=0.95, compare=NULL, rope=NULL,
                       data=NULL, dv=NULL, iv=NULL, group=NULL, add.data="n",
                       main=NULL, xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, vlim=NULL, curve=FALSE, lwd=NULL, breaks=15,
                       bcol=NULL, lcol=NULL, pcol=NULL, xpt=NULL, tgt=NULL, tgtcol="gray", tpline=NULL, tpcol=NULL,
                       pline=20, add.legend=NULL, legend=NULL, cex=1, cex.lab=NULL, cex.axis=NULL, cex.main=NULL,
                       cex.text=NULL, cex.legend=NULL, HDItext=0.7, math="n", es="n", round.c=2, ...) {
  if (any(class(x) == "Bayes") == FALSE) {stop("Error: Expecting Bayes class object." )}
  #Looking for 1 parameter name
  if(!center %in% c("mode","median","mean")) {
    stop("Error: Expecting center as either 'mode', 'median', or 'mean'.")
  }
  # ensure parameter list isn't used for non-math function 'post' graphs
  if(math != "n") {
    if(!is.list(parameter)) {
      stop("Error: Expecting a parameter list when math is not 'n'.")
    }
  }
  if(math == "n") {
    if(is.list(parameter)) {
      stop("Error: Expecting a character vector when math is 'n'.")
    }
  }
  # ensure no other distributions for effect sizes are used right now
  if(es != "n") {
    if(es != "beta") {
      stop("Error: Expecting 'beta' distribution for es argument or 'n'.")
    }
  }
  # ensure parameter only has 1 item for diagnostics
  if(y %in% c('dxa', 'dxd', 'dxg', 'dxt')) {
    if(length(parameter) != 1) {
      stop("Error: Expecting 1 single parameter when doing diagnostics.")
    }
  }
  # ensure data and dependent variable are both selected if needed
  if(!is.null(data)) {
    if(is.null(dv)) {
      stop("Error: Expecting 'dv' argument has an input when 'data' argument is not NULL.")
    }
  }
#Hierarchical group variable and group level
  if ( !is.null(ctype)) {
    if ( !is.null(group)) {
      if ( length(group) != 2) {
        stop("Error: Expecting a 2 element list with group variable and specific group level.")
      }
    }
  }
#Assign new objects
  MCMC <- x$MCMC
  cenTend <- center
  compVal <- compare
  ROPE <- rope
  credMass <- mass
  showCurve <- curve
  HDItextPlace <- HDItext
  #bar colors
  if(is.null(bcol)) {
    bcol <- "gray"
  } else {
    bcol <- bcol
  }
  #point colors
  if(is.null(pcol)) {
    pcol <- "gray"
  } else {
    pcol <- pcol
  }
  #Line colors
  if(is.null(lcol)) {
    lcol <- "gray"
  } else {
    lcol <- lcol
  }
  #######
  # CEX #
  #######
  #Make CEX for legend
  if(!is.null(cex.legend)) {
    cex.legend <- cex.legend
  } else {
    cex.legend <- 1
  }
  #cex.axis
  if(!is.null(cex.axis)) {
    cex.axis <- cex.axis
  } else {
    cex.axis <- 1
  }
  #cex.lab
  if(!is.null(cex.lab)) {
    cex.lab <- cex.lab
  } else {
    cex.lab <- 1
  }
  #cex.main
  if(!is.null(cex.main)) {
    cex.main <- cex.main
  } else {
    cex.main <- 1
  }
  #cex.text
  if(!is.null(cex.text)) {
    cex.text <- cex.text
  } else {
    cex.text <- 1
  }

  #Chain statistics
  if(y %in% c('dxa', 'dxd', 'dxg', 'dxt')) {
  n_rows <- dim(MCMC[, parameter, drop=FALSE])[1]
  n_chains <- max(MCMC[, "CHAIN"])
  n_rowchn <- n_rows/n_chains
  DBDAplColors = c("skyblue","black","royalblue","steelblue")
  }

  ####################
  # Effect size Beta #
  ####################
  fncESBeta <-  function(yVal1, yVal2) {
    as1 <- (asin(sign( yVal1)  * sqrt(abs( yVal1 ))))*2
    as2 <- (asin(sign( yVal2 ) * sqrt(abs( yVal2 ))))*2
    Effect.Size.Output <- abs(as1 - as2 )

    return("Effect.Size.Posterior"=Effect.Size.Output )
  }

  ########################
  ## Set math functions ##
  ########################
  paramSampleVec <- switch(math,
                           "n" =   rowMeans(as.matrix(MCMC[, parameter, drop=FALSE])),
                           "add" =  rowMeans(as.matrix(MCMC[, parameter[[1]], drop=FALSE ])) + rowMeans(as.matrix(MCMC[, parameter[[2]], drop=FALSE ])),
                           "subtract" = if(es== "beta") fncESBeta(rowMeans(as.matrix(MCMC[, parameter[[1]], drop=FALSE ])), rowMeans(as.matrix(MCMC[, parameter[[2]], drop=FALSE ]))) else
                             rowMeans(as.matrix(MCMC[, parameter[[1]], drop=FALSE ])) - rowMeans(as.matrix(MCMC[, parameter[[2]], drop=FALSE ])),
                           "multiply" = rowMeans(as.matrix(MCMC[, parameter[[1]], drop=FALSE ])) * rowMeans(as.matrix(MCMC[, parameter[[2]], drop=FALSE ])),
                           "divide" = rowMeans(as.matrix(MCMC[, parameter[[1]], drop=FALSE ])) / rowMeans(as.matrix(MCMC[, parameter[[2]], drop=FALSE ]))
  )


################################################################################


  ################################################################################
  #                           Bayesian Analysis                                  #
  ################################################################################

  ################################################################################
  #                  1. Expand aggregated data into  full data                   #
  ################################################################################
  #This function expands y ~ x1 + X2 BINARY aggregated data in multi-row data
  #X1= lower level hierarchy (e.g., patients), X2= higher level hierarchy (e.g., States)
  #Z= Outcome, N= Total count of denominator (e.g., Z/N= rate)
  fncExpAgr <- function(DF, X1, X2, Z, N, Level) {
    #Add variable that tracks number of 0s
    t_exp_df <- DF
    t_exp_df$Yzero <- t_exp_df[, N] - t_exp_df[, Z]
    #Total rows
    tot_rows <- nrow(DF)
    #For loop to get the vectors of 0s and 1s
    z_ls <- vector(mode = "list", length = tot_rows)
    #Level 1 or 2 models
    if (Level <= 2) {
      for (i in 1:tot_rows) {
        z_ls[[i]] <- data.frame(Z=c(rep(0, t_exp_df[i, "Yzero"]), rep(1, t_exp_df[i, Z])),
                                X1=rep(t_exp_df[i, X1], t_exp_df[i, N]))
      }
    }
    #Level 3 models
    if (Level == 3) {
      for (i in 1:tot_rows) {
        z_ls[[i]] <- data.frame(Z=c(rep(0, t_exp_df[i, "Yzero"]), rep(1, t_exp_df[i, Z])),
                                X1=rep(t_exp_df[i, X1], t_exp_df[i, N]),
                                X2=rep(t_exp_df[i, X2], t_exp_df[i, N]))
      }
    }
    #Turn list into data frame
    ExpDF <- do.call(rbind.data.frame, z_ls)
    #Give column names
    if (Level <= 2) {
      colnames(ExpDF) <- c(Z, X1)
    }
    if (Level == 3) {
      colnames(ExpDF) <- c(Z, X1, X2)
    }
    return("ExpDF"=ExpDF)
  }

  ################################################################################
  # 2. Function to get the binary (non)hierarchical estimation posterior summary #
  ################################################################################
  #Uses DBDA function below, summarizePost()
  #MCmatrix= MCMC matrix after: mcmcMat <- as.matrix(codaSamples, chains=TRUE)
  #mydf: Original data frame (i.e., not the data list used in JAGS)
  #Level= A 1, 2, or 3 level indicator for the type of model
  #Outcome= Model outcome
  #Group2 & Group3= level 2 and 3 group names
  fncHdiBinSmry <- function(MCmatrix, datFrm, Level, Outcome, Group2, Group3=NULL,
                            Theta=NULL, Omega2=NULL, Omega3=NULL, Average=NULL,
                            AggrDF="No", AggrN=NULL, Distribution=NULL, Cred.Mass=0.95 ) {
    ###################################################################
    ## These next 30 lines will exclude irrelevant parameters ##
    keep_theta_cols <- NULL
    keep_omega2_cols <- NULL
    keep_omega3_cols <- NULL
    #Level-1, non-hierarchical model
    if(Level== 1) {
      keep_theta_cols <- grep(Theta, colnames(MCmatrix))
    }
    #Level-2, hierarchical model
    if(Level== 2) {
      keep_theta_cols <- grep(Theta, colnames(MCmatrix))
    }
    if(Level== 2) {
      keep_omega2_cols <- grep(Omega2, colnames(MCmatrix))
    }
    #Level-3, hierarchical model
    if(Level== 3) {
      keep_theta_cols <- grep(Theta, colnames(MCmatrix))
    }
    if(Level== 3) {
      keep_omega3_cols <- grep(Omega3, colnames(MCmatrix))
    }
    if(Level== 3) {
      keep_omega2_cols <- setdiff(grep(Omega2, colnames(MCmatrix)), keep_omega3_cols)
    }
    #These are just the columns I need
    keep_these_cols <- c(which(colnames(MCmatrix) == "CHAIN"), keep_theta_cols, keep_omega2_cols, keep_omega3_cols)
    #Revise MCMC matrix to just the columns I need
    MCmatrix <- MCmatrix[, keep_these_cols]
    ###################################################################
    #Get order of participants in rows of aggregated data
    if (AggrDF == "Yes") {
      group2_aggr_factor <- factor(datFrm[, Group2], levels=c(datFrm[, Group2]))
    } else {
      group2_aggr_factor <- levels(factor(datFrm[, Group2], levels=names(table(sort(datFrm[, Group2]))) ))
    }
    # Use the aggregated data if needed
    if (AggrDF == "Yes") {
      datFrm <- fncExpAgr(DF=datFrm, X1=Group2, X2=Group3, Z=Outcome, N=AggrN, Level=Level)
    } else {
      datFrm <- datFrm
    }
    #Make a factor from aggregated data so that it follows the same order
    if (AggrDF == "Yes") {
      datFrm[, Group2] <- factor(datFrm[, Group2], levels=group2_aggr_factor)
    } else {
      datFrm[, Group2] <- factor(datFrm[, Group2], levels=group2_aggr_factor)
    }
    #Get the type of estimate
    if (is.null(Average)) {
      average_type <- "Mode"
    } else {
      average_type <- Average
    }
    #Number of level-2 groups
    numGroups <- length(table(datFrm[, Group2]))
    #Number of level-3 categories
    if(Level== 3) {
      numCats <- length(table(datFrm[, Group3]))
    }
    #Get the level-2 group names
    Group2.Names <- sort(unique(datFrm[, Group2]))
    #Get the level-3 group names
    if(Level== 3) {  #Get group-3 rates
      Group3.Names <- sort(unique(datFrm[, Group3]))
    } else {
      Group3.Names <- NULL
    }
    #Get the column numbers with the Theta and Omega in it
    #Level-1, non-hierarchical model
    if(Level== 1) {
      theta_cols <- grep(Theta, colnames(MCmatrix))
    }
    #Level-2, hierarchical model
    if(Level== 2) {
      theta_cols <- grep(Theta, colnames(MCmatrix))
    }
    if(Level== 2) {
      omega2_cols <- grep(Omega2, colnames(MCmatrix))
    }
    #Level-3, hierarchical model
    if(Level== 3) {
      theta_cols <- grep(Theta, colnames(MCmatrix))
    }
    if(Level== 3) {
      omega3_cols <- grep(Omega3, colnames(MCmatrix))
    }
    if(Level== 3) {
      omega2_cols <- setdiff(grep(Omega2, colnames(MCmatrix)), omega3_cols)
    }

    #Get the column numbers with the Theta and Omega in it
    mat_cols <- 1:length(colnames(MCmatrix))      #Get range of matrix columns
    #Level-1, non-hierarchical model
    if(Level== 1) {
      pName <- colnames(MCmatrix)
    }
    #Level-2, hierarchical model
    if(Level== 2) {
      pName <- colnames(MCmatrix)[c(1, theta_cols, omega2_cols, setdiff(mat_cols, c(1, theta_cols, omega2_cols) ))]
    }
    #Level-3, hierarchical model
    if(Level== 3) {
      pName <- colnames(MCmatrix)[c(1, theta_cols, omega2_cols, omega3_cols, setdiff(mat_cols, c(1, theta_cols, omega2_cols, omega3_cols)))]
    }

    ## Create posterior chain summary ##
    # pName <- colnames(MCmatrix)   #Parameter names
    postDF <- list()
    #  for (i in 1:length(pName[-1]))  {
    for (i in 1:length(pName[which(pName != "CHAIN")]))  {
      #    postDF[[i]] <- summarizePost( MCmatrix[, pName[i] ] , compVal=NULL , ROPE=NULL )
      postDF[[i]] <- summarizePost( MCmatrix[, pName[which(pName != "CHAIN")[i]] ] , compVal=NULL , ROPE=NULL, credMass=Cred.Mass )
    }
    #Turn summary into data frame
    postDF <- data.frame(do.call( "rbind", postDF))
    #  rownames(postDF) <- pName[-1]
    rownames(postDF) <- pName[which(pName != "CHAIN")]
    ## Get number of parameters to create Group 2 variable
    m_param_tot <- length(colnames(MCmatrix)) - 1 #Total parameters from MCmatrix
    #Level-2, hierarchical model
    if(Level== 2) {
      param2_so_far <- length(c(theta_cols, omega2_cols))
      other_param2 <- m_param_tot - param2_so_far
      num_rep_Group2 <- other_param2 + 1
    }
    #Level-3, hierarchical model
    if(Level== 3) {
      param3_so_far <- length(c(theta_cols, omega2_cols, omega3_cols))
      other_param3 <- (m_param_tot - param3_so_far) / (numCats + 1)
      num_rep_Group3 <- other_param3
    }
    #Enter Group/Cat into summary
    #Level-1, non-hierarchical model
    if(Level== 1) {
      row_name <- c(names(table(datFrm[, Group2])) )
    }
    #Level-2, hierarchical model
    if(Level== 2) {
      row_name <- c(names(table(datFrm[, Group2])), rep("Overall", num_rep_Group2) )
    }
    #Level-3, hierarchical model. try() used if "omega" is only param passed into JAGS function
    if(Level== 3) {
      row_name <- c(names(table(datFrm[, Group2])),
                    rep(names(table(datFrm[, Group3])), 1), "Overall",
                    try(rep(names(table(datFrm[, Group3])), num_rep_Group3)), try(rep("Overall", num_rep_Group3)) )
    }
    #Make a variable for the group 2 and 3 names
    postDF[, Group2] <- row_name

    #Enter Group/Cat counts
    #Level-1, non-hierarchical model
    if(Level== 1) {
      postDF[, Outcome] <- c(table(datFrm[, Outcome], datFrm[, Group2])[2,])
    }
    #Level-2, hierarchical model
    if(Level== 2) {
      postDF[, Outcome] <- c(table(datFrm[, Outcome], datFrm[, Group2])[2,],
                             rep(0, nrow(postDF) - length(table(datFrm[, Group2])) ))
    }
    #Level-3, hierarchical model
    if(Level== 3) {
      postDF[, Outcome] <- c(table(datFrm[, Outcome], datFrm[, Group2])[2,],
                             table(datFrm[, Outcome], datFrm[, Group3])[2,],
                             rep(0, nrow(postDF) -
                                   (length(table(datFrm[, Group2])) + length(table(datFrm[, Group3]))) ))
    }
    #Enter Group/Cat Ns
    #Level-1, non-hierarchical model
    if(Level== 1) {
      postDF[, "N"] <- c(colSums(table(datFrm[, Outcome], datFrm[, Group2])))
    }
    #Level-2, hierarchical model
    if(Level== 2) {
      postDF[, "N"] <- c( colSums(table(datFrm[, Outcome], datFrm[, Group2])),
                          rep(0, nrow(postDF) - length(table(datFrm[, Group2])) ))
    }
    #Level-3, hierarchical model
    if(Level== 3) {
      postDF[, "N"] <- c( colSums(table(datFrm[, Outcome], datFrm[, Group2])),
                          colSums(table(datFrm[, Outcome], datFrm[, Group3])),
                          rep(0, nrow(postDF) -
                                (length(table(datFrm[, Group2])) + length(table(datFrm[, Group3]))) ))
    }
    #Enter Group/Cat sums
    #Level-1, non-hierarchical model
    if(Level== 1) {
      cont_sum_ls1 <- by(datFrm[, Outcome], datFrm[, Group2], FUN=sum, na.rm = TRUE)
      class(cont_sum_ls1) <- "list"
      #cont_sum_ls1 <- factor(datFrm[, Group2], levels=group2_aggr_factor)
      postDF[, "Sum"] <- unlist(cont_sum_ls1)
    }
    #Level-2, hierarchical model
    if(Level== 2) {
      cont_sum_ls1 <- by(datFrm[, Outcome], datFrm[, Group2], FUN=sum, na.rm = TRUE)
      class(cont_sum_ls1) <- "list"
      postDF[, "Sum"] <- c( unlist(cont_sum_ls1),
                            rep(0, nrow(postDF) - length(table(datFrm[, Group2])) ))
    }
    #Level-3, hierarchical model
    if(Level== 3) {
      cont_sum_ls1 <- by(datFrm[, Outcome], datFrm[, Group2], FUN=sum, na.rm = TRUE)
      class(cont_sum_ls1) <- "list"
      cont_sum_ls2 <- by(datFrm[, Outcome], datFrm[, Group3], FUN=sum, na.rm = TRUE)
      class(cont_sum_ls2) <- "list"
      postDF[, "Sum"] <- c( unlist(cont_sum_ls1), unlist(cont_sum_ls2) ,
                            rep(0, nrow(postDF) -
                                  (length(table(datFrm[, Group2])) + length(table(datFrm[, Group3]))) ))
    }

    #Observed rate
    if (Distribution == "Beta") {
      postDF$Obs.Rate <- postDF[, Outcome] / postDF[, "N"]
    } else {
      postDF$Obs.Rate <- postDF[, "Sum"] / postDF[, "N"]
    }

    #Get the row numbers with the Theta and Omega in it
    #Level-1, non-hierarchical model
    if(Level== 1) {
      theta_rows <- grep(Theta, rownames(postDF))
    }
    #Level-2, hierarchical model
    if(Level== 2) {
      theta_rows <- grep(Theta, rownames(postDF))
    }
    if(Level== 2) {
      omega2_rows <- grep(Omega2, rownames(postDF))
    }
    #Level-3, hierarchical model
    if(Level== 3) {
      theta_rows <- grep(Theta, rownames(postDF))
    }
    if(Level== 3) {
      omega2_rows <- grep(Omega2, rownames(postDF))
    }
    if(Level== 3) {
      omega3_rows <- grep(Omega3, rownames(postDF))
    }
    #First make length of theta, omega2 and omega3 rows
    if(Level== 1) {
      LTR <- length(theta_rows)
    }
    if(Level== 1) {
      LO2R<- NULL
    }
    if(Level== 1) {
      LO3R <- NULL
    }
    if(Level== 2) {
      LTR <- length(theta_rows)
    }
    if(Level== 2) {
      LO2R <- length(omega2_rows)
    }
    if(Level== 2) {
      LO3R <- NULL
    }
    if(Level== 3) {
      LTR <- length(theta_rows)
    }
    if(Level== 3) {
      LO2R <- length(setdiff(omega2_rows, omega3_rows))
    }
    if(Level== 3) {
      LO3R <- length(omega3_rows)
    }
    #Create an order by parameter
    #Level-2, hierarchical model order of results
    if(Level== 1) {
      o6 <- c(order(postDF[, average_type][1:numGroups], decreasing = T))
    }
    #ISSUE: first 2 elements are kappas and skipped in the order below colnames(mcmcMatTT)
    if(Level== 2) {
      o6 <- c(order(postDF[theta_rows, average_type], decreasing = T),  #Theta
              omega2_rows[order(postDF[omega2_rows, average_type], decreasing = T)], #Omega2
              setdiff(1:nrow(postDF), c(theta_rows, omega2_rows)))  #All others
    }
    if(Level== 3) {
      o6 <- c(order(postDF[, average_type][theta_rows], decreasing = T), #Theta and then Omega2
              setdiff(omega2_rows, omega3_rows)[order(postDF[, average_type][setdiff(omega2_rows, omega3_rows)], decreasing = T)],
              omega3_rows[ order(postDF[, average_type][omega3_rows], decreasing = T)], #Omega3
              ((numGroups+numCats+LO3R ):nrow(postDF))[((numGroups+numCats+LO3R):nrow(postDF)) != omega3_rows] ) #All other
    }
    #Re-order the rows now
    if(Level== 1) {
      postDFb <- postDF[o6, ]
    }
    if(Level== 2) {
      postDFb <- rbind(postDF[o6[1:LTR], ],
                       postDF[o6[(LTR + 1):(LTR + LO2R )], ],
                       postDF[ o6[( (LTR + LO2R) + 1):nrow(postDF)], ])
    }
    if(Level== 3) {  #In this order: theta, Omega2, omega3
      postDFb <- rbind(postDF[o6[1:LTR], ],                                   #Thetas
                       postDF[o6[(LTR + 1):(LTR + LO2R )], ],                 #omega2
                       postDF[o6[(LTR + LO2R + 1):(LTR + LO2R + 1)], ],       #omega3
                       postDF[ o6[( (LTR + LO2R + LO3R) + 1):nrow(postDF)], ])  #all others
    }
    ## Put postDF in reverse order so that it will plot correctly
    if(Level== 1) {
      postDFa <- postDF[rev(1:LTR), ]
    }
    if(Level== 2) {
      postDFa <- rbind(postDF[rev(1:LTR), ],
                       postDF[rev(omega2_rows), ],
                       postDF[ rev(( (LTR + LO2R) + 1):nrow(postDF)), ])
    }
    if(Level== 3) {  #In this order: theta, Omega2, omega3
      postDFa <- rbind(postDF[ rev(1:LTR), ],                                   #Thetas
                       postDF[ rev((LTR + 1):(LTR + LO2R )), ],                 #omega2
                       postDF[ omega3_rows, ],                                  #omega3
                       postDF[ rev(setdiff(1:(nrow(postDF)), c(1:(LTR + LO2R), omega3_rows))), ])  #all others
    }

    ## Get the level-3 rates if doing a level-3 model for the plot points
    #I Changed Area into a 3 level factor to get a better 3rd level
    if(Level== 3) {
      hspa <- aggregate(datFrm[, Outcome] ~ datFrm[, Group3] + datFrm[, Group2] , data=datFrm, FUN="sum")
    }
    if(Level== 3) {
      hspb <- aggregate(datFrm[, Outcome] ~ datFrm[, Group3] + datFrm[, Group2] , data=datFrm, FUN="length")
    }
    #Merge
    if(Level== 3) {
      a1hsp <- cbind(hspa, hspb[, 3])
    }
    if(Level== 3) {
      colnames(a1hsp)[4] <- "Nsamp"
    }
    if(Level== 3) {
      colnames(a1hsp)[1:3] <- c(Group3, Group2, Outcome)
    }
    ##(Add 1 to month and) make it a factor so it begins at 1
    if(Level== 3) {
      a1hsp[, Group2] <- as.numeric(a1hsp[, Group2])
    }
    if(Level== 3) {
      a1hsp[, Group3] <- as.numeric( as.factor(a1hsp[, Group3]) )  #Turning into factor to get numeric
    }
    #Make the rate
    if(Level== 3) {
      a1hsp$Rate <- a1hsp[, (ncol(a1hsp)-1)] / a1hsp[, ncol(a1hsp)]
    }
    #For loop to create object
    if(Level== 3) {  #Get group-3 rates
      Group3.Obs <- list()
      for (i in 1:LO2R) {
        Group3.Obs[[i]] <- a1hsp[a1hsp[, Group3] == i, "Rate"]
      }
    } else {
      Group3.Obs <- NULL
    }
    #Reverse order to match Post1
    if(Level== 3) {  #Get group-3 rates
      Group3.Obs1 <- rev(Group3.Obs)
    } else {
      Group3.Obs1 <- NULL
    }
    #Get numerical order to match with Post2
    if(Level== 3) {  #Get group-3 rates
      g3_order <- as.numeric(gsub("[^0-9.-]", "", rownames(postDFb[ ((LTR + 1):(LTR + LO2R )), ]) ))
    } else {
      g3_order <- NULL
    }
    #Get level-3 estimates in numerical order
    if(Level== 3) {  #Get group-3 rates
      Group3.Obs2 <- Group3.Obs[g3_order]
    } else {
      Group3.Obs2 <- NULL
    }
    return(list("Post"=postDF,"Post1"=postDFa, "Post2"=postDFb, "Level"=Level, "Outcome"=Outcome,
                "Group2.Names"=Group2.Names, "Group3.Names"=Group3.Names,
                "Group2"=Group2, "Group3"=Group3,
                "Theta"=Theta, "Omega2"=Omega2, "Omega3"=Omega3, "Average"=Average,
                "Order"=o6, "LTR"=LTR, "LO2R"=LO2R, "LO3R"=LO3R,
                "Lower"= intersect("HDIlow", colnames(postDF)),
                "Upper"= intersect("HDIhigh",colnames(postDF)),
                "ciconf_lev"= unique(postDF$HDImass), "g3_order"=g3_order,
                "Group3.Obs1"=Group3.Obs1, "Group3.Obs2"=Group3.Obs2 ))
  }

  ################################################################################
  #           3. Function to plot HDIs for hierarchical estimation               #
  ################################################################################
  fncHdiBinP <- function(MCmatrix, Level, View.Order="Alphabetical", View.Level="No",  #View.Order= alpha/numerical, View.Level=Yes/No "View 3rd level?"
                         GroupX=NULL, Lcol, Pcol, P3.col, tgt=NULL, tgt.col,
                         Cbar, plyCol, labMulti=1, lineMulti=1,
                         roundVal, XLim1, XLim2,Add.Lgd, Leg.Loc) {
    # Assign objects from MCMC matrix objects
    Group2 <- MCmatrix$Group2
    Group3 <- MCmatrix$Group3
    Outcome <- MCmatrix$Outcome
    ciconf_lev <- MCmatrix$ciconf_lev
    Average <- MCmatrix$Average
    Theta <- MCmatrix$Theta
    Omega2 <- MCmatrix$Omega2
    Omega3 <- MCmatrix$Omega3
    LTR <- MCmatrix$LTR
    LO2R <- MCmatrix$LO2R
    LO3R <- MCmatrix$LO3R
    Lower <- MCmatrix$Lower
    Upper <- MCmatrix$Upper
    Group3.Obs1 <- MCmatrix$Group3.Obs1
    Group3.Obs2 <- MCmatrix$Group3.Obs2

    #Select the group levels that determine which rows go into the data frame
    if(Level== 1) {
      if (View.Level == "No") {
        row_numbers <- 1:LTR
      }
    }
    #Level-2, hierarchical model
    if(Level== 2) {
      if (View.Level == "No") {
        row_numbers <- 1:(LTR + LO2R)
      }
    }
    #Level-3, hierarchical model
    if(Level== 3) {
      if (View.Level == "Yes") {
        row_numbers <- setdiff(1:(LTR + LO2R + LO3R), 1:LTR)
      } else {
        row_numbers <- setdiff(1:(LTR + LO2R + LO3R), (LTR + 1):(LTR + LO2R))
      }
    }
    #Create hdidf table of Bayesian estimates
    if(View.Order == "Alphabetical") {                                  #Post1
      hdidf <- MCmatrix$Post1[row_numbers , c(Group2, Average, Lower, Upper, "Obs.Rate")]
    }
    if(View.Order == "Numerical") {                                  #Post2
      hdidf <- MCmatrix$Post2[row_numbers , c(Group2, Average, Lower, Upper, "Obs.Rate")]
    }
    #Create adf table of observed values
    if(View.Order == "Alphabetical") {                                  #Post1
      adf <- MCmatrix$Post1[row_numbers , c(Group2, "Obs.Rate")]
    }
    if(View.Order == "Numerical") {                                  #Post2
      adf <- MCmatrix$Post2[row_numbers , c(Group2, "Obs.Rate")]
    }
    #Hierarchical average for the highest level (e.g., Omega)
    if(Level >= 2) {
      mainYmn <- hdidf[nrow(hdidf), which(colnames(hdidf)== Average)]
    } else {
      mainYmn <- NA
    }
    #Select which 3-level category data gets reported
    if(View.Order == "Alphabetical") {                                  #Post1
      Group3.Obs <- Group3.Obs1
    }
    if(View.Order == "Numerical") {                                  #Post2
      Group3.Obs <- Group3.Obs2
    }
    #Main X labels
    if(Level >= 2) {
      X_Label <- paste0("Dashed line= Overall hierarchical est. of ", round(mainYmn, roundVal), ", ", ciconf_lev * 100, "% ", "HDI",
                        " [", round(hdidf[nrow(hdidf), which(colnames(hdidf)== "HDIlow")], roundVal), ", ",
                        round(hdidf[nrow(hdidf), which(colnames(hdidf)== "HDIhigh")], roundVal),"]")
    } else {
      X_Label <- paste0(Group2, " posterior estimates")
    }
    #Main title
    #Level-3, hierarchical model
    if(Level== 3) {
      if (View.Level == "Yes") {
        main_ttl <- paste0(ciconf_lev * 100, "% ", "Highest Density Intervals of ", Outcome, " by ", Group3)
      }
    }
    if(Level== 3) {
      if (View.Level == "No") {
        main_ttl <- paste0(ciconf_lev * 100, "% ", "Highest Density Intervals of ", Outcome, " by ", Group2)
      }
    }
    if(Level < 3) {
      main_ttl <- paste0(ciconf_lev * 100, "% ", "Highest Density Intervals of ", Outcome, " by ", Group2)
    }
    #Legend
    #Level-3, hierarchical model
    if(Level== 3) {
      if (View.Level == "Yes") {
        legend_text <- c(paste0("Observed ", Group2), paste0("Observed ", Group3), "Hierarchical Estimate")
        legend_type <- c(3, 2, 24)
        pcol_vector <- c(P3.col, Pcol, Pcol)
      }
    }
    if(Level== 3) {
      if (View.Level == "No") {
        legend_text <- c("Observed Rate", "Hierarchical Estimate")
        legend_type <- c(2, 24)
        pcol_vector <- Pcol
      }
    }
    if(Level == 2) {
      legend_text <- c("Observed Rate", "Hierarchical Estimate")
      legend_type <- c(2, 24)
      pcol_vector <- Pcol
    }
    if(Level == 1) {
      legend_text <- c("Estimate")
      legend_type <- c(24)
      pcol_vector <- Pcol
    }

    #Get names of level 1:3 or just overall level-3 groups
    if(Level== 3) {
      if (View.Level == "Yes") {
        group_names <- hdidf[-nrow(hdidf), Group2]
      } else {
        group_names <- hdidf[1:LTR, Group2]
      }
    } else {
      group_names <- hdidf[1:LTR, Group2]
    }
    #Determine which group names to plot
    if (is.null(GroupX)) {
      plot_group_names <- 1:length(group_names)
    } else {
      plot_group_names <- which(group_names %in% GroupX)
    }

    #Get rows to use for plots, level 1 print everything, other levels print everything - last row
    if(Level== 1) {
      plot_row_numbers <- (1:length(row_numbers))[plot_group_names]
    } else {
      plot_row_numbers <- (1:(length(row_numbers) - 1))[plot_group_names]
    }
    ## Create plot
    rng <- seq(min(adf[, "Obs.Rate"], na.rm=TRUE)* 0.95, max(adf[, "Obs.Rate"], na.rm=TRUE)* 1.05, length.out=nrow(adf[plot_row_numbers,]))
    plot(rng, 1:length(rng), type="n", ylab="",
         xlab= X_Label,
         axes=F,  cex.lab=1*labMulti, xlim=c(XLim1, XLim2))
    #axes=F,  cex.lab=1*labMulti)
    title(main_ttl, cex.main = 1*labMulti)
    #Merge 2 tables so I can get points in correct order
    for (i in 1:(length(plot_row_numbers)) ) {
      lines(c(hdidf[plot_row_numbers, Lower][i], hdidf[plot_row_numbers, Upper][i]),
            c((1:length(plot_row_numbers))[i], (1:length(plot_row_numbers))[i]),
            lwd=1*lineMulti, col=Lcol)
      #Points for observed rates and Bayesian estimates
      points(hdidf[plot_row_numbers, Average][i ], i, pch=24, col=Pcol, lwd=1, bg=Pcol, cex=1.75*labMulti)
      if(Level >= 2) {
        points(hdidf[plot_row_numbers, "Obs.Rate"][i ], (1:length(plot_row_numbers))[i], pch=2, col=Pcol, lwd=1, bg=Pcol, cex=1.75*labMulti)
      }
    }
    #Add points for the level-3 category for each group per category
    if(Level== 3) {
      if (View.Level == "Yes") {
        for (i in 1:LO2R) {
          points( Group3.Obs[[plot_row_numbers[i]]], rep( (1:LO2R)[i], length(Group3.Obs[[plot_row_numbers[i]]])), pch=3,
                  col=P3.col, lwd=1)
        }
      }
    }
    #Mean line
    if(Level >= 2) {
      abline(v=mainYmn, lwd=lwd, col="grey", lty=3)
    }
    axis(1)
    axis(2, at=1:length(plot_row_numbers), labels= substr(hdidf[plot_row_numbers, Group2], 1, 10), las=1, cex.axis=1*labMulti )
    axis(4, at=1:length(plot_row_numbers), labels= round(hdidf[plot_row_numbers, Average], roundVal), las=1, cex.axis= 1*labMulti*.75 )
    ## Add overall confidence bar ##
    #Create x and y data
    Cbar_x <- c(rep(hdidf[nrow(hdidf), Lower], length(plot_row_numbers)), rep(hdidf[nrow(hdidf), Upper], length(plot_row_numbers) ))
    Cbar_y <- c(1:length(plot_row_numbers), length(plot_row_numbers):1)
    #Create shading
    if(Cbar=="Yes") {
      polygon(Cbar_x, Cbar_y, col = adjustcolor(plyCol, alpha.f = 0.4), border= plyCol )
    }
    #Add legend
    if(Add.Lgd =="Yes") {
      legend(Leg.Loc, legend=legend_text, col=pcol_vector, lwd=lwd,
             pch=legend_type, pt.bg=pcol_vector, cex = 2, bty="n", inset=c(0, .05))
    }
    #Target line
    abline(v=tgt, lwd=lwd, col=tgt.col, lty=1)
    box()
  }

  ################################################################################
  #                  4. Posterior Predictive Check for groups                    #
  ################################################################################
  #Use the coda object and dataset. Works for normal and log-normal distributions.
  fncGrpPostPredCheck <- function(MCMC, datFrm, Outcome, Group=NULL, Group.Level=NULL,
                                  Mean.Var, SD.Var, MCnu=NULL, Distribution, Num.Lines=NULL,
                                  Main.Title=NULL, X.Lab=NULL, Y.Lab=NULL, #new
                                  Bar.Color=NULL, lwd=NULL, #new
                                  Line.Color=NULL, Hist.Breaks=NULL,
                                  #CEX.size=NULL,
                                  cex.lab=NULL, cex= 1, cex.main=NULL, cex.axis=NULL, #new
                                  X.Lim=NULL, Y.Lim=NULL,
                                  Min.Val=NULL, Max.Val=NULL, Round.Digits=NULL,
                                  Point.Loc= NULL, PCol=NULL,
                                  #Add.Lgd= NULL, #drop arg...replace with loc
                                  Leg.Loc= NULL, legend= NULL, cex.legend= NULL ) { #new
    #Make coda into as.matrix
    MC.Chain <- MCMC
    chainLength <- NROW(MC.Chain)  #Chain length
    #Get min and max value for key parameter
    if(is.null(Min.Val)) {
      Min.Val <- min(MC.Chain[, Mean.Var])
    }
    if(is.null(Max.Val)) {
      Max.Val <- max(MC.Chain[, Mean.Var])
    }
    #Get a number of pseudo-random chains
    pltIdx <- floor(seq(1, chainLength, length= Num.Lines))
    #Get spread in outcome variable values
    #  xComb <- seq( Min.Val , max(datFrm[, Outcome], na.rm=TRUE) , length=501 )
    xComb <- seq( Min.Val , Max.Val , length=501 )
    #Make X limit values, I can set my minimum value
    if (is.null(X.Lim)) {
      #    X.Lim <- c(Min.Val, round(max(datFrm[, Outcome], na.rm=TRUE), digits=Round.Digits))
      X.Lim <- c(Min.Val, round(Max.Val, digits=Round.Digits))
    }
    ## Graph ##
    #Allows me to run if I only have 1 group by leaving "generate levels =="No"
    if ( is.null(Group)) {
      hist( datFrm[, Outcome], xlab= X.Lab, ylab=Y.Lab,
            main= Main.Title, breaks=Hist.Breaks, col= Bar.Color, border="white",
            prob=TRUE, cex.lab=cex.lab, cex=cex, cex.main=cex.main,
            xlim=X.Lim, ylim=Y.Lim, axes=FALSE)
    } else {
      hist( datFrm[, Outcome][datFrm[, Group] == Group.Level], xlab= X.Lab, ylab=Y.Lab,
            main= Main.Title, breaks=Hist.Breaks, col= Bar.Color, border="white",
            prob=TRUE, cex.lab=cex.lab, cex=cex, cex.main=cex.main,
            xlim=X.Lim, ylim=Y.Lim, axes=FALSE)
    }
    axis(1, cex.axis=cex.axis)  #Put values in labels
    #This adds in minimum value in case it isn't in range (e.g., show negatve range of normal distribution)
    axis(1, at=X.Lim[1], cex.axis=cex.axis)
    axis(2, cex.axis=cex.axis)  #Put values in density
    # box()   #Dropping this for now because it looks better without
    #Add in posterior estimate lines
    for ( chnIdx in pltIdx ) {
      #Normal Distribution
      if (Distribution == "n") {
        lines( xComb ,
               dnorm( xComb, MC.Chain[chnIdx, Mean.Var], MC.Chain[chnIdx, SD.Var] ),
               col= Line.Color, lwd=lwd )
      }
      #Log-Normal Distribution
      if (Distribution == "ln") {
        lines( xComb ,
               dlnorm( xComb, MC.Chain[chnIdx, Mean.Var], MC.Chain[chnIdx, SD.Var] ),
               col= Line.Color , lwd=lwd)
      }
      #Skew-Normal Distribution
      if (Distribution == "sn") {
        lines( xComb ,
               dsn( xComb, xi=MC.Chain[chnIdx, Mean.Var], omega=MC.Chain[chnIdx, SD.Var],
                    alpha=MC.Chain[chnIdx, MCnu]), col= Line.Color, lwd=lwd )
      }
      #Weibull Distribution
      if (Distribution == "w") {
        lines( xComb ,
               dweibull( xComb, shape=MC.Chain[chnIdx, Mean.Var], scale=MC.Chain[chnIdx, SD.Var] ),
               col= Line.Color, lwd=lwd )
      }
      #Gamma Distribution
      if (Distribution == "g") {
        lines( xComb ,
               dgamma( xComb, shape=MC.Chain[chnIdx, Mean.Var], rate=MC.Chain[chnIdx, SD.Var] ),
               col= Line.Color, lwd=lwd )
      }
      #t Distribution
      if (Distribution == "t") {
        lines( xComb ,
               dt( xComb, df= MC.Chain[chnIdx, MCnu], ncp= MC.Chain[chnIdx, Mean.Var] ),
               col= Line.Color, lwd=lwd )
      }
      #Add points
      if (!is.null(Point.Loc)) {
        for (i in 1:length(Point.Loc)) {
          points(x=Point.Loc[i], y=0, pch=3, lwd=3, cex=cex, col=PCol)
        }
      }
      #Add legend
      if(!is.null(Leg.Loc) ) {
        legend_text <- if (!is.null(legend)) legend else c(paste0("Observed ", abbreviate(Group.Level, 8)), "Posterior Estimate")
        legend_type <- c(1, 1)
        pcol_vector <- c(Bar.Color, Line.Color)
        legend(x=Leg.Loc, legend=legend_text, col=pcol_vector, lty=legend_type,
               pt.bg=pcol_vector, cex = cex.legend, bty="n", inset=c(0, .05))
      }
    }
  } #End of function

  ################################################################################
  #             5. Posterior predictive check for ANOVA                          #
  ################################################################################
  #This is a modified function from PlotMCmeanC: Jags-Ymet-Xnom1fac-MrobustHet.r.
  #The original plot produces a generic plot, then it shifts the X and Y axes to
  #a completely different area to produce the graphs. Old code embedded below
  #from what looks like normal distributions.
  fncPlotMcANOVA <- function( codaSamples=NULL, datFrm=NULL , yName=NULL , xName=NULL,
                              MCmean=NULL, MCsigma=NULL, MCnu=NULL, Num.Lines=NULL,
                              Main.Title=NULL, X.Lab=NULL, Line.Color=NULL,
                              CEX.size=NULL, X.Lim=NULL, Y.Lim=NULL, PCol = NULL,
                              Add.Lgd= NULL, Leg.Loc=NULL, T.Percentage=NULL ) {
    mcmcMat <- as.matrix(codaSamples, chains=TRUE)
    chainLength <- NROW( mcmcMat )
    y <- datFrm[, yName]
    x <- as.numeric(as.factor(datFrm[, xName]))
    xlevels <- levels(as.factor(datFrm[, xName]))
    #Make x-limits
    if (is.null(X.Lim)) {
      X.Limits <- c(0.1,length(xlevels) + 0.1)
    } else {
      X.Limits <- X.Lim
    }
    #Make y-limits
    if (is.null(Y.Lim)) {
      Y.Limits <- c(min(y) - 0.2 * (max(y) - min(y)), max(y) + 0.2*(max(y) - min(y)))
    } else {
      Y.Limits <- Y.Lim
    }
    #Get generic mean parameter name to use for graphing
    mean_par <- strsplit(MCmean, "[", fixed=TRUE)[[1]][1]
    #Get generic sigma (SD) parameter name to use for graphing
    sigma_par <- strsplit(MCsigma, "[", fixed=TRUE)[[1]][1]
    # Display data with posterior predictive distributions
    plot(-1,0,
         #       xlim=c(0.1,length(xlevels) + 0.1) ,
         #       ylim=c(min(y) - 0.2 * (max(y) - min(y)), max(y) + 0.2*(max(y) - min(y))) ,
         xlim= X.Limits, xlab=X.Lab , xaxt="n" , ylab= yName ,
         ylim= Y.Limits, main=Main.Title,
         cex.lab=CEX.size, cex=CEX.size, cex.main=CEX.size )
    axis( 1 , at=1:length(xlevels) , tick=FALSE , lab=xlevels )
    for ( xidx in 1:length(xlevels) ) {
      xPlotVal = xidx
      yVals = y[ x == xidx ]
      points( rep(xPlotVal, length(yVals)) + runif(length(yVals), -0.05, 0.05) ,
              yVals , pch=1 , cex=CEX.size , col= PCol ) #COLOR
      chainSub = round(seq(1, chainLength, length= Num.Lines)) #20
      for ( chnIdx in chainSub ) {
        #      m = mcmcMat[chnIdx, paste("m[", xidx, "]", sep="")]
        # m = mcmcMat[chnIdx,paste("b[",xidx,"]",sep="")]
        #      s = mcmcMat[chnIdx, paste("ySigma[", xidx,"]", sep="")]
        m = mcmcMat[chnIdx, paste(mean_par, "[", xidx, "]", sep="")]
        s = mcmcMat[chnIdx, paste(sigma_par, "[", xidx,"]", sep="")]
        nu = mcmcMat[chnIdx, MCnu]
        #This controls tails of t distribution. Coverage "*.01" to get proportion
        tlim= qt( c((0.5 - (T.Percentage*0.01)/2), (0.5 + (T.Percentage*0.01)/2)) , df= nu )
        #This controls tails of t distribution
        yl = m + tlim[1]*s
        yh = m + tlim[2]*s
        ycomb=seq(yl, yh, length=501) ##201
        #ynorm = dnorm(ycomb,mean=m,sd=s)
        #ynorm = 0.67*ynorm/max(ynorm)
        yt = dt( (ycomb - m) / s , df= nu )
        yt = 0.67 * yt / max(yt)           #This controls heighth of curve peaks
        lines( xPlotVal - yt , ycomb , col= Line.Color ) #COLOR
      }
    }
    #Add legend
    if(Add.Lgd =="Yes") {
      legend_text <- c("Observed Value", "Posterior Estimate")
      legend_type <- c(0, 1)
      pch_type <- c(1, -1)
      pcol_vector <- c(PCol, Line.Color)
      legend(Leg.Loc, legend=legend_text, col=pcol_vector,
             lty=legend_type, pt.bg=pcol_vector, cex = 2, pch=pch_type,
             bty="n", inset=c(0, .05))
    }
  }

  ################################################################################
  #             5B. Posterior predictive check for ANOVA, single group           #
  ################################################################################
  fncPlotSingleT <- function( codaSamples=NULL, datFrm=NULL , yName=NULL ,
                              MCmean=NULL, MCsigma=NULL, MCnu=NULL, Num.Lines=NULL,
                              Main.Title=NULL, X.Lab=NULL, Line.Color=NULL,
                              CEX.size=NULL, X.Lim=NULL, Y.Lim=NULL, PCol = NULL,
                              Add.Lgd= NULL, Leg.Loc=NULL, T.Percentage=NULL ) {
    mcmcMat <- as.matrix(codaSamples, chains=TRUE)
    chainLength <- NROW( mcmcMat )
    y <- datFrm[, yName]
    #  x <- as.numeric(as.factor(datFrm[, xName]))
    #  xlevels <- levels(as.factor(datFrm[, xName]))
    #Make x-limits
    if (is.null(X.Lim)) {
      X.Limits <- c(0.6, 1 + 0.1)
    } else {
      X.Limits <- X.Lim
    }
    #Make y-limits
    if (is.null(Y.Lim)) {
      Y.Limits <- c(min(y) - 0.2 * (max(y) - min(y)), max(y) + 0.2*(max(y) - min(y)))
    } else {
      Y.Limits <- Y.Lim
    }
    #Get generic mean parameter name to use for graphing
    # mean_par <- strsplit(MCmean, "[", fixed=TRUE)[[1]][1]
    #Get generic sigma (SD) parameter name to use for graphing
    #  sigma_par <- strsplit(MCsigma, "[", fixed=TRUE)[[1]][1]
    # Display data with posterior predictive distributions
    plot(-1,0,
         xlim= X.Limits, xlab=X.Lab , xaxt="n" , ylab= yName ,
         ylim= Y.Limits, main=Main.Title,
         cex.lab=CEX.size, cex=CEX.size, cex.main=CEX.size )
    #  axis( 1 , at=1:length(xlevels) , tick=FALSE , lab=xlevels )
    for ( xidx in 1:1 ) {
      xPlotVal = xidx
      yVals = y
      points( rep(xPlotVal, length(yVals)) + runif(length(yVals), -0.05, 0.05) ,
              yVals , pch=1 , cex=CEX.size , col= PCol ) #COLOR
      chainSub = round(seq(1, chainLength, length= Num.Lines)) #20
      for ( chnIdx in chainSub ) {
        #      m = mcmcMat[chnIdx, paste("m[", xidx, "]", sep="")]
        # m = mcmcMat[chnIdx,paste("b[",xidx,"]",sep="")]
        #      s = mcmcMat[chnIdx, paste("ySigma[", xidx,"]", sep="")]
        m = mcmcMat[chnIdx, MCmean]
        s = mcmcMat[chnIdx, MCsigma]
        nu = mcmcMat[chnIdx, MCnu]
        #This controls tails of t distribution. Coverage "*.01" to get proportion
        tlim= qt( c((0.5 - (T.Percentage*0.01)/2), (0.5 + (T.Percentage*0.01)/2)) , df= nu )
        #This controls tails of t distribution
        yl = m + tlim[1]*s
        yh = m + tlim[2]*s
        ycomb=seq(yl, yh, length=501) ##201
        #ynorm = dnorm(ycomb,mean=m,sd=s)
        #ynorm = 0.67*ynorm/max(ynorm)
        yt = dt( (ycomb - m) / s , df= nu )
        #     yt = 0.67 * yt / max(yt)           #This controls heighth of curve peaks
        #      lines( xPlotVal - yt , ycomb , col= Line.Color ) #COLOR
        lines( xPlotVal - yt , ycomb , col= Line.Color ) #COLOR
      }
    }
    #Add legend
    if(Add.Lgd =="Yes") {
      legend_text <- c("Observed Value", "Posterior Estimate")
      legend_type <- c(0, 1)
      pch_type <- c(1, -1)
      pcol_vector <- c(PCol, Line.Color)
      legend(Leg.Loc, legend=legend_text, col=pcol_vector,
             lty=legend_type, pt.bg=pcol_vector, cex = 2, pch=pch_type,
             bty="n", inset=c(0, .05))
    }
  }

  ################################################################################
  #           6. Get proportions above/below specific values                     #
  ################################################################################
  #This function calculates the proportion above specific values.
  fncPropGtY <- function( Coda.Object=NULL, Distribution=NULL, yVal=NULL, qVal=NULL,
                          Center=NULL, Spread=NULL, Skew=NULL, CenTend=NULL ) {
    #Convert into a matrix
    MC.Matrix <- as.matrix(Coda.Object, chains=TRUE)

    #Shape and rate parameters using the mode values of omega and kappa
    a_shape <- MC.Matrix[, Center] * (MC.Matrix[, Spread] - 2) + 1
    b_shape <- (1 - MC.Matrix[, Center]) * (MC.Matrix[, Spread] - 2) + 1

    ###################
    ## Mean for Beta ##
    ###################
    #if(!is.null(yVal)) {
    if(Distribution == "Beta") {
      mean_val <- a_shape/MC.Matrix[, Spread]
    }
    #  }

    #  if(!is.null(yVal)) {
    if(Distribution == "Beta") {
      mean_val_dist <- summarizePost(mean_val)[c(c("Mode","Median",
                                                   "Mean")[which(c("Mode","Median","Mean")== CenTend)],"HDIlow","HDIhigh")]
    }
    # }
    else {
      mean_val_dist <- NA
    }

    #Make NA if not from a beta distribution
    #  if (Distribution == "Beta") {
    #    mean_val_dist <- mean_val_dist
    #  } else {
    #    mean_val_dist <- NA
    #  }

    #######################
    ## Beta distribution ##
    #######################
    ## Get summary ##
    # Proportion greater than Y
    PbetaGtY <- list()
    if(!is.null(yVal)) {
      if(Distribution == "Beta") {
        for (i in 1:length(yVal)) {
          PbetaGtY[[i]] <- summarizePost( 1- pbeta(yVal[i], a_shape,
                                                   b_shape) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(PbetaGtY)[i] <- paste0("Y_", yVal[i])
        }
      }
    }
    # Quantiles of Y
    QbetaGtY <- list()
    if(!is.null(qVal)) {
      if(Distribution == "Beta") {
        for (i in 1:length(qVal)) {
          QbetaGtY[[i]] <- summarizePost( qbeta(qVal[i], a_shape,
                                                b_shape) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(QbetaGtY)[i] <- paste0("Percentile_", qVal[i])
        }
      }
    }
    #Effect size for 2 values, in "yVal"
    betaEffSize2Y <- list()
    if(length(yVal) == 2) {
      if(Distribution == "Beta") {
        es1 <- (asin(sign(1- pbeta(yVal[1], a_shape, b_shape) ) * sqrt(abs(1- pbeta(yVal[1], a_shape, b_shape) ))))*2
        es2 <- (asin(sign(1- pbeta(yVal[2], a_shape, b_shape)  ) * sqrt(abs(1- pbeta(yVal[2], a_shape, b_shape) ))))*2
        #Get the posterior summary on effect size between the 2 Y-values
        betaEffSize2Y <- summarizePost(abs(es1 - es2) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
      }
    }
    #Return NAs for NULL objects
    #probability
    if (length(PbetaGtY)==0 ) {
      PbetaGtY <- NA
    } else {
      PbetaGtY <- PbetaGtY
    }
    #quantile
    if (length(QbetaGtY)==0 ) {
      QbetaGtY <- NA
    } else {
      QbetaGtY <- QbetaGtY
    }
    #Effect size
    if (length(betaEffSize2Y)== 0 ) {
      betaEffSize2Y <- NA
    } else {
      betaEffSize2Y <- betaEffSize2Y
    }

    #############################
    ## Log-normal distribution ##
    #############################
    ## Get summary ##
    # Proportion greater than Y
    PlogGtY <- list()
    if(!is.null(yVal)) {
      if(Distribution == "Log-normal") {
        for (i in 1:length(yVal)) {
          PlogGtY[[i]] <- summarizePost( plnorm(q=yVal[i], meanlog= MC.Matrix[, Center],
                                                sdlog= MC.Matrix[, Spread], lower.tail=FALSE) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(PlogGtY)[i] <- paste0("Y_", yVal[i])
        }
      }
    }
    # Quantiles of Y
    QlogGtY <- list()
    if(!is.null(qVal)) {
      if(Distribution == "Log-normal") {
        for (i in 1:length(qVal)) {
          QlogGtY[[i]] <- summarizePost( qlnorm(p=qVal[i], meanlog= MC.Matrix[, Center],
                                                sdlog= MC.Matrix[, Spread]) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(QlogGtY)[i] <- paste0("Percentile_", qVal[i])
        }
      }
    }
    #Return NAs for NULL objects
    #probability
    if (length(PlogGtY)==0 ) {
      PlogGtY <- NA
    } else {
      PlogGtY <- PlogGtY
    }
    #quantile
    if (length(QlogGtY)==0 ) {
      QlogGtY <- NA
    } else {
      QlogGtY <- QlogGtY
    }

    #########################
    ## Normal distribution ##
    #########################
    ## Get summary ##
    # Proportion greater than Y
    PnormGtY <- list()
    if(!is.null(yVal)) {
      if(Distribution == "Normal") {
        for (i in 1:length(yVal)) {
          PnormGtY[[i]] <- summarizePost( pnorm(q=yVal[i], mean= MC.Matrix[, Center],
                                                sd= MC.Matrix[, Spread], lower.tail=FALSE) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(PnormGtY)[i] <- paste0("Y_", yVal[i])
        }
      }
    }
    # Quantiles of Y
    QnormGtY <- list()
    if(!is.null(qVal)) {
      if(Distribution == "Normal") {
        for (i in 1:length(qVal)) {
          QnormGtY[[i]] <- summarizePost( qnorm(p=qVal[i], mean= MC.Matrix[, Center],
                                                sd= MC.Matrix[, Spread]) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(QnormGtY)[i] <- paste0("Percentile_", qVal[i])
        }
      }
    }
    #Return NAs for NULL objects
    #probability
    if (length(PnormGtY)==0 ) {
      PnormGtY <- NA
    } else {
      PnormGtY <- PnormGtY
    }
    #quantile
    if (length(QnormGtY)==0 ) {
      QnormGtY <- NA
    } else {
      QnormGtY <- QnormGtY
    }

    ##############################
    ## Skew-Normal distribution ##
    ##############################
    ## Get summary ##
    # Proportion greater than Y
    PsnormGtY <- list()
    if(!is.null(yVal)) {
      if(Distribution == "Skew-normal") {
        for (i in 1:length(yVal)) {         #I need to subtract 1-psn to get the right prop > 1
          PsnormGtY[[i]] <- summarizePost( 1 - psn(x=yVal[i], xi= MC.Matrix[, Center], omega= MC.Matrix[, Spread],
                                                   alpha= MC.Matrix[, Skew], lower.tail=FALSE) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(PsnormGtY)[i] <- paste0("Y_", yVal[i])
        }
      }
    }
    # Quantiles of Y.
    # Needs mapply for qsn() b/c it creates an impossible error for "omega" <= 0.
    QsnormGtY <- list()
    if(!is.null(qVal)) {
      if(Distribution == "Skew-normal") {
        for (i in 1:length(qVal)) {
          QsnormGtY[[i]] <- summarizePost(mapply(qsn, p=qVal[i], xi=MC.Matrix[, Center], omega=MC.Matrix[, Spread],
                                                 alpha=MC.Matrix[, Skew]))[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(QsnormGtY)[i] <- paste0("Percentile_", qVal[i])
        }
      }
    }

    #Return NAs for NULL objects
    #probability
    if (length(PsnormGtY)==0 ) {
      PsnormGtY <- NA
    } else {
      PsnormGtY <- PsnormGtY
    }
    #quantile
    if (length(QsnormGtY)==0 ) {
      QsnormGtY <- NA
    } else {
      QsnormGtY <- QsnormGtY
    }

    ####################
    ## t distribution ##
    ####################
    ## Get summary ##
    # Proportion greater than Y
    PtGtY <- list()
    if(!is.null(yVal)) {
      if(Distribution == "t") {
        for (i in 1:length(yVal)) {
          PtGtY[[i]] <- summarizePost( pt(q=yVal[i], df= MC.Matrix[, Skew],
                                          ncp= MC.Matrix[, Center], lower.tail=FALSE) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(PtGtY)[i] <- paste0("Y_", yVal[i])
        }
      }
    }
    # Quantiles of Y
    QtGtY <- list()
    if(!is.null(qVal)) {
      if(Distribution == "t") {
        for (i in 1:length(qVal)) {
          QtGtY[[i]] <- summarizePost( qt(p=qVal[i], df= MC.Matrix[, Skew],
                                          ncp= MC.Matrix[, Center]) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(QtGtY)[i] <- paste0("Percentile_", qVal[i])
        }
      }
    }
    #Return NAs for NULL objects
    #probability
    if (length(PtGtY)==0 ) {
      PtGtY <- NA
    } else {
      PtGtY <- PtGtY
    }
    #quantile
    if (length(QtGtY)==0 ) {
      QtGtY <- NA
    } else {
      QtGtY <- QtGtY
    }

    ##########################
    ## Weibull distribution ##
    ##########################
    ## Get summary ##
    # Proportion greater than Y
    PWeibGtY <- list()
    if(!is.null(yVal)) {
      if(Distribution == "Weibull") {
        for (i in 1:length(yVal)) {
          PWeibGtY[[i]] <- summarizePost( pweibull(q=yVal[i], shape= MC.Matrix[, Center],
                                                   scale= MC.Matrix[, Spread], lower.tail=FALSE) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(PWeibGtY)[i] <- paste0("Y_", yVal[i])
        }
      }
    }
    # Quantiles of Y
    QWeibGtY <- list()
    if(!is.null(qVal)) {
      if(Distribution == "Weibull") {
        for (i in 1:length(qVal)) {
          QWeibGtY[[i]] <- summarizePost( qweibull(p=qVal[i], shape= MC.Matrix[, Center],
                                                   scale= MC.Matrix[, Spread]) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(QWeibGtY)[i] <- paste0("Percentile_", qVal[i])
        }
      }
    }
    #Return NAs for NULL objects
    #probability
    if (length(PWeibGtY)==0 ) {
      PWeibGtY <- NA
    } else {
      PWeibGtY <- PWeibGtY
    }
    #quantile
    if (length(QWeibGtY)==0 ) {
      QWeibGtY <- NA
    } else {
      QWeibGtY <- QWeibGtY
    }

    ########################
    ## Gamma distribution ##
    ########################
    ## Get summary ##
    # Proportion greater than Y
    PGammaGtY <- list()
    if(!is.null(yVal)) {
      if(Distribution == "Gamma") {
        for (i in 1:length(yVal)) {
          PGammaGtY[[i]] <- summarizePost( pgamma(q=yVal[i], shape= MC.Matrix[, Center],
                                                  rate= MC.Matrix[, Spread], lower.tail=FALSE) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(PGammaGtY)[i] <- paste0("Y_", yVal[i])
        }
      }
    }
    # Quantiles of Y
    QGammaGtY <- list()
    if(!is.null(qVal)) {
      if(Distribution == "Gamma") {
        for (i in 1:length(qVal)) {
          QGammaGtY[[i]] <- summarizePost( qgamma(p=qVal[i], shape= MC.Matrix[, Center],
                                                  rate= MC.Matrix[, Spread]) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(QGammaGtY)[i] <- paste0("Percentile_", qVal[i])
        }
      }
    }
    #Return NAs for NULL objects
    #probability
    if (length(PGammaGtY)==0 ) {
      PGammaGtY <- NA
    } else {
      PGammaGtY <- PGammaGtY
    }
    #quantile
    if (length(QGammaGtY)==0 ) {
      QGammaGtY <- NA
    } else {
      QGammaGtY <- QGammaGtY
    }

    ######################################
    ## Create final distribution values ##
    ######################################
    ## 1. Probability ##
    if (Distribution == "Beta") {
      PdisGtY <- PbetaGtY
    }
    if (Distribution == "Log-normal") {
      PdisGtY <- PlogGtY
    }
    if (Distribution == "Normal") {
      PdisGtY <- PnormGtY
    }
    if (Distribution == "Skew-normal") {
      PdisGtY <- PsnormGtY
    }
    if (Distribution == "t") {
      PdisGtY <- PtGtY
    }
    if (Distribution == "Weibull") {
      PdisGtY <- PWeibGtY
    }
    if (Distribution == "Gamma") {
      PdisGtY <- PGammaGtY
    }
    #Make NA if the above weren't selected
    if (is.null(PdisGtY)) {
      PdisGtY <- NA
    }
    ## 2. Quantile ##
    if (Distribution == "Beta") {
      QdisGtY <- QbetaGtY
    }
    if (Distribution == "Log-normal") {
      QdisGtY <- QlogGtY
    }
    if (Distribution == "Normal") {
      QdisGtY <- QnormGtY
    }
    if (Distribution == "Skew-normal") {
      QdisGtY <- QsnormGtY
    }
    if (Distribution == "t") {
      QdisGtY <- QtGtY
    }
    if (Distribution == "Weibull") {
      QdisGtY <- QWeibGtY
    }
    if (Distribution == "Gamma") {
      QdisGtY <- QGammaGtY
    }
    #Make NA if the above weren't selected
    if (is.null(QdisGtY)) {
      QdisGtY <- NA
    }
    ## 3. Effect size ##
    if (Distribution == "Beta") {
      disEsY <- betaEffSize2Y
    }
    ##################################################
    ## Gets effect sizes for non-Beta distributions ##
    ##################################################
    nonBetaEffSize2Y <- vector()
    if(length(yVal) == 2) {
      if(Distribution != "Beta") {
        oes1 <- (asin(sign( PdisGtY[[1]][1] ) * sqrt(abs(PdisGtY[[1]][1] ))))*2
        oes2 <- (asin(sign( PdisGtY[[2]][1]) * sqrt(abs( PdisGtY[[2]][1]))))*2
        #Get the posterior summary on effect size between the 2 Y-values
        nonBetaEffSize2Y <- abs(oes1 - oes2)
      }
    }
    #Effect size
    if (length(nonBetaEffSize2Y)== 0 ) {
      nonBetaEffSize2Y <- NA
    } else {
      nonBetaEffSize2Y <- nonBetaEffSize2Y
    }

    #This is temp code for log-normal that makes it NA for now
    if (Distribution != "Beta") {
      disEsY <- nonBetaEffSize2Y
    }

    #Make NA if the above weren't selected
    if (is.null(disEsY)) {
      disEsY <- NA
    }

    return(list("Est.Prop.GT.Y"= PdisGtY,
                "Est.Effect.Size.2Y"= disEsY,
                "Est.Quantile.Y"= QdisGtY,
                "Est.Mean.Beta"=mean_val_dist) )
  }

  ################################################################################
  #                 7. R2 for models with metric only predictors                 #
  ################################################################################
  #This produces R2 for normal or t-distribution models with only metric predictors.
  #The formula is used in the bottom example, just needs the correlation matrix
  #from the actual observed dataset and some matrix algebra of the MCMC object's
  #beta coefficients to get the R2. This works on standardized data but
  #it should work on regular data. And it might work with nominal predictors or
  #maybe factors. The code for an older Kruschke version is below the function.

  fncXmetR2 <- function( codaSamples=NULL , data=NULL ,
                         Z.Beta=NULL, xName=NULL, yName=NULL) {
    y = data[, yName]
    x = as.matrix(data[, xName])
    MCMC_Mat = as.matrix(codaSamples, chains=TRUE)
    #  zbeta  = MCMC_Mat[,grep("^zbeta$|^zbeta\\[", colnames(MCMC_Mat))]
    zbeta  = MCMC_Mat[,grep(paste0("^", Z.Beta,"$|^", Z.Beta, "\\[" ),colnames(MCMC_Mat))]
    if ( ncol(x)==1 ) {
      zbeta = matrix( zbeta , ncol=1 )
    }
    #-----------------------------------------------------------------------------
    # Compute R^2 for credible parameters:
    YcorX = cor( y , x ) # correlation of y with each x predictor
    R2 = zbeta %*% matrix( YcorX , ncol=1 )
    #-----------------------------------------------------------------------------
    return( "R2"=R2)
  }

  ################################################################################
  #           7B. Alternative R2 for models with metric only predictors          #
  ################################################################################
  fncSingleXR2 <- function( Coda.Object=NULL, Intercept=NULL, Betas=NULL,
                            my.data=NULL, xName=NULL, yName=NULL) {
    MCMC_Mat = as.matrix(Coda.Object, chains=TRUE)
    chainLength <- nrow(MCMC_Mat)
    Rsq = rep(0, chainLength)
    for ( stepIdx in 1:chainLength ) {
      predY = my.data[, xName] %*% cbind(MCMC_Mat[, Betas][stepIdx]) +
        MCMC_Mat[, Intercept][stepIdx]
      #    predY = t(my.data[, xName]) %*% as.matrix(cbind(MCMC_Mat[, Betas][stepIdx]) +
      #      MCMC_Mat[, Intercept][stepIdx])
    }
    Rsq = cor(my.data[, yName], predY)^2
    return("R.Square"=Rsq)
  }
  #m1r2 <- fncSingleXR2( Coda.Object=codaN1, Intercept="beta_0", Betas="beta_1",
  #                      my.data=myd2, xName="hrt", yName="slos")
  #summary(m1r2)

  ################################################################################
  #              7c. Gelman R2 for models with metric only predictors            #
  ################################################################################

  #Use the coda object and dataset. Works for normal and log-normal distributions.
  fncBayesOlsR2 <- function(Coda_Object, datFrm, xName=NULL, Intercept=NULL,
                            Betas=NULL, Level1.Sigma=NULL, Average.type =NULL) {
    mcmc_coda_object <- as.matrix(Coda_Object, chains=TRUE)
    mean.Intercept <-  mean(mcmc_coda_object[, which(colnames(mcmc_coda_object) == Intercept)])
    median.Intercept <-  median(mcmc_coda_object[, which(colnames(mcmc_coda_object) == Intercept)])
    #Make list to store fitted values (minus the intercept)
    fitval.mean <- vector(mode="list", length= nrow(datFrm))
    fitval.median <- vector(mode="list", length= nrow(datFrm))
    #when there is only 1 X
    #when there are multiple X
    for (i in 1:nrow(datFrm)) {
      for (j in 1:length(Betas)) {
        if (Average.type=="Mean") {
          fitval.mean[[i]][j] <- mean(mcmc_coda_object[, which(colnames(mcmc_coda_object) %in% Betas[j])]*
                                        datFrm[, which(colnames(datFrm) %in% xName[j])][i])
        } else {
          fitval.median[[i]][j] <- median(mcmc_coda_object[, which(colnames(mcmc_coda_object) %in% Betas[j])]*
                                            datFrm[, which(colnames(datFrm) %in% xName[j])][i])
        }
      }
    }
    #Get the predicted values from the mean
    if (Average.type=="Mean") {
      yPRED <- mapply(fitval.mean, FUN="sum", + mean.Intercept)
    }
    if (Average.type=="Median") {
      yPRED <- mapply(fitval.median, FUN="sum", + median.Intercept)
    }
    #Get variance of the fit
    varFit <- sd(yPRED)^2
    #Get variance of the residuals
    varRes <- mean(mcmc_coda_object[, Level1.Sigma]^2)
    #Calculate R^2
    R2 <- varFit/(varFit + varRes)
    return(list("R2"=R2, "Variance.Pred.Y"=varFit, "Variance.Residuals"=varRes, "yPRED"=yPRED))
  } #End of function


  #fncBayesOlsR2(Coda.Object=codaSamples, mydf=mtcars, xName=c("hp","wt"),
  #              Intercept= "beta0", Betas=c("beta1", "beta2"),
  #              Level1.Sigma="sigma", Average.type="Mean")

  ################################################################################
  #                    8. Bayesian Effect sizes                                  #
  ################################################################################
  #This function calculates the proportion above specific values.
  fncBayesEffectSize <- function( MCMC, Distribution=NULL, yVal1=NULL, yVal2=NULL, yVal3=NULL, CenTend=NULL ) {
    #Convert into a matrix
    MC.Matrix <- as.matrix(MCMC[, -1])
    ###########################
    ## Calculate effect size ##
    ###########################

    ##########
    ## Beta ##
    ##########
    if(Distribution == "beta") {
      as1 <- (asin(sign( rowMeans(MC.Matrix[, yVal1, drop=FALSE]) ) * sqrt(abs( rowMeans(MC.Matrix[, yVal1, drop=FALSE]) ))))*2
      as2 <- (asin(sign( rowMeans(MC.Matrix[, yVal2, drop=FALSE]) ) * sqrt(abs( rowMeans(MC.Matrix[, yVal2, drop=FALSE]) ))))*2
      Effect.Size.Output <- abs(as1 - as2 )
    }
    ##########
    ## t  ##
    ##########
    if(Distribution %in% c("n","normal", "logn", "skewn", "gamma", "weibull", "t")) {
      tnum <- abs(MC.Matrix[, yVal1[1]] - MC.Matrix[, yVal2[1]])
      tdenom <- mean(c(MC.Matrix[, yVal1[2]], MC.Matrix[, yVal2[2]]))
      Effect.Size.Output <- tnum/tdenom
    }
    ################
    # Correlations #
    ################
    #This calculates the q effec size index
    if(Distribution == "correlation") {
      Zr1 <- 1/2 * log((1 + MC.Matrix[, yVal1[1]])/(1 - MC.Matrix[, yVal1[1]]))
      Zr2 <- 1/2 * log((1 + MC.Matrix[, yVal2[1]])/(1 - MC.Matrix[, yVal2[1]]))
      Effect.Size.Output <- abs(Zr1 -Zr2 )
    }
    return("Effect.Size.Posterior"=Effect.Size.Output )
  }

  ################################################################################
  #                9. Posterior Predictive Check for trend lines                 #
  ################################################################################
  #May only need code converting y-axis to logits for logistic regression to work
  fncBayesOlsPrtPred <- function(Coda.Object=NULL , datFrm=NULL,  Reg.Type=NULL,
                                 Outcome=NULL , Group=NULL,
                                 Group.Level=NULL, xName=NULL, parX=NULL, View.Lines=NULL,
                                 Num.Lines=NULL, Main.Title=NULL, X.Lab=NULL,
                                 Line.Color=NULL, CEX.size=NULL, X.Lim=NULL, Y.Lim=NULL,
                                 X.Min=NULL, X.Max=NULL,
                                 PCol=NULL, Add.Lgd=NULL, Leg.Loc=NULL) {
    y = datFrm[, Outcome]
    x = datFrm[, xName, drop=FALSE][1]
    s = factor(datFrm[, Group])
    nSubj = length(unique(s)) # should be same as max(s)
    mcmcMat = as.matrix(Coda.Object, chains=TRUE)
    chainLength = NROW( mcmcMat )
    #-----------------------------------------------------------------------------
    # datFrm with superimposed regression lines and noise distributions:
    #Original par
    # Plot datFrm values:
    xRang = max(x, na.rm=TRUE) - min(x, na.rm=TRUE)
    yRang = max(y, na.rm=TRUE) - min(y, na.rm=TRUE)
    xLimMult = 0.2
    yLimMult = 0.2
    xLim= c( min(x, na.rm=TRUE) - xLimMult*xRang , max(x, na.rm=TRUE) + xLimMult*xRang )
    yLim= c( min(y) - yLimMult*yRang , max(y) + yLimMult*yRang )
    #############################
    ## Make prediction formula ##
    #############################
    #This creates the xComb based on the primary predictor
    #  xComb = seq(xLim[1], xLim[2], length=301)
    xComb = seq(X.Min, X.Max, length=301)
    #parX vector stores the parameter names from the chains
    #xName has X variable names
    #Vector with X variable mean values
    txVarMeans <- colMeans(datFrm[, xName, drop=FALSE], na.rm=TRUE)
    #Get beta coefficient names
    tlCoef <- vector()
    for (i in 1:length(parX)) {
      tlCoef[i] <- paste0("tlis$B", i-1, "[i]")
    }
    #Combines object elements and variable means for polynomial models.
    ttfrm <- cbind(tlCoef[-1], txVarMeans)
    #This will change the mean value to the xComb for linear models
    if (Reg.Type %in% c("OLS: Linear", "OLS: Quadratic", "OLS: Cubic",
                        "Logistic: Linear", "Logistic: Quadratic","Logistic: Cubic") ) {
      ttfrm[1, 2] <- "xComb"
    }
    #This will change the mean value to the xComb^2 for quadratic models
    if (Reg.Type %in% c("OLS: Quadratic", "OLS: Cubic",
                        "Logistic: Quadratic","Logistic: Cubic")) {
      ttfrm[2, 2] <- "xComb^2"
    }
    #This will change the mean value to the xComb^3 for cubic models
    if (Reg.Type %in% c("OLS: Cubic","Logistic: Cubic")) {
      ttfrm[2, 3] <- "xComb^3"
    }
    #This multiplies each coefficient by the x-value and stores it in a vector
    ttnew <- vector()
    for (i in 1:nrow(ttfrm)) {
      ttnew[i] <- paste(ttfrm[i, ], collapse = "*")
    }
    #This adds coefficients*x-value and put in intercept
    ttnew2 <- paste(c(tlCoef[1], ttnew), collapse = "+")
    #Make list that has key parameter names
    tlis <- list()
    for (i in 1:length(parX)) {
      tlis[[i]] <- NA
      names(tlis)[i] <- paste0("B", i-1)
    }
    #This adds coefficients to each of the key parameter names
    trow_ls <- floor(seq(1, nrow(mcmcMat), length= Num.Lines))
    #This creates the values needed for the graphs
    for (i in 1:length(trow_ls)) {
      for (j in 1:length(parX)) {
        tlis[[ j]][i] <- mcmcMat[i, paste0( parX[j])]
      }
    }
    ## Points vs. lines for observed data ##
    if (View.Lines == "All") {
      line_type <-  "p"
    }
    if (View.Lines == "All: Lines") {
      line_type <-  "o"
    }
    if (View.Lines == "Unit") {
      line_type <-  "p"
    }
    if (View.Lines == "Unit: Lines") {
      line_type <-  "o"
    }
    ##############################################################################
    # DID functions
    if (Reg.Type %in% "OLS: DID" ) {
      tab1 <- table(datFrm[, xName[1]], datFrm[, xName[2]])
      xvalPost <- as.numeric(rownames(tab1))
    }
    PostVals <- vector()
    if (Reg.Type %in% "OLS: DID" ) {
      if (nrow(tab1) >  ncol(tab1)) {  #Leaving this in case people put in Time and Intervention backwards
        PostVals <- xvalPost[which(tab1[, 2] > 0 )]
      }
    }
    #Creates zComb for DID intervention binary indicator
    if (Reg.Type %in% "OLS: DID" ) {
      zComb0 <- rep(0, 301)
      zComb1 <- ifelse(xComb < min(PostVals), 0, 1)
    }
    #This makes it for the DID lines (1 for an intervention, 1 for control)
    #Combines object elements and variable means for polynomial models.
    if (Reg.Type %in% "OLS: DID" ) {
      DIDfrm0 <- cbind(tlCoef[-1], txVarMeans)
      DIDfrm1 <- cbind(tlCoef[-1], txVarMeans)
    }
    #This will change the mean value to the xComb for linear models
    if (Reg.Type %in% "OLS: DID" ) {
      DIDfrm0[1, 2] <- "xComb"
      DIDfrm1[1, 2] <- "xComb"
      DIDfrm0[2, 2] <- "zComb0"
      DIDfrm1[2, 2] <- "zComb1"
      DIDfrm0[3, 2] <- "xComb*zComb0"
      DIDfrm1[3, 2] <- "xComb*zComb1"
    }
    #This multiplies each coefficient by the x-value and stores it in a vector
    if (Reg.Type %in% "OLS: DID" ) {
      DIDnew0 <- vector()
      DIDnew1 <- vector()
    }
    #This creates the DID regression weights for the intervention and control groups
    if (Reg.Type %in% "OLS: DID" ) {
      for (i in 1:nrow(DIDfrm0)) {
        DIDnew0[i] <- paste(DIDfrm0[i, ], collapse = "*")
        DIDnew1[i] <- paste(DIDfrm1[i, ], collapse = "*")
      }
    }
    #This adds DID coefficients*x-value and put in intercept
    if (Reg.Type %in% "OLS: DID" ) {
      DIDnew02 <- paste(c(tlCoef[1], DIDnew0), collapse = "+")
      DIDnew12 <- paste(c(tlCoef[1], DIDnew1), collapse = "+")
    }
    #Aggregates data to put in as optional observed trend values
    #Make smaller datasets for the intervention and control groups
    if (Reg.Type %in% "OLS: DID" ) {
      datFrm0 <- datFrm[which( datFrm[, xName[2]] == min(datFrm[, xName[2]], na.rm=T)), c(Outcome, xName[1:2])]
      datFrm1 <- datFrm[which( datFrm[, xName[2]] == max(datFrm[, xName[2]], na.rm=T)), c(Outcome, xName[1:2])]
    }
    #Make DID aggregated data observed points in the graph
    if (Reg.Type %in% "OLS: DID" ) {
      agDID0  <- aggregate(datFrm0[, Outcome] ~ datFrm0[, xName[1]] + datFrm0[, xName[2]], FUN="mean")
      agDID1a <- aggregate(datFrm1[, Outcome] ~ datFrm1[, xName[1]] + datFrm1[, xName[2]], FUN="mean")
    }
    ##############################################################################

    ##################
    ## Create plots ##
    ##################
    plot( unlist(x) , y , pch="" , cex=CEX.size , col="black" ,
          xlim=X.Lim, ylim=Y.Lim,xlab=X.Lab , ylab=Outcome ,
          main= Main.Title, cex.lab=CEX.size, cex.main=CEX.size, cex.axis=CEX.size )
    #All groups added at once for the overall term
    if (Reg.Type %in% c("OLS: Linear", "OLS: Quadratic", "OLS: Cubic") ) {
      if (View.Lines %in% c("All", "All: Lines")) {
        for ( sIdx in 1:nSubj ) {
          thisSrows = (as.numeric(s)==sIdx)
          lines( x[thisSrows, ] , y[thisSrows] , type=line_type , pch=19, col= PCol, cex=CEX.size*0.75)
        }
      }
    }
    #Logistic regression probabilities
    if (Reg.Type %in% c("Logistic: Linear", "Logistic: Quadratic","Logistic: Cubic") ) {
      if (View.Lines %in% c("All", "All: Lines")) {
        for ( sIdx in 1:nSubj ) {
          thisSrows = (as.numeric(s)==sIdx)
          lines( x[thisSrows, ] , y[thisSrows] , type=line_type , pch=19, col= PCol, cex=CEX.size*0.75)
        }
      }
    }
    # Superimpose a smattering of believable regression lines:
    #For each line (e.g., 30), it cycles the 301 X-comb values to create 301 Ys for plot
    #This plots out random regression lines
    if (Reg.Type %in% c("OLS: Linear", "OLS: Quadratic", "OLS: Cubic") ) {
      for ( i in 1:length(floor(seq(1, nrow(mcmcMat), length = Num.Lines))) ) {
        lines( xComb , eval(parse(text= ttnew2)) , col= Line.Color )
      }
    }
    #Logistic regression probabilities
    if (Reg.Type %in% c("Logistic: Linear", "Logistic: Quadratic","Logistic: Cubic") ) {
      for ( i in 1:length(floor(seq(1, nrow(mcmcMat), length = Num.Lines))) ) {
        lines( xComb , (1/(1+ exp(-( (eval(parse(text= ttnew2))) )))) , col= Line.Color )
      }
    }
    #DID posterior lines
    if (Reg.Type %in% "OLS: DID" ) {
      for ( i in 1:length(floor(seq(1, nrow(mcmcMat), length = Num.Lines))) ) {
        lines( xComb , eval(parse(text= DIDnew12)) , col= Line.Color[2] )
        lines( xComb , eval(parse(text= DIDnew02)) , col= Line.Color[1] )
        #SAVE: these 2 lines make are transparent, they' wi'll blend colors for groups, pre-intervention
        #      lines( xComb , eval(parse(text= DIDnew12)) , col= adjustcolor( Line.Color[2], alpha.f = 0.4) )
        #      lines( xComb , eval(parse(text= DIDnew02)) , col= adjustcolor( Line.Color[1], alpha.f = 0.4) )
      }
    }

    #Determine which observed datFrm lines to view
    if (View.Lines %in% c("Unit", "Unit: Lines")) {
      #For specific groups
      for ( sIdx in 1:nSubj ) {
        thisSrows <- (as.numeric(s) == as.numeric(s[s == Group.Level]))
        lines( x[thisSrows, ] , y[thisSrows] , type= line_type, pch=19, col= PCol, cex=CEX.size )
      }
    }

    #DID observed data
    if (View.Lines == "DID: Groups") {
      #For specific groups
      for ( sIdx in 1:nSubj ) {
        thisSrows <- (as.numeric(s) == as.numeric(s[s == Group.Level]))
        lines(agDID1a[,1], agDID1a[,3], type="o" , pch=19, col= tail(PCol, 1), cex=CEX.size)
        lines(agDID0[, 1], agDID0[, 3], type="o" , pch=19, col= head(PCol, 1), cex=CEX.size)
      }
    }

    #Legend color
    if (View.Lines== "None") {
      pcol_vector <- c(Line.Color)
    }
    if (View.Lines== "Unit") {
      pcol_vector <- c(PCol, Line.Color)
    }
    if (View.Lines== "All") {
      pcol_vector <- c(PCol, Line.Color)
    }
    if (View.Lines == "DID: Groups") {
      pcol_vector <- c(PCol, Line.Color)
    }
    #Legend text
    if (Reg.Type %in% c("OLS: Linear", "OLS: Quadratic", "OLS: Cubic",
                        "Logistic: Linear", "Logistic: Quadratic","Logistic: Cubic") ) {
      if (View.Lines== "None") {
        if (nchar(Group.Level) > 0 ){
          legend_text <- c(paste0("Posterior Estimate ", abbreviate(Group, 8), ": ",
                                  abbreviate(Group.Level, 8)))
        } else {
          legend_text <- c(paste0("Posterior Estimate: ", abbreviate(Group, 8)))
        }
      }
    }
    if (View.Lines== "Unit") {
      legend_text <- c(paste0("Observed ", abbreviate(Group, 8), ": ",
                              abbreviate(Group.Level, 8)), "Posterior Estimate")
    }
    if (View.Lines== "All") {
      legend_text <- c(paste0("Observed ", abbreviate(Group, 8),": All"), "Posterior Estimate")
    }
    #DID
    if (Reg.Type %in% "OLS: DID" ) {
      if (View.Lines== "DID: Groups") {
        legend_text <- c("Observed: Control", "Observed: Intervention", "Posterior Estimate: Control", "Posterior Estimate: Intervention")
      } else {
        legend_text <- c("Posterior Estimate: Control", "Posterior Estimate: Intervention")
      }
    }

    #Legend points
    if (View.Lines== "None") {
      legend_points <- NULL
    }
    if (View.Lines== "Unit") {
      legend_points <- c(19, NA)
    }
    if (View.Lines== "All") {
      legend_points <- c(19, NA)
    }
    if (View.Lines== "DID: Groups") {
      legend_points <- c(19,19, NA, NA)
    }

    #Add legend
    if(Add.Lgd == "Yes") {
      legend_type <- c(1)
      legend(Leg.Loc, legend=legend_text, col=pcol_vector, lty=legend_type,
             pch=legend_points, pt.bg=pcol_vector, cex = 2, lwd=3,  bty="n", inset=c(0, .05))
    }
    #  return(list(ttnew2=ttnew2 ))
    #-----------------------------------------------------------------------------
  }

  ################################################################################

  ################################################################################
  #             10. Function to sort parameter names into a list                 #
  ################################################################################
  fncBayesMultiOlsPPpar <- function(Coda.Object=NULL , datFrm=NULL,  Reg.Type=NULL,
                                    Outcome=NULL , Group=NULL,
                                    xName=NULL, parX=NULL) {
    #Make list of parameter names, requires that they are entered by parameter type
    num_parX <- length(parX) #number of parameters
    #Create lists to store parameters
    if(Reg.Type %in% c("Hierarchical OLS: Linear","Hierarchical Log OLS: Linear",
                       "Hierarchical Logistic: Linear")) {
      parX_ls <- vector(mode="list", length= num_parX/2)
      num_parX_types <- 2 #The number of parameters for linear models
    }
    #Use this for a quadratic model
    if(Reg.Type %in% c("Hierarchical OLS: Quadratic", "Hierarchical Log OLS: Quadratic",
                       "Hierarchical Logistic: Quadratic")) {
      parX_ls <- vector(mode="list", length= num_parX/3)
      num_parX_types <- 3 #The number of parameters for quadratic models
    }
    #Use this for a cubic model
    if(Reg.Type %in% c("Hierarchical OLS: Cubic", "Hierarchical Log OLS: Cubic",
                       "Hierarchical Logistic: Cubic")) {
      parX_ls <- vector(mode="list", length= num_parX/4)
      num_parX_types <- 4 #The number of parameters for cubic models
    }
    #Put parameters into the list in the appropriate order
    group_sorted_parX <- list()
    if(Reg.Type %in% c("Hierarchical OLS: Linear","Hierarchical Log OLS: Linear",
                       "Hierarchical Logistic: Linear")) {
      for (i in 1:(num_parX/num_parX_types)) {
        group_sorted_parX[[i]] <- parX[seq(i, i + (num_parX/num_parX_types), by= num_parX/num_parX_types)]
      }
    }
    #Quadratic models
    if(Reg.Type %in% c("Hierarchical OLS: Quadratic", "Hierarchical Log OLS: Quadratic",
                       "Hierarchical Logistic: Quadratic")) {
      for (i in 1:(num_parX/num_parX_types)) {
        group_sorted_parX[[i]] <- parX[seq(i, i + (num_parX/num_parX_types)*2, by= num_parX/num_parX_types)]
      }
    }
    #Cubic models
    if(Reg.Type %in% c("Hierarchical OLS: Cubic", "Hierarchical Log OLS: Cubic",
                       "Hierarchical Logistic: Cubic")) {
      for (i in 1:(num_parX/num_parX_types)) {
        group_sorted_parX[[i]] <- parX[seq(i, i + (num_parX/num_parX_types)*3, by= num_parX/num_parX_types)]
      }
    }
    return(group_sorted_parX)
  }

  ################################################################################
  #                11. Function to put coefficients into a formula               #
  ################################################################################
  fncBayesOlsPPcoef <- function( parX=NULL, Coda.Object=NULL , datFrm=NULL,
                                 Reg.Type=NULL, Outcome=NULL, Group=NULL,
                                 xName=NULL, mc_row_number=NULL) {
    #Makes the mcmc object
    mcmcMat = as.matrix(Coda.Object, chains=TRUE)
    #Row number if still == NULL
    if (is.null(  mc_row_number)) {
      mc_row_number <- 1
    }
    #parX vector stores the parameter names from the chains
    #xName has X variable names
    #Vector with X variable mean values
    txVarMeans <- colMeans(datFrm[, xName, drop=FALSE], na.rm=TRUE)
    #Get beta coefficient names
    tlCoef <- vector()
    for (i in 1:length(parX)) {
      tlCoef[i] <- paste0("tlis$B", i-1, "[i]")
    }
    #Combines object elements and variable means for polynomial models.
    ttfrm <- cbind(tlCoef[-1], txVarMeans)
    #This will change the mean value to the xComb for linear models
    if (Reg.Type %in% c("Hierarchical OLS: Linear", "Hierarchical OLS: Quadratic", "Hierarchical OLS: Cubic",
                        "Hierarchical Log OLS: Linear", "Hierarchical Log OLS: Quadratic",
                        "Hierarchical Log OLS: Cubic","Hierarchical Logistic: Linear",
                        "Hierarchical Logistic: Quadratic", "Hierarchical Logistic: Cubic") ) {
      ttfrm[1, 2] <- "xComb"
    }
    #This will change the mean value to the xComb^2 for quadratic models
    if (Reg.Type %in% c("Hierarchical OLS: Quadratic", "Hierarchical OLS: Cubic",
                        "Hierarchical Log OLS: Quadratic", "Hierarchical Log OLS: Cubic",
                        "Hierarchical Logistic: Quadratic", "Hierarchical Logistic: Cubic")) {
      ttfrm[2, 2] <- "xComb^2"
    }
    #This will change the mean value to the xComb^3 for cubic models
    if (Reg.Type %in% c("Hierarchical OLS: Cubic", "Hierarchical Log OLS: Cubic",
                        "Hierarchical Logistic: Cubic")) {
      ttfrm[2, 3] <- "xComb^3"
    }
    #This multiplies each coefficient by the x-value and stores it in a vector
    ttnew <- vector()
    for (i in 1:nrow(ttfrm)) {
      ttnew[i] <- paste(ttfrm[i, ], collapse = "*")
    }
    #This adds coefficients*x-value and put in intercept
    ttnew2 <- paste(c(tlCoef[1], ttnew), collapse = "+")
    #Make list that has key parameter names
    tlis <- list()
    for (i in 1:length(parX)) {
      tlis[[i]] <- NA
      names(tlis)[i] <- paste0("B", i-1)
    }
    #This adds coefficients to each of the key parameter names
    #This creates the values needed for the graphs
    for (j in 1:length(parX)) {
      tlis[[ j]] <- mcmcMat[mc_row_number, paste0( parX[j])]
    }
    return(list( "ttnew2"=ttnew2, "tlis"=tlis))
  }  #end of subfunction


  ################################################################################
  #       12. Function to graph multiple hierarchical regression lines           #
  ################################################################################
  fncBayesMultiOlsPrtPred <- function(Coda.Object=NULL , datFrm=NULL,  Reg.Type=NULL,
                                      Outcome=NULL , Group=NULL, xName=NULL, parX=NULL,
                                      View.Lines=NULL, Main.Title=NULL, X.Lab=NULL,
                                      Line.Color=NULL, CEX.size=NULL, X.Lim=NULL, Y.Lim=NULL,
                                      X.Min=NULL, X.Max=NULL, PCol=NULL, Add.Lgd=NULL,
                                      Leg.Loc=NULL, mc_row_number=NULL) {
    #-----------------------------------------------------------------------------
    y = datFrm[, Outcome]
    x = datFrm[, xName, drop=FALSE][1]
    s = factor(datFrm[, Group])
    nSubj = length(unique(s)) # should be same as max(s)
    mcmcMat = as.matrix(Coda.Object, chains=TRUE)
    chainLength = NROW( mcmcMat )
    # Plot settings
    #Original par
    xRang = max(x, na.rm=TRUE) - min(x, na.rm=TRUE)
    yRang = max(y, na.rm=TRUE) - min(y, na.rm=TRUE)
    xLimMult = 0.2
    yLimMult = 0.2
    xLim= c( min(x, na.rm=TRUE) - xLimMult*xRang , max(x, na.rm=TRUE) + xLimMult*xRang )
    yLim= c( min(y) - yLimMult*yRang , max(y) + yLimMult*yRang )
    #The min and max are alternatives to x limits
    #This creates the xComb based on the primary predictor
    #  xComb = seq(xLim[1], xLim[2], length=301)
    xComb = seq(X.Min, X.Max, length=301)
    ## Make parameters ##
    multi_pars <- fncBayesMultiOlsPPpar(Coda.Object=Coda.Object , datFrm=datFrm,
                                        Reg.Type=Reg.Type, Outcome=Outcome , Group=Group,
                                        xName=xName, parX=parX)
    #Get the list of parameters for ttnew2 and tlis
    multi_coefs <- lapply(multi_pars, fncBayesOlsPPcoef, Coda.Object=Coda.Object, datFrm=datFrm,
                          Reg.Type=Reg.Type, Outcome=Outcome, Group=Group, xName=xName,
                          mc_row_number=mc_row_number)
    #Seperate the list elements
    tl11 <- lapply(multi_coefs, `[[`, 1) #linear formula
    tl22 <- lapply(multi_coefs, `[[`, 2) #regression coefficients

    #Convert into a data frame for graphing
    tlis <- do.call(rbind.data.frame, tl22)
    #Convert into linear formula for graphing
    ttnew2 <- as.character(tl11)
    #Make line colors, 1st spot is for the overall mean if needed
    multi_line_color <- c()
    if(length(Line.Color) ==1 ) {
      multi_line_color <- rep(Line.Color, nSubj)
    } else {
      multi_line_color <- c( rep(Line.Color[1], length(ttnew2)-1), Line.Color[2])
    }
    #Multiple line width
    multi_line_width <- c()
    if(length(Line.Color) ==1 ) {
      multi_line_width <- rep(4, nSubj)
    } else {
      multi_line_width <- c( rep(4, length(ttnew2)-1), 7)
    }

    ##################
    ## Create plots ##
    ##################
    plot( unlist(x) , y , pch="" , cex=CEX.size , col="black" ,
          xlim=X.Lim, ylim=Y.Lim,xlab=X.Lab , ylab=Outcome ,
          main= Main.Title, cex.lab=CEX.size, cex.main=CEX.size, cex.axis=CEX.size )
    #All groups added at once for the overall term
    if (Reg.Type %in% c("Hierarchical OLS: Linear", "Hierarchical OLS: Quadratic", "Hierarchical OLS: Cubic",
                        "Hierarchical Log OLS: Linear", "Hierarchical Log OLS: Quadratic", "Hierarchical Log OLS: Cubic",
                        "Hierarchical Logistic: Linear", "Hierarchical Logistic: Quadratic", "Hierarchical Logistic: Cubic") ) {
      if (View.Lines %in% c("All", "All: Lines")) {
        for ( sIdx in 1:nSubj ) {
          thisSrows = (as.numeric(s)==sIdx)
          lines( x[thisSrows, ] , y[thisSrows] , type="p" , pch=19, col= PCol, cex=CEX.size*0.75)
        }
      }
    }
    # Superimpose a smattering of believable regression lines:
    #For each line (e.g., 30), it cycles the 301 X-comb values to create 301 Ys for plot
    #This plots out random regression lines
    if (Reg.Type %in% c("Hierarchical OLS: Linear", "Hierarchical OLS: Quadratic", "Hierarchical OLS: Cubic")) {
      for ( i in 1:length(ttnew2) ) {
        lines( xComb , eval(parse(text= ttnew2[i])) , col= multi_line_color[i], lwd= multi_line_width[i] )
      }
    }
    #Log-normal models
    if (Reg.Type %in% c("Hierarchical Log OLS: Linear", "Hierarchical Log OLS: Quadratic", "Hierarchical Log OLS: Cubic")) {
      for ( i in 1:length(ttnew2) ) {
        lines( xComb , exp(eval(parse(text= ttnew2[i]))) , col= multi_line_color[i], lwd= multi_line_width[i] )
      }
    }
    #Probabilities from logistic regression models
    if (Reg.Type %in% c("Hierarchical Logistic: Linear", "Hierarchical Logistic: Quadratic", "Hierarchical Logistic: Cubic")) {
      for ( i in 1:length(ttnew2) ) {
        lines( xComb, 1 /(1+ exp(-( (eval(parse(text= ttnew2[i]))) ))), col= multi_line_color[i], lwd= multi_line_width[i] )
      }
    }
    #Legend color
    if (View.Lines %in% c("All", "All: Lines")) {
      pcol_vector <- c(PCol, Line.Color)
    } else {
      pcol_vector <- c(Line.Color)
    }

    #Legend text
    if (View.Lines %in% c("All", "All: Lines")) {
      if (length(Line.Color) == 1) {
        legend_text <- c(paste0("Observed ", abbreviate(Group, 8),": All"), "Posterior Estimate")
      } else {
        legend_text <- c(paste0("Observed ", abbreviate(Group, 8),": All"), "Group Posterior Estimate", "Overall Posterior Estimate")
      }
    } else {
      if (length(Line.Color) == 1) {
        legend_text <- c( "Posterior Estimate")
      } else {
        legend_text <- c( "Group Posterior Estimate", "Overall Posterior Estimate")
      }
    }

    #Legend points
    if (View.Lines %in% c("All", "All: Lines")) {
      legend_points <- c(19, NA, NA)
    } else {
      legend_points <- c(NA, NA, NA)
    }

    #Legend type
    if (View.Lines %in% c("All", "All: Lines")) {
      legend_type <- c(NA,1,1)
    } else {
      legend_type <- c(1,1)
    }

    #Add legend
    if(Add.Lgd == "Yes") {
      legend(Leg.Loc, legend=legend_text, col=pcol_vector, lty=legend_type,
             pch=legend_points, pt.bg=pcol_vector, cex = 2, lwd=3,  bty="n", inset=c(0, .05))
    }
    #-----------------------------------------------------------------------------
  }

  ##########################################
  ## DBDA Functions for Bayesian analysis ##
  ##########################################

  ## Need summarizePost and HDIofMCMC
  ########################
  ## DBDA function code ##
  ########################
  summarizePost <- function( paramSampleVec ,
                             compVal=NULL , ROPE=NULL , credMass=0.95 ) {
    meanParam = mean( paramSampleVec )
    medianParam = median( paramSampleVec )
    dres = density( paramSampleVec )
    modeParam = dres$x[which.max(dres$y)]
    mcmcEffSz = round( fncESS( paramSampleVec ) , 1 )
    names(mcmcEffSz) = NULL
    hdiLim = HDIofMCMC( paramSampleVec , credMass=credMass )
    if ( !is.null(compVal) ) {
      pcgtCompVal = ( 100 * sum( paramSampleVec > compVal )
                      / length( paramSampleVec ) )
    } else {
      compVal=NA
      pcgtCompVal=NA
    }
    if ( !is.null(ROPE) ) {
      pcltRope = ( 100 * sum( paramSampleVec < ROPE[1] )
                   / length( paramSampleVec ) )
      pcgtRope = ( 100 * sum( paramSampleVec > ROPE[2] )
                   / length( paramSampleVec ) )
      pcinRope = 100-(pcltRope+pcgtRope)
    } else {
      ROPE = c(NA,NA)
      pcltRope=NA
      pcgtRope=NA
      pcinRope=NA
    }
    return( c( Mean=meanParam , Median=medianParam , Mode=modeParam ,
               ESS=mcmcEffSz ,
               HDImass=credMass , HDIlow=hdiLim[1] , HDIhigh=hdiLim[2] ,
               CompVal=compVal , PcntGtCompVal=pcgtCompVal ,
               ROPElow=ROPE[1] , ROPEhigh=ROPE[2] ,
               PcntLtROPE=pcltRope , PcntInROPE=pcinRope , PcntGtROPE=pcgtRope ) )
  }

  ########################
  ## DBDA function code ##
  ########################
  HDIofMCMC <- function( sampleVec , credMass=0.95 ) {
    # Computes highest density interval from a sample of representative values,
    #   estimated as shortest credible interval.
    # Arguments:
    #   sampleVec
    #     is a vector of representative values from a probability distribution.
    #   credMass
    #     is a scalar between 0 and 1, indicating the mass within the credible
    #     interval that is to be estimated.
    # Value:
    #   HDIlim is a vector containing the limits of the HDI
    sortedPts = sort( sampleVec )
    ciIdxInc = ceiling( credMass * length( sortedPts ) )
    nCIs = length( sortedPts ) - ciIdxInc
    ciWidth = rep( 0 , nCIs )
    for ( i in 1:nCIs ) {
      ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
    }
    HDImin = sortedPts[ which.min( ciWidth ) ]
    HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
    HDIlim = c( HDImin , HDImax )
    return( HDIlim )
  }

  ################################################################################
  #                          Functions for Chain Diagnostics                     #
  ################################################################################

  #######################
  ## Run Gelman-Rubin  ##
  #######################
  gelman.rubin <- function(MCMC, parameter, n_rows, n_chains, n_rowchn, samp) {
    #Make a chain list with 1 element per chain
    chain_ls <- vector(mode="list", length=n_chains)
    for(i in 1:n_chains) {
      chain_ls[[i]] <- MCMC[MCMC$CHAIN== i, parameter]
    }
    #Make chains object
    chains <- data.frame(do.call(cbind, chain_ls))
    chains <- chains[1:samp, ]
    m <- n_chains
    n <-  n_rowchn

    if (m < 2) {
      stop("Gelman-Rubin statistic requires at least 2 chains.")
    }
    if (length(unique(sapply(chains, length))) > 1) {
      stop("All chains must have the same number of iterations.")
    }
    # 1. Calculate the mean of each chain (theta_j_bar)
    chain_means <- sapply(chains, mean)
    # 2. Calculate the grand mean across all chains (theta_double_bar)
    grand_mean <- mean(chain_means)
    # 3. Calculate the between-chain variance (B)
    B <- n / (m - 1) * sum((chain_means - grand_mean)^2)
    # 4. Calculate the variance within each chain (s_j^2)
    # The snippet below does this in one go for all chains
    within_chain_vars <- sapply(chains, var) # In R, var() uses (n-1) in the denominator
    # 5. Calculate the average of the within-chain variances (W)
    W <- mean(within_chain_vars)
    # 6. Calculate the estimated posterior variance (var_plus)
    var_plus <- (n - 1) / n * W + B / n
    # 7. Calculate the Potential Scale Reduction Factor (PSRF), R-hat
    R_hat <- sqrt(var_plus / W)
    return(R_hat)
  }
  #############
  # Traceplot #
  #############
  traceplot <- function(MCMC, parameter, n_rowchn) {
      #plot works much faster than lines, doesn't matter if it's a for loop
      plot(1:(n_rowchn), MCMC[1:(n_rowchn), parameter], type="n",
           main="Traceplot", ylab="Param. Value", xlab="Chain iterations")
      for(i in 1:max(MCMC$CHAIN)) {
        lines(MCMC[MCMC$CHAIN == i, parameter], lty=i, col=DBDAplColors[i])
      }
    }
    ########################
    # Autocorrelation plot #
    ########################
  DbdaAcfPlot <- function( MCMC , parName, nChain,
                           plColors=NULL ) {
    #Make a chain list with 1 element per chain
    chain_ls <- vector(mode="list", length=nChain)
    for(i in 1:nChain) {
      chain_ls[[i]] <- MCMC[MCMC$CHAIN== i, parName]
    }
    #Make codaObject out of chain list
    codaObject <- chain_ls

    if ( is.null(plColors) ) plColors=1:nChain
    xMat = NULL
    yMat = NULL
    for ( cIdx in 1:nChain ) {
      acfInfo = acf(codaObject[[cIdx]],plot=FALSE)
      xMat = cbind(xMat,acfInfo$lag)
      yMat = cbind(yMat,acfInfo$acf)
    }
    matplot( xMat , yMat , type="o" , pch=20 , col=plColors , ylim=c(0,1) ,
             main="Autocorrelation factor" , xlab="Lag" , ylab="Autocorrelation" )
    abline(h=0,lty="dashed")
    EffChnLngth = fncESS(MCMC[,c(parName)])
    text( x=max(xMat) , y=max(yMat) , adj=c(1.0,1.0) , cex=1.5 ,
          labels=paste("Eff.Samp.Size =",round(EffChnLngth, 1)) )
  }
  ################
  # Density plot #
  ################
  DbdaDensPlot <- function( MCMC , parName, plColors, nChain) {
    #Make a chain list with 1 element per chain
    chain_ls <- vector(mode="list", length=nChain)
    for(i in 1:nChain) {
      chain_ls[[i]] <- MCMC[MCMC$CHAIN== i, parName]
    }
    #Make codaObject out of chain list
    codaObject <- chain_ls

    if ( is.null(plColors) ) plColors=1:nChain
    xMat = NULL
    yMat = NULL
    hdiLims = NULL
    for ( cIdx in 1:nChain ) {
      densInfo = density(codaObject[[cIdx]])
      xMat = cbind(xMat,densInfo$x)
      yMat = cbind(yMat,densInfo$y)
      hdiLims = cbind(hdiLims,HDIofMCMC(codaObject[[cIdx]]))
    }
    matplot( xMat , yMat , type="l" , col=plColors ,
             main="Density plots for convergence" , xlab="Param. Value" , ylab="Density" )
    abline(h=0)
    points( hdiLims[1,] , rep(0,nChain) , col=plColors , pch="|" )
    points( hdiLims[2,] , rep(0,nChain) , col=plColors , pch="|" )
    text( mean(hdiLims) , 0 , "95% HDI" , adj=c(0.5,-0.2) )
    EffChnLngth = fncESS(MCMC[,c(parName)])
    MCSE = sd(MCMC[, parName])/sqrt(EffChnLngth)
    text( max(xMat) , max(yMat) , adj=c(1.0,1.0) , cex=1.5 ,
          paste("MCSE =\n",signif(MCSE,3)) )
  }

  #############################
  ## Posterior distributions ##
  #############################
  plotPost <- function( paramSampleVec , cenTend=c("mode","median","mean")[1] ,
                        compVal=NULL, ROPE=NULL, credMass=0.95, HDItextPlace=0.7,
                        xlab=NULL , xlim=NULL , yaxt=NULL , ylab=NULL ,
                        main=NULL , cex=NULL , cex.lab=NULL ,
                        bcol=NULL , lcol=NULL , border=NULL, showCurve=FALSE,
                        breaks=NULL, math=math, es=es,
                        ... ) {
    # Override defaults of hist function, if not specified by user:
    # (additional arguments "..." are passed to the hist function)
    if(is.null(xlab)) {
      if(math != "n") {
        xlab <- math
      } else {
        xlab <- parameter
      }
    }
    if ( is.null(cex.lab) ) cex.lab=1.5
    if ( is.null(cex) ) cex=1.4
    if ( is.null(xlim) ) xlim=range( c( compVal , ROPE , paramSampleVec ) )
    if ( is.null(main) ) main=""
    if ( is.null(yaxt) ) yaxt="n"
    if ( is.null(ylab) ) ylab=""
    if ( is.null(bcol) ) bcol="gray"
    if ( is.null(border) ) border= bcol

    # convert coda object to matrix:
    if (any(class(paramSampleVec) == "mcmc.list") == TRUE) {
      paramSampleVec = as.matrix(paramSampleVec)
    }

    summaryColNames = c("ESS","mean","median","mode",
                        "hdiMass","hdiLow","hdiHigh",
                        "compVal","pGtCompVal",
                        "ROPElow","ROPEhigh","pLtROPE","pInROPE","pGtROPE")
    postSummary = matrix( NA , nrow=1 , ncol=length(summaryColNames) ,
                          dimnames=list( c( xlab ) , summaryColNames ) )

    # for fncESS function
    postSummary[,"ESS"] = fncESS(paramSampleVec)

    postSummary[,"mean"] = mean(paramSampleVec)
    postSummary[,"median"] = median(paramSampleVec)
    mcmcDensity = density(paramSampleVec)
    postSummary[,"mode"] = mcmcDensity$x[which.max(mcmcDensity$y)]

    HDI = HDIofMCMC( paramSampleVec , credMass )
    postSummary[,"hdiMass"]=credMass
    postSummary[,"hdiLow"]=HDI[1]
    postSummary[,"hdiHigh"]=HDI[2]

    # Plot histogram.
    cvCol = lcol[1]
    ropeCol = rep(tail(lcol, 1), 2)
    if ( is.null(breaks) ) {
      if ( max(paramSampleVec) > min(paramSampleVec) ) {
        breaks = c( seq( from=min(paramSampleVec) , to=max(paramSampleVec) ,
                         by=(HDI[2]-HDI[1])/18 ) , max(paramSampleVec) )
      } else {
        breaks=c(min(paramSampleVec)-1.0E-6,max(paramSampleVec)+1.0E-6)
        border="white"
      }
    }
    if ( !showCurve ) {
      histinfo = hist( paramSampleVec , xlab=xlab , yaxt=yaxt , ylab=ylab ,
                       freq=F , border="white" , col=bcol ,
                       xlim=xlim , main=main , cex=cex , cex.lab=cex.lab ,
                       breaks=breaks , ... )
    }
    if ( showCurve ) {
      histinfo = hist( paramSampleVec , plot=F )
      densCurve = density( paramSampleVec , adjust=2 )
      plot( densCurve$x , densCurve$y , type="l" , lwd=5 , col=bcol , bty="n" ,
            xlim=xlim , xlab=xlab , yaxt=yaxt , ylab=ylab ,
            main=main , cex=cex , cex.lab=cex.lab , ... )
    }
    cenTendHt = 0.9*max(histinfo$density)
    cvHt = 0.7*max(histinfo$density)
    ROPEtextHt = 0.55*max(histinfo$density)
    # Display central tendency:
    mn = mean(paramSampleVec)
    med = median(paramSampleVec)
    mcmcDensity = density(paramSampleVec)
    mo = mcmcDensity$x[which.max(mcmcDensity$y)]
    if ( cenTend=="mode" ){
      text( mo , cenTendHt ,
            bquote(mode==.(signif(mo,3))) , adj=c(.5,0) , cex=cex )
    }
    if ( cenTend=="median" ){
      text( med , cenTendHt ,
            bquote(median==.(signif(med,3))) , adj=c(.5,0) , cex=cex , col=cvCol )
    }
    if ( cenTend=="mean" ){
      text( mn , cenTendHt ,
            bquote(mean==.(signif(mn,3))) , adj=c(.5,0) , cex=cex )
    }
    # Display the comparison value.
    if ( !is.null( compVal ) ) {
      pGtCompVal = sum( paramSampleVec > compVal ) / length( paramSampleVec )
      pLtCompVal = 1 - pGtCompVal
      lines( c(compVal,compVal) , c(0.96*cvHt,0) ,
             lty="dotted" , lwd=2 , col=cvCol )
      text( compVal , cvHt ,
            bquote( .(round(100*pLtCompVal, 1)) * "% < " *
                      .(signif(compVal,3)) * " < " *
                      .(round(100*pGtCompVal, 1)) * "%" ) ,
            adj=c(pLtCompVal,0) , cex=0.8*cex , col=cvCol )
      postSummary[,"compVal"] = compVal
      postSummary[,"pGtCompVal"] = pGtCompVal
    }
    # Display the ROPE.
    if ( !is.null( ROPE ) ) {
      pInROPE = ( sum( paramSampleVec > ROPE[1] & paramSampleVec < ROPE[2] )
                  / length( paramSampleVec ) )
      pGtROPE = ( sum( paramSampleVec >= ROPE[2] ) / length( paramSampleVec ) )
      pLtROPE = ( sum( paramSampleVec <= ROPE[1] ) / length( paramSampleVec ) )
      lines( c(ROPE[1],ROPE[1]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
             col=ropeCol[1] )
      lines( c(ROPE[2], ROPE[2]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
             col= ropeCol[2])
      text( mean(ROPE) , ROPEtextHt ,
            bquote( .(round(100*pLtROPE, 1)) * "% < " * .(ROPE[1]) * " < " *
                      .(round(100*pInROPE, 1)) * "% < " * .(ROPE[2]) * " < " *
                      .(round(100*pGtROPE, 1)) * "%" ) ,
            adj=c(pLtROPE+.5*pInROPE,0) , cex=1 , col=ropeCol )

      postSummary[,"ROPElow"]=ROPE[1]
      postSummary[,"ROPEhigh"]=ROPE[2]
      postSummary[,"pLtROPE"]=pLtROPE
      postSummary[,"pInROPE"]=pInROPE
      postSummary[,"pGtROPE"]=pGtROPE
    }
    # Display the HDI.
    lines( HDI , c(0,0) , lwd=4 , lend=1 )
    text( mean(HDI) , 0 , bquote(.(100*credMass) * "% HDI" ) ,
          adj=c(.5,-1.7) , cex=cex )
    text( HDI[1] , 0 , bquote(.(signif(HDI[1],3))) ,
          adj=c(HDItextPlace,-0.5) , cex=cex )
    text( HDI[2] , 0 , bquote(.(signif(HDI[2],3))) ,
          adj=c(1.0-HDItextPlace,-0.5) , cex=cex )
    #
   # return( postSummary )
  }


  #------------------------------------------------------------------------------

  ################################################################################
  #                     Modified effective sample size                           #
  ################################################################################
  fncESS <- function (x=paramSampleVec)  {
    spectral <- function(x) {
      x <- as.matrix(x)
      v0 <- order <- numeric(ncol(x))
      names(v0) <- names(order) <- colnames(x)
      z <- 1:nrow(x)
      for (i in 1:ncol(x)) {
        lm.out <- lm(x[, i] ~ z)
        if (identical(all.equal(sd(residuals(lm.out)), 0), TRUE)) {
          v0[i] <- 0
          order[i] <- 0
        }
        else {
          ar.out <- ar(x[, i], aic = TRUE)
          v0[i] <- ar.out$var.pred/(1 - sum(ar.out$ar))^2
          order[i] <- ar.out$order
        }
      }
      return(list(spec = v0, order = order))
    }
    #Run ESS
    x <- as.matrix(x)
    spec <- spectral(x)$spec
    ans <- ifelse(spec == 0, 0, nrow(x) * apply(x, 2, var)/spec)
    return(ans)
  }

  ## End of Bayesian section ##
  ################################################################################

if(y == "post") {
  plotPost( paramSampleVec=paramSampleVec , cenTend=cenTend , compVal=compVal,
            ROPE=ROPE, credMass=credMass, HDItextPlace=HDItextPlace,
                        xlab=xlab , xlim=xlim , yaxt=NULL , ylab=ylab ,
                        main=main , cex=cex , cex.lab=cex.lab ,
                        bcol=bcol , lcol=lcol , border=NULL ,
            showCurve=showCurve , breaks=breaks , math=math, es=es, ... )
  }
  ## Diagnostics ##
  if(y == "dxa") {
    DbdaAcfPlot(MCMC=MCMC, parName=parameter, nChain=n_chains,
                plColors=DBDAplColors)
  }
  if(y == "dxd") {
    DbdaDensPlot(MCMC=MCMC, parName=parameter, nChain=n_chains,
                 plColors=DBDAplColors)
  }
  if(y== "dxg") {
    glsamp <- sort(round(c(100,500, (n_rowchn)/5:1)))
    glstat <- vector(length= length(glsamp))
    for(i in 1:length(glsamp)) {
      glstat[i] <- gelman.rubin(MCMC=MCMC, parameter=parameter, n_rows=n_rows,
                                n_chains=n_chains, n_rowchn=n_rowchn, samp=glsamp[i])
    }
    plot(glsamp, glstat, main="Gelman-Rubin statistic", xlab="Chain iterations",
         ylab= "Shrink Factor", type='b', col=DBDAplColors[3])
  }
  if(y == "dxt") {
    traceplot( MCMC=MCMC, parameter=parameter, n_rowchn=n_rowchn)
  }
  ## Posterior Predictive Checks ##
if(y == "check") {
  switch(ctype,
         "n" = fncGrpPostPredCheck(MCMC=MCMC, datFrm=data, Outcome=dv, Group=group[[1]],
                                   Group.Level=group[[2]], Mean.Var=parameter[1], SD.Var=parameter[2],
                                   MCnu=NULL, Distribution=ctype, Num.Lines=pline,
                                   Main.Title=main, X.Lab=xlab, Bar.Color=bcol,
                                   Line.Color=lcol, Hist.Breaks=breaks, X.Lim=xlim, Y.Lim=ylim,  Min.Val=vlim[1],
                                   Max.Val=vlim[2], Round.Digits=round.c, Point.Loc= xpt, PCol=pcol,
                                   Leg.Loc= add.legend, cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                                   cex.axis=cex.axis, legend=legend, cex.legend=cex.legend, lwd=lwd, Y.Lab=ylab ),
         "ln" = fncGrpPostPredCheck(MCMC=MCMC, datFrm=data, Outcome=dv, Group=group[[1]],
                                   Group.Level=group[[2]], Mean.Var=parameter[1], SD.Var=parameter[2],
                                   MCnu=NULL, Distribution=ctype, Num.Lines=pline,
                                   Main.Title=main, X.Lab=xlab, Bar.Color=bcol,
                                   Line.Color=lcol, Hist.Breaks=breaks, X.Lim=xlim, Y.Lim=ylim,  Min.Val=vlim[1],
                                   Max.Val=vlim[2], Round.Digits=round.c, Point.Loc= xpt, PCol=pcol,
                                   Leg.Loc= add.legend, cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                                   cex.axis=cex.axis, legend=legend, cex.legend=cex.legend, lwd=lwd, Y.Lab=ylab ),
         #MCnu extra parameter not in above!!!!!!
             "Skew-normal" =     fncGrpPostPredCheck(Coda.Object=DBDA_coda_object_df(), datFrm=df(),
                                                     Outcome=dbda_post_check_grp_Y(), Group=dbda_post_check_grp_X(),
                                                     Group.Level=dbda_post_check_grp_level_X(),
                                                     Mean.Var=dbda_post_check_grp_pm(),
                                                     SD.Var=dbda_post_check_grp_psd(), MCnu= dbda_post_check_grp_pnu(), #MCnu extra
                                                     Distribution=dbda_post_check_grp_distr(),
                                                     Num.Lines=dbda_post_check_grp_number_lines(),
                                                     Main.Title=dbda_post_check_grp_main_title(),
                                                     X.Lab=dbda_post_check_grp_x_label(),
                                                     Bar.Color=dbda_post_check_grp_bar_colors(),
                                                     Line.Color=dbda_post_check_grp_line_colors(),
                                                     Hist.Breaks=dbda_post_check_grp_number_bars(),
                                                     CEX.size=dbda_post_check_grp_label_multiplier(),
                                                     X.Lim=(eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                     Y.Lim=(eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                     Min.Val=dbda_post_check_grp_min_value(),
                                                     Max.Val=dbda_post_check_grp_max_value(),
                                                     Round.Digits=dbda_post_check_grp_round_place(),
                                                     Point.Loc= (eval(parse(text=dbda_post_check_grp_x_axis_points() )) ),
                                                     PCol = dbda_post_check_point_colors(),
                                                     Add.Lgd= dbda_post_check_add_legend(),
                                                     Leg.Loc=dbda_post_check_legend_location() ) ,
             "Weibull" =     fncGrpPostPredCheck(Coda.Object=DBDA_coda_object_df(), datFrm=df(),
                                                 Outcome=dbda_post_check_grp_Y(), Group=dbda_post_check_grp_X(),
                                                 Group.Level=dbda_post_check_grp_level_X(),
                                                 Mean.Var=dbda_post_check_grp_pm(),
                                                 SD.Var=dbda_post_check_grp_psd(), MCnu= dbda_post_check_grp_pnu(),
                                                 Distribution=dbda_post_check_grp_distr(),
                                                 Num.Lines=dbda_post_check_grp_number_lines(),
                                                 Main.Title=dbda_post_check_grp_main_title(),
                                                 X.Lab=dbda_post_check_grp_x_label(),
                                                 Bar.Color=dbda_post_check_grp_bar_colors(),
                                                 Line.Color=dbda_post_check_grp_line_colors(),
                                                 Hist.Breaks=dbda_post_check_grp_number_bars(),
                                                 CEX.size=dbda_post_check_grp_label_multiplier(),
                                                 X.Lim=(eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                 Y.Lim=(eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                 Min.Val=dbda_post_check_grp_min_value(),
                                                 Max.Val=dbda_post_check_grp_max_value(),
                                                 Round.Digits=dbda_post_check_grp_round_place(),
                                                 Point.Loc= (eval(parse(text=dbda_post_check_grp_x_axis_points() )) ),
                                                 PCol = dbda_post_check_point_colors(),
                                                 Add.Lgd= dbda_post_check_add_legend(),
                                                 Leg.Loc=dbda_post_check_legend_location() ) ,
             "Gamma" =     fncGrpPostPredCheck(Coda.Object=DBDA_coda_object_df(), datFrm=df(),
                                               Outcome=dbda_post_check_grp_Y(), Group=dbda_post_check_grp_X(),
                                               Group.Level=dbda_post_check_grp_level_X(),
                                               Mean.Var=dbda_post_check_grp_pm(),
                                               SD.Var=dbda_post_check_grp_psd(), MCnu= dbda_post_check_grp_pnu(),
                                               Distribution=dbda_post_check_grp_distr(),
                                               Num.Lines=dbda_post_check_grp_number_lines(),
                                               Main.Title=dbda_post_check_grp_main_title(),
                                               X.Lab=dbda_post_check_grp_x_label(),
                                               Bar.Color=dbda_post_check_grp_bar_colors(),
                                               Line.Color=dbda_post_check_grp_line_colors(),
                                               Hist.Breaks=dbda_post_check_grp_number_bars(),
                                               CEX.size=dbda_post_check_grp_label_multiplier(),
                                               X.Lim=(eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                               Y.Lim=(eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                               Min.Val=dbda_post_check_grp_min_value(),
                                               Max.Val=dbda_post_check_grp_max_value(),
                                               Round.Digits=dbda_post_check_grp_round_place(),
                                               Point.Loc= (eval(parse(text=dbda_post_check_grp_x_axis_points() )) ),
                                               PCol = dbda_post_check_point_colors(),
                                               Add.Lgd= dbda_post_check_add_legend(),
                                               Leg.Loc=dbda_post_check_legend_location() ) ,
             "t" =     fncGrpPostPredCheck(Coda.Object=DBDA_coda_object_df(), datFrm=df(),
                                           Outcome=dbda_post_check_grp_Y(), Group=dbda_post_check_grp_X(),
                                           Group.Level=dbda_post_check_grp_level_X(),
                                           Mean.Var=dbda_post_check_grp_pm(),
                                           SD.Var=dbda_post_check_grp_psd(), MCnu= dbda_post_check_grp_pnu(),
                                           Distribution=dbda_post_check_grp_distr(),
                                           Num.Lines=dbda_post_check_grp_number_lines(),
                                           Main.Title=dbda_post_check_grp_main_title(),
                                           X.Lab=dbda_post_check_grp_x_label(),
                                           Bar.Color=dbda_post_check_grp_bar_colors(),
                                           Line.Color=dbda_post_check_grp_line_colors(),
                                           Hist.Breaks=dbda_post_check_grp_number_bars(),
                                           CEX.size=dbda_post_check_grp_label_multiplier(),
                                           X.Lim=(eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                           Y.Lim=(eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                           Min.Val=dbda_post_check_grp_min_value(),
                                           Max.Val=dbda_post_check_grp_max_value(),
                                           Round.Digits=dbda_post_check_grp_round_place(),
                                           Point.Loc= (eval(parse(text=dbda_post_check_grp_x_axis_points() )) ),
                                           PCol = dbda_post_check_point_colors(),
                                           Add.Lgd= dbda_post_check_add_legend(),
                                           Leg.Loc=dbda_post_check_legend_location() ) ,
             "t: 1 group" = fncPlotSingleT(codaSamples=DBDA_coda_object_df(), datFrm=df(),
                                           yName=dbda_post_check_grp_Y(),
                                           MCmean=dbda_post_check_grp_pm(),
                                           MCsigma=dbda_post_check_grp_psd(),
                                           MCnu= dbda_post_check_grp_pnu(),
                                           Num.Lines=dbda_post_check_grp_number_lines(),
                                           Main.Title=dbda_post_check_grp_main_title(),
                                           X.Lab=dbda_post_check_grp_x_label(),
                                           Line.Color=dbda_post_check_grp_line_colors(),
                                           CEX.size=dbda_post_check_grp_label_multiplier(),
                                           X.Lim=(eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                           Y.Lim=(eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                           PCol = dbda_post_check_point_colors(),
                                           Add.Lgd= dbda_post_check_add_legend(),
                                           Leg.Loc=dbda_post_check_legend_location(),
                                           T.Percentage=dbda_post_check_grp_min_value() ),
             "t: ANOVA" = fncPlotMcANOVA(codaSamples=DBDA_coda_object_df(), datFrm=df(),
                                         yName=dbda_post_check_grp_Y(), xName=dbda_post_check_grp_X(),
                                         MCmean=dbda_post_check_grp_pm(),
                                         MCsigma=dbda_post_check_grp_psd(),
                                         MCnu= dbda_post_check_grp_pnu(),
                                         Num.Lines=dbda_post_check_grp_number_lines(),
                                         Main.Title=dbda_post_check_grp_main_title(),
                                         X.Lab=dbda_post_check_grp_x_label(),
                                         Line.Color=dbda_post_check_grp_line_colors(),
                                         CEX.size=dbda_post_check_grp_label_multiplier(),
                                         X.Lim=(eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                         Y.Lim=(eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                         PCol = dbda_post_check_point_colors(),
                                         Add.Lgd= dbda_post_check_add_legend(),
                                         Leg.Loc=dbda_post_check_legend_location(),
                                         T.Percentage=dbda_post_check_grp_min_value() ),
             "OLS: Linear" = fncBayesOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                Reg.Type= dbda_post_check_grp_distr(),
                                                Outcome= dbda_post_check_grp_Y(),
                                                Group= dbda_post_check_grp_X(),
                                                Group.Level= dbda_post_check_grp_level_X(),
                                                xName= dbda_post_check_part_pred_X(),
                                                parX= dbda_post_check_part_pred_pars(),
                                                View.Lines= dbda_post_check_part_pred_data(),
                                                Num.Lines= dbda_post_check_grp_number_lines(),
                                                Main.Title= dbda_post_check_grp_main_title(),
                                                X.Lab= dbda_post_check_grp_x_label(),
                                                Line.Color= dbda_post_check_grp_line_colors(),
                                                CEX.size= dbda_post_check_grp_label_multiplier(),
                                                X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                X.Min= dbda_post_check_grp_min_value(),
                                                X.Max= dbda_post_check_grp_max_value(),
                                                PCol= dbda_post_check_point_colors(),
                                                Add.Lgd= dbda_post_check_add_legend(),
                                                Leg.Loc= dbda_post_check_legend_location()),
             "OLS: Quadratic" = fncBayesOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                   Reg.Type= dbda_post_check_grp_distr(),
                                                   Outcome= dbda_post_check_grp_Y(),
                                                   Group= dbda_post_check_grp_X(),
                                                   Group.Level= dbda_post_check_grp_level_X(),
                                                   xName= dbda_post_check_part_pred_X(),
                                                   parX= dbda_post_check_part_pred_pars(),
                                                   View.Lines= dbda_post_check_part_pred_data(),
                                                   Num.Lines= dbda_post_check_grp_number_lines(),
                                                   Main.Title= dbda_post_check_grp_main_title(),
                                                   X.Lab= dbda_post_check_grp_x_label(),
                                                   Line.Color= dbda_post_check_grp_line_colors(),
                                                   CEX.size= dbda_post_check_grp_label_multiplier(),
                                                   X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                   Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                   X.Min= dbda_post_check_grp_min_value(),
                                                   X.Max= dbda_post_check_grp_max_value(),
                                                   PCol= dbda_post_check_point_colors(),
                                                   Add.Lgd= dbda_post_check_add_legend(),
                                                   Leg.Loc= dbda_post_check_legend_location()),
             "OLS: Cubic" = fncBayesOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                               Reg.Type= dbda_post_check_grp_distr(),
                                               Outcome= dbda_post_check_grp_Y(),
                                               Group= dbda_post_check_grp_X(),
                                               Group.Level= dbda_post_check_grp_level_X(),
                                               xName= dbda_post_check_part_pred_X(),
                                               parX= dbda_post_check_part_pred_pars(),
                                               View.Lines= dbda_post_check_part_pred_data(),
                                               Num.Lines= dbda_post_check_grp_number_lines(),
                                               Main.Title= dbda_post_check_grp_main_title(),
                                               X.Lab= dbda_post_check_grp_x_label(),
                                               Line.Color= dbda_post_check_grp_line_colors(),
                                               CEX.size= dbda_post_check_grp_label_multiplier(),
                                               X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                               Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                               X.Min= dbda_post_check_grp_min_value(),
                                               X.Max= dbda_post_check_grp_max_value(),
                                               PCol= dbda_post_check_point_colors(),
                                               Add.Lgd= dbda_post_check_add_legend(),
                                               Leg.Loc= dbda_post_check_legend_location()),
             "Logistic: Linear" = fncBayesOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                     Reg.Type= dbda_post_check_grp_distr(),
                                                     Outcome= dbda_post_check_grp_Y(),
                                                     Group= dbda_post_check_grp_X(),
                                                     Group.Level= dbda_post_check_grp_level_X(),
                                                     xName= dbda_post_check_part_pred_X(),
                                                     parX= dbda_post_check_part_pred_pars(),
                                                     View.Lines= dbda_post_check_part_pred_data(),
                                                     Num.Lines= dbda_post_check_grp_number_lines(),
                                                     Main.Title= dbda_post_check_grp_main_title(),
                                                     X.Lab= dbda_post_check_grp_x_label(),
                                                     Line.Color= dbda_post_check_grp_line_colors(),
                                                     CEX.size= dbda_post_check_grp_label_multiplier(),
                                                     X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                     Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                     X.Min= dbda_post_check_grp_min_value(),
                                                     X.Max= dbda_post_check_grp_max_value(),
                                                     PCol= dbda_post_check_point_colors(),
                                                     Add.Lgd= dbda_post_check_add_legend(),
                                                     Leg.Loc= dbda_post_check_legend_location()),
             "Logistic: Quadratic" = fncBayesOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                        Reg.Type= dbda_post_check_grp_distr(),
                                                        Outcome= dbda_post_check_grp_Y(),
                                                        Group= dbda_post_check_grp_X(),
                                                        Group.Level= dbda_post_check_grp_level_X(),
                                                        xName= dbda_post_check_part_pred_X(),
                                                        parX= dbda_post_check_part_pred_pars(),
                                                        View.Lines= dbda_post_check_part_pred_data(),
                                                        Num.Lines= dbda_post_check_grp_number_lines(),
                                                        Main.Title= dbda_post_check_grp_main_title(),
                                                        X.Lab= dbda_post_check_grp_x_label(),
                                                        Line.Color= dbda_post_check_grp_line_colors(),
                                                        CEX.size= dbda_post_check_grp_label_multiplier(),
                                                        X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                        Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                        X.Min= dbda_post_check_grp_min_value(),
                                                        X.Max= dbda_post_check_grp_max_value(),
                                                        PCol= dbda_post_check_point_colors(),
                                                        Add.Lgd= dbda_post_check_add_legend(),
                                                        Leg.Loc= dbda_post_check_legend_location()),
             "Logistic: Cubic" = fncBayesOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                    Reg.Type= dbda_post_check_grp_distr(),
                                                    Outcome= dbda_post_check_grp_Y(),
                                                    Group= dbda_post_check_grp_X(),
                                                    Group.Level= dbda_post_check_grp_level_X(),
                                                    xName= dbda_post_check_part_pred_X(),
                                                    parX= dbda_post_check_part_pred_pars(),
                                                    View.Lines= dbda_post_check_part_pred_data(),
                                                    Num.Lines= dbda_post_check_grp_number_lines(),
                                                    Main.Title= dbda_post_check_grp_main_title(),
                                                    X.Lab= dbda_post_check_grp_x_label(),
                                                    Line.Color= dbda_post_check_grp_line_colors(),
                                                    CEX.size= dbda_post_check_grp_label_multiplier(),
                                                    X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                    Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                    X.Min= dbda_post_check_grp_min_value(),
                                                    X.Max= dbda_post_check_grp_max_value(),
                                                    PCol= dbda_post_check_point_colors(),
                                                    Add.Lgd= dbda_post_check_add_legend(),
                                                    Leg.Loc= dbda_post_check_legend_location()),
             "OLS: DID" = fncBayesOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                             Reg.Type= dbda_post_check_grp_distr(),
                                             Outcome= dbda_post_check_grp_Y(),
                                             Group= dbda_post_check_grp_X(),
                                             Group.Level= dbda_post_check_grp_level_X(),
                                             xName= dbda_post_check_part_pred_X(),
                                             parX= dbda_post_check_part_pred_pars(),
                                             View.Lines= dbda_post_check_part_pred_data(),
                                             Num.Lines= dbda_post_check_grp_number_lines(),
                                             Main.Title= dbda_post_check_grp_main_title(),
                                             X.Lab= dbda_post_check_grp_x_label(),
                                             Line.Color= dbda_post_check_grp_line_colors(),
                                             CEX.size= dbda_post_check_grp_label_multiplier(),
                                             X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                             Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                             PCol= dbda_post_check_point_colors(),
                                             Add.Lgd= dbda_post_check_add_legend(),
                                             Leg.Loc= dbda_post_check_legend_location()),
             "Hierarchical OLS: Linear" = fncBayesMultiOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                                  Reg.Type= dbda_post_check_grp_distr(),
                                                                  Outcome= dbda_post_check_grp_Y(),
                                                                  Group= dbda_post_check_grp_X(),
                                                                  xName= dbda_post_check_part_pred_X(),
                                                                  parX= dbda_post_check_part_pred_pars(),
                                                                  View.Lines= dbda_post_check_part_pred_data(),
                                                                  Main.Title= dbda_post_check_grp_main_title(),
                                                                  X.Lab= dbda_post_check_grp_x_label(),
                                                                  Line.Color= dbda_post_check_grp_line_colors(),
                                                                  CEX.size= dbda_post_check_grp_label_multiplier(),
                                                                  X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                                  Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                                  X.Min= dbda_post_check_grp_min_value(),
                                                                  X.Max= dbda_post_check_grp_max_value(),
                                                                  PCol= dbda_post_check_point_colors(),
                                                                  Add.Lgd= dbda_post_check_add_legend(),
                                                                  Leg.Loc= dbda_post_check_legend_location(),
                                                                  mc_row_number= dbda_post_check_grp_number_lines()),
             "Hierarchical OLS: Quadratic" = fncBayesMultiOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                                     Reg.Type= dbda_post_check_grp_distr(),
                                                                     Outcome= dbda_post_check_grp_Y(),
                                                                     Group= dbda_post_check_grp_X(),
                                                                     xName= dbda_post_check_part_pred_X(),
                                                                     parX= dbda_post_check_part_pred_pars(),
                                                                     View.Lines= dbda_post_check_part_pred_data(),
                                                                     Main.Title= dbda_post_check_grp_main_title(),
                                                                     X.Lab= dbda_post_check_grp_x_label(),
                                                                     Line.Color= dbda_post_check_grp_line_colors(),
                                                                     CEX.size= dbda_post_check_grp_label_multiplier(),
                                                                     X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                                     Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                                     X.Min= dbda_post_check_grp_min_value(),
                                                                     X.Max= dbda_post_check_grp_max_value(),
                                                                     PCol= dbda_post_check_point_colors(),
                                                                     Add.Lgd= dbda_post_check_add_legend(),
                                                                     Leg.Loc= dbda_post_check_legend_location(),
                                                                     mc_row_number= dbda_post_check_grp_number_lines()),
             "Hierarchical OLS: Cubic" = fncBayesMultiOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                                 Reg.Type= dbda_post_check_grp_distr(),
                                                                 Outcome= dbda_post_check_grp_Y(),
                                                                 Group= dbda_post_check_grp_X(),
                                                                 xName= dbda_post_check_part_pred_X(),
                                                                 parX= dbda_post_check_part_pred_pars(),
                                                                 View.Lines= dbda_post_check_part_pred_data(),
                                                                 Main.Title= dbda_post_check_grp_main_title(),
                                                                 X.Lab= dbda_post_check_grp_x_label(),
                                                                 Line.Color= dbda_post_check_grp_line_colors(),
                                                                 CEX.size= dbda_post_check_grp_label_multiplier(),
                                                                 X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                                 Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                                 X.Min= dbda_post_check_grp_min_value(),
                                                                 X.Max= dbda_post_check_grp_max_value(),
                                                                 PCol= dbda_post_check_point_colors(),
                                                                 Add.Lgd= dbda_post_check_add_legend(),
                                                                 Leg.Loc= dbda_post_check_legend_location(),
                                                                 mc_row_number= dbda_post_check_grp_number_lines()),
             "Hierarchical Log OLS: Linear" = fncBayesMultiOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                                      Reg.Type= dbda_post_check_grp_distr(),
                                                                      Outcome= dbda_post_check_grp_Y(),
                                                                      Group= dbda_post_check_grp_X(),
                                                                      xName= dbda_post_check_part_pred_X(),
                                                                      parX= dbda_post_check_part_pred_pars(),
                                                                      View.Lines= dbda_post_check_part_pred_data(),
                                                                      Main.Title= dbda_post_check_grp_main_title(),
                                                                      X.Lab= dbda_post_check_grp_x_label(),
                                                                      Line.Color= dbda_post_check_grp_line_colors(),
                                                                      CEX.size= dbda_post_check_grp_label_multiplier(),
                                                                      X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                                      Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                                      X.Min= dbda_post_check_grp_min_value(),
                                                                      X.Max= dbda_post_check_grp_max_value(),
                                                                      PCol= dbda_post_check_point_colors(),
                                                                      Add.Lgd= dbda_post_check_add_legend(),
                                                                      Leg.Loc= dbda_post_check_legend_location(),
                                                                      mc_row_number= dbda_post_check_grp_number_lines()),
             "Hierarchical Log OLS: Quadratic" = fncBayesMultiOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                                         Reg.Type= dbda_post_check_grp_distr(),
                                                                         Outcome= dbda_post_check_grp_Y(),
                                                                         Group= dbda_post_check_grp_X(),
                                                                         xName= dbda_post_check_part_pred_X(),
                                                                         parX= dbda_post_check_part_pred_pars(),
                                                                         View.Lines= dbda_post_check_part_pred_data(),
                                                                         Main.Title= dbda_post_check_grp_main_title(),
                                                                         X.Lab= dbda_post_check_grp_x_label(),
                                                                         Line.Color= dbda_post_check_grp_line_colors(),
                                                                         CEX.size= dbda_post_check_grp_label_multiplier(),
                                                                         X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                                         Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                                         X.Min= dbda_post_check_grp_min_value(),
                                                                         X.Max= dbda_post_check_grp_max_value(),
                                                                         PCol= dbda_post_check_point_colors(),
                                                                         Add.Lgd= dbda_post_check_add_legend(),
                                                                         Leg.Loc= dbda_post_check_legend_location(),
                                                                         mc_row_number= dbda_post_check_grp_number_lines()),
             "Hierarchical Log OLS: Cubic" = fncBayesMultiOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                                     Reg.Type= dbda_post_check_grp_distr(),
                                                                     Outcome= dbda_post_check_grp_Y(),
                                                                     Group= dbda_post_check_grp_X(),
                                                                     xName= dbda_post_check_part_pred_X(),
                                                                     parX= dbda_post_check_part_pred_pars(),
                                                                     View.Lines= dbda_post_check_part_pred_data(),
                                                                     Main.Title= dbda_post_check_grp_main_title(),
                                                                     X.Lab= dbda_post_check_grp_x_label(),
                                                                     Line.Color= dbda_post_check_grp_line_colors(),
                                                                     CEX.size= dbda_post_check_grp_label_multiplier(),
                                                                     X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                                     Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                                     X.Min= dbda_post_check_grp_min_value(),
                                                                     X.Max= dbda_post_check_grp_max_value(),
                                                                     PCol= dbda_post_check_point_colors(),
                                                                     Add.Lgd= dbda_post_check_add_legend(),
                                                                     Leg.Loc= dbda_post_check_legend_location(),
                                                                     mc_row_number= dbda_post_check_grp_number_lines()),
             "Hierarchical Logistic: Linear" = fncBayesMultiOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                                       Reg.Type= dbda_post_check_grp_distr(),
                                                                       Outcome= dbda_post_check_grp_Y(),
                                                                       Group= dbda_post_check_grp_X(),
                                                                       xName= dbda_post_check_part_pred_X(),
                                                                       parX= dbda_post_check_part_pred_pars(),
                                                                       View.Lines= dbda_post_check_part_pred_data(),
                                                                       Main.Title= dbda_post_check_grp_main_title(),
                                                                       X.Lab= dbda_post_check_grp_x_label(),
                                                                       Line.Color= dbda_post_check_grp_line_colors(),
                                                                       CEX.size= dbda_post_check_grp_label_multiplier(),
                                                                       X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                                       Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                                       X.Min= dbda_post_check_grp_min_value(),
                                                                       X.Max= dbda_post_check_grp_max_value(),
                                                                       PCol= dbda_post_check_point_colors(),
                                                                       Add.Lgd= dbda_post_check_add_legend(),
                                                                       Leg.Loc= dbda_post_check_legend_location(),
                                                                       mc_row_number= dbda_post_check_grp_number_lines()),
             "Hierarchical Logistic: Quadratic" = fncBayesMultiOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                                          Reg.Type= dbda_post_check_grp_distr(),
                                                                          Outcome= dbda_post_check_grp_Y(),
                                                                          Group= dbda_post_check_grp_X(),
                                                                          xName= dbda_post_check_part_pred_X(),
                                                                          parX= dbda_post_check_part_pred_pars(),
                                                                          View.Lines= dbda_post_check_part_pred_data(),
                                                                          Main.Title= dbda_post_check_grp_main_title(),
                                                                          X.Lab= dbda_post_check_grp_x_label(),
                                                                          Line.Color= dbda_post_check_grp_line_colors(),
                                                                          CEX.size= dbda_post_check_grp_label_multiplier(),
                                                                          X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                                          Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                                          X.Min= dbda_post_check_grp_min_value(),
                                                                          X.Max= dbda_post_check_grp_max_value(),
                                                                          PCol= dbda_post_check_point_colors(),
                                                                          Add.Lgd= dbda_post_check_add_legend(),
                                                                          Leg.Loc= dbda_post_check_legend_location(),
                                                                          mc_row_number= dbda_post_check_grp_number_lines()),
             "Hierarchical Logistic: Cubic" = fncBayesMultiOlsPrtPred(Coda.Object=DBDA_coda_object_df() , datFrm=df(),
                                                                      Reg.Type= dbda_post_check_grp_distr(),
                                                                      Outcome= dbda_post_check_grp_Y(),
                                                                      Group= dbda_post_check_grp_X(),
                                                                      xName= dbda_post_check_part_pred_X(),
                                                                      parX= dbda_post_check_part_pred_pars(),
                                                                      View.Lines= dbda_post_check_part_pred_data(),
                                                                      Main.Title= dbda_post_check_grp_main_title(),
                                                                      X.Lab= dbda_post_check_grp_x_label(),
                                                                      Line.Color= dbda_post_check_grp_line_colors(),
                                                                      CEX.size= dbda_post_check_grp_label_multiplier(),
                                                                      X.Lim= (eval(parse(text= dbda_post_check_grp_x_axis_limits() )) ),
                                                                      Y.Lim= (eval(parse(text= dbda_post_check_grp_y_axis_limits() )) ),
                                                                      X.Min= dbda_post_check_grp_min_value(),
                                                                      X.Max= dbda_post_check_grp_max_value(),
                                                                      PCol= dbda_post_check_point_colors(),
                                                                      Add.Lgd= dbda_post_check_add_legend(),
                                                                      Leg.Loc= dbda_post_check_legend_location(),
                                                                      mc_row_number= dbda_post_check_grp_number_lines())
      )
    }


} #end of plot.bayes

