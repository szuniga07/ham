#' Bayesian plots for various analyses
#'
#' Graph X-bar charts, p-charts, and u-charts. This includes
#' producing means center lines, 3-sigma upper and lower control limits. Users can also calculate
#' values before and after an intervention to see if a change in the control process happened. Values are
#' returned in a data frame.
#'
#' @param x Bayes class object.
#' @param y character vector for the type of plot to graph. Select 'post', 'dxa', 'dxd', 'dxg', 'dxt', 'check',
#' 'multi' (specialized version of 'check'), or 'target' for posterior summary, diagnostics (4 'dx' plots produced:
#' autocorrelation factor, density plots on chain convergence, Gelman-Rubin statistic, and traceplot), posterior predictive
#' check, multilevel or hierarchical model summary (up to 3 levels), or target summary plots. Default is 'post'.
#' @param type character vector of length == 1 that indicates the likelihood function used in the model when y='check' or y='multi'.
#' Posterior predictive checks allow us to see how well our estimates match the observed data. These checks are
#' available for Bayesian estimation of outcomes and regression polynomial trend line using various distributions in the
#' likelihood function. Select 'n', 'ln', 'sn', 'w', 'g', 't', 'taov', 'taov1', 'ol', 'oq','oc', 'lnl', 'lnq', 'lnc',
#' 'logl', 'logq', 'logc', 'bern', and 'bin' for these respective options in Bayesian estimation (multilevel): 'Normal', 'Log-normal',
#' 'Skew-normal', 'Weibull', 'Gamma', 't', 't: ANOVA, side view', 't: ANOVA 1 group, side view'; and for regression trend lines:
#' 'OLS: Linear', 'OLS: Quadratic', 'OLS: Cubic', 'Log-normal: Linear', 'Log-normal: Quadratic', 'Log-normal: Cubic',
#' 'Logistic: Linear', 'Logistic: Quadratic', and 'Logistic: Cubic', 'Bernoulli', and 'binomial'. The first 8 selections are for Bayesian
#' estimation of outcomes, the next 9 options were developed to assess regression trend lines from ordinary least squares (OLS),
#' log-normal, and logistic models and also for hierarchical model versions. And the remaining two ('bern' and 'bin') are for
#' when y='multi'. Additional models analogous to 'Generalized Linear Models' can also be graphed on the logit scale using 'OLS'
#' options. For example, plot a logistic model on the logit scale when type='ol' (i.e., view straight trend lines). Or if you prefer
#' viewing results on the probability scale, select type='logl' (i.e., curved lines). And consider using type= 'lnl', 'lnq', 'lnc'
#' for log-normal and Poisson models for lines with exponentiated values. In general, it is important to note that the observed data
#' may not be on the same scale as the parameter estimates and may not be visible in the graph. Default is NULL.
#' @param parameter a character vector of length >= 1 or a 2 element list with the name(s) of parameter in MCMC chains to produce
#' summary statistics. Use a 1 element vector to get posterior estimates of a single parameter. Use a 2 or more element vector
#' to estimate the average joint effects of multiple parameters (e.g., average infection rate for interventions A and B when
#' parameter= c('IntA', 'IntB')). Use a 2 element list to perform mathematical calculations of multiple parameters (see 'math' below).
#' For example, use parameter=list('hospital_A', 'hospital_Z') if you want to estimate the difference between the hospital's outcomes.
#' Use parameter= list(c('hospital_A','hospital_B'), ('hospital_Y','hospital_Z')) to estimate how different the combined hospitals A
#' and B values are from the combined Hospital Y and Z values. When y='check', use either a multiple element character vector that represents
#' center, spread, and additional distribution parameters in order of 1st, 2nd, and 3rd distribution parameters. For example,
#' mean and sd for a normal distribution; mean log and sd log of a log-normal dist.; xi, omega, and alpha of a skew-normal dist.;
#' shape, scale, and lambda of a Weibull dist.; shape and rate of a Gamma dist.; and mean, SD and nu/d.f. of a t-distribution.
#' Or indicate regression parameters in order (e.g., intercept, B1, B2, etc.). When y='multi', use a multiple element character vector
#' to list the parameter names of the hierarchy, in order of the nesting with the lowest level first (e.g., exam, patient, hospital).
#' When y='multi', for parameters from multiple groups such as various hospitals, only enter the first unit's prefix of each parameter
#' and the remaining units will be set up for graphing all units. For example, parameter=c('theta', 'omega') will plot data for
#' `theta[1]` to `theta[8]` and `omega[1]` to `omega[8]` as well.
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
#' when y='check' or y='multi'. Default is NULL.
#' @param iv character vector of length >= 1 for the independent variable name(s) in the observed data frame
#' when y='check' or y='multi'. When y='multi', enter the lower to higher level clustering or group names (e.g, for
#' health data, iv=c("patient", "hospital"). When type='taov', enter the name of the test group variable.  Default is NULL.
#' @param add.data character vector of length == 1 to determine the type of observed data added to the plot
#' when y='check' and type= 'ol', 'oq','oc', 'lnl', 'lnq', 'lnc', 'logl', 'logq', or 'logc'. Select 'a', 'u', 'al', 'ul',
#' 'n' for these observed data options: 'All', 'Unit', 'All: Lines', 'Unit: Lines', 'none'. Default is 'n' for none.
#' @param group character list of length == 2 for 1) the grouping variable name and 2) specific group(s) in the
#' observed data frame. This is primarily used for multilevel or hierarchical models when y='check' or y='multi'
#' that the hierarchies are based on (e.g., hospitals nested within health systems).
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
#' @param pct a numeric integer vector of length == 1 for the percentage of the posterior predictive check heavy tail lines
#' to be drawn when type= 'taov' or 'taov1'. Valid values are 0 < pct < 100. Default is 95 (e.g., 95%).
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
#' @param es one element vector that indicates which type likelihood distribution is relevant in calculating Jacob Cohen's effect
#' sizes between 2 parameters when y='post'. Options are 'beta' and 'n' for the beta (or Bernoulli or binomial) distribution for
#' binary outcomes and none (i.e., no distribution, hence no effect size calculated). For example, to get the posterior distribution
#' summary for the difference between the intervention and control groups on 30-day readmissions or not, use es='beta' when y='post',
#' math='subtract', and parameter=list('intMean', 'ctlMean'). Default is 'n' which indicates not to calculate the effect size.
#' @param subset a single or multiple element character or numeric vector of group names that are a subset of the observations to use in the
#' grouping when y='multi'. The default is NULL, thereby using all observations. Specify, for example, enter c('NY', 'Toronto', 'LA', 'Vancouver')
#' to view a graph with only these cities. Default is NULL.
#' use just those observations.
#' @param level a numeric integer of length == 1, either 1, 2, or 3 that indicates the level of the hierarchical/multilevel
#' model when y='multi' and the type of graph to plot. For example, a multilevel model that estimates the proportion of
#' successful exams by patients is considered level=2. And the successful exam rates by various hospitals is level=3.
#' Graphs can be created separately for both level=2 and level=3 when there is a three-level model. The graph when y='multi'
#' can be produced when level=1 for non-hierarchical models if there are estimates for groups. For example, estimating the patient
#' infection rate of hospitals without a hierarchical structure in the model. Default is NULL.
#' @param aorder a logical indicator on whether the ordering of the group levels are in alphabetical order or not. If aorder=TRUE,
#' results are displayed in an increasing alphabetical order based on level name (e.g., 'LA' before 'NY'). If aorder=FALSE, an
#' increasing numeric order based on group parameter values is performed (e.g., 0.65 before 0.70). Default is TRUE.
#' @param round.c an integer indicating the number of decimal places when rounding numbers such as for y.axis.
#' Default is 2.
#' @param ... additional arguments.
#'
#' @return plot of Shewhart control charts: X-bar charts, p-charts, and u-charts with 3-sigma control limits.
#' @importFrom graphics lines plot abline points text arrows hist layout matplot mtext plot.new
#' @importFrom utils head tail
#' @importFrom methods is
#' @importFrom stats lm acf cor dgamma dlnorm dnorm dt dweibull pbeta pgamma plnorm pnorm pweibull qbeta qgamma qlnorm qweibull runif qchisq df
#' @export
#' @references
#' Kruschke, J. (2014). Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and
#' Stan, Second Edition. New York: Academic Press. ISBN: 9780124058880
#'
#' @examples
#' ## Hospital LOS and readmissions ##


plot.Bayes <- function(x, y=NULL, type="n", parameter=NULL, center="mode", mass=0.95, compare=NULL, rope=NULL,
                       data=NULL, dv=NULL, iv=NULL, add.data="n", group=NULL,
                       main=NULL, xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, vlim=NULL, curve=FALSE, lwd=NULL, breaks=15,
                       bcol=NULL, lcol=NULL, pcol=NULL, xpt=NULL, tgt=NULL, tgtcol="gray", tpline=NULL, tpcol=NULL,
                       pline=20, pct=95, add.legend=NULL, legend=NULL, cex=1, cex.lab=NULL, cex.axis=NULL, cex.main=NULL,
                       cex.text=NULL, cex.legend=NULL, HDItext=0.7, math="n", es="n",
                       subset=NULL, level=NULL, aorder=TRUE, round.c=2, ...) {
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
  if ( !is.null(type)) {
    if ( !is.null(group)) {
      if ( length(group) != 2) {
        stop("Error: Expecting a 2 element list with group variable and specific group level.")
      }
    }
  }
  #pct within the right range
  if(pct <= 0 & pct >= 100) {
    stop("Error: Expecting pct within this range: 0 < pct < 100.")
    }

#Assign new objects
  MCMC <- x$MCMC
  multi_smry <- x$Multilevel
  cenTend <- center
  compVal <- compare
  ROPE <- rope
  credMass <- mass
  showCurve <- curve
  HDItextPlace <- HDItext
  level <- as.character(level) #not working as numeric level
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
  #                     Modified effective sample size                           #
  ################################################################################

  ################################################################################
  #           3. Function to plot HDIs for hierarchical estimation               #
  ################################################################################
  #MCmatrix is the output object from fncHdiBinSmry()
  fncHdiBinP <- function(multi_smry=multi_smry, View.Order=TRUE, View.Level=NULL,
                         GroupX=NULL, Lcol=NULL, Pcol=NULL,
                         tgt=NULL, tgt.col=NULL, plyCol=NULL, roundVal=NULL,
                         XLim1=NULL, XLim2=NULL, legend=NULL, Leg.Loc=NULL,
                         lwd=NULL, cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                         cex.axis=cex.axis, cex.legend=cex.legend, X.Lab=NULL) {
    #Convert View.Level back to a numeric value
    View.Level <- as.numeric(level)
    # Assign objects from MCMC matrix objects
    Group2 <- multi_smry$Group2
    Group3 <- multi_smry$Group3
    Outcome <- multi_smry$Outcome
    ciconf_lev <- multi_smry$ciconf_lev
    Average <- multi_smry$Average
    Theta <- multi_smry$Theta
    Omega2 <- multi_smry$Omega2
    Omega3 <- multi_smry$Omega3
    LTR <- multi_smry$LTR
    LO2R <- multi_smry$LO2R
    LO3R <- multi_smry$LO3R
    Lower <- multi_smry$Lower
    Upper <- multi_smry$Upper
    Group3.Obs1 <- multi_smry$Group3.Obs1
    Group3.Obs2 <- multi_smry$Group3.Obs2
    Level <- multi_smry$Level

    #X.Min and X.Max
    if (!is.null(XLim1)) {
      XLim1 <- XLim1
    } else {
      XLim1 <- NULL
    }
    if (!is.null(XLim2)) {
      XLim2 <- XLim2
    } else {
      XLim2 <- NULL
    }

    #Select the group levels that determine which rows go into the data frame
    if(Level== 1) {
      if (View.Level <= 2) {
        row_numbers <- 1:LTR
      }
    }
    #Level-2, hierarchical model
    if(Level== 2) {
      if (View.Level <= 2) {
        row_numbers <- 1:(LTR + LO2R)
      }
    }
    #Level-3, hierarchical model
    if(Level== 3) {
      if (View.Level == 3) {
        row_numbers <- setdiff(1:(LTR + LO2R + LO3R), 1:LTR)
      } else {
        row_numbers <- setdiff(1:(LTR + LO2R + LO3R), (LTR + 1):(LTR + LO2R))
      }
    }
    #Create hdidf table of Bayesian estimates
    if(View.Order == TRUE) {                                  #Post1
      hdidf <- multi_smry$Post1[row_numbers , c(Group2, Average, Lower, Upper, "Obs.Rate")]
    }
    if(View.Order == FALSE) {                                  #Post2
      hdidf <- multi_smry$Post2[row_numbers , c(Group2, Average, Lower, Upper, "Obs.Rate")]
    }
    #Create adf table of observed values
    if(View.Order == TRUE) {                                  #Post1
      adf <- multi_smry$Post1[row_numbers , c(Group2, "Obs.Rate")]
    }
    if(View.Order == FALSE) {                                  #Post2
      adf <- multi_smry$Post2[row_numbers , c(Group2, "Obs.Rate")]
    }
    #Hierarchical average for the highest level (e.g., Omega)
    if(Level >= 2) {
      mainYmn <- hdidf[nrow(hdidf), which(colnames(hdidf)== Average)]
    } else {
      mainYmn <- NA
    }
    #Select which 3-level category data gets reported
    if(View.Order == TRUE) {                                  #Post1
      Group3.Obs <- Group3.Obs1
    }
    if(View.Order == FALSE) {                                  #Post2
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
    #Make xlabel
    if (!is.null(X.Lab)) {
      X.Lab <- X.Lab
    } else {
      X.Lab <- X_Label
    }
    #Main title
    #Level-3, hierarchical model
    if(Level== 3) {
      if (View.Level == 3) {
        main_ttl <- paste0(ciconf_lev * 100, "% ", "Highest Density Intervals of ", Outcome, " by ", Group3)
      }
    }
    if(Level== 3) {
      if (View.Level <= 2) {
        main_ttl <- paste0(ciconf_lev * 100, "% ", "Highest Density Intervals of ", Outcome, " by ", Group2)
      }
    }
    if(Level < 3) {
      main_ttl <- paste0(ciconf_lev * 100, "% ", "Highest Density Intervals of ", Outcome, " by ", Group2)
    }
    #Legend
    legend_text <- NA
    #Level-3, hierarchical model
    if(Level== 3) {
      if (View.Level == 3) {
        legend_text <- c(paste0("Observed ", Group2), paste0("Observed ", Group3), "Hierarchical Estimate")
        legend_type <- c(3, 2, 24)
        pcol_vector <- c(Pcol[2], Pcol[1], Pcol[1])
      }
    }
    if(Level== 3) {
      if (View.Level <= 2) {
        legend_text <- c("Observed Rate", "Hierarchical Estimate")
        legend_type <- c(2, 24)
        pcol_vector <- Pcol[1]
      }
    }
    if(Level == 2) {
      legend_text <- c("Observed Rate", "Hierarchical Estimate")
      legend_type <- c(2, 24)
      pcol_vector <- Pcol[1]
    }
    if(Level == 1) {
      legend_text <- c("Estimate")
      legend_type <- c(24)
      pcol_vector <- Pcol[1]
    }
    #Use legend text if specified
    legend_text <- if (!is.null(legend)) legend else legend_text

    #Get names of level 1:3 or just overall level-3 groups
    if(Level== 3) {
      if (View.Level == 3) {
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
         xlab= X.Lab,
         axes=F,  cex.lab=cex.lab, xlim=c(XLim1, XLim2))
    title(main_ttl, cex.main= cex.main)
    #Merge 2 tables so I can get points in correct order
    for (i in 1:(length(plot_row_numbers)) ) {
      lines(c(hdidf[plot_row_numbers, Lower][i], hdidf[plot_row_numbers, Upper][i]),
            c((1:length(plot_row_numbers))[i], (1:length(plot_row_numbers))[i]),
            lwd=lwd, col=Lcol)
      #Points for observed rates and Bayesian estimates
      points(hdidf[plot_row_numbers, Average][i ], i, pch=24, col=Pcol[1], lwd=lwd, bg=Pcol[1], cex=cex)
      if(Level >= 2) {
        points(hdidf[plot_row_numbers, "Obs.Rate"][i ], (1:length(plot_row_numbers))[i], pch=2, col=Pcol[1], lwd=lwd, bg=Pcol[1], cex=cex)
      }
    }
    #Add points for the level-3 category for each group per category
    if(Level== 3) {
      if (View.Level == 3) {
        for (i in 1:LO2R) {
          points( Group3.Obs[[plot_row_numbers[i]]], rep( (1:LO2R)[i], length(Group3.Obs[[plot_row_numbers[i]]])), pch=3,
                  col=Pcol[2], lwd=lwd)
        }
      }
    }
    #Mean line
    if(Level >= 2) {
      abline(v=mainYmn, lwd=lwd, col="grey", lty=3)
    }
    axis(1, cex.axis=cex.axis)
    axis(2, at=1:length(plot_row_numbers), labels= substr(hdidf[plot_row_numbers, Group2], 1, 10), las=1, cex.axis=cex.axis )
    axis(4, at=1:length(plot_row_numbers), tick = F, line=-.6,
         labels= round(hdidf[plot_row_numbers, Average], roundVal),
         las=1, cex.axis= cex.axis )
    ## Add overall confidence bar ##
    #Create x and y data
    Cbar_x <- c(rep(hdidf[nrow(hdidf), Lower], length(plot_row_numbers)), rep(hdidf[nrow(hdidf), Upper], length(plot_row_numbers) ))
    Cbar_y <- c(1:length(plot_row_numbers), length(plot_row_numbers):1)
    #Create shading
    if(Level >= 2) {
      polygon(Cbar_x, Cbar_y, col = adjustcolor(plyCol, alpha.f = 0.4), border= plyCol )
    }
    #Add legend
    if(!is.null(Leg.Loc) ) {
      legend(Leg.Loc, legend=legend_text, lty=NA,
             col=pcol_vector, lwd=lwd,
             pch=legend_type, pt.bg=pcol_vector, cex = cex.legend,
             bty="n", inset=c(0, .05))
    }
    #Target line
    abline(v=tgt, lwd=lwd, col=tgt.col, lty=1)
    box()
  }

  ################################################################################
  #                            4a. Skew-normal density                           #
  ################################################################################
  #density
  dskewn <- function (x, xi = 0, omega = 1, alpha = 0, tau = 0, dp = NULL,
                      log = FALSE)
  {
    if (!is.null(dp)) {
      if (!missing(alpha))
        stop("You cannot set both 'dp' and component parameters")
      xi <- dp[1]
      omega <- dp[2]
      alpha <- dp[3]
      tau <- if (length(dp) > 3)
        dp[4]
      else 0
    }
    za <- cbind((x - xi)/omega, alpha)
    z <- za[, 1]
    alpha <- za[, 2]
    logN <- (-log(sqrt(2 * pi)) - logb(omega) - z^2/2)
    logS <- numeric(length(z))
    ok <- (abs(alpha) < Inf)
    logS[ok] <- pnorm(tau * sqrt(1 + alpha[ok]^2) + (alpha *
                                                       z)[ok], log.p = TRUE)
    logS[!ok] <- log(as.numeric((sign(alpha) * z)[!ok] + tau >
                                  0))
    logPDF <- as.numeric(logN + logS - pnorm(tau, log.p = TRUE))
    logPDF <- replace(logPDF, abs(x) == Inf, -Inf)
    logPDF <- replace(logPDF, omega <= 0, NaN)
    out <- if (log)
      logPDF
    else exp(logPDF)
    names(out) <- names(x)
    return(out)
  }
  #probability
  pskewn <- function (x, xi = 0, omega = 1, alpha = 0, tau = 0, dp = NULL,
                      engine="T.Owen", ...)
  {
    T.Owen <- function (h, a, jmax = 50, cut.point = 8)
    {
      T.int <- function(h, a, jmax, cut.point) {
        fui <- function(h, i) (h^(2 * i))/((2^i) * gamma(i +
                                                           1))
        seriesL <- seriesH <- NULL
        i <- 0:jmax
        low <- (h <= cut.point)
        hL <- h[low]
        hH <- h[!low]
        L <- length(hL)
        if (L > 0) {
          b <- outer(hL, i, fui)
          cumb <- apply(b, 1, cumsum)
          b1 <- exp(-0.5 * hL^2) * t(cumb)
          matr <- matrix(1, jmax + 1, L) - t(b1)
          jk <- rep(c(1, -1), jmax)[1:(jmax + 1)]/(2 * i +
                                                     1)
          matr <- t(matr * jk) %*% a^(2 * i + 1)
          seriesL <- (atan(a) - as.vector(matr))/(2 * pi)
        }
        if (length(hH) > 0)
          seriesH <- atan(a) * exp(-0.5 * (hH^2) * a/atan(a)) *
          (1 + 0.00868 * (hH * a)^4)/(2 * pi)
        series <- c(seriesL, seriesH)
        id <- c((1:length(h))[low], (1:length(h))[!low])
        series[id] <- series
        series
      }
      if (!is.vector(a) | length(a) > 1)
        stop("'a' must be a vector of length 1")
      if (!is.vector(h))
        stop("'h' must be a vector")
      aa <- abs(a)
      ah <- abs(h)
      if (is.na(aa))
        stop("parameter 'a' is NA")
      if (aa == Inf)
        return(sign(a) * 0.5 * pnorm(-ah))
      if (aa == 0)
        return(rep(0, length(h)))
      na <- is.na(h)
      inf <- (ah == Inf)
      ah <- replace(ah, (na | inf), 0)
      if (aa <= 1)
        owen <- T.int(ah, aa, jmax, cut.point)
      else owen <- (0.5 * pnorm(ah) + pnorm(aa * ah) * (0.5 -
                                                          pnorm(ah)) - T.int(aa * ah, (1/aa), jmax, cut.point))
      owen <- replace(owen, na, NA)
      owen <- replace(owen, inf, 0)
      return(owen * sign(a))
    }

    #pskewn code
    if (!is.null(dp)) {
      if (!missing(alpha))
        stop("You cannot set both 'dp' and component parameters")
      xi <- dp[1]
      omega <- dp[2]
      alpha <- dp[3]
      tau <- if (length(dp) > 3)
        dp[4]
      else 0
    }
    z <- (x - xi)/omega
    prob <- rep(NA, length(z))
    plain <- is.finite(z) & (omega > 0)
    if (any(!plain)) {
      prob <- replace(prob, z == -Inf, 0)
      prob <- replace(prob, z == Inf, 1)
      prob <- replace(prob, is.na(z) | (omega <= 0), NA)
    }
    if (sum(plain) == 0)
      return(prob)
    na <- length(alpha)
    za <- matrix(cbind(z, alpha), ncol = 2)[plain, , drop = FALSE]
    z <- za[, 1]
    nz <- length(z)
    if (missing(engine))
      engine <- if (na == 1 & nz > 3 & all(z * za[, 2] > -5) &
                    (tau == 0))
        "T.Owen"
#    else "biv.nt.prob" #removing biv.nt.prob from mnormt
    if (engine == "T.Owen") {
      if (tau != 0 | na > 1)
        stop("engine='T.Owen' not compatible with other arguments")
      p <- pnorm(z) - 2 * T.Owen(z, alpha, ...)
    }
#    else {
#      p <- numeric(nz)
#      alpha <- za[, 2]
#      delta <- delta.etc(alpha)
#      p.tau <- pnorm(tau)
#      for (k in seq_len(nz)) {
#        if (abs(z[k]) == Inf)
#          p[k] <- (sign(z[k]) + 1)/2
#        else {
#          if (abs(alpha[k]) == Inf) {
#            p[k] <- if (alpha[k] > 0)
#              (pnorm(pmax(z[k], -tau)) - pnorm(-tau))/p.tau
#            else {
#              1 - (pnorm(tau) - pnorm(pmin(z[k], tau)))/p.tau
#            }
#          }
##          else { #removing mnormt
##            R <- matrix(c(1, -delta[k], -delta[k], 1),
##                        2, 2)
##            p[k] <- mnormt::biv.nt.prob(0, rep(-Inf, 2),
##                                        c(z[k], tau), c(0, 0), R)/p.tau
##          }
#        }
#      }
#    }
    p <- pmin(1, pmax(0, as.numeric(p)))
    names(prob) <- names(x)
    replace(prob, plain, p)
  }
  #quantile
  qskewn <- function (p, xi = 0, omega = 1, alpha = 0, tau = 0, dp = NULL,
                      tol = 0.00000001, solver = "NR", ...)
  {
    sn.cumulants <- function (xi = 0, omega = 1, alpha = 0, tau = 0, dp = NULL,
                              n = 4)
    {
      #zeta
      zeta <- function (k, x)
      {
        if (k < 0 | k > 5 | k != round(k))
          return(NULL)
        na <- is.na(x)
        x <- replace(x, na, 0)
        x2 <- x^2
        z <- switch(k + 1, pnorm(x, log.p = TRUE) + log(2), ifelse(x >
                    (-50), exp(dnorm(x, log = TRUE) - pnorm(x, log.p = TRUE)),
                     -x/(1 - 1/(x2 + 2) + 1/((x2 + 2) * (x2 + 4)) - 5/((x2 +
                      2) * (x2 + 4) * (x2 + 6)) + 9/((x2 + 2) * (x2 +
                       4) * (x2 + 6) * (x2 + 8)) - 129/((x2 + 2) * (x2 +
                        4) * (x2 + 6) * (x2 + 8) * (x2 + 10)))), (-zeta(1,
                         x) * (x + zeta(1, x))), (-zeta(2, x) * (x + zeta(1,
                          x)) - zeta(1, x) * (1 + zeta(2, x))), (-zeta(3, x) *
                             (x + 2 * zeta(1, x)) - 2 * zeta(2, x) * (1 + zeta(2,
                               x))), (-zeta(4, x) * (x + 2 * zeta(1, x)) - zeta(3,
                                x) * (3 + 4 * zeta(2, x)) - 2 * zeta(2, x) * zeta(3,
                                 x)), NULL)
        neg.inf <- (x == -Inf)
        if (any(neg.inf))
          z <- switch(k + 1, z, replace(z, neg.inf, Inf), replace(z,
                       neg.inf, -1), replace(z, neg.inf, 0), replace(z,
                        neg.inf, 0), replace(z, neg.inf, 0), NULL)
        if (k > 1)
          z <- replace(z, x == Inf, 0)
        replace(z, na, NA)
      }
      cumulants.half.norm <- function(n = 4) {
        n <- max(n, 2)
        n <- as.integer(2 * ceiling(n/2))
        half.n <- as.integer(n/2)
        m <- 0:(half.n - 1)
        a <- sqrt(2/pi)/(gamma(m + 1) * 2^m * (2 * m + 1))
        signs <- rep(c(1, -1), half.n)[seq_len(half.n)]
        a <- as.vector(rbind(signs * a, rep(0, half.n)))
        coeff <- rep(a[1], n)
        for (k in 2:n) {
          ind <- seq_len(k - 1)
          coeff[k] <- a[k] - sum(ind * coeff[ind] * a[rev(ind)]/k)
        }
        kappa <- coeff * gamma(seq_len(n) + 1)
        kappa[2] <- 1 + kappa[2]
        return(kappa)
      }
      if (!is.null(dp)) {
        if (!missing(alpha))
          stop("You cannot set both 'dp' and the component parameters")
        dp <- c(dp, 0)[1:4]
        dp <- matrix(dp, 1, ncol = length(dp))
      }
      else dp <- cbind(xi, omega, alpha, tau)
      delta <- ifelse(abs(dp[, 3]) < Inf, dp[, 3]/sqrt(1 + dp[,
                                                              3]^2), sign(dp[, 3]))
      tau <- dp[, 4]
      if (all(tau == 0)) {
        kv <- cumulants.half.norm(n)
        if (length(kv) > n)
          kv <- kv[-(n + 1)]
        kv[2] <- kv[2] - 1
        kappa <- outer(delta, 1:n, "^") * matrix(rep(kv, nrow(dp)),
                                                 ncol = n, byrow = TRUE)
      }
      else {
        if (n > 4) {
          warning("n>4 not allowed with ESN distribution")
          n <- min(n, 4)
        }
        kappa <- matrix(0, nrow = length(delta), ncol = 0)
        for (k in 1:n) kappa <- cbind(kappa, zeta(k, tau) *
                                        delta^k)
      }
      kappa[, 2] <- kappa[, 2] + 1
      kappa <- kappa * outer(dp[, 2], (1:n), "^")
      kappa[, 1] <- kappa[, 1] + dp[, 1]
      kappa[, , drop = TRUE]
    }
    #qskewn code starts below
    if (!is.null(dp)) {
      if (!missing(alpha))
        stop("You cannot set both 'dp' and component parameters")
      xi <- dp[1]
      omega <- dp[2]
      alpha <- dp[3]
      tau <- if (length(dp) > 3)
        dp[4]
      else 0
    }
    if (omega <= 0)
      stop("argument 'omega' (or dp[2]) must be positive")
    max.q <- sqrt(qchisq(p, 1)) + tau
    min.q <- -sqrt(qchisq(1 - p, 1)) + tau
    if (tau == 0) {
      if (alpha == Inf)
        return(xi + omega * max.q)
      if (alpha == -Inf)
        return(xi + omega * min.q)
    }
    na <- is.na(p) | (p < 0) | (p > 1)
    zero <- (p == 0)
    one <- (p == 1)
    ok <- !(na | zero | one)
    q.all <- numeric(length(p))
    names(q.all) <- names(p)
    q.all <- replace(q.all, na, NA)
    q.all <- replace(q.all, zero, -Inf)
    q.all <- replace(q.all, one, Inf)
    if (sum(ok) == 0)
      return(q.all)
    p <- p[ok]
    dp0 <- c(0, 1, alpha, tau)
    if (solver == "NR") {
      dp0 <- c(0, 1, alpha, tau)
      cum <- sn.cumulants(dp = dp0, n = 4)
      g1 <- cum[3]/cum[2]^(3/2)
      g2 <- cum[4]/cum[2]^2
      x <- qnorm(p)
      x <- (x + (x^2 - 1) * g1/6 + x * (x^2 - 3) * g2/24 -
              x * (2 * x^2 - 5) * g1^2/36)
      x <- cum[1] + sqrt(cum[2]) * x
      px <- pskewn(x, dp = dp0, ...)
      max.err <- 1
      while (max.err > tol) {
        x1 <- x - (px - p)/dskewn(x, dp = dp0)
        x <- x1
        px <- pskewn(x, dp = dp0, ...)
        max.err <- max(abs(px - p))
        if (is.na(max.err))
          stop("failed convergence, try with solver=\"RFB\"")
      }
      q <- as.numeric(xi + omega * x)
    }
    else {
      if (solver == "RFB") {
        abs.alpha <- abs(alpha)
        if (alpha < 0)
          p <- (1 - p)
        x <- xa <- xb <- xc <- fa <- fb <- fc <- rep(NA,
                                                     length(p))
        nc <- rep(TRUE, length(p))
        fc[!nc] <- 0
        xa[nc] <- qnorm(p[nc])
        xb[nc] <- sqrt(qchisq(p[nc], 1)) + abs(tau)
        fa[nc] <- pskewn(xa[nc], 0, 1, abs.alpha, tau, ...) -
          p[nc]
        fb[nc] <- pskewn(xb[nc], 0, 1, abs.alpha, tau, ...) -
          p[nc]
        regula.falsi <- FALSE
        while (sum(nc) > 0) {
          xc[nc] <- if (regula.falsi)
            xb[nc] - fb[nc] * (xb[nc] - xa[nc])/(fb[nc] -
                                                   fa[nc])
          else (xb[nc] + xa[nc])/2
          fc[nc] <- pskewn(xc[nc], 0, 1, abs.alpha, tau,
                        ...) - p[nc]
          pos <- (fc[nc] > 0)
          xa[nc][!pos] <- xc[nc][!pos]
          fa[nc][!pos] <- fc[nc][!pos]
          xb[nc][pos] <- xc[nc][pos]
          fb[nc][pos] <- fc[nc][pos]
          x[nc] <- xc[nc]
          nc[(abs(fc) < tol)] <- FALSE
          regula.falsi <- !regula.falsi
        }
        Sign <- function(x) sign(x) + as.numeric(x == 0)
        q <- as.numeric(xi + omega * Sign(alpha) * x)
      }
      else stop("unknown solver")
    }
    q.all[ok] <- q
    names(q.all) <- names(q)
    return(q.all)
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
               dskewn( xComb, xi=MC.Chain[chnIdx, Mean.Var], omega=MC.Chain[chnIdx, SD.Var],
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
               pt.bg=pcol_vector, cex = cex.legend, bty="n", inset=c(0, .05),
               lwd=cex.legend)
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
  fncPlotMcANOVA <- function( MCMC=NULL, datFrm=NULL , yName=NULL , xName=NULL,
                              MCmean=NULL, MCsigma=NULL, MCnu=NULL, Num.Lines=NULL,
                              Main.Title=NULL, X.Lab=NULL, Line.Color=NULL, cex=NULL,
                              cex.legend=NULL, cex.lab=NULL, cex.main=NULL, cex.axis=NULL,
                              X.Lim=NULL, Y.Lim=NULL, PCol = NULL, lwd=NULL, legend=NULL,
                              Leg.Loc=NULL, T.Percentage=NULL ) {
    #Make coda into as.matrix
    mcmcMat <- MCMC
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
    #Make x-label
    if (!is.null(X.Lab)) {
      X.Lab <- X.Lab
    } else {
      X.Lab <- ""
    }
    #Get generic mean parameter name to use for graphing
    mean_par <- strsplit(MCmean, "[", fixed=TRUE)[[1]][1]
    #Get generic sigma (SD) parameter name to use for graphing
    sigma_par <- strsplit(MCsigma, "[", fixed=TRUE)[[1]][1]
    # Display data with posterior predictive distributions
    plot(-1,0,
         xlim= X.Limits, xlab=X.Lab , xaxt="n" , ylab= yName ,
         ylim= Y.Limits, main=Main.Title, cex.axis=cex.axis,
         cex.lab=cex.lab, cex=cex, cex.main=cex.main )
    axis( 1 , at=1:length(xlevels) , tick=FALSE , labels=xlevels )
    for ( xidx in 1:length(xlevels) ) {
      xPlotVal = xidx
      yVals = y[ x == xidx ]
      points( rep(xPlotVal, length(yVals)) + runif(length(yVals), -0.05, 0.05) ,
              yVals , pch=1 , cex=cex , col= PCol ) #COLOR
      chainSub = round(seq(1, chainLength, length= Num.Lines)) #20
      for ( chnIdx in chainSub ) {
        m = mcmcMat[chnIdx, paste(mean_par, "[", xidx, "]", sep="")]
        s = mcmcMat[chnIdx, paste(sigma_par, "[", xidx,"]", sep="")]
        nu = mcmcMat[chnIdx, MCnu]
        #This controls tails of t distribution. Coverage "*.01" to get proportion
        tlim= qt( c((0.5 - (T.Percentage*0.01)/2), (0.5 + (T.Percentage*0.01)/2)) , df= nu )
        #This controls tails of t distribution
        yl = m + tlim[1]*s
        yh = m + tlim[2]*s
        ycomb=seq(yl, yh, length=501) ##201
        yt = dt( (ycomb - m) / s , df= nu )
        yt = 0.67 * yt / max(yt)           #This controls heighth of curve peaks
        lines( xPlotVal - yt , ycomb , col= Line.Color, lwd=lwd ) #COLOR
      }
    }
    #Add legend
    if(!is.null(Leg.Loc) ) {
      legend_text <- if (!is.null(legend)) legend else c(paste0("Observed ", yName), "Posterior Estimate")
      legend_type <- c(0, 1)
      pch_type <- c(1, -1)
      pcol_vector <- c(PCol, Line.Color)
      legend(Leg.Loc, legend=legend_text, col=pcol_vector, lwd=cex.legend,
             lty=legend_type, pt.bg=pcol_vector, cex= cex.legend, pch=pch_type,
             bty="n", inset=c(0, .05))
    }
  }

  ################################################################################
  #             5B. Posterior predictive check for ANOVA, single group           #
  ################################################################################
  fncPlotSingleT <- function( MCMC, datFrm=NULL , yName=NULL ,
                              MCmean=NULL, MCsigma=NULL, MCnu=NULL, Num.Lines=NULL,
                              Main.Title=NULL, X.Lab=NULL, Line.Color=NULL, cex=NULL,
                              cex.legend=NULL, cex.lab=NULL, cex.main=NULL, cex.axis=NULL,
                              X.Lim=NULL, Y.Lim=NULL, PCol = NULL, lwd=NULL, legend=NULL,
                              Leg.Loc=NULL, T.Percentage=NULL ) {
    #Make coda into as.matrix
    mcmcMat <- MCMC
    chainLength <- NROW( mcmcMat )
    y <- datFrm[, yName]
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
    #Make x-label
    if (!is.null(X.Lab)) {
      X.Lab <- X.Lab
    } else {
      X.Lab <- ""
    }
    #Get generic mean parameter name to use for graphing
    # Display data with posterior predictive distributions
    plot(-1,0,
         xlim= X.Limits, xlab=X.Lab , xaxt="n" , ylab= yName ,
         ylim= Y.Limits, main=Main.Title, cex.axis=cex.axis,
         cex.lab=cex.lab, cex=cex, cex.main=cex.main )
    for ( xidx in 1:1 ) {
      xPlotVal = xidx
      yVals = y
      points( rep(xPlotVal, length(yVals)) + runif(length(yVals), -0.05, 0.05) ,
              yVals , pch=1 , cex=cex , col= PCol ) #COLOR
      chainSub = round(seq(1, chainLength, length= Num.Lines)) #20
      for ( chnIdx in chainSub ) {
        m = mcmcMat[chnIdx, MCmean]
        s = mcmcMat[chnIdx, MCsigma]
        nu = mcmcMat[chnIdx, MCnu]
        #This controls tails of t distribution. Coverage "*.01" to get proportion
        tlim= qt( c((0.5 - (T.Percentage*0.01)/2), (0.5 + (T.Percentage*0.01)/2)) , df= nu )
        #This controls tails of t distribution
        yl = m + tlim[1]*s
        yh = m + tlim[2]*s
        ycomb=seq(yl, yh, length=501) ##201
        yt = dt( (ycomb - m) / s , df= nu )
        lines( xPlotVal - yt , ycomb , col= Line.Color, lwd=lwd ) #COLOR
      }
    }
    #Add legend
    if(!is.null(Leg.Loc) ) {
      legend_text <- if (!is.null(legend)) legend else c(paste0("Observed ", yName), "Posterior Estimate")
      legend_type <- c(0, 1)
      pch_type <- c(1, -1)
      pcol_vector <- c(PCol, Line.Color)
      legend(Leg.Loc, legend=legend_text, col=pcol_vector, lwd=cex.legend,
             lty=legend_type, pt.bg=pcol_vector, cex = cex.legend, pch=pch_type,
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
          PsnormGtY[[i]] <- summarizePost( 1 - pskewn(x=yVal[i], xi= MC.Matrix[, Center], omega= MC.Matrix[, Spread],
                                                   alpha= MC.Matrix[, Skew], lower.tail=FALSE) )[c(c("Mode","Median","Mean")[which(c("Mode","Median","Mean") == CenTend)], "HDIlow", "HDIhigh")]
          names(PsnormGtY)[i] <- paste0("Y_", yVal[i])
        }
      }
    }
    # Quantiles of Y.
    # Needs mapply for qskewn() b/c it creates an impossible error for "omega" <= 0.
    QsnormGtY <- list()
    if(!is.null(qVal)) {
      if(Distribution == "Skew-normal") {
        for (i in 1:length(qVal)) {
#          QsnormGtY[[i]] <- summarizePost(mapply(qsn, p=qVal[i], xi=MC.Matrix[, Center], omega=MC.Matrix[, Spread],
          QsnormGtY[[i]] <- summarizePost(mapply(qskewn, p=qVal[i], xi=MC.Matrix[, Center], omega=MC.Matrix[, Spread],
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
  fncBayesOlsPrtPred <- function(MCMC=NULL , datFrm=NULL,  Reg.Type=NULL,
                                 Outcome=NULL , Group=NULL,
                                 Group.Level=NULL, xName=NULL, parX=NULL, View.Lines=NULL,
                                 Num.Lines=NULL, Main.Title=NULL, X.Lab=NULL, Y.Lab=NULL,
                                 Line.Color=NULL, cex.lab = cex.lab, cex = cex,
                                 cex.main=cex.main, cex.axis=cex.axis, cex.legend=cex.legend,
                                 X.Lim=NULL, Y.Lim=NULL, X.Min=NULL, X.Max=NULL,
                                 PCol=NULL, Leg.Loc=NULL) {
    y = datFrm[complete.cases(datFrm), Outcome]
    x = datFrm[complete.cases(datFrm), xName, drop=FALSE][1]
    if(!is.null(Group)) {
      s = factor(datFrm[, Group])
      nSubj = length(unique(s)) # should be same as max(s)
    }
    #Make coda into as.matrix
    mcmcMat <- MCMC
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
    #X.Min and X.Max
    if (!is.null(X.Min)) {
      X.Min <- X.Min
    } else {
      X.Min <- min(data[, xName[1]], na.rm=TRUE)
    }
    if (!is.null(X.Max)) {
      X.Max <- X.Max
    } else {
      X.Max <- max(data[, xName[1]], na.rm=TRUE)
    }
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
    if (Reg.Type %in% c("ol", "oq", "oc", 'lnl', 'lnq', 'lnc', "logl", "logq", "logc") ) {
      ttfrm[1, 2] <- "xComb"
    }
    #This will change the mean value to the xComb^2 for quadratic models
    if (Reg.Type %in% c("oq", "oc", 'lnq', 'lnc', "logq", "logc")) {
      ttfrm[2, 2] <- "xComb^2"
    }
    #This will change the mean value to the xComb^3 for cubic models
    if (Reg.Type %in% c("oc", 'lnc', "logc")) {
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
    if (View.Lines == "a") {
      line_type <-  "p"
    }
    if (View.Lines == "al") {
      line_type <-  "o"
    }
    if (View.Lines == "u") {
      line_type <-  "p"
    }
    if (View.Lines == "ul") {
      line_type <-  "o"
    }
    #X and Y labels
    if (!is.null(X.Lab)) {
      X.Lab <- X.Lab
    } else {
      X.Lab <- xName[1]
    }
    if (!is.null(Y.Lab)) {
      Y.Lab <- Y.Lab
    } else {
      Y.Lab <- dv
    }

    ##################
    ## Create plots ##
    ##################
    plot( unlist(x) , y , pch="" , cex=cex ,
          type= "n", #col="black" ,
          xlim=X.Lim, ylim=Y.Lim,xlab=X.Lab , ylab=Y.Lab ,
          main= Main.Title, cex.lab=cex.lab, cex.main=cex.main, cex.axis=cex.axis )
    ###################
    ## Observed Data ##
    ###################
    #With groups: All groups added at once for the overall term
    if (Reg.Type %in% c("ol", "oq", "oc","logl","logq","logc","lnl","lnq","lnc") ) {
      if (View.Lines %in% c('a','al')) {
        if(!is.null(Group)) {
          for ( sIdx in 1:nSubj ) {
            thisSrows = (as.numeric(s)==sIdx)
            lines( x[thisSrows, ] , y[thisSrows] , type=line_type , pch=19, col= PCol, cex=cex, lwd=lwd)
          }
        }
      }
    }
    #Units only
    #With groups: Specific unit's observed data
    if (Reg.Type %in% c("ol", "oq", "oc","logl","logq","logc","lnl","lnq","lnc") ) {
      if (View.Lines %in% c('u', 'ul')) {
        if(!is.null(Group)) {
            thisSrows = (as.numeric(s)== which(levels(s) == Group.Level) )
            lines( x[thisSrows, ] , y[thisSrows] , type=line_type , pch=19, col= PCol, cex=cex, lwd=lwd)
        }
      }
    }
    #No groups: All groups added at once for the overall term
    if (Reg.Type %in% c("ol", "oq", "oc","logl","logq","logc","lnl","lnq","lnc") ) {
      if (View.Lines %in% c('a', 'al')) {
        if(is.null(Group)) {
          lines( unlist(x), y, type=line_type , pch=19, col= PCol, cex=cex, lwd=lwd)
        }
      }
    }
    ####################
    ## Posterior line ##
    ####################
    # Superimpose a smattering of believable regression lines:
    #For each line (e.g., 30), it cycles the 301 X-comb values to create 301 Ys for plot
    #This plots out random regression lines
    if (Reg.Type %in% c("ol", "oq", "oc") ) {
      for ( i in 1:length(floor(seq(1, nrow(mcmcMat), length = Num.Lines))) ) {
        lines( xComb , eval(parse(text= ttnew2)) , col= Line.Color, lwd= lwd  )
      }
    }
    #Log-normal regression probabilities
    if (Reg.Type %in% c("lnl", "lnq", "lnc") ) {
      for ( i in 1:length(floor(seq(1, nrow(mcmcMat), length = Num.Lines))) ) {
        lines( xComb , exp(eval(parse(text= ttnew2))) , col= Line.Color, lwd= lwd )
      }
    }
    #Logistic regression probabilities
    if (Reg.Type %in% c("logl", "logq", "logc") ) {
      for ( i in 1:length(floor(seq(1, nrow(mcmcMat), length = Num.Lines))) ) {
        lines( xComb , (1/(1+ exp(-( (eval(parse(text= ttnew2))) )))) , col= Line.Color, lwd= lwd)
      }
    }
    #Determine which observed datFrm lines to view
    if (View.Lines %in% c("u", "ul")) {
      if(!is.null(Group)) {
        #For specific groups
        for ( sIdx in 1:nSubj ) {
          thisSrows = (as.numeric(s)== which(levels(s) == Group.Level) )
          lines( x[thisSrows, ] , y[thisSrows] , type= line_type, pch=19, col= PCol, cex=cex, lwd=lwd )
        }
      }
    }
    #Legend color
    if (View.Lines== "n") {
      pcol_vector <- c(Line.Color)
    }
    if (View.Lines %in% c("u", "ul")) {
      pcol_vector <- c(PCol, Line.Color)
    }
    if (View.Lines %in% c("a", "al")) {
      pcol_vector <- c(PCol, Line.Color)
    }
    #Legend text
    if (Reg.Type %in% c("ol", "oq", "oc", "logl", "logq", "logc") ) {
      if (View.Lines== "n") {
          legend_text <- if (!is.null(legend)) legend else "Posterior Estimate"
      }
    }
    if (View.Lines %in% c("u", "ul") ) {
      legend_text <- if (!is.null(legend)) legend else c(paste0("Observed ", abbreviate(Group, 8), ": ",
                                                                abbreviate(Group.Level, 8)), "Posterior Estimate")
    }
    # for no groups
    if (View.Lines %in% c("a", "al")) {
      legend_text <- if (!is.null(legend)) legend else c("Observed: All", "Posterior Estimate")
    }
    #Legend points
    if (View.Lines== "n") {
      legend_points <- NULL
    }
    if (View.Lines %in% c("u", "ul")) {
      legend_points <- c(19, NA)
    }
    if (View.Lines %in% c("a", "al")) {
      legend_points <- c(19, NA)
    }
    #Add legend
    if(!is.null(Leg.Loc) ) {
      if (View.Lines %in% c("al","ul")) {
      legend_type <- c(1,1)
      }
      if (View.Lines %in% c("a","u")) {
        legend_type <- c(1, NA)
      }
      if (View.Lines %in% c("n")) {
        legend_type <- 1
      }
      legend(Leg.Loc, legend=c(legend_text[2], legend_text[1]),
             col= c(pcol_vector[2], pcol_vector[1]),
             pch= c(legend_points[2], legend_points[1]),
             lty= legend_type, pt.bg=pcol_vector, cex = cex.legend,
             lwd=cex.legend, bty="n",  inset=c(0, .05))
    }
  }

  ################################################################################

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
  switch(type,
         "n" = fncGrpPostPredCheck(MCMC=MCMC, datFrm=data, Outcome=dv, Group=group[[1]],
                                   Group.Level=group[[2]], Mean.Var=parameter[1], SD.Var=parameter[2],
                                   MCnu=NULL, Distribution=type, Num.Lines=pline,
                                   Main.Title=main, X.Lab=xlab, Bar.Color=bcol,
                                   Line.Color=lcol, Hist.Breaks=breaks, X.Lim=xlim, Y.Lim=ylim,  Min.Val=vlim[1],
                                   Max.Val=vlim[2], Round.Digits=round.c, Point.Loc= xpt, PCol=pcol,
                                   Leg.Loc= add.legend, cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                                   cex.axis=cex.axis, legend=legend, cex.legend=cex.legend, lwd=lwd, Y.Lab=ylab ),
         "ln" = fncGrpPostPredCheck(MCMC=MCMC, datFrm=data, Outcome=dv, Group=group[[1]],
                                   Group.Level=group[[2]], Mean.Var=parameter[1], SD.Var=parameter[2],
                                   MCnu=NULL, Distribution=type, Num.Lines=pline,
                                   Main.Title=main, X.Lab=xlab, Bar.Color=bcol,
                                   Line.Color=lcol, Hist.Breaks=breaks, X.Lim=xlim, Y.Lim=ylim,  Min.Val=vlim[1],
                                   Max.Val=vlim[2], Round.Digits=round.c, Point.Loc= xpt, PCol=pcol,
                                   Leg.Loc= add.legend, cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                                   cex.axis=cex.axis, legend=legend, cex.legend=cex.legend, lwd=lwd, Y.Lab=ylab ),
         "sn" = fncGrpPostPredCheck(MCMC=MCMC, datFrm=data, Outcome=dv, Group=group[[1]],
                                    Group.Level=group[[2]], Mean.Var=parameter[1], SD.Var=parameter[2],
                                    MCnu=parameter[3], Distribution=type, Num.Lines=pline,
                                    Main.Title=main, X.Lab=xlab, Bar.Color=bcol,
                                    Line.Color=lcol, Hist.Breaks=breaks, X.Lim=xlim, Y.Lim=ylim,  Min.Val=vlim[1],
                                    Max.Val=vlim[2], Round.Digits=round.c, Point.Loc= xpt, PCol=pcol,
                                    Leg.Loc= add.legend, cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                                    cex.axis=cex.axis, legend=legend, cex.legend=cex.legend, lwd=lwd, Y.Lab=ylab ),
         "w" = fncGrpPostPredCheck(MCMC=MCMC, datFrm=data, Outcome=dv, Group=group[[1]],
                                    Group.Level=group[[2]], Mean.Var=parameter[1], SD.Var=parameter[2],
                                    MCnu=parameter[3], Distribution=type, Num.Lines=pline,
                                    Main.Title=main, X.Lab=xlab, Bar.Color=bcol,
                                    Line.Color=lcol, Hist.Breaks=breaks, X.Lim=xlim, Y.Lim=ylim,  Min.Val=vlim[1],
                                    Max.Val=vlim[2], Round.Digits=round.c, Point.Loc= xpt, PCol=pcol,
                                    Leg.Loc= add.legend, cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                                    cex.axis=cex.axis, legend=legend, cex.legend=cex.legend, lwd=lwd, Y.Lab=ylab ),
         "g" = fncGrpPostPredCheck(MCMC=MCMC, datFrm=data, Outcome=dv, Group=group[[1]],
                                    Group.Level=group[[2]], Mean.Var=parameter[1], SD.Var=parameter[2],
                                    MCnu=NULL, Distribution=type, Num.Lines=pline,
                                    Main.Title=main, X.Lab=xlab, Bar.Color=bcol,
                                    Line.Color=lcol, Hist.Breaks=breaks, X.Lim=xlim, Y.Lim=ylim,  Min.Val=vlim[1],
                                    Max.Val=vlim[2], Round.Digits=round.c, Point.Loc= xpt, PCol=pcol,
                                    Leg.Loc= add.legend, cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                                    cex.axis=cex.axis, legend=legend, cex.legend=cex.legend, lwd=lwd, Y.Lab=ylab ),
         "t" = fncGrpPostPredCheck(MCMC=MCMC, datFrm=data, Outcome=dv, Group=group[[1]],
                                    Group.Level=group[[2]], Mean.Var=parameter[1], SD.Var=parameter[2],
                                    MCnu=parameter[3], Distribution=type, Num.Lines=pline,
                                    Main.Title=main, X.Lab=xlab, Bar.Color=bcol,
                                    Line.Color=lcol, Hist.Breaks=breaks, X.Lim=xlim, Y.Lim=ylim,  Min.Val=vlim[1],
                                    Max.Val=vlim[2], Round.Digits=round.c, Point.Loc= xpt, PCol=pcol,
                                    Leg.Loc= add.legend, cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                                    cex.axis=cex.axis, legend=legend, cex.legend=cex.legend, lwd=lwd, Y.Lab=ylab ),
         "taov" = fncPlotMcANOVA(MCMC=MCMC, datFrm=data, yName=dv, xName=iv[1],
                                  MCmean=parameter[1], MCsigma=parameter[2], MCnu=parameter[3],
                                  Num.Lines=pline, Main.Title=main, X.Lab=xlab,
                                  Line.Color=lcol, X.Lim=xlim, Y.Lim=ylim,  T.Percentage=pct,
                                  PCol=pcol, Leg.Loc= add.legend, cex= cex, cex.legend=cex.legend,
                                  cex.axis=cex.axis, cex.lab= cex.lab, cex.main=cex.main,
                                  lwd=lwd, legend=legend),
         "taov1" = fncPlotSingleT(MCMC=MCMC, datFrm=data, yName=dv,
                                  MCmean=parameter[1], MCsigma=parameter[2], MCnu=parameter[3],
                                  Num.Lines=pline, Main.Title=main, X.Lab=xlab,
                                  Line.Color=lcol, X.Lim=xlim, Y.Lim=ylim,  T.Percentage=pct,
                                  PCol=pcol, Leg.Loc= add.legend, cex= cex, cex.legend=cex.legend,
                                  cex.axis=cex.axis, cex.lab= cex.lab, cex.main=cex.main,
                                  lwd=lwd, legend=legend),
         "ol" = fncBayesOlsPrtPred(MCMC=MCMC, datFrm=data, Reg.Type=type,
                                   Outcome=dv , Group=group[[1]], Group.Level=group[[2]],
                                   xName=iv, parX=parameter, View.Lines=add.data, Num.Lines=pline,
                                   Main.Title=main, X.Lab=xlab, Y.Lab=ylab, Line.Color=lcol,
                                   cex.lab= cex.lab, cex= cex, cex.main=cex.main, cex.axis=cex.axis,
                                   cex.legend=cex.legend, X.Lim=xlim, Y.Lim=ylim,
                                   X.Min=vlim[1], X.Max=vlim[2], PCol=pcol, Leg.Loc=add.legend),
         "oq" = fncBayesOlsPrtPred(MCMC=MCMC, datFrm=data, Reg.Type=type,
                                   Outcome=dv , Group=group[[1]], Group.Level=group[[2]],
                                   xName=iv, parX=parameter, View.Lines=add.data, Num.Lines=pline,
                                   Main.Title=main, X.Lab=xlab, Y.Lab=ylab, Line.Color=lcol,
                                   cex.lab= cex.lab, cex= cex, cex.main=cex.main, cex.axis=cex.axis,
                                   cex.legend=cex.legend, X.Lim=xlim, Y.Lim=ylim,
                                   X.Min=vlim[1], X.Max=vlim[2], PCol=pcol, Leg.Loc=add.legend),
         "oc" = fncBayesOlsPrtPred(MCMC=MCMC, datFrm=data, Reg.Type=type,
                                   Outcome=dv , Group=group[[1]], Group.Level=group[[2]],
                                   xName=iv, parX=parameter, View.Lines=add.data, Num.Lines=pline,
                                   Main.Title=main, X.Lab=xlab, Y.Lab=ylab, Line.Color=lcol,
                                   cex.lab= cex.lab, cex= cex, cex.main=cex.main, cex.axis=cex.axis,
                                   cex.legend=cex.legend, X.Lim=xlim, Y.Lim=ylim,
                                   X.Min=vlim[1], X.Max=vlim[2], PCol=pcol, Leg.Loc=add.legend),
         "lnl" = fncBayesOlsPrtPred(MCMC=MCMC, datFrm=data, Reg.Type=type,
                                    Outcome=dv , Group=group[[1]], Group.Level=group[[2]],
                                    xName=iv, parX=parameter, View.Lines=add.data, Num.Lines=pline,
                                    Main.Title=main, X.Lab=xlab, Y.Lab=ylab, Line.Color=lcol,
                                    cex.lab= cex.lab, cex= cex, cex.main=cex.main, cex.axis=cex.axis,
                                    cex.legend=cex.legend, X.Lim=xlim, Y.Lim=ylim,
                                    X.Min=vlim[1], X.Max=vlim[2], PCol=pcol, Leg.Loc=add.legend),
         "lnq" = fncBayesOlsPrtPred(MCMC=MCMC, datFrm=data, Reg.Type=type,
                                    Outcome=dv , Group=group[[1]], Group.Level=group[[2]],
                                    xName=iv, parX=parameter, View.Lines=add.data, Num.Lines=pline,
                                    Main.Title=main, X.Lab=xlab, Y.Lab=ylab, Line.Color=lcol,
                                    cex.lab= cex.lab, cex= cex, cex.main=cex.main, cex.axis=cex.axis,
                                    cex.legend=cex.legend, X.Lim=xlim, Y.Lim=ylim,
                                    X.Min=vlim[1], X.Max=vlim[2], PCol=pcol, Leg.Loc=add.legend),
         "lnc" = fncBayesOlsPrtPred(MCMC=MCMC, datFrm=data, Reg.Type=type,
                                    Outcome=dv , Group=group[[1]], Group.Level=group[[2]],
                                    xName=iv, parX=parameter, View.Lines=add.data, Num.Lines=pline,
                                    Main.Title=main, X.Lab=xlab, Y.Lab=ylab, Line.Color=lcol,
                                    cex.lab= cex.lab, cex= cex, cex.main=cex.main, cex.axis=cex.axis,
                                    cex.legend=cex.legend, X.Lim=xlim, Y.Lim=ylim,
                                    X.Min=vlim[1], X.Max=vlim[2], PCol=pcol, Leg.Loc=add.legend),
         "logl" = fncBayesOlsPrtPred(MCMC=MCMC, datFrm=data, Reg.Type=type,
                                    Outcome=dv , Group=group[[1]], Group.Level=group[[2]],
                                    xName=iv, parX=parameter, View.Lines=add.data, Num.Lines=pline,
                                    Main.Title=main, X.Lab=xlab, Y.Lab=ylab, Line.Color=lcol,
                                    cex.lab= cex.lab, cex= cex, cex.main=cex.main, cex.axis=cex.axis,
                                    cex.legend=cex.legend, X.Lim=xlim, Y.Lim=ylim,
                                    X.Min=vlim[1], X.Max=vlim[2], PCol=pcol, Leg.Loc=add.legend),
         "logq" = fncBayesOlsPrtPred(MCMC=MCMC, datFrm=data, Reg.Type=type,
                                    Outcome=dv , Group=group[[1]], Group.Level=group[[2]],
                                    xName=iv, parX=parameter, View.Lines=add.data, Num.Lines=pline,
                                    Main.Title=main, X.Lab=xlab, Y.Lab=ylab, Line.Color=lcol,
                                    cex.lab= cex.lab, cex= cex, cex.main=cex.main, cex.axis=cex.axis,
                                    cex.legend=cex.legend, X.Lim=xlim, Y.Lim=ylim,
                                    X.Min=vlim[1], X.Max=vlim[2], PCol=pcol, Leg.Loc=add.legend),
         "logc" = fncBayesOlsPrtPred(MCMC=MCMC, datFrm=data, Reg.Type=type,
                                    Outcome=dv , Group=group[[1]], Group.Level=group[[2]],
                                    xName=iv, parX=parameter, View.Lines=add.data, Num.Lines=pline,
                                    Main.Title=main, X.Lab=xlab, Y.Lab=ylab, Line.Color=lcol,
                                    cex.lab= cex.lab, cex= cex, cex.main=cex.main, cex.axis=cex.axis,
                                    cex.legend=cex.legend, X.Lim=xlim, Y.Lim=ylim,
                                    X.Min=vlim[1], X.Max=vlim[2], PCol=pcol, Leg.Loc=add.legend)
      )
    }
## Multilevel summary ##
    if(y == "multi") {
    switch(level,
           "1"= fncHdiBinP(multi_smry=multi_smry, View.Order=aorder, View.Level= "1",
                         GroupX=subset, Lcol=lcol, Pcol=pcol,
                         tgt=tgt, tgt.col=tgtcol, plyCol=bcol, roundVal=round.c,
                         XLim1=xlim[1], XLim2=xlim[2], legend=legend, Leg.Loc=add.legend,
                         lwd=lwd, cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                         cex.axis=cex.axis, cex.legend=cex.legend, X.Lab=xlab),
           "2"= fncHdiBinP(multi_smry=multi_smry, View.Order=aorder, View.Level="2",
                         GroupX=subset, Lcol=lcol, Pcol=pcol,
                         tgt=tgt, tgt.col=tgtcol, plyCol=bcol, roundVal=round.c,
                         XLim1=xlim[1], XLim2=xlim[2], legend=legend, Leg.Loc=add.legend,
                         lwd=lwd, cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                         cex.axis=cex.axis, cex.legend=cex.legend, X.Lab=xlab),
           "3"= fncHdiBinP(multi_smry=multi_smry, View.Order=aorder, View.Level="3",
                         GroupX=subset, Lcol=lcol, Pcol=pcol,
                         tgt=tgt, tgt.col=tgtcol, plyCol=bcol, roundVal=round.c,
                         XLim1=xlim[1], XLim2=xlim[2], legend=legend, Leg.Loc=add.legend,
                         lwd=lwd, cex.lab= cex.lab, cex= cex, cex.main=cex.main,
                         cex.axis=cex.axis, cex.legend=cex.legend, X.Lab=xlab)
           )
  }


} #end of plot.bayes

