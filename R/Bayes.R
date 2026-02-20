#' Summarize Bayesian Markov Chain Monte Carlo (MCMC) object
#'
#' Convert a list of Bayesian analysis chains (e.g., coda package mcmc.list objects) into a data frame
#' for analysis and creating plots. Calculates a set of descriptive statistics that summarize
#' MCMC parameters. MCMC converted to data frame and summary values are also returned as a data frame.
#'
#' @param x list object of MCMC chains (e.g, mcmc.list).
#' @param parameter single character vector name of parameter in MCMC chains to produce summary statistics. Default is NULL.
#' @param mass numeric vector the specifies the credible mass used in the Highest Density Interval (HDI). Default is 0.95.
#' @param compare numeric vector with one comparison value to determine how much of the distribution is above or below
#' the comparison value. Default is NULL.
#' @param rope numeric vector with two values that define the Region of Practical Equivalence (ROPE).
#' Test hypotheses by setting low and high values to determine if the Highest Density Interval (HDI)
#' is within or without the ROPE. Parameter value declared not credible if the entire ROPE lies
#' outside the HDI of the parameter’s posterior (i.e., we reject the null hypothesis). For example,
#' the ROPE of a coin is set to 0.45 to 0.55 but the posterior 95% HDI is 0.61 - 0.69 so we reject
#' the null hypothesis value of 0.50. We can accept the null hypothesis if the entire 95% HDI falls with the ROPE. Default is NULL.
#' @param newdata optional logical vector that indicates if you want the new MCMC data returned. When newdata=TRUE,
#' it will return the list object of MCMC chains, converted into a data frame. This new data can
#' be used for analysis or plots. The default is newdata=FALSE.
#'
#' @return data frame of summary statistics for MCMC parameter's distribution and/or MCMC data frame.
#' Statistics include highest density interval, effective sample size, proportion of distribution
#' within and outside of a ROPE, distribution compared with a set value, and the parameter's mean,
#' median, and mode. And a MCMC data frame.
#' @importFrom stats sd ar density median residuals var
#' @export
#' @references
#' Kruschke, J. (2014). Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and
#' Stan, Second Edition. New York: Academic Press. ISBN: 9780124058880

#' @examples
#' ## Hospital LOS and readmissions ##
#' # X-bar chart statistics

Bayes <- function(x, parameter=NULL, mass=NULL, compare=NULL,
                    rope=NULL, newdata=FALSE) {
  #Looking for a list
  if (any(class(x) %in% c("list", "mcmc.list")) == FALSE) {stop("Error: Expecting list class object." )}
  #Looking for 1 parameter name
    if(!is.null(parameter)) {
      if(length(parameter) != 1 ) {
        stop("Error: Expecting parameter length equal to 1.")
    }
    }
  #Looking for 1 credible mass value within 0 and 1
  if(length(mass) > 1 ) {
    stop("Error: Expecting only 1 mass value.")
  }
  if(!is.null(mass)) {
  proper_mass <- ifelse(mass > 0 & mass < 1, TRUE, FALSE)
  }
  if(!is.null(mass)) {
  if(proper_mass == FALSE) {
    stop("Error: Expecting mass value 0 > mass > 1.")
  }
  }
  #Looking for 1 comparison value
  if(length(compare) > 1 ) {
    stop("Error: Expecting only 1 compare value.")
  }
  #Looking for 2 ROPE values
  if(!is.null(rope) ) {
    if(length(rope) != 2 ) {
    stop("Error: Expecting 2 rope values.")
  }
  }

################################################################################
#                 Function to convert coda to data frame                       #
################################################################################

fncMCMC <- function(x) {
  #Get the number of chains, rows, and columns per chain
  n_chains <- length(x)
  n_rows <- dim(x[[1]])[1]
  n_cols <- dim(x[[1]])[2]
  #Convert coda object to data.frame
  mcmc <- data.frame(do.call(rbind, x)) #drop data.frame if matrix/array needed
  #Create CHAIN variable
  mcmc$CHAIN <- rep(1:n_chains, each=n_rows)
  #Re-order so CHAIN is in the 1st spot
  mcmc <- mcmc[, c((n_cols+1), 1:n_cols)]
  return(mcmc)
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

#############################
## Posterior distributions ##
#############################
sumPost <- function( paramSampleVec , compVal=NULL, ROPE=NULL, credMass=0.95 ) {
  # Override defaults of hist function, if not specified by user:
  # (additional arguments "..." are passed to the hist function)
#  if ( is.null(xlab) ) xlab="Param. Val."
  xlab <- parameter

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

  # comparison value.
  if ( !is.null( compVal ) ) {
    pGtCompVal = sum( paramSampleVec > compVal ) / length( paramSampleVec )
    pLtCompVal = 1 - pGtCompVal
    postSummary[,"compVal"] = compVal
    postSummary[,"pGtCompVal"] = pGtCompVal
  }
  # ROPE.
  if ( !is.null( ROPE ) ) {
    pInROPE = ( sum( paramSampleVec > ROPE[1] & paramSampleVec < ROPE[2] )
                / length( paramSampleVec ) )
    pGtROPE = ( sum( paramSampleVec >= ROPE[2] ) / length( paramSampleVec ) )
    pLtROPE = ( sum( paramSampleVec <= ROPE[1] ) / length( paramSampleVec ) )
    postSummary[,"ROPElow"]=ROPE[1]
    postSummary[,"ROPEhigh"]=ROPE[2]
    postSummary[,"pLtROPE"]=pLtROPE
    postSummary[,"pInROPE"]=pInROPE
    postSummary[,"pGtROPE"]=pGtROPE
  }
  return( postSummary )
}

################################################################################
#                     Modified effective sample size                           #
################################################################################
fncESS <- function (x)  {
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

  #################
  ## Create MCMC ##
  #################
  MCMC <- fncMCMC(x)

  ######################
  # Reset object names #
  ######################
  paramSampleVec <- MCMC[, parameter]
  compVal <- compare
  ROPE <- rope
  credMass <- mass

  #################
  ## Run objects ##
  #################
  #posterior
  if(!is.null(parameter)) {
    Posterior.Summary <- data.frame(sumPost( paramSampleVec=paramSampleVec ,
                                  compVal=compVal, ROPE=ROPE, credMass=credMass ))
  } else {
    Posterior.Summary <- NA
  }
  #Final output
  if(newdata == FALSE) {
    MCMC <- NA
  }
  #Combine in list
  z <- list(Posterior.Summary=Posterior.Summary, MCMC=MCMC)
  # Assign ham classes
  class(z) <- c("bayes","ham", "list")
  return(z)
} # End of Bayesian section #

