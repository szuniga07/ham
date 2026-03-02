#' Summarize Bayesian Markov Chain Monte Carlo (MCMC) object
#'
#' Convert a list of Bayesian analysis chains (e.g., coda package mcmc.list objects) into a data frame
#' for analysis and creating plots. Calculates a set of descriptive statistics that summarize
#' MCMC parameters. MCMC converted to data frame and summary values are also returned as a data frame.
#'
#' @param x list object of MCMC chains (e.g, mcmc.list).
#' @param posterior logical value that indicates whether to provide a summary of a posterior estimate. Default is FALSE.
#' @param parameter single or multiple element character vector name of parameter in MCMC chains to produce summary statistics. Default is NULL.
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
#' @param multi logical vector for a multilevel/hierarchical summary that can be used in a plot too. It provides a multilevel
#' or hierarchical model summary (up to 3 levels). Default is FALSE.
#' @param type character vector of length == 1 that indicates the likelihood function used in the model when multi=TRUE.
#' Select 'n', 'ln', 'sn', 'w', 'g', 't', 'bern', and 'bin' for these respective options in Bayesian estimation (multilevel):
#' 'Normal', 'Log-normal', 'Skew-normal', 'Weibull', 'Gamma', 't', 'Bernoulli', or 'binomial'. Default is NULL.
#' @param center character vector that selects the type of central tendency to use when reporting parameter values.
#' Choices include: 'mean', 'median', and 'mode'. Default is 'mode'.
#' @param data object name for the observed data when multi=TRUE. Default is NULL.
#' @param dv character vector of length == 1 for the dependent variable name in the observed data frame
#' when multi=TRUE. Default is NULL.
#' @param iv character vector of length >= 1 for the independent variable name(s) in the observed data frame
#' when y='check' or y='multi'. When y='multi', enter the lower to higher level clustering or group names (e.g, for
#' health data, iv=c("patient", "hospital"). When type='taov', enter the name of the test group variable.  Default is NULL.
#' @param expand a character vector of length == 1 indicating the variable name to expand aggregated data into non-aggregated
#' data frames when  multi=TRUE. This variable is the denominator that can be used to calculate a rate in the formula
#' numerator/denominator. For example, when the 'numerator' column is 4 and the 'denominator' column is 10, then this single row
#' of data is expanded to 10 rows with four values of 1 and six values of 0 when expand='denominator'. Default is NULL.
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

Bayes <- function(x, posterior=FALSE,parameter=NULL, mass=.95, compare=NULL,
                    rope=NULL, newdata=FALSE, multi=FALSE, type=NULL, center="Mode",
                  data=NULL, dv=NULL, iv=NULL, expand=NULL) {
  #Looking for a list
  if (any(class(x) %in% c("list", "mcmc.list")) == FALSE) {stop("Error: Expecting list class object." )}
  #Looking for 1 parameter name
  if(!is.null(parameter)) {
    if(posterior ==TRUE ) {
      if(length(parameter) != 1 ) {
      stop("Error: Expecting parameter length equal to 1.")
    }
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
  #Get the column names, number of chains, rows, and columns per chain
  df_nms <- colnames(as.matrix(x[[1]]))
  n_chains <- length(x)
  n_rows <- dim(x[[1]])[1]
  n_cols <- dim(x[[1]])[2]
  #Convert coda object to data.frame
  mcmc <- data.frame(do.call(rbind, x)) #drop data.frame if matrix/array needed
  #Create CHAIN variable
  mcmc$CHAIN <- rep(1:n_chains, each=n_rows)
  #Re-order so CHAIN is in the 1st spot
  mcmc <- mcmc[, c((n_cols+1), 1:n_cols)]
  #Put original column names back in
  colnames(mcmc)[-1] <- df_nms
  return(mcmc)
}

#################
## Create MCMC ##
#################
MCMC <- fncMCMC(x)

######################
# Reset object names #
######################
if(posterior == TRUE) {
paramSampleVec <- MCMC[, parameter]
}
compVal <- compare
ROPE <- rope
credMass <- mass

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
################################################################################
#                  1. Expand aggregated data into  full data                   #
################################################################################
#This function expands y ~ x1 + X2 BINARY aggregated data in multi-row data
#X1= lower level hierarchy (e.g., patients), X2= higher level hierarchy (e.g., States)
#Z= Outcome, N= Total count of denominator (e.g., Z/N= rate)
fncExpAgr <- function(DF, X1, X2=NULL, Z, N, Level) {
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
fncHdiBinSmry <- function(MCmatrix, expand=NULL, datFrm, Outcome, Group2, Group3=NULL,
                          Theta=NULL, Omega2=NULL, Omega3=NULL, Average_type=NULL,
                          Distribution=NULL, Cred.Mass=0.95 ) {
  ###################################################################
  #Get the level of the model
  Level <- length(c(Theta, Omega2, Omega3))

  ## These next 30 lines will exclude irrelevant parameters ##
  keep_theta_cols <- NULL
  keep_omega2_cols <- NULL
  keep_omega3_cols <- NULL
  #Level-1, non-hierarchical model
  if(Level== 1) {
    keep_theta_cols <- grep(paste0(Theta, "\\["), colnames(MCmatrix))
  }
  #Level-2, hierarchical model
  if(Level== 2) {
    keep_theta_cols <- grep(paste0(Theta, "\\["), colnames(MCmatrix))
  }
  if(Level== 2) {
    keep_omega2_cols <- grep(paste0(Omega2, "\\["), colnames(MCmatrix))
  }
  #Level-3, hierarchical model
  if(Level== 3) {
    keep_theta_cols <- grep(paste0(Theta, "\\["), colnames(MCmatrix))
  }
  if(Level== 3) {
    #    keep_omega3_cols <- grep(paste0(Omega3, "\\["), colnames(MCmatrix))
    keep_omega3_cols <- grep(Omega3, colnames(MCmatrix))
  }
  if(Level== 3) {
    keep_omega2_cols <- setdiff(grep(paste0(Omega2, "\\["), colnames(MCmatrix)), keep_omega3_cols)
  }
  #These are just the columns I need
  keep_these_cols <- c(which(colnames(MCmatrix) == "CHAIN"), keep_theta_cols, keep_omega2_cols, keep_omega3_cols)
  #Revise MCMC matrix to just the columns I need
  MCmatrix <- MCmatrix[, keep_these_cols]
  ###################################################################
  #Get order of participants in rows of aggregated data
  if (!is.null(expand)) {
    group2_aggr_factor <- factor(datFrm[, Group2], levels=c(datFrm[, Group2]))
  } else {
    group2_aggr_factor <- levels(factor(datFrm[, Group2], levels=names(table(sort(datFrm[, Group2]))) ))
  }
  # Use the aggregated data if needed
  if (!is.null(expand)) {
    datFrm <- fncExpAgr(DF=datFrm, X1=Group2, X2=Group3, Z=Outcome, N=expand, Level=Level)
  } else {
    datFrm <- datFrm
  }
  #Make a factor from aggregated data so that it follows the same order
  if (!is.null(expand)) {
    datFrm[, Group2] <- factor(datFrm[, Group2], levels=group2_aggr_factor)
  } else {
    datFrm[, Group2] <- factor(datFrm[, Group2], levels=group2_aggr_factor)
  }
  #Get the type of estimate
  if (is.null(Average_type)) {
    average_type <- "Mode"
  } else {
    average_type <- Average_type
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
    theta_cols <- grep(paste0(Theta, "\\["), colnames(MCmatrix))
  }
  #Level-2, hierarchical model
  if(Level== 2) {
    theta_cols <- grep(paste0(Theta, "\\["), colnames(MCmatrix))
  }
  if(Level== 2) {
    omega2_cols <- grep(paste0(Omega2, "\\["), colnames(MCmatrix))
  }
  #Level-3, hierarchical model
  if(Level== 3) {
    theta_cols <- grep(paste0(Theta, "\\["), colnames(MCmatrix))
  }
  if(Level== 3) {
    omega3_cols <- grep(Omega3, colnames(MCmatrix))
  }
  if(Level== 3) {
    omega2_cols <- setdiff(grep(paste0(Omega2, "\\["), colnames(MCmatrix)), omega3_cols)
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
  #if (Distribution == "Beta") {
  if (Distribution %in% c("bern", "bin")) {
    postDF$Obs.Rate <- postDF[, Outcome] / postDF[, "N"]
  } else {
    postDF$Obs.Rate <- postDF[, "Sum"] / postDF[, "N"]
  }

  #Get the row numbers with the Theta and Omega in it
  #Level-1, non-hierarchical model
  if(Level== 1) {
    theta_rows <- grep(paste0(Theta, "\\["), rownames(postDF))
  }
  #Level-2, hierarchical model
  if(Level== 2) {
    theta_rows <- grep(paste0(Theta, "\\["), rownames(postDF))
  }
  if(Level== 2) {
    omega2_rows <- grep(paste0(Omega2, "\\["), rownames(postDF))
  }
  #Level-3, hierarchical model
  if(Level== 3) {
    theta_rows <- grep(paste0(Theta, "\\["), rownames(postDF))
  }
  if(Level== 3) {
    omega2_rows <- grep(paste0(Omega2, "\\["), rownames(postDF))
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
              "Theta"=Theta, "Omega2"=Omega2, "Omega3"=Omega3, "Average"=Average_type,
              "Order"=o6, "LTR"=LTR, "LO2R"=LO2R, "LO3R"=LO3R,
              "Lower"= intersect("HDIlow", colnames(postDF)),
              "Upper"= intersect("HDIhigh",colnames(postDF)),
              "ciconf_lev"= unique(postDF$HDImass), "g3_order"=g3_order,
              "Group3.Obs1"=Group3.Obs1, "Group3.Obs2"=Group3.Obs2 ))
}

#################
## Run objects ##
#################
#posterior
if(posterior == TRUE) {
  Posterior.Summary <- as.data.frame(as.list(summarizePost( paramSampleVec=paramSampleVec ,
                                                            compVal=compVal, ROPE=ROPE, credMass=credMass )))
} else {
  Posterior.Summary <- NA
}
#Get multilevel summary
if (multi==TRUE) {
  multi_smry <- fncHdiBinSmry(MCmatrix=MCMC, expand=expand, datFrm=data,
                              Outcome=dv, Group2=iv[1], Group3=iv[2],
                              Theta=parameter[1], Omega2=parameter[2], Omega3=parameter[3],
                              Average_type=center, Distribution=type, Cred.Mass=mass)
} else {
  multi_smry <- NA
}
#Final output
if(newdata == FALSE) {
  MCMC <- NA
}

#Combine in list
z <- list(Posterior.Summary=Posterior.Summary, MCMC=MCMC,
          Multilevel=multi_smry)
# Assign ham classes
class(z) <- c("Bayes","ham", "list")
return(z)
} # End of Bayesian section #

