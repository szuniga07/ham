#' Summarize Bayesian Markov Chain Monte Carlo (MCMC) object
#'
#' Convert a list of Bayesian analysis chains (e.g., coda package mcmc.list objects) into a data frame
#' for analysis and creating plots. Calculates a set of descriptive statistics that summarize
#' MCMC parameters. MCMC converted to data frame and summary values are also returned as a data frame.
#'
#' @param x list object of MCMC chains (e.g, mcmc.list).
#' @param posterior logical value that indicates whether to provide a summary of a posterior estimate. This produces results
#' for one parameter. This may conflict with 'multi' and 'target' settings. For example, when multi=TRUE, the abbreviated
#' parameter name 'theta' for `theta[1]` to `theta[8]` will prevent it from running. Default is FALSE.
#' @param parameter single or multiple element character vector name of parameter in MCMC chains to produce summary statistics.
#' Default is NULL.
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
#' @param target list of one or two named elements (p, y) with numeric values that represent quantile values (p) in the distribution to return
#' associated outcome values and/or specific outcome values (y) to retrieve associated probabilities. For example, a
#' distribution of harmful hospital readmission rates has an estimated median value of 0.25. Staff are considering 2 types of targets,
#' percentiles (p) of key interest and specific outcome rates (y). They want to know the readmission rate that is at
#' the 40th percentile for a reduced readmission rate and the probability greater than a rate of 0.20. They get this information
#' by entering target=list(p=0.40, y=0.20); calculating 1 - prob(y) from returned results gives them an idea about the effort
#' needed to meet this target of a reduced readmission rate. Default is NULL. Select type= one of these options: 'n', 'ln',
#' 'sn', 'w', 'g', 't', 'bern', 'bin'. Also select parameter= the appropriate center, spread, and possible 3rd shape distribution
#' parameter (e.g., parameter=c('mean', 'sd')). And option to select center= 'mean', 'median', 'mode'.
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
                  data=NULL, dv=NULL, iv=NULL, expand=NULL, target=NULL) {
  #Looking for a list
  if (any(class(x) %in% c("list", "mcmc.list")) == FALSE) {stop("Error: Expecting list class object." )}
  #Looking for 1 parameter name
#  if(!is.null(parameter)) {
#    if(posterior ==TRUE ) {
#      if(length(parameter) != 1 ) {
#      stop("Error: Expecting parameter length equal to 1.")
#    }
#    }
#  }
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
  #Make sure any target value shows up with a name
  if(!is.null(target) ) {
    if( is.null(names(target) )) {
      stop("Error: Expecting a named list for 'target'.")
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
  paramSampleVec <- MCMC[, parameter[1]]
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

################################################################################
#           6. Get proportions above/below specific values                     #
################################################################################
#This function calculates the proportion above specific values.
fncPropGtY <- function( MCMC=NULL, Distribution=NULL, yVal=NULL, qVal=NULL,
                        Center=NULL, Spread=NULL, Skew=NULL, CenTend=NULL ) {
  #Convert into a matrix
  MC.Matrix <- MCMC

  #Make first letter capitalized
  firstchar <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }
  #Creat central tendency variable
  if(!is.null(CenTend)) {
    CenTend <- firstchar(CenTend)
  } else {
    CenTend <- "Mode"
  }

  #Shape and rate parameters using the mode values of omega and kappa
  a_shape <- MC.Matrix[, Center] * (MC.Matrix[, Spread] - 2) + 1
  b_shape <- (1 - MC.Matrix[, Center]) * (MC.Matrix[, Spread] - 2) + 1

  ###################
  ## Mean for Beta ##
  ###################
  #if(!is.null(yVal)) {
  if(Distribution %in% c("bern", "bin")) {
    mean_val <- a_shape/MC.Matrix[, Spread]
  }
  #  }

  if(Distribution %in% c("bern", "bin")) {
    mean_val_dist <- summarizePost(mean_val)[c(c("Mode","Median",
                                                 "Mean")[which(c("Mode","Median","Mean")== CenTend)],"HDIlow","HDIhigh")]
  }
  else {
    mean_val_dist <- NA
  }

  #######################
  ## Beta distribution ##
  #######################
  ## Get summary ##
  # Proportion greater than Y
  PbetaGtY <- list()
  if(!is.null(yVal)) {
    if(Distribution %in% c("bern", "bin")) {
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
    if(Distribution %in% c("bern", "bin")) {
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
    if(Distribution %in% c("bern", "bin")) {
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
    if(Distribution == "ln") {
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
    if(Distribution == "ln") {
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
    if(Distribution == "n") {
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
    if(Distribution == "n") {
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
    if(Distribution == "sn") {
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
    if(Distribution == "sn") {
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
    if(Distribution == "w") {
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
    if(Distribution == "w") {
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
    if(Distribution == "g") {
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
    if(Distribution == "g") {
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
  if (Distribution %in% c("bern", "bin")) {
    PdisGtY <- PbetaGtY
  }
  if (Distribution == "ln") {
    PdisGtY <- PlogGtY
  }
  if (Distribution == "n") {
    PdisGtY <- PnormGtY
  }
  if (Distribution == "sn") {
    PdisGtY <- PsnormGtY
  }
  if (Distribution == "t") {
    PdisGtY <- PtGtY
  }
  if (Distribution == "w") {
    PdisGtY <- PWeibGtY
  }
  if (Distribution == "g") {
    PdisGtY <- PGammaGtY
  }
  #Make NA if the above weren't selected
  if (is.null(PdisGtY)) {
    PdisGtY <- NA
  }
  ## 2. Quantile ##
  if (Distribution %in% c("bern", "bin")) {
    QdisGtY <- QbetaGtY
  }
  if (Distribution == "ln") {
    QdisGtY <- QlogGtY
  }
  if (Distribution == "n") {
    QdisGtY <- QnormGtY
  }
  if (Distribution == "sn") {
    QdisGtY <- QsnormGtY
  }
  if (Distribution == "t") {
    QdisGtY <- QtGtY
  }
  if (Distribution == "w") {
    QdisGtY <- QWeibGtY
  }
  if (Distribution == "g") {
    QdisGtY <- QGammaGtY
  }
  #Make NA if the above weren't selected
  if (is.null(QdisGtY)) {
    QdisGtY <- NA
  }

  return(list("Est.Prop.GT.Y"= PdisGtY,
              "Est.Quantile.Y"= QdisGtY,
              "Est.Mean.Beta"=mean_val_dist) )
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
## Targets ##
if(!is.null(target) ) {
  target_smry <- fncPropGtY(MCMC=MCMC, Distribution=type, yVal=target[[2]],
                            qVal=target[[1]], Center=parameter[1],
                            Spread=parameter[2], Skew=parameter[3],
                            CenTend=center )
} else {
  target_smry <- NA
}

#Final output
if(newdata == FALSE) {
  MCMC <- NA
}

#Combine in list
z <- list(Posterior.Summary=Posterior.Summary, MCMC=MCMC,
          Multilevel=multi_smry, Target=target_smry)
# Assign ham classes
class(z) <- c("Bayes","ham", "list")
return(z)
} # End of Bayesian section #

