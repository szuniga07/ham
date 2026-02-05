#' Assess models with regression
#'
#' Fit ordinary least squares (OLS) and logistic models. And fit models for causal inference such
#' as differences-in-differences and interrupted time series. Run these models to evaluate program performance
#' or test intervention effects (e.g., healthcare programs). Options are available
#' for top coding the outcome variable as well as propensity scores. New data can
#' optionally be returned that has these additional variables and constructed variables
#' that are used for DID and ITS models.
#'
#'
#' @param formula a formula object. Use 'Y ~ .' in DID and ITS models to only
#' specify the constructed model variables (e.g., right side of the DID model:
#' Y ~ Post.All + Int.Var + DID). If regression=ols or regression=logistic, 'Y ~ .'
#' will use all variables in the data.frame as is standard in formulas.
#' @param data a data.frame in which to interpret the variables named in the formula.
#' @param regression Select a regression method for standard regression models
#' (i.e., neither DID nor ITS). Options are regression="ols" (ordinary least squares AKA linear)
#' or regression="logistic". Default is regression="none" for no standard regression model.
#' @param did option for Differences-in-Differences (DID) regression. Select did="two" for
#' models with only 2 time points (e.g., pre/post-test). Select did="many" for >= 3 time points
#' (e.g., monthly time points in 12 months of data). Default is did="none" for no DID.
#' @param its option for Interrupted Time Series (ITS) regression. Select its="one" for one
#' group (e.g., intervention only). Select its="two" for two groups (intervention and control).
#' Default is did="none" for no ITS.
#' @param intervention optional intervention variable name selected for DID, ITS, and propensity score
#' models that indicate which cases are in the intervention or not.
#' @param int.time optional intervention time variable name selected for DID or ITS models. This
#' indicates the duration of time relative to when the intervention started.
#' @param treatment optional treatment start period variable name selected for DID models.
#' Select 1 value from 'int.time' to indicate the start of the intervention.
#' @param interrupt optional interruption (or intervention) period(s) variable name selected for ITS
#' models. Select 1 or 2 values from 'int.time' to indicate the start and/or key intervention periods.
#' @param subset an expression defining a subset of the observations to use in the regression model. The default
#' is NULL, thereby using all observations. Specify, for example, data$hospital == "NY" or c(1:100,200:300) respectively to
#' use just those observations. This is helpful when doing a submodel for DID or ITS after identifying similar groups.
#' DID and ITS models could be improved by limiting the choice of control groups to only those with similar values on
#' the intervention indicator and baseline trend variable (e.g., 'ITS.Time' and 'ITS.Int') with p-values >= 0.10.
#' @param stagger optional list to indicate staggered entry into the intervention or treatment group.
#' Relevant model variables are re-coded to appropriate values and can be used for a form of 'stacked' DID
#' or ITS. If a group of cases joins X months after the primary sample, model variables are adjusted X months.
#' This three element list named: 'a' = a character vector for the name of the grouping column; 'b' = specific
#' categories or levels that indicate which cases have a staggered entry; and 'c' = the time point values
#' at staggered entry. Both 'b' and 'c' must have identical lengths. For ITS models, the staggered entry
#' time must be: interrupt 1 < stagger time < interrupt 2. For example, a WHO health policy may have
#' started in the 3rd year of the study period in NY and Toronto but Chicago and LA joined 6 and 12
#' months later, therefore stagger= list(a= 'city', b=c('Chicago', 'LA')), c=(30, 36) while interrupt= 25.
#' Default is NULL.
#' @param topcode optional value selected to top code Y (or left-hand side) of the formula. Analyses
#' will be performed using the new top coded variable.
#' @param propensity optional character vector of variable names to perform a propensity score model.
#' This requires the 'intervention' option to be selected. All models will include 'pscore' (propensity
#' score) in the analysis as a covariate adjustment using the propensity score.
#' @param newdata optional logical value that indicates if you want the new data returned. newdata=TRUE
#' will return the data with any new columns created from the DID, ITS, propensity score, or top coding.
#' The default is newdata=FALSE. No new data will be returned if none was created.
#'
#'
#' @return a list of results from selected regression models. Will return new data if selected.
#' And returns relevant model information such as variable names, type of analysis, formula, study
#' information, and summary of ITS effects if analyzed.
#' @export
#'
#' @references
#' Angrist, J. D., & Pischke, J. S. (2009). Mostly Harmless Econometrics:
#' An Empiricist's Companion. Princeton University Press. ISBN: 9780691120355.
#'
#' Gebski, V., et al. (2012). Modelling Interrupted Time Series to Evaluate Prevention
#' and Control of Infection in Healthcare. Epidemiology & Infections, 140, 2131–2141.
#' https://doi.org/10.1017/S0950268812000179
#'
#' Linden, A. (2015). Conducting Interrupted Time-series Analysis for Single- and
#' Multiple-group Comparisons. The Stata Journal, 15, 2, 480-500. https://doi.org/10.1177/1536867X1501500208
#'
#' @examples
#' # ordinary least squares R^2
#' summary(assess(hp ~ mpg+wt, data=mtcars, regression="ols")$model)
#'
#' # logistic
#' summary(assess(formula=vs~mpg+wt+hp, data=mtcars, regression="logistic")$model)
#'
#' # OLS with a propensity score
#' summary(assess(formula=los ~ month+program, data=hosprog, intervention = "program",
#' regression="ols", propensity=c("female","age","risk"))$model)
#'
#' # OLS: top coding los at 8.27 and propensity score means (top.los and pscore)
#' summary(assess(formula=los ~ month+program, data=hosprog, intervention = "program",
#' regression="ols", topcode=8.27, propensity=c("female","age","risk"),
#' newdata=TRUE)$newdata[, c("los", "top.los", "pscore")])
#'
#' # differences-in-differences model: using 2 time periods, pre- and post-intervention
#' summary(assess(formula=los ~ ., data=hosprog, intervention = "program",
#' int.time="month", treatment = 5, did="two")$DID)
#'
#' # DID model: using time points
#' summary(assess(formula=los ~ ., data=hosprog, intervention = "program",
#' int.time="month", treatment = 5, did="many")$DID)
#'
#' #interrupted time series model: two groups and 1 interruption (interrupt= 5)
#' summary(assess(formula=los ~ ., data=hosprog, intervention = "program",
#' int.time="month", its="two", interrupt = 5)$ITS)
#'
#' #interrupted time series model: two groups and 2 interruptions (interrupt= c(5,9))
#' summary(assess(formula=los ~ ., data=hosprog, intervention = "program",
#' int.time="month", its="two", interrupt = c(5,9))$ITS)
#'
#' @importFrom stats as.formula binomial plogis predict update aggregate
assess <- function(formula, data, regression= "none", did ="none", its ="none",
                   intervention =NULL, int.time=NULL, treatment=NULL,interrupt=NULL, subset=NULL,
                   stagger= NULL, topcode =NULL, propensity =NULL, newdata =FALSE) {
  # Use various formulas for the different models
  primary_formula <- formula
  #Get formula variables
  xyvar <- all.vars(primary_formula)
  yvar <- xyvar[1]
  xvar <- xyvar[-1]

#Identify all rows to use for subsets
  all_rows <- nrow(data)
  if(!is.null(subset)) {
    subset <- subset
  } else {
    subset <- 1:all_rows
  }
#Create subset data if needed
  if(!is.null(sub)) {
    data <-   eval(substitute(data[subset , ], list(subset =subset))  )
  }
  # Identify duplicate variable names in new data
  if (is.null(data)) {
    stop("Error: No data found.")
  }

  main_data_vars <- colnames(data)
  newdata_vars <- c("Post.All", "Period", "DID","DID.Trend","Int.Var","pscore")
  dup_vars <- intersect(main_data_vars, newdata_vars)
  name_stop_fnc <-paste0("Error: Duplicated column name(s): ", paste(dup_vars, collapse = ", "),
                         ". Rename variables, these are reserved for function purposes.")
  if (length( dup_vars) >= 1) {
    stop(name_stop_fnc)
  }
  #Stop too many treatment and duplicated interruption periods
  if(length(treatment) > 1) stop("Error: treatment > 1. Expecting only 1 time.")
  if(any(duplicated(interrupt)) == TRUE) stop("Error: Duplicated 'interrupt'. Expecting unique values.")

  #Identify if there will be new data created
  if(did !="none") {
    create_did <- TRUE
  } else {
    create_did <- FALSE
  }
  if(its !="none") {
    create_its <- TRUE
  } else {
    create_its <- FALSE
  }
  if(!is.null(topcode)) {
    create_topcode <- TRUE
  } else {
    create_topcode <- FALSE
  }
  if(!is.null(propensity)) {
    create_propensity <- TRUE
  } else {
    create_propensity <- FALSE
  }
  #Indicate if any data is created
  additional_data <-  c(create_did, create_its, create_topcode, create_propensity)

  # Top coding #
  if( !is.null(topcode)) {
    temp.topcode.y <- ifelse(data[, yvar] >= topcode, topcode, data[, yvar])
  }

  # Propensity score model #
  # Proper propensity score arguments
  if(!is.null(propensity)) {
    if(is.null(intervention)) {
      stop("Error: 'intervention=NULL'. Need intervention name for propensity scores.")
    }
  }

  if (!is.null(intervention)) {
    if(all(c(did, its) ==  "none")) {
      if(is.null(propensity)) {
        stop("Error: Not expecting intervention when there is no did, its, or propensity model specified.")
      }
    }
  }

  # Creates propensity score model formula
  if(!is.null(propensity)) {
    prop_mdl_fmla <- as.formula(paste(paste0(intervention , "~"),
                                      paste(propensity, collapse= "+")))
  } else {
    prop_mdl_fmla <- NULL
  }
  if(!is.null(propensity)) {
    propensity_model <- stats::glm(prop_mdl_fmla, family=binomial(link='logit'), data=data)
    pscore <- plogis(predict(propensity_model, newdata= data))
  }
  if(!is.null(propensity)) {
    # Add variables to model formula
    if(xyvar[2] != ".") {
      primary_formula <- update(primary_formula, paste("~ . +", "pscore"))
    } else {
      primary_formula <- update(primary_formula, paste("~  +", "pscore"))
    }
  }

  #Data frames to make later
  did_data <- NULL
  its_data <- NULL
  prop_data <- NULL
  top_data <- NULL

  #Proper regression arguments
  if (!regression %in% c("none","ols","logistic")) {
    stop("Error: 'regression='. Only select 'none', 'ols','logistic'.")
  }
  if(regression %in% c("ols","logistic")) {
    if(any(c(did,its) != "none")) {
      stop("Error: regression= ols or logistic. Does not run concurrently with did or its.")
    }
  }
  #Proper DID arguments
  if (!did %in% c("none","two","many")) {
    stop("Error: 'did='. Only select 'none', 'two', 'many'.")
  }
  if (did== "two") {
    if(is.null(intervention)) {
      stop("Error: 'intervention' is missing.")
    }
  }
  if (did== "two") {
    if(is.null(int.time)) {
      stop("Error: 'int.time' is missing.")
    }
  }
  if (did== "two") {
    if(is.null(treatment)) {
      stop("Error: 'treatment' is missing.")
    }
  }
  if (did== "many") {
    if(is.null(intervention)) {
      stop("Error: 'intervention' is missing.")
    }
  }
  if (did== "many") {
    if(is.null(int.time)) {
      stop("Error: 'int.time' is missing.")
    }
  }
  if (did== "many") {
    if(is.null(treatment)) {
      stop("Error: 'treatment' is missing.")
    }
  }
  # Create DID variables #
  if (did %in% c("two","many")) {
    Post.All <- ifelse(data[, int.time] >= treatment[1], 1, 0 )
    Period <- as.numeric(ordered(data[, int.time]))
  }
  if (did == "two") {
    Int.Var <- data[, intervention]
    DID <- Post.All * Int.Var
  }
  if (did == "many") {
    DID <- ifelse(Post.All == 1 & data[, intervention] ==1, 1, 0)
    DID.Trend <- DID * Period
  }
  #DID type to highlight effects later
  did_type <- did
  #Default regression type to highlight effects later
  regression_type <- regression

  ## ITSA ##
  if (!its %in% c("none","one","two")) {
    stop("Error: 'its='. Only select 'none', 'one', 'two'.")
  }
  if (its== "one") {
    if(is.null(intervention)) {
      stop("Error: 'intervention' is missing.")
    }
  }
  if (its== "one") {
    if(is.null(int.time)) {
      stop("Error: 'int.time' is missing.")
    }
  }
  if (its== "one") {
    if(is.null(interrupt)) {
      stop("Error: 'interrupt' is missing.")
    }
  }
  if (its== "two") {
    if(is.null(intervention)) {
      stop("Error: 'intervention' is missing.")
    }
  }
  if (its== "two") {
    if(is.null(int.time)) {
      stop("Error: 'int.time' is missing.")
    }
  }
  if (its== "two") {
    if(is.null(interrupt)) {
      stop("Error: 'interrupt' is missing.")
    }
  }
  #############################
  ## Individual ITS function ##
  #############################
  #indicate type of ITSA
  if(its == "none") {
    itsa_type <- its
  }
  if(its == "one") {
    if(length(interrupt) == 1) {
      itsa_type <- "sgst"
    } else {
      itsa_type <- "sgmt"
    }
  }
  if(its == "two") {
    if(length(interrupt) == 1) {
      itsa_type <- "mgst"
    } else {
      itsa_type <- "mgmt"
    }
  }
  #Stop: Too few time points between periods for ITS
  multi_interrupt_diff <- NULL
  if (itsa_type %in% c("sgmt","mgmt")) {
    multi_interrupt_diff <- 0:1 %in% diff(sort(interrupt))
  }
  if(any(multi_interrupt_diff) ==TRUE) stop("Error: Difference between time points is < 2 for the 'interrupt' argument. ITS can be calculated in segments of 2 time points but consider using 3 or more time points for a more informative analysis.")

  #No propensity score for single group ITSA
  if(itsa_type %in% c("sgst", "sgmt")) {
    if (!is.null(propensity) ) {
      stop("Error: No propensity score calculated without a control group.")
    }
  }

  ##########
  ## post ##
  ##########
  fncITSPost <- function(itime= ITS.Time, unqitime= unique_its_time,
                         interrupt= interrupt ) {
    #Create NULL to prevent notes
    ITS.Time <- NULL
    unique_its_time <- NULL
    # 1. Create data frame with a numeric structure and temp column names
    df <- data.frame(matrix(nrow = length(itime), ncol = length(interrupt)))
    # 2. Make each column a numeric variable to pre-allocate space in the for loop
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    # 3. Initialize a vector to store the new names
    new_names <- vector("character", ncol(df))
    # 4. Use a for loop to generate names
    for (i in 1:length(interrupt)) {
      new_names[i] <- paste0("post", sort(interrupt)[i])
    }
    # 5. Assign the new names to the data frame
    colnames(df) <- new_names
    # 6. Create "post" values with a new function
    postfnc <- function(df, itime= itime, unqitime= unqitime,
                        interrupt= interrupt) {
      #For loop to create "post" variables
      for (i in 1:length(interrupt)) {
        df[, i] <- ifelse(itime >= which(unqitime == sort(interrupt[i])), 1, 0 )
      }
      return(df)
    }
    # 7. Run subfunction to create final data frame
    df <- postfnc(df=df, itime= itime, unqitime= unqitime,
                  interrupt= interrupt)

    return(df)
  }

  #########
  ## txp ##
  #########
  fncITStxp <- function(itime= ITS.Time, unqitime= unique_its_time,
                        interrupt= interrupt, postdf= postdf ) {
    #Create NULL to prevent notes
    ITS.Time <- NULL
    unique_its_time <- NULL
    # 1. Create data frame with a numeric structure and temp column names
    df <- data.frame(matrix(nrow = length(itime), ncol = length(interrupt)))
    # 2. Make each column a numeric variable to pre-allocate space in the for loop
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    # 3. Initialize a vector to store the new names
    new_names <- vector("character", ncol(df))
    # 4. Use a for loop to generate names
    for (i in 1:length(interrupt)) {
      new_names[i] <- paste0("txp", sort(interrupt)[i])
    }
    # 5. Assign the new names to the data frame
    colnames(df) <- new_names
    # 6. Create "txp" values with a new function
    txpfnc <- function(df, itime= itime, unqitime= unqitime,
                       interrupt= interrupt, postdf= postdf) {
      #For loop to create "txp" variables
      for (i in 1:length(interrupt)) {
        df[, i] <- (itime - which(unqitime == sort(interrupt)[i])) * postdf[, i]
      }
      return(df)
    }
    # 7. Run subfunction to create final data frame
    df <- txpfnc(df=df, itime= itime, unqitime= unqitime,
                 interrupt= interrupt, postdf= postdf)

    return(df)
  }

  #########
  ## ixp ##
  #########
  fncITSixp <- function(itime= ITS.Time, iInt= ITS.Int, interrupt= interrupt,
                        postdf= postdf ) {
    #Create NULL to prevent notes
    ITS.Time <- NULL
    ITS.Int <- NULL
    # 1. Create data frame with a numeric structure and temp column names
    df <- data.frame(matrix(nrow = length(itime), ncol = length(interrupt)))
    # 2. Make each column a numeric variable to pre-allocate space in the for loop
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    # 3. Initialize a vector to store the new names
    new_names <- vector("character", ncol(df))
    # 4. Use a for loop to generate names
    for (i in 1:length(interrupt)) {
      new_names[i] <- paste0("ixp", sort(interrupt)[i])
    }
    # 5. Assign the new names to the data frame
    colnames(df) <- new_names
    # 6. Create "ixp" values with a new function
    ixpfnc <- function(df, iInt= iInt, postdf=postdf, interrupt= interrupt) {
      #For loop to create "ixp" variables
      for (i in 1:length(interrupt)) {
        df[, i] <- iInt * postdf[, i]
      }
      return(df)
    }
    # 7. Run subfunction to create final data frame
    df <- ixpfnc(df=df, iInt= iInt, postdf=postdf, interrupt= interrupt)

    return(df)
  }

  ##########
  ## txip ##
  ##########
  fncITStxip <- function(itime= ITS.Time, iInt= ITS.Int,
                         interrupt= interrupt, txpdf= txpdf ) {
    #Create NULL to prevent notes
    ITS.Time <- NULL
    ITS.Int <- NULL
    # 1. Create data frame with a numeric structure and temp column names
    df <- data.frame(matrix(nrow = length(itime), ncol = length(interrupt)))
    # 2. Make each column a numeric variable to pre-allocate space in the for loop
    df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    # 3. Initialize a vector to store the new names
    new_names <- vector("character", ncol(df))
    # 4. Use a for loop to generate names
    for (i in 1:length(interrupt)) {
      new_names[i] <- paste0("txip", sort(interrupt)[i])
    }
    # 5. Assign the new names to the data frame
    colnames(df) <- new_names
    # 6. Create "ixp" values with a new function
    txipfnc <- function(df, iInt= ITS.Int, txpdf=txpdf, interrupt= interrupt) {
      #For loop to create "txip" variables
      for (i in 1:length(interrupt)) {
        df[, i] <- iInt * txpdf[, i]
      }
      return(df)
    }
    # 7. Run subfunction to create final data frame
    df <- txipfnc(df=df, iInt= iInt, txpdf=txpdf, interrupt= interrupt)

    return(df)
  }

# Make ITS data #
  fncITSdata <- function(data, intervention = intervention, int.time=int.time,
                         interrupt= interrupt, its=its, itsa_type=itsa_type) {

    # Get unique times
    if(its %in%  c("one", "two")) {
      unique_its_time <- sort(unique(data[, int.time]))
    }
    # ITS type
    if(itsa_type != "none") {
      ITS.Time <- as.numeric(ordered(data[, int.time]))
    }
    if(itsa_type %in% c("mgst", "mgmt")) {
      ITS.Int <- data[, intervention]
    }
    if(itsa_type %in% c("mgst", "mgmt")) {
      txi <- ITS.Int * ITS.Time
    }
    # Create ITS data sections #
    # post
    postdf <- fncITSPost(itime= ITS.Time, unqitime= unique_its_time,
                         interrupt= interrupt )
    # txp
    txpostdf <- fncITStxp(itime= ITS.Time, unqitime= unique_its_time,
                          interrupt= interrupt, postdf= postdf)
    # ixp
    if(itsa_type %in% c("mgst", "mgmt")) {
      ixpdf <- fncITSixp(itime= ITS.Time, iInt= ITS.Int, interrupt= interrupt,
                         postdf= postdf )
    }
    # txip
    if(itsa_type %in% c("mgst", "mgmt")) {
      txipdf <- fncITStxip(itime= ITS.Time, iInt= ITS.Int,
                           interrupt= interrupt, txpdf= txpostdf )
    }
    # Combine and arrange data
    # sgst and sgmt
    if(itsa_type %in% c("sgst", "sgmt")) {
      combined_df <- cbind(postdf, txpostdf)
      n <- ncol(postdf)
      interweave_order <- order(c(1:n, 1:n))
      combined_df <- combined_df[, interweave_order]
    }
    # mgst and mgmt
    if(itsa_type %in% c("mgst", "mgmt")) {
      combined_df <- cbind(postdf, txpostdf, ixpdf, txipdf)
      n <- ncol(postdf)
      interweave_order <- order(c(1:n, 1:n, 1:n, 1:n))
      combined_df <- combined_df[, interweave_order]
    }
    #sgmt
    if(itsa_type %in% c("sgst", "sgmt")) {
      its_data <- cbind(ITS.Time, combined_df)
    }
    #mgmt
    if(itsa_type %in% c("mgst", "mgmt")) {
      its_data <- cbind(ITS.Time, ITS.Int, txi, combined_df)
    }
    return(its_data)
  }
  #####################
  ## Create ITS data ##
  #####################
  if(itsa_type != "none") {
    its_data <- fncITSdata(data=data, intervention = intervention, int.time=int.time,
                           interrupt= interrupt, its=its, itsa_type=itsa_type)
  }

  # Staggered entry
  if(!is.null(stagger)) {
    #Declare objects
    staggerA <- NULL
    staggerB <- NULL
    staggerC <- NULL
    stagger_diff <- NULL
    # Errors from incorrectly using stagger argument #
    if(is(stagger[[1]])[1] != "character") stop("Error: Expecting a character vector for 'a' of the stagger argument.")
    if(is(stagger[[3]])[1] != "numeric") stop("Error: Expecting a numeric vector for 'c' of the stagger argument.")
    if(length(stagger[[2]]) != length(stagger[[2]])) stop("Error: There are an unequal number of elements in 'b' and 'c' of the stagger argument.")
    if(length(interrupt) > 1) {
      if(any(stagger[[3]] >= sort(interrupt)[2])) stop("Error: Staggered entry not performed if any 'stagger' time values are >= the 2nd lowest interruption value in the interrupt argument (e.g., stagger of 25 when interrupt=c(12, 24).")
    }

    if(its != "none") {
      int_time_entry <- interrupt[1]
    }
    if(did != "none") {
      int_time_entry <- treatment
    }
    #Get stagger values for later use
    staggerA <- stagger[[1]]
    staggerB <- stagger[[2]]
    staggerC <- stagger[[3]]
    #Convert model entry time into converted time (if necessary) to
    int_time_E <- which(sort(unique((data[, int.time]))) == int_time_entry)
    #Get staggered entry time as ordered rank to fit with converted data
    stagC <- vector(mode="numeric", length= length(staggerC))
    for (i in 1:length(staggerC)) {
      stagC[i] <- which(sort(unique((data[, int.time]))) == staggerC[i])
    }
    # Get difference between primary entry period and staggered groups
    stagger_diff <- int_time_E - stagC

  }

  ########################
  ## Staggered ITS data ##
  ########################
  if(!is.null(stagger)) {

    ##########
    ## sgst ##
    ##########
    if (itsa_type == "sgst") {
      # Recode the binary post intervention variables for post and ixp
      for (i in 1:length(staggerB)) {
        its_data[data[, staggerA] == staggerB[i], 2][its_data[data[, staggerA] == staggerB[i], 1] < staggerC[i]] <- 0
      }
      # Recode the trend variables for txp and txip
      for (i in 1:length(staggerB)) {
        its_data[, 3][data[, staggerA] == staggerB[i]] <- its_data[, 3][data[, staggerA] == staggerB[i]] + stagger_diff[i]
      }
      # Recode any negative values as 0
      its_data[, 3] <- ifelse(its_data[, 3] < 0, 0, its_data[, 3])
    }
    ##########
    ## sgmt ##
    ##########
    if (itsa_type == "sgmt") {
      # Recode the binary post intervention variables for post and ixp
      for (i in 1:length(staggerB)) {
        its_data[data[, staggerA] == staggerB[i], 2][its_data[data[, staggerA] == staggerB[i], 1] < staggerC[i]] <- 0
        its_data[data[, staggerA] == staggerB[i], 4][its_data[data[, staggerA] == staggerB[i], 1] < staggerC[i]] <- 0
      }
      # Recode the trend variables for txp and txip
      for (i in 1:length(staggerB)) {
        its_data[, 3][data[, staggerA] == staggerB[i]] <- its_data[, 3][data[, staggerA] == staggerB[i]] + stagger_diff[i]
        its_data[, 5][data[, staggerA] == staggerB[i]] <- its_data[, 5][data[, staggerA] == staggerB[i]] + stagger_diff[i]
      }
      # Recode any negative values as 0
      its_data[, 3] <- ifelse(its_data[, 3] < 0, 0, its_data[, 3])
      its_data[, 5] <- ifelse(its_data[, 5] < 0, 0, its_data[, 5])
    }
    #################
    ## mgst & mgmt ##
    #################
    if (itsa_type %in% c("mgst","mgmt")) {
      # Recode the binary post intervention variables for post and ixp
      for (i in 1:length(staggerB)) {
        its_data[data[, staggerA] == staggerB[i], 4][its_data[data[, staggerA] == staggerB[i], 1] < staggerC[i]] <- 0
        its_data[data[, staggerA] == staggerB[i], 6][its_data[data[, staggerA] == staggerB[i], 1] < staggerC[i]] <- 0
      }
      # Recode the trend variables for txp and txip
      for (i in 1:length(staggerB)) {
        its_data[, 5][data[, staggerA] == staggerB[i]] <- its_data[, 5][data[, staggerA] == staggerB[i]] + stagger_diff[i]
        its_data[, 7][data[, staggerA] == staggerB[i]] <- its_data[, 7][data[, staggerA] == staggerB[i]] + stagger_diff[i]
      }
      # Recode any negative values as 0
      its_data[, 5] <- ifelse(its_data[, 5] < 0, 0, its_data[, 5])
      its_data[, 7] <- ifelse(its_data[, 7] < 0, 0, its_data[, 7])
    }
  }

  #####################
  ## Create DID data ##
  #####################
  # Make DID data #
  if (did == "two") {
    did_data <- data.frame(Post.All, Int.Var, Period, DID)
  }
  if (did == "many") {
    did_data <- data.frame(Post.All, Period, DID, DID.Trend)
  }

  ########################
  ## Staggered ITS data ##
  ########################

  if(!is.null(stagger)) {
  #############
  ## DID two ##
  #############
  if(did_type == "two" ) {
    # Recode the binary post intervention variables for Post.All and DID
    for (i in 1:length(staggerB)) {
      did_data[data[, staggerA] == staggerB[i], 1][did_data[data[, staggerA] == staggerB[i], 3] < staggerC[i]] <- 0
      did_data[data[, staggerA] == staggerB[i], 4][did_data[data[, staggerA] == staggerB[i], 3] < staggerC[i]] <- 0
    }
  }
    ##############
    ## DID many ##
    ##############
    if(did_type == "many" ) {
      # Recode the binary post intervention variables for post and ixp
      for (i in 1:length(staggerB)) {
        did_data[data[, staggerA] == staggerB[i], 1][did_data[data[, staggerA] == staggerB[i], 2] < staggerC[i]] <- 0
        did_data[data[, staggerA] == staggerB[i], 3][did_data[data[, staggerA] == staggerB[i], 2] < staggerC[i]] <- 0
      }
      # Recode the trend variables for txp and txip
      for (i in 1:length(staggerB)) {
        did_data[, 4][data[, staggerA] == staggerB[i]] <- did_data[, 4][data[, staggerA] == staggerB[i]] + stagger_diff[i]
      }
      # Recode any pre-intervention values as 0
      did_data[, 4] <- ifelse(did_data[, 4] < int_time_entry, 0, did_data[, 4])
    }
    }

  #Get DID column names for later interpretations
  if(did_type != "none") {
    DID.Names <- colnames(did_data)
  } else {
    DID.Names <- NULL
  }

  #Get ITS column names for later interpretations
  if(itsa_type != "none") {
    ITS.Names <- colnames(its_data)
  } else {
    ITS.Names <- NULL
  }

  # Top code data #
  if(!is.null(topcode)) {
    top_data <- temp.topcode.y
    new_topcode_yvar_name <- paste0("top.", yvar)
  } else {
    new_topcode_yvar_name <- NULL
  }
  # Add variables to model formula
  if(!is.null(topcode)) {
    if(!is.null(propensity)) {
      primary_formula <- update(primary_formula, paste(new_topcode_yvar_name, "~ . +", "pscore"))
    }
    if(is.null(propensity)) {
      if(xvar[1] == ".") {
        primary_formula <- as.formula(paste(new_topcode_yvar_name, "~  ."))
      } else {
        primary_formula <- update(primary_formula, paste(new_topcode_yvar_name, "~ ."))
      }
    }
  }
  # Propenisty score data #
  if(!is.null(propensity)) {
    prop_data <- pscore
  }
  # Makes propensity score NULL if not done so formula is ok in DID regression
  if(!is.null(propensity)) {
    prop_score_name <- "pscore"
  } else {
    prop_score_name <- NULL
  }

  ## Create newdata object ##
  # Range of new data includes: "did","its","propensity", "top code"
  if (any(additional_data) == TRUE) {
    newdata_list <- list(data, did_data, its_data, topy_var=top_data, pscore=prop_data)
    combined_df <- do.call(cbind, newdata_list[!sapply(newdata_list, is.null)])
    colnames(combined_df)[which(colnames(combined_df) =="topy_var")] <- new_topcode_yvar_name
  } else {
    combined_df <- data
  }
  if (any(additional_data) == FALSE) {
    if(newdata==TRUE) {
      warning("Warning: 'newdata==TRUE'. No new data because no additional data was created.")
    }
  }

  #Prepare new data to be returned
  if (newdata == TRUE) {
    new_df <- combined_df
  } else {
    #    new_df <- combined_df
    new_df <- NULL
  }

  # Regressions #
  #Standard covariate adjustment
  if(regression == "ols") {
    model_1 <- stats::lm(formula= primary_formula, data=combined_df)
  }
  if(regression == "logistic") {
    model_1 <- stats::glm(formula= primary_formula, family=binomial(link='logit'), data=combined_df)
  }

  # DID models # additional_data c(create_did, create_its, create_topcode, create_propensity)
  if (create_did == TRUE) {
    if (all.vars(primary_formula)[-1][1] != ".") {
      pri_for_x_did <- all.vars(primary_formula)[-1]
    } else {
      pri_for_x_did <- NULL
    }
  }
  #Updates formulas
  if (create_did == TRUE) {
    if(did== "many") {
      DID_formula <- update(primary_formula,
                            paste("~ ", paste(c(c("Period","DID","DID.Trend"), pri_for_x_did), collapse = "+") ))
    }
    if(did== "two") {
      DID_formula <- update(primary_formula,
                            paste("~ ", paste(c(c("Post.All", "Int.Var","DID"), pri_for_x_did), collapse = "+") ))
    }
  } else {
    DID_formula <- NULL
  }
  # Make return object NULL when needed
  if (regression != "none") {
    model_1 <- model_1
  } else {
    model_1 <- NULL
  }
  if (create_did == TRUE) {
    did_model <- stats::lm(formula=DID_formula , data=combined_df)
  } else {
    did_model <- NULL
  }

  # ITS model #
  if (create_its == TRUE) {
    if (all.vars(primary_formula)[-1][1] != ".") {
      pri_for_x_its <- all.vars(primary_formula)[-1]
      ITS_formula <- update(primary_formula,
                            paste("~ ", paste(c(colnames(its_data), pri_for_x_its), collapse = "+") ))
    }
    if (all.vars(primary_formula)[-1][1] == ".") {
      ITS_formula <- update(primary_formula,
                            paste("~ ", paste(c(colnames(its_data)), collapse = "+") ))
    }
  } else {
    ITS_formula <- NULL
  }
  # Finishes model
  if (create_its == TRUE) {
    its_model <- stats::lm(formula=ITS_formula , data=combined_df)
  } else {
    its_model <- NULL
  }

  # ITSA treatment effects
  if (create_its == TRUE) {
    ITS.Effects <- itsEffect(model= its_model, type= itsa_type,
                             interruptions= length(interrupt))
  } else {
    ITS.Effects <- NULL
  }

  # Removes environment from propensity formula
  if(!is.null(propensity)) {
    propensity_formula <- prop_mdl_fmla
    attributes(propensity_formula) <- NULL
  } else {
    propensity_formula <- NULL
  }
  if (create_its == TRUE) {
    attributes(ITS_formula) <- NULL
  }
  if (create_did == TRUE) {
    attributes(DID_formula) <- NULL
  }

  # Calculate group means per time period #
  #Pick a model's data for aggregating
  #DID
  if(!is.null(did_model) ==TRUE) {
    dcmodel_df <- did_model$model
  } else {
    dcmodel_df <- NULL
  }
  #ITS
  if(!is.null(its_model) ==TRUE) {
    cmodel_df <- its_model$model
  } else {
    cmodel_df <- NULL
  }
  # Get aggregated data formula #
  if(itsa_type %in% c("sgst", "sgmt") ) {
    iaggr_fmla  <- formula(paste(colnames(cmodel_df)[1], "~ ", paste(colnames(cmodel_df)[2], collapse = " + ") ))
  }
  if(itsa_type %in% c("mgst", "mgmt") ) {
    iaggr_fmla  <- formula(paste(colnames(cmodel_df)[1], "~ ", paste(colnames(cmodel_df)[2:3], collapse = " + ") ))
  }
  if(did_type %in% c("two", "many") ) {
    daggr_fmla  <- formula(paste(colnames(dcmodel_df)[1], "~ ", paste(colnames(dcmodel_df)[2:3], collapse = " + ") ))
  }

  #Calculate group means
  #Keeping the actual data group means (not the model data)
  if(any(!is.null(c(intervention, int.time)) ==  TRUE)) {
    aggr_fmla <- as.formula(paste(paste0(yvar , "~"),
                                  paste(c(int.time,intervention), collapse= "+")))
    group_means <- stats::aggregate(x=aggr_fmla, data=data, FUN="mean", na.rm=TRUE)
    colnames(group_means) <-  c(eval(int.time), eval(intervention), eval(yvar))
  } else {
    group_means <- NULL
  }

  #DID
  if(!is.null(did_model) ==  TRUE) {
    if(did_type == "two") {
      group_means_did <- stats::aggregate(x=daggr_fmla, data=dcmodel_df, FUN="mean", na.rm=TRUE)
    }
    if(did_type == "many") {
      group_means_did <- stats::aggregate(x=daggr_fmla, data=dcmodel_df, FUN="mean", na.rm=TRUE)
      #rows_needed <- nrow(group_means_did[group_means_did$DID ==0, ]) - nrow(group_means_did[group_means_did$DID ==1, ])
      #This adds in full data for missing periods b/c of how variables are coded, won't show in plot.assess
      #extra_rows <- group_means[group_means[, 2] ==1, ][1:rows_needed, ]
      #colnames(extra_rows) <- colnames(group_means_did)
      #extra_rows[, 1][1:rows_needed] <- group_means_did[group_means_did$DID ==0, 1][1:rows_needed]
      #group_means_did <- rbind(group_means_did[group_means_did$DID ==0, ], extra_rows, group_means_did[group_means_did$DID ==1, ])
      #rownames(group_means_did) <- 1:nrow(group_means_did)
    }
  } else {
    group_means_did <- NULL
  }

#ITS
  if(!is.null(its_model) ==  TRUE) {
    group_means_its <- stats::aggregate(x=iaggr_fmla, data=cmodel_df, FUN="mean", na.rm=TRUE)
  } else {
    group_means_its <- NULL
  }

  z <- list(model=model_1, DID=did_model, DID.Names=DID.Names, ITS=its_model,
            ITS.Effects=ITS.Effects,ITS.Names=ITS.Names, newdata=new_df,
            formula=list(primary_formula=primary_formula,
                         propensity_formula=propensity_formula,
                         DID_formula=DID_formula,
                         ITS_formula=ITS_formula),
            analysis_type=list(regression_type=regression_type,
                               did_type=did_type, itsa_type=itsa_type),
            study= list(regression=regression, did=did, its=its, intervention=intervention,
                        int.time=int.time, treatment=treatment, interrupt=interrupt, group_means=group_means,
                        group_means_did=group_means_did, group_means_its=group_means_its))
  class(z) <- c("assess","ham", "list")

  #Returned objects
  return(z)
}


