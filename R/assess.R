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
                   intervention =NULL, int.time=NULL, treatment=NULL,
                   interrupt=NULL, topcode =NULL, propensity =NULL, newdata =FALSE) {
  # Use various formulas for the different models
  primary_formula <- formula
  #Get formula variables
  xyvar <- all.vars(primary_formula)
  yvar <- xyvar[1]
  xvar <- xyvar[-1]

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
  #Stop too many treatment and interruption periods
  if(length(treatment) > 1) stop("Error: treatment > 1. Expecting only 1 time.")
  if(length(interrupt) > 2) stop("Error: interrupt > 2. Expecting only 1 or 2 times.")
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
        stop("Error: Not expecting interention when there is no did, its, or propensity model specified.")
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

  # Get unique times
  if(its %in%  c("one", "two")) {
    unique_its_time <- sort(unique(data[, int.time]))
  }
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
  # Create ITS variables #
  # All groups, all times
  if(itsa_type != "none") {
    ITS.Time <- as.numeric(ordered(data[, int.time]))
  }
  if(itsa_type %in% c("mgst", "mgmt")) {
    ITS.Int <- data[, intervention]
  }
  if(itsa_type %in% c("mgst", "mgmt")) {
    txi <- ITS.Int * ITS.Time
  }
  # Time 1
  if(itsa_type != "none") {
    post1 <- ifelse(ITS.Time >= which(unique_its_time == sort(interrupt)[1]), 1, 0 )
  }
  if(itsa_type != "none") {
    txp1 <- (ITS.Time - which(unique_its_time == sort(interrupt)[1])) * post1
  }
  if(itsa_type %in% c("mgst", "mgmt")) {
    ixp1 <- ITS.Int * post1
  }
  if(itsa_type %in% c("mgst", "mgmt")) {
    txip1 <- ITS.Int * txp1
  }
  # Time 2
  if(itsa_type %in% c("sgmt", "mgmt")) {
    post2 <- ifelse(ITS.Time >= which(unique_its_time == sort(interrupt)[2]), 1, 0 )
  }
  if(itsa_type %in% c("sgmt", "mgmt")) {
    txp2 <- (ITS.Time - which(unique_its_time == sort(interrupt)[2])) * post2
  }
  if(itsa_type == "mgmt") {
    ixp2 <- ITS.Int * post2
  }
  if(itsa_type == "mgmt") {
    txip2 <- ITS.Int * txp2
  }
  #No propensity score for single group ITSA
  if(itsa_type %in% c("sgst", "sgmt")) {
    if (!is.null(propensity) ) {
      stop("Error: No propensity score calculated without a control group.")
    }
  }
  # Make DID data #
  if (did == "two") {
    did_data <- data.frame(Post.All, Int.Var, Period, DID)
  }
  if (did == "many") {
    did_data <- data.frame(Post.All, Period, DID, DID.Trend)
  }
  #Get DID column names for later interpretations
  if(did_type != "none") {
    DID.Names <- colnames(did_data)
  } else {
    DID.Names <- NULL
  }

  # Make ITS dataframes #
  # Single group, single treatment (sgst) #
  if(itsa_type == "sgst") {
    its_data <- data.frame(ITS.Time, post1, txp1)
    its_nms22 <- c("post1","txp1")
    colnames(its_data)[which(colnames(its_data) %in% its_nms22)] <-
      c(paste0("post", sort(interrupt)[1]), paste0("txp", sort(interrupt)[1]))
  }
  # Single group, multi-treatments (sgmt) #
  if(itsa_type == "sgmt") {
    its_data <- data.frame(ITS.Time, post1,txp1,post2, txp2)
    its_nms22 <- c("post1","txp1","post2", "txp2")
    colnames(its_data)[which(colnames(its_data) %in% its_nms22)] <-
      c(paste0("post", sort(interrupt)[1]), paste0("txp", sort(interrupt)[1]),
        paste0("post", sort(interrupt)[2]), paste0("txp", sort(interrupt)[2]))
  }
  # Multi-group, single treatments (mgst) #
  if(itsa_type == "mgst") {
    its_data <- data.frame(ITS.Time, ITS.Int, txi, post1,txp1,ixp1, txip1)
    its_nms22 <- c("post1","txp1","ixp1", "txip1")
    colnames(its_data)[which(colnames(its_data) %in% its_nms22)] <-
      c(paste0("post", sort(interrupt)[1]), paste0("txp", sort(interrupt)[1]),
        paste0("ixp", sort(interrupt)[1]), paste0("txip", sort(interrupt)[1]))
  }
  # Multi-group, multi-treatments (mgmt) #
  if(itsa_type == "mgmt") {
    its_data <- data.frame(ITS.Time, ITS.Int, txi, post1,txp1,ixp1,
                           txip1,post2, txp2,ixp2, txip2)
    its_nms22 <- c("post1","txp1","ixp1", "txip1","post2", "txp2","ixp2", "txip2")
    colnames(its_data)[which(colnames(its_data) %in% its_nms22)] <-
      c(paste0("post", sort(interrupt)[1]), paste0("txp", sort(interrupt)[1]),
        paste0("ixp", sort(interrupt)[1]), paste0("txip", sort(interrupt)[1]),
        paste0("post", sort(interrupt)[2]), paste0("txp", sort(interrupt)[2]),
        paste0("ixp", sort(interrupt)[2]), paste0("txip", sort(interrupt)[2]))
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
    ITS.Effects <- itsEffect(model= its_model, type= itsa_type)
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

  #Calculate group means per time period
  if(any(!is.null(c(intervention, int.time)) ==  TRUE)) {
    aggr_fmla <- as.formula(paste(paste0(yvar , "~"),
                                  paste(c(int.time,intervention), collapse= "+")))
    group_means <- stats::aggregate(x=aggr_fmla, data=data, FUN="mean")
    colnames(group_means) <-  c(eval(int.time), eval(intervention), eval(yvar))
  } else {
    group_means <- NULL
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
                        int.time=int.time, treatment=treatment, interrupt=interrupt, group_means=group_means))
  class(z) <- c("assess","ham", "list")

  #Returned objects
  return(z)
}


