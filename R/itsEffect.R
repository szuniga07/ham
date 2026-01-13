#' Interrupted time series analysis effects
#'
#' Calculates effects for intervention and control groups based on interrupted
#' time series models from an assess class object. Within a period (or interruption),
#' the effect that represents the trend during the period is calculated for both
#' groups as well as the difference between the groups. Summary statistics
#' are provided that include the effect sizes, t-statistic, standard errors,
#' p-values, and 95% confidence intervals of the effect sizes. These values are
#' provided for the intervention group, control group (when applicable), and the
#' differences between the two groups (Linden, 2015). These values are automatically
#' generated while running a model in assess.
#'
#' @param model an interrupted time series (ITS) model with the "lm" class,
#' @param type analysis type for single or multiple groups and single or multiple
#' time periods. If selected type="sgst", it is single-group single-time;
#' type="sgmt", it is single-group multiple-time; type="mgst", it is multiple-group single-time;
#' and type="mgmt", it is multiple-group multiple-time.
#' @param interruptions ITS number of interruptions as numeric vector greater than 0.
#'
#' @return a data.frame object of ITS effects and summary statistics. Row names are indicated with
#' first, second, and additional numbers when there are multiple interruptions (e.g., 'Intervention.2').
#' Generally run within the assess function.
#' @export
#'
#' @references
#' Linden, Ariel. (2015). Conducting Interrupted Time-series Analysis for Single- and Multiple-group
#' Comparisons. The Stata Journal, 2015, 15(2), 480-500, https://doi.org/10.1177/1536867X1501500208
#'
#' @examples
#' i21 <- assess(formula=survey ~ ., data=hosprog, intervention = "program",topcode =NULL,
#' int.time="month", regression="none", interrupt=5, its="two", newdata=TRUE, propensity=NULL)
#' itsEffect(model= i21$ITS, type= "mgst", interruptions= 1)
#'
#' @importFrom stats coef pt qt vcov
itsEffect <- function(model, type, interruptions) {
  # Gets degrees of freedom
  its_df_resid <- model[["df.residual"]]
  #Using a matrix to get standard errors
  model.vcov <- stats::vcov(model)
  # Model coefficients
  mod_coefs <- stats::coef( model )
  # Determine length of coefficients
  total_coefs <- length(mod_coefs)
  #This gives DFs if using rms' Gls objects
  its_df_resid <- ifelse(is.null(its_df_resid), model[["dims"]][["N"]] - total_coefs, its_df_resid)
  # Get the quantile of the degrees of freedom of the residual
  quant_df_res <- stats::qt(.975,  its_df_resid)
  ## Get expected number of coefficients for an ITS model only (i.e., exlcudes non-ITS coefficients) ##
  # Number of groups
  if(type %in% c("sgst", "sgmt")) {
    num_groups <- 1
  }
  if(type %in% c("mgst", "mgmt")) {
    num_groups <- 2
  }
  # Number of interruptions in the model
  num_interrupts <- interruptions
  if(num_interrupts == 0) stop("Error: No interruptions in your model. Expecting an Interrupted Time Series model.")
  # Number of coefficients during the baseline period
  num_baseline_coef <- ifelse(num_groups == 2, 3, 1)
  # Formula for the number of expected coefficients
  expect_coefs <- ((num_groups * 2) * num_interrupts) + num_baseline_coef + 1 #1=intercept

  # Get effects, coefficient matrix, standard errors, p-value
  if(type == "sgst") {
    # Treatment effect
    Treated <- mod_coefs[[2]] + mod_coefs[[4]]
    # Makes number of extra coefficients to add to coef matrix
    add_coefs <- total_coefs - expect_coefs
    # Coefficient matrix
    mat_coefs <- matrix(c(0, 1, 0, 1, rep(0, add_coefs)), 1)
    # standard error
    stanErr <- sqrt(mat_coefs %*% model.vcov %*% t(mat_coefs))
    # t-value
    t.value <- Treated/stanErr
    # 2-tail p-value
    p.value <- (1 - stats::pt(abs(t.value), its_df_resid)) * 2
    # 95% CIs
    L95 <- Treated - (stanErr * quant_df_res)
    U95 <- Treated + (stanErr * quant_df_res)
    effect_list <- list("Intervention"= c(Treated, stanErr, t.value, p.value,
                                          L95, U95))
    effect_list <- do.call(rbind.data.frame, effect_list)
    colnames(effect_list) <- c("Effect", "S.E.", "t", "p.value", "L95","U95")
    rownames(effect_list) <- c("Intervention")
  }
  if(type == "sgmt") {
    #Create vector of coefficients of length 1:number
    tot_coefs_index <- 1:expect_coefs
    #Use the number of interruptions to make a list to store treated/control indexes
    trt_coef_index_list <- vector(mode = "list", length = num_interrupts)
    #Range to use in for loop for the different segments of time points
    range_of_periods <- 1:num_interrupts
    #For loop to fill in values for treatment group
    per_trt_num_coef <- vector(mode="list", length=num_interrupts)
    for(i in 1:num_interrupts) {
      per_trt_num_coef[[i]] <- (1:(((num_groups * 2) * range_of_periods[i]) + num_baseline_coef + 1))
      #Make a vector with all 0s
      trt_coef_index_list[[i]] <- rep(0, expect_coefs)
      #Creates values of 1 for evens/treatment groups at different interruptions
      trt_coef_index_list[[i]][per_trt_num_coef[[i]][(per_trt_num_coef[[i]]) %% 2 == 0]] <- 1
    }

    # Get the treated sum of coefficients by interruption period #
    Treated <- vector(mode="list", length= num_interrupts)
    for (i in 1:num_interrupts) {
      Treated[[i]] <- sum(mod_coefs[per_trt_num_coef[[i]][trt_coef_index_list[[i]]==TRUE]])
    }
    # How many coefficients were added to the standard models #
    add_coefs <- total_coefs - expect_coefs
    # Binary values for matrix calculations #
    mat_coefsT <- vector(mode="list", length= num_interrupts) #Treated
    for (i in 1:num_interrupts) {
      mat_coefsT[[i]] <- matrix(c(trt_coef_index_list[[i]], rep(0, add_coefs)), 1)
    }
    # Standard errors of each coefficient of main ITS variables #
    stanErrT <- vector(mode="list", length= num_interrupts) #Treated
    for (i in 1:num_interrupts) {
      stanErrT[[i]] <- sqrt(mat_coefsT[[i]] %*% model.vcov %*% t(mat_coefsT[[i]]))
    }
    # t statistic values #
    t.valueT <- vector(mode="list", length= num_interrupts) #Treated
    for (i in 1:num_interrupts) {
      t.valueT[[i]] <- Treated[[i]]/stanErrT[[i]]
    }
    # p-values #
    p.valueT <- vector(mode="list", length= num_interrupts) #Treated
    for (i in 1:num_interrupts) {
      p.valueT[[i]] <- (1 - stats::pt(abs(t.valueT[[i]]), its_df_resid)) * 2
    }
    # Upper and Lower confidence limits #
    L95T <- vector(mode="list", length= num_interrupts) #Treated
    U95T <- vector(mode="list", length= num_interrupts) #Treated
    for (i in 1:num_interrupts) {
      L95T[[i]] <- Treated[[i]] - (stanErrT[[i]] * quant_df_res)
      U95T[[i]] <- Treated[[i]] + (stanErrT[[i]] * quant_df_res)
    }

    # Get final data together into the effect list #
    effT <- vector(mode="list", length= num_interrupts) #Treated
    for (i in 1:num_interrupts) {
      effT[[i]] <- do.call(cbind.data.frame, list(Treated[[i]], stanErrT[[i]], t.valueT[[i]], p.valueT[[i]], L95T[[i]], U95T[[i]]))
    }
    #Temporary column names
    for (i in 1:num_interrupts) {
      colnames(effT[[i]]) <- c("trt", "se","t","p", "l", "u")
    }

    #Combine it into dataframes for reach interruption period
    effect_list <- vector(mode="list", length= num_interrupts) #Difference
    for (i in 1:num_interrupts) {
      effect_list[[i]] <- do.call(rbind.data.frame, list(effT[[i]]))
    }

    #Make final data frame
    effect_list <- do.call(rbind.data.frame, effect_list)
    #Column names
    colnames(effect_list) <- c("Effect", "S.E.", "t", "p.value", "L95","U95")
    #Official row names
    tnms <- vector(mode="list", length= num_interrupts) #row names
    for (i in 1:num_interrupts) {
      tnms[[i]] <- paste0(c("Intervention"), ".", i)
    }
    #Assign row names
    rownames(effect_list) <- unlist(tnms)
  }
  if(type == "mgst") {
    Treated <- mod_coefs[[2]] + mod_coefs[[4]] + mod_coefs[[6]] + mod_coefs[[8]]
    Control <- mod_coefs[[2]] + mod_coefs[[6]]
    Difference <- mod_coefs[[4]] + mod_coefs[[8]]
    add_coefs <- total_coefs - expect_coefs
    mat_coefs1T <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, rep(0, add_coefs)), 1)
    mat_coefs1C <- matrix(c(0, 1, 0, 0, 0, 0, 1, 0, rep(0, add_coefs)), 1)
    mat_coefs1D <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, rep(0, add_coefs)), 1)
    stanErr1T <- sqrt(mat_coefs1T %*% model.vcov %*% t(mat_coefs1T))
    stanErr1C <- sqrt(mat_coefs1C %*% model.vcov %*% t(mat_coefs1C))
    stanErr1D <- sqrt(mat_coefs1D %*% model.vcov %*% t(mat_coefs1D))
    t.value1T <- Treated/stanErr1T
    t.value1C <- Control/stanErr1C
    t.value1D <- Difference/stanErr1D
    p.value1T <- (1 - stats::pt(abs(t.value1T), its_df_resid)) * 2
    p.value1C <- (1 - stats::pt(abs(t.value1C), its_df_resid)) * 2
    p.value1D <- (1 - stats::pt(abs(t.value1D), its_df_resid)) * 2
    L951T <- Treated - (stanErr1T * quant_df_res)
    U951T <- Treated + (stanErr1T * quant_df_res)
    L951C <- Control - (stanErr1C * quant_df_res)
    U951C <- Control + (stanErr1C * quant_df_res)
    L951D <- Treated - (stanErr1D * quant_df_res)
    U951D <- Treated + (stanErr1D * quant_df_res)
    effect_list <- list("Intervention"= c(Treated, stanErr1T, t.value1T,
                                          p.value1T, L951T, U951T),
                        "Control"= c(Control, stanErr1C, t.value1C,
                                     p.value1C, L951C, U951C),
                        "Difference"= c(Difference, stanErr1D, t.value1D,
                                        p.value1D, L951D, U951D))
    effect_list <- do.call(rbind.data.frame, effect_list)
    colnames(effect_list) <- c("Effect", "S.E.", "t", "p.value", "L95","U95")
    rownames(effect_list) <- c("Intervention","Control","Difference")
  }
  if(type == "mgmt") {
    #Create vector of coefficients of length 1:number
    tot_coefs_index <- 1:expect_coefs
    #Use the number of interruptions to make a list to store treated/control indexes
    trt_coef_index_list <- vector(mode = "list", length = num_interrupts)
    #Range to use in for loop for the different segments of time points
    range_of_periods <- 1:num_interrupts
    #For loop to fill in values for treatment group
    per_trt_num_coef <- vector(mode="list", length=num_interrupts)
    for(i in 1:num_interrupts) {
      per_trt_num_coef[[i]] <- (1:(((num_groups * 2) * range_of_periods[i]) + num_baseline_coef + 1))
      #Make a vector with all 0s
      trt_coef_index_list[[i]] <- rep(0, expect_coefs)
      #Creates values of 1 for evens/treatment groups at different interruptions
      trt_coef_index_list[[i]][per_trt_num_coef[[i]][(per_trt_num_coef[[i]]) %% 2 == 0]] <- 1
    }

    ## Control group
    #Use the number of interruptions to make a list to store treated/control indexes
    ctl_coef_index_list <- vector(mode = "list", length = num_interrupts)
    #For loop to fill in values for treatment group
    per_ctl_num_coef <- vector(mode="list", length=num_interrupts)
    for(i in 1:num_interrupts) {
      per_ctl_num_coef[[i]] <- (1:(((num_groups * 2) * range_of_periods[i]) + num_baseline_coef + 1))
      #Make a vector with all 0s
      ctl_coef_index_list[[i]] <- rep(0, expect_coefs)
      #Creates values of 1 for evens/treatment groups at different interruptions
      ctl_coef_index_list[[i]][per_ctl_num_coef[[i]][seq(2, expect_coefs, by=4)]] <- 1
    }

    # Get Difference scores for interruptions #
    # Define a function to add two numbers
    subfnc <- function(x, y) {
      return(abs(x - y))
    }
    # Apply the function using mapply
    dif_coef_index_list <- mapply(subfnc, trt_coef_index_list, ctl_coef_index_list, SIMPLIFY=F)

    # Get the treated sum of coefficients by interruption period #
    Treated <- vector(mode="list", length= num_interrupts)
    Control <- vector(mode="list", length= num_interrupts)
    Difference <- vector(mode="list", length= num_interrupts)
    for (i in 1:num_interrupts) {
      Treated[[i]] <- sum(mod_coefs[per_trt_num_coef[[i]][trt_coef_index_list[[i]]==TRUE]])
      Control[[i]] <- sum(mod_coefs[per_trt_num_coef[[i]][ctl_coef_index_list[[i]]==TRUE]])
      Difference[[i]] <- sum(mod_coefs[per_trt_num_coef[[i]][dif_coef_index_list[[i]]==TRUE]])
    }
    # How many coefficients were added to the standard models #
    add_coefs <- total_coefs - expect_coefs
    # Binary values for matrix calculations #
    mat_coefsT <- vector(mode="list", length= num_interrupts) #Treated
    mat_coefsC <- vector(mode="list", length= num_interrupts) #Control
    mat_coefsD <- vector(mode="list", length= num_interrupts) #Difference
    for (i in 1:num_interrupts) {
      mat_coefsT[[i]] <- matrix(c(trt_coef_index_list[[i]], rep(0, add_coefs)), 1)
      mat_coefsC[[i]] <- matrix(c(ctl_coef_index_list[[i]], rep(0, add_coefs)), 1)
      mat_coefsD[[i]] <- matrix(c(dif_coef_index_list[[i]], rep(0, add_coefs)), 1)
    }
    # Standard errors of each coefficient of main ITS variables #
    stanErrT <- vector(mode="list", length= num_interrupts) #Treated
    stanErrC <- vector(mode="list", length= num_interrupts) #Control
    stanErrD <- vector(mode="list", length= num_interrupts) #Difference
    for (i in 1:num_interrupts) {
      stanErrT[[i]] <- sqrt(mat_coefsT[[i]] %*% model.vcov %*% t(mat_coefsT[[i]]))
      stanErrC[[i]] <- sqrt(mat_coefsC[[i]] %*% model.vcov %*% t(mat_coefsC[[i]]))
      stanErrD[[i]] <- sqrt(mat_coefsD[[i]] %*% model.vcov %*% t(mat_coefsD[[i]]))
    }
    # t statistic values #
    t.valueT <- vector(mode="list", length= num_interrupts) #Treated
    t.valueC <- vector(mode="list", length= num_interrupts) #Control
    t.valueD <- vector(mode="list", length= num_interrupts) #Difference
    for (i in 1:num_interrupts) {
      t.valueT[[i]] <- Treated[[i]]/stanErrT[[i]]
      t.valueC[[i]] <- Control[[i]]/stanErrC[[i]]
      t.valueD[[i]] <- Difference[[i]]/stanErrD[[i]]
    }
    # p-values #
    p.valueT <- vector(mode="list", length= num_interrupts) #Treated
    p.valueC <- vector(mode="list", length= num_interrupts) #Control
    p.valueD <- vector(mode="list", length= num_interrupts) #Difference
    for (i in 1:num_interrupts) {
      p.valueT[[i]] <- (1 - stats::pt(abs(t.valueT[[i]]), its_df_resid)) * 2
      p.valueC[[i]] <- (1 - stats::pt(abs(t.valueC[[i]]), its_df_resid)) * 2
      p.valueD[[i]] <- (1 - stats::pt(abs(t.valueD[[i]]), its_df_resid)) * 2
    }
    # Upper and Lower confidence limits #
    L95T <- vector(mode="list", length= num_interrupts) #Treated
    U95T <- vector(mode="list", length= num_interrupts) #Treated
    L95C <- vector(mode="list", length= num_interrupts) #Control
    U95C <- vector(mode="list", length= num_interrupts) #Control
    L95D <- vector(mode="list", length= num_interrupts) #Difference
    U95D <- vector(mode="list", length= num_interrupts) #Difference
    for (i in 1:num_interrupts) {
      L95T[[i]] <- Treated[[i]] - (stanErrT[[i]] * quant_df_res)
      U95T[[i]] <- Treated[[i]] + (stanErrT[[i]] * quant_df_res)
      L95C[[i]] <- Control[[i]] - (stanErrC[[i]] * quant_df_res)
      U95C[[i]] <- Control[[i]] + (stanErrC[[i]] * quant_df_res)
      L95D[[i]] <- Difference[[i]] - (stanErrD[[i]] * quant_df_res)
      U95D[[i]] <- Difference[[i]] + (stanErrD[[i]] * quant_df_res)
    }

    # Get final data together into the effect list #
    effT <- vector(mode="list", length= num_interrupts) #Treated
    effC <- vector(mode="list", length= num_interrupts) #Control
    effD <- vector(mode="list", length= num_interrupts) #Difference
    for (i in 1:num_interrupts) {
      effT[[i]] <- do.call(cbind.data.frame, list(Treated[[i]], stanErrT[[i]], t.valueT[[i]], p.valueT[[i]], L95T[[i]], U95T[[i]]))
      effC[[i]] <- do.call(cbind.data.frame, list(Control[[i]], stanErrC[[i]], t.valueC[[i]], p.valueC[[i]], L95C[[i]], U95C[[i]]))
      effD[[i]] <- do.call(cbind.data.frame, list(Difference[[i]], stanErrD[[i]], t.valueD[[i]], p.valueD[[i]], L95D[[i]], U95D[[i]]))
    }
    #Temporary column names
    for (i in 1:num_interrupts) {
      colnames(effT[[i]]) <- c("trt", "se","t","p", "l", "u")
      colnames(effC[[i]]) <- c("trt", "se","t","p", "l", "u")
      colnames(effD[[i]]) <- c("trt", "se","t","p", "l", "u")
    }

    #Combine it into dataframes for reach interruption period
    effect_list <- vector(mode="list", length= num_interrupts) #Difference
    for (i in 1:num_interrupts) {
      effect_list[[i]] <- do.call(rbind.data.frame, list(effT[[i]], effC[[i]], effD[[i]]))
    }

    #Make final data frame
    effect_list <- do.call(rbind.data.frame, effect_list)
    #Column names
    colnames(effect_list) <- c("Effect", "S.E.", "t", "p.value", "L95","U95")
    #Official row names
    tnms <- vector(mode="list", length= num_interrupts) #row names
    for (i in 1:num_interrupts) {
      tnms[[i]] <- paste0(c("Intervention", "Control", "Difference"), ".", i)
    }
    #Assign row names
    rownames(effect_list) <- unlist(tnms)
  }
  return(effect_list)
}
