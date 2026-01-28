#' Interpret model output
#'
#' Provides simple interpretations of regression coefficients and Cronbach's
#' alpha from assess and alpha function classes. The interpretations describe
#' coefficients and significance values as well as modifying item scales.
#' The interpretations are text comments associated with specific parameters
#' of the various analyses.
#'
#' @param object alpha and assess class objects: alpha, ITS, DID, linear (ols) or logistic models.
#'
#' @return a list with interpretations of Cronbach's alpha scales or regression model results.
#' @export
#'
#' @examples
#' # Interpret Cronbach's alpha
#' interpret(alpha(items=c("i1","i2","i3","i4","i5"), data=cas))
#'
#' # interpret a standard linear (OLS) regression
#' hos1 <- assess(formula=survey ~ program + month, data=hosprog, regression= "ols")
#' interpret(hos1)$model
#'
#' # interpret a differences-in-differences model
#' hos2 <- assess(formula=survey ~ ., data=hosprog, intervention = "program",
#' int.time="month", treatment = 5, did="two", newdata=TRUE)
#' interpret(hos2)$did  #interpret(hos2) also runs, returns ITS results if present
#'
#' # interpret an interrupted time series model
#' hos3 <- assess(formula=survey ~ ., data=hosprog, intervention = "program",
#' int.time="month", its="two", interrupt = 5)
#' interpret(hos3)$its
interpret <- function(object) {

  if(any(class(object) %in% c("alpha","assess")) == FALSE) {stop("Error: Expecting 'alpha' or 'assess' class object.")}
  # Sets "assess" return objects to NULL for model, DID, ITS
  model <- NULL; did <- NULL; its <- NULL
  #alpha objects
  if("alpha" %in% class(object) ) {
    #Alpha cutoff levels
    if (object$Scale.Statistics$alpha < 0.6) alpha_rank <- "'non-acceptable'"
    if (object$Scale.Statistics$alpha >= 0.6) alpha_rank <- "'acceptable'"
    if (object$Scale.Statistics$alpha >= 0.7) alpha_rank <- "'good'"
    if (object$Scale.Statistics$alpha >= 0.8) alpha_rank <- "'very good'"
    if (object$Scale.Statistics$alpha >= 0.9) alpha_rank <- "'excellent'"
    #Items that are deleted
    delete_items <- names(which(object$Item.Deleted$alpha.item.deleted > object$Scale.Statistics$alpha))
    #Interpretations
    alpha_overall <- paste0("Your ", object$Scale.Statistics$Items, " item scale has a Cronbach\'s alpha of ",
                            round(object$Scale.Statistics$alpha, 2), ". ", "This is \ngenerally considered as being in the ",
                            alpha_rank, " range.")
    descriptives <- paste0("The scale mean is ", round(object$Scale.Statistics$Overall.Mean, 2),
                           " and has a standard deviation of ", round(object$Scale.Statistics$Overall.SD, 2),
                           ".")
    deleted <- paste0("Removing one of these item(s): ", paste(delete_items,collapse=", "),
                      ", can improve the Cronbach\'s \nalpha in a new scale to a higher level than the current alpha \nbased on all items.")
    excluded <- paste0(object$Sample.N$Excluded," row(s) of data excluded from the analysis because of missing \ndata.")
  }
  #Determine which objects are returned
  if(any(class(object) %in% c("alpha"))) interpret_type <- "alpha"
  if(any(class(object) %in% c("assess"))) interpret_type <- "assess"

  # OLS model #
  if("assess" %in% class(object) ) {
    if(object$analysis_type$regression_type == "ols") {
      Y_var_ols <- all.vars(object$formula$primary_formula)[1]
      intercept_col <- grep("Intercept", row.names(summary(object$model)[["coefficients"]]) )
      if(length(which(summary(object$model)[["coefficients"]][-intercept_col, "Pr(>|t|)"] < .05)) > 0) {
        ols_sig_b <- names(which(summary(object$model)[["coefficients"]][-intercept_col, "Pr(>|t|)"] < .05))
      } else {
        ols_sig_b <- "No significant coefficients in your model at the 0.05 alpha level."
      }
      # Determine if there was an increase or decrease in coefficients
      if(length(intersect(names(which(summary(object$model)[["coefficients"]][-intercept_col, "Estimate"] > 0 )), ols_sig_b) > 0)) {
        ols_sig_increase <- intersect(names(which(summary(object$model)[["coefficients"]][-intercept_col, "Estimate"] > 0 )), ols_sig_b)
      } else {
        ols_sig_increase <- "No positive coefficients in your model were significant."
      }
      #Decrease
      if(length(intersect(names(which(summary(object$model)[["coefficients"]][-intercept_col, "Estimate"] < 0 )), ols_sig_b) > 0)) {
        ols_sig_decrease <- intersect(names(which(summary(object$model)[["coefficients"]][-intercept_col, "Estimate"] < 0 )), ols_sig_b)
      } else {
        ols_sig_decrease <- "No negative coefficients in your model were significant."
      }
      ols_r2 <- summary(object$model)$r.squared
      #OLS interpretations
      introduction <- c("These estimates tell you about the relationship between the \nindependent variables and the dependent variable. These estimates \ntell the amount of change in outcome scores that would be \npredicted by a 1 unit increase in the predictor.")
      all_significant <- paste0("The following predictor variable(s) have coefficient(s) \nsignificantly different from 0 using an alpha of 0.05:\n", paste(ols_sig_b, collapse=", "))
      positive_beta <- paste0("For every 1 unit increase in these predictor variables,\n", Y_var_ols, " is predicted to increase by the value of the \ncoefficient, holding all other variables constant. The following \npredictor variable(s) have positive coefficient(s) that \nincrease the predicted value of the outcome: \n", paste(ols_sig_increase, collapse=", "))
      negative_beta <- paste0("For every 1 unit increase in these predictor variables,\n", Y_var_ols, " is predicted to decrease by the value of the \ncoefficient, holding all other variables constant. The following \npredictor variable(s) have negative coefficient(s) that \ndecrease the predicted value of the outcome: \n", paste(ols_sig_decrease, collapse=", "))
      R2 <- paste0("R-Squared (R2) is the proportion of variance in the dependent \nvariable which can be predicted from the independent \nvariable(s). For example, if R2 = 0.50, 50% of the variance \nin test scores can be predicted from the 5 variables. R2 >= 0.80 \nmay be at a level to reliably make individual predictions. \nLower R2 may be helpful in group level predictions. And low R2 can \nstill be adequate for hypothesis testing. This model has a R2 of ", round(ols_r2, 3), ".")
    }
  }
  # Logistic model #
  if("assess" %in% class(object) ) {
    if(object$analysis_type$regression_type == "logistic") {
      Y_var_log <- all.vars(object$formula$primary_formula)[1]
      intercept_col <- grep("Intercept", row.names(summary(object$model)[["coefficients"]]) )
      if(length(which(summary(object$model)[["coefficients"]][-intercept_col, "Pr(>|z|)"] < .05)) > 0) {
        log_sig_b <- names(which(summary(object$model)[["coefficients"]][-intercept_col, "Pr(>|z|)"] < .05))
      } else {
        log_sig_b <- "No significant coefficients in your model at the 0.05 alpha level."
      }
      # Determine if there was an increase or decrease in coefficients
      if(length(intersect(names(which(summary(object$model)[["coefficients"]][-intercept_col, "Estimate"] > 0 )), log_sig_b) > 0)) {
        log_sig_increase <- intersect(names(which(summary(object$model)[["coefficients"]][-intercept_col, "Estimate"] > 0 )), log_sig_b)
      } else {
        log_sig_increase <- "No positive coefficients in your model were significant."
      }
      #Decrease
      if(length(intersect(names(which(summary(object$model)[["coefficients"]][-intercept_col, "Estimate"] < 0 )), log_sig_b) > 0)) {
        log_sig_decrease <- intersect(names(which(summary(object$model)[["coefficients"]][-intercept_col, "Estimate"] < 0 )), log_sig_b)
      } else {
        log_sig_decrease <- "No negative coefficients in your model were significant."
      }
      #Logistic interpretations
      introduction <- c("These estimates tell you about the relationship between the \nindependent variables and the dependent variable. These estimates \ntell the amount of change in outcome scores that would be \npredicted by a 1 unit increase in the predictor.")
      all_significant <- paste0("The following predictor variable(s) have coefficient(s) \nsignificantly different from 0 using an alpha of 0.05:\n", paste(log_sig_b, collapse=", "))
      positive_beta <- paste0("For every 1 unit increase in these predictor variables,\n", Y_var_log, " is predicted to increase by the value of the \ncoefficient, holding all other variables constant. The following \npredictor variable(s) have positive coefficient(s) that \nincrease the predicted value of the outcome: \n", paste(log_sig_increase, collapse=", "))
      negative_beta <- paste0("For every 1 unit increase in these predictor variables,\n", Y_var_log, " is predicted to decrease by the value of the \ncoefficient, holding all other variables constant. The following \npredictor variable(s) have negative coefficient(s) that \ndecrease the predicted value of the outcome: \n", paste(log_sig_decrease, collapse=", "))
      R2 <- "There is no R2 information provided."
    }
  }
  # DID model #
  if("assess" %in% class(object) ) {
    if(object$analysis_type$did_type == "many") {
      Y_var_did <- all.vars(object$formula$DID_formula)[1]
      # Significance test
      did_b1_sig <- ifelse(summary(object$DID)[["coefficients"]][, "Pr(>|t|)"][which(names(coef(object$DID))=="Period")] < .05, "significant", "non-significant")
      did_b2_sig <- ifelse(summary(object$DID)[["coefficients"]][, "Pr(>|t|)"][which(names(coef(object$DID))=="DID")] < .05, "significant", "non-significant")
      did_b3_sig <- ifelse(summary(object$DID)[["coefficients"]][, "Pr(>|t|)"][which(names(coef(object$DID))=="DID.Trend")] < .05, "significant", "non-significant")
      # Determine if there was an increase or decrease in coefficients
      did_b1_change <- ifelse(coef(object$DID)[which(names(coef(object$DID))=="Period")] > 0, "increase", "decrease")
      did_b2_change <- ifelse(coef(object$DID)[which(names(coef(object$DID))=="DID")] > 0, "increase", "decrease")
      did_b3_change <- ifelse(coef(object$DID)[which(names(coef(object$DID))=="DID.Trend")] > 0, "increase", "decrease")
      #many times period interpretations
      B0_coef <- coef(object$DID)[which(names(coef(object$DID))== "(Intercept)")]
      B1_coef <- coef(object$DID)[which(names(coef(object$DID))== "Period")]
      B2_coef <- coef(object$DID)[which(names(coef(object$DID))== "DID")]
      B3_coef <- coef(object$DID)[which(names(coef(object$DID))== "DID.Trend")]
      B_0 <- paste0("The intercept represents the starting point of the control \ngroup's trend line at the baseline period (Time 1): ", round(B0_coef, 3), ".")
      B_1 <- paste0("Period is the change in the control group's ", Y_var_did, " value trend \nline after the baseline period. There was a ", did_b1_sig, " ", did_b1_change, " \nfor the control group after the baseline period: ",round(B1_coef, 3), ".")
      B_2 <- paste0("DID estimates the difference in mean overall level between \nthe intervention and both the non-intervention period/group. \nIn other words, there was a ", did_b2_sig, " ", did_b2_change, " in the \nmean ", Y_var_did, " by ", round(B2_coef, 3)," for the intervention group.")
      B_3 <- paste0("DID.Trend is the difference in the intervention group's \ntrend line after the intervention period started (> Time 1). \nThe intervention group had a ", did_b3_sig, " ", did_b3_change, " in trend \nof the mean ", Y_var_did, " by ", round(B3_coef, 3),  " after the intervention started.")
      did_covariates <- c("If there are additional variables in the model then the coefficients \nabove represent the effects after controlling for the other variables.")
    }
    if(object$analysis_type$did_type == "two") {
      Y_var_did <- all.vars(object$formula$DID_formula)[1]
      # Significance test
      did_b1_sig <- ifelse(summary(object$DID)[["coefficients"]][, "Pr(>|t|)"][which(names(coef(object$DID))=="Post.All")] < .05, "significant", "non-significant")
      did_b2_sig <- ifelse(summary(object$DID)[["coefficients"]][, "Pr(>|t|)"][which(names(coef(object$DID))=="Int.Var")] < .05, "significant", "non-significant")
      did_b3_sig <- ifelse(summary(object$DID)[["coefficients"]][, "Pr(>|t|)"][which(names(coef(object$DID))=="DID")] < .05, "significant", "non-significant")
      # Determine if there was an increase or decrease in coefficients
      did_b1_change <- ifelse(coef(object$DID)[which(names(coef(object$DID))=="Post.All")] > 0, "increase", "decrease")
      did_b2_change <- ifelse(coef(object$DID)[which(names(coef(object$DID))=="Int.Var")] > 0, "increase", "decrease")
      did_b3_change <- ifelse(coef(object$DID)[which(names(coef(object$DID))=="DID")] > 0, "increase", "decrease")
      # 2 time periods
      B0_coef <- coef(object$DID)[which(names(coef(object$DID))== "(Intercept)")]
      B1_coef <- coef(object$DID)[which(names(coef(object$DID))== "Post.All")]
      B2_coef <- coef(object$DID)[which(names(coef(object$DID))== "Int.Var")]
      B3_coef <- coef(object$DID)[which(names(coef(object$DID))=="DID")]
      B_0 <- paste0("The intercept represents the mean ", Y_var_did, " value of the \ncontrol group at the baseline period (Time 1): ", round(B0_coef, 3), ".")
      B_1 <- paste0("Post.All is the change in the control group\'s ", Y_var_did, " \nvalue in the 2nd time period (Time 2). There was a \n", did_b1_sig, " ", did_b1_change," for the control group \nat time 2: ", round(B1_coef, 3), ".")
      B_2 <- paste0("Int.Var is the difference between the intervention \nand control group at the baseline period (Time 1). The \nintervention group had a ", did_b2_sig, " ", did_b2_change, " in the \nmean ", Y_var_did, " value compared to the control group: ", round(B2_coef, 3), ".")
      B_3 <- paste0("DID estimates the average treatment effect on the \ntreated group (ATET). This interaction represents the \ndifference in the trend differences for the intervention and \ncontrol groups: \n(Int. Time 2 - Int. Time 1) - (Ctl. Time 2 - Ctl. Time 1) = ", round(B3_coef, 3), ".", " ", " \nIn other words, there was a ", did_b3_sig, " ", did_b3_change," in the \nmean ", Y_var_did, " trend by ", round(B3_coef, 3)," for the intervention group.")
      did_covariates <- c("If there are additional variables in the model then the coefficients \nabove represent the effects after controlling for the other variables.")
    }
  }

  # ITSA model #
  if("assess" %in% class(object) ) {
    if(object$analysis_type$itsa_type == "sgst") {
      Y_var_its <- all.vars(object$formula$ITS_formula)[1]
      X_var_its <- all.vars(object$formula$ITS_formula)[-1][1:3]
      # Coefficients
      its_b0_coef <- summary(object$ITS)[["coefficients"]][1, "Estimate"]
      its_b1_coef <- summary(object$ITS)[["coefficients"]][2, "Estimate"]
      its_b2_coef <- summary(object$ITS)[["coefficients"]][3, "Estimate"]
      its_b3_coef <- summary(object$ITS)[["coefficients"]][4, "Estimate"]
      Smry_int_coef <- object$ITS.Effects[1, "Effect"]
      # Significance test
      its_b0_sig <- ifelse(summary(object$ITS)[["coefficients"]][1, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b1_sig <- ifelse(summary(object$ITS)[["coefficients"]][2, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b2_sig <- ifelse(summary(object$ITS)[["coefficients"]][3, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b3_sig <- ifelse(summary(object$ITS)[["coefficients"]][4, "Pr(>|t|)"] < .05, "significant", "non-significant")
      Smry_int_sig <- ifelse(object$ITS.Effects[1, "p.value"] < .05, "significant", "non-significant")
      # Interpretations
      its_intro <- c("Note: Some variable names below based on time points (or 'interruptions'). \nThis analysis is for a one-group, single intervention period (interruption).")
      B0 <- paste0("Intercept is ", round(its_b0_coef, 3), " and the starting value of the trend \nfor the intervention group.")
      B1 <- paste0("ITS.Time is ", round(its_b1_coef, 3), " and the slope prior to intervention. \nThe coefficient is ", its_b1_sig, ".")
      B2 <- paste0(X_var_its[2], " is ", round(its_b2_coef, 3), " and the immediate shift in the trend line \nafter the intervention start (e.g., 1st year of intervention). \nThe coefficient is ", its_b2_sig, ".")
      B3 <- paste0(X_var_its[3], " is ", round(its_b3_coef, 3), " and the difference between pre- and \npost-intervention slopes (e.g., change in the pre-intervention \nslope). The coefficient is ", its_b3_sig,".")
      its_Summary <- paste0("Summary: The results show that after the start of the intervention, \nthere was a ", Smry_int_sig, " change in the ", Y_var_its, " trend. This gives \na total post-intervention trend in the ", Y_var_its, " of ", round(Smry_int_coef, 3), " \nover time (i.e., the total combined value of change not the \nchange relative to pre-intervention).")
      its_covariates <- c("If there are additional variables in the model then the coefficients \nabove represent effects after controlling for the other variables.")
    }
    # mgst
    if(object$analysis_type$itsa_type == "mgst") {
      Y_var_its <- all.vars(object$formula$ITS_formula)[1]
      X_var_its <- all.vars(object$formula$ITS_formula)[-1][1:7]
      # Coefficients
      its_b0_coef <- summary(object$ITS)[["coefficients"]][1, "Estimate"]
      its_b1_coef <- summary(object$ITS)[["coefficients"]][2, "Estimate"]
      its_b2_coef <- summary(object$ITS)[["coefficients"]][3, "Estimate"]
      its_b3_coef <- summary(object$ITS)[["coefficients"]][4, "Estimate"]
      its_b4_coef <- summary(object$ITS)[["coefficients"]][5, "Estimate"]
      its_b5_coef <- summary(object$ITS)[["coefficients"]][6, "Estimate"]
      its_b6_coef <- summary(object$ITS)[["coefficients"]][7, "Estimate"]
      its_b7_coef <- summary(object$ITS)[["coefficients"]][8, "Estimate"]
      Smry_int_coef <- object$ITS.Effects[1, "Effect"]
      Smry_con_coef <- object$ITS.Effects[2, "Effect"]
      Smry_diff_coef <- object$ITS.Effects[3, "Effect"]
      # Significance test
      its_b0_sig <- ifelse(summary(object$ITS)[["coefficients"]][1, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b1_sig <- ifelse(summary(object$ITS)[["coefficients"]][2, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b2_sig <- ifelse(summary(object$ITS)[["coefficients"]][3, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b3_sig <- ifelse(summary(object$ITS)[["coefficients"]][4, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b4_sig <- ifelse(summary(object$ITS)[["coefficients"]][5, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b5_sig <- ifelse(summary(object$ITS)[["coefficients"]][6, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b6_sig <- ifelse(summary(object$ITS)[["coefficients"]][7, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b7_sig <- ifelse(summary(object$ITS)[["coefficients"]][8, "Pr(>|t|)"] < .05, "significant", "non-significant")
      Smry_int_sig <- ifelse(object$ITS.Effects[1, "p.value"] < .05, "significant", "non-significant")
      Smry_con_sig <- ifelse(object$ITS.Effects[2, "p.value"] < .05, "significant", "non-significant")
      Smry_diff_sig <- ifelse(object$ITS.Effects[3, "p.value"] < .05, "significant", "non-significant")
      # Interpretations
      its_intro <- c("Note: Some variable names below based on time points (or 'interruptions'). \nThis analysis is for a two-group, single intervention period (interruption). \nPositive values indicate higher intervention group values and vice-versa for: \npost1, txp1, ixp1, txip1.")
      B0 <- paste0("Intercept is ", round(its_b0_coef, 3), " and the starting value of the trend for the \ncontrol group.")
      B1 <- paste0("ITS.Time is ", round(its_b1_coef, 3), " and the control group\'s slope prior to intervention. \nThe coefficient is ", its_b1_sig, ".")
      B2 <- paste0("ITS.Int is ", round(its_b2_coef, 3), " and the difference in the level between intervention \nand control group prior to intervention (intervention - control). \nThe coefficient is ", its_b2_sig, ".")
      B3 <- paste0("txi is ", round(its_b3_coef, 3), " and the difference between the intervention and \ncontrol group\'s pre-intervention slopes (intervention - control). \nThe coefficient is ", its_b3_sig, ".")
      B4 <- paste0(X_var_its[4], " is ", round(its_b4_coef, 3), " and the immediate shift in the control group \ntrend line after the intervention start. The coefficient is \n", its_b4_sig, ".")
      B5 <- paste0(X_var_its[5], " is ", round(its_b5_coef, 3), " and the difference between pre- and post- \ncontrol group slopes (e.g., change in the pre-intervention \nslope). The coefficient is ", its_b5_sig, ".")
      B6 <- paste0(X_var_its[6], " is ", round(its_b6_coef, 3), " and the difference between the intervention and \ncontrol groups (intervention - control) in the period immediately \nafter the intervention started (e.g., 1st year of intervention). \nThe coefficient is ", its_b6_sig, ".")
      B7 <- paste0(X_var_its[7], " is ", round(its_b7_coef, 3), " and ", its_b7_sig, "."," This is the difference in both \ngroup\'s slope changes since pre-intervention (pre-slopes compared \nto post-slopes). For example, both have pre-intervention slopes \nof 2, the control group\'s slope remained the same, therefore the \npost-intervention slope is 0. And the intervention group's slope \nincreased by 2, then txip1 = 2 (= 2 - 0).")
      its_Summary <- paste0("Summary: For the intervention period, the results show that the \nintervention group\'s ", Smry_int_sig, " shift in ",Y_var_its, ", \npost-intervention was ", round(Smry_int_coef, 3), ". The control group\'s ",Smry_con_sig, " \nshift in ",Y_var_its, ", post-intervention was ", round(Smry_con_coef, 3), ". The ", Smry_diff_sig, " \ndifference between both groups is ", round(Smry_diff_coef, 3), ".")
      its_covariates <- c("If there are additional variables in the model then the coefficients \nabove represent effects after controlling for the other variables.")
    }
    # sgmt
    if(object$analysis_type$itsa_type == "sgmt") {
      # Get specific values #
      Y_var_its <- all.vars(object$formula$ITS_formula)[1]
      X_var_its <- object[["ITS.Names"]] # main causal model terms
      #Full list of variables for later coefficient names
      mainvars2 <- c("(Intercept)", X_var_its)
      #Get interruptions
      interruptions <- object$study$interrupt
      # Number of expected main ITS coefficients
      expect_coefs <- length(mainvars2)
      # Coefficients
      its_b0_coef <- summary(object$ITS)[["coefficients"]][1, "Estimate"]
      its_b1_coef <- summary(object$ITS)[["coefficients"]][2, "Estimate"]
      # Make for loops for types of variables #
      post_its_coef <- vector(mode="numeric", length=length(interruptions))
      txp_its_coef <- vector(mode="numeric", length=length(interruptions))
      for(i in 1:length(interruptions)) {
        post_its_coef[i] <- summary(object$ITS)[["coefficients"]][seq(3, expect_coefs, by=2)[i], "Estimate"]
        txp_its_coef[i] <- summary(object$ITS)[["coefficients"]][seq(4, expect_coefs, by=2)[i], "Estimate"]
      }
      # Give names for the elements of the types of variables #
      for(i in 1:length(interruptions)) {
        names(post_its_coef)[i] <- paste0("its_b", seq(3, expect_coefs, by=2)[i]-1, "_coef")
        names(txp_its_coef)[i] <-  paste0("its_b", seq(4, expect_coefs, by=2)[i]-1, "_coef")
      }
      ## Summary of ITS effects ##
      Smry_int_coef <- object$ITS.Effects[, "Effect"]
      # Significance test for coefficients #
      its_b_sig <- ifelse(summary(object$ITS)[["coefficients"]][, "Pr(>|t|)"] < .05, "significant", "non-significant")
      # Significance test for ITS effects #
      Smry_int_sig <- ifelse(object$ITS.Effects[, "p.value"] < .05, "significant", "non-significant")
      # Interpretations #
      its_intro <- c("Note: Some variable names below based on time points (or 'interruptions'). \nThis analysis is for a two-group, single intervention period (interruption). \nPositive values indicate higher intervention group values and vice-versa for: \npost1, txp1, ixp1, txip1, post2, txp2, ixp2, txip2.")
      # These are static interpretations that are NOT repeated
      B0 <- paste0("Intercept is ", round(its_b0_coef, 3), " and the starting value of the trend for the \ncontrol group.")
      B1 <- paste0("ITS.Time is ", round(its_b1_coef, 3), " and the group\'s slope prior to intervention. \nThe coefficient is ", its_b_sig[2], ".")
      # Interpretations that come from a for loop #
      # post
      post_interpret <- vector(mode="character", length=length(interruptions))
      for(i in 1:length(interruptions)) {
        post_interpret[i] <- paste0(mainvars2[seq(3, expect_coefs, by=2)][i], " is ", round(post_its_coef[i], 3), " and the immediate shift in the group trend \nline after this intervention time starts. The coefficient is \n", its_b_sig[seq(3, expect_coefs, by=2)][i], ".")
      }
      # txp
      txp_interpret <- vector(mode="character", length=length(interruptions))
      for(i in 1:length(interruptions)) {
        txp_interpret[i] <- paste0(mainvars2[seq(4, expect_coefs, by=2)][i], " is ", round(txp_its_coef[i], 3), " and the difference between current and prior intervention \ngroup slopes (e.g., change in the pre-intervention slope). \nThe coefficient is ", its_b_sig[seq(4, expect_coefs, by=2)][i], ".")
      }
      ## Intervention period effect ##
      its_Summary <- vector(mode="character", length=length(interruptions))
      for(i in 1:length(interruptions)) {
        its_Summary[i] <- paste0("Summary ", i, ": For this intervention period ", i, ", ", "the results show that \nthe group\'s ", Smry_int_sig[i], " shift in ", Y_var_its, ", post intervention \nwas ", round(Smry_int_coef[i], 3), " (e.g., the total combined value of change for later \nperiods not the change relative to solely the prior period).")
      }
      # additional variables
      its_covariates <- c("If there are additional variables in the model then the coefficients \nabove represent effects after controlling for the other variables.")
    }
    # mgmt
    if(object$analysis_type$itsa_type == "mgmt") {
      # Get specific values #
      Y_var_its <- all.vars(object$formula$ITS_formula)[1]
      X_var_its <- object[["ITS.Names"]] # main causal model terms
      #Full list of variables for later coefficient names
      mainvars2 <- c("(Intercept)", X_var_its)
      #Get interruptions
      interruptions <- object$study$interrupt
      # Number of expected main ITS coefficients
      expect_coefs <- length(mainvars2)
      # Coefficients
      its_b0_coef <- summary(object$ITS)[["coefficients"]][1, "Estimate"]
      its_b1_coef <- summary(object$ITS)[["coefficients"]][2, "Estimate"]
      its_b2_coef <- summary(object$ITS)[["coefficients"]][3, "Estimate"]
      its_b3_coef <- summary(object$ITS)[["coefficients"]][4, "Estimate"]
      # Make for loops for types of variables #
      post_its_coef <- vector(mode="numeric", length=length(interruptions))
      txp_its_coef <- vector(mode="numeric", length=length(interruptions))
      ixp_its_coef <- vector(mode="numeric", length=length(interruptions))
      txip_its_coef <- vector(mode="numeric", length=length(interruptions))
      for(i in 1:length(interruptions)) {
        post_its_coef[i] <- summary(object$ITS)[["coefficients"]][seq(5, expect_coefs, by=4)[i], "Estimate"]
        txp_its_coef[i] <- summary(object$ITS)[["coefficients"]][seq(6, expect_coefs, by=4)[i], "Estimate"]
        ixp_its_coef[i] <- summary(object$ITS)[["coefficients"]][seq(7, expect_coefs, by=4)[i], "Estimate"]
        txip_its_coef[i] <- summary(object$ITS)[["coefficients"]][seq(8, expect_coefs, by=4)[i], "Estimate"]
      }
      # Give names for the elements of the types of variables #
      for(i in 1:length(interruptions)) {
        names(post_its_coef)[i] <- paste0("its_b", seq(5, expect_coefs, by=4)[i]-1, "_coef")
        names(txp_its_coef)[i] <-  paste0("its_b", seq(6, expect_coefs, by=4)[i]-1, "_coef")
        names(ixp_its_coef)[i] <-  paste0("its_b", seq(7, expect_coefs, by=4)[i]-1, "_coef")
        names(txip_its_coef)[i] <-  paste0("its_b", seq(8, expect_coefs, by=4)[i]-1, "_coef")
      }
      ## Summary of ITS effects ##
      Smry_int_coef <- vector(mode="numeric", length=length(interruptions))
      Smry_con_coef <- vector(mode="numeric", length=length(interruptions))
      Smry_diff_coef <- vector(mode="numeric", length=length(interruptions))
      for(i in 1:length(interruptions)) {
        Smry_int_coef[i] <- object$ITS.Effects[seq(1, length(interruptions)*3, by=3)[i], "Effect"]
        Smry_con_coef[i] <- object$ITS.Effects[seq(2, length(interruptions)*3, by=3)[i], "Effect"]
        Smry_diff_coef[i] <- object$ITS.Effects[seq(3, length(interruptions)*3, by=3)[i], "Effect"]
      }
      # Significance test for coefficients #
      its_b_sig <- ifelse(summary(object$ITS)[["coefficients"]][, "Pr(>|t|)"] < .05, "significant", "non-significant")
      # Significance test for ITS effects #
      Smry_sig_all <- ifelse(object$ITS.Effects[, "p.value"] < .05, "significant", "non-significant")
      # Make vectors for intervention, control, and difference values
      Smry_int_sig <- vector(mode="character", length=length(interruptions))
      Smry_con_sig <- vector(mode="character", length=length(interruptions))
      Smry_diff_sig <- vector(mode="character", length=length(interruptions))
      for(i in 1:length(interruptions)) {
        Smry_int_sig[i] <- Smry_sig_all[seq(1, length(Smry_sig_all), by=3)][i]
        Smry_con_sig[i] <- Smry_sig_all[seq(2, length(Smry_sig_all), by=3)][i]
        Smry_diff_sig[i] <- Smry_sig_all[seq(3, length(Smry_sig_all), by=3)][i]
      }
      # Interpretations #
      its_intro <- c("Note: Some variable names below based on time points (or 'interruptions'). \nThis analysis is for a two-group, single intervention period (interruption). \nPositive values indicate higher intervention group values and vice-versa for: \npost1, txp1, ixp1, txip1, post2, txp2, ixp2, txip2.")
      # These are static interpretations that are NOT repeated
      B0 <- paste0("Intercept is ", round(its_b0_coef, 3), " and the starting value of the trend for the \ncontrol group.")
      B1 <- paste0("ITS.Time is ", round(its_b1_coef, 3), " and the control group\'s slope prior to intervention. \nThe coefficient is ", its_b_sig[2], ".")
      B2 <- paste0("ITS.Int is ", round(its_b2_coef, 3), " and the difference in the level between intervention \nand control group prior to intervention 1 (intervention - control). \nThe coefficient is ", its_b_sig[3], ".")
      B3 <- paste0("txi is ", round(its_b3_coef, 3), " and the difference between the intervention and \ncontrol group\'s pre-intervention slopes (intervention - control). \nThe coefficient is ", its_b_sig[4], ".")
      # Interpretations that come from a for loop #
      # post
      post_interpret <- vector(mode="character", length=length(interruptions))
      for(i in 1:length(interruptions)) {
        post_interpret[i] <- paste0(mainvars2[seq(5, expect_coefs, by=4)][i], " is ", round(post_its_coef[i], 3), " and the immediate shift in the control group trend \nline after this intervention time starts. The coefficient is \n", its_b_sig[seq(5, expect_coefs, by=4)][i], ".")
      }
      # txp
      txp_interpret <- vector(mode="character", length=length(interruptions))
      for(i in 1:length(interruptions)) {
        txp_interpret[i] <- paste0(mainvars2[seq(6, expect_coefs, by=4)][i], " is ", round(txp_its_coef[i], 3), " and the difference between current and prior intervention \ncontrol group slopes (e.g., change in the pre-intervention slope). \nThe coefficient is ", its_b_sig[seq(6, expect_coefs, by=4)][i], ".")
      }
      # ixp
      ixp_interpret <- vector(mode="character", length=length(interruptions))
      for(i in 1:length(interruptions)) {
        ixp_interpret[i] <- paste0(mainvars2[seq(7, expect_coefs, by=4)][i], " is ", round(ixp_its_coef[i], 3), " and the difference between the intervention and \ncontrol groups (intervention - control) in the period immediately \nafter this intervention started (e.g., 1st year of intervention 1). \nThe coefficient is ", its_b_sig[seq(7, expect_coefs, by=4)][i], ".")
      }
      # txip
      txip_interpret <- vector(mode="character", length=length(interruptions))
      for(i in 1:length(interruptions)) {
        txip_interpret[i] <- paste0(mainvars2[seq(8, expect_coefs, by=4)][i], " is ", round(txip_its_coef[i], 3), " and ", its_b_sig[seq(8, expect_coefs, by=4)][i], "."," This is the difference in both \ngroup\'s slope changes since the prior intervention (pre-slopes compared \nto post-slopes). For example, both have pre-intervention slopes \nof 2, the control group\'s slope remained the same, therefore the \npost 1st intervention slope is 0. And the intervention group's slope \nincreased by 2, then txip1 = 2 (= 2 - 0).")
      }
      ## Intervention period effect ##
      its_Summary <- vector(mode="character", length=length(interruptions))
      for(i in 1:length(interruptions)) {
        its_Summary[i] <- paste0("Summary ", i, ": For this intervention period ", i, ", ", "the results show that the \nintervention group\'s ", Smry_int_sig[i], " shift in ", Y_var_its, ", \npost intervention was ", round(Smry_int_coef[i], 3), ". The control group\'s ",Smry_con_sig[i], " \nshift in ", Y_var_its, ", post intervention was ", round(Smry_con_coef[i], 3), ". The ", Smry_diff_sig[i], " \ndifference between both groups is ", round(Smry_diff_coef[i], 3), ".")
      }
      # additional variables
      its_covariates <- c("If there are additional variables in the model then the coefficients \nabove represent effects after controlling for the other variables.")
    }
  }
  #Return
  if (interpret_type == "alpha") {
    z <- list(alpha_overall=alpha_overall, descriptives=descriptives, deleted=deleted,
              excluded=excluded)
    class(z) <- c("interpret", "alpha", "ham", "list")
    return(z)
  }
  if (interpret_type == "assess") {
    if(object$analysis_type$regression_type %in% c("ols","logistic")) {
      model <- list(introduction=introduction, all_significant=all_significant,
                    positive_beta=positive_beta, negative_beta=negative_beta, R2=R2)
    }
    if(object$analysis_type$did_type %in% c("two","many")) {
      did <- list(B_0=B_0, B_1=B_1, B_2=B_2, B_3=B_3, did_covariates=did_covariates)
    }  #"sgst", "sgmt", "mgst", "mgmt"
    if(object$analysis_type$itsa_type == "sgst") {
      its <- list(its_intro=its_intro, B0=B0, B1=B1, B2=B2, B3=B3,
                  its_Summary=its_Summary, its_covariates=its_covariates)
    }
    if(object$analysis_type$itsa_type == "mgst") {
      its <- list(its_intro=its_intro, B0=B0, B1=B1, B2=B2, B3=B3,
                  B4=B4,B5=B5,B6=B6,B7=B7, its_Summary=its_Summary,
                  its_covariates=its_covariates)
    }
    if(object$analysis_type$itsa_type == "sgmt") {
      its <- list(its_intro=its_intro, B0=B0, B1=B1, post_interpret=post_interpret,
                  txp_interpret=txp_interpret, its_Summary=its_Summary, its_covariates=its_covariates)
    }
    if(object$analysis_type$itsa_type == "mgmt") {
      its <- list(its_intro=its_intro, B0=B0, B1=B1, B2=B2, B3=B3,
                  post_interpret=post_interpret, txp_interpret=txp_interpret, ixp_interpret=ixp_interpret,
                  txip_interpret=txip_interpret, its_Summary=its_Summary, its_covariates=its_covariates)
    }
  }
  # Regression models
  if (interpret_type == "assess") {
    z <- list(model=model, did=did, its=its)
    if(!is.null(z$model)) {
      class(z$model) <- c("interpret", "assess","model", "ham", "list")
    }
    if(!is.null(z$did)) {
      class(z$did) <- c("interpret", "assess", "did","ham", "list")
    }
    if(!is.null(z$its)) {
      if(object$analysis_type$itsa_type == "sgst") {
        class(z$its) <- c("interpret", "assess", "its","sgst","ham", "list")
      }
    }
    if(!is.null(z$its)) {
      if(object$analysis_type$itsa_type == "mgst") {
        class(z$its) <- c("interpret", "assess", "its","mgst","ham", "list")
      }
    }
    if(!is.null(z$its)) {
      if(object$analysis_type$itsa_type == "sgmt") {
        class(z$its) <- c("interpret", "assess", "its","sgmt","ham", "list")
      }
    }
    if(!is.null(z$its)) {
      if(object$analysis_type$itsa_type == "mgmt") {
        class(z$its) <- c("interpret", "assess", "its","mgmt","ham", "list")
      }
    }
    return(z)
  }

}
