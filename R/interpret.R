#' Interpret assess model output
#'
#' @param object assess class objects: alpha, ITS, DID, or ols or logistic models.
#'
#' @return a list with interpretations of alpha scales or regression model results.
#' @export
#'
#' @examples
#' interpret(alpha(items=c("i1","i2","i3","i4","i5"), data=cas))
#'
#' hos2 <- assess(formula=survey ~ ., data=hosprog, intervention = "program",
#' int.time="month", treatment = 5, did="two", newdata=TRUE)
#' interpret(hos2)$did  #interpret(hos2) also runs, returns ITS results if present
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
      positive_beta <- paste0("For every 1 unit increase in these predictor variable, the \noutcome is predicted to increase by the value of the \ncoefficient, holding all other variables constant. The following \npredictor variable(s) have positive coefficient(s) that \nincrease the predicted value of the outcome: \n", paste(ols_sig_increase, collapse=", "))
      negative_beta <- paste0("For every 1 unit increase in these predictor variable, the \noutcome is predicted to decrease by the value of the \ncoefficient, holding all other variables constant. The following \npredictor variable(s) have negative coefficient(s) that \ndecrease the predicted value of the outcome: \n", paste(ols_sig_decrease, collapse=", "))
      R2 <- paste0("R-Squared (R2) is the proportion of variance in the dependent \nvariable which can be predicted from the independent \nvariable(s). For example, if R2 = 0.50, 50% of the variance \nin test scores can be predicted from the 5 variables. R2 >= 0.80 \nmay be at a level to reliably make individual predictions. \nLower R2 may be helpful in group level predictions. And low R2 can \nstill be adequate for hypothesis testing. This model has a R2 of ", round(ols_r2, 3), ".")
    }
  }
  # Logistic model #
  if("assess" %in% class(object) ) {
    if(object$analysis_type$regression_type == "logistic") {
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
      positive_beta <- paste0("For every 1 unit increase in these predictor variables, the \noutcome is predicted to increase by the value of the \ncoefficient, holding all other variables constant. The following \npredictor variable(s) have positive coefficient(s) that \nincrease the predicted value of the outcome: \n", paste(log_sig_increase, collapse=", "))
      negative_beta <- paste0("For every 1 unit increase in these predictor variables, the \noutcome is predicted to decrease by the value of the \ncoefficient, holding all other variables constant. The following \npredictor variable(s) have negative coefficient(s) that \ndecrease the predicted value of the outcome: \n", paste(log_sig_decrease, collapse=", "))
      R2 <- "There is no R2 information provided."
    }
  }
  # DID model #
  if("assess" %in% class(object) ) {
    if(object$analysis_type$did_type == "many") {
      # Significance test
      did_b1_sig <- ifelse(summary(object$DID)[["coefficients"]][, "Pr(>|t|)"][which(names(coef(object$DID))=="Period")] < .05, "significant", "non-significant")
      did_b2_sig <- ifelse(summary(object$DID)[["coefficients"]][, "Pr(>|t|)"][which(names(coef(object$DID))=="DID")] < .05, "significant", "non-significant")
      did_b3_sig <- ifelse(summary(object$DID)[["coefficients"]][, "Pr(>|t|)"][which(names(coef(object$DID))=="DID.Trend")] < .05, "significant", "non-significant")
      # Determine if there was an increase or decrease in coefficients
      did_b1_change <- ifelse(coef(object$DID)[which(names(coef(object$DID))=="Period")] > 0, "increase", "decrease")
      did_b2_change <- ifelse(coef(object$DID)[which(names(coef(object$DID))=="DID")] > 0, "increase", "decrease")
      did_b3_change <- ifelse(coef(object$DID)[which(names(coef(object$DID))=="DID.Trend")] > 0, "increase", "decrease")
      #many time period interpretations
      B2_coef <- coef(object$DID)[which(names(coef(object$DID))== "DID")]
      B3_coef <- coef(object$DID)[which(names(coef(object$DID))== "DID.Trend")]
      B_0 <- paste0("The intercept represents the starting point of the control \ngroup's trend line at the baseline period (Time 1).")
      B_1 <- paste0("Period is the change in the control group's outcome value \ntrend line after the baseline period. There was a ", did_b1_sig, " \n", did_b1_change, " for the control group after the baseline period.")
      B_2 <- paste0("DID estimates the average treatment effect on the treated \ngroup (ATET). This represents the difference in the mean \noverall level between the intervention and control groups. \nIn other words, there was a ", did_b2_sig, " ", did_b2_change, " in the \nmean outcome by ", round(B2_coef, 3)," for the intervention group.")
      B_3 <- paste0("DID.Trend is the difference in the intervention group's \ntrend line after the intervention period started (> Time 1). \nThe intervention group had a ", did_b3_sig, " ", did_b3_change, " in trend \nof the mean outcome by ", round(B3_coef, 3),  " after the intervention started.")
      did_covariates <- c("If there are additional variables in the model then the coefficients above represent effects after controlling for the other variables.")
    }
    if(object$analysis_type$did_type == "two") {
      # Significance test
      did_b1_sig <- ifelse(summary(object$DID)[["coefficients"]][, "Pr(>|t|)"][which(names(coef(object$DID))=="Post.All")] < .05, "significant", "non-significant")
      did_b2_sig <- ifelse(summary(object$DID)[["coefficients"]][, "Pr(>|t|)"][which(names(coef(object$DID))=="Int.Var")] < .05, "significant", "non-significant")
      did_b3_sig <- ifelse(summary(object$DID)[["coefficients"]][, "Pr(>|t|)"][which(names(coef(object$DID))=="DID")] < .05, "significant", "non-significant")
      # Determine if there was an increase or decrease in coefficients
      did_b1_change <- ifelse(coef(object$DID)[which(names(coef(object$DID))=="Post.All")] > 0, "increase", "decrease")
      did_b2_change <- ifelse(coef(object$DID)[which(names(coef(object$DID))=="Int.Var")] > 0, "increase", "decrease")
      did_b3_change <- ifelse(coef(object$DID)[which(names(coef(object$DID))=="DID")] > 0, "increase", "decrease")
      # 2 time periods
      B3_coef <- coef(object$DID)[which(names(coef(object$DID))=="DID")]
      B_0 <- paste0("The intercept represents the mean outcome value of \nthe control group at the baseline period (Time 1).")
      B_1 <- paste0("Post.All is the change in the control group\'s outcome \nvalue in the 2nd time period (Time 2). There was a \n", did_b1_sig, " ", did_b1_change," for the control group \nat time 2.")
      B_2 <- paste0("Int.Var is the difference between the intervention \nand control group at the baseline period (Time 1). The \nintervention group had a ", did_b2_sig, " ", did_b2_change, " in the \nmean outcome value compared to the control group.")
      B_3 <- paste0("DID estimates the average treatment effect on the \ntreated group (ATET). This interaction represents the \ndifference in the trend differences for the intervention and \ncontrol groups: \n(Int. Time 2 - Int. Time 1) - (Ctl. Time 2 - Ctl. Time 1) = ", round(B3_coef, 3), ".", " ", " \nIn other words, there was a ", did_b3_sig, " ", did_b3_change," in the \nmean outcome trend by ", round(B3_coef, 3)," for the intervention group.")
      did_covariates <- c("If there are additional variables in the model then the coefficients above represent effects after controlling for the other variables.")
    }
  }

  # ITSA model #
  if("assess" %in% class(object) ) {
    if(object$analysis_type$itsa_type == "sgst") {
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
      its_intro <- c("Note: Variable names below based on time points (or 'interruptions') \nwon't match identically with the output (e.g., if time 1 = 12 months, \nthe corresponding variables are post1 = post12). \nThis analysis is for a one-group, single intervention period (interruption).")
      B0 <- paste0("Intercept is ", round(its_b0_coef, 3), " and the starting value of the trend \nfor the intervention group.")
      B1 <- paste0("ITS.Time is ", round(its_b1_coef, 3), " and the slope prior to intervention. \nThe coefficient is ", its_b1_sig, ".")
      B2 <- paste0("post1 is ", round(its_b2_coef, 3), " and the immediate shift in the trend line \nafter the intervention start (e.g., 1st year of intervention). \nThe coefficient is ", its_b2_sig, ".")
      B3 <- paste0("txp1 is ", round(its_b3_coef, 3), " and the difference between pre- and \npost-intervention slopes (e.g., change in the pre-intervention \nslope). The coefficient is ", its_b3_sig,".")
      its_Summary <- paste0("Summary: The results show that after the start of the intervention, \nthere was a ", Smry_int_sig, " change in the outcome trend. This gives \na total post-intervention trend in the outcome of ", round(Smry_int_coef, 3), " \nover time (i.e., the total combined value of change not the \nchange relative to pre-intervention).")
      its_covariates <- c("If there are additional variables in the model then the coefficients \nabove represent effects after controlling for the other variables.")
    }
    # mgst
    if(object$analysis_type$itsa_type == "mgst") {
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
      its_intro <- c("Note: Variable names below based on time points (or 'interruptions') \nwon't match identically with the output (e.g., if time 1 = 12 months, \nThis analysis is for a two-group, single intervention period (interruption). \nPositive values indicate higher intervention group values and vice-versa for: \npost1, txp1, ixp1, txip1.")
      B0 <- paste0("Intercept is ", round(its_b0_coef, 3), " and the starting value of the trend for the \ncontrol group.")
      B1 <- paste0("ITS.Time is ", round(its_b1_coef, 3), " and the control group\'s slope prior to intervention. \nThe coefficient is ", its_b1_sig, ".")
      B2 <- paste0("ITS.Int is ", round(its_b2_coef, 3), " and the immediate shift in the control group \ntrend line after the intervention start. The coefficient is \n", its_b2_sig, ".")
      B3 <- paste0("txi is ", round(its_b3_coef, 3), " and the difference between pre- and post- \ncontrol group slopes (e.g., change in the pre-intervention \nslope). The coefficient is ", its_b3_sig, ".")
      B4 <- paste0("post1 is ", round(its_b4_coef, 3), " and the difference in the level between intervention \nand control group prior to intervention (intervention - control). \nThe coefficient is ", its_b4_sig, ".")
      B5 <- paste0("txp1 is ", round(its_b5_coef, 3), " and the difference between the intervention and \ncontrol group\'s pre-intervention slopes (intervention - control). \nThe coefficient is ", its_b5_sig, ".")
      B6 <- paste0("ixp1 is ", round(its_b6_coef, 3), " and the difference between the intervention and \ncontrol groups (intervention - control) in the period immediately \nafter the intervention started (e.g., 1st year of intervention). \nThe coefficient is ", its_b6_sig, ".")
      B7 <- paste0("txip1 is ", round(its_b7_coef, 3), " and ", its_b7_sig, "."," This is the difference in both \ngroup\'s slope changes since pre-intervention (pre-slopes compared \nto post-slopes). For example, both have pre-intervention slopes \nof 2, the control group\'s slope remained the same, therefore the \npost-intervention slope is 0. And the intervention group's slope \nincreased by 2, then txip1 = 2 (= 2 - 0).")
      its_Summary <- paste0("Summary: For the intervention period, the results show that the \nintervention group\'s ", Smry_int_sig, " shift in outcome, \npost-intervention was ", round(Smry_int_coef, 3), ". The control group\'s ",Smry_con_sig, " \nshift in outcome, post-intervention was ", round(Smry_con_coef, 3), ". The ", Smry_diff_sig, " \ndifference between both groups is ", round(Smry_diff_coef, 3), ".")
      its_covariates <- c("If there are additional variables in the model then the coefficients \nabove represent effects after controlling for the other variables.")
    }
    # sgmt
    if(object$analysis_type$itsa_type == "sgmt") {
      # Coefficients
      its_b0_coef <- summary(object$ITS)[["coefficients"]][1, "Estimate"]
      its_b1_coef <- summary(object$ITS)[["coefficients"]][2, "Estimate"]
      its_b2_coef <- summary(object$ITS)[["coefficients"]][3, "Estimate"]
      its_b3_coef <- summary(object$ITS)[["coefficients"]][4, "Estimate"]
      its_b4_coef <- summary(object$ITS)[["coefficients"]][5, "Estimate"]
      its_b5_coef <- summary(object$ITS)[["coefficients"]][6, "Estimate"]
      Smry_int1_coef <- object$ITS.Effects[1, "Effect"]
      Smry_int2_coef <- object$ITS.Effects[2, "Effect"]
      # Significance test
      its_b0_sig <- ifelse(summary(object$ITS)[["coefficients"]][1, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b1_sig <- ifelse(summary(object$ITS)[["coefficients"]][2, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b2_sig <- ifelse(summary(object$ITS)[["coefficients"]][3, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b3_sig <- ifelse(summary(object$ITS)[["coefficients"]][4, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b4_sig <- ifelse(summary(object$ITS)[["coefficients"]][5, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b5_sig <- ifelse(summary(object$ITS)[["coefficients"]][6, "Pr(>|t|)"] < .05, "significant", "non-significant")
      Smry_int1_sig <- ifelse(object$ITS.Effects[1, "p.value"] < .05, "significant", "non-significant")
      Smry_int2_sig <- ifelse(object$ITS.Effects[2, "p.value"] < .05, "significant", "non-significant")
      # Interpretations
      its_intro <- c("Note: Variable names below based on time points (or 'interruptions') \nwon't match identically with the output (e.g., if time 1 = 12 months, \nthe corresponding variables are post1 = post12). \nThis analysis is for a one-group, double intervention period (2 interruptions).")
      B0 <- paste0("Intercept is ", round(its_b0_coef, 3), " and the starting value of the trend \nfor the intervention group.")
      B1 <- paste0("ITS.Time is ", round(its_b1_coef, 3), " and the slope prior to intervention. \nThe coefficient is ", its_b1_sig, ".")
      B2 <- paste0("post1 is ", round(its_b2_coef, 3), " and the immediate shift in the trend line \nafter the intervention start (e.g., 1st year of intervention). \nThe coefficient is ", its_b2_sig, ".")
      B3 <- paste0("txp1 is ", round(its_b3_coef, 3), " and the difference between pre- and \npost-intervention slopes (e.g., change in the pre-intervention \nslope). The coefficient is ", its_b3_sig,".")
      B4 <- paste0("post2 is ", round(its_b4_coef, 3), " and the immediate shift in the trend line \nafter the first intervention (e.g., 1st year of 2nd intervention). \nThe coefficient is ", its_b4_sig, ".")
      B5 <- paste0("txp2 is ", round(its_b5_coef, 3), " and the difference between 1st- and \n2nd-intervention slopes (e.g., change in the pre-intervention \nslope). The coefficient is ", its_b5_sig,".")
      its_Summary1 <- paste0("Summary 1: The results show that after the start of the 1st intervention, \nthere was a ", Smry_int1_sig, " change in the outcome trend. This gives \na total post 1st intervention trend in the outcome of ", round(Smry_int1_coef, 3), " \nover time (i.e., the total combined value of change not the \nchange relative to pre-intervention).")
      its_Summary2 <- paste0("Summary 2: The results show that after the start of the 2nd intervention, \nthere was a ", Smry_int2_sig, " change in the outcome trend. This gives \na total post 2nd intervention trend in the outcome of ", round(Smry_int2_coef, 3), " \nover time (i.e., the total combined value of change not the \nchange relative to the 1st intervention).")
      its_covariates <- c("If there are additional variables in the model then the coefficients \nabove represent effects after controlling for the other variables.")
    }
    # mgmt
    if(object$analysis_type$itsa_type == "mgmt") {
      # Coefficients
      its_b0_coef <- summary(object$ITS)[["coefficients"]][1, "Estimate"]
      its_b1_coef <- summary(object$ITS)[["coefficients"]][2, "Estimate"]
      its_b2_coef <- summary(object$ITS)[["coefficients"]][3, "Estimate"]
      its_b3_coef <- summary(object$ITS)[["coefficients"]][4, "Estimate"]
      its_b4_coef <- summary(object$ITS)[["coefficients"]][5, "Estimate"]
      its_b5_coef <- summary(object$ITS)[["coefficients"]][6, "Estimate"]
      its_b6_coef <- summary(object$ITS)[["coefficients"]][7, "Estimate"]
      its_b7_coef <- summary(object$ITS)[["coefficients"]][8, "Estimate"]
      its_b8_coef <- summary(object$ITS)[["coefficients"]][9, "Estimate"]
      its_b9_coef <- summary(object$ITS)[["coefficients"]][10, "Estimate"]
      its_b10_coef <- summary(object$ITS)[["coefficients"]][11, "Estimate"]
      its_b11_coef <- summary(object$ITS)[["coefficients"]][12, "Estimate"]
      Smry_int1_coef <- object$ITS.Effects[1, "Effect"]
      Smry_con1_coef <- object$ITS.Effects[2, "Effect"]
      Smry_diff1_coef <- object$ITS.Effects[3, "Effect"]
      Smry_int2_coef <- object$ITS.Effects[4, "Effect"]
      Smry_con2_coef <- object$ITS.Effects[5, "Effect"]
      Smry_diff2_coef <- object$ITS.Effects[6, "Effect"]
      # Significance test
      its_b0_sig <- ifelse(summary(object$ITS)[["coefficients"]][1, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b1_sig <- ifelse(summary(object$ITS)[["coefficients"]][2, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b2_sig <- ifelse(summary(object$ITS)[["coefficients"]][3, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b3_sig <- ifelse(summary(object$ITS)[["coefficients"]][4, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b4_sig <- ifelse(summary(object$ITS)[["coefficients"]][5, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b5_sig <- ifelse(summary(object$ITS)[["coefficients"]][6, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b6_sig <- ifelse(summary(object$ITS)[["coefficients"]][7, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b7_sig <- ifelse(summary(object$ITS)[["coefficients"]][8, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b8_sig <- ifelse(summary(object$ITS)[["coefficients"]][9, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b9_sig <- ifelse(summary(object$ITS)[["coefficients"]][10, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b10_sig <- ifelse(summary(object$ITS)[["coefficients"]][11, "Pr(>|t|)"] < .05, "significant", "non-significant")
      its_b11_sig <- ifelse(summary(object$ITS)[["coefficients"]][12, "Pr(>|t|)"] < .05, "significant", "non-significant")
      Smry_int1_sig <- ifelse(object$ITS.Effects[1, "p.value"] < .05, "significant", "non-significant")
      Smry_con1_sig <- ifelse(object$ITS.Effects[2, "p.value"] < .05, "significant", "non-significant")
      Smry_diff1_sig <- ifelse(object$ITS.Effects[3, "p.value"] < .05, "significant", "non-significant")
      Smry_int2_sig <- ifelse(object$ITS.Effects[4, "p.value"] < .05, "significant", "non-significant")
      Smry_con2_sig <- ifelse(object$ITS.Effects[5, "p.value"] < .05, "significant", "non-significant")
      Smry_diff2_sig <- ifelse(object$ITS.Effects[6, "p.value"] < .05, "significant", "non-significant")
      # Interpretations
      its_intro <- c("Note: Variable names below based on time points (or 'interruptions') \nwon't match identically with the output (e.g., if time 1 = 12 months, \nThis analysis is for a two-group, single intervention period (interruption). \nPositive values indicate higher intervention group values and vice-versa for: \npost1, txp1, ixp1, txip1, post2, txp2, ixp2, txip2.")
      B0 <- paste0("Intercept is ", round(its_b0_coef, 3), " and the starting value of the trend for the \ncontrol group.")
      B1 <- paste0("ITS.Time is ", round(its_b1_coef, 3), " and the control group\'s slope prior to intervention. \nThe coefficient is ", its_b1_sig, ".")
      B2 <- paste0("ITS.Int is ", round(its_b2_coef, 3), " and the immediate shift in the control group \ntrend line after the intervention start. The coefficient is \n", its_b2_sig, ".")
      B3 <- paste0("txi is ", round(its_b3_coef, 3), " and the difference between pre- and post-intervention \ncontrol group slopes (e.g., change in the pre-intervention \nslope). The coefficient is ", its_b3_sig, ".")
      B4 <- paste0("post1 is ", round(its_b4_coef, 3), " and the difference in the level between intervention \nand control group prior to intervention 1 (intervention - control). \nThe coefficient is ", its_b4_sig, ".")
      B5 <- paste0("txp1 is ", round(its_b5_coef, 3), " and the difference between the intervention and \ncontrol group\'s pre-intervention slopes (intervention - control). \nThe coefficient is ", its_b5_sig, ".")
      B6 <- paste0("ixp1 is ", round(its_b6_coef, 3), " and the difference between the intervention and \ncontrol groups (intervention - control) in the period immediately \nafter the intervention started (e.g., 1st year of intervention 1). \nThe coefficient is ", its_b6_sig, ".")
      B7 <- paste0("txip1 is ", round(its_b7_coef, 3), " and ", its_b7_sig, "."," This is the difference in both \ngroup\'s slope changes since pre-intervention (pre-slopes compared \nto post-slopes). For example, both have pre-intervention slopes \nof 2, the control group\'s slope remained the same, therefore the \npost 1st intervention slope is 0. And the intervention group's slope \nincreased by 2, then txip1 = 2 (= 2 - 0).")
      B8 <- paste0("post2 is ", round(its_b8_coef, 3), " and the difference in the level between intervention \nand control group prior to the 2nd intervention (intervention - control). \nThe coefficient is ", its_b8_sig, ".")
      B9 <- paste0("txp2 is ", round(its_b9_coef, 3), " and the difference between the intervention and \ncontrol group\'s 1st intervention slopes (intervention - control). \nThe coefficient is ", its_b9_sig, ".")
      B10 <- paste0("ixp2 is ", round(its_b10_coef, 3), " and the difference between the intervention and \ncontrol groups (intervention - control) in the period immediately \nafter the 2nd intervention started (e.g., 1st year of intervention 2). \nThe coefficient is ", its_b10_sig, ".")
      B11 <- paste0("txip2 is ", round(its_b11_coef, 3), " and ", its_b11_sig, "."," This is the difference in both group\'s \nslope changes since the 1st intervention (1st intervention slope compared \nto the 2nd). For example, both have 1st intervention slopes of 2, the control \ngroup\'s slope remained the same, therefore the 2nd intervention slope is 0. And \nthe intervention group's slope increased by 2, then txip2 = 2 (= 2 - 0).")
      its_Summary1 <- paste0("Summary 1: For the 1st intervention period, the results show that the \nintervention group\'s ", Smry_int1_sig, " shift in outcome, \npost 1st intervention was ", round(Smry_int1_coef, 3), ". The control group\'s ",Smry_con1_sig, " \nshift in outcome, post 1st intervention was ", round(Smry_con1_coef, 3), ". The ", Smry_diff1_sig, " \ndifference between both groups is ", round(Smry_diff1_coef, 3), ".")
      its_Summary2 <- paste0("Summary 2: For the 2nd intervention period, the results show that the \nintervention group\'s ", Smry_int2_sig, " shift in outcome, \npost 2nd intervention was ", round(Smry_int2_coef, 3), ". The control group\'s ",Smry_con2_sig, " \nshift in outcome, post 2nd intervention was ", round(Smry_con2_coef, 3), ". The ", Smry_diff2_sig, " \ndifference between both groups is ", round(Smry_diff2_coef, 3), ".")
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
      did <- list(B_0=B_0, B_1=B_1, B_2=B_2, B_3=B_3)
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
      its <- list(its_intro=its_intro, B0=B0, B1=B1, B2=B2, B3=B3, B4=B4, B5=B5,
                  its_Summary1=its_Summary1, its_Summary2=its_Summary2,its_covariates=its_covariates)
    }
    if(object$analysis_type$itsa_type == "mgmt") {
      its <- list(its_intro=its_intro, B0=B0, B1=B1, B2=B2, B3=B3, B4=B4, B5=B5,
                  B6=B6, B7=B7, B8=B8, B9=B9, B10=B10, B11=B11, its_Summary1=its_Summary1,
                  its_Summary2=its_Summary2, its_covariates=its_covariates)
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
