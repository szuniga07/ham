#' Statistics for Decision Curve Analysis and logistic regression model classification
#'
#' Calculate statistics such as sensitivity, specificity, positive and negative predictive values, and
#' net benefit and interventions saved from a decision curve analysis.
#'
#' @param x assess regression object, currently logistic regression.
#' @param threshold numeric vector of length == 1 that sets the threshold used to calculate sensitivity,
#' specificity, positive and negative predictive values, and net benefit and interventions saved from a
#' decision curve analysis. Select thresholds on appropriate linear scale such as logits for logistic
#' regression models.
#'
#' @return summary of model classification based on the selected threshold/cutoff, area under the curve
#' (AUC)/c-statistic, predicted outcomes (transformed if applicable), decision curve analysis
#' values at various percentiles, and sensitivity and specificity related statistics for the regression
#' model at the specified threshold.
#' @importFrom stats sd terms
#' @export
#' @references
#' Vickers, A. & Elkin, E. (2006). Decision Curve Analysis: A Novel Method for Evaluating Prediction Models.
#' Society for Medical Decision Making, 26, 6, 565-574. https://doi.org/10.1177/0272989X06295361

#' @examples
#' ## Predicting car engine shape type, v or straight  ##
#' # run the model
#' car_m1 <- assess(formula=vs ~ hp + am, data=mtcars, regression="logistic")
#' # create a decide object, enter the model name and a threshold on the logit scale
#' d1 <- decide(x=car_m1, threshold= -0.767)
#' # View model classification related statistics
#' print(d1$Model.Summary$Classification)
#'
#' # View decision curve analysis results like 'net benefit' at various thresholds
#' print(d1$DCA)

decide <- function(x, threshold) {

  if (any(class(x) == "assess") == FALSE) {stop("Error: Expecting assess class object." )}

  #Model fit
  model_fit <- x$model
  #Regression type
  reg_type <- x$analysis_type$regression_type
  #Get data
  data <- x$model$data
  #Get outcome variable name
  all_vars <- all.vars(x$formula$primary_formula)
  # Get the 'response' attribute which indicates the index of the outcome variable
  response_index <- attr(terms(x$formula$primary_formula), "response")
  # Extract the outcome variable name using the index
  outcome_name <- all_vars[response_index]

################################
## Function for predicted Yhat ##
################################
#Function to get predictions and transformed values #
fncPredTrans <- function(FIT, RegType) {
  #Predicted scores
  Yhat <- predict(FIT)
  #Transform scores
  Transformed.Yhat <- switch(RegType,
                             "ols" = predict(FIT),
                             "logistic" = plogis(predict(FIT)),
                             "Proportion Y Logistic" = plogis(predict(FIT)),
                             "Ordinal Logistic" = plogis(predict(FIT)),
                             "Poisson" = exp(predict(FIT)),
                             "Quantile" = predict(FIT),
                             "Cox PH" = plogis(predict(FIT)),
                             "Cox PH with censoring" = plogis(predict(FIT)),
                             "AFT"  = exp(predict(FIT)),
                             "AFT with censoring"     = exp(predict(FIT)),
                             "Generalized Least Squares" = predict(FIT) )
  return(list("Yhat"=Yhat, "Transformed.Yhat"=Transformed.Yhat))
}

fncTrnsfYhatSmry <- function(YhatRslt, RegType) {
  prob_vals <- c(.01, .05, .10, .25, .50, .75, .90, .95, .99)
  #Transform scores
  Transformed.Yhat <- quantile(YhatRslt, probs=prob_vals )
  #Add names
  names(Transformed.Yhat) <- prob_vals
  #Transform range of lowest and highest values
  Transformed.Full.Range <- range(YhatRslt)
  return(list("Yhat"=Transformed.Yhat, "Range"= Transformed.Full.Range))
}
##############
## Get data ##
##############
fncYhatClassDf <- function(Fit, Y, Threshold, Censor=NULL, PredTime=NULL, RegType, DF, OffSetName=NULL)  {
  tm1 <- Fit
  atime <- PredTime
  tdf <- DF
  tY <- Y
  tcensor <- Censor
  #  PREDrange <- range(predict(Fit), na.rm=T)
  if(RegType == "Poisson" & length(OffSetName) == 0 ) {
    PREDrange <- range(predict(Fit), na.rm=T)
  } else {
    #    PREDrange <- range(exp(predict( Fit, newdata=DF) )* mean(DF[, OffSetName]), na.rm=T)
    PREDrange <- range(predict( Fit, newdata=DF), na.rm=T )
  }
  # Need to make object for this so I can calculate the trapezoid AUC:
  threshLev <- Threshold
  #Get predictions for values at threshold
  #Make data for predict()
  newtdf1 <- switch(RegType,
                    "ols"   = tdf[ tdf[,  tY] >= threshLev, ],
                    "logistic" = tdf[ tdf[,  tY] == 1, ],
                    "Proportion Y Logistic" = tdf[ tdf[,  tY] == 1, ],
                    "Ordinal Logistic"  = tdf[ tdf[,  tY] > 1, ],
                    "Poisson"  = tdf[ tdf[,  tY] >= exp(threshLev), ],
                    "Quantile" = tdf[ tdf[,  tY] >= threshLev, ],
                    "Cox PH"   = tdf[tdf[,  tY] < atime, ],
                    #old                    "Cox PH with censoring"  = tdf[ tdf[, tcensor ] == max(tdf[, tcensor ], na.rm=T) & tdf[,  tY] >= atime, ],
                    "Cox PH with censoring"  = tdf[ eval(parse(text=paste0("tdf$",tcensor) )) == TRUE & tdf[,  tY] < atime, ],
                    "AFT"  = tdf[tdf[,  tY] < atime, ],
                    #old                    "AFT with censoring"     = tdf[ tdf[, tcensor ] ==max(tdf[, tcensor ], na.rm=T) & tdf[,  tY] >= atime, ],
                    "AFT with censoring"     = tdf[eval(parse(text=paste0("tdf$",tcensor) )) == TRUE & tdf[,  tY] < atime, ],
                    "Generalized Least Squares" = tdf[ tdf[,  tY] >= threshLev, ] )
  newtdf2 <- switch(RegType,
                    "ols"   = tdf[ tdf[,  tY] < threshLev, ],
                    "logistic" = tdf[ tdf[,  tY] == 0, ],
                    "Proportion Y Logistic" = tdf[ tdf[,  tY] == 0, ],
                    "Ordinal Logistic"  = tdf[ tdf[,  tY]  ==1, ],
                    "Poisson"  = tdf[ tdf[,  tY] < exp(threshLev), ],
                    "Quantile" = tdf[ tdf[,  tY] < threshLev, ],
                    "Cox PH"   = tdf[tdf[,  tY] >= atime, ],
                    #old                    "Cox PH with censoring"  = tdf[ tdf[, tcensor ] ==min(tdf[, tcensor ], na.rm=T) & tdf[,  tY] < atime, ],
                    "Cox PH with censoring"  = tdf[ eval(parse(text=paste0("tdf$",tcensor) )) == FALSE & tdf[,  tY] >= atime, ],
                    "AFT"  = tdf[tdf[,  tY] >= atime, ],
                    #old                    "AFT with censoring"     = tdf[ tdf[, tcensor ] ==min(tdf[, tcensor ], na.rm=T) & tdf[,  tY] >= atime, ],
                    "AFT with censoring"     = tdf[eval(parse(text=paste0("tdf$",tcensor) )) == FALSE & tdf[,  tY] < atime, ],
                    "Generalized Least Squares" = tdf[ tdf[,  tY] < threshLev, ] )
  #Get tranformed values of threshold when using logits
  Transform.Threshold <- switch(RegType,
                                "ols"   = NA,
                                "logistic" = plogis(threshLev),
                                "Proportion Y Logistic" = plogis(threshLev),
                                "Ordinal Logistic"  = plogis(threshLev),
                                "Poisson"  = exp(threshLev),
                                "Quantile" = NA,
                                "Cox PH"   = plogis(threshLev),
                                "Cox PH with censoring"  = plogis(threshLev),
                                "AFT"  = plogis(threshLev),
                                "AFT with censoring"     = plogis(threshLev),
                                "Generalized Least Squares" = NA )
  #Predictions
  pm1 <- predict(tm1, newdata= newtdf1)
  pm2 <- predict(tm1, newdata= newtdf2)
  #Get values for xlim of plot
  senspcXmin <- PREDrange[1]
  senspcXmax <- PREDrange[2]
  ## This determines the amount of predictions above a certain level
  #Sensitivity
  #Get probability for 4 types of response
  pr_table1 <- prop.table(table(factor(round(pm1, 10) >= round(threshLev, 10), levels=c("FALSE","TRUE") )))
  pr_table2 <- prop.table(table(factor(round(pm2, 10) >= round(threshLev, 10), levels=c("FALSE","TRUE") )))
  #Sensitivity and 1 - specificity
  propAbovMY1 <-  pr_table1["TRUE"]  #Sensitivity
  fls_Neg <-  pr_table1["FALSE"]  #FALSE negative
  propAbovMY0 <- pr_table2["TRUE"]  #1-specificity or false-positive
  specifity <-  pr_table2["FALSE"]  #Specificity
  #Get frequencies
  f_table1 <- table(factor(round(pm1, 10) >= round(threshLev, 10), levels=c("FALSE","TRUE") ))
  f_table2 <- table(factor(round(pm2, 10) >= round(threshLev, 10), levels=c("FALSE","TRUE") ))
  #Sensitivity and 1 - specificity
  N.AbovMY1 <-  f_table1["TRUE"]  #Sensitivity
  N.fls_Neg <-  f_table1["FALSE"]  #FALSE negative
  N.AbovMY0 <- f_table2["TRUE"]  #1-specificity or false-positive
  N.specifity <-  f_table2["FALSE"]  #Specificity

  return(list("pm1"=pm1, "pm2"=pm2, "threshLev"=threshLev, "senspcXmin"=senspcXmin, "senspcXmax"=senspcXmax,
              "propAbovMY1"=propAbovMY1, "fls_Neg"=fls_Neg,  "propAbovMY0"=propAbovMY0, "specifity"=specifity,
              "Transform.Threshold"=Transform.Threshold,
              "N.AbovMY1"=N.AbovMY1, "N.fls_Neg"=N.fls_Neg,  "N.AbovMY0"=N.AbovMY0, "N.specifity"=N.specifity))

}
#####################################################
## Function to create AUC for each threshold I set ##
#####################################################
fncThreshAUC <- function(ClassDF) {
  ## This AUC is based only on the cutoff level. Use this to get the binary classification AUC.
  yClassSens <- c(ClassDF$propAbovMY1)
  yClassSpec <- c(ClassDF$specifity)
  #Add in values of 0 and 1 for perfect sensitivity and specificity
  yClassSens <- c(1, yClassSens, 0)
  yClassSpec <- c(0, yClassSpec, 1)

  #Gets differences between specificity values
  spc_diff <- diff(yClassSpec)

  #Adds sensitivity portions of formula
  sns_diff <- vector(length= length(yClassSens) - 1)
  i <- 1
  while (i < length(yClassSens) ) {
    sns_diff[i] <- yClassSens[i] + yClassSens[i+1]
    i = i+1
  }
  #Command to get AUC#
  Threshold.AUC <- sum(.5*(spc_diff * sns_diff))
  return("Threshold.AUC"=Threshold.AUC)
}
########################
## Get summary values ##
########################
fncClassDfSmry <- function(ClassDF, RegType) {
  #Sensitivity value
  propAbovMY1 <- switch(RegType,
                        "ols"   = unname(ClassDF$propAbovMY1),
                        "logistic" = unname(ClassDF$propAbovMY1),
                        "Proportion Y Logistic" = unname(ClassDF$propAbovMY1),
                        "Ordinal Logistic"  = unname(ClassDF$propAbovMY1),
                        "Poisson"  = unname(ClassDF$propAbovMY1),
                        "Quantile" = unname(ClassDF$propAbovMY1),
                        "Cox PH"   = unname(ClassDF$propAbovMY1),
                        "Cox PH with censoring"  = unname(ClassDF$propAbovMY1),
                        "AFT"  = 1-unname(ClassDF$propAbovMY1),
                        "AFT with censoring"     = 1-unname(ClassDF$propAbovMY1),
                        "Generalized Least Squares" = unname(ClassDF$propAbovMY1) )
  #Specificity value
  specifity <- switch(RegType,
                      "ols"   = unname(ClassDF$specifity),
                      "logistic" = unname(ClassDF$specifity),
                      "Proportion Y Logistic" = unname(ClassDF$specifity),
                      "Ordinal Logistic"  = unname(ClassDF$specifity),
                      "Poisson"  = unname(ClassDF$specifity),
                      "Quantile" = unname(ClassDF$specifity),
                      "Cox PH"   = unname(ClassDF$specifity),
                      "Cox PH with censoring"  = unname(ClassDF$specifity),
                      "AFT"  = 1-unname(ClassDF$specifity),
                      "AFT with censoring"     = 1-unname(ClassDF$specifity),
                      "Generalized Least Squares" = unname(ClassDF$specifity) )
  #False.Positives value
  propAbovMY0 <- switch(RegType,
                        "ols"   = unname(ClassDF$propAbovMY0),
                        "logistic" = unname(ClassDF$propAbovMY0),
                        "Proportion Y Logistic" = unname(ClassDF$propAbovMY0),
                        "Ordinal Logistic"  = unname(ClassDF$propAbovMY0),
                        "Poisson"  = unname(ClassDF$propAbovMY0),
                        "Quantile" = unname(ClassDF$propAbovMY0),
                        "Cox PH"   = unname(ClassDF$propAbovMY0),
                        "Cox PH with censoring"  = unname(ClassDF$propAbovMY0),
                        "AFT"  = 1-unname(ClassDF$propAbovMY0),
                        "AFT with censoring"     = 1-unname(ClassDF$propAbovMY0),
                        "Generalized Least Squares" = unname(ClassDF$propAbovMY0) )
  #False.Negatives value
  fls_Neg <- switch(RegType,
                    "ols"   = unname(ClassDF$fls_Neg),
                    "logistic" = unname(ClassDF$fls_Neg),
                    "Proportion Y Logistic" = unname(ClassDF$fls_Neg),
                    "Ordinal Logistic"  = unname(ClassDF$fls_Neg),
                    "Poisson"  = unname(ClassDF$fls_Neg),
                    "Quantile" = unname(ClassDF$fls_Neg),
                    "Cox PH"   = unname(ClassDF$fls_Neg),
                    "Cox PH with censoring"  = unname(ClassDF$fls_Neg),
                    "AFT"  = 1-unname(ClassDF$fls_Neg),
                    "AFT with censoring"     = 1-unname(ClassDF$fls_Neg),
                    "Generalized Least Squares" = unname(ClassDF$fls_Neg) )

  ## Frequencies ##
  #Sensitivity value
  N.AbovMY1 <- switch(RegType,
                      "ols"   = unname(ClassDF$N.AbovMY1),
                      "logistic" = unname(ClassDF$N.AbovMY1),
                      "Proportion Y Logistic" = unname(ClassDF$N.AbovMY1),
                      "Ordinal Logistic"  = unname(ClassDF$N.AbovMY1),
                      "Poisson"  = unname(ClassDF$N.AbovMY1),
                      "Quantile" = unname(ClassDF$N.AbovMY1),
                      "Cox PH"   = unname(ClassDF$N.AbovMY1),
                      "Cox PH with censoring"  = unname(ClassDF$N.AbovMY1),
                      "AFT"  = unname(ClassDF$N.fls_Neg),
                      "AFT with censoring"     = unname(ClassDF$N.fls_Neg),
                      "Generalized Least Squares" = unname(ClassDF$N.AbovMY1) )
  #Specificity value
  N.specifity <- switch(RegType,
                        "ols"   = unname(ClassDF$N.specifity),
                        "logistic" = unname(ClassDF$N.specifity),
                        "Proportion Y Logistic" = unname(ClassDF$N.specifity),
                        "Ordinal Logistic"  = unname(ClassDF$N.specifity),
                        "Poisson"  = unname(ClassDF$N.specifity),
                        "Quantile" = unname(ClassDF$N.specifity),
                        "Cox PH"   = unname(ClassDF$N.specifity),
                        "Cox PH with censoring"  = unname(ClassDF$N.specifity),
                        "AFT"  = unname(ClassDF$N.AbovMY0),
                        "AFT with censoring"     = unname(ClassDF$N.AbovMY0),
                        "Generalized Least Squares" = unname(ClassDF$N.specifity) )
  #False.Positives value
  N.AbovMY0 <- switch(RegType,
                      "ols"   = unname(ClassDF$N.AbovMY0),
                      "logistic" = unname(ClassDF$N.AbovMY0),
                      "Proportion Y Logistic" = unname(ClassDF$N.AbovMY0),
                      "Ordinal Logistic"  = unname(ClassDF$N.AbovMY0),
                      "Poisson"  = unname(ClassDF$N.AbovMY0),
                      "Quantile" = unname(ClassDF$N.AbovMY0),
                      "Cox PH"   = unname(ClassDF$N.AbovMY0),
                      "Cox PH with censoring"  = unname(ClassDF$N.AbovMY0),
                      "AFT"  = unname(ClassDF$N.specifity),
                      "AFT with censoring"     = unname(ClassDF$N.specifity),
                      "Generalized Least Squares" = unname(ClassDF$N.AbovMY0) )
  #False.Negatives value
  N.fls_Neg <- switch(RegType,
                      "ols"   = unname(ClassDF$N.fls_Neg),
                      "logistic" = unname(ClassDF$N.fls_Neg),
                      "Proportion Y Logistic" = unname(ClassDF$N.fls_Neg),
                      "Ordinal Logistic"  = unname(ClassDF$N.fls_Neg),
                      "Poisson"  = unname(ClassDF$N.fls_Neg),
                      "Quantile" = unname(ClassDF$N.fls_Neg),
                      "Cox PH"   = unname(ClassDF$N.fls_Neg),
                      "Cox PH with censoring"  = unname(ClassDF$N.fls_Neg),
                      "AFT"  = unname(ClassDF$N.AbovMY1),
                      "AFT with censoring"     = unname(ClassDF$N.AbovMY1),
                      "Generalized Least Squares" = unname(ClassDF$N.fls_Neg) )

  #Total N
  total_N <- sum(N.AbovMY1,N.AbovMY0, N.specifity, N.fls_Neg)

  #Weighted threshold value
  wt_thresh <- switch(RegType,
                      "ols"   = pnorm( ClassDF$threshLev, mean = mean(data[, outcome_name], na.rm=T), sd = sd(data[, outcome_name], na.rm=T)),
                      "logistic" = plogis(ClassDF$threshLev),
                      "Proportion Y Logistic" = plogis(ClassDF$threshLev),
                      "Ordinal Logistic"  = plogis(ClassDF$threshLev),
                      "Poisson"  = pnorm( exp(ClassDF$threshLev), mean = mean(data[, outcome_name], na.rm=T), sd = sd(data[, outcome_name], na.rm=T)),
                      "Quantile" = pnorm( ClassDF$threshLev, mean = mean(data[, outcome_name], na.rm=T), sd = sd(data[, outcome_name], na.rm=T)),
                      "Cox PH"   = plogis(ClassDF$threshLev),
                      "Cox PH with censoring"  = plogis(ClassDF$threshLev),
                      "AFT"  = pnorm(ClassDF$threshLev , mean = mean(c(ClassDF$pm1, ClassDF$pm2), na.rm=T), sd = sd(c(ClassDF$pm1, ClassDF$pm2), na.rm=T)),
                      "AFT with censoring"     = pnorm(ClassDF$threshLev , mean = mean(c(ClassDF$pm1, ClassDF$pm2), na.rm=T), sd = sd(c(ClassDF$pm1, ClassDF$pm2), na.rm=T)),
                      "Generalized Least Squares" = pnorm( ClassDF$threshLev, mean = mean(data[, outcome_name], na.rm=T), sd = sd(data[, outcome_name], na.rm=T)) )

  #Accuracy
  Accuracy.Rate <- (N.AbovMY1 + N.specifity)/ total_N
  #Error rate
  Error.Rate <- (N.AbovMY0 + N.fls_Neg)/ total_N
  #Positive Predictive Value
  PPV <- N.AbovMY1/ (N.AbovMY1 + N.AbovMY0)
  #Negative Predictive Value
  NPV <- N.specifity/ (N.specifity + N.fls_Neg)
  #Net benefit
  Net.Benefit <- N.AbovMY1/total_N - N.AbovMY0/total_N * (wt_thresh /(1 - wt_thresh ))
  #All treated
  All.Treated <- (N.AbovMY1 + N.fls_Neg)/total_N - (N.AbovMY0 + N.specifity)/total_N * (wt_thresh /(1 - wt_thresh))
  #Interventions avoided
  Interventions.Saved <- N.specifity/total_N -  N.fls_Neg/total_N * (1 - wt_thresh)/ wt_thresh
  return(list("Classification"= c("Sensitivity"=propAbovMY1, "Specifity"= specifity, "False.Positives"= propAbovMY0, "False.Negatives"= fls_Neg,
                                  "Accuracy.Rate"=Accuracy.Rate, "Error.Rate"=Error.Rate),
              "Predictive.Values"= c("Positive.Predictive.Value"=PPV, "Negative.Predictive.Value"=NPV),
              "Frequencies"= c("N.Sensitivity"=N.AbovMY1, "N.Specifity"=N.specifity,
                               "N.False.Positives"= N.AbovMY0, "N.False.Negatives"= N.fls_Neg, "N"=total_N),
              "Decision.Curve.Analysis"= c("Net.Benefit"=Net.Benefit, "All.Treated"=All.Treated,
                                           "Interventions.Avoided"=Interventions.Saved)))
}

###########################################
## Create a decision curve analysis plot ##
###########################################
fncThreshQntl <- function(Fit, Y, Threshold, Censor=NULL, PredTime=NULL, RegType, DF, OffSetName=NULL) {
  Threshold <- Threshold[["Yhat"]]
  #Get sensitivity and specificity for IQR of predicted values, offset is NULL for 1st version
  yClass.01 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=Threshold[1], Censor=Censor, PredTime=PredTime,
                                   RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.05 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=Threshold[2], Censor=Censor, PredTime=PredTime,
                                   RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.10 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=Threshold[3], Censor=Censor, PredTime=PredTime,
                                   RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.25 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=Threshold[4], Censor=Censor, PredTime=PredTime,
                                   RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.50 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=Threshold[5], Censor=Censor, PredTime=PredTime,
                                   RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.75 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=Threshold[6], Censor=Censor, PredTime=PredTime,
                                   RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.90 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=Threshold[7], Censor=Censor, PredTime=PredTime,
                                   RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.95 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=Threshold[8], Censor=Censor, PredTime=PredTime,
                                   RegType=RegType, DF=DF, OffSetName=OffSetName))
  yClass.99 <-  try(fncYhatClassDf(Fit=Fit, Y=Y, Threshold=Threshold[9], Censor=Censor, PredTime=PredTime,
                                   RegType=RegType, DF=DF, OffSetName=OffSetName))
  #Group the output
  YClass <- list("yClass.01"=yClass.01, "yClass.05"=yClass.05, "yClass.10"=yClass.10, "yClass.25"=yClass.25,"yClass.50"=yClass.50,
                 "yClass.75"=yClass.75, "yClass.90"=yClass.90, "yClass.95"=yClass.95, "yClass.99"=yClass.99)

  #Quantile levels for YClass
  YClass_Quants <- as.numeric(names(Threshold))

  ## Total N ##
  total_N <- vector()
  for (i in 1:length(YClass)) {
    total_N[i] <- sum(YClass[[i]]$N.AbovMY1, YClass[[i]]$N.fls_Neg, YClass[[i]]$N.AbovMY0, YClass[[i]]$N.specifity)
  }

  #List threshold percentiles
  Threshold.Percentiles <- c("0.01", "0.05", "0.10", "0.25", "0.50", "0.75", "0.90", "0.95", "0.99")

  #Weighted threshold value
  Threshold.Level <- vector()
  for (i in 1:length(YClass)) {
    Threshold.Level[i] <- switch(RegType,
                                 "ols"   = YClass_Quants[i],
                                 "logistic" = plogis(YClass[[i]]$threshLev),
                                 "Proportion Y Logistic" = plogis(YClass[[i]]$threshLev),
                                 "Ordinal Logistic"  = YClass_Quants[i],
                                 "Poisson"  = YClass_Quants[i],
                                 "Quantile" = YClass_Quants[i],
                                 "Cox PH"   = plogis(YClass[[i]]$threshLev),
                                 "Cox PH with censoring"  = plogis(YClass[[i]]$threshLev),
                                 "AFT"  = 1 - YClass_Quants[i],
                                 "AFT with censoring"     = 1 - YClass_Quants[i],
                                 "Generalized Least Squares" = YClass_Quants[i] )
  }
  #################
  ## Net Benefit ##
  #################
  Net.Benefit <- vector()
  Interventions.Saved <- vector()
  All.Treated <- vector()
  if(RegType %in% c("AFT with censoring","AFT")) {
    for (i in 1:length(YClass)) {
      #Net benefit: True positives - False positives * weighting by the relative harm of a false-positive and a false-negative result
      Net.Benefit[i] <- YClass[[i]]$N.fls_Neg/total_N[i] - YClass[[i]]$N.specifity/total_N[i] * (Threshold.Level[i] /(1 - Threshold.Level[i]))
      #All treated: All positives - all negatives * weighting by the relative harm of a false-positive and a false-negative result
      All.Treated[i] <- (YClass[[i]]$N.fls_Neg + YClass[[i]]$N.AbovMY1)/total_N[i] - (YClass[[i]]$N.specifity + YClass[[i]]$N.AbovMY0)/total_N[i] * (Threshold.Level[i] /(1 - Threshold.Level[i]))
      #Net benefit for True Negatives...Interventions avoided
      Interventions.Saved[i] <- YClass[[i]]$N.AbovMY0/total_N[i] - YClass[[i]]$N.AbovMY1/total_N[i] *  (1 - Threshold.Level[i]) / Threshold.Level[i]
    }
  } else (
    for (i in 1:length(YClass)) {
      #Net benefit: True positives - False positives * weighting by the relative harm of a false-positive and a false-negative result
      Net.Benefit[i] <- YClass[[i]]$N.AbovMY1/total_N[i] - YClass[[i]]$N.AbovMY0/total_N[i] * (Threshold.Level[i] /(1 - Threshold.Level[i]))
      #All treated: All positives - all negatives * weighting by the relative harm of a false-positive and a false-negative result
      All.Treated[i] <- (YClass[[i]]$N.AbovMY1 + YClass[[i]]$N.fls_Neg)/total_N[i] - (YClass[[i]]$N.AbovMY0 + YClass[[i]]$N.specifity)/total_N[i] * (Threshold.Level[i] /(1 - Threshold.Level[i]))
      #Net benefit for True Negatives...Interventions avoided
      Interventions.Saved[i] <- YClass[[i]]$N.specifity/total_N[i] - YClass[[i]]$N.fls_Neg/total_N[i] *  (1 - Threshold.Level[i]) / Threshold.Level[i]
    }
  )
  #Give percentiles to each level
  names(total_N) <- Threshold.Percentiles
  names(Threshold.Level) <- Threshold.Percentiles
  names(Net.Benefit) <- Threshold.Percentiles
  names(All.Treated) <- Threshold.Percentiles
  names(Interventions.Saved) <- Threshold.Percentiles
  return(list("total_N"=total_N, "Threshold.Level"=Threshold.Level,
              "Net.Benefit"=Net.Benefit, "All.Treated"=All.Treated, "Interventions.Saved"=Interventions.Saved))
}

###################
## Run functions ##
###################

#Get predicted Yhat values
yhat_res <- fncPredTrans(FIT=model_fit, RegType=reg_type)
#Get thresholds from predicted Yhat at .01, .05, .10, .25, .50, .75, .90, .95, .99 percentiles
threshyhat <- fncTrnsfYhatSmry(YhatRslt=yhat_res[["Yhat"]], RegType=reg_type)
#Model classification output
class_output <- fncYhatClassDf(Fit= model_fit, Y=outcome_name, Threshold= threshold,
                               RegType=reg_type, DF= data)
#AUC at the selected threshold
AUC_output <- fncThreshAUC(ClassDF=class_output )
#Classification and Decision Curve Analysis results
model_smry <- fncClassDfSmry(ClassDF=class_output, RegType= reg_type)
#Get decision curve analysis
threshQ <- fncThreshQntl(Fit=model_fit, Y=outcome_name, Threshold=threshyhat,
                         RegType=reg_type, DF=data)

#Combine in list
z <- list(Model.Summary=model_smry, AUC=AUC_output,
          DCA=threshQ, Classification=class_output,
          Yhat.Range=threshyhat,
          type=reg_type, outcome=outcome_name)
# Assign ham classes
class(z) <- c("decide", "ham", "list")
return(z)

}

