#' Patient survey data
#'
#' Artificial data of a 5 item hospital satisfaction survey
#' for a Cronbach's alpha scale (cas).
#'
#' @format ## `cas`
#' An artificial data frame with 100 rows and 5 columns:
#' \describe{
#'   \item{i1 - i5}{5 survey items}
#'   ...
#' }
#' @source Artificial dataset created with rbinom for 5 items.
#' For example, rbinom(100, 5, .9) generates 1 item. The prob
#' argument is modified to give more or less consistent ratings per item.
#'
"cas"

#' Patient hospital program/intervention data, intervention group only
#' @format ## `hosp1`
#' An artificial data frame with 352 rows and 10 columns, intervention patients only:
#' \describe{
#'   \item{survey}{Patient satisfaction survey mean score.}
#'   \item{ los }{Hospital length of stay (los)}
#'   \item{ cost}{Hospital stay cost}
#'   \item{ rdm30}{Patient readmission within 30 days of discharge}
#'   \item{ death30}{Patient death within 30 days of discharge}
#'   \item{ female}{Patient sex, 1 indicates female, 0 otherwise}
#'   \item{ age }{Patient age}
#'   \item{ risk}{Patient health risk score ranging from 0 to 1}
#'   \item{ month}{12 month indicator (1 to 12)}
#'   \item{ program}{Indicates patient program participation. 1='yes', 0='no'}
#'   ...
#' }
#' @source hosp1 is a subset of the artificial dataset hosprog. It is
#' the intervention group's data used for single group interrupted time series.
#'
"hosp1"

#' Patient hospital program/intervention data
#' @format ## `hosprog`
#' An artificial data frame with 720 rows and 10 columns:
#' \describe{
#'   \item{survey}{Patient satisfaction survey mean score.}
#'   \item{ los }{Hospital length of stay (los)}
#'   \item{ cost}{Hospital stay cost}
#'   \item{ rdm30}{Patient readmission within 30 days of discharge}
#'   \item{ death30}{Patient death within 30 days of discharge}
#'   \item{ female}{Patient sex, 1 indicates female, 0 otherwise}
#'   \item{ age }{Patient age}
#'   \item{ risk}{Patient health risk score ranging from 0 to 1}
#'   \item{ month}{12 month indicator (1 to 12)}
#'   \item{ program}{Indicates patient program participation. 1='yes', 0='no'}
#'   ...
#' }
#' @source Artificial dataset created by using runif. The strength in
#' the association between each variable is weighted by multiplying
#' each subsequent predictor in increments of 1. For example,
#' Y equals runif(720) multiplied by 1 plus runif(720) multiplied by 2
#' and so on. This allows some predictors to have stronger correlations with Y.
#'
"hosprog"

#' Hospital acquired infections (HAI) during 41 months.
#' @format ## `infections`
#' An artificial data frame with 41 rows and 3 columns:
#' \describe{
#'   \item{ Month }{Calendar month as an integer, ranging from 1 to 41.}
#'   \item{ HAI }{Count of hospital acquired infections (HAI) within 1 month.}
#'   \item{ PatientDays }{Count of hospital patient days within 1 month (i.e., the sum of days in the hospital for all patients).}
#'   ...
#' }
#' @source infections is an artificial data frame of reasonable hospital acquired infections (HAI) estimates
#' made entirely for the purpose of demonstrating control charts.
#'
"infections"

#' USA's unemployment rate between 1929 and 2024
#' @format ## `unemployment`
#' An artificial data frame with 96 rows and 5 columns:
#' \describe{
#'   \item{Year}{Calendar year as an integer from 1929 to 2024.}
#'   \item{ rate }{Unemployment rate defined as the proportion of the US Labor Market that were unemployed.}
#'   \item{ event}{Notable events in the United States that may have impacted the unemployment rate. Other than notable events (or unspecified) listed as 'other'.}
#'   \item{ year}{Integer for the number of years for 1929 to 2024, ranging from 1 to 96.}
#'   \item{ usa}{Value of 1 to indicate data for the USA.}
#'   ...
#' }
#' @source unemployment is an artificial data frame of reasonable estimates made entirely
#' for the purpose of demonstrating interrupted time series with many interruptions. For
#' precise rates, please see a reliable source.
#'
"unemployment"

#' Markov Chain Monte Carlo linear regression estimates of plant's CO2 uptake regressed on ambient carbon dioxide concentrations
#' @format ## `co2mcmc`
#' MCMC list of 3 chains with 2000 rows and 4 columns in each of the list elements:
#' \describe{
#'   \item{ b0}{Regression intercept.}
#'   \item{ b1 }{Regression coefficient of the carbon dioxide concentration effect.}
#'   \item{ sigma}{Square root of model residual variance.}
#'   \item{ lp__}{Total log unnormalized joint posterior density of the model parameters.}
#'   ...
#' }
#' @source co2mcmc is a list of 3 MCMC simulations. It estimates a linear regression using the CO2 data frame from base R.
#' A regression model was specified using a normal distribution maximum likelihood and normal and uniform distribution priors for
#' the beta coefficients and sigma parameters.
#'
"co2mcmc"

#' Bayes class object with summarized Markov Chain Monte Carlo estimates of plant's CO2 uptake as a binary variable that is above or below the median level
#' @format ## `co2multi`
#' Bayes class list of 7 elements with varying rows and columns in each of the list elements:
#' \describe{
#'   \item{ Posterior.Summary}{Standard element that is NA in this object because it is not applicable to this analysis.}
#'   \item{ MCMC}{Dataframe of 4 combined MCMC chain simulations with 8000 rows and 18 columns. Column 1 is CHAIN
#'   indicates the 4 chains, columns 2-13 are 'theta' and indicates each plant's rate of having above the median
#'   uptake level, columns 14-17 are 'omega' group rates of 'Type' by 'Treatment', and column 18 is the overall group rate.}
#'   \item{ Multilevel}{Posterior estimate summaries in alphabetical order of plant or group names and numerical order plant or group rates.}
#'   \item{ Target}{Standard element that is NA in this object because it is not applicable to this analysis.}
#'   \item{ targets}{Standard element that is NA in this object because it is not applicable to this analysis.}
#'   \item{ R2.Summary}{Standard element that is NA in this object because it is not applicable to this analysis.}
#'   \item{ parameter}{3 parameter names in the multilevel analysis, 'theta', 'omega', and 'omegaO'.}
#'   ...
#' }
#' @source co2multi is a list of summarized Markov Chain Monte Carlo estimates of plant's CO2 uptake as a binary
#' variable that is above or below the median level. It summarizes measurements nested within plants nested within
#' the 4 levels from a combination of 'Treatment' by 'Type'. An estimation used the CO2 data frame from base R.
#' A hierarchical estimation model was specified using a gamma distribution maximum likelihood and normal
#' and uniform distribution priors at various group levels.
#'
"co2multi"

#' Markov Chain Monte Carlo estimates of hospital length of stay from the hosprog data frame
#' @format ## `losmcmc`
#' MCMC list of 4 chains with 5000 rows and 2 columns in each of the list elements:
#' \describe{
#'   \item{ muOfY}{Estimate of the mean parameter.}
#'   \item{ sigmaOfY }{Estimate of the square root of variance parameter.}
#'   ...
#' }
#' @source losmcmc is a list of 4 MCMC simulations. It estimates the mean and standard deviation using the artificial hosprog
#' data frame from ham. An estimation was specified using a normal distribution maximum likelihood and normal and uniform
#' distribution priors for the mean and sigma parameters.
#'
"losmcmc"
