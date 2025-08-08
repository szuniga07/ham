#' Patient survey data
#'
#' Artificial data of a 5 item hospital satisfaction survey to be used
#' for a Cronbach's alpha scale.
#'
#' @format ## `cas`
#' An artificial data frame with 100 rows and 5 columns:
#' \describe{
#'   \item{i1 -i5}{5 survey items}
#'   ...
#' }
#' @source Artificial dataset created with rbinom for 5 items.
#' For example, rbinom(100, 5, .9) generates 1 item. The prob
#' argument is modified to give more or less consistent ratings per item.
#'
"cas"

#' Patient hospital intervention data
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

#' Patient hospital intervention data, intervention group only
#' @format ## `hosprog`
#' An artificial data frame with 360 rows and 10 columns:
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
