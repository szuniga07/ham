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
#' @source
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
#' @source
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
#' @source
#'
"hosp1"
