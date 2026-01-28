#' Print interpret object
#'
#' Formats interpretations from interpret class objects. Provides simple
#' interpretations of regression coefficients and Cronbach's alpha. Print
#' specific model interpretations (or run all), returned in sentence and
#' paragraph formats.
#'
#' @param x interpret object.
#'
#' @param ... Additional arguments.
#'
#' @return formatted interpret object results.
#' @export
#' @importFrom methods show
#' @examples
#'
#' #Cronbach's alpha
#' print(interpret(alpha(items=c("i1","i2","i3","i4","i5"), data=cas)))
#'
#' #' # interpret a standard linear (OLS) regression
#' hos1 <- assess(formula=survey ~ program + month, data=hosprog, regression= "ols")
#' print(interpret(hos1)$model)
#'
#' # interpret a differences-in-differences model
#' hos2 <- assess(formula=survey ~ ., data=hosprog, intervention = "program",
#' int.time="month", treatment = 5, did="two", newdata=TRUE)
#' interpret(hos2)$did
#'
#' # interpret an interrupted time series model
#' hos3 <- assess(formula=survey ~ ., data=hosprog, intervention = "program",
#' int.time="month", its="two", interrupt = 5)
#' interpret(hos3)$its
#'
print.interpret <- function(x, ...) {
  object <- x
  #alpha objects
  if("interpret" %in% class(object) ) {
    if("alpha" %in% class(object) ) {
      cat("Interpretations: Alpha", "\n" )
      cat("----------------------", "\n" )
      cat(object$alpha_overall, "\n" )
      cat("\n" )
      cat(object$descriptives, "\n" )
      cat("\n" )
      cat(object$deleted , "\n" )
      cat("\n" )
      cat(object$excluded , "\n" )
    }
  }
  # standard models
  if("interpret" %in% class(object) ) {
    if("model" %in% class(object) ) {
      cat("Interpretations: Model", "\n" )
      cat("----------------------", "\n" )
      cat(object$introduction, "\n" )
      cat("\n" )
      cat(object$all_significant, "\n" )
      cat("\n" )
      cat(object$positive_beta , "\n" )
      cat("\n" )
      cat(object$negative_beta , "\n" )
      cat("\n" )
      cat(object$R2 , "\n" )
    }
  }
  # DID
  if("interpret" %in% class(object) ) {
    if("did" %in% class(object) ) {
      cat("Interpretations: DID", "\n" )
      cat("--------------------", "\n" )
      cat(object$B_0, "\n" )
      cat("\n" )
      cat(object$B_1, "\n" )
      cat("\n" )
      cat(object$B_2 , "\n" )
      cat("\n" )
      cat(object$B_3 , "\n" )
      cat("\n" )
      cat(object$did_covariates , "\n" )
    }
  }
  # ITS
  if("interpret" %in% class(object) ) {
    if("sgst" %in% class(object) ) {
      cat("Interpretations: ITS", "\n" )
      cat("--------------------", "\n" )
      cat(object$its_intro, "\n" )
      cat("\n" )
      cat(object$B0, "\n" )
      cat("\n" )
      cat(object$B1, "\n" )
      cat("\n" )
      cat(object$B2 , "\n" )
      cat("\n" )
      cat(object$B3 , "\n" )
      cat("\n" )
      cat(object$its_Summary , "\n" )
      cat("\n" )
      cat(object$its_covariates , "\n" )
    }
  }
  # mgst
  if("interpret" %in% class(object) ) {
    if("mgst" %in% class(object) ) {
      cat("Interpretations: ITS", "\n" )
      cat("--------------------", "\n" )
      cat(object$its_intro, "\n" )
      cat("\n" )
      cat(object$B0, "\n" )
      cat("\n" )
      cat(object$B1, "\n" )
      cat("\n" )
      cat(object$B2 , "\n" )
      cat("\n" )
      cat(object$B3 , "\n" )
      cat("\n" )
      cat(object$B4 , "\n" )
      cat("\n" )
      cat(object$B5 , "\n" )
      cat("\n" )
      cat(object$B6 , "\n" )
      cat("\n" )
      cat(object$B7 , "\n" )
      cat("\n" )
      cat(object$its_Summary , "\n" )
      cat("\n" )
      cat(object$its_covariates , "\n" )
    }
  }
  if("interpret" %in% class(object) ) {
    if("sgmt" %in% class(object) ) {
      cat("Interpretations: ITS", "\n" )
      cat("--------------------", "\n" )
      cat(object$its_intro, "\n" )
      cat("\n" )
      cat(object$B0, "\n" )
      cat("\n" )
      cat(object$B1, "\n" )
      cat("\n" )
      for(i in 1:length(object$post_interpret)) {
        cat(object$post_interpret[i] , "\n" )
        cat("\n" )
        cat(object$txp_interpret[i] , "\n" )
        cat("\n" )
      }
      for(i in 1:length(object$its_Summary)) {
        cat(object$its_Summary[i] , "\n" )
        cat("\n" )
      }
      cat(object$its_covariates , "\n" )
    }
  }
  if("interpret" %in% class(object) ) {
    if("mgmt" %in% class(object) ) {
      cat("Interpretations: ITS", "\n" )
      cat("--------------------", "\n" )
      cat(object$its_intro, "\n" )
      cat("\n" )
      cat(object$B0, "\n" )
      cat("\n" )
      cat(object$B1, "\n" )
      cat("\n" )
      cat(object$B2 , "\n" )
      cat("\n" )
      cat(object$B3 , "\n" )
      cat("\n" )

      for(i in 1:length(object$post_interpret)) {
      cat(object$post_interpret[i] , "\n" )
      cat("\n" )
      cat(object$txp_interpret[i] , "\n" )
      cat("\n" )
      cat(object$ixp_interpret[i] , "\n" )
      cat("\n" )
      cat(object$txip_interpret[i] , "\n" )
      cat("\n" )
      }
      for(i in 1:length(object$its_Summary)) {
      cat(object$its_Summary[i] , "\n" )
      cat("\n" )
      }
      cat(object$its_covariates , "\n" )
    }
  }

}
