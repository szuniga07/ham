#' Interrupted time series analysis effects
#'
#' @param model an interrupted time series (ITS) model with the "lm" class,
#' @param type analysis type for single or multiple groups and single or multiple
#' time periods. If selected type="sgst", it is single-group single-time;
#' type="sgmt", it is single-group multiple-time; type="mgst", it is multiple-group single-time;
#' and type="mgmt", it is multiple-group multiple-time.
#'
#' @return a data.frame object of ITS effects and summary statistics.
#' Generally run within assess().
#' @export
#'
#' @examples
#' i21 <- assess(formula=survey ~ ., data=hosprog, intervention = "program",topcode =NULL,
#' int.time="month", regression="none", interrupt=5, its="two", newdata=TRUE, propensity=NULL)
#' itsEffect(model= i21$ITS, type= "mgst")
#' @importFrom stats coef pt qt vcov
itsEffect <- function(model, type) {
  # Gets degrees of freedom
  its_df_resid <- model[["df.residual"]]
  #Using a matrix to get standard errors
  model.vcov <- stats::vcov(model)
  # Determine length of coefficients
  total_coefs <- length(stats::coef(model))
  #This gives DFs if using rms' Gls objects
  its_df_resid <- ifelse(is.null(its_df_resid), model[["dims"]][["N"]] - total_coefs, its_df_resid)
  # Get effects, coefficient matrix, standard errors, p-value
  if(type == "sgst") {
    # Treatment effect
    Treated <- stats::coef(model)[[2]] + stats::coef(model)[[4]]
    # Minimum coefficients from the model
    expect_coefs <- length(c("ITS.Time", "post1", "txp1")) + 1 # 1 intercept
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
    L95 <- Treated - (stanErr * stats::qt(.975, its_df_resid))
    U95 <- Treated + (stanErr * stats::qt(.975, its_df_resid))
    effect_list <- list("Intervention"= c(Treated, stanErr, t.value, round(p.value, 5),
                                          L95, U95))
    effect_list <- do.call(rbind.data.frame, effect_list)
    colnames(effect_list) <- c("Effect", "S.E.", "t", "p.value", "L95","U95")
    rownames(effect_list) <- c("Intervention")
  }
  if(type == "sgmt") {
    Treated1 <- stats::coef(model)[[2]] + stats::coef(model)[[4]]
    Treated2 <- stats::coef(model)[[2]] + stats::coef(model)[[4]] + stats::coef(model)[[6]]
    expect_coefs <- length(c("ITS.Time", "post1","txp1", "post2", "txp2")) + 1
    add_coefs <- total_coefs - expect_coefs
    mat_coefs1 <- matrix(c(0, 1, 0, 1, 0, 0, rep(0, add_coefs)), 1)
    mat_coefs2 <- matrix(c(0, 1, 0, 1, 0, 1, rep(0, add_coefs)), 1)
    stanErr1 <- sqrt(mat_coefs1 %*% model.vcov %*% t(mat_coefs1))
    stanErr2 <- sqrt(mat_coefs2 %*% model.vcov %*% t(mat_coefs2))
    t.value1 <- Treated1/stanErr1
    t.value2 <- Treated2/stanErr2
    p.value1 <- (1 - stats::pt(abs(t.value1), its_df_resid)) * 2
    p.value2 <- (1 - stats::pt(abs(t.value2), its_df_resid)) * 2
    L951 <- Treated1 - (stanErr1 * stats::qt(.975, its_df_resid))
    L952 <- Treated2 - (stanErr2 * stats::qt(.975, its_df_resid))
    U951 <- Treated1 + (stanErr1 * stats::qt(.975, its_df_resid))
    U952 <- Treated2 + (stanErr2 * stats::qt(.975, its_df_resid))
    effect_list <- list("Intervention1"= c(Treated1, stanErr1, t.value1, round(p.value1, 5),
                                           L951, U951),
                        "Intervention2"= c(Treated2, stanErr2, t.value2, round(p.value2, 5),
                                           L952, U952))
    effect_list <- do.call(rbind.data.frame, effect_list)
    colnames(effect_list) <- c("Effect", "S.E.", "t", "p.value", "L95","U95")
    rownames(effect_list) <- c("Intervention.1","Intervention.2")
  }
  if(type == "mgst") {
    Treated <- stats::coef(model)[[2]] + stats::coef(model)[[4]] + stats::coef(model)[[6]] + stats::coef(model)[[8]]
    Control <- stats::coef(model)[[2]] + stats::coef(model)[[6]]
    Difference <- stats::coef(model)[[4]] + stats::coef(model)[[8]]
    expect_coefs <- length(c("ITS.Time", "ITS.Int", "txi", "post1", "txp1", "ixp1", "txip1")) + 1
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
    L951T <- Treated - (stanErr1T * stats::qt(.975, its_df_resid))
    U951T <- Treated + (stanErr1T * stats::qt(.975, its_df_resid))
    L951C <- Control - (stanErr1C * stats::qt(.975, its_df_resid))
    U951C <- Control + (stanErr1C * stats::qt(.975, its_df_resid))
    L951D <- Treated - (stanErr1D * stats::qt(.975, its_df_resid))
    U951D <- Treated + (stanErr1D * stats::qt(.975, its_df_resid))
    effect_list <- list("Intervention"= c(Treated, stanErr1T, t.value1T,
                                          round(p.value1T, 5), L951T, U951T),
                        "Control"= c(Control, stanErr1C, t.value1C,
                                     round(p.value1C, 5), L951C, U951C),
                        "Difference"= c(Difference, stanErr1D, t.value1D,
                                        round(p.value1D, 5), L951D, U951D))
    effect_list <- do.call(rbind.data.frame, effect_list)
    colnames(effect_list) <- c("Effect", "S.E.", "t", "p.value", "L95","U95")
    rownames(effect_list) <- c("Intervention","Control","Difference")
  }
  if(type == "mgmt") {
    Treated1 <- stats::coef(model)[[2]] + stats::coef(model)[[4]] + stats::coef(model)[[6]] + stats::coef(model)[[8]]
    Treated2 <- stats::coef(model)[[2]] + stats::coef(model)[[4]] + stats::coef(model)[[6]] + stats::coef(model)[[8]] +
      stats::coef(model)[[10]] + stats::coef(model)[[12]]
    Control1 <- stats::coef(model)[[2]] + stats::coef(model)[[6]]
    Control2 <- stats::coef(model)[[2]] + stats::coef(model)[[6]] + stats::coef(model)[[10]]
    Difference1 <- stats::coef(model)[[4]] + stats::coef(model)[[8]]
    Difference2 <- stats::coef(model)[[4]] + stats::coef(model)[[8]] + stats::coef(model)[[12]]
    expect_coefs <- length(c("ITS.Time", "ITS.Int", "txi", "post1", "txp1", "ixp1",
                             "txip1", "post2", "txp2", "ixp2", "txip2")) + 1
    add_coefs <- total_coefs - expect_coefs
    mat_coefs1T <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, rep(0, add_coefs)), 1)
    mat_coefs2T <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, rep(0, add_coefs)), 1)
    mat_coefs1C <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, rep(0, add_coefs)), 1)
    mat_coefs2C <- matrix(c(0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, rep(0, add_coefs)), 1)
    mat_coefs1D <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, rep(0, add_coefs)), 1)
    mat_coefs2D <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, rep(0, add_coefs)), 1)
    stanErr1T <- sqrt(mat_coefs1T %*% model.vcov %*% t(mat_coefs1T))
    stanErr1C <- sqrt(mat_coefs1C %*% model.vcov %*% t(mat_coefs1C))
    stanErr1D <- sqrt(mat_coefs1D %*% model.vcov %*% t(mat_coefs1D))
    stanErr2T <- sqrt(mat_coefs2T %*% model.vcov %*% t(mat_coefs2T))
    stanErr2C <- sqrt(mat_coefs2C %*% model.vcov %*% t(mat_coefs2C))
    stanErr2D <- sqrt(mat_coefs2D %*% model.vcov %*% t(mat_coefs2D))
    t.value1T <- Treated1/stanErr1T
    t.value1C <- Control1/stanErr1C
    t.value1D <- Difference1/stanErr1D
    t.value2T <- Treated2/stanErr2T
    t.value2C <- Control2/stanErr2C
    t.value2D <- Difference2/stanErr2D
    p.value1T <- (1 - stats::pt(abs(t.value1T), its_df_resid)) * 2
    p.value1C <- (1 - stats::pt(abs(t.value1C), its_df_resid)) * 2
    p.value1D <- (1 - stats::pt(abs(t.value1D), its_df_resid)) * 2
    p.value2T <- (1 - stats::pt(abs(t.value2T), its_df_resid)) * 2
    p.value2C <- (1 - stats::pt(abs(t.value2C), its_df_resid)) * 2
    p.value2D <- (1 - stats::pt(abs(t.value2D), its_df_resid)) * 2
    L951T <- Treated1 - (stanErr1T * stats::qt(.975, its_df_resid))
    U951T <- Treated1 + (stanErr1T * stats::qt(.975, its_df_resid))
    L952T <- Treated2 - (stanErr2T * stats::qt(.975, its_df_resid))
    U952T <- Treated2 + (stanErr2T * stats::qt(.975, its_df_resid))
    L951C <- Control1 - (stanErr1C * stats::qt(.975, its_df_resid))
    U951C <- Control1 + (stanErr1C * stats::qt(.975, its_df_resid))
    L952C <- Control2 - (stanErr2C * stats::qt(.975, its_df_resid))
    U952C <- Control2 + (stanErr2C * stats::qt(.975, its_df_resid))
    L951D <- Difference1 - (stanErr1D * stats::qt(.975, its_df_resid))
    U951D <- Difference1 + (stanErr1D * stats::qt(.975, its_df_resid))
    L952D <- Difference2 - (stanErr2D * stats::qt(.975, its_df_resid))
    U952D <- Difference2 + (stanErr2D * stats::qt(.975, its_df_resid))
    effect_list <- list("Intervention1"= c(Treated1, stanErr1T, t.value1T,
                                           round(p.value1T, 5), L951T, U951T),
                        "Control1"= c(Control1, stanErr1C, t.value1C,
                                      round(p.value1C,5), L951C, U951C),
                        "Difference1"= c(Difference1, stanErr1D, t.value1D,
                                         round(p.value1D,5), L951D, U951D),
                        "Intervention2"= c(Treated2, stanErr2T, t.value2T,
                                           round(p.value2T,5), L952T, U952T),
                        "Control2"= c(Control2, stanErr2C, t.value2C,
                                      round(p.value2C,5), L952C, U952C),
                        "Difference2"= c(Difference2, stanErr2D, t.value2D,
                                         round(p.value2D,5), L952D, U952D))
    effect_list <- do.call(rbind.data.frame, effect_list)
    colnames(effect_list) <- c("Effect", "S.E.", "t", "p.value", "L95","U95")
    rownames(effect_list) <- c("Intervention.1","Control.1","Difference.1","Intervention.2",
                               "Control.2","Difference.2")
  }
  return(effect_list)
}
