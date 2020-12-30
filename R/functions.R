#' fit_constrained_harmonic
#'
#' @description fit parameters of a constrained harmonic
#' @param data_for_harmonic_fitting tibble with headers epiweek and s_pct
#' @importFrom nloptr nloptr
#' @return optimization output; a list
#'
fit_constrained_harmonic <- function(data_for_harmonic_fitting){

  # get params to initialize model
  lm(s_pct ~ sin(2 * pi * epiweek / 52) + cos( 2 * pi * epiweek / 52),
     data = data_for_harmonic_fitting) ->
    initial_model

  intercept <- initial_model$coefficients[[1]]
  sin_term <- initial_model$coefficients[[2]]
  cosine_term <- initial_model$coefficients[[3]]

  # parameters 4 and 5 (upper and lower constraints on harmonic)
  ub_on_curve <- unname(data_for_harmonic_fitting$s_pct %>% quantile(0.9))
  lb_on_curve <- unname(data_for_harmonic_fitting$s_pct %>% quantile(0.1))

  # function to evaluate goodness-of-fit of fitted harmonic...
  # ... (used as objective function in optimization of constrained harmonic)
  evaluate_harmonic <- function(x){

    sin_term_vector <- sin(2 * pi * data_for_harmonic_fitting[["epiweek"]] / 52)
    cosin_term_vector <- cos(2 * pi * data_for_harmonic_fitting[["epiweek"]] / 52)

    x[1] + x[2] * sin_term_vector + x[3] * cosin_term_vector ->
      fitted_harmonic
    fitted_harmonic[which(fitted_harmonic > x[4])] <- x[4]
    fitted_harmonic[which(fitted_harmonic < x[5])] <- x[5]

    (data_for_harmonic_fitting[["s_pct"]] - fitted_harmonic) ^ 2 %>%
      mean() %>% sqrt()

  }

  nloptr(c(intercept, sin_term, cosine_term, ub_on_curve, lb_on_curve),
         eval_f = evaluate_harmonic,
         eval_g_ineq = function(x) x[5] - x[4],
         opts = list(algorithm = "NLOPT_LN_COBYLA", xtol_rel = 1e-5, maxeval = 1000),
         lb = c(0, -Inf, -Inf, 0, 0),
         ub = c(Inf, Inf, Inf, 100, intercept)) ->
    optimized_constrained_harmonic

  return(optimized_constrained_harmonic)

}

