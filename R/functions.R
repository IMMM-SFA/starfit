#' convert_parameters_to_storage_targets
#'
#' @description fit parameters of a constrained harmonic
#' @param parameters vector of length 5 giving, in order, intercept, sine term, cosine term, and upper and lower constraints of the harmonic.
#' @param target_name optional. Character string naming the target. E.g., "flood" or "conservation." Default is simply "target."
#' @param constrain logical. Constrain targets?
#' @return a tibble of storage target levels by week
#' @importFrom tibble tibble
#' @importFrom dplyr mutate if_else
#' @export
#'
convert_parameters_to_targets <- function(parameters, target_name, constrain = TRUE){

  parameters[1] -> p1
  parameters[2] -> p2
  parameters[3] -> p3
  if_else(constrain == TRUE, parameters[4], Inf) -> p4
  if_else(constrain == TRUE, parameters[5], -Inf) -> p5

  tibble(epiweek = 1:52) %>%
    mutate(target = p1 + p2 * sin(2 * pi * epiweek / 52) + p3 * cos(2 * pi * epiweek / 52)) %>%
    mutate(target = if_else(target > p4, p4, target)) %>%
    mutate(target = if_else(target < p5, p5, target)) ->
    targets

  if(!missing(target_name)) names(targets) <- c("epiweek", target_name)

  return(targets)

}


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

  # if no sine or cosine term to the harmonic, assume a constant...
  # ... and return result with intercept parameter only
  if(round(intercept, 5) == 100 | round(intercept, 5) == 0 |
     round(sin_term, 5) == 0 & round(cosine_term, 5) == 0 |
     round(ub_on_curve, 1) == round(lb_on_curve, 1)){
    return(
      list(solution = c(intercept, 0, 0, Inf, -Inf))
    )
  }

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


#' find_closest_dam
#'
#' @description finds the dam that is closest in terms of purposes served and Euclidean distance
#' @param dam_attr attributes of target dam
#' @param other_dams table of attributes for possible canditate dams to replicate
#' @return GRAND_ID of the target dam
#' @importFrom tibble tibble
#' @importFrom dplyr mutate if_else arrange select first
#' @importFrom sp spDistsN1
#' @export
#'
find_closest_dam <- function(dam_attr, other_dams){

  if(nrow(other_dams) == 0) return(tibble(GRAND_ID = NA_character_, matches = -Inf))

  dam_attr[["flood"]] -> fl
  dam_attr[["hydro"]] -> hy
  dam_attr[["supply"]] -> su
  dam_attr[["irr"]] -> ir


  # determine best matches
  other_dams %>%
    mutate(flood_match = if_else(flood == fl, 1, 0),
           hydro_match = if_else(hydro == hy, 1, 0),
           supply_match = if_else(supply == su, 1, 0),
           irr_match = if_else(irr == ir, 1, 0),
           matches = flood_match + hydro_match + supply_match + irr_match) %>%
    arrange(-matches) %>%
    filter(matches == first(.[["matches"]])) -> best_matches

  # return if there is a clear winning match
  if(nrow(best_matches) == 1) return(select(best_matches, GRAND_ID, matches))

  # otherwise find closest distance

  best_matches %>%
    mutate(
      euc_dist = spDistsN1(
        as.matrix(select(best_matches, lon, lat)),
        as.matrix(select(dam_attr, lon, lat))
        )
      ) %>%
    arrange(-euc_dist) %>% .[1,] -> best_match

  return(select(best_match, GRAND_ID, matches))

}


#' aggregate_to_epiweeks
#'
#' Aggregate data to epiweek and back-calculate release and inflow using mass balance
aggregate_to_epiweeks <- function(x){

    which(x$epiweek %>% diff() != 0) %>% first() -> start_snip

    if(!(start_snip %in% 1:8)) stop("first water week duration > 8 days!")

    if(start_snip < 7) {
      x_snipped <- x[-(1:start_snip), ]
    }else{
      x_snipped <- x
    }

    x_snipped %>%
      mutate(s_end = lead(s, 8)) %>%
      group_by(year, epiweek) %>%
      summarise(i = sum(i),
                r = sum(r),
                s_start = first(s),
                s_end = first(s_end)) %>%
      ungroup() %>%
      filter(epiweek != 53,
             !is.na(s_end))
}

#' back_calc_missing_flows
#'
#' Compute i or r from mass balance (if either is missing)
back_calc_missing_flows <- function(x){
  x %>%
    mutate(s_change = s_end - s_start,
           r_ = if_else((i - s_change) < 0, 0, i - s_change),
           i_ = r + s_change) -> x_

  x_ %>%
    filter(!is.na(r) & !is.na(i)) -> full_data_points

  if(nrow(full_data_points) == 0){
    data_points_on_most_data_scarce_epiweek <- -Inf
  }else{
    full_data_points %>%
      group_by(epiweek) %>%
      count() %>% .[["n"]] %>% min() ->
      data_points_on_most_data_scarce_epiweek
  }



  if(data_points_on_most_data_scarce_epiweek >= min_r_i_datapoints){
    return(
      x_ %>%
        select(year, epiweek, s_start, i, r)
    )
  }

  sum(is.na(x_$i)) -> missing_i
  sum(is.na(x_$r)) -> missing_r


  if(missing_i <= missing_r){
    return(
      x_ %>%
        mutate(i = if_else(is.na(i) & !is.na(r), i_, i),
               r = if_else(is.na(r_), r, r_)) %>%
        select(year, epiweek, s_start, i, r)
    )
  }

  if(missing_i > missing_r){
    return(
      x_ %>%
        mutate(r = if_else(is.na(r) & !is.na(i), r_, r),
               i = if_else(is.na(i_), i, i_)) %>%
        select(year, epiweek, s_start, i, r)
    )

  }



}
