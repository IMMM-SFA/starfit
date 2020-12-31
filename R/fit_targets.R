#' fit_targets
#'
#' @description fit parameters of storage targets
#' @param USRDATS_path path to USRDATS data
#' @param dam_id integer id of dam; same as GRanD ID
#' @importFrom lubridate year epiweek
#' @importFrom dplyr select group_by ungroup filter summarise pull mutate arrange if_else first last left_join
#' @importFrom vroom vroom cols
#' @return tibble of observed dam data (storage, inflow, release)
#' @export
#'
fit_targets <- function(USRDATS_path, dam_id){

  read_reservoir_attributes(USRDATS_path, dam_id) ->
    reservoir_attributes

  info(paste0("Fitting targets for dam ", dam_id, ": ",
                 reservoir_attributes[["DAM_NAME"]]))

  reservoir_attributes[[capacity_variable]] ->
    storage_capacity_MCM

  read_reservoir_data(USRDATS_path, dam_id) %>%
    select(date, s_MCM) %>%
    filter(!is.na(s_MCM)) ->
    storage_daily

  if(nrow(storage_daily) < min_allowable_days_of_storage){
    return(
      list(
        "id" = dam_id,
        "weekly storage" = tibble(),
        "flood target parameters" = rep(NA_real_, 5),
        "conservation target parameters" = rep(NA_real_, 5)
      )
    )
  }

  storage_daily$date %>% first() -> start_date
  storage_daily$date %>% last() -> end_date

  left_join(
    tibble(date = seq.Date(start_date, end_date, by = 1)),
    storage_daily, by = "date"
  ) -> storage_daily_clipped

  year(end_date) -> last_year_of_data
  year(start_date) -> first_year_of_data

  if(last_year_of_data < cutoff_year){
    cutoff_year <- first_year_of_data
    problem(paste0("dam ", dam_id, "cutoff year set back to ", first_year_of_data))
  }

  # convert to weekly storage (as % of capacity)
  storage_daily_clipped %>%
    mutate(year = year(date), epiweek = epiweek(date)) %>%
    filter(year >= cutoff_year) %>%
    group_by(year, epiweek) %>%
    summarise(s_pct = round(100 * median(s_MCM) / storage_capacity_MCM, 2)) %>%
    ungroup() %>%
    filter(epiweek %in% 1:52) ->
    storage_weekly

  # throw warning for cases where...
  # ... (1) average weekly storage levels exceed capacity, and ...
  # ... (2) average weekly storage levels are less than zero.
  storage_weekly %>%
    filter(s_pct > 100) -> capacity_violations

  storage_weekly %>%
    filter(s_pct < 0) -> minimum_violations

  if(nrow(capacity_violations) > 0){
    problem(paste0(nrow(capacity_violations), " capacity violations found for dam ", dam_id, "... "))
  }
  if(nrow(minimum_violations) > 0){
    problem(paste0(nrow(minimum_violations), " minimum violations found for dam ", dam_id, "... "))
  }

  storage_weekly %>%
    mutate(s_pct = if_else(s_pct > 100, 100, s_pct),
           s_pct = if_else(s_pct < 0, 0, s_pct)) ->
    storage_weekly_for_fitting

  storage_weekly_for_fitting %>%
    group_by(epiweek) %>%
    mutate(rank = rank(-s_pct, ties.method = "first", na.last = "keep")) %>%
    filter(rank <= n_points) %>%
    ungroup() %>% select(epiweek, s_pct) %>%
    arrange(epiweek) ->
    data_for_flood_harmonic

  storage_weekly_for_fitting %>%
    group_by(epiweek) %>%
    mutate(rank = rank(s_pct, ties.method = "first", na.last = "keep")) %>%
    filter(rank <= n_points) %>%
    ungroup() %>% select(epiweek, s_pct) %>%
    arrange(epiweek) ->
    data_for_conservation_harmonic


  # fit the flood harmonic
  fit_constrained_harmonic(data_for_flood_harmonic) %>%
    .[["solution"]] ->
    p_flood_harmonic

  # fit the conservation harmonic
  fit_constrained_harmonic(data_for_conservation_harmonic) %>%
    .[["solution"]] ->
    p_conservation_harmonic

  # evaluate targets to remove any superfluous constraints
  convert_parameters_to_storage_targets(p_flood_harmonic,
                                        constrain = FALSE) -> targets_flood
  convert_parameters_to_storage_targets(p_conservation_harmonic,
                                        constrain = FALSE) -> targets_cons

  targets_flood[["target"]] %>% max() -> max_flood_target
  targets_flood[["target"]] %>% min() -> min_flood_target
  targets_cons[["target"]] %>% max() -> max_cons_target
  targets_cons[["target"]] %>% min() -> min_cons_target

  if(p_flood_harmonic[4] > max_flood_target) p_flood_harmonic[4] <- Inf
  if(p_flood_harmonic[5] < min_flood_target) p_flood_harmonic[5] <- -Inf
  if(p_conservation_harmonic[4] > max_cons_target) p_conservation_harmonic[4] <- Inf
  if(p_conservation_harmonic[5] < min_cons_target) p_conservation_harmonic[5] <- -Inf

  return(
    list(
      "id" = dam_id,
      "weekly storage" = storage_weekly,
      "flood target parameters" = p_flood_harmonic,
      "conservation target parameters" = p_conservation_harmonic
    )
  )

}




