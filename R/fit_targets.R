#' fit_targets
#'
#' @description fit parameters of storage targets
#' @param USRDATS_path path to USRDATS data
#' @param dam_id integer id of dam; same as GRanD ID
#' @importFrom lubridate year epiweek
#' @importFrom dplyr select group_by ungroup filter summarise pull mutate arrange
#' @importFrom vroom vroom cols
#' @return tibble of observed dam data (storage, inflow, release)
#' @export
#'
fit_targets <- function(USRDATS_path, dam_id){

  read_reservoir_attributes(USRDATS_path, dam_id) ->
    reservoir_attributes

  reservoir_attributes[[capacity_variable]] ->
    storage_capacity_MCM

  read_reservoir_data(USRDATS_path, dam_id) %>%
    select(date, s_MCM) ->
    storage_daily

  # convert to weekly storage (as % of capacity)
  storage_daily %>%
    mutate(year = year(date), epiweek = epiweek(date)) %>%
    filter(year >= cutoff_year) %>%
    group_by(year, epiweek) %>%
    summarise(s_pct = round(100 * median(s_MCM) / storage_capacity_MCM, 2)) %>%
    ungroup() %>%
    filter(epiweek %in% 1:52) ->
    storage_weekly

  # throw warning for cases average weekly storage levels exceed capacity
  storage_weekly %>%
    filter(s_pct > 100) -> capacity_violations

  message(paste0(nrow(capacity_violations), " capacity violations found for dam ", dam_id, "... ",
                 paste(pull(capacity_violations), collapse = " | ")))

  storage_weekly %>%
    mutate(s_pct = if_else(s_pct > 100, 100, s_pct)) ->
    storage_weekly_for_fitting

  storage_weekly_for_fitting %>%
    group_by(epiweek) %>%
    mutate(rank = rank(-s_pct)) %>%
    filter(rank <= n_points) %>%
    ungroup() %>% select(epiweek, s_pct) %>%
    arrange(epiweek) ->
    data_for_flood_harmonic

  storage_weekly_for_fitting %>%
    group_by(epiweek) %>%
    mutate(rank = rank(s_pct)) %>%
    filter(rank <= n_points) %>%
    ungroup() %>% select(epiweek, s_pct) %>%
    arrange(epiweek) ->
    data_for_conservation_harmonic


  # fit the flood harmonic
  fit_constrained_harmonic(data_for_flood_harmonic) ->
    flood_harmonic_opt_output

  # fit the conservation harmonic
  fit_constrained_harmonic(data_for_conservation_harmonic) ->
    conservation_harmonic_opt_output

  return(
    list(
      "id" = dam_id,
      "weekly storage" = storage_weekly,
      "flood target parameters" = flood_harmonic_opt_output$solution,
      "conservation target parameters" = conservation_harmonic_opt_output$solution
    )
  )

}




