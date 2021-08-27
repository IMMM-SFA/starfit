#' fit_release_function
#'
#' @description fit parameters of weekly-varying release function
#' @param USRDATS_path path to USRDATS data
#' @param GRanD_path path to v1.3 of GRanD database
#' @param dam_id integer id of dam; same as GRanD ID
#' @param targets_path path to fitted targets. If NULL, fit_targets() will be run.
#' @importFrom lubridate year epiweek
#' @importFrom dplyr select group_by ungroup filter summarise pull mutate arrange if_else first last left_join lead count
#' @importFrom readr read_csv cols
#' @return tibble of observed dam data (storage, inflow, release)
#' @export
#'
fit_release_function <- function(USRDATS_path, GRanD_path, dam_id, targets_path){

  read_reservoir_attributes(GRanD_path, dam_id) ->
    reservoir_attributes

  info(paste0("Fitting release function for dam ", dam_id, ": ",
              reservoir_attributes[["DAM_NAME"]]))

  if(missing(targets_path)){

    #info("targets_path not supplied; fitting storage targets...")

    fit_targets(USRDATS_path, GRanD_path, dam_id, reservoir_attributes) -> fitted_targets

    tibble(pf = fitted_targets[["NSR upper bound"]],
           pm = fitted_targets[["NSR lower bound"]]) ->
      storage_target_parameters

  }else{
    # read storage target parameters straight from file if already fitted
    read_csv(paste0(targets_path, "/", dam_id, ".csv"),
          col_types = cols()) ->
      storage_target_parameters

    }

  if(all(is.na(storage_target_parameters))){
    problem("Storage targets unavailable due to lack of data!")
    return(
      list()
    )
  }

  reservoir_attributes[[capacity_variable]] ->
    storage_capacity_MCM

  if(fitted_targets$`weekly storage` %>% .[["year"]] %>% last() < cutoff_year){
    cutoff_year <- fitted_targets$`weekly storage` %>% .[["year"]] %>% first()
    #problem(paste0("dam ", dam_id, "cutoff year set back to ", first_year_of_data))
  }

  read_reservoir_data(USRDATS_path, dam_id) %>%
    mutate(i = i_cumecs * m3_to_Mm3 * seconds_per_day,
           r = r_cumecs * m3_to_Mm3 * seconds_per_day) %>%
    select(date, s = s_MCM, i, r) %>%
    mutate(year = year(date), epiweek = epiweek(date)) %>%
    filter(year >= cutoff_year) -> daily_ops

  daily_ops %>% filter(s + i < storage_capacity_MCM) ->
    daily_ops_non_spill_periods

  daily_ops %>%
    aggregate_to_epiweeks() %>%
    back_calc_missing_flows() %>%
    filter(!is.na(i) & !is.na(r),
           i >= 0, r >= 0) ->
    weekly_ops_NA_removed


  # RETURN BLANK IF INSUFFICIENT RELEASE/INFOW DATA
  if(nrow(weekly_ops_NA_removed) <= min_r_i_datapoints){
    problem("Insufficient data to build release function")
    fitted_targets[["mean inflow from GRAND. (MCM / wk)"]] <- reservoir_attributes[["i_MAF_MCM"]] / weeks_per_year
    fitted_targets[["mean inflow from obs. (MCM / wk)"]] <- NA_real_
    fitted_targets[["release harmonic parameters"]] <- rep(NA_real_, 4)
    fitted_targets[["release residual model coefficients"]] <- rep(NA_real_, 3)
    fitted_targets[["release constraints"]] <- c(NA_real_, NA_real_)
    return(
      fitted_targets
    )
  }

  # get most representative mean flow value available
  # either from daily or weekly (back-calculated) data

  daily_ops %>% filter(!is.na(i)) %>% .[["i"]] -> i_daily

  if(length(i_daily) > min_r_i_datapoints * 7){
    i_mean <- mean(i_daily) * 7
  }else{
    i_mean <- mean(weekly_ops_NA_removed[["i"]])
  }

  weekly_ops_NA_removed %>%
    left_join(convert_parameters_to_targets(storage_target_parameters[["pf"]],
                                            "upper"), by = "epiweek") %>%
    left_join(convert_parameters_to_targets(storage_target_parameters[["pm"]],
                                            "lower"), by = "epiweek") %>%
    mutate(avail_pct = 100 * ((s_start) / storage_capacity_MCM)) %>%
    mutate(availability_status = (avail_pct - lower) / (upper - lower)) %>%
    mutate(
      i_st = (i / i_mean) - 1,
      r_st = (r / i_mean) - 1
    ) ->
    training_data_unfiltered

  # define max and min release constraints
  daily_ops_non_spill_periods %>% filter(!is.na(r)) %>% .[["r"]] -> r_daily

  # use daily release data to define max release (if possible)
  if(length(r_daily) > min_r_maxmin_days){
    r_st_max <- ((quantile(r_daily, r_st_max_quantile) %>% unname() %>% round(4) * 7) / i_mean) - 1
    r_st_min <- ((quantile(r_daily, r_st_min_quantile) %>% unname() %>% round(4) * 7) / i_mean) - 1
  }else{
    training_data_unfiltered %>%
      filter(s_start + i < storage_capacity_MCM) %>% .[["r_st"]] -> r_st_vector
    quantile(r_st_vector, r_st_min_quantile, na.rm = TRUE) %>% unname() %>% round(4) -> r_st_min
    quantile(r_st_vector, r_st_max_quantile, na.rm = TRUE) %>% unname() %>% round(4) -> r_st_max
  }

  # create final training data for normal operating period
  training_data_unfiltered %>%
    filter(availability_status <= 1,
           availability_status > 0) ->
    training_data

  ### harmonic regression (two harmonics) for standardized release
    lm(
      data = training_data,
      r_st ~ 0 +
        # first harmonic
        sin(2 * pi * epiweek / 52) +
        cos(2 * pi * epiweek / 52) +
        # second harmonic
        sin(4 * pi * epiweek / 52) +
        cos(4 * pi * epiweek / 52)
    ) %>% .[["coefficients"]] %>% unname() %>%
      round(4) ->
      st_r_harmonic

    training_data %>%
      mutate(st_r_harmonic =
               st_r_harmonic[1] * sin(2 * pi * epiweek / 52) +
               st_r_harmonic[2] * cos(2 * pi * epiweek / 52) +
               st_r_harmonic[3] * sin(4 * pi * epiweek / 52) +
               st_r_harmonic[4] * cos(4 * pi * epiweek / 52)) %>%
      # ggplot(aes(epiweek, r_st)) + geom_point() +
      # geom_point(aes(y = st_r_harmonic), col = "blue")
      mutate(r_st_resid = r_st - st_r_harmonic) ->
      data_for_linear_model_of_release_residuals

    lm(
      data = data_for_linear_model_of_release_residuals,
      r_st_resid ~ availability_status + i_st
    ) -> st_r_residual_model

    st_r_residual_model[["coefficients"]] %>% unname() %>%
      round(3) ->
      st_r_residual_model_coef

    # deal with any negative coefficients by setting to zero and re-fitting
    if(st_r_residual_model_coef[2] < 0 & st_r_residual_model_coef[3] >= 0){
      lm(
        data = data_for_linear_model_of_release_residuals,
        r_st_resid ~ i_st
      ) -> st_r_residual_model

      c(st_r_residual_model[["coefficients"]][[1]],
        0,
        st_r_residual_model[["coefficients"]][[2]]) %>%
        round(3) ->
        st_r_residual_model_coef
    }

    if(st_r_residual_model_coef[3] < 0 & st_r_residual_model_coef[2] >= 0){
      lm(
        data = data_for_linear_model_of_release_residuals,
        r_st_resid ~ availability_status
      ) -> st_r_residual_model

      c(st_r_residual_model[["coefficients"]][[1]],
        st_r_residual_model[["coefficients"]][[2]],
        0) %>%
        round(3) ->
        st_r_residual_model_coef
    }

    if(summary(st_r_residual_model) %>% .[["adj.r.squared"]] < r_sq_tol |
       st_r_residual_model_coef[2] < 0 |
       st_r_residual_model_coef[3] < 0) {
      info("Release residual model will be discarded; (release will be based harmonic function only)")
      st_r_residual_model_coef <- c(0, 0, 0)
    }

    # data_for_linear_model_of_release_residuals %>%
    #   mutate(r_st_predicted =
    #            st_r_residual_model_coef[1] +
    #            st_r_residual_model_coef[2] * availability_status +
    #            st_r_residual_model_coef[3] * i_st,
    #          r_pred = (1 + (r_st_predicted + st_r_harmonic)) * mean(i))
      # ggplot(aes(r, r_pred)) + geom_point() + geom_abline(slope = 1) +
      # scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10") %>%
      # NULL

    fitted_targets[["mean inflow from GRAND. (MCM / wk)"]] <- reservoir_attributes[["i_MAF_MCM"]] / weeks_per_year
    fitted_targets[["mean inflow from obs. (MCM / wk)"]] <- i_mean
    fitted_targets[["release harmonic parameters"]] <- st_r_harmonic
    fitted_targets[["release residual model coefficients"]] <- st_r_residual_model_coef
    fitted_targets[["release constraints"]] <- c(r_st_min, r_st_max)

    return(fitted_targets)

}
