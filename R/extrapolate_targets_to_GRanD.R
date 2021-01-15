#' extrapolate_targets_to_GRanD
#'
#' @description use an set of inferred storage targets to extrapolate storage targets to all dams in GRanD
#' @param USRDATS_path path to USRDATS data
#' @param targets_path path to fitted targets (generated using fit_targets())
#' @importFrom dplyr select group_by ungroup filter summarise pull mutate arrange if_else first last left_join
#' @importFrom vroom vroom cols
#' @importFrom purrr map_dfr
#' @return tibble dam ids to copy
#' @export
#'
extrapolate_targets_to_GRanD <- function(USRDATS_path, targets_path){

  # read GRanD data and join HUCs
  read_reservoir_attributes(USRDATS_path) %>%
    select(GRAND_ID, DAM_NAME, STATE, S_RATIO,
           flood = P_FLC, hydro = P_HYD, supply = P_SUP, irr = P_IRR,
           lon = LONG, lat = LAT) %>%
    filter(!is.na(GRAND_ID),
           !grepl("CAN", STATE),
           !grepl("MEX", STATE)) %>%
    left_join(read_GRanD_HUC8(), by = "GRAND_ID") %>%
    mutate(GRAND_ID = as.character(GRAND_ID),
           HUC8 = as.character(HUC8)) %>%
    mutate(HUC4 = substr(HUC8, 1, 4)) ->
    dam_attributes_and_HUCs

  # get list of dams that are already fitted with targets
  list.files(targets_path) %>%
    substr(1, nchar(.) - 4) ->
    fitted_dams

  # get list of dams that lack targets and need to be extrapolated
  dam_attributes_and_HUCs[["GRAND_ID"]] %>%
    .[which(!. %in% fitted_dams)] ->
    unfitted_dams

  # cycle through unfitted dams to find best candidate dam for copy

  unfitted_dams %>%
    map_dfr(function(dam){

      dam_attributes_and_HUCs %>%
        filter(GRAND_ID == dam) -> dam_attr

      dam_attributes_and_HUCs %>%
        filter(HUC4 %in% dam_attr[["HUC4"]],
               GRAND_ID != dam,
               GRAND_ID %in% fitted_dams) ->
        dams_same_HUC4

      dam_attributes_and_HUCs %>%
        mutate(HUC2 = substr(HUC4, 1, 2)) %>%
        filter(!HUC4 %in% dam_attr[["HUC4"]],
               HUC2 %in% substr(dam_attr[["HUC4"]], 1, 2),
               GRAND_ID != dam,
               GRAND_ID %in% fitted_dams) ->
        dams_same_HUC2

      find_closest_dam(dam_attr, dams_same_HUC4) -> huc4_match
      find_closest_dam(dam_attr, dams_same_HUC2) -> huc2_match

      if(huc4_match[["matches"]] >= 2) return(tibble(dam, match = huc4_match[["GRAND_ID"]]))

      if(huc4_match[["matches"]] < 2 & huc2_match[["matches"]] >= 2) return(tibble(dam, match = huc2_match[["GRAND_ID"]]))

      if(huc4_match[["matches"]] > 0) return(tibble(dam, match = huc4_match[["GRAND_ID"]]))

      if(huc2_match[["matches"]] > 0) return(tibble(dam, match = huc2_match[["GRAND_ID"]]))

      return(tibble(dam, match = huc2_match[["GRAND_ID"]]))

    })

}
