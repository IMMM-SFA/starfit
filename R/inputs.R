#' read_reservoir_data
#'
#' @description reads raw reservoir time series data
#' @param USRDATS_path directory containing reservoir input time series
#' @param dam_id integer id of dam; same as GRanD ID
#' @importFrom vroom vroom cols
#' @importFrom dplyr select
#' @return tibble of observed dam data (storage, inflow, release)
#' @export
#'
read_reservoir_data <- function(USRDATS_path, dam_id){

  vroom(paste0(USRDATS_path, "/TimeSeriesv2/",
               dam_id, ".csv"),
        col_types = cols(date = "D",
                         storage = "d",
                         inflow = "d",
                         outflow = "d",
                         elevation = "d",
                         evaporation = "d"),
        progress = FALSE) %>%
    # stamp units into column names
    select(date, s_MCM = storage, i_cumecs = inflow, r_cumecs = outflow)

}

#' read_reservoir_attributes
#'
#' @description reads reservoir time series data
#' @param USRDATS_path directory containing reservoir input time series
#' @param dam_id integer id of dam; same as GRanD ID. If NULL, all attributes are returned.
#' @importFrom vroom vroom cols
#' @importFrom dplyr select
#' @return tibble of reservoir attributes for selected dams
#' @export
#'
read_reservoir_attributes <- function(USRDATS_path, dam_id = NULL){

  vroom(paste0(USRDATS_path, "/attributes/",
               "Reservoir_Attributes.csv"),
        col_types = cols(), progress = FALSE) -> attributes_all

  if(is.null(dam_id)){
    return(attributes_all)
  }

  attributes_all %>%
    subset(GRAND_ID == dam_id) ->
    attributes_dam

  stopifnot(nrow(attributes_dam) == 1)

  return(attributes_dam)

}
