#' read_reservoir_data
#'
#' @description reads raw reservoir time series data
#' @param USRDATS_path directory containing reservoir input time series
#' @param dam_id integer id of dam; same as GRanD ID
#' @importFrom readr read_csv cols
#' @importFrom dplyr select
#' @return tibble of observed dam data (storage, inflow, release)
#' @export
#'
read_reservoir_data <- function(USRDATS_path, dam_id){

  read_csv(paste0(USRDATS_path, "/time_series_all/ResOpsUS_",
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
#' @importFrom readr read_csv cols
#' @importFrom dplyr select as_tibble
#' @importFrom sf st_read
#' @return tibble of reservoir attributes for selected dams
#' @export
#'
read_reservoir_attributes <- function(GRanD_path, dam_id = NULL){

  st_read(paste0(GRanD_path, "GRanD_dams_v1_3.shp")) %>%
    as_tibble() %>%
    filter(COUNTRY %in% "United States") -> attributes_all

  if(is.null(dam_id)){
    return(attributes_all)
  }

  attributes_all %>%
    subset(GRAND_ID == dam_id) ->
    attributes_dam

  stopifnot(nrow(attributes_dam) == 1)

  return(attributes_dam)

}


#' read_GRanD_HUC8
#'
#' @description gets HUC8 for all US GRanD IDs
#' @importFrom readr read_csv cols
#' @return tibble of HUC8s
#' @export
#'
read_GRanD_HUC8 <- function(){

  read_csv(
    paste0(system.file("extdata/", package = "starfit"),
           "GRAND_HUC8.csv"),
    comment = "#", col_types = cols())

}
