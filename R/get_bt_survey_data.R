#' @title Pull NEFSC Bottom Trawl Survey data
#' @description
#' Pulls, filters and renames NEFSC Bottom Trawl survey.
#'
#' @param channel connection to remote databases.
#' @param yr_range A vector with length of 2 indicating the start and end, inclusive, year of the desired time series
#'
#' @return a data frame.

get_bt_survey_data <- function(channel, yr_range) {
  ## Pull bt survey data
  data <- survdat::get_survdat_data(
    channel,
    getWeightLength = F,
    getLengths = F,
    getBio = F,
    conversion.factor = T
  )

  ## Pull species codes and names
  survSPP <- survdat::get_species(channel)$data |>
    dplyr::select(SCINAME, COMNAME, SVSPP) |>
    dplyr::arrange(SVSPP)

  # join data with lookup table, filter, mutate and rename
  dat <- data$survdat |>
    dplyr::left_join(survSPP, by = "SVSPP") |>
    dplyr::mutate(
      month = lubridate::month(EST_TOWDATE), #create month column since surv data doesn't come with one
      towID = paste(CRUISE6, STRATUM, TOW, STATION)
    ) |>
    dplyr::rename(
      lat = LAT,
      lon = LON,
      count = ABUNDANCE,
      name = SCINAME,
      year = YEAR
    ) |>
    dplyr::select(towID, year, month, lon, lat, count, name) |>
    dplyr::filter(year >= yr_range[1] & year <= yr_range[2])

  return(dat)
}
