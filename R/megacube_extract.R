# Main workhorse function
# combines gdalcubes with helper utilities and reshape utilities

megacube_extract <- function(dates = Sys.Date() - 1L,
                             ensemble = gefs_ensemble(),
                             bands = gefs_bands(date = date),
                             sites,
                             horizon = gefs_horizon(),
                             all_bands = gefs_all_bands(date = date),
                             url_builder = gefs_urls,
                             cycles =  c("00")) { #, "06", "12", "18")) {
  # v11 goes from 2017-01-01 to 2020-09-22.  BUT
  # on 2018-07-27, file name and url structure shift slightly
  
  #only cycle 00 has 35-day forecasts
  vars <- names(bands)
  sites_df <- sites |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::any_of(c("FID", "site_id")))

  gribs <-
    tidyr::expand_grid(ensemble = ensemble,
                       cycle = cycles,
                       reference_datetime = dates,
                       horizon = horizon) |>
    dplyr::mutate(url = url_builder(ensemble, reference_datetime,
                                    horizon, cycle=cycle),
                  time = format(as_datetime(reference_datetime + hours(horizon)),
                                "%Y-%m-%d %H:%M:%S") #Hacky way to re-join this later
                  #Don't worry, these dates are not actually used
                  ) 
  #Need all horizons to compute daily mean/sum because of rain
  
  message(nrow(gribs), " urls to process")

  ## don't make cube too big to fit in memory or try more urls than `ulimit -n`
  max_open_files <- as.integer(system("ulimit -n", intern=TRUE))
  stopifnot(nrow(gribs) < max_open_files)

  time <- cycle <- reference_datetime <- NULL # DUMMY
  
  # gdalcubes processing
  df <-
    gdalcubes::stack_cube(gribs$url,
                          datetime_values = gribs$time,
                          band_names = all_bands) |>
    gdalcubes::select_bands(bands) |>
    gdalcubes::extract_geom(sites) |>
    tibble::as_tibble()
  
  # reshape
  out <- df |>
    dplyr::rename({bands}) |>
    dplyr::mutate(time = sub("T", " ", as.character(time))) |>
    # unpack overloaded time dimension:
    dplyr::left_join(gribs %>%
                       mutate(time = as.character(time)), 
                     by="time") |>
    dplyr::select(-"url", -"time") |>
    dplyr::mutate(datetime = lubridate::as_datetime(reference_datetime) +
                    lubridate::hours(cycle) +
                    lubridate::hours(horizon)) |>
    # feature-id to site_id:
    dplyr::inner_join(sites_df, by = "FID") |>
    dplyr::select(-FID) |>
    # "long" (EFI) format
    tidyr::pivot_longer(vars,
                        names_to = "variable",
                        values_to = "prediction") |>
    dplyr::ungroup()
  
  return(out)
}
