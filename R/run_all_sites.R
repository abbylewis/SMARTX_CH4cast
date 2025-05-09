#Quick function to repeat for all sites
run_all_sites = function(var,
                        sites,
                        forecast_model,
                        noaa_past_mean,
                        noaa_future_monthly,
                        target,
                        horiz,
                        step,
                        forecast_date) {
  
  message(paste0("Running variable: ", var))
  forecast <- purrr::pmap(.l = list(sites),
                          .f = forecast_model,
                          var = var,
                          noaa_past_mean = noaa_past_mean,
                          noaa_future_monthly = noaa_future_monthly,
                          target = target,
                          horiz = horiz,
                          step = step,
                          forecast_date = forecast_date) %>%
    bind_rows()
  
}
