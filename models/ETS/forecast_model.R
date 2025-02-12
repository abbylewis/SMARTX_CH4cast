# ets model
# written by ASL

#### Step 0: load packages

library(tidyverse)
library(forecast)

#### Step 1: Set model specifications
model_id <- "ets"
all_forecast_vars <- read_csv(here::here("forecast_variables.csv"), show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = F #Whether the model requires NOAA data

#### Define the forecast model for a site
forecast_model <- function(site,
                           var,
                           noaa_past_mean = NULL,
                           noaa_future_monthly = NULL,
                           target,
                           horiz,
                           step,
                           forecast_date) {
  
  message(paste0("Running site: ", site))
  
  # Format site data for ETS model
  site_target_raw <- target |>
    dplyr::mutate(datetime = as.Date(datetime)) |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable == var, 
                  site_id == site,
                  datetime < forecast_date) 
  
  # Format
  site_target_raw <- site_target_raw |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation")
  
  if(!var %in% names(site_target_raw) || sum(!is.na(site_target_raw[var])) < 5){
    message(paste0("Insufficient target observations at site ", site, 
                   ". Skipping forecasts at this site."))
    return()
  }
  
  site_target = site_target_raw |>
    complete(datetime = seq.Date(from = min(datetime), 
                                 to = max(datetime),
                                 by = "month"), 
             site_id) |>
    mutate(month = month(datetime)) %>%
    group_by(site_id, month) |>
    mutate(CH4_slope_umol_m2_d = ifelse(is.na(CH4_slope_umol_m2_d),
                                        mean(CH4_slope_umol_m2_d, na.rm = T),
                                        CH4_slope_umol_m2_d)) %>%
    ungroup() %>%
    mutate(CH4_slope_umol_m2_d = na.interp(CH4_slope_umol_m2_d)) %>%
    #filter(!(datetime > "2022-10-13" & datetime <= "2023-06-27")) %>%
    arrange(datetime)
  
  dif_years = year(forecast_date) - year(max(site_target$datetime))
  h = month(forecast_date) - month(max(site_target$datetime)) +
    horiz +
    12*dif_years
    
  ts_data = as.ts(site_target[var])
  
  # Fit ets with interpolated data
  fit = ets(ts(ts_data, frequency = 12))

  # use the model to forecast target variable
  forecast_raw <- as.data.frame(forecast(fit,h=h,level=0.68))%>% #One SD
    mutate(sigma = `Hi 68`-`Point Forecast`)
  
  forecast = data.frame(project_id = "smartx",
                        model_id = model_id,
                        datetime = max(site_target$datetime) + months(1:h),
                        reference_datetime = forecast_date,
                        duration = "P1M",
                        site_id = site,
                        family = "normal",
                        variable = var,
                        mu = as.numeric(forecast_raw$`Point Forecast`),
                        sigma = as.numeric(forecast_raw$sigma)
  ) %>%
    pivot_longer(cols = c(mu,sigma), names_to = "parameter",values_to = "prediction")%>%
    select(project_id, model_id, datetime, reference_datetime, duration,
           site_id, family, parameter, variable, prediction)
  
  return(forecast)
}
