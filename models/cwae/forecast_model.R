# climate window arima errors
# written by ASL


#### Step 0: load packages

library(tidyverse)
library(forecast)

#### Step 1: Set model specifications
model_id <- "cwae"
all_forecast_vars <- read_csv(here::here("forecast_variables.csv"), show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = F #Whether the model requires NOAA data

#### Step 2: Define the forecast model
forecast_model <- function(site,
                           var,
                           noaa_past_mean = NULL,
                           noaa_future_monthly = NULL,
                           target,
                           horiz,
                           step,
                           forecast_date) {
  
  message(paste0("Running site: ", site))
  
  # Format site data for arima model
  site_target_raw <- target |>
    dplyr::mutate(datetime = as.Date(datetime)) |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable == var, 
                  site_id == site,
                  datetime < forecast_date) 
  
  # Format
  site_target_raw <- site_target_raw |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation")
  
  if(!var %in% names(site_target_raw) || sum(!is.na(site_target_raw[var])) == 0){
    message(paste0("No target observations at site ", site, 
                   ". Skipping forecasts at this site."))
    return()
  }

  site_target = site_target_raw |>
    complete(datetime = seq.Date(from = min(datetime), 
                                 to = max(datetime),
                                 by = "month"), 
             site_id)
  
  dif_years = year(forecast_date) - year(max(site_target$datetime))
  h = month(forecast_date) - month(max(site_target$datetime)) +
    horiz +
    12*dif_years
  
  months <- data.frame(month = 1:12, site_id = site)
  
  # calculate the mean and standard deviation for each doy
  target_clim <- site_target %>%
    mutate(month = month(datetime)) %>%
    group_by(month, site_id) %>%
    summarize(clim_mean = mean(get(!!var), na.rm = T),
              clim_sd = sd(get(!!var), na.rm = T),
              .groups = "drop") %>%
    #Calculate rolling mean
    full_join(months, by = c("month", "site_id")) %>%
    distinct()
  
  # what dates do we want a forecast of?
  forecast_dates <- max(site_target$datetime) + months(1:h)
  forecast_month <- month(forecast_dates)
  forecast_dates_df <- tibble(datetime = forecast_dates,
                              month = forecast_month)
  
  # Create forecast
  forecast <- target_clim %>%
    mutate(month = as.integer(month)) %>%
    filter(month %in% forecast_month) %>%
    full_join(forecast_dates_df, by = c('month')) %>%
    arrange(site_id, datetime)
  
  #Check for sufficient data
  if(sum(!is.na(forecast$clim_mean)) == 0 | sum(!is.na(forecast$clim_sd)) == 0){
    message(paste0("Insufficient historical observations at site ", site, 
                   ". Skipping forecasts at this site."))
    return()
  }
  
  # Interpolate if a month is missing
  combined <- target_clim %>%
    select(month, site_id, clim_mean, clim_sd) %>%
    group_by(month, site_id) %>%
    summarize(mu = imputeTS::na_interpolation(x = clim_mean),
              sigma = median(clim_sd, na.rm = TRUE),
              .groups = "drop")
  
  #Now, use arima model on the residuals
  resids <- combined %>%
    right_join(site_target %>%
                 mutate(month = month(datetime)), 
               by = c("month", "site_id")) %>%
    mutate(resid = CH4_slope_umol_m2_d - mu) %>%
    arrange(datetime)
  
  fit = auto.arima(resids$resid)
  forecast_raw <- as.data.frame(forecast(fit, h = h, level=0.68)) %>% #One SD
    mutate(sigma = `Hi 68`-`Point Forecast`)  
  
  #Compile into df
  arima_errors <- data.frame(mu_arima = as.numeric(forecast_raw$`Point Forecast`),
                             sigma_arima = as.numeric(forecast_raw$sigma),
                             datetime = forecast_dates)
  
  #Combine
  final <- forecast %>%
    left_join(arima_errors, by = c("datetime")) %>%
    filter(datetime >= forecast_date) %>%
    mutate(mu_final = mu_arima + clim_mean,
           sigma_final = sqrt(sigma_arima^2 + clim_sd^2))
  
  forecast = data.frame(project_id = "smartx",
                        model_id = model_id,
                        datetime = final$datetime,
                        reference_datetime = forecast_date,
                        duration = "P1M",
                        site_id = site,
                        family = "normal",
                        variable = var,
                        mu = as.numeric(final$mu_final),
                        sigma = as.numeric(final$sigma_final)
                        )%>%
    pivot_longer(cols = c(mu,sigma), names_to = "parameter", values_to = "prediction")%>%
    select(project_id, model_id, datetime, reference_datetime, duration,
           site_id, family, parameter, variable, prediction)
  
  return(forecast)
}

