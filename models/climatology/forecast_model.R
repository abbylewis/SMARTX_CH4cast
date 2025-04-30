# climate window
# written by ASL


#### Step 0: load packages

library(tidyverse)
library(forecast)

#### Step 1: Set model specifications
model_id <- "climatology"
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
  
  # Format site data for climatology model
  site_target_raw <- target |>
    dplyr::mutate(datetime = as.Date(datetime)) |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable == var, 
                  site_id == site) 
  
  # Format
  site_target_raw <- site_target_raw |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation")
  
  if(!var %in% names(site_target_raw) || sum(!is.na(site_target_raw[var])) < 5){
    message(paste0("Insufficient target observations at site ", site, 
                   ". Skipping forecasts at this site."))
    return()
  }

  # Create a complete time series of the target variable
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
  
  # what months do we want a forecast of?
  forecast_dates <- max(site_target$datetime) + months(1:h)
  forecast_month <- month(forecast_dates)
  
  # put in a table
  forecast_dates_df <- tibble(datetime = forecast_dates,
                              month = forecast_month)
  forecast <- target_clim %>%
    mutate(month = as.integer(month)) %>%
    filter(month %in% forecast_month) %>%
    full_join(forecast_dates_df, by = c('month')) %>%
    arrange(site_id, datetime)
  
  #Ensure we have sufficient data
  if(sum(!is.na(forecast$clim_mean)) == 0 | sum(!is.na(forecast$clim_sd)) == 0){
    message(paste0("Insufficient historical observations at site ", site, 
                   ". Skipping forecasts at this site."))
    return()
  }
  
  # Interpolate
  combined <- forecast %>%
    select(datetime, site_id, clim_mean, clim_sd) %>%
    rename(mean = clim_mean,
           sd = clim_sd) %>%
    mutate(mu = imputeTS::na_interpolation(x = mean),
           sigma = median(sd, na.rm = TRUE))
  
  # Format in EFI standard
  forecast = data.frame(project_id = "smartx",
                        model_id = model_id,
                        datetime = max(site_target$datetime) + months(1:h),
                        reference_datetime = forecast_date,
                        duration = "P1M",
                        site_id = site,
                        family = "normal",
                        variable = var,
                        mu = as.numeric(combined$mu),
                        sigma = as.numeric(combined$sigma)
                        )%>%
    pivot_longer(cols = c(mu,sigma), names_to = "parameter",values_to = "prediction")%>%
    select(project_id, model_id, datetime, reference_datetime, duration,
           site_id, family, parameter, variable, prediction)
  
  return(forecast)
}

