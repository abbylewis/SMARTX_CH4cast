# persistence model
# written by ASL

# This is like random walk, but SE is estimated separately for each horizon

#### Step 0: load packages

library(tidyverse)
library(forecast)

#### Step 1: Set model specifications
model_id <- "persistence"
all_forecast_vars <- read_csv(here::here("forecast_variables.csv"), show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = F #Whether the model requires NOAA data
boot_number = 100 #Number of bootstraps to run
bootstrap = F

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
  
  if(bootstrap == T){
    message("Bootstrapping is not supported for persistence model. Setting bootstrap to FALSE.")
    bootstrap = F
  }
  
  # Format site data
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
    message(paste0("No target observations at site ", site, 
                   ". Skipping forecasts at this site."))
    return()
  }

  site_target = site_target_raw |>
    complete(datetime = seq.Date(from = min(datetime), 
                                 to = max(datetime),
                                 by = "month"), 
             site_id) %>%
    mutate(index = row_number() - 1) 
  
  dif_years = year(forecast_date) - year(max(site_target$datetime))
  h = month(forecast_date) - month(max(site_target$datetime)) +
    horiz +
    12*dif_years

  # Fit RW model for each horizon individually
  fit_at_hi <- function(hi){
    RW_model <- site_target %>%
      mutate(var_unnamed = get(!!var)) %>%
      tsibble::as_tsibble(index = index, key = "site_id") %>%
      fabletools::model(RW = fable::RW(var_unnamed~ lag(hi)))
    
    forecast <- RW_model %>% 
      fabletools::forecast(h = hi) %>%
      filter(index == max(site_target$index) + hi)
    
    # extract parameters
    parameters <- distributional::parameters(forecast$var_unnamed)
    
    return(bind_cols(forecast, parameters))
  }
  
  fits <- purrr::pmap(list(1:h), fit_at_hi) |> 
    bind_rows() %>%
    mutate(datetime = min(site_target$datetime) + months(index))
  
  # make right format
  forecast <- fits |>
    pivot_longer(mu:sigma,
                 names_to = 'parameter',
                 values_to = 'prediction') |>
    mutate(model_id = model_id,
           family = 'normal',
           reference_datetime=forecast_date,
           variable = var,
           project_id = "smartx",
           duration = "P1M") |>
    data.frame() %>%
    dplyr::select(all_of(c("project_id", "model_id", "datetime", "reference_datetime",
                    "duration", "site_id", "family", "parameter", 
                    "variable", "prediction"))) |>
    filter(datetime > reference_datetime) |>
    as_tibble()
  
  return(forecast)
}

