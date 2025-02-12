# rEDM model
# written by ASL


#### Step 0: load packages
library(tidyverse)
library(rEDM)

#### Step 1: Set model specifications
model_id <- "rEDM"
all_forecast_vars <- read_csv(here::here("forecast_variables.csv"), show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = F #Whether the model requires NOAA data
message(paste0("Running EDM. Sites are ", sites))

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
  
  # Format site data for rEDM model
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
  
  # Fit rEDM model
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
    arrange(datetime)
  
  dif_years = year(forecast_date) - year(max(site_target$datetime))
  h = month(forecast_date) - month(max(site_target$datetime)) +
    horiz +
    12*dif_years
  n <- nrow(site_target)
  lib <- c(1, round(n*.8)) # use 80% of data for training
  pred <- c(round(n*.8) + 1, n)
  
  dims <- rEDM::EmbedDimension(dataFrame = data.frame(site_target %>% select(-site_id)),
                         columns = "CH4_slope_umol_m2_d", target = "CH4_slope_umol_m2_d",
                         lib = lib, pred = pred,  # which portions of the data to train and predict
                         maxE = 12, Tp = 6)
  opt_dim <- which.max(dims$rho)
  
  fits <- rEDM::SMap(dataFrame = data.frame(site_target %>% select(-site_id)),
                       columns = "CH4_slope_umol_m2_d", target = "CH4_slope_umol_m2_d", 
                       lib = c(1,n), pred = c(1,n), # which portions of the data to train and predict
                       E = opt_dim)$predictions
  
  fits %>%
    ggplot(aes(x = datetime)) +
    geom_point(aes(y = Observations), color = "blue") +
    geom_line(aes(y = Predictions), color = "red")
  
  #Use all data for final forecasts
  output <- rEDM::SMap(dataFrame = data.frame(site_target %>% select(-site_id)),
                          columns = "CH4_slope_umol_m2_d", target = "CH4_slope_umol_m2_d", 
                          lib = c(1,n), pred = c(1,n), # which portions of the data to train and predict
                          E = opt_dim, generateSteps = h)$predictions
  
  #generateSteps will override prediction interval
  #See more here: https://sugiharalab.github.io/EDM_Documentation/simplex_/

  forecast = data.frame(project_id = "smartx",
                        model_id = model_id,
                        datetime = as.Date(paste(year(output$datetime), month(output$datetime), "01", sep = "-")),
                        reference_datetime = forecast_date,
                        duration = "P1M",
                        site_id = site,
                        family = "normal",
                        variable = var,
                        mu = output$Predictions,
                        sigma = sqrt(output$Pred_Variance)
                        )%>%
    pivot_longer(cols = c(mu,sigma), names_to = "parameter",values_to = "prediction")%>%
    select(project_id, model_id, datetime, reference_datetime, duration,
           site_id, family, parameter, variable, prediction)
  
  return(forecast)
}

