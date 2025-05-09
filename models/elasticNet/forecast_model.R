# elasticNet model
# written by ASL


#### Step 0: load packages
library(tidyverse)
library(caret)

#### Step 1: Set model specifications
model_id <- "elasticNet"
all_forecast_vars <- read_csv(here::here("forecast_variables.csv"), show_col_types = FALSE)
model_variables <- all_forecast_vars$`"official" targets name`
# Global parameters used in generate_tg_forecast()
all_sites = F #Whether the model is /trained/ across all sites
sites = "all" #Sites to forecast
noaa = T #Whether the model requires NOAA data

#### Define the forecast model for a site
forecast_model <- function(site,
                           var,
                           noaa_past_mean,
                           noaa_future_monthly,
                           target,
                           horiz,
                           step,
                           forecast_date) {
  
  message(paste0("Running site: ", site))

  # Filter to desired variable, site, date
  site_target_raw <- target |>
    dplyr::mutate(datetime = as.Date(datetime)) |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable == var, 
                  site_id == site,
                  datetime < forecast_date) 
  
  # Merge in past NOAA data into the targets file, matching by date.
  site_target <- site_target_raw |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean, 
                     by = c("datetime")) %>%
    na.omit()
  
  if(!var %in% names(site_target) || sum(!is.na(site_target[var])) == 0){
    message(paste0("No target observations at site ",site,
                   ". Skipping forecasts at this site."))
    return()
    
  } else if(sum(!is.na(site_target$Temp_K) & 
                !is.na(site_target$WindSpeed_ms) &
                !is.na(site_target$PrecipRate_ms) &
                !is.na(site_target$Pressure_Pa) &
                !is.na(site_target$SolarRad_Wm2) &
                !is.na(site_target[var]))<10){
    message(paste0("Insufficient met data that corresponds with target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else {
    # Set training control
    train_control <- trainControl(method = "repeatedcv",
                                  number = 5,
                                  repeats = 5,
                                  search = "random",
                                  verboseIter = TRUE)
    
    train_data <- site_target |>
      select(CH4_slope_umol_m2_d, Temp_K, SolarRad_Wm2, PrecipRate_ms, WindSpeed_ms, Pressure_Pa) 
    
    # Train the model
    fit <- train(CH4_slope_umol_m2_d ~ .,
                 data = train_data,
                 method = "glmnet",
                 preProcess = c("center", "scale"),
                 #tuneLength = 25,
                 trControl = train_control)
    
    fit$pred <- predict(fit, train_data)
    
    #  Get 30-day predicted temp ensemble at the site
    new_data <- noaa_future_monthly |>
      select(Temp_K, SolarRad_Wm2, PrecipRate_ms, WindSpeed_ms, Pressure_Pa)
    
    preds <- predict(fit, new_data) #THIS IS THE FORECAST STEP
    
    # use model to forecast target variable for each ensemble member
    forecast <- noaa_future_monthly |> 
      mutate(site_id = site,
             prediction = preds, 
             variable = var) %>%
      group_by(datetime, reference_datetime, site_id, variable) %>%
      summarise(mu = mean(prediction, na.rm = T),
                sigma = sqrt(sd(prediction, na.rm = T)^2 + 
                               sd(fit$pred - site_target[[var]], na.rm = T)^2),
                .groups = "drop") %>%
      pivot_longer(cols = c(mu, sigma), names_to = "parameter", values_to = "prediction")
    
    # Format results to EFI standard
    forecast <- forecast |>
      mutate(project_id = "smartx",
             model_id = model_id,
             reference_datetime = forecast_date,
             duration = "P1M",
             family = "normal") |>
      select(project_id, model_id, datetime, reference_datetime, duration,
             site_id, family, parameter, variable, prediction)
  }
}
