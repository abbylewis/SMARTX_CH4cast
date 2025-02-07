source(here::here("R","run_all_sites.R"))
#source(here::here("R","load_hist_weather.R"))
#source(here::here("R","load_and_save_gefs.R"))
library(tidyverse)

generate_tg_forecast <- function(forecast_date,
                                 forecast_model,
                                 model_variables,
                                 model_id,
                                 model_timestep,
                                 all_sites = all_sites, #Whether the model is /trained/ across all sites
                                 sites = sites, #Sites to forecast
                                 noaa = T,
                                 save = T,
                                 plot = F,
                                 use_ref_year = T) {
  
  ### Step 1: Download latest target data
  target <- read_csv(here::here("L1_target.csv"), show_col_types = F) %>%
    dplyr::mutate(datetime = as.Date(datetime),
                  datetime = as.Date(paste(year(datetime), month(datetime), "01", sep = "-"))) %>%
    group_by(site_id, datetime, project_id, duration, variable) %>%
    summarize(observation = mean(observation, na.rm = T), .groups = "drop")
  
  ### Step 2: Set forecast specifications
  if(sites == "all"){
    sites <- unique(target$site_id)
  }
  horiz = 6
  step = 1
  
  ### Step 3: Get NOAA driver data
  forecast_date <- as.Date(forecast_date)
  
  #Load forecasts
  noaa_future_monthly <- read_csv(here::here("met_downloads",
                                             "monthly_forecasts.csv"),
                                  show_col_types = F) %>%
    pivot_wider(names_from = "variable", values_from = "prediction") %>%
    filter(reference_datetime <= forecast_date) %>%
    filter(reference_datetime == max(reference_datetime))
  
  
  # Load historical data
  noaa_past_mean <- read_csv(here::here("met_downloads",
                                        "monthly_forecasts.csv"),
                             show_col_types = F) %>%
    filter(horizon == 1) %>%
    group_by(datetime, variable) %>%
    summarize(prediction = mean(prediction, na.rm = T), .groups = "drop") %>%
    pivot_wider(names_from = "variable", values_from = "prediction")
  
  ### Step 4: forecast!
  # Run all variables
  if(all_sites == F) {
    # Run all sites and depths individually for each variable
    forecast <- purrr::pmap(.l = list(model_variables),
                            .f = run_all_sites,
                            sites = sites,
                            forecast_model = forecast_model,
                            noaa_past_mean = noaa_past_mean,
                            noaa_future_monthly = noaa_future_monthly,
                            target = target,
                            horiz = horiz,
                            step = step,
                            forecast_date = forecast_date) %>%
      bind_rows()
  } else {
    # Fit model across all sites together for each variable
    forecast <- purrr::pmap(.l = list(model_variables),
                            .f = forecast_model,
                            sites = sites,
                            noaa_past_mean = noaa_past_mean,
                            noaa_future_monthly = noaa_future_monthly,
                            target = target,
                            horiz = horiz,
                            step = step,
                            forecast_date = forecast_date) %>%
      bind_rows()
  }
  
  if(nrow(forecast) == 0){
    stop("No forecast generated")
  }
  
  forecast <- forecast %>%
    mutate(model_id = model_id) %>%
    filter(datetime >= forecast_date)
  
  ### Step 5: Format and submit
  
  # Write forecast to disk
  if(save){
    forecast_file <- paste0(here::here("outputs", paste0("daily-", forecast_date, "-", model_id, ".csv.gz")))
    write_csv(forecast, forecast_file)
  }
  
  if(plot) {
    if(unique(forecast$family) == "ensemble"){
      p1 <- forecast %>%
        #mutate(site_id = factor(site_id, levels = chamber_levels)) %>%
        ggplot(aes(x = datetime, y = prediction)) +
        geom_hline(yintercept = 0, color = "grey50") +
        geom_vline(xintercept = forecast_date) +
        geom_line(aes(group = parameter), alpha = 0.3) +
        geom_point(data = target %>%
                     #mutate(site_id = factor(site_id, levels = chamber_levels)) %>%
                     filter(datetime >= forecast_date - 35 * step,
                            datetime <= forecast_date + horiz * step), 
                   aes(x = datetime, y = observation, alpha = datetime >= forecast_date)) +
        scale_alpha_manual(values = c(1, .5)) +
        theme(legend.position = "none")
      if(length(model_variables) == 1) {
        p1 <- p1 +
          facet_wrap(~site_id)
      } else {
        p1 <- p1 +
          facet_grid(rows = vars(variable), cols = vars(site_id))
      }
    } else {
      p1 <- forecast %>%
        #mutate(site_id = factor(site_id, levels = chamber_levels)) %>%
        pivot_wider(names_from = "parameter", values_from = "prediction") %>%
        ggplot(aes(x = datetime)) +
        geom_hline(yintercept = 0, color = "grey50") +
        geom_vline(xintercept = forecast_date) +
        geom_line(aes(y = mu)) +
        geom_ribbon(aes(ymin = mu - sigma, ymax = mu + sigma), alpha = 0.3) +
        geom_point(data = target %>%
                     #mutate(site_id = factor(site_id, levels = chamber_levels)) %>%
                     filter(datetime >= forecast_date - horiz * 30 * step,
                            datetime <= forecast_date + horiz * 30 * step), 
                   aes(x = datetime, y = observation, alpha = datetime >= forecast_date)) +
        scale_alpha_manual(values = c(1, .5)) +
        theme(legend.position = "none") +
        ylab("Flux (µmol/m2/day)")
      if(length(model_variables) == 1) {
        p1 <- p1 +
          facet_wrap(~site_id)
      } else {
        p1 <- p1 +
          facet_grid(rows = vars(variable), cols = vars(site_id))
      }
    }
    print(p1)
  }
}
