
#Load new target data (optional)
source(here::here("R","generate_target.R"))

#Change file path here to try a different model
source(here::here("models","brnn","forecast_model.R"))

#Run remaining code and visualize forecasts
source(here::here("R","generate_tg_forecast.R"))
generate_tg_forecast(forecast_date = "2024-05-01",
                     forecast_model = forecast_model,
                     model_variables = model_variables,
                     model_id = model_id,
                     model_timestep = model_timestep,
                     all_sites = all_sites,
                     sites = sites,
                     noaa = noaa, 
                     plot = T,
                     save = F,
                     comb_reps = F)

