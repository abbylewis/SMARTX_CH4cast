forecast_date <- as.Date(Sys.Date())

### Step 1: Download latest target data
target <- read_csv(here::here("L1_target.csv"), show_col_types = F) %>%
  dplyr::mutate(datetime = as.Date(datetime),
                datetime = as.Date(paste(year(datetime), month(datetime), "01", sep = "-"))) %>%
  group_by(site_id, datetime, project_id, duration, variable) %>%
  summarize(observation = mean(observation, na.rm = T), .groups = "drop") %>%
  filter(datetime <= forecast_date)


### Step 2: Set forecast specifications
site = 310
horiz = 6
step = 1
var = model_variables[1]

### Step 3: Get NOAA driver data

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
