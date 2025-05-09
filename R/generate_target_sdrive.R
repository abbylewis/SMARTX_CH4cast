setwd("S:/Biogeochemistry/GCREW/4-SMARTX/1-Data/Routine Data/CH4 and NEE data")

library(tidyverse)

#' generate_target
#'
#' @description
#' This is the master file to load data from dropbox, calculate fluxes, and QAQC the data
#'
#' @param reprocess whether to reprocess files we have already processed
#'
#' @return target data based on all dropbox CH4 flux data
#' @export
#'
#' @examples
generate_target_sdrive <- function(path){
  files <- list.files(pattern = "final_CH4_fluxes", recursive = T)
  data <- files %>%
    map(read_csv, col_types = cols(.default = col_character())) %>%
    bind_rows() %>%
    mutate(Date = parse_date_time(Date, 
                                  orders = c("m d Y", "Y m d")),
           flux.CH4 = as.numeric(flux.CH4),
           flux.CH4 = ifelse(Date == as.Date("2024-08-06") & Plot %in% c("333", "313"),
                             NA, #Severely non-linear
                             flux.CH4),
           flux.CH4 = ifelse(Date == as.Date("2018-12-19") & Plot %in% c("321"),
                             NA, #Severely non-linear
                             flux.CH4),
           flux.CH4 = ifelse(Date == as.Date("2019-06-12"),
                             NA, #Something weird happened where concentrations generally got quite high. Removing to be safe
                             flux.CH4),
           flux.CH4 = ifelse(Date %in% as_datetime(c("2018-09-19", "2018-08-27")) & Plot == "313",
                             NA, #Readme.txt says to remove
                             flux.CH4)
           ) %>%
    filter(is.na(Light) | Light %in% c("Dark")) %>%
    select(Date, Plot, flux.CH4) # These are the only columns that are in all files rn
  
  #Format as target data
  target <- data %>%
    mutate(project_id = "smartx",
           duration = "P1M",
           CH4_slope_umol_m2_d = as.numeric(flux.CH4)) %>% #Need to figure out units
    rename(site_id = Plot,
           datetime = Date) %>%
    select(project_id, site_id, datetime, duration, CH4_slope_umol_m2_d) %>%
    pivot_longer(cols = CH4_slope_umol_m2_d, 
                 names_to = "variable", values_to = "observation") %>%
    mutate(datetime = as.Date(datetime)) %>%
    filter(!is.na(observation),
           !is.na(datetime)) %>%
    group_by(project_id, site_id, datetime, duration, variable) %>%
    summarise(observation = median(observation, na.rm = TRUE), 
              n = n(),
              .groups = "drop") %>%
    mutate(year = year(datetime)) %>%
    filter(year >= 2017,
           year <= 2024) %>%
    select(-year)
  
  write.csv(target, paste0(path,"/L1_target.csv"), row.names = FALSE)
  return(target)
}

target <- generate_target_sdrive(path = "C:/Users/lewisa4/OneDrive - Smithsonian Institution/Desktop/SMARTX_CH4cast")

target %>%
  ggplot(aes(x = datetime, y = observation, color = site_id)) +
  geom_point()
