install.packages("here")
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
generate_target <- function(){
  files <- list.files(here::here("Raw_data", "SMARTX_fluxes"), full.names = TRUE)
  
  data <- files %>%
    map(read_csv, show_col_types = F) %>%
    bind_rows() %>%
    #filter(is.na(Light) | Light %in% c("Light", "Full")) %>%
    select(Date, Plot, flux.CH4, R2.CH4)
  
  #Format as target data
  target <- data %>%
    mutate(project_id = "smartx",
           duration = "P1M",
           Date = as.Date(Date, format = "%m/%d/%Y"),
           CH4_slope_umol_m2_d = flux.CH4) %>% #Need to figure out units
    rename(site_id = Plot,
           datetime = Date) %>%
    select(project_id, site_id, datetime, duration, CH4_slope_umol_m2_d) %>%
    pivot_longer(cols = CH4_slope_umol_m2_d, 
                 names_to = "variable", values_to = "observation") %>%
    mutate(datetime = as.Date(datetime)) %>%
    filter(!is.na(observation)) %>%
    group_by(project_id, site_id, datetime, duration, variable) %>%
    summarise(observation = median(observation, na.rm = TRUE), 
              n = n(),
              .groups = "drop")
  
  write.csv(target, here::here("L1_target_not_sdrive.csv"), row.names = FALSE)
  return(target)
}

target <- generate_target()
