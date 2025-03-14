plot_raw_data <- function(filename, data_raw){
  chamber_levels = c("c_1_amb", "c_2_amb", "c_3_e0.75", "c_4_e1.5", "c_5_e2.25",
                     "c_6_e2.25", "c_7_e3.0", "c_8_e3.75", "c_9_e3.75", "c_10_e4.5",
                     "c_11_e5.25", "c_12_e6.0")
  
  #Account for different formatting among files
  if("LGR_Time" %in% colnames(data_raw)){
    data_small <- data_raw %>%
      filter(is.na(LGR_Time) |
               !duplicated(LGR_Time) | 
               !duplicated(CH4d_ppm)) %>% #I've spent some time looking into this and there are some duplicated LGR rows
      select(TIMESTAMP, CH4d_ppm, CO2d_ppm, MIU_VALVE, GasT_C)
  } else {
    data_small <- data_raw %>%
      mutate(CO2_ppm = NA) %>%
      select(TIMESTAMP, CH4d_ppm, CO2d_ppm, MIU_VALVE, GasT_C)
  }
  rm(data_raw) #Save memory
  
  ### Calculate slopes ###
  #Prep data
  index_cutoff <- 30 #Cutoff to use when we have plenty of data
  summer_index_cutoff <- 15 #Cutoff to use when there are < 35 measurements
  filtered_data_all <- data_small %>%
    mutate(CH4d_ppm = as.numeric(CH4d_ppm),
           CO2d_ppm = as.numeric(CO2d_ppm),
           GasT_C = as.numeric(GasT_C),
           MIU_VALVE = as.numeric(MIU_VALVE)) %>%
    filter(!MIU_VALVE == 16,
           !is.na(MIU_VALVE)) %>%
    arrange(TIMESTAMP) %>%
    mutate(group = group_fun(MIU_VALVE)) %>%
    group_by(group, MIU_VALVE) %>%
    mutate(start = min(TIMESTAMP),
           change = as.numeric(difftime(TIMESTAMP, start, units = "days")),
           index = row_number(),
           cutoff = ifelse(max(index) < 35, 
                           summer_index_cutoff,
                           index_cutoff),
           n = sum(index > cutoff & !is.na(CH4d_ppm))) %>%
    #Issues during this window (concs > 100000 ppm)
    filter(TIMESTAMP > "2024-07-09 15:00:00" | TIMESTAMP < "2024-07-09 6:00:00") %>%
    mutate(Date = as.Date(TIMESTAMP))
  
  #Look at data to confirm index is okay
  p1 <- filtered_data_all %>%
    mutate(Date = as.factor(Date),
           MIU_VALVE = factor(MIU_VALVE,
                              levels = 1:12,
                              labels = chamber_levels)) %>%
    filter(!is.na(MIU_VALVE)) %>%
    ggplot(aes(x = index, y = CH4d_ppm, group = start, color = Date)) +
    geom_line()+
    scale_color_viridis_d()+
    facet_wrap(~MIU_VALVE)+
    ggtitle(paste0("Raw data from ", filename))
  
  return(p1)
}

#Create function to assign groups for separate readings
group_fun <- function(MIU_VALVE) {
  group <- rep(1, length(MIU_VALVE))
  for (i in 2:length(MIU_VALVE)) {
    if(MIU_VALVE[i] == MIU_VALVE[i - 1]) {
      group[i] <- group[i - 1]
    } else {
      group[i] <- group[i - 1] + 1
    }
  }
  return(group)
}