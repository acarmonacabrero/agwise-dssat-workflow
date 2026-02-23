response_plots_and_statistics_for_fert_factorial <- function(country, useCaseName, Crop, season) {
  dir_path <- paste0("/home/jovyan/agwise-fertilizer/Data/useCase_", country, "_",
                     useCaseName, "/", Crop, "/result/DSSAT/AOI/")
  
  dssat_data <- readRDS(paste0(dir_path, "useCase_", country, "_", useCaseName,
                               "_", Crop, "_AOI_season_", season, ".RDS"))
  
  long_term_avg_dssat_data <- dssat_data %>%
    mutate(
      HDAT = as.Date(HDAT),
      Year = year(HDAT),
      N_rate = as.numeric(str_extract(TNAM, "(?<=^)[0-9]+(?=N)")),
      P_rate = as.numeric(str_extract(TNAM, "(?<=\\.)[0-9]+(?=P)")),
      fert_trt = sub("@.*", "", TNAM),
      plant_trt = sub(".*@", "", TNAM),
      lat_lon = paste(XLAT, LONG, sep = ", ")) %>%
    group_by(fert_trt, lat_lon, Variety, plant_trt, N_rate, P_rate) %>%
    dplyr::summarise(HWAH_avg = mean(HWAH), .groups = "drop") #%>%
  
  write_csv(long_term_avg_dssat_data,
            paste0(dir_path, "long_term_avg_yields.csv"))
  
  long_for_plot <- long_term_avg_dssat_data
  
  for (variety in unique(long_term_avg_dssat_data$Variety)) {
    for (location in long_term_avg_dssat_data$lat_lon[1:5]) {
      for (P_rate_i in unique(long_term_avg_dssat_data$P_rate)) {
        df_loc <- long_for_plot %>% filter(lat_lon == location,
                                           Variety == variety,
                                           P_rate == P_rate_i) %>%
          group_by(fert_trt, lat_lon, Variety, N_rate, P_rate) %>%
          dplyr::summarise(avg_yield = mean(HWAH_avg))
        
        p <- ggplot(df_loc, aes(x = N_rate, y = avg_yield)) +
          geom_line() +
          geom_point() +
          theme_bw() +
          labs(title = paste0("Location: ", location, ". Variety: ", variety, 
                              ". P rate: ", P_rate_i, " kg/ha"),
               x = "N rate (kg/ha)",
               y = "Yield (kg/ha)")
        
        ggsave(
          filename = paste0(dir_path, "avg_yield_", location, "_", variety, "_P",
                            P_rate_i, ".png"),
          plot = p, width = 8, height = 6, dpi = 300
        )
      }
    }
  }
}