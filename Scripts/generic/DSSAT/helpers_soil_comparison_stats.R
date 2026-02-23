read_isric_isda_data <- function(project_root, country, useCaseName, Crop, season, AOI) {
  if (AOI) {
    dir_path <- paste0(project_root, "/Data/useCase_", country, "_", useCaseName,
                       "/", Crop, "/result/DSSAT/AOI/")
  } else {
    dir_path <- paste0(project_root, "/Data/useCase_", country, "_", useCaseName,
                       "/", Crop, "/result/DSSAT/fieldData/")
  }
  isric_path <- paste0(dir_path, "ISRIC_useCase_", country, "_", useCaseName, "_",
                       Crop, "_AOI_season_", season, ".RDS")
  isda_path <- paste0(dir_path, "ISDA_useCase_", country, "_", useCaseName, "_",
                      Crop, "_AOI_season_", season, ".RDS")
  if (file.exists(isric_path) && file.exists(isda_path)) {
    isric_data <- readRDS(isric_path)
    isda_data <- readRDS(isda_path)
  } else {
    stop("Missing ISRIC/ISDA file. Change config file and rerun for the other soil source.")
  }
  
  return(list(
    ISRIC = isric_data, 
    ISDA = isda_data
    ))
}

add_year_column <- function(df, date_col = "HDAT") {
  df$Year <- as.numeric(format(as.Date(df[[date_col]]), "%Y"))
  return(df)
}


get_common_pixels <- function(isric_data, isda_data) {
  common_pixels <- inner_join(
    isric_data %>% distinct(Lat, Long),
    isda_data  %>% distinct(Lat, Long),
    by = c("Lat", "Long")
  )
  common_pixels
}

