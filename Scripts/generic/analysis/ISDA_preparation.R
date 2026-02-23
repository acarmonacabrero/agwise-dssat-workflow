require(tidyverse)
require(sf)
require(raster)
require(terra)
require(geodata)
require(arrow)


make_variable_map <- function(soil_property, soil_depth) {
  # Extend as needed
  prop_map <- c(
    "log.n_tot_ncs" = "N",
    "log.p_mehlich3" = "Pext",
    "sand_tot_psa" = "Sand",
    "clay_tot_psa" = "Clay",
    "silt_tot_psa" = "Silt",
    "db_od" = "BD",
    "ph_h2o" = "pH",
    "log.oc" = "OC"
  )
  
  if (!soil_property %in% names(prop_map)) {
    stop("Parameter not found in mapping: ", soil_property)
  }
  
  old_name <- paste0(soil_property, "_", soil_depth, "cm")
  
  new_name <- paste0(prop_map[[soil_property]], "_", 
                     gsub("\\.\\.", "-", soil_depth), "_ISDA")
  
  setNames(old_name, new_name)
}


get_ISDA_from_point <- function(sample_data, soil_property, soil_depth,
                                isda_folder) {
  
  message(paste0("Obtaining point data for "), soil_property, " at ", 
          soil_depth)
  
  isda_file <- file.path(
    isda_folder, paste0("sol_", soil_property, "_m_30m_", soil_depth,
                        "cm_2001..2017_v0.13_wgs84.tif"))
  
  isda_raster <- rast(isda_file)
  
  unique_points <- data[!duplicated(st_coordinates(data)), ]
  points_vect <- vect(unique_points)
  
  values <- extract(isda_raster, points_vect)
  new_var_name <- names(make_variable_map(soil_property, soil_depth))
  
  points_vect[[new_var_name]] <- values[, -1]
  
  coords <- crds(points_vect)
  df_points <- cbind(as.data.frame(points_vect), longitude = coords[,1], 
                     latitude = coords[,2])
  
  df_points <- df_points %>% select(c(longitude, latitude, all_of(new_var_name)))
  
  return(df_points)
}



ISDA_preparation <- function(Country, useCaseName, soil_property, soil_depth,
                             adm_level = 1, zone = NULL,
                             force_intersect = TRUE, sample_data = NULL,
                             isda_folder = "/home/jovyan/common_data/isda/raw"){

  # Country shape file
  zone_vect <- geodata::gadm(country = Country, level = adm_level,
                             path = ".")
  
  ### Define paths
  processed_files_folder <- file.path(
    "/home/jovyan/rs-soil-comparison-africa/Data", 
    paste("useCase", Country, useCaseName, sep = "_"), "ISDA")  # Processed files
  if (!dir.exists(processed_files_folder)) dir.create(
    processed_files_folder, recursive = TRUE)
  tile_files <- list.files(isda_folder,
                           pattern = "\\.tif$", full.names = TRUE)  # All tiles in folder
  output_raster_file <- file.path(processed_files_folder,
                                  paste0(Country, "_",
                                         soil_property, "_",
                                         soil_depth, "cm_raster.tif"))  # Output raster
  parquet_file <- file.path(
    processed_files_folder, paste0(Country, "_", soil_property, "_", soil_depth,
                                   ".parquet"))  # Output dataframe
  
  if(!is.null(sample_data)){
    isda_data <- get_ISDA_from_point(
      sample_data = sample_data, soil_property = soil_property, 
      soil_depth = soil_depth, isda_folder = isda_folder)
    return(list(df = isda_data, raster = c(NA)))
  }
  
  # If adm_level == 0, do not modify zone_vect
  if(adm_level != 0){
    attrs <- values(zone_vect)
    idx <- which(attrs[[paste0("NAME_", adm_level)]] == zone)
    zone_vect <- zone_vect[idx, ]
    
    if (adm_level == 1) zone_path <- paste0("NAME1_", zone)
    if (adm_level == 2) zone_path <- paste0("NAME2_", zone)
    
    processed_files_folder <- file.path(processed_files_folder, zone_path)
    if (!dir.exists(processed_files_folder)) dir.create(
      processed_files_folder, recursive = TRUE)
    
    output_raster_file <- file.path(processed_files_folder,
                                    paste0(soil_property, "_",
                                           soil_depth, "cm_raster.tif"))  # Output raster
    parquet_file <- file.path(
      processed_files_folder, paste0("ISRIC_", soil_property, "_", soil_depth,
                                     ".parquet"))  # Output dataframe
    
  }
    
  if (!force_intersect & file.exists(output_raster_file) & file.exists(parquet_file)){
    message(paste("Using saved files for "), soil_property, " at ", 
            soil_depth)
    output_raster <- rast(output_raster_file)
    isda_data <- read_parquet(parquet_file)
  } else{
    isda_file <- file.path(
      isda_folder, paste0("sol_", soil_property, "_m_30m_", soil_depth,
                          "cm_2001..2017_v0.13_wgs84.tif"))
    
    isda_raster <- rast(isda_file)
    output_raster <- crop(isda_raster, zone_vect)
    output_raster <- mask(output_raster, zone_vect)
    
    # TODO: Revisit below
    # # # # #
    writeRaster(output_raster, output_raster_file, overwrite = TRUE)
    isda_data <- as.data.frame(output_raster, xy = TRUE, na.rm = TRUE) %>%
      rename(latitude = y,
             longitude = x,
             !!paste("ISRIC", soil_property, soil_depth, sep="_") := !!sym(names(output_raster)))
    
    # Save as Parquet
    write_parquet(isda_data, parquet_file)
  }
  return(list(
    df = isda_data, 
    raster = output_raster))
}

