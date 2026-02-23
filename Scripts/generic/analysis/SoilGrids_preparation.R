require(tidyverse)
require(sf)
require(raster)
require(terra)
require(geodata)
require(arrow)


make_variable_map <- function(soil_property, soil_depth) {
  # Extend as needed
  prop_map <- c(
    "nitrogen" = "N",
    "af_ptot" = "Ptot",
    "af_p" = "Pext",
    "sand" = "Sand",
    "clay" = "Clay",
    "silt" = "Silt",
    "bdod" = "BD",
    "cec" = "CEC",
    "phh2o" = "pH",
    "soc" = "OC"
  )
  
  if (!soil_property %in% names(prop_map)) {
    stop("Parameter not found in mapping: ", soil_property)
  }
  
  old_name <- paste0(soil_property, "_", soil_depth, "cm")
  
  new_name <- paste0(prop_map[[soil_property]], "_", soil_depth, "_SG")
  
  setNames(old_name, new_name)
}


# These equations are vinculated to the scripts used to produce the soil data
# used for modeling and may require updating. They are found in:
# ~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData_V2_phosphorus.R
# TODO: P must depend on whether the soil is calcareous or not (pass flag)
extrapolate_P <- function(P_mean_0_30, z, k){
  A <- P_mean_0_30 * (30 * k) / (1 - exp(-30 * k))
  P <- A * exp(-k * z)
  return(P)
}


mehlich3_to_olsen <- function(mehlich3_P){
  # Equations from: https://www.nature.com/articles/s41597-023-02022-4
  soil_calcareous <- FALSE
  # TODO: add logic for calcareous or soil pH
  if (!soil_calcareous) olsen_P <- 0.47 * mehlich3_P + 2.4
  if (soil_calcareous) olsen_P <- 0.41 * mehlich3_P + 1.1
  return(olsen_P)
}


# TODO: This should work for phosphorus too
get_SoilGrids_from_point <- function(sample_data, soil_property, soil_depth,
                                     tiles_path) {
  
  if (soil_property %in% c("af_p", "af_ptot")) {
    tiles_path <- "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/soilGrids"

    message(paste0("Obtaining point data for "), soil_property, " at ", 
            soil_depth)
    
    r <- rast(file.path(tiles_path, paste0(soil_property, "_0-30cm_30s.tif")))
    names(r) <- paste0(soil_property, "_0-30cm")
  } else {
    message(paste0("Obtaining point data for "), soil_property, " at ", 
            soil_depth)
    
    r <- rast(file.path(
      tiles_path, paste0(soil_property, "_", soil_depth, "cm_mean_30s.tif")))
  }
  
  unique_points <- data[!duplicated(st_coordinates(data)), ]
  points_vect <- vect(unique_points)
  
  values <- extract(r, points_vect)
  
  if (soil_property %in% c("af_p", "af_ptot")){
    new_var_name_map <- make_variable_map(soil_property, "0-30")
    values <- values %>% rename(!!!new_var_name_map)
    old_var_name <- names(new_var_name_map)
    new_var_name <- sub("_(.*)_SG", paste0("_", soil_depth, "_SG"), old_var_name)
    values <- values %>%
      rename(
        !!new_var_name := !!old_var_name
        )
    
    depth_intervals <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")
    midpoints <- c(2.5, 10, 22.5, 45, 80, 150)
    
    depth_map <- setNames(midpoints, depth_intervals)
    
    z <- depth_map[[soil_depth]]
    
    values[[new_var_name]] <- extrapolate_P(
      values[[new_var_name]], z = z, k = 0.03)
    
    values[[new_var_name]] <- mehlich3_to_olsen(
      values[[new_var_name]])
  } else{
    new_var_name_map <- make_variable_map(soil_property, soil_depth)
    values <- values %>% rename(!!!new_var_name_map)
    new_var_name <- names(new_var_name_map)
  }

  points_vect[[colnames(values)[2]]] <- values[, -1]
  
  coords <- crds(points_vect)
  df_points <- cbind(as.data.frame(points_vect), longitude = coords[, 1], latitude = coords[, 2])
  
  df_points <- df_points %>% select(c(longitude, latitude, all_of(new_var_name)))
  
  return(df_points)
}


# TODO: Fix function for non-sample data
# TODO: Add optional AOI filtering for long-scale comparison
SoilGrids_preparation <- function(Country, useCaseName, soil_property, soil_depth,
                                  adm_level = 1, zone = NULL,
                                  force_intersect = TRUE, sample_data = NULL,
                                  tiles_path = "/home/jovyan/common_data/soilgrids/raw") {
  
  # Country shape file
  zone_vect <- geodata::gadm(country = Country, level = adm_level,
                                path = ".")
  
  ### Define paths
  processed_files_folder <- file.path(
    "/home/jovyan/rs-soil-comparison-africa/Data",
    paste("useCase", Country, useCaseName, sep = "_"), "ISRIC")  # Processed files
  if (!dir.exists(processed_files_folder)) dir.create(
    processed_files_folder, recursive = TRUE)
  # tile_folder <- file.path(
  #   tiles_path, soil_property, soil_depth, "tiles")  # Tile folder
  # tile_files <- list.files(
  #   tile_folder, pattern = "\\.tif$", full.names = TRUE)  # All tiles in folder
  # tile_ids_file <- file.path(
  #   processed_files_folder, paste0(Country, "_tile_ids.csv"))  # Country tiles IDs
  output_raster_file <- file.path(
    processed_files_folder, paste0(Country, "_", soil_property, "_",
                                   soil_depth, "cm_raster.tif"))  # Output raster
  parquet_file <- file.path(
    processed_files_folder, paste0(Country, "_", soil_property, "_", soil_depth,
                                   ".parquet"))  # Output dataframe
  
  
  if(!is.null(sample_data)){
    sg_data <- get_SoilGrids_from_point(
      sample_data = sample_data, soil_property = soil_property, 
      soil_depth = soil_depth, tiles_path = tiles_path)
    return(list(df = sg_data, raster = c(NA)))
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
    
    tile_ids_file <- file.path(
      processed_files_folder, "tile_ids.csv")  # Country tiles IDs
    output_raster_file <- file.path(
      processed_files_folder, paste0(soil_property, "_", soil_depth,
                                     "cm_raster.tif"))  # Output raster
    parquet_file <- file.path(
      processed_files_folder, paste0("ISRIC_", soil_property, "_", soil_depth,
                                     ".parquet"))  # Output dataframe
  }
  
  
  # Get tiles that intersect with the study area
  intersect_tiles <- c()
  
  if (file.exists(tile_ids_file) & !force_intersect) {
    
    tile_ids <- read.csv(tile_ids_file)$tile_id
    
    intersect_tiles <- file.path(tiles_path,
                                 paste0(soil_property, "_", soil_depth,
                                        "cm_mean_", tile_ids, ".tif"))
    
    if (file.exists(output_raster_file) & file.exists(parquet_file)) {
      message(paste("Using saved files for "), soil_property, " at ", 
              soil_depth)
      output_raster <- rast(output_raster_file)
      sg_data <- read_parquet(parquet_file)
    } else {
      
      tile_rasters <- lapply(intersect_tiles, rast)
      if (length(tile_rasters) > 1){
        merged_raster <- do.call(merge, tile_rasters)
      } else if (length(tile_rasters) == 1) {
        merged_raster <- tile_rasters[[1]]
      } else {
        stop("No rasters found.")
      }
      message(paste("Cropping files for intesect tiles for "), soil_property, " at ", 
              soil_depth)
      
      output_raster <- crop(merged_raster, zone_vect)
      output_raster <- mask(output_raster, zone_vect)
      
      writeRaster(output_raster, output_raster_file, overwrite = TRUE)
      sg_data <- as.data.frame(output_raster, xy = TRUE, na.rm = TRUE) %>%
        rename(latitude = y,
               longitude = x,
               !!paste("ISRIC", soil_property, soil_depth, sep="_") := !!sym(names(output_raster)))
      
      # Save as Parquet
      write_parquet(sg_data, parquet_file)
    }
    
  } else {
    message(paste("Computing intersecting tiles for "), soil_property, " at ", 
            soil_depth)
    
    intersect_tiles <- Filter(function(f) {
      !is.null(intersect(ext(rast(f)), ext(zone_vect)))
    }, tile_files)
    
    # Extract tile indices and save for future use
    tile_ids <- gsub(".*_mean_|\\.tif$", "", intersect_tiles) |> as.integer()
    
    write.csv(data.frame(tile_id = tile_ids), tile_ids_file, row.names = FALSE)
    
    tile_rasters <- lapply(intersect_tiles, rast)
    if (length(tile_rasters) > 1){
      merged_raster <- do.call(merge, tile_rasters)
    } else if (length(tile_rasters) == 1) {
      merged_raster <- tile_rasters[[1]]
    } else {
      stop("No rasters found.")
    }
    
    output_raster <- crop(merged_raster, zone_vect)
    output_raster <- mask(output_raster, zone_vect)
    
    writeRaster(output_raster, output_raster_file, overwrite = TRUE)
    
    sg_data <- as.data.frame(output_raster, xy = TRUE, na.rm = TRUE) %>%
      rename(latitude = y,
             longitude = x,
             !!paste("SG", soil_property, soil_depth, sep="_") := !!sym(names(output_raster)))
    
    # Save as Parquet
    write_parquet(sg_data, parquet_file)

    message(paste("Tile ID, raster and files saved for", soil_property, "at",
                  soil_depth))
  }
  return(list(
    df = sg_data, 
    raster = output_raster))
}
