require(tidyverse)
require(sf)
require(raster)
require(terra)
require(arrow)

merge_SoilGrids_data <- function(df_list, raster_list, Country, 
                                 zone = NULL, adm_level = NULL, 
                                 sample_data = NULL){
  
  if (!is.null(sample_data)) {
    rbind(df_list)
    df_merged <- reduce(df_list, full_join, by = c("longitude", "latitude"))
    
    processed_files_folder <- file.path(
      "/home/jovyan/rs-soil-comparison-africa/Data",
      paste("useCase", Country, useCaseName, sep = "_"), "ISRIC")  # Processed files
    
    write_parquet(df_merged, file.path(processed_files_folder,
                                       "sample_isric_data.parquet"))

    return(list(df = df_merged, merged_raster = NA))
  }
  
  # Since P and other SG rasters don't have the same resolution resampling is necessary
  same_res <- length(unique(lapply(raster_list, res))) == 1
  
  if (same_res) {
    df <- reduce(df_list, left_join, by = c("longitude", "latitude"))
    merged_raster <- rast(raster_list)
    message(paste("All rasters had the same resolution"))
  } else if (!same_res){
    # Match resolution of the P variables to the other ISRIC variables
    target_raster <- raster_list[[1]]
    raster_list <- lapply(raster_list, function(r) resample(r, target_raster))
    merged_raster <- rast(raster_list)
    
    df <- as.data.frame(merged_raster, xy = TRUE, na.rm = TRUE) %>%
      rename(longitude = x,
             latitude = y)
    message(paste("Rasters had different resolution."))
  }
  
  if (!is.null(zone) & !is.null(adm_level)){
    write_parquet(df, file.path(processed_files_folder, 
                                paste0(zone, "_full_isric_data.parquet")))
    writeRaster(merged_raster, 
                file.path(processed_files_folder, 
                          paste0(zone, "_merged_isric.tif")), 
                overwrite = TRUE)
  }
  else{
    write_parquet(df, file.path(processed_files_folder, "full_isric_data.parquet"))
    writeRaster(merged_raster, 
                file.path(processed_files_folder, "merged_isric.tif"), 
                overwrite = TRUE)
  }
  
  return(list(df = df, merged_raster = merged_raster))
}
