require(tidyverse)
require(sf)
require(raster)
require(terra)
require(arrow)

merge_ISDA_data <- function(df_list, raster_list, zone = NULL, 
                            adm_level = NULL, sample_data = NULL) {
  
  if (!is.null(sample_data)){
    df_merged <- reduce(df_list, left_join, by = c("longitude", "latitude"))
    
    processed_files_folder <- file.path(
      "/home/jovyan/rs-soil-comparison-africa/Data", 
      paste("useCase", Country, useCaseName, sep = "_"), "ISDA")  # Processed files
    
    write_parquet(df_merged, file.path(processed_files_folder,
                                       "sample_isda_data.parquet"))
    
    return(list(df = df_merged, merged_raster = NA))
    
  }
  
  df <- reduce(df_list, left_join, by = c("longitude", "latitude"))
  merged_raster <- rast(raster_list)
  
  if (!is.null(zone) & !is.null(adm_level)){
    write_parquet(df, file.path(processed_files_folder,
                                paste0(zone, "_full_isda_data.parquet")))
    
    writeRaster(merged_raster, 
                file.path(processed_files_folder, 
                          paste0(zone, "_merged_isda.tif")), 
                overwrite = TRUE)
  } else{
    write_parquet(df, file.path(processed_files_folder, "full_isda_data.parquet"))
    
    writeRaster(merged_raster, 
                file.path(processed_files_folder, "merged_isda.tif"), 
                overwrite = TRUE)
  }
  
  return(list(df = df, merged_raster = merged_raster))
}
