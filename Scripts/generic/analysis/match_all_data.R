require(tidyverse)
require(sf)
require(raster)
require(terra)
require(arrow)


mehlich3_to_olsen <- function(mehlich3_P){
  # Equations from: https://www.nature.com/articles/s41597-023-02022-4
  soil_calcareous <- FALSE
  # TODO: add logic for calcareous or soil pH
  if (!soil_calcareous) olsen_P <- 0.47 * mehlich3_P + 2.4
  if (soil_calcareous) olsen_P <- 0.41 * mehlich3_P + 1.1
  return(olsen_P)
}


isric_to_isda_avg <- function(sg_data) {
  # Identify SG variable prefixes (ignore lat/lon)
  sg_cols <- grep("_SG$", names(sg_data), value = TRUE)
  sg_vars <- unique(sub("_\\d+-\\d+_SG$", "", sg_cols))
  
  # Initialize result with lat/lon
  result <- sg_data[, c("latitude", "longitude")]
  
  # Loop over SG variables
  for (var in sg_vars) {
    # Identify SG columns for this variable
    v0_5   <- paste0(var, "_0-5_SG")
    v5_15  <- paste0(var, "_5-15_SG")
    v15_30 <- paste0(var, "_15-30_SG")
    v30_60 <- paste0(var, "_30-60_SG")
    
    # Weighted averages for 0..20 cm
    result[[paste0(var, "_0-20_SG")]] <- 
      (5*sg_data[[v0_5]] + 10*sg_data[[v5_15]] + 5*sg_data[[v15_30]]) / 20
    
    # Weighted averages for 20..50 cm
    result[[paste0(var, "_20-50_SG")]] <- 
      (10*sg_data[[v15_30]] + 20*sg_data[[v30_60]]) / 30
  }
  
  return(result)
}


convert_data <- function(data, keep_original = TRUE) {
  
  # Conversion for SoilGrids
  convert_SG <- list(
    N = function(x) x,  # /100, # It seems it is already as g/kg. Previous conversion should be N (cg/kg → g/kg)
    Sand = identity,  # Already as %
    Silt = identity,  # Already as %
    Clay = identity,  # Already as %
    OC = function(x) x / 10,  # OC (dg/kg → g/kg)
    Pext = function(x) x / 100,  # Pext (mg/100kg → mg/kg)
    BD = function(x) x / 100,  # BD (cg/cm3 → g/cm3)
    pH = identity
  )
  # Conversion for ISDA
  convert_ISDA <- list(
    N = function(x) exp(x / 100) - 1,  # log N → g/kg
    Sand = identity,  # Already as %
    Silt = identity,  # Already as %
    Clay = identity,  # Already as %
    OC = function(x) exp(x / 10) - 1,  # log OC → g/kg,
    Pext = function(x) exp(x / 10) - 1,  # log Pext → mg/kg,
    BD = function(x) x / 100,  # g/100cm3 → g/cm3
    pH = identity
  )
  # Conversion for soil samples
  convert_sample <- list(
    N = function(x) x * 10,  # N (%) → g/kg
    Sand = identity,  # Already as %
    Silt = identity,  # Already as %
    Clay = identity,  # Already as %
    OC = function(x) x * 10,  # OC (%) → g/kg
    Pext = mehlich3_to_olsen,  # Pext Mehlich3 → Pext Olsen
    BD = identity,
    pH = identity,
    Ptot = identity  # Already as mg/kg (ppm)
  )
  
  data_conv <- data
  
  for (col in names(data)) {
    
    # Extract property (before first "_") and suffix (after last "_")
    property <- sub("_.*$", "", col)
    suffix   <- sub("^.*_", "", col)
    suffix   <- tolower(suffix)
    
    # Select conversion table
    conv_table <- switch(suffix,
                         "sg"     = convert_SG,
                         "isda"   = convert_ISDA,
                         "sample" = convert_sample,
                         NULL)
    
    if (is.null(conv_table)) next
    if (!property %in% names(conv_table)) next
    
    # Keep original column if requested
    if (keep_original) {
      ori_col <- paste0(col, "_ori")
      data_conv[[ori_col]] <- data[[col]]
    }
    
    # Apply conversion
    data_conv[[col]] <- conv_table[[property]](data[[col]])
  }
  
  return(data_conv)
}


match_all_data <- function(sg_data, isda_data, sample_data = NULL,
                           Country, adm_level = 1, zone = NULL
                           ) {
  
  if(!is.null(sample_data)) {
    sample_data <- sample_data %>%
      st_coordinates() %>%
      as.data.frame() %>%
      cbind(st_drop_geometry(data))
    
    names(sample_data)[1:2] <- c("longitude", "latitude")
  }
  
  sg_data_conv <- convert_data(data = sg_data, keep_original = FALSE)
  
  isda_data_conv <- convert_data(data = isda_data, keep_original = FALSE)
  
  if(!is.null(sample_data)) {
    sample_data_conv <- convert_data(data = sample_data, keep_original = FALSE)
    
    sample_data_conv <- sample_data_conv %>%
      group_by(latitude, longitude, sample_depth) %>%
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                .groups = "drop")
    
    # Select columns to pivot
    cols_to_pivot <- setdiff(names(sample_data_conv),
                             c("latitude", "longitude", "sample_depth"))
    
    sample_data_conv <- sample_data_conv %>%
      pivot_wider(
        id_cols = c(latitude, longitude),
        names_from = sample_depth,
        values_from = matches("_sample$"),
        names_glue = "{str_remove(.value, '_sample$')}_{sample_depth}_sample"
      )
    # sample_data_conv <- sample_data_conv %>%
    #   pivot_wider(
    #     id_cols = c(latitude, longitude),
    #     names_from = sample_depth,
    #     values_from = all_of(cols_to_pivot),
    #     names_glue = "{.value}_{sample_depth}"   # ensures column name + depth
    #   )
  }
  
  all_data <- list(isda_data_conv, sg_data_conv, sample_data_conv) %>%
    reduce(left_join, by = c("longitude", "latitude"))
  sg_to_isda_data <- isric_to_isda_avg(sg_data_conv)
  
  all_data <- left_join(all_data, sg_to_isda_data, 
                        by = c("longitude", "latitude"))
  
  processed_files_folder <- file.path(
    "/home/jovyan/rs-soil-comparison-africa/Data",
    paste("useCase", Country, useCaseName, sep = "_"))  # Processed files
  
  if (!dir.exists(processed_files_folder)) dir.create(
    processed_files_folder, recursive = TRUE)
  
  if (!is.null(zone)) {
    parquet_file <- file.path(
      processed_files_folder, paste0(zone, "_all_data_", Country, ".parquet"))  # Output dataframe
    
  } else { 
    parquet_file <- file.path(
      processed_files_folder, paste0("all_data_", Country, ".parquet"))  # Output dataframe
    
  }
  
  write_parquet(all_data, parquet_file)
  
  return(all_data)
}

