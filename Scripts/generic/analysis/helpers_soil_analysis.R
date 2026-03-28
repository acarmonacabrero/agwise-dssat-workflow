#######################################
### Common ISRIC and ISDA functions ###
#######################################
packages_required <- c(
  "arrow", "raster", "yardstick", "DescTools", "gridExtra", "hexbin", "ggplot2"
)

invisible(lapply(packages_required, load_or_install))

# Creates names for renaming variables
get_canonical_name <- function(soil_property, source, soil_depth, 
                               soil_var_map, depth_map) {
  
  
  canonical_var <- soil_var_map %>%
    filter(.data[[source]] == soil_property) %>%
    pull(canonical)
  
  if (length(canonical_var) == 0) {
    stop(paste("Variable", soil_property, "not found for source", source))
  }
  
  # Map depth to canonical
  if (source == "ISDA") {
    # Map ISDA depth to canonical depth
    depth_idx <- which(depth_map$ISDA == soil_depth)
    if (length(depth_idx) == 0) stop(paste("Depth", soil_depth, "not found in depth_map for ISDA"))
    canonical_depth <- depth_map$canonical[depth_idx]
  } else if (source == "ISRIC") {
    # For ISRIC do nothing, it will require interpolation
    canonical_depth <- soil_depth
  }
  
  # Build final name: source, canonical, canonical depth
  paste(source, canonical_var, canonical_depth, sep = "_")
}

# Get sample data or prepare to run without observational data
get_sample_data <- function(sample_soil_file_name, project_root, read_data = F) 
  {
  if (read_data) {
    sample_data <- read_csv(paste0(
      project_root, '/Data/soil_dataset/', sample_soil_file_name))
    message("Routine for processing sample data needs to be implemented.")
    return(sample_data)
  } else {
    return(NULL)
  }
}

# # Function to select tiles that intersect a set of coordinates
select_tiles_from_AOI <- function(tile_files, location_df, required_tiles_path) {
  # Assumes location_df has columns: lon, lat
  aoi_vect <- vect(location_df, geom = c("lon", "lat"), crs = "EPSG:4326")
  
  # Filter tiles that intersect the AOI
  valid_tiles <- Filter(function(f) {
    r <- tryCatch(rast(f), error = function(e) NULL)
    if (is.null(r)) return(FALSE)
    
    # Transform AOI to raster CRS if needed
    if (!compareCRS(r, aoi_vect)) {
      aoi_r <- project(aoi_vect, crs(r))
    } else {
      aoi_r <- aoi_vect
    }
    
    # Check intersection
    !is.null(intersect(ext(r), ext(aoi_r)))
    
  }, tile_files)
  
  write.csv(valid_tiles, required_tiles_path, row.names = F)
  
  return(valid_tiles)
}


# TODO: P must depend on whether the soil is calcareous or not (pass flag)
# Convert Mehlich3 P to Olsen
mehlich3_to_olsen <- function(mehlich3_P){
  message("An Olsen P value of 2.4 (or 1.1 in calcareous soils) corresponds to a Mehlich-3 P value of 0.")  # Equations from: https://www.nature.com/articles/s41597-023-02022-4
  soil_calcareous <- FALSE
  if (!soil_calcareous) olsen_P <- 0.47 * mehlich3_P + 2.4
  if (soil_calcareous) olsen_P <- 0.41 * mehlich3_P + 1.1
  return(olsen_P)
}


### Make unit transformations
apply_soil_transformations <- function(df, soil_var_map, depth_map, source) {
  result <- df %>% 
    dplyr::select(country, NAME_1, NAME_2, longitude, latitude)
  
  vars <- soil_var_map$canonical
  source_cols <- soil_var_map[[source]]
  transform_funs <- soil_var_map[[paste0(source, "_transformation")]]
  
  for (i in seq_along(vars)) {
    var <- vars[i]
    src_var <- source_cols[i]
    fun <- transform_funs[[i]]
    
    for (depth in depth_map$canonical) {
      col_name <- paste(source, var, depth, sep = "_")
      
      if (!col_name %in% names(df)) next
      
      out_col <- paste(source, var, depth, "cm", sep = " ")
      
      # Apply transformation
      result[[out_col]] <- fun(df[[col_name]])
    }
  }
  
  return(result)
}


### Merge 2+ dfs using coordinates. Extra observations can be dropped or kept
### dfs: a list of dataframes to merge
### keep_extra: T -> full join (keep all rows)
###             F -> inner join (only matching rows)
merge_by_coords <- function(
    dfs, keep_extra = F, project_root, Country, useCaseName, adm_level = 0,
    zones = NULL, Crop = "analysis") {
  if (!is.list(dfs) || length(dfs) < 2) {
    stop("dfs must be a list of at least 2 dataframes")
  }
  
  merged_df <- dfs[[1]]
  
  if (length(dfs) == 2) {
    merge_by_cols <- c("latitude", "longitude", "NAME_1", "NAME_2", "country")
  } else {
    merge_by_cols <- c("latitude", "longitude")
  }
  
  # Loop through remaining dataframes
  for (i in 2:length(dfs)) {
    if (keep_extra) {
      merged_df <- full_join(merged_df, dfs[[i]], by = merge_by_cols)
    } else {
      merged_df <- inner_join(merged_df, dfs[[i]], by = merge_by_cols)
    }
  }
  
  save_path <- paste0(project_root, "Data/useCase_", Country, "_", useCaseName, "/", Crop, "/results/")
  
  if (!dir.exists(save_path)) dir.create(save_path, recursive = T)
  
  parquet_name <- paste0(save_path, "/all_data_", Country, "_", useCaseName,
                         "_adm_level_", adm_level, ".parquet")
  
  write_parquet(merged_df, parquet_name)
  
  return(merged_df)
}

#######################
### ISRIC FUNCTIONS ###
#######################
# TODO: P must depend on whether the soil is calcareous or not (pass flag)
# Extrapolate P to ISRIC 
extrapolate_P <- function(P_mean_0_30, z, k) {
  A <- P_mean_0_30 * (30 * k) / (1 - exp(-30 * k))
  P <- A * exp(-k * z)
  return(P)
}


### Read and process ISRIC (P extrapolation, Olsen conversion [optional]) files
# TODO: Test for entire country
process_ISRIC_data <- function(
    soil_var_map, depth_map, project_root, Country, useCaseName, adm_level = 0,
    zones = NULL, sample_data = NULL, resltn = 0.05, Crop = "analysis",
    district = NULL, Olsen_conversion = F, force_reanalysis = T,
    tiles_path = "/home/jovyan/common_data/soilgrids/raw", p_var = "af_p") {
  
  processed_files_folder <- paste0(
    project_root, "Data/", paste("useCase", Country, useCaseName, sep = "_"),
    "/analysis/ISRIC")  # Processed files
  
  if (!dir.exists(processed_files_folder)) dir.create(
    processed_files_folder, recursive = T)
  
  # Sample data usage route
  if(!is.null(sample_data)) {
    message("Function for sample data is missing. Sample data would be used instead of AOI_GPS to extract soil grid properties.")
    return(NULL)
  }
  
  # AOI_GPS usage route
  AOI_GPS <- getGridCoordinates(
    country = Country, useCaseName = useCaseName, Crop = Crop, 
    resltn = resltn, project_root = project_root, provinces = zones, district = district)
  
  # Select zone if analysis not for entire country
  if (!is.null(zones)) {
    if (adm_level == 1) AOI_GPS <- AOI_GPS %>% filter(NAME_1 %in% zones)
    else if (adm_level == 2) AOI_GPS <- AOI_GPS %>% filter(NAME_2 %in% district)
  }
  
  # List of tiles that intersect with zone
  tiles_vect <- get_intersect_tiles(
    Country, useCaseName, adm_level, zones, tiles_path, soil_var_map, depth_map,
    sample_data = sample_data, AOI_GPS = AOI_GPS, processed_files_folder, 
    force_reanalysis)
  location_vect <- vect(AOI_GPS, geom = c("lon", "lat"), crs = "EPSG:4326")
  tile_ids <- tiles_vect$tile_ids
  
  df_list <- list()
  
  # Loop through ISIRC tiles
  for (soil_property in soil_var_map$ISRIC) {
    if (soil_property %in% c(p_var)) next
    
    for (soil_depth in depth_map$ISRIC) {
      intersect_tiles <- get_tiles_to_read(soil_property, soil_depth, tile_ids)
      tile_rasters <- lapply(intersect_tiles, rast)
      if (length(tile_rasters) > 1){
        merged_raster <- do.call(merge, tile_rasters)
      } else if (length(tile_rasters) == 1) {
        merged_raster <- tile_rasters[[1]]
      }
      
      raster_values <- terra::extract(merged_raster, location_vect)
      
      df_data <- AOI_GPS %>%
        bind_cols(as_tibble(raster_values[,-1])) %>%  # remove ID column
        rename(
          !!get_canonical_name(
            soil_property, "ISRIC", soil_depth, soil_var_map, depth_map
          ) := "value",
          longitude = lon,
          latitude = lat
        )
      
      df_list[[paste(soil_property, soil_depth, sep = "_")]] <- df_data
      message(paste("Processed", soil_property, "at", soil_depth, 'cm'))
      
    }
  }
  
  df_merged <- Reduce(function(x, y) full_join(
    x, y, by = c("longitude", "latitude", "country", "NAME_1", "NAME_2")), df_list)  %>% 
    filter(complete.cases(.))  # remove rows with NA
  
  
  if (p_var %in% soil_var_map$ISRIC) {
    
    p_df_data <- process_p_vars(
      soil_var_map, location_vect, AOI_GPS, p_var = p_var, 
      new_depths = c("0-20", "20-50"), mid_points = c(10, 35), k = 0.03)
    
    message("Processed", p_var, "at 0-30 cm")
    
    df_merged <- left_join(
      df_merged, p_df_data, 
      by = c("longitude", "latitude", "country", "NAME_1", "NAME_2")) %>% 
      filter(complete.cases(.))  # remove rows with NA
  }
  
  
  return(df_merged)
}


### Process ISRIC P variables
process_p_vars <- function(
    soil_var_map, location_vect, AOI_GPS, p_var = "af_p", 
    new_depths = c("0-20", "20-50"), mid_points = c(10, 35), k = 0.03,
    p_tiles_path = "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/soilGrids/",
    Olsen_conversion = F
) {
  
  p_path <- paste0(p_tiles_path, p_var, "_0-30cm_30s.tif")
  p_rast <- rast(p_path)
  
  raster_values <- terra::extract(p_rast, location_vect)
  
  p_df_data <- AOI_GPS %>%
    bind_cols(as_tibble(raster_values[,-1])) %>%  # remove ID column
    rename(
      longitude = lon,
      latitude = lat
    )
  
  canonical_p_name <- soil_var_map %>% filter(ISRIC == p_var) %>%
    pull(canonical)
  p_vars_names <- paste("ISRIC", canonical_p_name, new_depths, sep = "_")
  
  extrapolated_p <- t(
    mapply(extrapolate_P, p_df_data$value, k,
           MoreArgs = list(z = mid_points)))
  colnames(extrapolated_p) <- p_vars_names
  
  p_df_data <- p_df_data %>%
    dplyr::select(-value) %>%
    bind_cols(as.data.frame(extrapolated_p))
  
  if (Olsen_conversion) {
    p_df_data <- p_df_data %>%
      mutate(across(all_of(p_vars_names), mehlich3_to_olsen))
  }
  return(p_df_data)
}


### Get list of tiles that intersect with zone and SpatVector of locations of interest
get_intersect_tiles <- function(
    Country, useCaseName, adm_level, zones, tiles_path, soil_var_map, depth_map,
    sample_data = NULL, AOI_GPS = NULL, processed_files_folder, 
    force_reanalysis = T) {
  
  required_tiles_path <- file.path(
    processed_files_folder, paste(Country, useCaseName, "adm_level", 
                                  adm_level, "tile_ids.csv", sep = "_")
  )
  
  if (file.exists(required_tiles_path) && !force_reanalysis) {
    intersect_tiles <- read.csv(required_tiles_path)
    location_vect <- vect(AOI_GPS, geom = c("lon", "lat"), crs = "EPSG:4326")
    message("Tile intersection read from existing files.")
  } else {
    # Determine tiles to check
    tile_folder <- file.path(
      tiles_path, soil_property = soil_var_map$ISRIC[1], 
      soil_depth = depth_map$ISRIC[1], "tiles")
    tile_files <- list.files(tile_folder, pattern = "\\.tif$", full.names = T)
    
    # Filter out unreadable tiles
    valid_tiles <- sapply(tile_files, function(f) {
      tryCatch({
        rast(f)
        T
      }, error = function(e) {
        message("Skipping unreadable tile: ", f)
        F
      })
    })
    tile_files <- tile_files[valid_tiles]
    
    message("Intersection reanalyzed.")
    
    if (!is.null(sample_data)) {
      intersect_tiles <- select_tiles_from_AOI(
        tile_files, location_df = sample_data, required_tiles_path)
      location_vect <- vect(sample_data, geom = c("lon", "lat"), crs = "EPSG:4326")
    } else {
      intersect_tiles <- select_tiles_from_AOI(
        tile_files, location_df = AOI_GPS, required_tiles_path)
      location_vect <- vect(AOI_GPS, geom = c("lon", "lat"), crs = "EPSG:4326")
    }
  }
  
  # Ensure intersect_tiles is a data frame
  if (is.null(intersect_tiles) || length(intersect_tiles) == 0) {
    intersect_tiles <- data.frame(x = character(0))
    tile_ids <- character(0)
  } else {
    if (!"x" %in% names(intersect_tiles)) intersect_tiles <- data.frame(x = intersect_tiles)
    tile_ids <- sub(".*_(\\d+)\\.tif$", "\\1", intersect_tiles$x)
  }
  
  return(list(
    intersect_tiles = intersect_tiles,
    tile_ids = tile_ids,
    location_vect = location_vect
  ))
}


### Construct the list of required
get_tiles_to_read <- function(
    soil_property, soil_depth, tile_ids, 
    tiles_path = "/home/jovyan/common_data/soilgrids/raw/") {
  file_names <- paste0(
    soil_property, "_", soil_depth, "cm_mean_", tile_ids, ".tif"
  )
  
  file_paths <- file.path(tiles_path, soil_property, soil_depth, "tiles", file_names)
  
  return(file_paths)
}


### Convert ISRIC data to ISDA depths
interpolate_isric_depth_isda <- function(
    isric_df, soil_var_map, depth_map, source = "ISRIC", isric_p_var = "af_p") {
  
  # Name of ISRIC p_var
  canonical_p_var <- soil_var_map %>%
    dplyr::filter(ISRIC == isric_p_var) %>%
    dplyr::pull(canonical)
  
  # Helper: convert "a-b" → numeric vector
  parse_depth <- function(x) {
    as.numeric(strsplit(x, "-")[[1]])
  }
  
  # Helper: compute overlap between two depth intervals
  overlap <- function(a, b) {
    max(0, min(a[2], b[2]) - max(a[1], b[1]))
  }
  
  result <- isric_df
  
  # Loop through variables defined in soil_var_map
  for (i in seq_len(nrow(soil_var_map))) {
    
    var <- soil_var_map$canonical[i]
    
    # Do not change P variable, already in canonical format
    if (length(canonical_p_var) == 1 && var == canonical_p_var) next
    
    for (target_depth in depth_map$canonical) {
      
      target_range <- parse_depth(target_depth)
      target_width <- diff(target_range)
      
      weighted_sum <- 0
      
      for (isric_depth_i in depth_map$ISRIC) {
        
        src_range <- parse_depth(isric_depth_i)
        w <- overlap(target_range, src_range)
        
        if (w > 0) {
          col_name <- paste0(source, "_", var, "_", isric_depth_i)
          
          if (col_name %in% names(isric_df)) {
            weighted_sum <- weighted_sum + w * isric_df[[col_name]]
          }
        }
      }
      
      # New output column name
      out_col <- paste("ISRIC", var, target_depth, sep = "_")
      result[[out_col]] <- weighted_sum / target_width
    }
  }
  
  return(result)
}


######################
### ISDA FUNCTIONS ###
######################

### Read and process ISDA (Olsen conversion [optional]) files
# TODO: Test for entire country
process_ISDA_data <- function(
    soil_var_map, depth_map, project_root, Country,
    useCaseName, adm_level = 0, zones = NULL, sample_data = NULL, resltn = 0.05,
    Crop = "analysis", district = NULL, Olsen_conversion = F, force_reanalysis = T,
    isda_folder = "/home/jovyan/common_data/isda/raw", p_var = "log.p_mehlich3"
) {
  
  processed_files_folder <- paste0(
    project_root, "Data/", paste("useCase", Country, useCaseName, sep = "_"),
    "/analysis/ISDA")  # Processed files
  
  if (!dir.exists(processed_files_folder)) dir.create(
    processed_files_folder, recursive = T)
  
  # Sample data usage route
  if(!is.null(sample_data)) {
    message("Function for sample data is missing. Sample data would be used instead of AOI_GPS to extract soil grid properties.")
    return(NULL)
  }
  
  # AOI_GPS usage route
  AOI_GPS <- getGridCoordinates(
    country = Country, useCaseName = useCaseName, Crop = Crop, 
    resltn = resltn, project_root = project_root, provinces = zones, district = district)
  
  # Select zone if analysis not for entire country
  if (!is.null(zones)) {
    if (adm_level == 1) AOI_GPS <- AOI_GPS %>% filter(NAME_1 %in% zones)
    else if (adm_level == 2) AOI_GPS <- AOI_GPS %>% filter(NAME_2 %in% district)
  }
  
  location_vect <- vect(AOI_GPS, geom = c("lon", "lat"), crs = "EPSG:4326")
  
  tile_files <- list.files(isda_folder,
                           pattern = "\\.tif$", full.names = TRUE)  # All tiles in folder
  
  df_list <- list()
  
  for (soil_property in soil_var_map$ISDA) {
    for (soil_depth in depth_map$ISDA) {
      isda_file <- file.path(
        isda_folder, paste0("sol_", soil_property, "_m_30m_", soil_depth,
                            "cm_2001..2017_v0.13_wgs84.tif"))
      
      isda_raster <- rast(isda_file)
      
      raster_values <- terra::extract(isda_raster, location_vect)
      
      df_data <- AOI_GPS %>%
        bind_cols(as_tibble(raster_values[,-1])) %>%  # remove ID column
        rename(
          !!get_canonical_name(
            soil_property, "ISDA", soil_depth, soil_var_map, depth_map
          ) := "value",
          longitude = lon,
          latitude = lat
        )
      df_list[[paste(soil_property, soil_depth, sep = "_")]] <- df_data
      message(paste("Processed", soil_property, "at", soil_depth, 'cm'))
    }
  }
  
  df_merged <- Reduce(function(x, y) full_join(
    x, y, by = c("longitude", "latitude", "country", "NAME_1", "NAME_2")), df_list)  %>% 
    filter(complete.cases(.))  # remove rows with NA
  
  
  if (Olsen_conversion) {
    canonical_p_var <- soil_var_map %>%
      dplyr::filter(ISDA == p_var) %>%
      dplyr::pull(canonical)
    
    p_var_names <- paste("ISDA", canonical_p_var, depth_map$canonical, sep = "_")
    
    df_merged <- df_merged %>%
      mutate(across(all_of(p_var_names), mehlich3_to_olsen))
  }
  
  return(df_merged)
}


#########################
### Figures and stats ###
#########################
### Make ISRIC and ISDA var names
make_var_names <- function(var, depth) {
  list(
    isric = paste0("ISRIC ", var, " ", depth, " cm"),
    isda  = paste0("ISDA ", var, " ", depth, " cm")
  )
}


### Get pairs of ISRIC and ISDA vars
get_pair_data <- function(df, var, depth) {
  vars <- make_var_names(var, depth)
  
  out <- df %>%
    dplyr::select(
      longitude, latitude,
      ISRIC = all_of(vars$isric),
      ISDA  = all_of(vars$isda)
    ) %>%
    dplyr::filter(!is.na(ISRIC), !is.na(ISDA))
  
  return(out)
}


### Compare basic stats (ISDA vs ISRIC)
compare_stats <- function(
    df, var, depth, country, adm_level, project_root, useCaseName) {
  d <- get_pair_data(df, var, depth)
  
  diff <- d$ISDA - d$ISRIC
  
  message("Bias: ISDA - ISRIC")
  
  stats_table <- tibble::tibble(
    variable = var,
    depth = depth,
    n = nrow(d),
    mean_ISRIC = mean(d$ISRIC),
    mean_ISDA  = mean(d$ISDA),
    bias = mean(diff),
    mae  = mean(abs(diff)),
    rmse = sqrt(mean(diff^2)),
    cor  = cor(d$ISRIC, d$ISDA),
    r2   = cor(d$ISRIC, d$ISDA)^2
  )
  
  return(stats_table)
}


### Save all stats
save_stats <- function(
    all_stats, project_root, Country, useCaseName, adm_level, Crop = "analysis"
    ) {
  
  save_path <- paste0(
    project_root,
    "Data/useCase_", Country, "_", useCaseName, "/",
    Crop, "/results/"
  )
  
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }
  
  csv_name <- file.path(
    save_path,
    paste0("all_stats_", Country, "_", useCaseName, "_adm_level_", adm_level, ".csv")
  )
  
  readr::write_csv(all_stats, csv_name)
}


### Plot hexbins
plot_hexbin <- function(
    df, var, depth,
    country, zones, adm_level = 1, project_root,
    useCaseName, Crop = "analysis", 
    save = TRUE, width = 8, height = 6, dpi = 300
) {
  
  # Extract paired data
  d <- get_pair_data(df, var, depth)
  
  # Calculate max for axis limits
  max_val <- max(c(d$ISRIC, d$ISDA), na.rm = TRUE)
  
  # Build plot
  p <- ggplot2::ggplot(d, ggplot2::aes(ISRIC, ISDA)) +
    ggplot2::geom_hex() +
    
    # 1:1 line
    ggplot2::geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", linewidth = 1
    ) +
    
    # Force square axes starting at 0
    ggplot2::coord_equal(
      xlim = c(0, max_val),
      ylim = c(0, max_val)
    ) +
    
    ggplot2::labs(
      title = paste("Hexbin:", var, depth),
      subtitle = "Dashed = 1:1",
      x = "ISRIC",
      y = "ISDA"
    ) +
    
    ggplot2::theme_minimal()
  
  # Build save path
  save_path <- paste0(
    project_root,
    "Data/useCase_", country, "_", useCaseName, "/",
    Crop, "/results/"
  )
  
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }
  
  # Clean names
  var_clean   <- gsub(" ", "_", var)
  depth_clean <- gsub("-", "_", depth)
  
  var_clean <- gsub("%", "pct", gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", var)))
  
  plot_name <- paste0(
    save_path,
    "hexbin_",
    var_clean, "_", depth_clean,
    "_", country, "_", useCaseName,
    "_adm_level_", adm_level,
    ".png"
  )
  
  # Save the plot
  if (save) {
    ggplot2::ggsave(
      filename = plot_name,
      plot = p,
      width = width,
      height = height,
      dpi = dpi
    )
  }
  
  return(p)
}

### Make scatter plot
plot_scatter <- function(
    df, var, depth,
    add_loess = FALSE,
    country, zones, adm_level = 1, project_root,
    useCaseName, Crop = "analysis", 
    save = TRUE, width = 8, height = 6, dpi = 300
) {
  
  d <- get_pair_data(df, var, depth)
  
  lims <- range(c(d$ISRIC, d$ISDA), na.rm = TRUE)
  
  p <- ggplot2::ggplot(d, ggplot2::aes(ISRIC, ISDA)) +
    ggplot2::geom_point(alpha = 0.6) +
    
    # 1:1 line
    ggplot2::geom_abline(
      slope = 1, intercept = 0,
      linetype = "dashed", linewidth = 1
    ) +
    
    # Linear regression
    ggplot2::geom_smooth(
      method = "lm",
      se = TRUE,
      formula = y ~ x
    ) +
    
    # Same limits
    ggplot2::scale_x_continuous(limits = lims) +
    ggplot2::scale_y_continuous(limits = lims) +
    
    # Square plot
    ggplot2::coord_equal() +
    
    ggplot2::labs(
      title = paste("ISRIC vs ISDA:", var, depth),
      subtitle = "Solid = linear fit, dashed = 1:1",
      x = "ISRIC",
      y = "ISDA"
    ) +
    
    ggplot2::theme_minimal()
  
  # Optional LOESS
  if (add_loess) {
    p <- p +
      ggplot2::geom_smooth(
        method = "loess",
        se = FALSE,
        linetype = "dotted"
      )
  }
  
  # Save path
  save_path <- paste0(
    project_root,
    "Data/useCase_", country, "_", useCaseName, "/",
    Crop, "/results/"
  )
  
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }
  
  # Clean names
  var_clean   <- gsub(" ", "_", var)
  depth_clean <- gsub("-", "_", depth)
  
  var_clean <- gsub("%", "pct", gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", var)))
  
  plot_name <- paste0(
    save_path,
    "scatter_",
    var_clean, "_", depth_clean,
    "_", country, "_", useCaseName,
    "_adm_level_", adm_level,
    ".png"
  )
  
  # Save
  if (save) {
    ggplot2::ggsave(
      filename = plot_name,
      plot = p,
      width = width,
      height = height,
      dpi = dpi
    )
  }
  
  return(p)
}

### Plot ISDA - ISRIC differences for a variable
plot_diff_map <- function(
    df, var, depth, country, zones, adm_level = 1, project_root,
    useCaseName, Crop = "analysis", 
    save = TRUE, width = 8, height = 6, dpi = 300
) {
  
  # Get paired data
  d <- get_pair_data(df, var, depth) %>%
    dplyr::mutate(diff = ISDA - ISRIC)
  
  # Get GADM boundaries
  countrySpVec <- geodata::gadm(country, level = 2, path = ".")
  country_sf <- sf::st_as_sf(countrySpVec)
  
  admin_field <- if (adm_level == 1) "NAME_1" else "NAME_2"
  
  zones_sf <- country_sf %>%
    dplyr::filter(.data[[admin_field]] %in% zones)
  
  # Build plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = zones_sf,
      fill = NA,
      color = "black",
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = d,
      ggplot2::aes(longitude, latitude, color = diff),
      size = 3
    ) +
    ggplot2::scale_color_gradient2(midpoint = 0) +
    ggplot2::labs(
      title = paste("Difference (ISDA - ISRIC):", var, depth),
      subtitle = paste("Zones:", paste(zones, collapse = ", ")),
      color = "Difference"
    ) +
    ggplot2::theme_minimal()
  
  save_path <- paste0(
    project_root,
    "Data/useCase_", country, "_", useCaseName, "/",
    Crop, "/results/"
  )
  
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }
  
  var_clean   <- gsub(" ", "_", var)
  depth_clean <- gsub("-", "_", depth)
  
  var_clean <- gsub("%", "pct", gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", var)))
  
  plot_name <- paste0(
    save_path,
    "diff_map_",
    var_clean, "_", depth_clean,
    "_", country, "_", useCaseName,
    "_adm_level_", adm_level,
    ".png"
  )
  
  # Save
  if (save) {
    ggplot2::ggsave(
      filename = plot_name,
      plot = p,
      width = width,
      height = height,
      dpi = dpi
    )
  }
  
  return(p)
}