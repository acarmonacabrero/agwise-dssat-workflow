require(tidyverse)
require(sf)
require(raster)
require(terra)
require(arrow)
select <- dplyr::select


# "...1", "plotID", "trialID", "fieldID", "newTrialID", "geopoint.Latitude", "geopoint.Longitude", "soilSampleID", "sampleBatchID", "motherID", "generation", "AncestorID", "plantPart", "rootYield", "trialValidity", "reportAction", "trialCode", "Level", "placing", "split", "forward", "soilSampleID_TA", "labName", "typeAnalysis", "description", "countryCollection", "zoneState", "soilDepth_cm", "useCase", "yearCollection", "farmersName", "cluster", "district", "batchCode", "samplingPeriod", "pH_H2O_1_2.5", "OC_perc", "N_perc", "Meh_P_ppm", "sand_perc", "silt_perc", "clay_perc", "Ca_cmol_per_kg", "Mg_cmol_per_kg", "K_cmol_per_kg", "Na_cmol_per_kg", "Exch_Acidity_cmol_per_kg", "ECEC_cmol_per_kg", "Zn_ppm", "Cu_ppm", "Mn_ppm", "Fe_ppm", "pH", "P_ppm", "S_ppm", "Na_ppm", "Mg_ppm", "Hp_ppm", "ECs_ppm", "Ca_ppm", "B_ppm", "Al_ppm", "pH_H2O", "K_ppm", "Ti_ppm", "Cr_ppm", "Co_ppm", "Ni_ppm", "As_ppm", "Se_ppm", "Cd_ppm", "Pb_ppm", "EC.s_ppm", "oldTrialID", "newPlotID", "treatCode", "treatCode_label", "fieldbookID", "plantingDate", "LGA", "typeTrial", "labID", "sampleID", "yearCollected", "oldTrialCode", "subZoneLGA", "PD_PlantDensity", "WC", "textureClass", "sampleType", "platform", "depth", "entity", "today", "samplingDay", "soilSampleIDnew", "GeoPoint_Lat", "GeoPoint_long", "HHID.Farmer.s.ID", "Trial_type", "zoneState.1", "subZoneLGA.1", "trialPlantingYear", "trialType", "farmersName.1", "cluster.1", "samplingDepth_cm", "plotNum", "yield", "T1_PloughType", "T2_FlatRidge", "FA_FertNil", "SN", "soilSampleIDold", "ancestorID", "treatNr"
make_variable_map <- function(soil_property) {
  # Extend as needed
  prop_map <- c(
    "N_perc" = "N",
    "Meh_P_ppm" = "Pext",
    "sand_perc" = "Sand",
    "clay_perc" = "Clay",
    "silt_perc" = "Silt",
    "P_ppm" = "Ptot",
    "pH" = "pH",
    "OC_perc" = "OC"
  )
  
  if (!soil_property %in% names(prop_map)) {
    stop("Parameter not found in mapping: ", soil_property)
  }
  
  old_name <- paste0(soil_property)
  
  new_name <- paste0(prop_map[[soil_property]], "_sample")
  
  setNames(old_name, new_name)
}



available_zones_with_samples <- function(
    sample_data, Country, adm_level = 1, zone = NULL, 
    study_variables = NULL, drop_cols_if_na = c("all", "any")){
  
  zone_vect <- geodata::gadm(country = Country, level = adm_level,
                             path = ".")
  
  if (!is.null(zone)){
    attrs <- values(zone_vect)
    idx <- which(attrs[[paste0("NAME_", adm_level)]] == zone)
    zone_vect <- zone_vect[idx, ]
    
  }
  
  zone_sf <- zone_vect |>
    st_as_sf()
  if (!is.null(drop_cols_if_na) & !is.null(study_variables)){
    if (drop_cols_if_na == "all") {
      # Drop row if ALL required columns are NA
      sample_data <- sample_data[rowSums(is.na(sample_data[study_variables])) != 
                                   length(study_variables), ]
      message(
      "Soil samples availability after dropping observations for which study 
      variables are ALL NA. Soil depth information may still be missing.")
    } else if (drop_cols_if_na == "any") {
      # Drop row if ANY required column is NA
      sample_data <- sample_data[rowSums(is.na(sample_data[study_variables])) == 0, ]
      message(
      "Soil samples availability after dropping observations for which study
      variables have ANY NA. Soil depth information may still be missing.")
    } 
  } else {
    message(
    "Soil samples availability for all zones without considering study variables
    availability. Soil depth information may still be missing.")
  }
  
  
  samples_sf <- sample_data |> 
    st_as_sf(coords = c("geopoint.Longitude", "geopoint.Latitude"), crs = 4326)
  
  samples_in_zone <- st_intersection(samples_sf, zone_sf)
  
  
  return(table(samples_in_zone$NAME_1))
} 


# TODO: revise assumptions
# Assumptions: 1) remove missing depth observations. 2) 0-50 observations are
# indeed 0-50 depths. 3) GeoPoint columns are corrections of lat/lon columns.
drymatter_samples_preparation <- function(
    sample_data, zone = NULL, adm_level = 1, study_variables, 
    drop_cols_if_na = c("all", "any")) {
  
  to_drop_variables <- c(
    "...1", "plotID", "trialID", "fieldID", "newTrialID", "soilSampleID",
    "sampleBatchID", "motherID", "generation", "AncestorID", "placing", "split", 
    "forward", "soilSampleID_TA", "labName", "typeAnalysis", "countryCollection",
    "zoneState", "useCase", "yearCollection", "farmersName", "cluster",
    "district", "batchCode", "samplingPeriod", "Ca_cmol_per_kg", "Mg_cmol_per_kg",
    "Na_cmol_per_kg", "Exch_Acidity_cmol_per_kg", "Zn_ppm", "Cu_ppm", "Mn_ppm",
    "Fe_ppm", "S_ppm", "Na_ppm", "Mg_ppm", "Hp_ppm", "Ca_ppm", "B_ppm", "Al_ppm",
    "Ti_ppm", "Cr_ppm", "Co_ppm", "Ni_ppm", "As_ppm", "Se_ppm", "Cd_ppm", 
    "Pb_ppm", "oldTrialID", "newPlotID", "treatCode", "treatCode_label",
    "fieldbookID", "LGA", "typeTrial", "labID", "sampleID", "yearCollected", 
    "oldTrialCode", "subZoneLGA", "PD_PlantDensity", "WC", "platform", "entity", 
    "today", "soilSampleIDnew", "HHID.Farmer.s.ID", "Trial_type", "zoneState.1", 
    "subZoneLGA.1", "trialPlantingYear", "trialType", "farmersName.1", 
    "cluster.1", "plotNum", "yield", "T1_PloughType", "T2_FlatRidge", 
    "FA_FertNil", "SN", "soilSampleIDold", "ancestorID", "treatNr", "plantPart",
    "rootYield", "reportAction", "trialCode", "plantingDate", "trialValidity",
    # Potentially useful variables
    "Level", "description", "K_cmol_per_kg", "ECs_ppm", "ECs_ppm", "K_ppm",
    "EC.s_ppm", "textureClass", "sampleType", "samplingDay", "ECEC_cmol_per_kg")
  
  sample_data <- sample_data %>% 
    select(-any_of(to_drop_variables)) %>% 
    rename(longitude = geopoint.Longitude,
           latitude = geopoint.Latitude)
  
  # Replace longitude and latitude with values in GeoPoint columns if not NAs
  sample_data <- sample_data %>%
    mutate(
      longitude = coalesce(GeoPoint_long, longitude),
      latitude  = coalesce(GeoPoint_Lat, latitude)
    ) %>%
    select(-c("GeoPoint_Lat", "GeoPoint_long"))
  
  # Merge pH despite measuring methods differ
  sample_data <- sample_data %>%
    mutate(
      pH_final = coalesce(pH_H2O_1_2.5, pH_H2O, pH)
    ) %>% 
    rename(pH_old = pH,
           pH = pH_final) %>%
    select(-any_of(c("pH_H2O_1_2.5", "pH_H2O", "pH_old")))
  
  depth_vars <- c("soilDepth_cm", "depth", "samplingDepth_cm")
  
  sample_data <- sample_data %>%
    filter(if_any(all_of(depth_vars), ~ !is.na(.x)))  # 943 obs.

  sample_data <- sample_data %>%
    mutate(samplingDepth_cm = gsub(" ", "", samplingDepth_cm),
           depth = gsub("cm", "", depth),
           depth = na_if(depth, "-"),
           soilDepth_cm = gsub("20-50cm", "20-50", soilDepth_cm),
           soilDepth_cm = gsub("0-20cm", "0-20", soilDepth_cm),
           sample_depth = coalesce(samplingDepth_cm, depth, soilDepth_cm)
    ) %>%
    select(-all_of(depth_vars))
  
  sample_data <- sample_data %>%
    mutate(across(all_of(
      sample_study_variables), as.numeric))
  
  if (!is.null(drop_cols_if_na) & !is.null(study_variables)){
    if (drop_cols_if_na == "all") {
      # Drop row if ALL required columns are NA
      sample_data <- sample_data[rowSums(is.na(sample_data[study_variables]))
                                 != length(study_variables), ]
      message(
        "Filtering out observations for which study variables are ALL NA and 
        soil depth is missing.")
    } else if (drop_cols_if_na == "any") {
      # Drop row if ANY required column is NA
      message(
        "Filtering out observations for which study variables have ANY NA and
        soil depth is missing.")
      sample_data <- sample_data[rowSums(is.na(sample_data[study_variables]))
                                 == 0, ]
    } 
  } else {
    message(
      "Not filtering out observations for which study variables have NAs.
      Filtering observations for which soil depth is missing.")
  }
  
  # Rename columns
  mapping <- purrr::map(study_variables, make_variable_map) %>%
    unlist()
  sample_data <- sample_data %>%
    rename(!!!mapping )
  
  ### Keep only samples in study zone
  samples_sf <- sample_data |> 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  zone_vect <- geodata::gadm(country = Country, level = adm_level,
                             path = ".")
  
  if (!is.null(zone)) {
    # Keep observations for lower than country level
    attrs <- values(zone_vect)
    idx <- which(attrs[[paste0("NAME_", adm_level)]] == zone)
    zone_vect <- zone_vect[idx, ]

    zone_sf <- zone_vect |>
      st_as_sf()
    
    samples_in_zone <- st_intersection(samples_sf, zone_sf)
  } else {
    # Keep observations at country level
    zone_sf <- zone_vect |>
      st_as_sf()
    
    samples_in_zone <- st_intersection(samples_sf, zone_sf)
  }
  
  n_points <- min(table(samples_in_zone$sample_depth)[c("0-20", "20-50")])
  
  # message("Unique lat-lon pairs: ", length(unique(samples_in_zone[!duplicated(st_coordinates(samples_in_zone)), ])))
  
  title_text <- str_wrap(
    paste(
      n_points, "available points in", paste0(Country, "."),
      "No NA depth. Dropped observations if", drop_cols_if_na,
      "study cols are NA."
    ),
    width = 60
  )
  
  p <- ggplot() +
    geom_sf(data = zone_sf, fill = "white", color = "black") +
    geom_sf(data = samples_in_zone, size = 2) + 
    theme_minimal() +
    labs(title = title_text) 
  print(p)
  
  return(samples_in_zone)
}

