# This script prepares measured, SoilGrids and ISDA data

# TODO: Be sure units from different data sets are the same

#############################################################
### COMMON SETTINGS FOR SOIL SAMPLES, SOIL GRIDS AND ISDA ###
#############################################################
Country <- "Nigeria"
useCaseName <- "Example"
zone <- NULL
adm_level <- 1

# data <- NULL  # Use it to run without sample soils
# 
# zone_vect <- geodata::gadm(country = Country, level = adm_level,
#                            path = ".")

########################
### SOIL SAMPLE DATA ###
########################
source('/home/jovyan/rs-soil-comparison-africa/Scripts/generic/analysis/drymatter_samples_preparation.R')

# Load sample data
sample_data <- read_csv('/home/jovyan/rs-soil-comparison-africa/Data/soil_dataset/obs_soils_dry_matter.csv')

sample_study_variables <- c("N_perc", "sand_perc", "silt_perc", "clay_perc",
                            "Meh_P_ppm","P_ppm", "pH", "OC_perc")  # "ECEC_cmol_per_kg", )

# Print soil samples availability in country/zone
available_zones_with_samples(
  sample_data = sample_data, Country = Country, adm_level = adm_level,
  zone = zone, study_variables = sample_study_variables, 
  drop_cols_if_na = "all")


data <- drymatter_samples_preparation(
  sample_data = sample_data, zone = zone, adm_level = adm_level, 
  study_variables = sample_study_variables, drop_cols_if_na = "all")

##############################################
### SOIL GRIDS DATA AND RASTER PREPARATION ###
##############################################

source("/home/jovyan/rs-soil-comparison-africa/Scripts/generic/analysis/SoilGrids_preparation.R")
source("/home/jovyan/rs-soil-comparison-africa/Scripts/generic/analysis/merge_SoilGrids_data.R")

sg_soil_properties <- c(
  "sand", "silt", "clay", "nitrogen", "af_p", "af_ptot", "phh2o", "soc", "bdod")

# sg_soil_properties <- c("nitrogen", "bdod", "cec", "cfvo", "clay", "ocd", #"ocs",
#                         "phh2o", "sand", "silt", "soc", "p_ext", "p_tot")
# soil_properties <- c("p_ext", "p_tot")
sg_soil_depths <- c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200")

force_intersect <- F  # Set to TRUE to redo tile search and file processing

df_list <- list()
raster_list <- list()
# Loop here for SoilGrids
for (soil_property in sg_soil_properties){
  for (soil_depth in sg_soil_depths){
    sg <- SoilGrids_preparation(
      Country = Country, useCaseName = useCaseName, 
      soil_property = soil_property, soil_depth = soil_depth, 
      adm_level = adm_level, zone = zone, force_intersect = force_intersect, 
      sample_data = data,  # Set to NULL if not running point data
      tiles_path = "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/soilGrids/profile"
      # tiles_path = "/home/jovyan/common_data/soilgrids/raw"
      )

    df_list <- c(df_list, list(sg$df))
    raster_list <- c(raster_list, list(sg$raster))
  }
}

merged_sg <- merge_SoilGrids_data(df_list = df_list, raster_list = raster_list, 
                                  Country, zone = zone, 
                                  adm_level = adm_level, sample_data = data)

sg_data <- merged_sg$df

rm(df_list, raster_list, merged_sg)

########################################
### ISDA DATA AND RASTER PREPARATION ###
########################################
source("/home/jovyan/rs-soil-comparison-africa/Scripts/generic/analysis/ISDA_preparation.R")
source("/home/jovyan/rs-soil-comparison-africa/Scripts/generic/analysis/merge_ISDA_data.R")

isda_soil_properties <- c(
  "sand_tot_psa", "silt_tot_psa", "clay_tot_psa", "log.n_tot_ncs", "db_od", 
  "log.p_mehlich3", "ph_h2o", "log.oc")

# isda_soil_properties <- c(
#   "clay_tot_psa", "sand_tot_psa", "silt_tot_psa", "db_od", "ph_h2o", "log.oc", 
#   "log.n_tot_ncs", "log.p_mehlich3", "log.k_mehlich3", "log.ecec.f")  # Other variables not added

isda_soil_depths <- c("0..20", "20..50")

force_intersect <- F  # Set to TRUE to redo tile search and file processing

df_list <- list()
raster_list <- list()
# Loop here for ISDA
for (soil_property in isda_soil_properties){
  for (soil_depth in isda_soil_depths){
    isda <- ISDA_preparation(
      Country = Country, useCaseName = useCaseName,
      soil_property = soil_property, soil_depth = soil_depth, 
      adm_level = adm_level, zone = zone, force_intersect = force_intersect, 
      sample_data = data, isda_folder = "/home/jovyan/common_data/isda/raw"
    )
    
    df_list <- c(df_list, list(isda$df))
    raster_list <- c(raster_list, list(isda$raster))
  }
}

merged_isda <- merge_ISDA_data(df_list = df_list, raster_list = raster_list, 
                               zone = zone, adm_level = adm_level, 
                               sample_data = data)

isda_data <- merged_isda$df

rm(df_list, raster_list, merged_isda)

###############################
## MATCH DATA SETS AND UNITS ##
###############################
source('/home/jovyan/rs-soil-comparison-africa/Scripts/generic/analysis/match_all_data.R')

all_data <- match_all_data(
  sg_data = sg_data, isda_data = isda_data, sample_data = data, 
  Country = Country, adm_level = adm_level, zone = zone)
