# This script compares soil variables from ISDA, ISRIC and optionally sample data

project_root <- "/home/jovyan/Alvaro_repos/agwise-dssat-workflow/"
source(paste0(project_root, "Scripts/generic/DSSAT/common_helpers.R"))
source(paste0(project_root, "Scripts/generic/analysis/helpers_soil_analysis.R"))
source(paste0(project_root, "Data/useCase_Kenya_SoilComparison/Maize/DSSAT/Landing/soil_analysis_config.R"))

########################
### Read Sample Data ###
########################
### Insert here functions to prepare and process Sample data

###################################
### Read and prepare ISRIC data ###
###################################
### Read and process ISRIC (P extrapolation, Olsen conversion [optional]) files
isric_df <- process_ISRIC_data(
  soil_var_map, depth_map, project_root, Country, useCaseName, 
  adm_level = adm_level, zones = zones,
  Olsen_conversion = Olsen_conversion, force_reanalysis = T)

### Convert interpolate ISRIC depths to ISDA depths
isric_df_canon_depths <- interpolate_isric_depth_isda(
  isric_df, soil_var_map, depth_map)

### Convert ISRIC units
isric_df_transf <- apply_soil_transformations(
  isric_df_canon_depths, soil_var_map, depth_map, source = "ISRIC")


##################################
### Read and prepare ISDA data ###
##################################
### Read and process (Olsen conversion [optional]) ISDA files
isda_df <- process_ISDA_data(
  soil_var_map, depth_map, project_root, Country, useCaseName, 
  adm_level = adm_level, zones = zones,
  Olsen_conversion = Olsen_conversion
)

### Convert ISDA units
isda_df_transf <- apply_soil_transformations(
  isda_df, soil_var_map, depth_map, source = "ISDA")


############################
### Merge ISDA and ISRIC ###
############################
merged_dfs <- merge_by_coords(
  dfs = list(isric_df_transf, isda_df_transf), keep_extra = F, project_root, 
  Country, useCaseName, adm_level, zones)

colnames(merged_dfs)
summary(merged_dfs)

#######################
### Simple cleaning ###
#######################
# Dropping one location that is probably an error
merged_dfs <- merged_dfs %>%
  filter(`ISRIC Sand (%) 0-20 cm` != 0)


#########################
### Figures and stats ###
#########################
### Statistics table
vars <- soil_var_map$canonical
depths <- depth_map$canonical

all_stats <- purrr::map_dfr(vars, function(v) {
  purrr::map_dfr(depths, function(d) {
    compare_stats(merged_dfs, v, d, Country, adm_level, project_root,
                  useCaseName)
  })
})

all_stats <- all_stats %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

save_stats(all_stats, project_root, Country, useCaseName, adm_level)

### Make plots
for (var in vars) {
  for (depth in depths) {
    plot_diff_map(merged_dfs, var, depth, Country, zones, adm_level, 
                  project_root, useCaseName)
    plot_hexbin(merged_dfs, var, depth, Country, zones, adm_level, 
                project_root, useCaseName)
    plot_scatter(merged_dfs, var, depth, add_loess = F, Country, zones, adm_level, 
                 project_root, useCaseName)
  }
}

