# TODO: Save message files
# ----------------------------------------- #
### CROP MODELLING SOIL COMPARISON SCRIPT ###
# ----------------------------------------- #
project_root <- "/home/jovyan/rs-soil-comparison-africa"

### Load user-defined inputs AND load helper functions:
source(paste0(project_root, '/Data/useCase_Kenya_Example/Maize/Landing/DSSAT/config.R'))


#########################################################################
### Step 1: Create weather and soil data in DSSAT format for AOI data ###
#########################################################################
source(paste0(project_root, "/Scripts/generic/DSSAT/readGeo_CM_zone.R"))


# Check for existence of ISDA data. If missing run script to get the data
check_and_get_ISDA_RDS(country, useCaseName, Crop, project_root,
                       inputData = NULL)


for (prov in provinces) {
  # Create DSSAT .WTH and .SOL files
  wth_sol_files_msg <- readGeo_CM_zone(
    country = country, useCaseName = useCaseName, Crop = Crop,
    project_root = project_root, AOI = AOI, season = season, zone = prov,
    level2 = level2, varietyid = varietyids[1], pathIn_zone = pathIn_zone,
    Depth = Depth
  )
  message <- paste("Province finished:", prov, Sys.time())
}
plan(sequential)
write_dssat_log(wth_sol_files_msg, file = "readGeo_CM_zone.log")

if (length(varietyids) > 1) {
  copy_WTH_SOIL_data_for_variety(
    country = country, useCaseName = useCaseName, Crop = Crop, 
    project_root = project_root, AOI = AOI, varietyids = varietyids)
}


##################################################################
### Step 2: Create DSSAT input files and run DSSAT simulations ###
##################################################################
source(paste0(project_root, "/Scripts/generic/DSSAT/DSSAT_expfile.R"))

### Create DSSAT input files ###
for (varietyid in varietyids) {
  for (prov in provinces) {
    expfile_msg <- invisible(
      dssat.expfile(
        country = country, useCaseName = useCaseName, Crop = Crop, 
        project_root = project_root, AOI = AOI, filex_temp = filex_temp,
        # month-day placeholders; real planting dates come from rs_schedule_df below
        Planting_month_date = Planting_month_date,
        Harvest_month_date  = Harvest_month_date,
        ID = ID, season = season,
        plantingWindow = plantingWindow,  # ignored when rs_schedule_df is provided
        varietyid = varietyid,
        zone = prov, level2 = level2, fertilizer = fertilizer,
        fert_factorial = fert_factorial, template_df = template_df,
        fert_grid_RS = fert_grid_RS, NPK_ranges = NULL,
        geneticfiles = geneticfiles, index_soilwat = index_soilwat,
        pathIn_zone = pathIn_zone, Forecast = Forecast, create_RS_schedule = T,
        fc_month = NA, fc_year = NA
      )
    )
  }
}
plan(sequential)
write_dssat_log(expfile_msg, file = "dssat.expfile.log")

### Run DSSAT Simulations ###
source(paste0(project_root, "/Scripts/generic/DSSAT/dssat_exec.R"))
TRT <- 1:(ncol(template_df) - 5) *  # Number of planting dates defined by the template
  length(varietyids)  # Number of varieties 

for (varietyid in varietyids) {
  for (prov in provinces) {
    exemodel_msg <- dssat.exec(country = country,  useCaseName = useCaseName, 
                               Crop = Crop, project_root = project_root, 
                               AOI = AOI, TRT = TRT, varietyid = varietyid, 
                               zone = prov)
  }
}
plan(sequential)
write_dssat_log(exemodel_msg, file = "dssat.exec.log")

##########################################################################
### Step 4: Merge DSSAT results, produce response statistics and plots ###
##########################################################################
source(paste0(project_root, "/Scripts/generic/DSSAT/merge_DSSAT_output.R"))

### Merge DSSAT outputs ###
merge_DSSAT_output(country = country, useCaseName = useCaseName, Crop = Crop,
                   project_root = project_root, Soil_source = Soil_source,
                   AOI = T, season = season, varietyids = varietyids,
                   zone_folder = T, level2_folder = F)

### Produce response statistics and plots ###
source(paste0(project_root, "/Scripts/generic/DSSAT/soil_comparison_stats.R"))

soil_comparison_data <- read_isric_isda_data(project_root, country, useCaseName,
                                             Crop, season, AOI)

common_pixels <- get_common_pixels(soil_comparison_data$ISRIC,
                                   soil_comparison_data$ISDA)

# data to plot and summarize
pixel_number <- 1
map_year <- 2018
variable <- "HWAH"

results <- run_full_soil_comparison(
  project_root = project_root,
  country = country,
  useCaseName = useCaseName,
  Crop = Crop,
  AOI = AOI,
  season = season,
  variable = variable,
  lat = common_pixels$Lat[pixel_number],
  lon = common_pixels$Long[pixel_number],
  map_year = map_year
)

results$overall_metrics
results$yearly_metrics
results$timeseries_plot
results$scatter_plot
results$spatial_map
