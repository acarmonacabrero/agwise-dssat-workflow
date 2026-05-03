# -------------------------------------------------- #
### CROP MODELLING FERTILIZER GRID & RS PDs SCRIPT ###
# --------------------------------------=====------- #
project_root <- "/home/jovyan/Alvaro_repos/agwise-dssat-workflow"

### Load user-defined inputs AND load helper functions:
source(paste0(project_root, '/Data/useCase_Kenya_FertilizerRec/Maize/Landing/DSSAT/config_FR.R'))


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

TRT <- get_n_treatments(
  template_df, Forecast, fertilizer, fert_factorial, fert_grid_RS)

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


###################################
### Step 4: Merge DSSAT results ###
###################################
source(paste0(project_root, "/Scripts/generic/DSSAT/merge_DSSAT_output.R"))

### Merge DSSAT outputs ###
all_results <- merge_DSSAT_output(
  country = country, useCaseName = useCaseName, Crop = Crop, 
  project_root = project_root, Soil_source = Soil_source, AOI = AOI,
  season = season, varietyids = varietyids, zone_folder = T, level2_folder = F)


#####################################################
### Step 4: Produce response statistics and plots ###
#####################################################
source(paste0(project_root, "/Scripts/generic/DSSAT/DSSAT_analyze_results.R"))

# data to plot and summarize
pixel_number <- 1
lat_pixel <- all_results$XLAT[pixel_number]
lon_pixel <- all_results$LONG[pixel_number]
map_year <- 2018
variable <- "HWAH"
treatment_to_plot <- "1st fert. 1st pd"

summary_DSSAT_results(all_results, variable = HWAH, by_treatment = T)
plot_DSSAT_pixel(all_results, lat = lat_pixel, lon = lon_pixel, variable = HWAH)
plot_aggregate(all_results, variable = HWAH)
plot_map(
  all_results, treatment = treatment_to_plot, year_selected = map_year,
  variable = HWAH)

