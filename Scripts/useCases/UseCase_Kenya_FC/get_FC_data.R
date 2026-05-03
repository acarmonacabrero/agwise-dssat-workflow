# Download BC Forecast data
################
### Settings ###
################
# 1.a. General Settings (edit as necessary)
project_root <- "/home/jovyan/Alvaro_repos/agwise-dssat-workflow"
country <- "Kenya"
useCaseName <- "FC"
Crop <- "Maize"

source(paste0(project_root, '/Scripts/generic/DSSAT/common_helpers.R'))
source(paste0(project_root, "/Scripts/generic/DSSAT/readGeo_CM_zone.R"))


# 1.b. Paths
# Input file (.csv) for fertilizer, planting dates, varieties, ...
temp_file <- "planting_date_rec_template.csv"

path.to.temdata <- paste0(
  project_root, "/Data/useCase_", country, "_",
  useCaseName, "/", Crop, "/Landing/DSSAT/")

# 1.c. Experimental settings
AOI <- T
Forecast <- T

season <- 1
pathIn_zone <- T
level2 <- NA
index_soilwat <- 1
ID <- "TLID"

# 1.d. Forecast inputs
country_code <- countrycode(country, 
                            origin = "country.name", destination = "iso3c")
init_month_user <- 10  # fc_month
season_length_months <- 4  # 
forecast_year <- 2025  # fc_year


# TODO: make below initialization into a function in the mother script
# 1.e. Read inputs
template_df <- read.csv(paste0(path.to.temdata, temp_file))
provinces <- unique(template_df$NAME_1)
varietyids <- unique(template_df$INGENO)

zone <- "Kisumu"

# Create and load AOI_GPS.RDS file
inputData <- load_or_generate_inputData(
  country = country, useCaseName = useCaseName, Crop = Crop, 
  project_root = project_root, inputData = NULL)

get_bc_forecast_data(
  project_root, country, useCaseName, Crop, zone, country_code, 
  init_month_user = init_month_user, season_length_months = season_length_months,
  forecast_year = forecast_year)


