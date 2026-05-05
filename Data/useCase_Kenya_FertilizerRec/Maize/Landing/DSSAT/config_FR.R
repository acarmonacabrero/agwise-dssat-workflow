### Load common helper functions
source(paste0(project_root, '/Scripts/generic/DSSAT/common_helpers.R'))

################
### Settings ###
################
# 1.a. General Settings (edit as necessary)
country <- "Kenya"
useCaseName <- "FertilizerRec"
Crop <- "Maize"
varietyids <- c("999993")

# 1.b. Paths
filex_temp <- "KEAG8104.MZX"
temp_file <- "Fertilizer_recommendation_template_V3.csv"  # Input file (.csv)
geneticfiles <- "MZCER048"

path.to.temdata <- paste0(
  project_root, "/Data/useCase_", country, "_",
  useCaseName, "/", Crop, "/Landing/DSSAT/")

# 1.c. Experimental settings
Soil_source <- "ISRIC"  # or Soil_source <- "ISDA"
AOI <- T
Forecast <- F
fertilizer <- F  # All treatments have one fertilizer level defined in the DSSAT template
fert_factorial <- T  # Fertilizer recommendation and plant date from CSV file
fert_grid_RS <- F  # Fertilizer from NPK grid and plant dates from CSV file

season <- 1
pathIn_zone <- T
level2 <- NA
index_soilwat <- 1
ID <- "TLID"

# 1.e. Read inputs
template_df <- read.csv(paste0(path.to.temdata, temp_file))
provinces <- unique(template_df$NAME_1)

# 1.f. Other inputs
Planting_month_date <- "08-01"; Harvest_month_date <- "06-30"
plantingWindow <- 4  # ignored when rs_schedule_df is provided


# 2. Variable assignment based on inputs
Depth <- if(Soil_source == "ISRIC") c(5, 15, 30, 60, 100, 200) else 
  if(Soil_source == "ISDA") c("0-20cm", "20-50cm")

# Create and load AOI_GPS.RDS file
inputData <- load_or_generate_inputData(
  country = country, useCaseName = useCaseName, Crop = Crop, 
  project_root = project_root, inputData = NULL)

