# OLD LOGIC FROM DSSAT_expfile.R


# TODO: This seems unnecessary or unnecesssarily complex
if(AOI == TRUE) {
  if(is.null(Planting_month_date) | is.null(Harvest_month_date)){
    print("with AOI=TRUE, Planting_month_date, Harvest_month_date can not be null, please provide mm-dd for both")
    return(NULL)
  }
  countryCoord <- readRDS(
    paste0(project_root, "/Data/useCase_", country, "_", useCaseName, "/",
           Crop, "/data_curation/", country, "/AOI_GPS.RDS"))
  if (!Forecast) {
    fc_year = 2000  # placeholder
  }
  countryCoord <- unique(countryCoord[, c("lon", "lat")])
  countryCoord <- countryCoord[complete.cases(countryCoord), ]
  
  Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
  Harvest_month  <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
  if(Planting_month < Harvest_month) {py <- fc_year; hy <- fc_year} else {py <- fc_year; hy <- fc_year + 1}
  
  Planting_month_date <- as.Date(paste0(py, "-", Planting_month_date))
  countryCoord$plantingDate <- Planting_month_date
  Planting_month_date <- Planting_month_date %m-% months(1)
  
  if(Crop == "Cassava"){
    duration <- as.Date(paste0(hy, "-",Harvest_month_date)) - Planting_month_date
    if (duration < 240) hy <- hy + 1
  }
  Harvest_month_date <- as.Date(paste0(hy, "-",Harvest_month_date))
  countryCoord$harvestDate  <- Harvest_month_date
  if(plantingWindow > 1 & plantingWindow <= 5){
    Harvest_month_date <- Harvest_month_date %m+% months(1)
  }else if(plantingWindow > 5 & plantingWindow <=30){
    Harvest_month_date <- Harvest_month_date %m+% months(2)
  }
  countryCoord$startingDate <- Planting_month_date
  countryCoord$endDate      <- Harvest_month_date
  
  countryCoord <- countryCoord[complete.cases(countryCoord), ]
  names(countryCoord) <- c("longitude", "latitude","plantingDate","harvestDate","startingDate","endDate")
  ground <- countryCoord
} else {
  # Remains unchanged
  GPS_fieldData <- readRDS(paste("/home/jovyan/agwise-datacuration/dataops/datacuration/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/compiled_fieldData.RDS", sep=""))
  countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate")])
  countryCoord <- countryCoord[complete.cases(countryCoord), ]
  countryCoord$startingDate <- as.Date(countryCoord$plantingDate, "%Y-%m-%d") %m-% months(1)
  names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate","startingDate")
  ground <- countryCoord
}

Soil <- readRDS(paste0("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",
                       useCaseName, "/", Crop, "/result/geo_4cropModel/", zone, "/ISDA_SoilDEM_PointData_AOI_profile.RDS"))

# ---- datasourcing paths ----
# TODO: No need to read from datasourcing?
if (Forecast) {
  general_pathIn <- paste0(project_root, "/Data/useCase_", country, "_", useCaseName,"/", Crop, "/transform/FC")
} else if (!Forecast) {
  general_pathIn <- paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_", useCaseName,"/", Crop, "/result/geo_4cropModel")
}

if (pathIn_zone == TRUE) {
  if(!is.na(level2) & !is.na(zone)){
    pathIn <- paste(general_pathIn,paste0(zone,'/',level2), sep = "/")
  }else if(is.na(level2) & !is.na(zone)){
    pathIn <- paste(general_pathIn,zone, sep = "/")
  }else if(!is.na(level2) & is.na(zone)){
    print("You need to define first a zone (administrative level 1) to be able to get data for level 2 in datasourcing. Process stopped")
    return(NULL)
  }else{
    pathIn <- general_pathIn
  }
}else{
  pathIn <- general_pathIn
}

if(AOI == TRUE) {
  if (Forecast) {
    Rainfall <- readRDS(paste0(pathIn, "/FC_", fc_month, "-", fc_year, "_Rainfall_Season_", season, "_PointData_AOI.RDS"))
    Soil <- readRDS(paste0(pathIn, "/SoilDEM_PointData_AOI_profile.RDS"))
  } else if (!Forecast) {
    Rainfall <- readRDS(paste0(pathIn, "/Rainfall_Season_", season, "_PointData_AOI.RDS"))
    Soil <- readRDS(paste0(pathIn,"/SoilDEM_PointData_AOI_profile.RDS"))
  }
} else {
  Rainfall <- readRDS(paste0(pathIn, "Rainfall_PointData_trial.RDS"))
  Soil     <- readRDS(paste(pathIn, "SoilDEM_PointData_trial_profile.RDS", sep=""))
}

names(Soil)[names(Soil)=="lat"] <- "latitude"
names(Soil)[names(Soil)=="lon"] <- "longitude"
Soil <- na.omit(Soil)

if ("Zone" %in% names(Rainfall)){ names(Rainfall)[names(Rainfall)=="Zone"] <- "NAME_1"}
if ("lat"  %in% names(Rainfall)){ names(Rainfall)[names(Rainfall)=="lat"]  <- "latitude"}
if ("lon"  %in% names(Rainfall)){ names(Rainfall)[names(Rainfall)=="lon"]  <- "longitude"}

if(AOI == TRUE){
  metaDataWeather <- as.data.frame(Rainfall[,c("longitude", 'latitude', "startingDate", "endDate", "NAME_1", "NAME_2")])
}else{
  metaDataWeather <- as.data.frame(Rainfall[,c("longitude", 'latitude', "startingDate", "endDate", "NAME_1", "NAME_2",
                                               "yearPi","yearHi","pl_j","hv_j")])
}
metaData_Soil <- Soil[,c("longitude", "latitude","NAME_1","NAME_2")]
metaData <- merge(metaDataWeather,metaData_Soil)

# ---- years span (AOI) ----
if(AOI == TRUE) {
  R1 <- Rainfall[1, ]
  if ("country" %in% names(R1)) {R1<- subset(R1, select = -country)}
  if ("ID" %in% names(R1)) {R1<- subset(R1, select = -ID)}
  R1 <- pivot_longer(R1,
                     cols=-c("longitude", "latitude","NAME_1","NAME_2","startingDate", "endDate"),
                     names_to = c("Variable", "Date"),
                     names_sep = "_",
                     values_to = "RAIN")
  number_years <- max(lubridate::year(as.Date(R1$Date, "%Y-%m-%d"))) -
    min(lubridate::year(as.Date(R1$Date, "%Y-%m-%d")))
  if (number_years == 0 ) {
    number_years <- 1
  }
} else {
  number_years <- 1
}

metaData <- unique(metaData[,c("longitude", "latitude","NAME_1","NAME_2")])
coords <- merge(metaData,ground)

if(!is.na(zone)){coords <- coords[coords$NAME_1==zone,] }
if(!is.na(level2)){ coords <- coords[coords$NAME_2==level2,] }

# ----------------------------------------------------------------------
# Attach RS planting schedule with robust join: rounded lon/lat (3 dp)
# ----------------------------------------------------------------------
# if (!is.null(rs_schedule_df) & !create_RS_schedule) {
#   rs_clean <- rs_schedule_df %>%
#     {
#       nm <- names(.)
#       if ("lon" %in% nm) rename(., longitude = lon) else .
#     } %>%
#     {
#       nm <- names(.)
#       if ("lat" %in% nm) rename(., latitude  = lat) else .
#     } %>%
#     mutate(
#       longitude = as.numeric(longitude),
#       latitude  = as.numeric(latitude),
#       lon_r = if ("lon_r" %in% names(.)) lon_r else round(longitude, 3),
#       lat_r = if ("lat_r" %in% names(.)) lat_r else round(latitude, 3)
#     ) %>%
#     select(lon_r, lat_r, planting_dates, startingDate, harvestDate)
#   
#   coords <- coords %>%
#     mutate(
#       longitude = as.numeric(longitude),
#       latitude  = as.numeric(latitude),
#       lon_r = round(longitude, 3),
#       lat_r = round(latitude, 3)
#     ) %>%
#     dplyr::left_join(rs_clean, by = c("lon_r","lat_r"), suffix = c("", ".rs")) %>%
#     mutate(
#       startingDate = ifelse(!purrr::map_lgl(planting_dates, ~is.null(.x) || all(is.na(.x))),
#                             as.Date(startingDate.rs), as.Date(startingDate)),
#       harvestDate  = ifelse(!purrr::map_lgl(planting_dates, ~is.null(.x) || all(is.na(.x))),
#                             as.Date(harvestDate.rs),  as.Date(harvestDate))
#     )
# } else if (is.null(rs_schedule_df) & !create_RS_schedule) {
#   coords$planting_dates <- replicate(nrow(coords), as.Date(NA), simplify = FALSE)
# }

grid <- as.matrix(coords)
if (nrow(coords) == 0) {
  print("No coordinates to process after filtering. Exiting.")
  return(NULL)
}