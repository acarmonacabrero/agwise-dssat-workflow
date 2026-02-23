# Extracts the geospatial data required for running DSSAT from Forecast, 
# historical and soil files


# TODO: Remove this function once not necessary due to folder reordering
country_map <- function(country_name) {
  country_lookup <- c(
    Kenya = "KEN",
    Rwanda = "RWA",
    Ethiopia = "ETH"
  )
  
  unname(country_lookup[country_name])
}


# Reads the forecast files
read_and_process_forecast <- function(
    fc_path, forecast_year, var_name, inputData, pathOut) {
  
  map <- c(
    Rainfall = "PRCP",
    TemperatureMax = "TMAX",
    TemperatureMin = "TMIN",
    SolarRadiation = "SRAD"
  )
  
  code <- map[[var_name]]
  file <- list.files(
    fc_path,
    pattern = paste0("^", code, ".*", forecast_year, ".*\\.nc$"),
    full.names = TRUE
  )
  
  r <- terra::rast(file)
  
  xy <- inputData %>%
    select(c(longitude, latitude))
  r_ext <- terra::extract(r, xy, method = 'simple', cells = F) %>%
    mutate(ID = NULL)
  
  if (var_name %in% c("temperatureMax","temperatureMin")) {
   r_ext <- r_ext - 273.3  # K to C
  } else if (var_name == "solarRadiation") {
    r_ext <- r_ext / 1000000  # J/m2 to MJ/m2
  }
  
  rt <- time(r)
  names(r_ext) <- paste(var_name, rt, sep = '_')
  
  data_points <- tibble(cbind(inputData, r_ext))
  
  
  # TODO: get historical data to append to the RDS if necessary
  # if ((fc_start_date - days(30)) < planting_date) {
  #   
  # }
  # TODO

  
  data_name <- paste0("FC_", init_month_user, "-", forecast_year, "_", var_name,
                      "_Season_", season, "_PointData_AOI.RDS")
  saveRDS(data_points, paste0(pathOut, data_name))
  
  list(
    data = data_points,
    var_code = code
  )
}

#####
#' @description a function to estimate P at different depths
#'
#' @param k = decay coefficient (controls how fast P decreases)
#' @param z = depth in cm
#' @param P_mean_0_30 = your measured mean P from 0–30 cm
#'
#' @return
#' @export
#'
#' @examples
#####
# TODO: Revise this equation
extrapolate_P <- function(P_mean_0_30, z, k){
  A <- P_mean_0_30 * (30 * k) / (1 - exp(-30 * k))
  P <- A * exp(-k * z)
  return(P)
}


# TODO: Revise these equations
mehlich3_to_olsen <- function(mehlich3_P){
  # Equations from: https://www.nature.com/articles/s41597-023-02022-4
  soil_calcareous <- FALSE
  # TODO: add logic for calcareous or soil pH
  if (!soil_calcareous) olsen_P <- 0.47 * mehlich3_P + 2.4
  if (soil_calcareous) olsen_P <- 0.41 * mehlich3_P + 1.1
  return(olsen_P)
}


#####
#' @description a function to be used to define path, input data (GPS files), geospatial layers
#'
#' @param country 
#' @param useCaseName 
#' @param Crop 
#' @param inputData 
#' @param Planting_month_date 
#' @param Harvest_month_date 
#' @param varName 
#' @param soilProfile true of false based on if teh user need to have soil data or not
#' @param AOI 
#' @param pathOut1 the path towrite out the sourced data 
#'
#' @return
#' @export
#'
#' @examples
#####
Paths_Vars <- function(
    country, useCaseName, Crop, inputData = NULL, Planting_month_date, 
    Harvest_month_date, varName, soilProfile =TRUE, AOI = TRUE, pathOut = NULL) {
  
  if(country=="Honduras"){
    varsbasePath <- "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Honduras/"
    varsbasePathSoil <- "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/"
  }else{
    varsbasePath <- "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/"
  }
  dataPath <- "~/agwise-datacuration/dataops/datacuration/Data/useCase_"
  OutputPath <- "~/agwise-datasourcing/dataops/datasourcing/Data/useCase_"
  
  readLayers_soil_isric <- NULL
  shapefileHC <- NULL
  
  if(is.null(inputData)){
    if(AOI == TRUE){
      inputData <- readRDS(paste(dataPath,country, "_", useCaseName,"/", Crop, "/result/AOI_GPS.RDS", sep=""))
    }else{
      inputData <- readRDS(paste(dataPath,country, "_", useCaseName,"/", Crop, "/result/compiled_fieldData.RDS", sep=""))
    }
  }
  
  listRasterRF <-list.files(path=paste0(varsbasePath, "Rainfall/chirps"), pattern=".nc$", full.names = TRUE)[-c(1:2)]
  listRasterTmax <-list.files(path=paste0(varsbasePath, "TemperatureMax/AgEra"), pattern=".nc$", full.names = TRUE)
  listRasterTMin <-list.files(path=paste0(varsbasePath, "TemperatureMin/AgEra"), pattern=".nc$", full.names = TRUE)
  listRasterRH <-list.files(path=paste0(varsbasePath, "RelativeHumidity/AgEra"), pattern=".nc$", full.names = TRUE)
  listRasterSR <-list.files(path=paste0(varsbasePath, "SolarRadiation/AgEra"), pattern=".nc$", full.names = TRUE)
  listRasterWS <-list.files(path=paste0(varsbasePath, "WindSpeed/AgEra"), pattern=".nc$", full.names = TRUE)
  
  if(soilProfile == TRUE){
    if(country=="Honduras"){
      listRaster_soil <-list.files(path=paste0(varsbasePathSoil, "Soil/soilGrids/profile/World"), pattern=".tif$")
      readLayers_soil <- terra::rast(paste(paste0(varsbasePathSoil, "Soil/soilGrids/profile/World"), listRaster_soil, sep="/"))
      shapefileHC <- st_read(paste0(varsbasePathSoil, "Soil/HC27/HC27 CLASSES.shp"), quiet= TRUE)%>%
        st_make_valid()
    }else{
      listRaster_soil <-list.files(path=paste0(varsbasePath, "Soil/soilGrids/profile"), pattern=".tif$")
      listRaster_soil_P <-list.files(path=paste0(varsbasePath, "Soil/soilGrids"), pattern="p.*\\.tif$")
      readLayers_soil <- terra::rast(paste(paste0(varsbasePath, "Soil/soilGrids/profile"), listRaster_soil, sep="/"))
      readLayers_soil_P <- NULL
      try(readLayers_soil_P <- terra::rast(paste(paste0(varsbasePath, "Soil/soilGrids"), listRaster_soil_P, sep="/")))
      shapefileHC <- st_read(paste0(varsbasePath, "Soil/HC27/HC27 CLASSES.shp"), quiet= TRUE)%>%
        st_make_valid() 
    }
    
    if(is.null(pathOut)){
      pathOut <- paste(OutputPath, country, "_", useCaseName,"/", Crop, "/result/geo_4cropModel/", sep="")
    }
  }else{
    listRaster_soil <-list.files(path=paste0(varsbasePath, "Soil/iSDA"), pattern=".tif$")
    readLayers_soil <- terra::rast(paste(paste0(varsbasePath, "Soil/iSDA"), listRaster_soil, sep="/"))
    listRaster_soil_isric <-list.files(path=paste0(varsbasePath, "Soil/soilGrids"), pattern=".tif$")
    readLayers_soil_isric <- terra::rast(paste(paste0(varsbasePath, "Soil/soilGrids"), listRaster_soil_isric, sep="/"))
    if(is.null(pathOut)){
      pathOut <- paste(OutputPath, country, "_", useCaseName,"/", Crop, "/result/geo_4ML/", sep="")
    }
  }
  return(list(inputData, listRasterRF, listRasterTmax, listRasterTMin, listRasterRH,listRasterSR, listRasterWS, readLayers_soil, readLayers_soil_isric, shapefileHC, pathOut,readLayers_soil_P))
}

#####
# https://rdrr.io/cran/geodata/man/soil_grids.html
# https://rdrr.io/cran/geodata/man/soil_af.html
# https://rdrr.io/cran/geodata/man/soil_af_isda.html
# https://rdrr.io/cran/geodata/man/elevation.html DEm data from SRTM
#' @description Is a helper function for extract_geoSpatialPointData. Extract geo-spatial data with no temporal dimension, i,e,. soil properties and topography variables
#' 
#' @param country country name to be sued to extract the first two level of administrative units to attach to the data. 
#' @param inputData is a data frame and must have the c(lat, lon) 
#' @param profile is true/false, if true data, isirc data for the six soil profiles will be processed. This is required for DSSAT and other crop models. 
#' @param pathOut is path used to download the DEM layers temporarily and these layers can be removed after obtaining the data from this function. 
#' 
#' 
#' @return a data frame with lon, lat,teh top two admistrnative zones, soil properties with columns named with variable names attached with depth,  
#' elevations variables attached for every GPS location 
#' @examples: get_soil_DEM_pointData(country = "Rwanda", profile = FALSE, pathOut = getwd(),
#' inputData = data.frame(lon=c(29.35667, 29.36788), lat=c(-1.534350, -1.538792)))
#####
get_soil_DEM_pointData <- function(
    country, inputData, soilProfile = F, pathOut, Layers_soil = Layers_soil, 
    Layers_soil_isric = Layers_soil_isric, shapefileHC = shapefileHC,
    Layers_soil_P = NULL){
  
  
  ## 2. read the shape file of the country and crop the global data
  countryShp <- geodata::gadm(country, level = 2, path='.')
  inputData$country = country
  
  # Simple fix
  inputData <- inputData %>%
    dplyr::rename(
      lon = longitude,
      lat = latitude
    )
  
  dd2 <- raster::extract(countryShp, inputData[, c("lon", "lat")])[, c("NAME_1", "NAME_2")]
  inputData$NAME_1 <- dd2$NAME_1
  inputData$NAME_2 <- dd2$NAME_2
  
  inputData2 <- unique(inputData[, c("lon", "lat", "NAME_1", "NAME_2", "country")])
  inputData2 <- inputData2[complete.cases(inputData2), ]
  inputData2$ID <- c(1:nrow(inputData2))
  gpsPoints <- unique(inputData2[, c("lon", "lat")])
  gpsPoints$lon <- as.numeric(gpsPoints$lon)
  gpsPoints$lat <- as.numeric(gpsPoints$lat)
  # gpsPoints <- gpsPoints[, c("x", "y")]
  areasCovered <- unique(c(raster::extract(countryShp, gpsPoints)$NAME_2))
  areasCovered <- areasCovered[!is.na(areasCovered)]
  print(areasCovered)
  
  
  for(aC in areasCovered){
    print(aC)
    countryShpA <- countryShp[countryShp$NAME_2 == aC]
    croppedLayer_soil <- terra::crop(Layers_soil, countryShpA)
    
    
    ## 3. apply pedo-transfer functions to get soil organic matter and soil hydraulics variables 
    if (soilProfile == TRUE){
      
      depths <- c("0-5cm","5-15cm","15-30cm","30-60cm","60-100cm","100-200cm")  
      
      
      ## Estimate P for all depths
      if(!is.null(Layers_soil_P)){
        croppedLayer_soil_P <- terra::crop(Layers_soil_P, countryShpA)
        
        # Convert 100mg/kg to mg/kg
        croppedLayer_soil_P$`P_0-30cm` <- croppedLayer_soil_P$`P_0-30cm` / 100
        
        mid_depths <- c(2.5, 10, 22.5, 45, 80, 150)
        k <- 0.03
        P_profile <- rast()
        for (p_rast_i in seq_along(names(croppedLayer_soil_P))){
          P_mean_rast <- croppedLayer_soil_P[[p_rast_i]]
          for(j in seq_along(mid_depths)){
            z <- mid_depths[j]
            new_layer <- app(P_mean_rast, fun = function(x) extrapolate_P(x, z, k))
            new_layer <- app(new_layer, fun = mehlich3_to_olsen)
            layer_name <- substr(names(croppedLayer_soil_P)[[p_rast_i]], 1, 
                                 nchar(names(croppedLayer_soil_P)[[p_rast_i]]) - 6)
            names(new_layer) <- paste0(layer_name, depths[j])
            P_profile <- c(P_profile, new_layer)
          }
        }
        croppedLayer_soil <- c(croppedLayer_soil, P_profile)
      }
      ## get soil organic matter as a function of organic carbon
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("SOM_",depths[i])]] <- (croppedLayer_soil[[paste0("soc_",depths[i])]] * 2)/10
      }
      
      
      ##### permanent wilting point (cm3/cm3) ####
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (-0.024 * croppedLayer_soil[[paste0("sand_",depths[i])]]/100) + 0.487 *
          croppedLayer_soil[[paste0("clay_",depths[i])]]/100 + 0.006 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
          0.005*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
          0.013*(croppedLayer_soil[[paste0("clay_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) +
          0.068*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay_",depths[i])]]/100 ) + 0.031
        croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (croppedLayer_soil[[paste0("PWP_",depths[i])]] + 
                                                            (0.14 * croppedLayer_soil[[paste0("PWP_",depths[i])]] - 0.02))
      }
      
      ##### FC (cm3/cm3) ######
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("FC_",depths[i])]] <- -0.251 * croppedLayer_soil[[paste0("sand_",depths[i])]]/100 + 0.195 * 
          croppedLayer_soil[[paste0("clay_",depths[i])]]/100 + 0.011 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
          0.006*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
          0.027*(croppedLayer_soil[[paste0("clay_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) + 
          0.452*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay_",depths[i])]]/100) + 0.299
        croppedLayer_soil[[paste0("FC_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]] + (1.283 * croppedLayer_soil[[paste0("FC_",depths[i])]]^2 - 0.374 * croppedLayer_soil[[paste0("FC_",depths[i])]] - 0.015))
        
      }
      
      
      ##### soil water at saturation (cm3/cm3) ######
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- 0.278*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100)+0.034*
          (croppedLayer_soil[[paste0("clay_",depths[i])]]/100)+0.022*croppedLayer_soil[[paste0("SOM_",depths[i])]] -
          0.018*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])- 0.027*
          (croppedLayer_soil[[paste0("clay_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])-
          0.584 * (croppedLayer_soil[[paste0("sand_",depths[i])]]/100*croppedLayer_soil[[paste0("clay_",depths[i])]]/100)+0.078
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("SWS_",depths[i])]] +(0.636*croppedLayer_soil[[paste0("SWS_",depths[i])]]-0.107))
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]]+croppedLayer_soil[[paste0("SWS_",depths[i])]]-(0.097*croppedLayer_soil[[paste0("sand_",depths[i])]]/100)+0.043)
        
      }
      
      ##### saturated conductivity (mm/h) ######
      for(i in 1:length(depths)) {
        b = (log(1500)-log(33))/(log(croppedLayer_soil[[paste0("FC_",depths[i])]])-log(croppedLayer_soil[[paste0("PWP_",depths[i])]]))
        lambda <- 1/b
        croppedLayer_soil[[paste0("KS_",depths[i])]] <- 1930*((croppedLayer_soil[[paste0("SWS_",depths[i])]]-croppedLayer_soil[[paste0("FC_",depths[i])]])^(3-lambda))
      }
      
      soilData <- croppedLayer_soil
      
    }else{
      
      depths <- c("0-20cm","20-50cm")  
      
      ## get soil organic matter as a function of organic carbon
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("SOM_",depths[i])]] <- (croppedLayer_soil[[paste0("oc_",depths[i])]] * 2)/10
      }
      
      ##### permanent wilting point (cm3/cm3) ####
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (-0.024 * croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100) + 0.487 *
          croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 + 0.006 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
          0.005*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
          0.013*(croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) +
          0.068*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 ) + 0.031
        croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (croppedLayer_soil[[paste0("PWP_",depths[i])]] + (0.14 * croppedLayer_soil[[paste0("PWP_",depths[i])]] - 0.02))
      }
      
      
      
      ##### FC (cm3/cm3) ######
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("FC_",depths[i])]] <- -0.251 * croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 + 0.195 * 
          croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 + 0.011 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
          0.006*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
          0.027*(croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) + 
          0.452*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100) + 0.299
        croppedLayer_soil[[paste0("FC_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]] + (1.283 * croppedLayer_soil[[paste0("FC_",depths[i])]]^2 - 0.374 * croppedLayer_soil[[paste0("FC_",depths[i])]] - 0.015))
        
      }
      
      
      ##### soil water at saturation (cm3/cm3) ######
      for(i in 1:length(depths)) {
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- 0.278*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100)+0.034*
          (croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100)+0.022*croppedLayer_soil[[paste0("SOM_",depths[i])]] -
          0.018*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])- 0.027*
          (croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])-
          0.584 * (croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100*croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100)+0.078
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("SWS_",depths[i])]] +(0.636*croppedLayer_soil[[paste0("SWS_",depths[i])]]-0.107))
        croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]]+croppedLayer_soil[[paste0("SWS_",depths[i])]]-(0.097*croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100)+0.043)
        
      }
      
      ##### saturated conductivity (mm/h) ######
      for(i in 1:length(depths)) {
        b = (log(1500)-log(33))/(log(croppedLayer_soil[[paste0("FC_",depths[i])]])-log(croppedLayer_soil[[paste0("PWP_",depths[i])]]))
        lambda <- 1/b
        croppedLayer_soil[[paste0("KS_",depths[i])]] <- 1930*((croppedLayer_soil[[paste0("SWS_",depths[i])]]-croppedLayer_soil[[paste0("FC_",depths[i])]])^(3-lambda))
      }
      
      names(croppedLayer_soil) <- gsub("0-20cm", "top", names(croppedLayer_soil))
      names(croppedLayer_soil) <- gsub("20-50cm", "bottom", names(croppedLayer_soil))
      names(croppedLayer_soil) <- gsub("_0-200cm", "", names(croppedLayer_soil))
      names(croppedLayer_soil) <- gsub("\\.", "_",  names(croppedLayer_soil)) 
      croppedLayer_isric <- terra::crop(Layers_soil_isric, countryShpA)
      names(croppedLayer_isric) <- gsub("0-30cm", "0_30", names(croppedLayer_isric))
      soilData <- c(croppedLayer_soil, croppedLayer_isric)
    }
    if(aC == areasCovered[1]){
      soilData_allregion <- soilData
    }else{
      soilData_allregion <- merge(soilData_allregion, soilData)
    }
  }
  
  
  ## 4. Extract point soil data 
  pointDataSoil <- as.data.frame(raster::extract(soilData_allregion, gpsPoints))
  pointDataSoil <- subset(pointDataSoil, select=-c(ID))
  pointDataSoil <- cbind(unique(inputData2[, c("country", "NAME_1", "NAME_2", "lon", "lat")]), pointDataSoil)
  

  ## 6. Extract harvest choice soil class and drainage rate (just for profile =TRUE)
  if(soilProfile == TRUE){
    coordinates_df <- data.frame(lat=pointDataSoil$lat, lon=pointDataSoil$lon)
    coordinates_sf <- st_as_sf(coordinates_df, coords = c("lon", "lat"), crs = 4326)
    intersecting_polygons <-st_join(coordinates_sf, shapefileHC)
    # Extract the geometry (latitude and longitude) from the 'joined_data' object
    intersecting_polygons  <-intersecting_polygons  %>%
      mutate(lon = st_coordinates(intersecting_polygons)[, "X"], 
             lat = st_coordinates(intersecting_polygons)[, "Y"]) 
    intersecting_polygons  <-as.data.frame(intersecting_polygons)
    intersecting_polygons$geometry <- NULL
    intersecting_polygons$ID <- NULL
    
    
    # Join the LDR (drainage rate) values to the intersecting_polygons data
    LDR_data <- data.frame(LDR = c(rep(0.2, 9), rep(0.5, 9), rep(0.75, 9)),
                           GRIDCODE = seq(1:27))
    
    LDR_data <- merge(intersecting_polygons,LDR_data)
    LDR_data$GRIDCODE <- NULL
    pointDataSoil <- unique(merge(pointDataSoil, LDR_data, by=c("lon", "lat")))
  }
  
  
  return(pointDataSoil)
}


extract_geoSpatialPointDataForecast <- function(
    country, useCaseName, Crop, inputData, init_month_user, forecast_year, 
    season_length_months, Planting_month_date, Harvest_month_date, 
    plantingWindow, season, pathOut, 
    AOI = T, soilData = T, weatherData = T, soilProfile = T) {
  message(prov)
  
  if (AOI) {  # TODO: ADD non-AOI case
    inputData <- inputData %>%
      dplyr::mutate(country = NULL,
             startingDate = Planting_month_date,
             endDate = Harvest_month_date,  # TODO: Is this even necessary?
             ID = 1:nrow(inputData)
             ) %>%
      dplyr::rename(longitude = lon,
             latitude = lat
             ) %>% 
      dplyr::relocate(longitude, latitude, startingDate, endDate, ID)
    
    
    # Process NC Forecast files to RDS
    fc_path <- paste0("/home/jovyan/agwise-planting-date-and-cultivar/Data/", 
                      country_map(country), "/forecast/bias_corrected/")
    var_names <- c("Rainfall", "TemperatureMax", "TemperatureMin",
                   "SolarRadiation")
    for (var_name in var_names) {
      x <- read_and_process_forecast(
        fc_path = fc_path, forecast_year = forecast_year, var_name = var_name,
        inputData = inputData, pathOut = pathOut)
      print(tibble(x$data))
    }
    
  }   
    
    
    
    
    # I haven't changed this below since it works
    ARD <- Paths_Vars(
      country = country, useCaseName = useCaseName, Crop = Crop, 
      inputData = inputData, Planting_month_date = Planting_month_date, 
      Harvest_month_date = Harvest_month_date, soilProfile = soilProfile, 
      AOI = AOI,  pathOut = pathOut)
    
    Layers_soil <- ARD[[8]]
    Layers_soil_isric <- ARD[[9]]
    shapefileHC <- ARD[[10]]
    Layers_soil_P <- ARD[[12]]
    
    if(soilData == TRUE & season == 1){
      sData <- get_soil_DEM_pointData(
        country = country, soilProfile = soilProfile, pathOut = pathOut,
        inputData = inputData, Layers_soil = Layers_soil, 
        Layers_soil_isric = Layers_soil_isric, shapefileHC = shapefileHC,
        Layers_soil_P = Layers_soil_P)
      
      if(AOI == TRUE){
        if (soilProfile == TRUE){
          s_name <- "SoilDEM_PointData_AOI_profile.RDS"
        }else{
          s_name <- "SoilDEM_PointData_AOI.RDS"
        }
      }else{
        if (soilProfile == TRUE){
          s_name <- "SoilDEM_PointData_trial_profile.RDS"
        }else{
          s_name <- "SoilDEM_PointData_trial.RDS"
        }
      }
      saveRDS(sData, paste0(pathOut, s_name))
    }
    
}

