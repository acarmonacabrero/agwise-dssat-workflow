project_root <- "/home/jovyan/rs-soil-comparison-africa"
country <- "Kenya"
useCaseName <- "Example"
Crop <- "Maize"

# Source helper functions
source(paste0(project_root, '/Scripts/generic/DSSAT/helpers_readGeo_CM_zone.R'))
source(paste0(project_root, '/Scripts/generic/DSSAT/common_helpers.R'))

# Produce ISDA RDS objects from server data
# get_ISDA_soilRDS <- function(
#     country, useCaseName, Crop, project_root, inputData = NULL) {
#   
#   baseSoilPath <- "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/"
#   shapefileHC <- st_read(paste0(baseSoilPath, "HC27/HC27 CLASSES.shp"), quiet = T) %>%
#     st_make_valid()
#   
#   baseSoilPath <- "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/iSDA"
#   listRaster_soil <- list.files(path = baseSoilPath, pattern = ".tif$")
#   Layers_soil <- terra::rast(paste(baseSoilPath, listRaster_soil, sep = "/"))
#   
#   
#   if (is.null(inputData)) {
#     dataPath <- paste0(project_root, "/Data/useCase_")
#     inputData <- readRDS(paste0(dataPath, country, "_", useCaseName, "/", Crop,
#                                 "/data_curation/", country, "/AOI_GPS.RDS"))
#   }
#   
#   countryShp <- geodata::gadm(country, level = 2, path = '.')
#   
#   ### Seems redundant based on how the inputData file is constructed
#   # inputData$country = country
#   # dd2 <- raster::extract(countryShp, inputData[, c("lon", "lat")])[, c("NAME_1", "NAME_2")]
#   # inputData$NAME_1 == dd2$NAME_1
#   # inputData$NAME_2 <- dd2$NAME_2
#   
#   ### No need for inputData2
#   # inputData2 <- unique(inputData)[, c("lon", "lat", "NAME_1", "NAME_2", "country")])
#   # inputData2 <- inputData2[complete.cases(inputData2), ]
#   # inputData2$ID <- c(1:nrow(inputData2))
#   gpsPoints <- inputData[, c("lon", "lat")]
#   gpsPoints$lon <- as.numeric(gpsPoints$lon)
#   gpsPoints$lat <- as.numeric(gpsPoints$lat)
#   
#   areasCovered <- unique(inputData$NAME_2)
#   areasCovered <- areasCovered[!is.na(areasCovered)]
#   
#   for(aC in areasCovered) {
#     print(aC)
#     countryShpA <- countryShp[countryShp$NAME_2 == aC]
#     croppedLayer_soil <- terra::crop(Layers_soil, countryShpA)
#     
#     depths <- c("0-20cm", "20-50cm")  
#     
#     ### SOM as function of OC
#     for(d in depths) {
#       croppedLayer_soil[[paste0("SOM_", d)]] <- (croppedLayer_soil[[paste0("oc_", d)]] * 2) / 10
#     }
#     
#     ### PWP (permanent wilting point)
#     for(d in depths) {
#       croppedLayer_soil[[paste0("PWP_", d)]] <- (-0.024 * croppedLayer_soil[[paste0("sand.tot.psa_", d)]] / 100) + 0.487 *
#         croppedLayer_soil[[paste0("clay.tot.psa_", d)]] / 100 + 0.006 * croppedLayer_soil[[paste0("SOM_", d)]] + 
#         0.005 * (croppedLayer_soil[[paste0("sand.tot.psa_", d)]] / 100 * croppedLayer_soil[[paste0("SOM_", d)]]) - 
#         0.013 * (croppedLayer_soil[[paste0("clay.tot.psa_", d)]] / 100 * croppedLayer_soil[[paste0("SOM_", d)]]) +
#         0.068 * (croppedLayer_soil[[paste0("sand.tot.psa_", d)]] / 100 * croppedLayer_soil[[paste0("clay.tot.psa_", d)]] / 100 ) + 0.031
#       croppedLayer_soil[[paste0("PWP_", d)]] <- (croppedLayer_soil[[paste0("PWP_", d)]] + (0.14 * croppedLayer_soil[[paste0("PWP_", d)]] - 0.02))
#     }
#     
#     ### FC (field capacity)
#     for(d in depths) {
#       croppedLayer_soil[[paste0("FC_", d)]] <- -0.251 * croppedLayer_soil[[paste0("sand.tot.psa_", d)]] / 100 + 0.195 * 
#         croppedLayer_soil[[paste0("clay.tot.psa_", d)]] / 100 + 0.011 * croppedLayer_soil[[paste0("SOM_", d)]] + 
#         0.006 * (croppedLayer_soil[[paste0("sand.tot.psa_", d)]] / 100 * croppedLayer_soil[[paste0("SOM_", d)]]) - 
#         0.027 * (croppedLayer_soil[[paste0("clay.tot.psa_", d)]] / 100 * croppedLayer_soil[[paste0("SOM_", d)]]) + 
#         0.452 * (croppedLayer_soil[[paste0("sand.tot.psa_", d)]] / 100 * croppedLayer_soil[[paste0("clay.tot.psa_", d)]] / 100) + 0.299
#       croppedLayer_soil[[paste0("FC_", d)]] <- (croppedLayer_soil[[paste0("FC_", d)]] + (1.283 * croppedLayer_soil[[paste0("FC_", d)]] ^ 2 - 0.374 * croppedLayer_soil[[paste0("FC_", d)]] - 0.015))
#       
#     }
#     
#     ### SWS (soil water at saturation)
#     for(d in depths) {
#       croppedLayer_soil[[paste0("SWS_", d)]] <- 0.278 * (croppedLayer_soil[[paste0("sand.tot.psa_", d)]] / 100) + 0.034 *
#         (croppedLayer_soil[[paste0("clay.tot.psa_", d)]] / 100) + 0.022 * croppedLayer_soil[[paste0("SOM_", d)]] -
#         0.018 * (croppedLayer_soil[[paste0("sand.tot.psa_", d)]] / 100 * croppedLayer_soil[[paste0("SOM_", d)]]) - 0.027 *
#         (croppedLayer_soil[[paste0("clay.tot.psa_", d)]] / 100 * croppedLayer_soil[[paste0("SOM_", d)]])-
#         0.584 * (croppedLayer_soil[[paste0("sand.tot.psa_", d)]] / 100 * croppedLayer_soil[[paste0("clay.tot.psa_", d)]] / 100) + 0.078
#       croppedLayer_soil[[paste0("SWS_", d)]] <- (croppedLayer_soil[[paste0("SWS_", d)]] + (0.636*croppedLayer_soil[[paste0("SWS_", d)]] - 0.107))
#       croppedLayer_soil[[paste0("SWS_", d)]] <- (croppedLayer_soil[[paste0("FC_", d)]] + croppedLayer_soil[[paste0("SWS_", d)]] - (0.097 * croppedLayer_soil[[paste0("sand.tot.psa_", d)]] / 100) + 0.043)
#       
#     }
#     
#     ### KS (saturated conductivity) (mm/h)
#     for(d in depths) {
#       b = (log(1500) - log(33))/(log(croppedLayer_soil[[paste0("FC_", d)]]) - log(croppedLayer_soil[[paste0("PWP_", d)]]))
#       lambda <- 1 / b
#       croppedLayer_soil[[paste0("KS_", d)]] <- 1930 * ((croppedLayer_soil[[paste0("SWS_", d)]] - croppedLayer_soil[[paste0("FC_", d)]]) ^ (3 - lambda))
#     }
#     
#     soilData <- c(croppedLayer_soil)
#     
#     if(aC == areasCovered[1]) {
#       soilData_allregion <- soilData
#     } else {
#       soilData_allregion <- merge(soilData_allregion, soilData)
#     }
#     
#   }
#   
#   pointDataSoil <- as.data.frame(raster::extract(soilData_allregion, gpsPoints))
#   pointDataSoil <- subset(pointDataSoil, select = -c(ID))
#   pointDataSoil <- cbind(inputData,
#                          pointDataSoil)
#   
#   pointDataSoil <- convert_ISDA_units(pointDataSoil)
#   
#   coordinates_df <- data.frame(lat = pointDataSoil$lat, lon = pointDataSoil$lon)
#   coordinates_sf <- st_as_sf(coordinates_df, coords = c("lon", "lat"), crs = 4326)
#   intersecting_polygons <- st_join(coordinates_sf, shapefileHC)
#   # Extract the geometry (latitude and longitude) from the 'joined_data' object
#   intersecting_polygons <- intersecting_polygons %>%
#     mutate(lon = st_coordinates(intersecting_polygons)[, "X"], 
#            lat = st_coordinates(intersecting_polygons)[, "Y"])
#   intersecting_polygons <- as.data.frame(intersecting_polygons)
#   intersecting_polygons$geometry <- NULL
#   intersecting_polygons$ID <- NULL
#   
#   
#   # Join the LDR (drainage rate) values to the intersecting_polygons data
#   LDR_data <- data.frame(LDR = c(rep(0.2, 9), rep(0.5, 9), rep(0.75, 9)),
#                          GRIDCODE = seq(1:27))
#   
#   LDR_data <- merge(intersecting_polygons, LDR_data)
#   LDR_data$GRIDCODE <- NULL
#   pointDataSoil <- unique(merge(pointDataSoil, LDR_data, by = c("lon", "lat")))
#   
#   for (prov in unique(inputData$NAME_1)) {
#     general_pathIn <- paste0(
#       "~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",
#       useCaseName, "/", Crop, "/result/geo_4cropModel")
#     pathIn <- define_pathIn(general_pathIn, level2 = NA, zone = prov,
#                             pathIn_zone = T, Forecast = F, create_path = T)
#     pointDataSoil_prov <- pointDataSoil %>% filter(NAME_1 == prov)
#     saveRDS(pointDataSoil_prov, paste0(pathIn, "/ISDA_SoilDEM_PointData_AOI_profile.RDS"))
#   }
# }


get_ISDA_soilRDS(country = country, useCaseName = useCaseName, Crop = Crop, project_root = project_root)
