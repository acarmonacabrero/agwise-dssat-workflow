getGridCoordinates <- function(
    country, useCaseName, Crop, project_root, resltn = 0.05, provinces = NULL, 
    district = NULL) { 

  pathOut <- paste0(project_root, "/Data/useCase_", country, "_",
                    useCaseName, "/", Crop, "/data_curation/",
                    country, "/")
  
  if (!dir.exists(pathOut)) {
    dir.create(file.path(pathOut), recursive = T)
  }
  
  ## get country abbreviation to used in gdam function
  # countryCC <- countrycode(country, origin = 'country.name', destination = 'iso3c')
  
  ## read the relevant shape file from gdam to be used to crop the global data
  countrySpVec <- geodata::gadm(country, level = 2, path = '.')
  
  if(!is.null(provinces)){
    level3 <- countrySpVec[countrySpVec$NAME_1 %in% provinces ]
  }else if (!is.null(district)){
    level3 <- countrySpVec[countrySpVec$NAME_2 %in% district, ]
  }else{
    level3 <- countrySpVec
  }
  
  plot(countrySpVec)
  plot(level3, add = T, col = "green")
  
  xmin <- ext(level3)[1]
  xmax <- ext(level3)[2]
  ymin <- ext(level3)[3]
  ymax <- ext(level3)[4]
  
  ## define a rectangular area that covers the whole study area (with buffer of 10 km around)
  lon_coors <- unique(round(seq(xmin - 0.1, xmax + 0.1, by = resltn),
                            digits = 3))
  lat_coors <- unique(round(seq(ymin - 0.1, ymax + 0.1, by = resltn),
                            digits = 3))
  rect_coord <- as.data.frame(expand.grid(x = lon_coors, y = lat_coors))
  
  if(resltn == 0.05){
    rect_coord$x <- floor(rect_coord$x * 10) / 10 + ifelse(
      rect_coord$x - (floor(rect_coord$x * 10) / 10) < 0.05, 0.025, 0.075)
    rect_coord$y <- floor(rect_coord$y * 10) / 10 + ifelse(
      abs(rect_coord$y) - (floor(abs(rect_coord$y) * 10) / 10) < 0.05, 0.025, 0.075)
  }
  rect_coord <- unique(rect_coord[, c("x", "y")])
  # }else if (resltn == 0.01) {
  #   rect_coord$x <- floor(rect_coord$x*100)/100
  #   rect_coord$y <- floor(rect_coord$y*100)/100
  #   rect_coord <- unique(rect_coord[,c("x", "y")])
  # }else{
  #  names(rect_coord) <- c("x", "y")
  # }
  
  State_LGA <- as.data.frame(raster::extract(countrySpVec, rect_coord))
  State_LGA$lon <- rect_coord$x
  State_LGA$lat <- rect_coord$y
  State_LGA$country <- country
  
  State_LGA <- unique(State_LGA[, c("country", "NAME_1", "NAME_2", "lon", "lat")])
  
  if(!is.null(provinces)){
    State_LGA <- droplevels(State_LGA[State_LGA$NAME_1 %in% provinces, ])
  }else if (!is.null(district)){
    State_LGA <- droplevels(State_LGA[State_LGA$NAME_2 %in% district, ])}
  
  State_LGA <- droplevels(State_LGA[!is.na(State_LGA$NAME_2), ])
  
  saveRDS(State_LGA, paste0(pathOut, "AOI_GPS.RDS"))
  
  return(State_LGA)
}
