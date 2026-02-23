### Load or install required packages
load_or_install <- function(pkg) {
  if (!requireNamespace(pkg, quietly = T)) {
    install.packages(pkg)
  }
  suppressPackageStartupMessages(library(pkg, character.only = T))
}


### Load required packages
packages_required <- c(
  "tidyverse", "lubridate", "DSSAT", "furrr", "future", "future.apply",
  "stringr", "geodata", "readr", "purrr", "terra"
)

invisible(lapply(packages_required, load_or_install))



### Common EXTpath
create_extdata_path <- function(project_root, country, useCaseName, Crop, 
                                varietyid, AOI = F) {
  subfolder <- ifelse(AOI, "AOI", "fieldData")
  path <- file.path(project_root, paste0(
    "Data/useCase_", country, "_", useCaseName),
    Crop, "transform/DSSAT", subfolder, varietyid)
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  return(path)
}


### Common DSSAT template data path
create_dssat_temdata_path <- function(project_root, country, useCaseName, Crop) {
  path.to.temdata <- paste0(project_root, "/Data/useCase_", country, 
                            "_", useCaseName, "/", Crop, "/Landing/DSSAT/")
  if (!dir.exists(path.to.temdata)) {
    stop("Directory with DSSAT Template Data (soil and weather files) does ", 
         "not exist, please add the template files. Process will stop.")
  }
  path.to.temdata
}


create_dssat_working_path <- function(path.to.extdata, i, zone = NA, level2 = NA) {
  
  # sanity check
  if (!is.na(level2) && is.na(zone)) {
    stop(
      "You need to define a zone (administrative level 1) ",
      "to be able to get data for level 2 (administrative level 2)."
    )
  }
  
  exte_id <- paste0("EXTE", formatC(as.integer(i), width = 4, flag = "0"))
  
  sub_path <- dplyr::case_when(
    !is.na(zone) & !is.na(level2) ~ file.path(zone, level2, exte_id),
    !is.na(zone) &  is.na(level2) ~ file.path(zone, exte_id),
    is.na(zone)  &  is.na(level2) ~ exte_id
  )
  
  working_path <- file.path(path.to.extdata, sub_path)
  
  if (!dir.exists(working_path)) {
    dir.create(working_path, recursive = TRUE)
  }
  
  working_path
}


### TODO: Check whether this is a good common function or not
define_pathOUT <- function(path.to.extdata, i, zone = NA, level2 = NA) {
  if (!is.na(level2) && is.na(zone)) {
    stop(
      "You need to define a zone (administrative level 1) ",
      "to get data for level 2 (administrative level 2)."
    )
  }
  exte_id <- paste0(
    "EXTE",
    formatC(as.integer(i), width = 4, flag = "0")
  )
  pathOUT <- if (!is.na(zone) && !is.na(level2)) {
    file.path(path.to.extdata, zone, level2, exte_id)
  } else if (!is.na(zone)) {
    file.path(path.to.extdata, zone, exte_id)
  } else {
    file.path(path.to.extdata, exte_id)
  }
  if (!dir.exists(pathOUT)) {
    dir.create(pathOUT, recursive = TRUE)
  }
  return(pathOUT)
}


### Produce the AOI_GPS.RDS file
getGridCoordinates <- function(
    country, useCaseName, Crop, resltn = 0.05, project_root, provinces = NULL, 
    district = NULL) { 
  
  pathOut <- paste0(project_root, "/Data/useCase_", country, "_", useCaseName,
                    "/", Crop, "/data_curation/", country, "/")
  
  if (!dir.exists(pathOut)) {
    dir.create(file.path(pathOut), recursive = T)
  }
  
  ### get country abbreviation to used in gdam function
  # countryCC <- countrycode(country, origin = 'country.name', destination = 'iso3c')
  
  ### read the relevant shape file from gdam to be used to crop the global data
  countrySpVec <- geodata::gadm(country, level = 2, path = '.')
  
  if(!is.null(provinces)) {
    level3 <- countrySpVec[countrySpVec$NAME_1 %in% provinces ]
  } else if (!is.null(district)) {
    level3 <- countrySpVec[countrySpVec$NAME_2 %in% district, ]
  } else {
    level3 <- countrySpVec
  }
  
  plot(countrySpVec)
  plot(level3, add = T, col = "green")
  
  xmin <- ext(level3)[1]
  xmax <- ext(level3)[2]
  ymin <- ext(level3)[3]
  ymax <- ext(level3)[4]
  
  ### define a rectangular area that covers the whole study area (with buffer of 10 km around)
  lon_coors <- unique(round(seq(xmin - 0.1, xmax + 0.1, by = resltn),
                            digits = 3))
  lat_coors <- unique(round(seq(ymin - 0.1, ymax + 0.1, by = resltn),
                            digits = 3))
  rect_coord <- as.data.frame(expand.grid(x = lon_coors, y = lat_coors))
  
  if(resltn == 0.05) {
    rect_coord$x <- floor(rect_coord$x * 10) / 10 + ifelse(
      rect_coord$x - (floor(rect_coord$x * 10) / 10) < 0.05, 0.025, 0.075)
    rect_coord$y <- floor(rect_coord$y * 10) / 10 + ifelse(
      abs(rect_coord$y) - (floor(abs(rect_coord$y) * 10) / 10) < 0.05, 0.025, 0.075)
  }
  
  rect_coord <- unique(rect_coord[, c("x", "y")])
  # } else if (resltn == 0.01) {
  #   rect_coord$x <- floor(rect_coord$x*100)/100
  #   rect_coord$y <- floor(rect_coord$y*100)/100
  #   rect_coord <- unique(rect_coord[,c("x", "y")])
  # } else {
  #  names(rect_coord) <- c("x", "y")
  # }
  
  State_LGA <- as.data.frame(raster::extract(countrySpVec, rect_coord))
  State_LGA$lon <- rect_coord$x
  State_LGA$lat <- rect_coord$y
  State_LGA$country <- country
  
  State_LGA <- unique(State_LGA[, c("country", "NAME_1", "NAME_2", "lon", "lat")])
  
  if(!is.null(provinces)) {
    State_LGA <- droplevels(State_LGA[State_LGA$NAME_1 %in% provinces, ])
  } else if (!is.null(district)) {
    State_LGA <- droplevels(State_LGA[State_LGA$NAME_2 %in% district, ])
  }
  
  State_LGA <- droplevels(State_LGA[!is.na(State_LGA$NAME_2), ])
  
  saveRDS(State_LGA, paste0(pathOut, "AOI_GPS.RDS"))
  
  return(State_LGA)
}


### Plan multisession
plan_multisession <- function(per_worker_gb) {
  # Detect RAM limits (container-aware)
  ram_limit_file <- "/sys/fs/cgroup/memory/memory.limit_in_bytes"
  if (file.exists(ram_limit_file)) {
    ram_limit_bytes <- as.numeric(readLines(ram_limit_file))
    if (!is.na(ram_limit_bytes) && ram_limit_bytes < 2 ^ 60) {
      available_ram_gb <- ram_limit_bytes / 1024 ^ 3
    } else {
      available_ram_gb <- as.numeric(system("grep MemAvailable /proc/meminfo | awk '{print $2}'", intern=TRUE)) / 1024 / 1024
    }
  } else {
    available_ram_gb <- as.numeric(system("grep MemAvailable /proc/meminfo | awk '{print $2}'", intern=TRUE)) / 1024 / 1024
  }
  
  # Compute safe number of workers
  workers <- floor(available_ram_gb / per_worker_gb)
  workers <- min(workers, availableCores() - 3)
  workers <- max(workers, 1)
  
  # Activate plan
  suppressWarnings(plan(multisession, workers = workers))
  
  message("Parallel plan: ", workers, " workers, estimated per-worker RAM: ", per_worker_gb, " GB")
}


### Load inputData. If missing, produce it
load_or_generate_inputData <- function(country, useCaseName, Crop, project_root,
                                       inputData = NULL) {
  
  if (is.null(inputData)) {
    
    dataPath <- paste0(project_root, "/Data/useCase_")
    inputData_path <- paste0(dataPath, country, "_", useCaseName, "/", Crop,
                             "/data_curation/", country, "/AOI_GPS.RDS")
    
    if (file.exists(inputData_path)) {
      inputData <- readRDS(inputData_path)
    } else {
      getGridCoordinates(country, useCaseName, Crop, project_root, 
                         resltn = 0.05, provinces = NULL, district = NULL)
      inputData <- readRDS(inputData_path)
    }
  }
  
  return(inputData)
}


### Function to write DSSAT progress log files
write_dssat_log <- function(messages_list, file) {
  file_path <- file.path(project_root, "Data",
                         paste0("useCase_", country, "_", useCaseName),
                         Crop,
                         file)
  # Flatten list to single character vector
  log_lines <- unlist(messages_list)
  
  # Write to file
  writeLines(log_lines, con = file_path)
  
  message("Log written to: ", file_path)
}
