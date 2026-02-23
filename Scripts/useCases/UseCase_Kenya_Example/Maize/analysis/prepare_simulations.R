country <- "Nigeria"
zone <- "Benue"
adm_level <- 1

processed_files_folder <- file.path(
  "/home/jovyan/rs-soil-comparison-africa/processed_files/ISRIC", country,
  paste0("NAME", adm_level, "_", zone), "DSSAT")

if (!dir.exists(processed_files_folder)) dir.create(
  processed_files_folder, recursive = TRUE)


AOI_GPS <- readRDS(
  "~/agwise-datacuration/dataops/datacuration/Data/useCase_Nigeria_AKILIMO/Maize/result/AOI_GPS.RDS")
AOI_GPS %>% filter(NAME_1 == zone)  # 0.05 lat, lon AOI
