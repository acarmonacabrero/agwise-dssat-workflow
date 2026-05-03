# 1. Extent definition
Country <- "Kenya"
zones <- c("Kisumu", "Siaya")
adm_level <- 1

# 2. Experiment definition
useCaseName <- "SoilComparison"
sample_soil_file_name <- "obs_soils_dry_matter.csv"
sample_data <- NULL  # Add path to read
force_reanalysis <- F  # Set to T to redo tile search and file processing
condition_to_drop <- "all"  # Requirements to drop an observation: all or any of the requested variable is NA

# 3. Variable assignment definition
# Note this only needs to be changed if a new observational dataset is used


# 4. ISDA and ISRIC characteristics
Olsen_conversion <- F  # If true, modify canonical P name
soil_var_map <- tibble(
  canonical = c("Sand (%)", "Silt (%)", "Clay (%)", "N (g/kg)",
                "P_mehlich (mg/kg)", "pH", "OC (g/kg)"),
  
  sample = c("sand_perc", "silt_perc", "clay_perc", "N_perc",
             "Meh_P_ppm", "pH", "OC_perc"),
  
  ISRIC = c("sand", "silt", "clay", "nitrogen",
            "af_p", "phh2o", "soc"),
  
  ISDA = c("sand_tot_psa", "silt_tot_psa", "clay_tot_psa",
           "log.n_tot_ncs", "log.p_mehlich3", "ph_h2o", "log.oc"),
  
  sample_transformation = c(identity, identity, identity, function(x) x * 10, 
                            identity, identity, function(x) x * 10),
  
  ISRIC_transformation = c(function(x) x/10, function(x) x/10, function(x) x/10,
                           function(x) x/100, function(x) x/100, 
                           function(x) x/10, function(x) x/10),
  
  ISDA_transformation = c(identity, identity, identity,
                          function(x) expm1(x / 100),
                          function(x) expm1(x / 10), function(x) x/10,
                          function(x) expm1(x / 10))
)

depth_map <- list(
  canonical = c("0-20", "20-50"),
  ISRIC = c("0-5", "5-15", "15-30", "30-60", "60-100", "100-200"),
  ISDA = c("0..20", "20..50"),
  Sample = c()
)