# ---------------------------------------------------------- #
### CROP MODELLING FERTILIZER RECOMMENDATION MOTHER SCRIPT ###
# ---------------------------------------------------------- #

### Global Settings
country <- "Kenya"
Crop <- "Maize"
useCaseName <- "KALRO_app2"

### Step 1: get Geo and Weather data
source('/home/jovyan/agwise-fertilizer/Script/useCases/UseCase_Example_CropModelApproach/Maize/DSSAT/01_get_CM_geo_KALRO_Maize.R')

### Step 2: Edit DSSAT inputs and Run DSSAT simulations
source('/home/jovyan/agwise-fertilizer/Script/useCases/UseCase_Example_CropModelApproach/Maize/DSSAT/02_run_DSSAT_Kenya_KALRO_app2_Maize_fert_trt.R')

### Step 3: Merge DSSAT outputs and produce response results
source('/home/jovyan/agwise-fertilizer/Script/useCases/UseCase_Example_CropModelApproach/Maize/DSSAT/03_merge_dssat_outputs.R')
