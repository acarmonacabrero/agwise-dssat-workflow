###############################################################################
# Script Name: 00_config_function.R
#
# Project: AgWISE – Planting Date & Cultivar Advisory System
#
# Purpose:
#   Generate standardized, country-level JSON configuration files for the
#   AgWISE climate–agronomy pipeline. The configuration defines spatial domain,
#   directory structure, seasonal initialization, and forecast metadata in a
#   reproducible and pipeline-agnostic format.
#
# ---------------------------------------------------------------------------
# INPUTS AND CONDITIONS OF USE
# ---------------------------------------------------------------------------
#
# Required Inputs:
#
#   country_code (character)
#     - ISO-3 country code (e.g. "ETH", "GHA").
#     - ALWAYS required.
#
#   base_dir (character)
#     - Root directory for AgWISE data storage.
#     - ALWAYS required.
#
# ---------------------------------------------------------------------------
# Optional Inputs (User-Controlled) and Conditions
# ---------------------------------------------------------------------------
#
#   use_manual_extent (logical)
#     - If TRUE:
#         * Spatial domain is defined by `extent_manual`.
#         * `manual_domain_name` is used as the domain identifier.
#         * National boundary data (GADM) is NOT used.
#     - If FALSE:
#         * Spatial domain is derived automatically from national (admin-0)
#           boundaries using GADM.
#         * A fixed buffer (0.5°) is applied to the bounding box.
#         * `country_code` is used as the domain identifier.
#     - Default: FALSE.
#
#   extent_manual (numeric vector of length 4)
#     - Bounding box in [North, West, South, East] order.
#     - USED ONLY IF `use_manual_extent == TRUE`.
#     - IGNORED otherwise.
#
#   manual_domain_name (character)
#     - Name assigned to the manual spatial domain.
#     - USED ONLY IF `use_manual_extent == TRUE`.
#     - IGNORED otherwise.
#
# ---------------------------------------------------------------------------
# Seasonal Initialization Inputs
# ---------------------------------------------------------------------------
#
#   init_month_user (integer, 1–12)
#     - Month in which forecast initialization occurs.
#     - If NA:
#         * Initialization month defaults to the current system month.
#     - If provided:
#         * Overrides the automatic month selection.
#
#   init_day_user (integer, 1–31)
#     - Day of month for forecast initialization.
#     - If NA:
#         * Initialization day defaults to 1.
#     - If provided:
#         * Overrides the automatic day selection.
#
#   season_length_months (integer)
#     - Length of the forecast season in months.
#     - ALWAYS used to construct the season label (e.g. DJFM, JJAS).
#     - Default: 4.
#
# ---------------------------------------------------------------------------
# Forecast Timing Inputs
# ---------------------------------------------------------------------------
#
#   forecast_year (integer)
#     - Target year for the forecast products.
#     - If not provided:
#         * Defaults to the current calendar year.
#
# ---------------------------------------------------------------------------
# OUTPUT
# ---------------------------------------------------------------------------
#
#   - A single JSON configuration file written to:
#       <base_dir>/<COUNTRY_CODE>/<COUNTRY_CODE>_<SEASON>_config_agwise.json
#
#   - The JSON file contains:
#       * File paths for raw and processed data
#       * Spatial domain definition
#       * Seasonal initialization parameters
#       * Forecast year and variable metadata
#
# ---------------------------------------------------------------------------
# DESIGN PRINCIPLES
# ---------------------------------------------------------------------------
#
#   - Generic: no country-specific logic is hard-coded.
#   - Reproducible: identical inputs always yield identical configurations.
#   - Explicit: each input is applied only under clearly defined conditions.
#   - Pipeline-agnostic: usable by both R and Python workflows.
#
# Author:
#   Jemal Ahmed (J.Ahmed@cgiar.org)
#
# Last Updated:
#   2025-10-04
###############################################################################
load_packages_install <- function(pkgs) {
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      install.packages(p, dependencies = TRUE)
    }
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }
}
###############################################################

#options(java.parameters = "-Xmx8000m")
options(java.parameters = "-Xmx64g")
options(warn = -1)
pkgs <- list("rJava","loadeR.java","geodata","jsonlite","ncdf4","loadeR","transformeR",
          "downscaleR","loadeR.2nc","visualizeR","parallel","terra","gridExtra","grid", "RColorBrewer")
lapply(pkgs, require, character.only = TRUE)
#load_packages_install(pkgs)


###############################################################

build_country_config <- function( country_code, base_dir, use_manual_extent = FALSE, extent_manual = extent_manual, 
				manual_domain_name = "Manual_Domain", init_month_user = NA_integer_, init_day_user = NA_integer_, 
				season_length_months = 4, forecast_year = forecast_year, year_start_obs, year_end_obs, year_hndS, year_hndE) {
					
					

	today <- Sys.Date()
	
	# 1) Country & base paths
	# -----------------------------
	country_dir  <- file.path(base_dir, country_code)
	

	dir_raw_mask	<- file.path(country_dir, "mask")
	dir_raw_admin	<- file.path(country_dir, "admin")

	dir_raw_obs	<- file.path(country_dir, "Observation")
	dir_raw_model	<- file.path(country_dir, "daily_model_data")
	
	dir_bc_fcst    	<- file.path(country_dir, "forecast", "bias_corrected")

	dir_ext_fcst   	<- file.path(country_dir, "forecast", "extremes")
	dir_onset	<- file.path(country_dir, "forecast", "Onset_DoY")

	dir_scores	<- file.path(country_dir, "scores")
	dir_logs	<- file.path(country_dir, "logs")

	dirs_to_create <- c(base_dir,dir_raw_obs, dir_raw_model, dir_bc_fcst, 
	                    dir_ext_fcst, dir_onset,dir_scores, dir_logs)

	for (d in dirs_to_create) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

  # -----------------------------
  # 3) EXTENT LOGIC (UNCHANGED)
  # -----------------------------
  if (use_manual_extent) {

    stopifnot(length(extent_manual) == 4)
    extent_nwse <- extent_manual
    domain_name <- manual_domain_name

  } else {

    adm0 <- geodata::gadm(country = country_code, level = 0, path = dir_raw_admin)
    if (!is.lonlat(adm0)) adm0 <- project(adm0, "EPSG:4326")

    e <- ext(adm0)
    relax_deg <- 0.5

    extent_nwse <- c(min(90,   ymax(e) + relax_deg),
      max(-180, xmin(e) - relax_deg),
      max(-90,  ymin(e) - relax_deg),
      min(180,  xmax(e) + relax_deg))

    domain_name <- country_code
  }

  # -----------------------------
  # 4) COUNTRY PARAMETERS (UNCHANGED)
  # -----------------------------
  center_variable <- c(
    "ECMWF_51.PRCP",
    "ECMWF_51.TMAX",
    "ECMWF_51.TMIN",
    "ECMWF_51.SRAD"
  )

  # -----------------------------
  # 5) INIT MONTH / DAY (UNCHANGED LOGIC)
  # -----------------------------
  init_month_auto <- as.integer(format(today, "%m"))
  init_day_auto   <- 1

  init_month <- ifelse(is.na(init_month_user), init_month_auto, init_month_user)
  init_day   <- ifelse(is.na(init_day_user),   init_day_auto,   init_day_user)

  make_season_name <- function(start_month, n_months) {
    month_letters <- c("J","F","M","A","M","J","J","A","S","O","N","D")
    idx <- ((start_month - 1 + 0:(n_months - 1)) %% 12) + 1
    paste(month_letters[idx], collapse = "")
  }

  season_name <- make_season_name(init_month, season_length_months)

  # -----------------------------
  # 6) BUILD JSON (UNCHANGED)
  # -----------------------------
  COUNTRY_CONFIGS <- setNames(
    list(list(
      domain_name       = domain_name,
      dir_s2s           = country_dir,
      dir_save_score    = dir_scores,
      dir_to_save_obs   = dir_raw_obs,
      dir_to_save_model = dir_raw_model,
      
      dir_raw_mask	= dir_raw_mask,
      dir_raw_admin	= dir_raw_admin,
      dir_raw_obs	= dir_raw_obs,
      dir_raw_model	= dir_raw_model,
      dir_bc_fcst	= dir_bc_fcst,

      extent_obs        = extent_nwse,
      extent_model      = extent_nwse,

      year_start_obs    = year_start_obs,
      year_end_obs      = year_end_obs,
      year_hndS         = year_hndS,
      year_hndE         = year_hndE,
      forecast_year     = forecast_year,
      season            = season_name,
      season_length_months = season_length_months,

      init_month        = init_month,
      init_day          = init_day,

      center_variable   = center_variable
    )),
    country_code
  )

  # -----------------------------
  # 7) WRITE JSON (UNCHANGED)
  # -----------------------------
  config_json_path <- file.path(
    country_dir,
    paste0(country_code, "_config_agwise.json")
  )

  write_json(
    COUNTRY_CONFIGS,
    path = config_json_path,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )

  message("Wrote: ", config_json_path)
  message("Extent [N,W,S,E] = ", paste(round(extent_nwse, 4), collapse = ", "))
  message("Season-month/day = ", season_name, "-", init_month, "/", init_day)

  invisible(config_json_path)
}



# Reed configuration files 
load_country_config_from_json <- function(config_json_path) {
  cfg_all <- read_json(config_json_path, simplifyVector = TRUE)
  
  # top-level key is the country code (e.g., "ETH")
  cc <- names(cfg_all)[1]
  cfg <- cfg_all[[cc]]
  
  # Expose variables your pipeline scripts expect
  country_code <- cc
  country_dir  <- cfg$dir_s2s
  
  dir_scores   <- cfg$dir_save_score
  dir_raw_obs  <- cfg$dir_to_save_obs
  dir_raw_model<- cfg$dir_to_save_model
  
  dir_raw_mask  <- cfg$dir_raw_mask
  dir_raw_admin <- cfg$dir_raw_admin
  dir_bc_fcst   <- cfg$dir_bc_fcst
  
  
  extent_obs   <- cfg$extent_obs     # [N,W,S,E]
  extent_model <- cfg$extent_model
  
  year_start_obs <- cfg$year_start_obs
  year_end_obs   <- cfg$year_end_obs
  
  year_hndS      <- cfg$year_hndS
  year_hndE      <- cfg$year_hndE
  
  forecast_year  <- cfg$forecast_year
  init_month     <- cfg$init_month
  init_day       <- cfg$init_day
  season_length_months = cfg$season_length_months
  season_name    <- cfg$season
  
  center_variable <- cfg$center_variable
  
  # Return everything as a list (safe and explicit)
  list(
    country_code = country_code,
    country_dir  = country_dir,
    dir_scores   = dir_scores,
    dir_raw_obs  = dir_raw_obs,
    dir_raw_model= dir_raw_model,
    dir_raw_mask = dir_raw_mask,
    dir_raw_admin= dir_raw_admin,
    dir_bc_fcst = dir_bc_fcst,
    extent_obs   = extent_obs,
    extent_model = extent_model,
    year_start_obs = year_start_obs,
    year_end_obs   = year_end_obs,
    year_hndS      = cfg$year_hndS,
    year_hndE      = cfg$year_hndE,
    forecast_year  = forecast_year,
    init_month     = init_month,
    init_day       = init_day,
    season_length_months = season_length_months,
    season_name    = season_name,
    center_variable= center_variable
  )
}

globalAttributeList <- c(sprintf("title=%s",
          "Bias-corrected daily ensemble mean seasonal climate forecast for CGIAR AgWISE framework"),
  
  sprintf("purpose=%s", paste("This dataset provides bias-corrected seasonal-to-subseasonal climate forecast variables",
            "generated for agricultural decision support under the AgWISE framework.",
            "The data are intended for use in crop modelling, agro-climatic risk analysis",
            "and climate advisory services at national and sub-national scales.",
            sep = " ")),
  
  "institution=Alliance Bioversity-CIAT, Addis Ababa, Ethiopia",
  "contact=Jemal Ahmed",
  "email=J.Ahmed@cgiar.org",
  "source=CDS: ECMWF SEAS5 seasonal forecast, bias-corrected against gridded observations",
  sprintf("created_on=%s", format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"))
)
