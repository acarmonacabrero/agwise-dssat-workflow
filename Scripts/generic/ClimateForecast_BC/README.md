# AgWise Climate Forecast Post-Processing Pipeline
## Description 

The AgWise Climate Post-Processing Pipeline provides a scientifically rigorous framework for transforming raw subseasonal–seasonal (S2S) and seasonal climate forecasts into agronomically meaningful inputs that can be reliably used in crop modeling, yield forecasting, early warning systems, and climate-smart advisory services. While global forecasting systems offer increasing skill, their raw outputs cannot be directly applied to agricultural decision-making: they are biased, coarse-resolution, and often unable to represent local rainfall onset, intra-seasonal rainfall variability, or extreme temperature and precipitation events. The AgWise pipeline resolves these challenges through a structured, multi-stage workflow that integrates Python-based data acquisition with R-based bias correction, statistical downscaling, and extreme-event derivation.

At the core of the system is a bias correction and statistical downscaling engine. Using long hindcast records, the pipeline quantifies systematic errors in global forecast models relative to high-quality observational datasets. These learned corrections are then applied to real-time forecasts, ensuring that daily rainfall, temperature, and solar radiation fields more accurately reflect the local climatology and variability patterns experienced by farmers. Such correction is essential for crop models like DSSAT, APSIM, and AquaCrop, which are sensitive to biases in rainfall intensity, heat stress, radiation accumulation, and sequence-dependent daily weather fluctuations. By calibrating forecasts against local observations, AgWise enables more realistic simulations of crop growth stages, planting suitability, water balance, soil moisture availability, and potential yield outcomes. Building on these corrected forecasts, the pipeline produces a suite of agro-climatic extreme indices that quantify the hazards most relevant for agricultural risk management. These include intense rainfall events (R10, R20, Rx1day, Rx5day), persistent dry spells (CDD), wet spells (CWD), high-temperature extremes (TX90p, heatwave metrics), and radiation-stress indicators. Such indices are core predictors of drought impacts, lodging, heat damage, pest emergence, and soil erosion risk. When generated from bias-corrected forecasts, these indices form a reliable foundation for early warning systems, advisory bulletins, and seasonal risk assessments.

A defining feature of the pipeline is its capacity to support rainfall onset prediction—a cornerstone for climate-smart agriculture in rainfed systems. Raw climate forecasts typically misrepresent the timing and temporal structure of early-season rainfall. Through daily bias correction and sequence reconstruction, the pipeline provides robust onset detection using proven methods that incorporate false-start screening, cumulative rainfall thresholds, and persistence criteria. These corrected onset metrics directly support planting date advisories, field preparation guidance, and cultivar selection, ensuring that farmers receive recommendations aligned with both historical realities and upcoming seasonal signals.

## Folder structure
The workflow is fully automated and multi-country capable. Python modules retrieve observations and model outputs from the Copernicus Climate Data Store, while R scripts handle correction, downscaling, and extreme-event computation. Outputs follow a standardized structure under each country folder:

```bash
/<country>/
    Observation/                     # Reference datasets
    daily_model_data/                # Raw hindcasts & forecasts
    processed/
        bias_corrected/forecast/     # Daily bias-corrected climate
        extremes/forecast/           # Extreme indices for risk monitoring
        onset/forecast/              # Rainfall onset & false-start metrics
```

This processed information provides a scientifically defensible climate foundation for AgWise and its partner systems, including national agro-advisory platforms (e.g., EDACaP, PSP, PICSA), early warning systems, crop modeling workflows, and regional climate services. By embedding rigorous post-processing methods, AgWise transforms global forecasting products into locally relevant, actionable climate intelligence, strengthening resilience, productivity, and decision-making across agricultural landscapes.

## Wokflow 
```bash
[Stage 1] Python downloader → raw observations + model data
    |
[Stage 2] Python orchestrator → organized multi-country datasets
    |
[Stage 3] R bias correction → BC daily climate for all variables
    |
[Stage 4] R extremes + onset → indicators for crop modeling & advisories
    |
[Final] AgWise / DSSAT-ready climate inputs + early-warning signals
```


## Contact 
Jemal S. Ahmed
Alliance of Bioversity International & CIAT 
jemal.ahmed@cgiar.org
