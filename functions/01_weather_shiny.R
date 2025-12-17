# FAST Weather Preprocessing for Onion Climate Model ----
#
# This script:
#   - Ensures required packages are installed
#   - Reads compressed weather data (historical + SSP scenarios)
#   - Assumes derived variables already exist
#   - Splits data into a nested list:
#       result[[scenario]][[id_season]] -> data.table of daily weather

# Process weather data function ----

process_weather_data <- function(
    file_path = "weather_koeln-bonn_final_compressed.rds",
    scenarios = c("historical", "ssp126", "ssp245", "ssp370", "ssp585")
) {
  
  # 1) Read and convert to data.table -----------------------------
  weather_combined <- base::readRDS(file_path)
  
  data.table::setDT(weather_combined)     # in-place conversion
  data.table::setkey(weather_combined, id_season, yday)
  
  # 2) Derived variables are assumed to already exist --------------
  
  # 3) Extract scenario name from id_season ------------------------
  
  weather_combined[
    ,
    scenario := data.table::tstrsplit(
      id_season,
      "--",
      fixed = TRUE,
      keep  = 1L
    )
  ]
  
  # 4) Split into nested list: scenario -> id_season ---------------
  
  # First split by scenario
  weather_precomputed <- base::split(
    weather_combined,
    by      = "scenario",
    keep.by = FALSE
  )
  
  # Then split by id_season within each scenario
  weather_precomputed <- base::lapply(
    weather_precomputed,
    function(dt) {
      base::split(dt, by = "id_season", keep.by = FALSE)
    }
  )
  
  return(weather_precomputed)
}


# Run preprocessing ----

weather_precomputed <- process_weather_data(
  file_path = "data/weather_koeln-bonn_final_compressed.rds"
)
