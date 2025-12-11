# FAST Weather Preprocessing for Onion Climate Model ----
#
# This script:
#   - Loads required packages (installing them if missing).
#   - Reads compressed weather data (historical + SSP scenarios).
#   - Computes:
#       * 7-day rolling mean of soil temperature at 5 cm (Ts_5cm_smooth)
#       * Photosynthetically active radiation (PAR) from Ra
#       * Daily growing degree days (GDD_daily) using a base temperature
#   - Splits the data into a nested list:
#       result[[scenario]][[id_season]] -> data.table of daily weather

# Process weather data function ----

process_weather_data <- function(
    file_path = file.path("data", "weather_koeln-bonn_final_compressed.rds"),
    scenarios = c("historical", "ssp126", "ssp245", "ssp370", "ssp585"),
    base_temp = 1
) 
  {
  #print(101)
  # 1) Read and convert to data.table ----------------------------------------
  weather_combined <- readRDS(file_path)
  data.table::setDT(weather_combined)  # in-place conversion
  
  # Sort once by season and day-of-year (yday) for correct temporal order
  data.table::setkey(weather_combined, id_season, yday)
  
  # 2) Create derived variables ----------------------------------------------
  
  # 2.1 7-day rolling mean of soil temperature at 5 cm depth
  weather_combined[
    ,
    Ts_5cm_smooth := data.table::frollmean(
      Ts_5cm,
      n      = 7,
      align  = "right",
      fill   = NA_real_
    ),
    by = id_season
  ]
  
  # 2.2 Photosynthetically active radiation (PAR)
  #      Approx. 45% of extraterrestrial radiation Ra
  weather_combined[, PAR := Ra * 0.45]
  
  # 2.3 Daily growing degree days (GDD)
  #      GDD_daily = max(0, Tavg - base_temp)
  weather_combined[, GDD_daily := pmax(0, Tavg - base_temp)]
  
  # 3) Extract scenario name from id_season -----------------------------------
  
  weather_combined[
    ,
    scenario := data.table::tstrsplit(id_season, "--", fixed = TRUE, keep = 1L)
  ]
  
  # 4) Split into a nested list: scenario -> id_season ------------------------
  
  # First split by scenario:
  #   result[[scenario]] -> all seasons for that scenario
  weather_precomputed <- split(
    weather_combined,
    by      = "scenario",
    keep.by = FALSE
  )
  
  # Then, within each scenario, split by id_season:
  #   result[[scenario]][[id_season]] -> one season's daily weather
  weather_precomputed <- lapply(weather_precomputed, function(dt) {
    split(dt, by = "id_season", keep.by = FALSE)
  })
  
  
  return(weather_precomputed)
}


# Run preprocessing ----

# This call reads the default weather file and prepares `weather_precomputed`.


weather_precomputed <- process_weather_data(
  file_path = file.path("data", "weather_koeln-bonn_final_compressed.rds"),
  base_temp = 1
)
