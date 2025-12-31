# Helper Functions for Onion Climate Model ----
#
# This script defines:
#   - A package installer helper: ensure_installed()
#   - A factory function: helper_function()
#     which compiles and returns (or attaches) a set of helper functions.

# Helper Function Factory ----

helper_function <- function(attach_to_global = FALSE) {
  
  # Generic utility functions ----
  
  # Saturating transform: x >= 0 -> [0,1) with slope controlled by `impact`
  sat <- compiler::cmpfun(function(x, impact) {
    base::pmin(1 - base::exp(-impact * base::pmax(x, 0)), 1)
  })
  
  # Count consecutive TRUEs in a logical vector (e.g. dry days in a row)
  consec_counter <- compiler::cmpfun(function(x_is_true) {
    x <- base::as.logical(x_is_true)
    n <- base::length(x)
    out <- base::integer(n)
    if (n == 0L) return(out)
    k <- 0L
    for (i in base::seq_len(n)) {
      if (base::isTRUE(x[i])) {
        k <- k + 1L
      } else {
        k <- 0L
      }
      out[i] <- k
    }
    out
  })
  
  # Stochastic gate: returns value_if with probability p, otherwise value_if_not
  fast_chance_event <- compiler::cmpfun(function(p, value_if, value_if_not = 1) {
    if (base::is.na(p)) return(value_if_not)
    if (stats::runif(1) < p) value_if else value_if_not
  })
  
  # Expected loss: risk × loss_fraction, NA-safe, bounded to [0,1]
  expected_loss <- compiler::cmpfun(function(risk, loss_fraction) {
    if (base::is.na(risk) || base::is.na(loss_fraction)) return(0)
    base::pmin(base::pmax(risk, 0), 1) * base::pmin(base::pmax(loss_fraction, 0), 1)
  })
  
  
  # Stress functions (abiotic & biotic) ----
  
  # Seedbed stress
  get_seedbed_stress <- compiler::cmpfun(function(Prec_before_sow,
                                                  prec_seedbed_medium_p,
                                                  prec_seedbed_high_p,
                                                  impact_days_seedbed_t) {
    
    wet_med <- base::sum(
      Prec_before_sow >= prec_seedbed_medium_p &
        Prec_before_sow <  prec_seedbed_high_p,
      na.rm = TRUE
    )
    wet_high <- base::sum(Prec_before_sow >= prec_seedbed_high_p, na.rm = TRUE)
    
    wet_days_weighted <- wet_med + 2 * wet_high
    
    risk_seedbed <- 1 - base::exp(- (impact_days_seedbed_t / 2) * wet_days_weighted)
    base::pmin(risk_seedbed, 1)
  })
  
  # Extreme rain stress: weighted count of extreme rainfall days
  get_extreme_rain_stress <- compiler::cmpfun(function(Prec,
                                                       prec_extreme_rain_medium_p,
                                                       prec_extreme_rain_high_p,
                                                       impact_days_extreme_rain_t) {
    
    extreme_days_medium <- base::sum(
      Prec >= prec_extreme_rain_medium_p & Prec < prec_extreme_rain_high_p,
      na.rm = TRUE
    )
    extreme_days_high <- base::sum(
      Prec >= prec_extreme_rain_high_p,
      na.rm = TRUE
    )
    
    weighted_extreme_days <- extreme_days_medium + 2 * extreme_days_high
    risk_extreme_rain <- 1 - base::exp(- (impact_days_extreme_rain_t / 2) * weighted_extreme_days)
    base::pmin(risk_extreme_rain, 1)
  })
  
  # Hail stress: proxy using warm + rainy days
  get_hail_stress <- compiler::cmpfun(function(Tavg,
                                               Prec,
                                               prec_hail_threshold_p,
                                               Tavg_hail_threshold_p,
                                               impact_days_hail_t) {
    
    hail_days <- base::sum(
      Prec >= prec_hail_threshold_p & Tavg >= Tavg_hail_threshold_p,
      na.rm = TRUE
    )
    risk_hail <- 1 - base::exp(- (impact_days_hail_t) * hail_days)
    base::pmin(risk_hail, 1)
  })
  
  # Heat stress (defined but not yet wired into main model)
  get_heat_stress <- compiler::cmpfun(function(Tmax,
                                               days_consec_hot,
                                               Tmax_heat_threshold_p,
                                               impact_temp_heat_t,
                                               impact_days_heat_t) {
    
    R_T <- base::ifelse(
      Tmax > Tmax_heat_threshold_p,
      1 - base::exp(- (impact_temp_heat_t / 2) * (Tmax - Tmax_heat_threshold_p)),
      0
    )
    R_T <- base::pmin(R_T, 1)
    
    R_D <- 1 - base::exp(- (impact_days_heat_t / 2) * days_consec_hot)
    R_D <- base::pmin(R_D, 1)
    
    base::pmin(R_T * R_D, 1)
  })
  
  
  # Botrytis stress
  get_botrytis_stress <- compiler::cmpfun(function(Tmin,
                                                   RH_max,
                                                   days_consec_wet,
                                                   rh_botrytis_threshold_p,
                                                   impact_rh_botrytis_t,
                                                   impact_days_wet_botrytis_t,
                                                   Topt_botrytis_p,
                                                   Twidth_botrytis_p) {
    
    R_H <- base::ifelse(
      RH_max >= rh_botrytis_threshold_p,
      1 - base::exp(- (impact_rh_botrytis_t / 2) * (RH_max - rh_botrytis_threshold_p)),
      0
    )
    R_H <- base::pmin(R_H, 1)
    
    R_D <- 1 - base::exp(- (impact_days_wet_botrytis_t / 2) * days_consec_wet)
    R_D <- base::pmin(R_D, 1)
    
    R_T <- base::exp(- (Tmin - Topt_botrytis_p)^2 / (2 * Twidth_botrytis_p^2))
    R_T <- base::pmin(R_T, 1)
    
    base::pmin(R_H * R_D * R_T, 1)
  })
  
  
  # Downy mildew stress
  get_downy_mildew_stress <- compiler::cmpfun(function(Tmin,
                                                       RH_max,
                                                       days_consec_wet,
                                                       rh_mildew_threshold_p,
                                                       impact_rh_mildew_t,
                                                       impact_days_wet_mildew_t,
                                                       Topt_mildew_p,
                                                       Twidth_mildew_p) {
    
    R_H <- base::ifelse(
      RH_max >= rh_mildew_threshold_p,
      1 - base::exp(- (impact_rh_mildew_t / 2) * (RH_max - rh_mildew_threshold_p)),
      0
    )
    R_H <- base::pmin(R_H, 1)
    
    R_D <- 1 - base::exp(- (impact_days_wet_mildew_t / 2) * days_consec_wet)
    R_D <- base::pmin(R_D, 1)
    
    R_T <- base::exp(- (Tmin - Topt_mildew_p)^2 / (2 * Twidth_mildew_p^2))
    R_T <- base::pmin(R_T, 1)
    
    base::pmin(R_H * R_D * R_T, 1)
  })
  
  
  # Fusarium stress
  get_fusarium_stress <- compiler::cmpfun(function(Ts_5cm,
                                                   RH_mean,
                                                   days_consec_wet,
                                                   rh_fusarium_threshold_p,
                                                   impact_rh_fusarium_t,
                                                   impact_days_wet_fusarium_t,
                                                   Tsopt_fusarium_p,
                                                   Tswidth_fusarium_p) {
    
    R_H <- base::ifelse(
      RH_mean >= rh_fusarium_threshold_p,
      1 - base::exp(- (impact_rh_fusarium_t / 2) * (RH_mean - rh_fusarium_threshold_p)),
      0
    )
    R_H <- base::pmin(R_H, 1)
    
    R_D <- 1 - base::exp(- (impact_days_wet_fusarium_t / 2) * days_consec_wet)
    R_D <- base::pmin(R_D, 1)
    
    R_T <- base::exp(- (Ts_5cm - Tsopt_fusarium_p)^2 / (2 * Tswidth_fusarium_p^2))
    R_T <- base::pmin(R_T, 1)
    
    base::pmin(R_H * R_D * R_T, 1)
  })
  
  
  # Thrips stress
  get_thrips_stress <- compiler::cmpfun(function(Tavg,
                                                 RH_mean,
                                                 days_consec_dry,
                                                 rh_thrips_threshold_p,
                                                 Topt_thrips_p,
                                                 Twidth_thrips_p,
                                                 impact_rh_thrips_t,
                                                 impact_days_dry_thrips_t) {
    
    R_H <- base::ifelse(
      RH_mean < rh_thrips_threshold_p,
      1 - base::exp(- (impact_rh_thrips_t / 2) * (rh_thrips_threshold_p - RH_mean)),
      0
    )
    R_H <- base::pmin(R_H, 1)
    
    R_T <- base::exp(- (Tavg - Topt_thrips_p)^2 / (2 * Twidth_thrips_p^2))
    R_T <- base::pmin(R_T, 1)
    
    R_D <- 1 - base::exp(- (impact_days_dry_thrips_t / 2) * days_consec_dry)
    R_D <- base::pmin(R_D, 1)
    
    base::pmin(R_H * R_T * R_D, 1)
  })
  
  
  # Biomass calculation (vectorized)
  # FIXED VERSION: Now uses input parameters for water stress
  # Previous version had hardcoded thresholds (0.70, 0.40, 0.20)
  # New version respects f_W_1_lower, f_W_0_5 from input parameters
  # Scientific basis: FAO (2012), Shock et al. (2000), Kumar et al. (2007)
  
  calc_bio_vectorized <- compiler::cmpfun(function(PAR, LAI, Tavg, Windex,
                                                   f_T_1_lower, f_T_1_upper,
                                                   f_T_0_lower, f_T_0_upper,
                                                   f_W_1_lower, f_W_1_upper,
                                                   f_W_0_5, LUE_onion, lec_k) {
    
    Topt_lower <- f_T_1_lower
    Topt_upper <- f_T_1_upper
    Tmin_base  <- f_T_0_lower
    Tmax_base  <- f_T_0_upper
    
    # Temperature response (unchanged)
    f_T <- base::ifelse(
      Tavg >= Topt_lower & Tavg <= Topt_upper,
      1.0,
      base::ifelse(
        Tavg < Topt_lower,
        base::pmax(0.2, 0.2 + 0.8 * (Tavg - Tmin_base) / (Topt_lower - Tmin_base)),
        base::pmax(0.2, 0.2 + 0.8 * (Tmax_base - Tavg) / (Tmax_base - Topt_upper))
      )
    )
    
    f_T <- base::pmin(base::pmax(f_T, 0.2), 1.0)
    
    
    # FIXED WATER STRESS FUNCTION
    # Now properly uses input parameters instead of hardcoded values
    # Scientific basis:
    # - FAO (2012): Onions tolerate 50-60% TAW depletion without yield loss
    # - Shock et al. (2000): Minimal yield reduction until theta < 0.60
    # - Kumar et al. (2007): 80% ETc → only 4% yield loss
    
    
    f_W <- base::ifelse(
      Windex >= f_W_1_lower,
      1.0,  # No stress above lower threshold
      base::ifelse(
        Windex >= f_W_0_5,
        # Linear interpolation from 1.0 at f_W_1_lower to 0.65 at f_W_0_5
        # This represents moderate stress response
        0.65 + 0.35 * (Windex - f_W_0_5) / (f_W_1_lower - f_W_0_5),
        base::ifelse(
          Windex >= 0.25,
          # Stronger stress: from 0.65 at f_W_0_5 to 0.35 at 0.25
          0.35 + 0.30 * (Windex - 0.25) / (f_W_0_5 - 0.25),
          # Severe stress below 0.25: asymptotic to 0.15
          base::pmax(0.15, 0.35 * Windex / 0.25)
        )
      )
    )
    f_W <- base::pmin(base::pmax(f_W, 0.15), 1.0)
    
    LUE_onion * PAR * (1 - base::exp(-lec_k * LAI)) * f_T * f_W
  })
  
  
  # Collect helpers and optionally attach to global environment ----
  
  helpers <- base::list(
    sat                     = sat,
    expected_loss           = expected_loss,
    fast_chance_event       = fast_chance_event,
    consec_counter          = consec_counter,
    get_seedbed_stress      = get_seedbed_stress,
    get_extreme_rain_stress = get_extreme_rain_stress,
    get_hail_stress         = get_hail_stress,
    get_heat_stress         = get_heat_stress,
    get_botrytis_stress     = get_botrytis_stress,
    get_downy_mildew_stress = get_downy_mildew_stress,
    get_thrips_stress       = get_thrips_stress,
    get_fusarium_stress     = get_fusarium_stress,
    calc_bio_vectorized     = calc_bio_vectorized
  )
  
  if (attach_to_global) {
    base::list2env(helpers, envir = base::.GlobalEnv)
    base::message("✅ Helper functions attached to global environment.")
  }
  
  helpers
}


# Attach helpers for interactive use ----

helper_function(attach_to_global = TRUE)

