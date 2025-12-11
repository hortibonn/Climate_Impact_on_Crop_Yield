## Main function: build combined VIP plot for all scenarios -----------

make_onion_vip_combined_plot <- function(onion_mc_simulation,
                                         scenarios = c("historical",
                                                       "ssp126",
                                                       "ssp245",
                                                       "ssp370",
                                                       "ssp585")) {
  
  
  ############################################################
  ## 1) Common Definitions
  ############################################################
  
  scenario_order_codes <- scenarios
  
  scenario_labels <- c(
    historical = "2020",
    ssp126     = "2075 (SSP 1-2.6)",
    ssp245     = "2075 (SSP 2-4.5)",
    ssp370     = "2075 (SSP 3-7.0)",
    ssp585     = "2075 (SSP 5-8.5)"
  )
  
  stress_labels <- c(
    # Keimphase (em)
    "m_exrain_em"   = "Starkregen (Keimphase)",
    "m_hail_em"     = "Hagel (Keimphase)",
    "m_fus_em"      = "Fusarium (Keimphase)",
    
    # Vegetative Phase (vg)
    "m_heat_vg"     = "Hitze (Vegetative Phase)",
    "m_exrain_vg"   = "Starkregen (Vegetative Phase)",
    "m_hail_vg"     = "Hagel (Vegetative Phase)",
    "m_fus_vg"      = "Fusarium (Vegetative Phase)",
    "m_mildew_vg"   = "Falscher Mehltau (Vegetative Phase)",
    "m_thrips_vg"   = "Thripse (Vegetative Phase)",
    
    # Zwiebelbildung (bl)
    "m_heat_bl"     = "Hitze (Zwiebelbildung)",
    "m_exrain_bl"   = "Starkregen (Zwiebelbildung)",
    "m_hail_bl"     = "Hagel (Zwiebelbildung)",
    "m_fus_bl"      = "Fusarium (Zwiebelbildung)",
    "m_mildew_bl"   = "Falscher Mehltau (Zwiebelbildung)",
    "m_botrytis_bl" = "Botrytis (Zwiebelbildung)",
    "m_thrips_bl"   = "Thripse (Zwiebelbildung)",
    
    # Reifephase (mt)
    "m_heat_mt"     = "Hitze (Reifephase)",
    "m_exrain_mt"   = "Starkregen (Reifephase)",
    "m_hail_mt"     = "Hagel (Reifephase)",
    "m_fus_mt"      = "Fusarium (Reifephase)",
    "m_mildew_mt"   = "Falscher Mehltau (Reifephase)",
    "m_botrytis_mt" = "Botrytis (Reifephase)",
    "m_thrips_mt"   = "Thripse (Reifephase)"
  )
  
  phase_order  <- c("Keimphase", "Vegetative Phase", "Zwiebelbildung", "Reifephase")
  order_labels <- unname(stress_labels)   # Reihenfolge der y-Achse
  
  ############################################################
  ## 2) Multipliers -> Damage (0 = no damage)
  ############################################################
  
  onion_mc_sim_damage <- onion_mc_simulation
  
  stress_cols <- grep("\\.m_", names(onion_mc_sim_damage$y), value = TRUE)
  if (length(stress_cols) > 0) {
    onion_mc_sim_damage$y[stress_cols] <-
      1 - onion_mc_sim_damage$y[stress_cols]
  }
  
  ############################################################
  ## 3) Helper: remove variables with too little variation
  ############################################################
  
  filter_stress_vars <- function(df, min_nonzero = 5) {
    keep <- sapply(df, function(x) sum(x != 0, na.rm = TRUE) >= min_nonzero)
    df[, keep, drop = FALSE]
  }
  
  ############################################################
  ## 4) VIP helper
  ############################################################
  
  VIP_table <- function (plsrResults, input_table = NULL, cut_off_line = 1, 
                         threshold = 0.8, x_axis_name = "Variable Importance in Projection", 
                         y_axis_name = NULL, legend_name = "Coefficient", 
                         legend_labels = c("Positive", "Negative"), 
                         pos_color = "cadetblue", neg_color = "firebrick", 
                         base_size = 11, ...) {
    
    assertthat::assert_that(inherits(plsrResults, "mvr"),
                            msg = "plsrResults ist nicht Klasse 'mvr'.")
    
    VIP <- function(object) {
      if (object$method != "oscorespls") 
        stop("Only implemented for oscorespls")
      if (nrow(object$Yloadings) > 1) 
        stop("Only for single-response models")
      SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
      Wnorm2 <- colSums(object$loading.weights^2)
      SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
      sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
    }
    
    if (plsrResults$ncomp == 1) {
      vipResult <- VIP(plsrResults)
    } else {
      vipResult <- VIP(plsrResults)["Comp 1", ]
    }
    
    coef <- plsrResults$coefficients[, , 1]
    pls_outputs <- data.frame(
      Variable    = names(vipResult), 
      VIP         = vipResult, 
      Coefficient = coef,
      row.names   = NULL
    )
    
    if (!is.null(input_table)) {
      combined_table <- dplyr::left_join(
        pls_outputs, input_table, by = c(Variable = "variable")
      )
    } else {
      combined_table <- pls_outputs
    }
    
    filtered <- dplyr::filter(combined_table, VIP > threshold)
    list(VIP_table_results = filtered)
  }
  
  ############################################################
  ## 5) PLSR für ein Szenario (VIP-Tabelle)
  ############################################################
  
  run_plsr_for_scenario_onion <- function(sim_list_damage, scen_code,
                                          min_nonzero = 5) {
    
    # response: irrigated final yield
    y_col <- paste0(scen_code, ".final_yield_per_ha_irrigated")
    if (!y_col %in% names(sim_list_damage$y))
      stop("Spalte ", y_col, " nicht in sim_list_damage$y gefunden.")
    
    y <- data.frame(yield = sim_list_damage$y[[y_col]])
    
    # predictors: all stress multiplier -> damage columns
    stress_base <- names(stress_labels)
    stress_cols_scen <- paste0(scen_code, ".", stress_base)
    stress_cols_scen <- stress_cols_scen[stress_cols_scen %in% names(sim_list_damage$y)]
    
    x_raw <- sim_list_damage$y[, stress_cols_scen, drop = FALSE]
    x <- filter_stress_vars(x_raw, min_nonzero = min_nonzero)
    if (ncol(x) == 0) {
      stop("Für Szenario ", scen_code,
           " gibt es keine Stressvariablen mit ausreichender Variation.")
    }
    
    # Szenario-Prefix aus Spaltennamen entfernen
    names(x) <- sub(paste0("^", scen_code, "\\."), "", names(x))
    
    sim_obj <- list(y = y, x = x)
    class(sim_obj) <- c("mcSimulation", "list")
    
    # PLSR
    pls_res <- plsr.mcSimulation(
      object      = sim_obj,
      resultName  = "yield",
      variables.x = names(sim_obj$x),
      ncomp       = 1
    )
    
    # VIP table + extra info
    vip_tab <- VIP_table(pls_res, threshold = 0.8)$VIP_table_results |>
      as_tibble() |>
      mutate(
        CleanVar   = Variable,
        stress_lab = stress_labels[CleanVar],
        Phase      = sub(".*\\((.*)\\).*", "\\1", stress_lab),
        Phase      = factor(Phase, levels = phase_order),
        coef_sign  = dplyr::case_when(
          Coefficient >  0 ~ "positive",
          Coefficient <  0 ~ "negative",
          TRUE             ~ "zero"
        ),
        scenario   = scen_code
      )
    
    # ordered factor für y-Achse
    vip_tab$stress_lab <- factor(
      vip_tab$stress_lab,
      levels  = order_labels,
      ordered = TRUE
    )
    
    return(vip_tab)
  }
  
  ############################################################
  ## 6) Combined VIP plot (alle Szenarien)
  ############################################################
  
  VIP_plot_onion <- function(sim_results, min_nonzero = 5) {
    scen_codes <- scenario_order_codes
    
    # Alle Szenarien laufen lassen und kombinieren
    vip_list <- lapply(
      scen_codes,
      function(sc) run_plsr_for_scenario_onion(sim_results, sc, min_nonzero = min_nonzero)
    )
    names(vip_list) <- scen_codes
    
    vip_combined <- dplyr::bind_rows(vip_list)
    
    # Szenario als ordered factor
    vip_combined$scenario <- factor(
      vip_combined$scenario,
      levels = scenario_order_codes,
      ordered = TRUE
    )
    
    # Labels für Facets
    vip_combined$scenario_label <- scenario_labels[as.character(vip_combined$scenario)]
    vip_combined$scenario_label <- factor(
      vip_combined$scenario_label,
      levels = unname(scenario_labels[scenario_order_codes]),
      ordered = TRUE
    )
    
    # WICHTIG: x ist jetzt eine Konstante -> eine Spalte Bubbles pro Facet, mittig
    p <- ggplot(vip_combined, aes(x = "VIP", y = stress_lab)) +
      geom_point(aes(size = VIP), shape = 16, color = "firebrick") +
      
      facet_wrap(~ scenario_label, nrow = 1, scales = "fixed") +
      
      scale_size_continuous(
        range  = c(4, 14),
        limits = c(0.8, 4),
        breaks = c(1, 2, 3, 4),
        name   = "VIP"
      ) +
      
      scale_y_discrete(
        limits = rev(order_labels),
        drop   = FALSE,
        expand = expansion(add = c(0.7, 0.7))  # mehr vertikaler Abstand
      ) +
      
      labs(
        x = NULL,
        y = "Stressfaktor (Phase)"
      ) +
      
      theme_minimal(base_size = 12) +
      theme(
        strip.text         = element_text(hjust = 0.5, size = 12),
        axis.text.y        = element_text(hjust = 0, vjust = 0.5),
        axis.text.x        = element_blank(),
        axis.ticks.x       = element_blank(),
        axis.title.x       = element_blank(),
        legend.position    = "bottom",
        legend.title       = element_text(hjust = 0),
        legend.key.height  = grid::unit(0.6, "cm"),
        legend.key.width   = grid::unit(0.6, "cm"),
        legend.text        = element_text(size = 9),
        panel.spacing      = unit(0.5, "lines")
      )
    
    
    return(p)
  }
  
  # *** RETURN VALUE OF MAIN FUNCTION ***
  VIP_plot_onion(onion_mc_sim_damage)
}

## Example usage -----------------------------------------------------

# p_vip <- make_onion_vip_combined_plot(onion_mc_simulation)
# p_vip
