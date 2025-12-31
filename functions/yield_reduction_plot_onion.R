## Main function: yield reduction heatmap by phase & stressor ----

make_yield_reduction_heatmap <- function(mcSimulation_results) {
  
  
  ############################################################
  ## 1) Definitions
  ############################################################
  
  scenario_order_codes <- c("historical", "ssp126", "ssp245", "ssp370", "ssp585")
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
  
  phase_order <- c("Keimphase", "Vegetative Phase", "Zwiebelbildung", "Reifephase")
  
  ############################################################
  ## 2) Multiplikatoren -> Damage
  ############################################################
  
  onion_mc_sim_damage <- mcSimulation_results #onion_mc_simulation
  stress_cols <- grep("\\.m_", names(onion_mc_sim_damage$y), value = TRUE)
  onion_mc_sim_damage$y[stress_cols] <-
    1 - onion_mc_sim_damage$y[stress_cols]
  
  ############################################################
  ## 3) Data preparation for Heatmap (long table)
  ############################################################
  
  yield_damage_long <- onion_mc_sim_damage$y |>
    dplyr::select(dplyr::matches("^(historical|ssp126|ssp245|ssp370|ssp585)\\.m_")) |>
    tidyr::pivot_longer(
      cols      = dplyr::everything(),
      names_to  = "full_name",
      values_to = "damage"
    ) |>
    tidyr::separate(
      full_name,
      into = c("scenario", "CleanVar"),
      sep  = "\\.",
      extra = "merge"
    ) |>
    dplyr::mutate(
      scenario = factor(
        scenario,
        levels = scenario_order_codes,
        labels = scenario_labels[scenario_order_codes]
      )
    )
  
  ############################################################
  ## 4) Summarise & build heatmap
  ############################################################
  
  yield_red_summary <- yield_damage_long |>
    dplyr::mutate(
      CleanVar   = factor(CleanVar, levels = names(stress_labels)),
      stress_lab = stress_labels[as.character(CleanVar)],
      Phase      = sub(".*\\((.*)\\).*", "\\1", stress_lab),
      Phase      = factor(Phase, levels = phase_order)
    ) |>
    dplyr::group_by(scenario, CleanVar, stress_lab, Phase) |>
    dplyr::summarise(
      mean_reduction = mean(damage, na.rm = TRUE) * 100,
      .groups        = "drop"
    )
  
  heatmap_plot <- ggplot(
    yield_red_summary,
    aes(
      x    = scenario,
      y    = stress_lab,
      fill = mean_reduction
    )
  ) +
    geom_tile(color = "white") +
    scale_fill_gradient(
      name   = "Reduzierung\ndes Biomassezuwachses [%] ",
      low    = "white",
      high   = "firebrick"
    ) +
    labs(
      #title = "Ertragsminderungen",
      x     = "Szenario",
      y     = "Stressfaktor (Phase)"
    ) +
    facet_grid(
      rows   = vars(Phase),
      scales = "free_y",
      space  = "free_y"
    ) +
    theme_minimal(base_size = 14) +  # BIGGER: Increased base size from 12 to 14
    theme(
      plot.title        = element_text(hjust = 0.5, size = 16),  # BIGGER: was 12
      axis.text.y       = element_text(size = 14),               # BIGGER: was 12, stressor labels on left
      axis.title.y      = element_text(size = 16),               # BIGGER: was 14, "Stressfaktor (Phase)"
      axis.text.x       = element_text(angle = 45, hjust = 1, size = 14),  # BIGGER: was 11, scenario labels
      axis.title.x      = element_text(size = 16),               # BIGGER: "Szenario" label
      strip.placement   = "outside",
      strip.text.y.right = element_text(angle = 0, hjust = 0, face = "bold", size = 18),  # BIGGER and HORIZONTAL: phase labels (Keimphase, etc.)
      
      panel.spacing.y    = unit(1.2, "lines"),     # space *between* phases
      plot.margin        = margin(5.5, 80, 5.5, 5.5),  # EXTRA room on the right for phase labels (was 40)
      
      legend.title      = element_text(size = 14),   # BIGGER: was 12, legend title
      legend.text       = element_text(size = 13)    # BIGGER: was 10, legend tick labels
    )
  
  return(heatmap_plot)
}