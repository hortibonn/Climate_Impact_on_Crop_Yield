plot_onion_irrigated_vs_rainfed <- function(onion_mc_simulation) {
  
  y <- onion_mc_simulation$y
  
  # --- 1) Build tidy data (FINAL yield only) ---
  results <- dplyr::bind_rows(
    data.frame(
      value    = y$historical.final_yield_per_ha_rainfed,
      water    = "Rainfed",
      scenario = "2020"
    ),
    data.frame(
      value    = y$historical.final_yield_per_ha_irrigated,
      water    = "Irrigated",
      scenario = "2020"
    ),
    data.frame(
      value    = y$ssp126.final_yield_per_ha_rainfed,
      water    = "Rainfed",
      scenario = "SSP1-2.6"
    ),
    data.frame(
      value    = y$ssp126.final_yield_per_ha_irrigated,
      water    = "Irrigated",
      scenario = "SSP1-2.6"
    ),
    data.frame(
      value    = y$ssp245.final_yield_per_ha_rainfed,
      water    = "Rainfed",
      scenario = "SSP2-4.5"
    ),
    data.frame(
      value    = y$ssp245.final_yield_per_ha_irrigated,
      water    = "Irrigated",
      scenario = "SSP2-4.5"
    ),
    data.frame(
      value    = y$ssp370.final_yield_per_ha_rainfed,
      water    = "Rainfed",
      scenario = "SSP3-7.0"
    ),
    data.frame(
      value    = y$ssp370.final_yield_per_ha_irrigated,
      water    = "Irrigated",
      scenario = "SSP3-7.0"
    ),
    data.frame(
      value    = y$ssp585.final_yield_per_ha_rainfed,
      water    = "Rainfed",
      scenario = "SSP5-8.5"
    ),
    data.frame(
      value    = y$ssp585.final_yield_per_ha_irrigated,
      water    = "Irrigated",
      scenario = "SSP5-8.5"
    )
  ) %>%
    dplyr::mutate(
      year_grp = dplyr::if_else(scenario == "2020", "2020", "2075"),
      scenario = factor(
        scenario,
        levels = c("2020","SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5")
      ),
      water = factor(water, levels = c("Rainfed","Irrigated"))
    )
  
  # --- 2) Plot ---
  p <- ggplot2::ggplot(results, ggplot2::aes(x = scenario, y = value, fill = water)) +
    ggplot2::geom_boxplot(
      position = ggplot2::position_dodge(width = 0.8),
      outlier.shape = NA
    ) +
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = 45.7, linetype = "baseline"),
      colour = "black",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    ggplot2::facet_grid(~ year_grp, scales = "free_x", space = "free_x") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::scale_y_continuous(
      name = "Zwiebel-Ertrag (final) [t/ha]",
      expand = ggplot2::expansion(mult = c(0.02, 0.10))
    ) +
    ggplot2::scale_x_discrete(name = "Klimaszenario") +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = c(
        "Rainfed"   = "#4C78A8",
        "Irrigated" = "#72B7B2"
      ),
      labels = c(
        "Rainfed"   = "Unbewässert",
        "Irrigated" = "Bewässert"
      )
    ) +
    ggplot2::scale_linetype_manual(
      name   = NULL,
      values = c(baseline = "dashed"),
      labels = c(baseline = "Referenzertrag\nRegierungsbezirk Köln (2024)")
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.text      = ggplot2::element_text(size = 16),
      strip.placement  = "outside",
      strip.background = ggplot2::element_rect(fill = "lightgrey", colour = NA),
      strip.text       = ggplot2::element_text(size = 12, face = "bold"),
      panel.border     = ggplot2::element_rect(colour = "grey40", fill = NA, linewidth = 0.6),
      panel.spacing.x  = grid::unit(10, "pt")
    ) +
    ggplot2::guides(
      linetype = ggplot2::guide_legend(order = 1),
      fill     = ggplot2::guide_legend(order = 2)
    )
  
  return(p)
}


