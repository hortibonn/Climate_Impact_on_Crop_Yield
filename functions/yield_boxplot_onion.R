plot_yield_onion <- function(onion_mc_simulation) {
  
  ############################################################
  ## Ertrags-Boxplots
  ## Updated version with improved data structure and fixed calculations
  ############################################################
  
  ############################################################
  ## 1) Gemeinsame Definitionen
  ############################################################
  
  scenario_order_codes <- c("historical", "ssp126", "ssp245", "ssp370", "ssp585")
  scenario_labels <- c(
    historical = "2020",
    ssp126     = "SSP1-2.6",
    ssp245     = "SSP2-4.5",
    ssp370     = "SSP3-7.0",
    ssp585     = "SSP5-8.5"
  )
  
  ############################################################
  ## 2) Multiplikatoren -> Schaden (für Konsistenz)
  ############################################################
  
  onion_mc_sim_damage <- onion_mc_simulation
  stress_cols <- grep("\\.m_", names(onion_mc_sim_damage$y), value = TRUE)
  if (length(stress_cols) > 0) {
    onion_mc_sim_damage$y[stress_cols] <-
      1 - onion_mc_sim_damage$y[stress_cols]
  }
  
  ############################################################
  ## 3) Ertrags-Daten zusammenbauen
  ############################################################
  
  results_yield_all <- rbind(
    data.frame(
      Ertrag               = onion_mc_sim_damage$y$historical.raw_yield_per_ha_irrigated,
      vermarktbarer_Ertrag = onion_mc_sim_damage$y$historical.final_yield_per_ha_irrigated,
      scenario_code        = "historical"
    ),
    data.frame(
      Ertrag               = onion_mc_sim_damage$y$ssp126.raw_yield_per_ha_irrigated,
      vermarktbarer_Ertrag = onion_mc_sim_damage$y$ssp126.final_yield_per_ha_irrigated,
      scenario_code        = "ssp126"
    ),
    data.frame(
      Ertrag               = onion_mc_sim_damage$y$ssp245.raw_yield_per_ha_irrigated,
      vermarktbarer_Ertrag = onion_mc_sim_damage$y$ssp245.final_yield_per_ha_irrigated,
      scenario_code        = "ssp245"
    ),
    data.frame(
      Ertrag               = onion_mc_sim_damage$y$ssp370.raw_yield_per_ha_irrigated,
      vermarktbarer_Ertrag = onion_mc_sim_damage$y$ssp370.final_yield_per_ha_irrigated,
      scenario_code        = "ssp370"
    ),
    data.frame(
      Ertrag               = onion_mc_sim_damage$y$ssp585.raw_yield_per_ha_irrigated,
      vermarktbarer_Ertrag = onion_mc_sim_damage$y$ssp585.final_yield_per_ha_irrigated,
      scenario_code        = "ssp585"
    )
  )
  
  # Szenarien als Factor mit Labels
  results_yield_all$scenario <- factor(
    results_yield_all$scenario_code,
    levels = scenario_order_codes,
    labels = scenario_labels[scenario_order_codes]
  )
  
  ############################################################
  ## 4) Long-Format + Jahres-Gruppierung
  ############################################################
  
  results_long <- results_yield_all %>%
    pivot_longer(
      cols      = c(Ertrag, vermarktbarer_Ertrag),
      names_to  = "name",
      values_to = "value"
    ) %>%
    mutate(
      year_grp = if_else(scenario == "2020", "2020", "2075"),
      scenario = factor(
        scenario,
        levels = c("2020", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
      ),
      name = factor(name, levels = c("Ertrag", "vermarktbarer_Ertrag"))
    )
  
  ############################################################
  ## 5) Prozent-Verlust berechnen (FIXED)
  ############################################################
  
  loss_df <- results_long %>%
    group_by(scenario, name) %>%
    summarise(m = mean(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = name, values_from = m) %>%
    mutate(
      # FIXED calculation: (raw - final) / raw * 100
      loss_percent = (Ertrag - vermarktbarer_Ertrag) / Ertrag * 100,
      year_grp = if_else(scenario == "2020", "2020", "2075")
    )
  
  ############################################################
  ## 6) Label-Position über oberstem Whisker
  ############################################################
  
  whisk_df <- results_long %>%
    group_by(year_grp, scenario, name) %>%
    summarise(upper = boxplot.stats(value)$stats[5], .groups = "drop") %>%
    group_by(year_grp, scenario) %>%
    summarise(y = max(upper), .groups = "drop")
  
  label_df <- left_join(
    whisk_df,
    loss_df %>% select(year_grp, scenario, loss_percent),
    by = c("year_grp", "scenario")
  ) %>%
    mutate(y = y * 1.05)
  
  ############################################################
  ## 7) Plot erstellen
  ############################################################
  
  ggplot(results_long, aes(x = scenario, y = value, fill = name)) +
    geom_boxplot(position = position_dodge(width = 0.8),
                 outlier.size = 1.8) +
    geom_text(
      data = label_df,
      aes(x = scenario, y = y, label = paste0(round(loss_percent), "%")),
      inherit.aes = FALSE,
      fontface = "bold",
      vjust = 0
    ) +
    geom_hline(
      aes(yintercept = 45.7, linetype = "baseline"),
      colour = "black",
      linewidth = 0.5,
      inherit.aes = FALSE
    ) +
    facet_grid(~ year_grp, scales = "free_x", space = "free_x") +
    coord_cartesian(clip = "off") +
    scale_y_continuous(
      name = "Zwiebel-Ertrag [t/ha]",
      expand = expansion(mult = c(0.02, 0.12))
    ) +
    scale_x_discrete(name = "Klimaszenario") +
    scale_fill_manual(
      name   = NULL,
      values = c("Ertrag" = "cadetblue",
                 "vermarktbarer_Ertrag" = "firebrick"),
      labels = c(
        "Ertrag"               = "Potentieller Ertrag\n(ohne Schäden)",
        "vermarktbarer_Ertrag" = "Tatsächlicher Ertrag"
      )
    ) +
    scale_linetype_manual(
      name   = NULL,
      values = c(baseline = "dashed"),
      labels = c(baseline = "Referenzertrag \nRegierungsbezirk Köln (2024)")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position  = "bottom",
      legend.text      = element_text(size = 14),  # Larger legend text
      strip.placement  = "outside",
      strip.background = element_rect(fill = "lightgrey", colour = NA),
      strip.text       = element_text(face = "bold", size = 16),  # Larger facet labels (2020, 2075)
      panel.border     = element_rect(colour = "grey40", fill = NA)
    )
}