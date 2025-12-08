plot_yield_onion <- function(mcSimulation_results)
{
  ############################################################
  ## Ertrags-Boxplots
  ## Input: mcSimulation_results (like onion_mc_simulation)
  ############################################################
  
  ## 0) Pakete laden
  load_if_needed <- function(pkgs) {
    for (pkg in pkgs) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
      }
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    }
  }
  
  load_if_needed(c(
    "ggplot2",
    "dplyr",
    "tidyr"
  ))
  
  ############################################################
  ## 1) Gemeinsame Definitionen
  ############################################################
  
  scenario_order_codes <- c("historical", "ssp126", "ssp245", "ssp370", "ssp585")
  scenario_labels <- c(
    historical = "2020",
    ssp126     = "2075 (SSP 1-2.6)",
    ssp245     = "2075 (SSP 2-4.5)",
    ssp370     = "2075 (SSP 3-7.0)",
    ssp585     = "2075 (SSP 5-8.5)"
  )
  
  ############################################################
  ## 2) Multiplikatoren -> Schaden (für Konsistenz)
  ############################################################
  
  onion_mc_sim_damage <- mcSimulation_results
  stress_cols <- grep("\\.m_", names(onion_mc_sim_damage$y), value = TRUE)
  if (length(stress_cols) > 0) {
    onion_mc_sim_damage$y[stress_cols] <-
      1 - onion_mc_sim_damage$y[stress_cols]
  }
  
  ############################################################
  ## 3) Ertrags-Boxplots
  ############################################################
  
  results_yield_onion <- rbind(
    data.frame(
      Ertrag               = onion_mc_sim_damage$y$historical.raw_yield_per_ha_irrigated,
      vermarktbarer_Ertrag = onion_mc_sim_damage$y$historical.final_yield_per_ha_irrigated,
      id                   = seq_len(nrow(onion_mc_sim_damage$y)),
      scenario_code        = "historical"
    ),
    data.frame(
      Ertrag               = onion_mc_sim_damage$y$ssp126.raw_yield_per_ha_irrigated,
      vermarktbarer_Ertrag = onion_mc_sim_damage$y$ssp126.final_yield_per_ha_irrigated,
      id                   = seq_len(nrow(onion_mc_sim_damage$y)),
      scenario_code        = "ssp126"
    ),
    data.frame(
      Ertrag               = onion_mc_sim_damage$y$ssp245.raw_yield_per_ha_irrigated,
      vermarktbarer_Ertrag = onion_mc_sim_damage$y$ssp245.final_yield_per_ha_irrigated,
      id                   = seq_len(nrow(onion_mc_sim_damage$y)),
      scenario_code        = "ssp245"
    ),
    data.frame(
      Ertrag               = onion_mc_sim_damage$y$ssp370.raw_yield_per_ha_irrigated,
      vermarktbarer_Ertrag = onion_mc_sim_damage$y$ssp370.final_yield_per_ha_irrigated,
      id                   = seq_len(nrow(onion_mc_sim_damage$y)),
      scenario_code        = "ssp370"
    ),
    data.frame(
      Ertrag               = onion_mc_sim_damage$y$ssp585.raw_yield_per_ha_irrigated,
      vermarktbarer_Ertrag = onion_mc_sim_damage$y$ssp585.final_yield_per_ha_irrigated,
      id                   = seq_len(nrow(onion_mc_sim_damage$y)),
      scenario_code        = "ssp585"
    )
  )
  
  results_yield_onion$scenario <- factor(
    results_yield_onion$scenario_code,
    levels = scenario_order_codes,
    labels = scenario_labels[scenario_order_codes]
  )
  
  names(results_yield_onion)[1:3] <- c("Ertrag", "vermarktbarer_Ertrag", "id")
  
  results_yield_onion_longer <- results_yield_onion |>
    tidyr::pivot_longer(
      cols      = c(Ertrag, vermarktbarer_Ertrag),
      names_to  = "name",
      values_to = "value"
    ) |>
    dplyr::mutate(
      name   = factor(
        name,
        levels = c("Ertrag", "vermarktbarer_Ertrag"),
        labels = c("Potentieller Ertrag ohne Schäden",
                   "Tatsächlicher Ertrag")
      ),
      period = ifelse(scenario == "2020", "Historical", "Future")
    )
  
  loss_df <- results_yield_onion_longer |>
    dplyr::group_by(scenario, name) |>
    dplyr::summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = name, values_from = mean_value) |>
    dplyr::mutate(
      loss_percent =
        (`Potentieller Ertrag ohne Schäden` - `Tatsächlicher Ertrag`) /
        `Potentieller Ertrag ohne Schäden` * 100
    )
  
  max_y <- max(results_yield_onion_longer$value, na.rm = TRUE)
  
  baseline_hist  <- 45.7
  baseline_label <- paste0(
    "Durchschnittsertrag Regierungsbezirk\nKöln (", baseline_hist, " t/ha)"
  )
  
  p <- ggplot(results_yield_onion_longer,
              aes(x = scenario, y = value, fill = name)) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    
    geom_text(
      data = loss_df,
      aes(x = scenario, y = max_y * 1.03, label = paste0(round(loss_percent), "%")),
      inherit.aes = FALSE,
      size = 4,
      fontface = "bold"
    ) +
    
    geom_hline(
      data = data.frame(yint = baseline_hist),
      aes(yintercept = yint, linetype = "baseline"),
      linewidth = 0.5,
      colour = "black"
    ) +
    scale_linetype_manual(
      name   = NULL,
      values = c(baseline = "dashed"),
      labels = c(baseline = baseline_label)
    ) +
    
    theme(
      legend.title = element_blank(),
      legend.position = "right",
      strip.background = element_rect(fill = "lightgrey"),
      strip.text = element_text(size = 12, face = "bold")
    ) +
    scale_x_discrete(name = "Klimaszenario") +
    scale_y_continuous(name = "Zwiebel-Ertrag [t/ha]") +
    guides(
      linetype = guide_legend(order = 1),
      fill     = guide_legend(order = 2)
    )
  
  return(p)
}
