plot_yield_asparagus <- function(sim_results_scen)
{
  #easy yield graph
  sim_results_scen <-readRDS("/asparagus/MC_results/MC_results_scenarios.RDS")
  
  results_marktyield_today <-
    sim_results_scen$y %>% select(ends_with("_today")) %>% rename_with( ~ str_remove(.x, "_today"))
  results_marktyield_126 <-
    sim_results_scen$y %>% select(ends_with("_ssp1")) %>% rename_with( ~ str_remove(.x, "_ssp1"))
  results_marktyield_245 <-
    sim_results_scen$y %>% select(ends_with("_ssp2")) %>% rename_with( ~ str_remove(.x, "_ssp2"))
  results_marktyield_370 <-
    sim_results_scen$y %>% select(ends_with("_ssp3")) %>% rename_with( ~ str_remove(.x, "_ssp3"))
  results_marktyield_585 <-
    sim_results_scen$y %>% select(ends_with("_ssp5")) %>% rename_with( ~ str_remove(.x, "_ssp5"))
  
  #adding row for scenario identification
  results_marktyield_today$scenario <- as.character("Year 2020")
  results_marktyield_126$scenario <- as.character("Year 2075\nSSP1-2.6")
  results_marktyield_245$scenario <- as.character("Year 2075\nSSP2-4.5")
  results_marktyield_370$scenario <- as.character("Year 2075\nSSP3-7.0")
  results_marktyield_585$scenario <- as.character("Year 2075\nSSP5-8.5")
  #all together
  results_yield_all <-
    rbind(
      results_marktyield_today,
      results_marktyield_126,
      results_marktyield_245,
      results_marktyield_370,
      results_marktyield_585
    )
  #rename column
  names(results_yield_all) <-
    c("Ertrag", "vermarktbarer_Ertrag", "id", "scenario")
  #direktvergleich
  results_yield_all_longer <-
    pivot_longer(results_yield_all, cols = c(Ertrag, vermarktbarer_Ertrag))
  results_yield_all_longer$name <-
    factor(results_yield_all_longer$name,
           levels = c("Ertrag", "vermarktbarer_Ertrag"))
  
  results_yield_all_longer <- results_yield_all_longer %>%
    mutate(period = ifelse(scenario == "Year 2020", "2020", "2075"))
  results_yield_all_longer$period <-
    as.factor(results_yield_all_longer$period)
  results_yield_all_longer$scenario <-
    as.factor(results_yield_all_longer$scenario)
  
  #mittelwerte
  summary_df <- results_yield_all_longer %>%
    group_by(scenario, name) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      q25 = quantile(value, 0.25, na.rm = TRUE),
      q75 = quantile(value, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = name,
                values_from = c(mean_value, q25, q75)) %>%
    mutate(
      percent = (mean_value_vermarktbarer_Ertrag / mean_value_Ertrag) * 100,
      IQR_marketable_yield = q75_vermarktbarer_Ertrag - q25_vermarktbarer_Ertrag
    )
  
  
  plot <-
    ggplot(results_yield_all_longer,
           aes(x = scenario, y = value, fill = name)) +
    geom_boxplot(position = position_dodge(width = 0.8)) +
    geom_text(
      data = summary_df,
      aes(
        x = scenario,
        y = max(results_yield_all_longer$value, na.rm = TRUE) + 1,
        label = paste0(round(100-percent), "%")
      ),
      inherit.aes = FALSE,
      size = 4
    ) +
    theme(
      legend.title = element_blank(),
      legend.position = "right",
      legend.text = element_text(size = 16),
      strip.background = element_rect(fill = "lightgrey"),
      strip.text = element_text(size = 12, face = "bold")
    ) +
    scale_x_discrete(name = "Klimaszenario") +
    scale_y_continuous(name = "Ertrag [dt/ha]") +
    geom_hline(
      data = data.frame(yint = 56.3),
      aes(yintercept = yint, linetype = "baseline"),
      linewidth = 0.5,
      colour = "black"
    ) +
    scale_linetype_manual(
      name   = NULL,
      values = c(baseline = "dashed"),
      labels = c(baseline = "Durchschnittsertrag Regierungsbezirk\nKöln 56,3 dt/ha)")
    )+
    scale_fill_discrete(
      name   = NULL,
      labels = c("Potentieller Ertrag\nohne Schäden","Vermarktbarer Ertrag" )
    )+
    guides(
      linetype = guide_legend(order = 1),
      fill     = guide_legend(order = 2)
    )
  
  return(plot)
}

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggbeeswarm)

plot_yield_asparagus2(MC_results_scenarios)

plot_yield_asparagus2 <- function(sim_results_scen) {

  
  # --- 1) Szenario-Frames bauen (Suffix entfernen) ---
  mk <- function(sfx, lab) {
    sim_results_scen$y %>%
      dplyr::select(dplyr::ends_with(sfx)) %>%
      dplyr::rename_with(~ stringr::str_remove(.x, paste0(sfx, "$"))) %>%
      dplyr::mutate(scenario = lab)
  }
  
  results_yield_all <- dplyr::bind_rows(
    mk("_today", "Historisch"),                # vorher: "Year 2020"
    mk("_ssp1",  "SSP1-2.6"),
    mk("_ssp2",  "SSP2-4.5"),
    mk("_ssp3",  "SSP3-7.0"),
    mk("_ssp5",  "SSP5-8.5")
  )
  
  # Spalten benennen (ggf. anpassen)
  names(results_yield_all) <- c("Ertrag", "vermarktbarer_Ertrag", "id", "scenario")
  
  # --- 2) Long-Format + Jahr-Gruppe (Facet) ---
  results_yield_all_longer <- results_yield_all %>%
    tidyr::pivot_longer(cols = c(Ertrag, vermarktbarer_Ertrag)) %>%
    mutate(
      # Facet-Gruppe (Überschrift oben)
      year_grp = if_else(scenario == "Historisch", "Historisch", "Jahr 2075"),
      # gewünschte Reihenfolge auf der x-Achse (Labels unten)
      scenario = factor(
        scenario,
        levels = c("Historisch", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
      ),
      # Legendenlabels erst im Scale setzen; hier nur Reihenfolge der beiden Reihen
      name = factor(name, levels = c("Ertrag", "vermarktbarer_Ertrag"))
    )
  
  # --- 3) Summary für Prozent-Label (rohe Namen verwenden) ---
  summary_df <- results_yield_all_longer %>%
    dplyr::group_by(scenario, name) %>%
    dplyr::summarise(mean_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = name, values_from = mean_value) %>%
    dplyr::mutate(
      percent  = (`vermarktbarer_Ertrag` / `Ertrag`) * 100,
      year_grp = if_else(scenario == "Historisch", "Historisch", "Jahr 2075")
    )
  
  # --- top vom boxplot rausfinden ---
  whisk_df <- results_yield_all_longer |>
    dplyr::group_by(year_grp, scenario, name) |>
    dplyr::summarise(upper = boxplot.stats(value)$stats[5], .groups = "drop")
  
  label_df <- whisk_df |>
    dplyr::group_by(year_grp, scenario) |>
    dplyr::summarise(y_pos = max(upper), .groups = "drop") |>
    dplyr::left_join(
      summary_df |> dplyr::select(year_grp, scenario, percent),
      by = c("year_grp","scenario")
    )
  
  y_lab_global <- max(label_df$y_pos, na.rm = TRUE) * 1.04
  label_df2 <- dplyr::mutate(label_df, y_lab = y_lab_global)
  
  # --- Plot direkt mit y_lab nutzen ---
  plot <- ggplot(results_yield_all_longer,
                 aes(x = scenario, y = value*10, fill = name)) +
    geom_boxplot(position = position_dodge(width = 0.8), outlier.shape = NA) +
    geom_text(
      data = label_df2,
      aes(x = scenario, y = y_lab*10, label = paste0(round(100 - percent), "%"),colour = "Prozent-Label"),
      vjust = -0.3, size = 4, inherit.aes = FALSE
    )+
    geom_hline(
      aes(yintercept = 56.3, linetype = "baseline"),
      colour = "black", linewidth = 0.5, inherit.aes = FALSE
    ) +
    facet_grid(~ year_grp, scales = "free_x", space = "free_x") +
    coord_cartesian(clip = "off") +
    scale_y_continuous(name = "Ertrag [dt/ha]",
                       expand = expansion(mult = c(0.02, 0.10))) +
    scale_x_discrete(name = "Klimaszenario") +
    scale_fill_manual(
      name   = NULL,
      values = c("Ertrag" = "#377eb8", "vermarktbarer_Ertrag" = "#4daf4a"),
      labels = c("Ertrag" = "Potentieller Ertrag\nohne Schäden",
                 "vermarktbarer_Ertrag" = "Vermarktbarer Ertrag")
    ) +
    scale_linetype_manual(
      name   = NULL,
      values = c(baseline = "dashed"),
      labels = c(baseline = "Durchschnittsertrag Regierungsbezirk\nKöln 56,3 dt/ha")
    ) +
    scale_color_manual(
      name   = NULL,
      values = c("Prozent-Label" = "black"),
      labels="Verlust durch Schäden"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position  = "right",
      legend.text      = element_text(size = 16),
      strip.placement  = "outside",
      strip.background = element_rect(fill = "lightgrey", colour = NA),
      strip.text       = element_text(size = 12, face = "bold"),
      panel.border     = element_rect(colour = "grey40", fill = NA, linewidth = 0.6),
      panel.spacing.x  = grid::unit(10, "pt")
    )+
    guides(
      colour = guide_legend(
        order = 1,
        override.aes = list(label = "%", size = 5)),
      linetype = guide_legend(order = 2),
      fill     = guide_legend(order = 3)
    )
  return(plot)
}

