htn_strat_folder <- file.path(output_folder, "hypertension_stratification")
if (!dir.exists(htn_strat_folder)) dir.create(htn_strat_folder, recursive = TRUE)

cli::cli_alert_success("- Running hypertension stratification analysis")
info(logger, "RUNNING HYPERTENSION STRATIFICATION ANALYSIS")

cdm <- CohortSymmetry::generateSequenceCohortSet(
  cdm = cdm,
  name = "nsaids_aesi_no_hypertension",
  cohortDateRange = c(starting_date, ending_date),
  daysPriorObservation = 365,
  combinationWindow = c(0, 365),
  washoutWindow = 365,
  indexTable = "nsaids_no_hypertension",
  markerTable = "aesi"
)

cdm <- CohortSymmetry::generateSequenceCohortSet(
  cdm = cdm,
  name = "nsaids_aesi_prior_hypertension",
  cohortDateRange = c(starting_date, ending_date),
  daysPriorObservation = 365,
  combinationWindow = c(0, 365),
  washoutWindow = 365,
  indexTable = "nsaids_prior_hypertension",
  markerTable = "aesi"
)

#no prior hypertension
cli::cli_alert_success("- Running no prior hypertension analysis")
info(logger, "RUNNING NO PRIOR HYPERTENSION ANALYSIS")

res <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$nsaids_aesi_no_hypertension)

sr_tidy_res <- res %>%
  omopgenerics::tidy() %>%
  dplyr::mutate(
    index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    pair = paste0(index_cohort_name, "->", marker_cohort_name),
    highlight = ifelse(lower_CI > 1, "Highlighted", "Not Highlighted")
  ) %>%
  dplyr::filter(
    variable_level == "sequence_ratio",
    variable_name == "adjusted",
    point_estimate != Inf,
    abs(upper_CI - lower_CI) <= 10
  )

labs_365 <- c("ASR", "Drug Pairs")

p_res <- visOmopResults::scatterPlot(
  sr_tidy_res,
  x = "index_cohort_name",
  y = "point_estimate",
  line = FALSE,
  point = TRUE,
  ribbon = FALSE,
  ymin = "lower_CI",
  ymax = "upper_CI",
  facet = "marker_cohort_name",
  colour = "highlight"
) +
  ggplot2::ylab(labs_365[1]) +
  ggplot2::xlab(labs_365[2]) +
  ggplot2::ylim(c(0, 10)) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::geom_hline(yintercept = 1, linetype = 2) +
  ggplot2::scale_shape_manual(values = rep(19, 5)) +
  ggplot2::theme(
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(),
    legend.position = "none",
    legend.title = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

# Save plot
p_res
srPlotName <- paste0("no_hypertension.png")
png(file.path(htn_strat_folder, srPlotName), width = 8, height = 6, units = "in", res = 1500, type = "cairo")
print(p_res, newpage = FALSE)
dev.off()

cli::cli_alert_success("- Ran no prior hypertension analysis")
info(logger, "RAN NO PRIOR HYPERTENSION ANALYSIS")

#prior hypertension
cli::cli_alert_success("- Running prior hypertension analysis")
info(logger, "RUNNING PRIOR HYPERTENSION ANALYSIS")

res_2 <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$nsaids_aesi_prior_hypertension)

sr_tidy_res_2 <- res_2 %>%
  omopgenerics::tidy() %>%
  dplyr::mutate(
    index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    pair = paste0(index_cohort_name, "->", marker_cohort_name),
    highlight = ifelse(lower_CI > 1, "Highlighted", "Not Highlighted")
  ) %>%
  dplyr::filter(
    variable_level == "sequence_ratio",
    variable_name == "adjusted",
    point_estimate != Inf,
    abs(upper_CI - lower_CI) <= 10
  )

labs_365 <- c("ASR", "Drug Pairs")

p_res_2 <- visOmopResults::scatterPlot(
  sr_tidy_res_2,
  x = "index_cohort_name",
  y = "point_estimate",
  line = FALSE,
  point = TRUE,
  ribbon = FALSE,
  ymin = "lower_CI",
  ymax = "upper_CI",
  facet = "marker_cohort_name",
  colour = "highlight"
) +
  ggplot2::ylab(labs_365[1]) +
  ggplot2::xlab(labs_365[2]) +
  ggplot2::ylim(c(0, 10)) +
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::geom_hline(yintercept = 1, linetype = 2) +
  ggplot2::scale_shape_manual(values = rep(19, 5)) +
  ggplot2::theme(
    panel.border = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(),
    legend.position = "none",
    legend.title = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(hjust = 0.5)
  )

p_res_2
srPlotName <- paste0("hypertension.png")
png(file.path(htn_strat_folder, srPlotName), width = 8, height = 6, units = "in", res = 1500, type = "cairo")
print(p_res_2, newpage = FALSE)
dev.off()

cli::cli_alert_success("- Ran prior hypertension analysis")
info(logger, "Ran PRIOR HYPERTENSION ANALYSIS")

#compare prior vs no prior hypertension 
cli::cli_alert_success("- Running prior vs no prior hypertension analysis")
info(logger, "RUNNING PRIOR VS NO PRIOR HYPERTENSION ANALYSIS")

# Add group label to each dataset
sr_tidy_res$hypertension_status <- "No Prior Hypertension"
sr_tidy_res_2$hypertension_status <- "Prior Hypertension"

# Combine both
sr_tidy_htn <- bind_rows(sr_tidy_res, sr_tidy_res_2)

# Plot
p_htn_comparison <- ggplot(sr_tidy_htn, aes(
  x = index_cohort_name,
  y = point_estimate,
  ymin = lower_CI,
  ymax = upper_CI,
  shape = hypertension_status,
  color = hypertension_status
)) +
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.4) +
  facet_wrap(~ marker_cohort_name, scales = "free_y") +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(
    title = "Risk of AESIs by Prior Hypertension Status",
    x = "NSAID",
    y = "Adjusted Sequence Ratio"
  ) +
  scale_shape_manual(values = c(
    "No Prior Hypertension" = 16,  # ●
    "Prior Hypertension" = 17      # ▲
  )) +
  scale_color_manual(values = c(
    "No Prior Hypertension" = "#1f77b4",
    "Prior Hypertension" = "#d62728"
  )) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Save plot
srPlotName <- paste0("hypertension_comparison", ".png")
png(here::here(htn_strat_folder, srPlotName), width = 8, height = 6, units = "in", res = 1500, type = "cairo")
print(p_htn_comparison, newpage = FALSE)
dev.off()

cli::cli_alert_success("- Ran prior vs no prior hypertension analysis")
info(logger, "RAN PRIOR VS NO PRIOR HYPERTENSION ANALYSIS")

cli::cli_alert_success("- Completed hypertension stratification analysis")
info(logger, "COMPLETED HYPERTENSION STRATIFICATION ANALYSIS")



