# run cohort symmetry on all index-marker pairs and controls (365-day version)

sensitivity_folder <- file.path(output_folder, "sensitivity_365")
if (!dir.exists(sensitivity_folder)) dir.create(sensitivity_folder, recursive = TRUE)

########################
# positive controls (we know has a signal)
########################
cli::cli_alert_info("- Generate SequenceCohortSet (365d) for positive controls")

cdm <- CohortSymmetry::generateSequenceCohortSet(
  cdm = cdm,
  name = "amiodarone_levothyroxine_365",
  cohortDateRange = c(starting_date, ending_date),
  indexTable = "amiodarone",
  markerTable = "levothyroxine",
  daysPriorObservation = 365,
  washoutWindow = 365,
  combinationWindow = c(0, 365)
)

amiodarone_levothyroxine_365 <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_levothyroxine_365)
amiodarone_levothyroxine_365_results <- CohortSymmetry::tableSequenceRatios(result = amiodarone_levothyroxine_365, type = "tibble")

cli::cli_alert_success("- Generated 365d SequenceCohortSet for positive controls")

##############################
# negative controls (we know there is no signal)
##############################
cli::cli_alert_info("- Generate SequenceCohortSet (365d) for negative controls")

cdm <- CohortSymmetry::generateSequenceCohortSet(
  cdm = cdm,
  name = "amiodarone_allopurinol_365",
  cohortDateRange = c(starting_date, ending_date),
  indexTable = "amiodarone",
  markerTable = "allopurinol",
  daysPriorObservation = 365,
  washoutWindow = 365,
  combinationWindow = c(0, 365)
)

amiodarone_allopurinol_365 <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_allopurinol_365)
amiodarone_allopurinol_365_results <- CohortSymmetry::tableSequenceRatios(result = amiodarone_allopurinol_365, type = "tibble")

cli::cli_alert_success("- Generated 365d SequenceCohortSet for negative controls")

##############################
# main study nsaids (index) - aesi (markers)
##############################
cli::cli_alert_info("- Generate SequenceCohortSet (365d) for nsaids-aesis")

tryCatch({
  
  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "nsaids_aesi_365",
    cohortDateRange = c(starting_date, ending_date),
    daysPriorObservation = 365,
    combinationWindow = c(0, 365),
    washoutWindow = 365,
    indexTable = "nsaids",
    markerTable = "aesi"
  )
  
  cli::cli_alert_success("- Generated SequenceCohortSet for nsaids-aesis (365d)")
  
  cli::cli_alert_info("- Generate SequenceRatios for nsaids-aesis (365d)")
  results_cs_365 <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_365"]])
  cli::cli_alert_success("- Generated SequenceRatios for nsaids-aesis (365d)")

}, error = function(e) { 
  writeLines(as.character(e), 
             here::here(sensitivity_folder, paste0(db_name, "_cs_365_error.txt"))) 
})

cli::cli_alert_success("- Got cohort symmetry results (365d)")

cli::cli_alert_info("- Export results for nsaids-aesis (365d)")

exportSummarisedResult( results_cs_365, 
                        path = here::here(sensitivity_folder), 
                        fileName = paste0(db_name, "_result_365.csv") )

# Export marker settings
marker_settings_365 <- settings(cdm[["nsaids_aesi_365"]])
write_csv(marker_settings_365,
          here::here(sensitivity_folder, paste0(cdmName(cdm), "_ssa_marker_settings_365.csv")))

# Export attrition table
attrition_seq_ratio_365 <- attrition(cdm[["nsaids_aesi_365"]])
write_csv(attrition_seq_ratio_365,
          here::here(sensitivity_folder, paste0(cdmName(cdm), "_ssa_attrition_365.csv")))

# Temporal sequence summary
summary_temp_trends_months_365 <- summariseTemporalSymmetry(cohort = cdm[["nsaids_aesi_365"]], timescale = "month")
write_csv(summary_temp_trends_months_365,
          here::here(sensitivity_folder, paste0(cdmName(cdm), "_ssa_temporal_symmetry_summary_365.csv")))

# Generate tidy data for plotting
cli::cli_alert_info("- Make a pretty plot for nsaids-aesis (365d)")

sr_tidy_365 <- results_cs_365 %>%
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

p_365 <- visOmopResults::scatterPlot(
  sr_tidy_365,
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
p_365

srPlotName <- paste0("nsaids_aesi_365", ".png")
png(here::here(sensitivity_folder, srPlotName), width = 8, height = 6, units = "in", res = 1500, type = "cairo") 
    print(p_365, newpage = FALSE) 
    dev.off()


