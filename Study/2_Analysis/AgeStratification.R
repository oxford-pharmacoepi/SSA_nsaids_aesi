age_strat_folder <- file.path(output_folder, "age_stratification")
if (!dir.exists(age_strat_folder)) dir.create(age_strat_folder, recursive = TRUE)

#add age and stratifying cohort
cli::cli_alert_info("Adding age variable and stratifying cohorts ({Sys.time()})")
info(logger, "ADDING AGE VARIABLE AND STRATIFYING COHORTS")

cdm$nsaids_age <- cdm$nsaids |>
  addAge(ageGroup = list("18_to_65" = c(18,64), "65_and_over" = c(65, Inf))) %>%
  stratifyCohorts(strata = "age_group", name = "nsaids_age")

info(logger, "ADDED AGE VARIABLE AND STRATIFYING COHORTS")

#run cohort symmetry for age-stratified data
cli::cli_alert_info("Running cohort symmetry for age-stratified cohorts ({Sys.time()})")
info(logger, "RUNNING COHORT SYMMETRY FOR AGE-STRATIFIED COHORTS")

tryCatch({
  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "nsaids_aesi_age",
    cohortDateRange = c(starting_date, ending_date),
    daysPriorObservation = 365,
    combinationWindow = c(0, 180),
    washoutWindow = 365,
    indexTable = "nsaids_age",
    markerTable = "aesi"
  )
  
  cli::cli_alert_success("- Generated SequenceCohortSet for nsaids-aesis-age")
  
  cli::cli_alert_info("- Generate SequenceRatios for nsaids-aesis-age")
  results_age <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_age"]])
  
  cli::cli_alert_success("- Generated SequenceRatios for nsaids-aesis-age")
  
}, error = function(e) {
  writeLines(as.character(e),
             here::here(age_strat_folder, paste0(db_name, "_cs_error.txt")))
})

cli::cli_alert_success("- Got cohort symmetry results")
info(logger, "RAN COHORT SYMMETRY FOR AGE-STRATIFIED COHORTS")

cli::cli_alert_info("- Export results for nsaids-aesis-age")
exportSummarisedResult(results_age,
                       path = here::here(age_strat_folder),
                       fileName = paste0(db_name, "_result_age.csv"))

info(logger, "EXPORTED AGE STRATIFIED COHORT SYMMETRY RESULTS")

#marker settings
marker_settings_age <- settings(cdm[["nsaids_aesi_age"]])
write_csv(marker_settings_age, here::here(age_strat_folder, paste0(cdmName(cdm), "_ssa_marker_settings_age.csv")))
info(logger, "WROTE MARKER SETTINGS CSV")

#attrition
attrition_seq_ratio_age <- attrition(cdm[["nsaids_aesi_age"]])
write_csv(attrition_seq_ratio_age, here::here(age_strat_folder, paste0(cdmName(cdm), "_ssa_attrition_age.csv")))
info(logger, "WROTE ATTRITION CSV")

#temporal plots
summary_temp_trends_months_age <- summariseTemporalSymmetry(cdm[["nsaids_aesi_age"]], timescale = "month")
write_csv(summary_temp_trends_months_age, here::here(age_strat_folder, paste0(cdmName(cdm), "_ssa_temporal_symmetry_summary_age.csv")))
info(logger, "WROTE TEMPORAL SEQUENCE SUMMARY CSV")

#prep data for temporal plots
cli::cli_alert_success("- Prepping data for temporal plots")
info(logger, "PREPPING DATA FOR TEMPORAL PLOTS")

prepped_temp_data <- summary_temp_trends_months_age |>
  dplyr::filter(variable_name == "temporal_symmetry", estimate_name == "count") |>
  dplyr::mutate(
    index = stringr::str_split_fixed(group_level, " &&& ", 2)[, 1],
    marker = stringr::str_split_fixed(group_level, " &&& ", 2)[, 2],
    strata_level = strata_level,
    index_name = stringr::str_remove(index, "_[0-9]{2,3}(to)?[0-9]{2,3}$"),
    time = as.integer(variable_level),
    count = as.integer(estimate_value)
  ) |>
  dplyr::select(index_name, marker, strata_level, time, count)

plotTemporalSymmetry1 <- function(result,
                                  plotTitle = NULL,
                                  labs = c("Time (months)", "Individuals (N)"),
                                  xlim = c(-12, 12),
                                  colours = c("blue", "red"),
                                  scales = "free") {
  
  
  plot_data <- result |>
    omopgenerics::splitGroup() |>
    dplyr::select(.data$index_name, .data$marker_name, .data$variable_name, .data$variable_level, .data$estimate_name, .data$estimate_value, .data$additional_level, .data$additional_name) |>
    dplyr::group_by(.data$estimate_name) |>
    dplyr::mutate(row = dplyr::row_number()) |>
    tidyr::pivot_wider(names_from = "variable_name",
                       values_from = "variable_level") |>
    tidyr::pivot_wider(names_from = "estimate_name",
                       values_from = "estimate_value") |>
    dplyr::select(-"row") |>
    dplyr::ungroup() |>
    dplyr::rename("time" = "temporal_symmetry") |>
    dplyr::filter(.data$time != 0) |>
    dplyr::mutate(colour = dplyr::if_else(.data$time > 0, "B", "A")) |>
    dplyr::mutate(index_name = paste0("Index: ", .data$index_name),
                  marker_name = paste0("Marker: ", .data$marker_name)) |>
    dplyr::mutate(count = as.integer(.data$count),
                  time = as.integer(.data$time)) |>
    dplyr::compute()
  
  colours = c("A" = colours[1], "B" = colours[2])
  
  width_range <- (xlim[2] - xlim[1])/2
  
  timescale_breaks <- if (grepl("months", labs[1], ignore.case = TRUE)) {
    seq(xlim[1], xlim[2], by = 1)
  } else if (grepl("days", labs[1], ignore.case = TRUE)) {
    seq(xlim[1], xlim[2], by = 52)
  } else {
    seq(xlim[1], xlim[2], by = 1)  # Default fallback
  }
  
  ggplot2::ggplot(data = plot_data, ggplot2::aes(
    x = .data$time, y = .data$count, fill = .data$colour)) +
    ggplot2::geom_col(width = 0.01*width_range) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$colour), size = 4) +
    ggplot2::coord_cartesian(xlim = c(xlim[1], xlim[2])) +
    ggplot2::labs(title = plotTitle, x = labs[1], y = labs[2]) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    ggplot2::scale_x_continuous(breaks = timescale_breaks) + 
    ggplot2::theme_minimal() +  # Use a minimal theme with a white background
    ggplot2::theme(legend.position = "none",
                   axis.line = ggplot2::element_line(colour = "black"),  # Make axis lines black
                   axis.ticks = ggplot2::element_line(colour = "black"),  # Make axis ticks black
                   axis.text = ggplot2::element_text(colour = "black", size = 20) ,   # Make axis text black
                   panel.grid.minor = ggplot2::element_blank() ,  # Remove minor grid lines
                   panel.grid.major.x = ggplot2::element_blank(),  # Remove vertical grid lines
                   panel.grid.major.y = ggplot2::element_line(colour = "grey96"),  # Keep horizontal grid lines
                   plot.title = ggplot2::element_text(hjust = 0.5),
                   strip.text = ggplot2::element_text(size = 20),  # Increase the facet strip labels' text size
                   axis.title = ggplot2::element_text(size = 20)
    ) +
    ggplot2::facet_wrap(~ index_name + marker_name, scales = scales) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_colour_manual(values = colours)
  
}



# Get index-marker combinations
cli::cli_alert_success("- Getting index-marker combinations")
info(logger, "GETTING INDEX MARKER COMBINATIONS")

index_age_combos <- summary_temp_trends_months_age |>
  dplyr::filter(variable_name == "temporal_symmetry", estimate_name == "count") |>
  dplyr::distinct(group_level) |>
  dplyr::mutate(
    index_strat = stringr::str_split_fixed(group_level, "&&&", 2)[, 1],
    marker_name = stringr::str_split_fixed(group_level, "&&&", 2)[, 2]
  )

info(logger, "GOT INDEX MARKER COMBINATIONS")

#Loop through for each combination
cli::cli_alert_success("- Looping through for each index-marker combinations")
info(logger, "LOOPING FOR EACH INDEX MARKER COMBINATIONS")

for (i in seq_len(nrow(index_age_combos))) {
  index_strat <- stringr::str_trim(index_age_combos$index_strat[i])
  marker <- stringr::str_trim(index_age_combos$marker_name[i])
  combo  <- paste(index_strat, "&&&", marker)
  
  filtered_result <- summary_temp_trends_months_age |>
    dplyr::filter(
      variable_name == "temporal_symmetry",
      estimate_name == "count",
      stringr::str_trim(group_level) == combo
    )
  
  if (nrow(filtered_result) == 0) {
    cli::cli_alert_warning(paste("Skipping:", combo, "- no data found"))
    next
  }
  

  age_group <- stringr::str_extract(index_strat, "(?<=_)(\\d+_to_\\d+)$")
  
  
#make temporal plots 
  #p_age_plot <- plotTemporalSymmetry1(
   # result = filtered_result,
  #  plotTitle = paste0("Temporal Trends - Age: ", age_group, " - ", index_strat)
  #)
  
  safe_index  <- stringr::str_replace_all(index_strat, "[^a-zA-Z0-9]", "_")
  safe_marker <- stringr::str_replace_all(marker, "[^a-zA-Z0-9]", "_")
  safe_age    <- stringr::str_replace_all(age_group, "[^a-zA-Z0-9]", "_")
  
  #plot_filename <- paste0("temporal_symmetry_age_", safe_age, "_", safe_index, "_", safe_marker, ".png")
  
  #png(file.path(age_strat_folder, plot_filename), width = 20, height = 10, units = "in", res = 300, type = "cairo")
  #print(p_age_plot)
  #dev.off()
}

cli::cli_alert_success("- Made temporal symmetry plots")
info(logger, "MADE TEMPORAL SYMMETRY PLOTS")

# Create scatter plot for ASRs
cli::cli_alert_success("- Creating ASR scatter plots")
info(logger, "CREATING ASR SCATTER PLOTS")

sr_tidy_age <- results_age |>
  omopgenerics::tidy() |>
  dplyr::mutate(
    index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
  ) |>
  dplyr::filter(variable_level == "sequence_ratio", variable_name == "adjusted") |>
  dplyr::mutate(pair = paste0(index_cohort_name, "->", marker_cohort_name)) |>
  dplyr::filter(point_estimate != Inf) |>
  dplyr::mutate(highlight = ifelse(lower_CI > 1, "Highlighted", "Not Highlighted")) |>
  dplyr::filter(abs(upper_CI - lower_CI) <= 10)

labs <- c("ASR", "Drug Pairs")

p_age <- visOmopResults::scatterPlot(
  sr_tidy_age,
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
  ggplot2::ylab(labs[1]) +
  ggplot2::xlab(labs[2]) +
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

srPlotName <- paste0("nsaids_aesi_age.png")
png(here::here(age_strat_folder, srPlotName), width = 8, height = 6, units = "in", res = 1500, type = "cairo")
print(p_age, newpage = FALSE)
dev.off()

info(logger, "CREATED ASR SCATTER PLOTS")

# Comparison plot of 18–65 vs 65+
cli::cli_alert_success("- Creating age group comparison ASR scatter plots")
info(logger, "CREATING AGE GROUP COMPARISON ASR SCATTER PLOTS")

sr_tidy_age <- results_age |>
  omopgenerics::tidy() |>
  dplyr::mutate(
    age_group = dplyr::case_when(
      stringr::str_detect(index_cohort_name, "18_to_65") ~ "18–65",
      stringr::str_detect(index_cohort_name, "65_and_over") ~ "65+",
      TRUE ~ NA_character_
    ),
    index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    index_cohort_name = stringr::str_remove(index_cohort_name, "_(18_to_65|65_and_over)$"),
    marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    pair = paste0(index_cohort_name, "->", marker_cohort_name)
  ) |>
  dplyr::filter(
    variable_level == "sequence_ratio",
    variable_name == "adjusted",
    point_estimate != Inf,
    abs(upper_CI - lower_CI) <= 10
  ) |>
  dplyr::filter(!is.na(age_group)) |>
  dplyr::mutate(
    highlight = ifelse(lower_CI > 1, "Highlighted", "Not Highlighted")
  )

p_age_comparison <- ggplot(sr_tidy_age, aes(
  x = index_cohort_name,
  y = point_estimate,
  ymin = lower_CI,
  ymax = upper_CI,
  shape = age_group,
  color = age_group
)) +
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.4) +
  facet_wrap(~ marker_cohort_name, scales = "free_y") +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(
    title = "18–65 vs 65+ Risk by NSAID and AESI",
    x = "NSAID",
    y = "Adjusted Sequence Ratio"
  ) +
  scale_shape_manual(values = c("18–65" = 16, "65+" = 17)) +  # ● for 18–65, ▲ for 65+
  scale_color_manual(values = c("18–65" = "#1f77b4", "65+" = "#d62728")) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold")
  )

# Save the comparison plot
srPlotName <- paste0("nsaids_aesi_risk_by_age", ".png")
png(here::here(age_strat_folder, srPlotName), width = 8, height = 6, units = "in", res = 1500, type = "cairo")
print(p_age_comparison, newpage = FALSE)
dev.off()

info(logger, "CREATED AGE GROUP COMPARISON ASR SCATTER PLOTS")
info(logger, "COMPLETED AGE STRATIFICATION ANALYSIS")
