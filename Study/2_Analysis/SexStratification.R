sex_strat_folder <- file.path(output_folder, "sex_stratification")
if (!dir.exists(sex_strat_folder)) dir.create(sex_strat_folder, recursive = TRUE)

# Add sex variable and stratify cohorts
cli::cli_alert_info("Adding sex variable and stratifying cohorts ({Sys.time()})")
info(logger, "ADDING SEX VARIABLE AND STRATIFYING COHORTS")

cdm$nsaids_sex <- cdm$nsaids |>
  addSex() %>%
  stratifyCohorts(strata = "sex", name = "nsaids_sex")

info(logger, "ADDED SEX VARIABLE AND STRATIFYING COHORTS")

# Run cohort symmetry analysis for sex-stratified data
cli::cli_alert_info("Running cohort symmetry for sex stratified data ({Sys.time()})")
info(logger, "RUNNING COHORT SYMMETRY FOR SEX STRATIFIED DATA")

tryCatch({
  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "nsaids_aesi_sex",
    cohortDateRange = c(starting_date, ending_date),
    daysPriorObservation = 365,
    combinationWindow = c(0, 180),
    washoutWindow = 365,
    indexTable = "nsaids_sex",
    markerTable = "aesi"
  )
  
  cli::cli_alert_success("- Generated SequenceCohortSet for nsaids-aesis-sex")
  info(logger, "RAN COHORT SYMMETRY FOR SEX STRATIFIED DATA")
  
  results_sex <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_sex"]])
  cli::cli_alert_success("- Generated SequenceRatios for nsaids-aesis-sex")
}, error = function(e) {
  writeLines(as.character(e),
             here(sex_strat_folder, paste0(db_name, "_cs_error.txt")))
})

cli::cli_alert_success("- Got cohort symmetry results")
info(logger, "GOT COHORT SYMMETRY RESULTS")

cli::cli_alert_info("- Export results for nsaids-aesis-sex")
# export the results (summarised only)
exportSummarisedResult(results_sex, 
                       path = here::here(sex_strat_folder), 
                       fileName = paste0(db_name,"_result_sex.csv"))


info(logger, "EXPORTED SUMMARISED RESULTS")

marker_settings_sex <- settings(cdm[["nsaids_aesi_sex"]])
write.csv(marker_settings_sex, file.path(sex_strat_folder, paste0(cdmName(cdm), "_ssa_marker_settings_sex.csv")))
info(logger, "WROTE CSV FOR MARKER SETTINGS")


#attrition
attrition_seq_ratio_sex <- attrition(cdm[["nsaids_aesi_sex"]])
write.csv(attrition_seq_ratio_sex, file.path(sex_strat_folder, paste0(cdmName(cdm), "_ssa_attrition_sex.csv")))
info(logger, "WROTE CSV FOR ATTRITION SEQ RATIO SEX")

#temporal symmetry plots 
summary_temp_trends_months_sex <- summariseTemporalSymmetry(
  cohort = cdm[["nsaids_aesi_sex"]],
  timescale = "month"
)
write_csv(summary_temp_trends_months_sex, file.path(sex_strat_folder, paste0(cdmName(cdm), "_ssa_temporal_symmetry_summary_sex.csv")))
info(logger, "WROTE CSV FOR TEMPORAL SEQUENCE SUMMARY")


#365 day analysis
cdm <- CohortSymmetry::generateSequenceCohortSet(
  cdm = cdm,
  name = "nsaids_aesi_sex_365",
  cohortDateRange = c(starting_date, ending_date),
  daysPriorObservation = 365,
  combinationWindow = c(0, 365),
  washoutWindow = 365,
  indexTable = "nsaids_sex",
  markerTable = "aesi"
)

results_sex_365 <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_sex_365"]])

exportSummarisedResult(results_sex_365, 
                       path = here::here(sex_strat_folder), 
                       fileName = paste0(db_name,"_result_sex_365.csv"))

#90 day analysis
cdm <- CohortSymmetry::generateSequenceCohortSet(
  cdm = cdm,
  name = "nsaids_aesi_sex_90",
  cohortDateRange = c(starting_date, ending_date),
  daysPriorObservation = 365,
  combinationWindow = c(0, 90),
  washoutWindow = 365,
  indexTable = "nsaids_sex",
  markerTable = "aesi"
)

results_sex_90 <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_sex_90"]])

exportSummarisedResult(results_sex_90, 
                       path = here::here(sex_strat_folder), 
                       fileName = paste0(db_name,"_result_sex_90.csv"))

# Prep data for temporal plot
cli::cli_alert_info("Prepping data for temporal plots ({Sys.time()})")
info(logger, "PREPPING DATA FOR TEMPORAL PLOTS")

# prepped_temp_data <- summary_temp_trends_months_sex |>
#   dplyr::filter(variable_name == "temporal_symmetry", estimate_name == "count") |>
#   dplyr::mutate(
#     index = stringr::str_split_fixed(group_level, " &&& ", 2)[, 1],
#     marker = stringr::str_split_fixed(group_level, " &&& ", 2)[, 2],
#     strata_level = dplyr::case_when(
#       stringr::str_detect(index, "_male$") ~ "Male",
#       stringr::str_detect(index, "_female$") ~ "Female",
#       TRUE ~ "Unknown"
#     ),
#     index_name = stringr::str_remove(index, "_(male|female)$"),
#     time = as.integer(variable_level),
#     count = as.integer(estimate_value)
#   ) |>
#   dplyr::select(index_name, marker, strata_level, time, count)
# 
# plotTemporalSymmetry1 <- function(result,
#                                   plotTitle = NULL,
#                                   labs = c("Time (months)", "Individuals (N)"),
#                                   xlim = c(-12, 12),
#                                   colours = c("blue", "red"),
#                                   scales = "free") {
#   
#   
#   plot_data <- result |>
#     omopgenerics::splitGroup() |>
#     dplyr::select(.data$index_name, .data$marker_name, .data$variable_name, .data$variable_level, .data$estimate_name, .data$estimate_value, .data$additional_level, .data$additional_name) |>
#     dplyr::group_by(.data$estimate_name) |>
#     dplyr::mutate(row = dplyr::row_number()) |>
#     tidyr::pivot_wider(names_from = "variable_name",
#                        values_from = "variable_level") |>
#     tidyr::pivot_wider(names_from = "estimate_name",
#                        values_from = "estimate_value") |>
#     dplyr::select(-"row") |>
#     dplyr::ungroup() |>
#     dplyr::rename("time" = "temporal_symmetry") |>
#     dplyr::filter(.data$time != 0) |>
#     dplyr::mutate(colour = dplyr::if_else(.data$time > 0, "B", "A")) |>
#     dplyr::mutate(index_name = paste0("Index: ", .data$index_name),
#                   marker_name = paste0("Marker: ", .data$marker_name)) |>
#     dplyr::mutate(count = as.integer(.data$count),
#                   time = as.integer(.data$time)) |>
#     dplyr::compute()
#   
#   colours = c("A" = colours[1], "B" = colours[2])
#   
#   width_range <- (xlim[2] - xlim[1])/2
#   
#   timescale_breaks <- if (grepl("months", labs[1], ignore.case = TRUE)) {
#     seq(xlim[1], xlim[2], by = 1)
#   } else if (grepl("days", labs[1], ignore.case = TRUE)) {
#     seq(xlim[1], xlim[2], by = 52)
#   } else {
#     seq(xlim[1], xlim[2], by = 1)  # Default fallback
#   }
#   
#   if (nrow(filtered_result) == 0) {
#     cli::cli_alert_warning(paste("Skipping plot for", index, "→", marker, "- no data found"))
#     next
#   }
#   
#   ggplot2::ggplot(data = plot_data, ggplot2::aes(
#     x = .data$time, y = .data$count, fill = .data$colour)) +
#     ggplot2::geom_col(width = 0.01*width_range) +
#     ggplot2::geom_point(ggplot2::aes(colour = .data$colour), size = 4) +
#     ggplot2::coord_cartesian(xlim = c(xlim[1], xlim[2])) +
#     ggplot2::labs(title = plotTitle, x = labs[1], y = labs[2]) +
#     scale_y_continuous(expand = expansion(mult = c(0, .1))) +
#     ggplot2::scale_x_continuous(breaks = timescale_breaks) + 
#     ggplot2::theme_minimal() +  # Use a minimal theme with a white background
#     ggplot2::theme(legend.position = "none",
#                    axis.line = ggplot2::element_line(colour = "black"),  # Make axis lines black
#                    axis.ticks = ggplot2::element_line(colour = "black"),  # Make axis ticks black
#                    axis.text = ggplot2::element_text(colour = "black", size = 20) ,   # Make axis text black
#                    panel.grid.minor = ggplot2::element_blank() ,  # Remove minor grid lines
#                    panel.grid.major.x = ggplot2::element_blank(),  # Remove vertical grid lines
#                    panel.grid.major.y = ggplot2::element_line(colour = "grey96"),  # Keep horizontal grid lines
#                    plot.title = ggplot2::element_text(hjust = 0.5),
#                    strip.text = ggplot2::element_text(size = 20),  # Increase the facet strip labels' text size
#                    axis.title = ggplot2::element_text(size = 20)
#     ) +
#     ggplot2::facet_wrap(~ index_name + marker_name, scales = scales) +
#     ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
#     ggplot2::scale_fill_manual(values = colours) +
#     ggplot2::scale_colour_manual(values = colours)
#   
# }

# Get index-marker combinations from group_level 
cli::cli_alert_success("- Getting index-marker combinations")
info(logger, "GETTING INDEX-MARKER COMBINATIONS")

index_sex_combos <- summary_temp_trends_months_sex |>
  dplyr::filter(variable_name == "temporal_symmetry", estimate_name == "count") |>
  dplyr::distinct(group_level) |>
  dplyr::mutate(
    index_name  = stringr::str_trim(stringr::str_split_fixed(group_level, "&&&", 2)[, 1]),
    marker_name = stringr::str_trim(stringr::str_split_fixed(group_level, "&&&", 2)[, 2])
  )

info(logger, "GOT INDEX-MARKER COMBINATIONS")

# Loop through each combination
cli::cli_alert_success("- Looping through each index-marker combinations")
info(logger, "LOOPING THROUGH EACH INDEX-MARKER COMBINATIONS")

# for (i in seq_len(nrow(index_sex_combos))) {
#   index  <- index_sex_combos$index_name[i]
#   marker <- index_sex_combos$marker_name[i]
#   combo  <- paste(index, "&&&", marker)
#   
#   
#   filtered_result <- summary_temp_trends_months_sex |>
#     dplyr::filter(
#       variable_name == "temporal_symmetry",
#       estimate_name == "count",
#       stringr::str_trim(group_level) == combo
#     )
#   
#   if (nrow(filtered_result) == 0) {
#     cli::cli_alert_warning(paste("Skipping:", combo, "- no data found"))
#     next
#   }
#   
#   sex <- stringr::str_extract(index, "(?<=_)(male|female)$")
#   
#   
#   # Make plot
#   
#   p_split_plot <- plotTemporalSymmetry1(
#     result = filtered_result,
#     plotTitle = paste0("Temporal Trends - ", sex, " - ", index)
#   )
#   
#   
#   safe_index  <- stringr::str_replace_all(index, "[^a-zA-Z0-9]", "_")
#   safe_marker <- stringr::str_replace_all(marker, "[^a-zA-Z0-9]", "_")
#   safe_sex    <- ifelse(is.na(sex), "unknown", stringr::str_to_lower(sex))
#   
#   plot_filename <- paste0("temporal_symmetry_", safe_sex, "_", safe_index, "_", safe_marker, ".png")
#   
#   png(file.path(sex_strat_folder, plot_filename), width = 20, height = 10, units = "in", res = 300, type = "cairo")
#   print(p_split_plot)
#   dev.off()
# 
# }

cli::cli_alert_success("- Made temporal symmetry plots")
info(logger, "MADE TEMPORAL SYMMETRY PLOTS")


# Create scatter plot for ASRs
cli::cli_alert_success("- Creating scatter plots for ASRs")
info(logger, "CREATING SCATTER PLOTS FOR ASRs")

# sr_tidy_sex <- results_sex |>
#   omopgenerics::tidy() |>
#   dplyr::mutate(
#     index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
#     marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
#   ) |>
#   dplyr::filter(variable_level == "sequence_ratio", variable_name == "adjusted") |>
#   dplyr::mutate(pair = paste0(index_cohort_name, "->", marker_cohort_name)) |>
#   dplyr::filter(point_estimate != Inf) |>
#   dplyr::mutate(highlight = ifelse(lower_CI > 1, "Highlighted", "Not Highlighted")) |>
#   dplyr::filter(abs(upper_CI - lower_CI) <= 10)
# 
# labs <- c("ASR", "Drug Pairs")
# 
# p_sex <- visOmopResults::scatterPlot(
#   sr_tidy_sex,
#   x = "index_cohort_name",
#   y = "point_estimate",
#   line = FALSE,
#   point = TRUE,
#   ribbon = FALSE,
#   ymin = "lower_CI",
#   ymax = "upper_CI",
#   facet = "marker_cohort_name",
#   colour = "highlight"
# ) +
#   ggplot2::ylab(labs[1]) +
#   ggplot2::xlab(labs[2]) +
#   ggplot2::ylim(c(0, 10)) +
#   ggplot2::coord_flip() +
#   ggplot2::theme_bw() +
#   ggplot2::geom_hline(yintercept = 1, linetype = 2) +
#   ggplot2::scale_shape_manual(values = rep(19, 5)) +
#   ggplot2::theme(
#     panel.border = ggplot2::element_blank(),
#     axis.line = ggplot2::element_line(),
#     legend.position = "none",
#     legend.title = ggplot2::element_blank(),
#     plot.title = ggplot2::element_text(hjust = 0.5)
#   )
# 
# # Save scatter plot
# srPlotName <- paste0("nsaids_aesi_sex.png")
# png(file.path(sex_strat_folder, srPlotName), width = 8, height = 6, units = "in", res = 1500, type = "cairo")
# print(p_sex, newpage = FALSE)
# dev.off()

info(logger, "CREATED SCATTER PLOTS FOR ASRs")

# This plot compares male vs female risk for each AESI by NSAID
cli::cli_alert_success("- Creating male vs female comparison scatter plots")
info(logger, "CREATING MALE VS FEMALE COMPARISON SCATTER PLOTS")

# sr_tidy_sex <- results_sex %>%
#   omopgenerics::tidy() %>%
#   dplyr::mutate(
#     # Extract sex BEFORE cleaning names
#     sex = dplyr::case_when(
#       stringr::str_detect(index_cohort_name, "_female$") ~ "Female",
#       stringr::str_detect(index_cohort_name, "_male$") ~ "Male",
#       TRUE ~ "Unspecified"
#     ),
#     # Clean cohort names AFTER extracting sex
#     index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
#     index_cohort_name = stringr::str_remove(index_cohort_name, "_(female|male)$"),
#     marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
#     pair = paste0(index_cohort_name, "->", marker_cohort_name)
#   ) %>%
#   dplyr::filter(
#     variable_level == "sequence_ratio",
#     variable_name == "adjusted",
#     point_estimate != Inf,
#     abs(upper_CI - lower_CI) <= 10
#   ) %>%
#   dplyr::mutate(
#     highlight = ifelse(lower_CI > 1, "Highlighted", "Not Highlighted"))
# 
# p_sex_comparison <- ggplot(sr_tidy_sex, aes(
#   x = index_cohort_name,
#   y = point_estimate,
#   ymin = lower_CI,
#   ymax = upper_CI,
#   shape = sex,
#   color = sex
# )) +
#   geom_pointrange(position = position_dodge(width = 0.5), size = 0.4) +
#   facet_wrap(~ marker_cohort_name, scales = "free_y") +
#   coord_flip() +
#   theme_bw() +
#   geom_hline(yintercept = 1, linetype = 2) +
#   labs(
#     title = "Male vs Female Risk by NSAID and AESI",
#     x = "NSAID",
#     y = "Adjusted Sequence Ratio"
#   ) +
#   scale_shape_manual(values = c("Male" = 17, "Female" = 16)) +  # ▲ triangle for Male, ● circle for Female
#   scale_color_manual(values = c("Male" = "#1f77b4", "Female" = "#d62728")) +
#   theme(
#     legend.position = "top",
#     legend.title = element_blank(),
#     strip.text = element_text(face = "bold"))
# 
# 
# srPlotName <- paste0("nsaids_aesi_risk_by_sex", ".png")
# png(here::here(sex_strat_folder, srPlotName), width = 8, height = 6, units = "in", res = 1500, type = "cairo")
# print(p_sex_comparison, newpage = FALSE)
# dev.off()

info(logger, "CREATED MALE VS FEMALE COMPARISON SCATTER PLOTS")
info(logger, "COMPLETED SEX STRATIFICATION ANALYSIS")
