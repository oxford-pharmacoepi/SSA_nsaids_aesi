# Create output folder for symmetry analysis
symmetry_folder <- file.path(output_folder, "symmetry")
if (!dir.exists(symmetry_folder)) dir.create(symmetry_folder, recursive = TRUE)

cli::cli_alert_info("- Running main analysis cohort symmetry")
info(logger, "RUNNING MAIN ANALYSIS COHORT SYMMETRY")

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

cli::cli_alert_info("- Generate SequenceCohortSet for nsaids-aesis")
info(logger, "GENERATE SEQUENCE COHORT SET FOR NSAIDS AESI")

tryCatch({
  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "nsaids_aesi",
    cohortDateRange = c(starting_date, ending_date),
    daysPriorObservation = 365,
    combinationWindow = c(0, 180),
    washoutWindow = 365,
    indexTable = "nsaids",
    markerTable = "aesi"
  )
  
  cli::cli_alert_success("- Generated SequenceCohortSet for nsaids-aesis")
  info(logger, "GENERATED SEQUENCE COHORT SET FOR NSAIDS AESI")
  
  cli::cli_alert_info("- Generate SequenceRatios for nsaids-aesis")
  info(logger, "GENERATE SEQUENCE RATIOS FOR NSAIDS AESI")
  
  results_cs <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi"]])
  
  cli::cli_alert_success("- Generated SequenceRatios for nsaids-aesis")
  info(logger, "GENERATED SEQUENCE RATIOS FOR NSAIDS AESI")
  
}, error = function(e) {
  cli::cli_alert_danger("- Cohort generation failed")
  writeLines(as.character(e),
             here::here(symmetry_folder, paste0(db_name, "_cs_error.txt")))
})

cli::cli_alert_success("- Got cohort symmetry results")
info(logger, "GOT COHORT SYMMETRY RESULTS")

cli::cli_alert_info("- Export results for nsaids-aesis")

info(logger, "EXPORTING RESULTS")

exportSummarisedResult(
  results_cs,
  path = here::here(symmetry_folder),
  fileName = paste0(db_name, "_result.csv")
)
info(logger, "EXPORTED RESULTS")

#marker settings
marker_settings <- settings(cdm[["nsaids_aesi"]])
write.csv(marker_settings, here::here(symmetry_folder, paste0(cdmName(cdm), "_ssa_marker_settings.csv")))
info(logger, "WROTE MARKER SETTINGS CSV")

#attrition
attrition_seq_ratio <- attrition(cdm[["nsaids_aesi"]])
write.csv(attrition_seq_ratio, here::here(symmetry_folder, paste0(cdmName(cdm), "_ssa_attrition.csv")))
info(logger, "WROTE ATTRITION CSV")

#temporal plots 
summary_temp_trends_months <- summariseTemporalSymmetry(cdm[["nsaids_aesi"]], timescale = "month")
write.csv(summary_temp_trends_months, file.path(symmetry_folder, paste0(cdmName(cdm), "_ssa_temporal_symmetry_summary.csv")))
info(logger, "WROTE TEMPORAL PLOTS CSV")


# Prep for plotting
cli::cli_alert_success("- Prepping data for temporal symmetry plots")
info(logger, "PREPPING DATA FOR TEMPORAL SYMMETRY PLOTS")

prepped_temp_data <- summary_temp_trends_months |>
  omopgenerics::splitGroup() |>
  dplyr::filter(variable_name == "temporal_symmetry", estimate_name == "count") |>
  dplyr::mutate(
    index = stringr::str_replace(index_name, "[^a-zA-Z0-9]", "_"),
    marker = stringr::str_replace(marker_name, "[^a-zA-Z0-9]", "_"),
    time = as.integer(variable_level),
    count = as.integer(estimate_value)
  ) |>
  dplyr::select(index, marker, time, count)

cli::cli_alert_success("- Prepped data for temporal symmetry plots")
info(logger, "PREPPED DATA FOR TEMPORAL SYMMETRY PLOTS")

# Get combinations of NSAID and AESI
cli::cli_alert_success("- Generating individual temporal symmetry plots")
info(logger, "GENERATING INDIVIDUAL TEMPORAL SYMMETRY PLOTS")

index_marker_combos <- prepped_temp_data |>
  dplyr::distinct(index, marker)

# Loop through each index–marker pair to make individual plots
# for (i in seq_len(nrow(index_marker_combos))) {
#   index <- index_marker_combos$index[i]
#   marker <- index_marker_combos$marker[i]
#   combo <- paste(index, "&&&", marker)
#   
#   filtered_result <- summary_temp_trends_months |>
#     dplyr::filter(
#       variable_name == "temporal_symmetry",
#       estimate_name == "count",
#       group_level == combo
#     )
#   
#   if (nrow(filtered_result) == 0) {
#     cli::cli_alert_warning(paste("Skipping plot for", index, "→", marker, "- no data found"))
#     next
#   }
  
  #p_individual_plot <- plotTemporalSymmetry1(
   # result = filtered_result,
    #plotTitle = paste0("Temporal Trends - ", index, " → ", marker)
  #)
  
  #plot_filename <- paste0("temporal_symmetry_", index, "_", marker, ".png")
  #png(file.path(symmetry_folder, plot_filename), width = 20, height = 10, units = "in", res = 300, type = "cairo")
  #print(p_individual_plot)
  #dev.off()
# }

cli::cli_alert_success("- Generated individual temporal symmetry plots")
info(logger, "GENERATED INDIVIDUAL TEMPORAL SYMMETRY PLOTS")

cli::cli_alert_success("- Generating record trends")
info(logger, "GENERATING RECORD TRENDS")

record_trends_overall_index <- cdm[["nsaids"]] %>%
  filter(cohort_start_date >= starting_date, cohort_start_date <= ending_date) %>%
  mutate(year = year(cohort_start_date)) %>%
  group_by(cohort_definition_id, year) %>%
  summarise(n_records = n(), .groups = "drop") %>%
  collect() %>%
  left_join(
    settings(cdm$nsaids) %>%
      select(cohort_definition_id, cohort_name),
    by = "cohort_definition_id"
  )

record_trends_overall_marker <- cdm[["aesi"]] %>%
  filter(cohort_start_date >= starting_date, cohort_start_date <= ending_date) %>%
  mutate(year = year(cohort_start_date)) %>%
  group_by(cohort_definition_id, year) %>%
  summarise(n_records = n(), .groups = "drop") %>%
  collect() %>%
  left_join(
    settings(cdm$aesi) %>%
      select(cohort_definition_id, cohort_name),
    by = "cohort_definition_id"
  )

drug_exposure_summary <- cdm$drug_exposure %>%
  filter(drug_exposure_start_date >= !!starting_date,
         drug_exposure_start_date <= !!ending_date) %>%
  mutate(year = year(drug_exposure_start_date)) %>%
  group_by(year) %>%
  summarise(n_records = n(), .groups = "drop") %>%
  mutate(name = "overall") %>%
  collect()

record_trends_overall <- bind_rows(
  record_trends_overall_index,
  record_trends_overall_marker,
  drug_exposure_summary
)

write_csv(summary_temp_trends_months, file.path(symmetry_folder, paste0(cdmName(cdm), "_ssa_temporal_symmetry_summary.csv")))

cli::cli_alert_success("- Generated record trends")
info(logger, "GENERATED RECORD TRENDS")

cli::cli_alert_info("- Making a pretty plot for nsaids-aesis")
info(logger, "MAKING ASR SCATTER PLOT")

# sr_tidy <- results_cs |>
#   omopgenerics::tidy() |>
#   dplyr::mutate(
#     index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
#     marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
#   ) |>
#   dplyr::filter(variable_level == "sequence_ratio" & variable_name == "adjusted") |>
#   dplyr::mutate(pair = paste0(index_cohort_name, "->", marker_cohort_name)) |>
#   filter(point_estimate != Inf) |>
#   mutate(highlight = ifelse(lower_CI > 1, "Highlighted", "Not Highlighted")) |>
#   filter(abs(upper_CI - lower_CI) <= 10)
# 
# labs <- c("ASR", "Drug Pairs")
# 
# p <- visOmopResults::scatterPlot(
#   sr_tidy,
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
#   ggplot2::ylim(c(0,10)) +
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
# srPlotName <- paste0("nsaids_aesi.png")
# png(file.path(symmetry_folder, srPlotName), width = 10, height = 6, units = "in", res = 300, type = "cairo")
# print(p, newpage = FALSE)
# dev.off()

cli::cli_alert_info("- Made a pretty plot for nsaids-aesis")
info(logger, "MADE ASR SCATTER PLOT")

cli::cli_alert_info("- Completed main analysis cohort symmetry")
info(logger, "COMPLETED MAIN ANALYSIS COHORT SYMMETRY")
