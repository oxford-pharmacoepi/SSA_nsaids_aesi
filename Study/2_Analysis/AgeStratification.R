cdm$nsaids_age <- cdm$nsaids |>
  addAge(ageGroup = list("18_to_65" = c(18,64), "65_and_over" = c(65, Inf))) |>
  stratifyCohorts(strata = "age_group", name = "nsaids_age")

tryCatch({
  
  # generate the sequence cohorts
  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                   name = "nsaids_aesi_age",
                                                   cohortDateRange = c(starting_date, ending_date),
                                                   daysPriorObservation = 365,
                                                   combinationWindow = c(0, 180),
                                                   washoutWindow = 365,
                                                   indexTable = "nsaids_age",
                                                   markerTable = "aesi")
  
  cli::cli_alert_success("- Generated SequenceCohortSet for nsaids-aesis-age")
  
  
  cli::cli_alert_info("- Generate SequenceRatios for nsaids-aesis-age")
  # get the sequence rations   
  results_age <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_age"]])
  
  cli::cli_alert_success("- Generated SequenceRatios for nsaids-aesis-age")
  
}, error = function(e) {
  writeLines(as.character(e),
             here(output_folder, paste0("/", db_name, "_cs_error.txt"
             )))
})

cli::cli_alert_success("- Got cohort symmetry results")

cli::cli_alert_info("- Export results for nsaids-aesis-age")
# export the results (summarised only)
exportSummarisedResult(results_age, 
                       path = here::here("Results", paste0(db_name)), 
                       fileName = paste0(db_name,"_result_age.csv"))

#null sequence ratio 
marker_settings_age <- 
  settings(cdm[["nsaids_aesi_age"]])

write_csv(marker_settings, here::here("Results", paste0(db_name, "/", cdmName(cdm), "_ssa_marker_settings_age.csv"
)))


#attrition for outcomes 
attrition_seq_ratio_age <- 
  attrition(cdm[["nsaids_aesi_age"]])

write_csv(attrition_seq_ratio, here::here("Results", paste0(db_name, "/", cdmName(cdm), "_ssa_attrition_age.csv"
)))


#temporal sequence plot
summary_temp_trends_months_age <- summariseTemporalSymmetry(cohort = cdm[["nsaids_aesi_age"]]
                                                            , timescale = "month")

write_csv(summary_temp_trends_months, here::here("Results", paste0(db_name, "/", cdmName(cdm), "_ssa_temporal_symmetry_summary_age.csv"
)))


#this creates a plot with each NSAID split by age group 
sr_tidy_age <- results_age |>
  omopgenerics::tidy() %>% 
  dplyr::mutate(
    index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
  ) |>
  dplyr::filter(variable_level == "sequence_ratio" & variable_name == "adjusted") |>
  dplyr::mutate(
    pair = paste0(index_cohort_name, "->", marker_cohort_name)
  ) %>% 
  filter(point_estimate != Inf) %>% 
  mutate(highlight = ifelse(lower_CI > 1, "Highlighted", "Not Highlighted")) %>% 
  filter(abs(upper_CI - lower_CI) <= 10) 

labs = c("ASR", "Drug Pairs")
custom_colors <- c("adjusted" = "black")


p <- visOmopResults::scatterPlot(
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
  ggplot2::ylim(c(0,10))+ # restricts the plot
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::geom_hline(yintercept = 1, linetype = 2) +
  ggplot2::scale_shape_manual(values = rep(19, 5)) +
  #ggplot2::scale_colour_manual(values = custom_colors) +
  ggplot2::theme(panel.border = ggplot2::element_blank(),
                 axis.line = ggplot2::element_line(),
                 legend.position="none" ,
                 legend.title = ggplot2::element_blank(),
                 plot.title = ggplot2::element_text(hjust = 0.5)) 

p

srPlotName <- paste0("nsaids_aesi_age", ".png")
png(paste0(here::here(output_folder, srPlotName)), width = 8, height = 6, units = "in", res = 1500, type="cairo")
print(p, newpage = FALSE)
dev.off()

#this plot compares younger adult vs older adult risk for each AESI by NSAID
sr_tidy_age <- results_age %>%
  omopgenerics::tidy() %>%
  dplyr::mutate(
    # Extract age group BEFORE cleaning names
    age_group = dplyr::case_when(
      stringr::str_detect(index_cohort_name, "18_to_65") ~ "18–65",
      stringr::str_detect(index_cohort_name, "65_and_over") ~ "65+",
      TRUE ~ NA_character_
    ),
    # Clean NSAID cohort names AFTER extracting age group
    index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    index_cohort_name = stringr::str_remove(index_cohort_name, "_(18_to_65|65_and_over)$"),
    marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    pair = paste0(index_cohort_name, "->", marker_cohort_name)
  ) %>%
  dplyr::filter(
    variable_level == "sequence_ratio",
    variable_name == "adjusted",
    point_estimate != Inf,
    abs(upper_CI - lower_CI) <= 10
  ) %>%
  dplyr::filter(!is.na(age_group)) %>%
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

srPlotName <- paste0("nsaids_aesi_age_new", ".png")
png(paste0(here::here(output_folder, srPlotName)), width = 8, height = 6, units = "in", res = 1500, type="cairo")
print(p_age_comparison, newpage = FALSE)
dev.off()