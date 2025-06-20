cdm$nsaids_sex <- cdm$nsaids |>
  addSex() %>% 
  stratifyCohorts(strata = "sex", name = "nsaids_sex")

tryCatch({
  
  # generate the sequence cohorts
  cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                   name = "nsaids_aesi_sex",
                                                   cohortDateRange = c(starting_date, ending_date),
                                                   daysPriorObservation = 365,
                                                   combinationWindow = c(0, 180),
                                                   washoutWindow = 365,
                                                   indexTable = "nsaids_sex",
                                                   markerTable = "aesi")
  
  cli::cli_alert_success("- Generated SequenceCohortSet for nsaids-aesis-sex")
  
  
  cli::cli_alert_info("- Generate SequenceRatios for nsaids-aesis-sex")
  # get the sequence rations   
  results_sex <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_sex"]])
  
  cli::cli_alert_success("- Generated SequenceRatios for nsaids-aesis-sex")
  
}, error = function(e) {
  writeLines(as.character(e),
             here(output_folder, paste0("/", db_name, "_cs_error.txt"
             )))
})

cli::cli_alert_success("- Got cohort symmetry results")

cli::cli_alert_info("- Export results for nsaids-aesis-sex")
# export the results (summarised only)
exportSummarisedResult(results_sex, 
                       path = here::here("Results", paste0(db_name)), 
                       fileName = paste0(db_name,"_result_sex.csv"))

#this creates a plot for AESIs with each NSAID split by sex 
sr_tidy_sex <- results_sex |>
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
  sr_tidy_sex,
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

srPlotName <- paste0("nsaids_aesi_sex", ".png")
png(paste0(here::here(output_folder, srPlotName)), width = 8, height = 6, units = "in", res = 1500, type="cairo")
print(p, newpage = FALSE)
dev.off()


#this plot compares male vs female risk for each AESI by NSAI
sr_tidy_sex <- results_sex %>%
  omopgenerics::tidy() %>%
  dplyr::mutate(
    # Extract sex BEFORE cleaning names
    sex = dplyr::case_when(
      stringr::str_detect(index_cohort_name, "_female$") ~ "Female",
      stringr::str_detect(index_cohort_name, "_male$") ~ "Male",
      TRUE ~ "Unspecified"
    ),
    # Clean cohort names AFTER extracting sex
    index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    index_cohort_name = stringr::str_remove(index_cohort_name, "_(female|male)$"),
    marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    pair = paste0(index_cohort_name, "->", marker_cohort_name)
  ) %>%
  dplyr::filter(
    variable_level == "sequence_ratio",
    variable_name == "adjusted",
    point_estimate != Inf,
    abs(upper_CI - lower_CI) <= 10
  ) %>%
  dplyr::mutate(
    highlight = ifelse(lower_CI > 1, "Highlighted", "Not Highlighted")
  )

p_sex_comparison <- ggplot(sr_tidy_sex, aes(
  x = index_cohort_name,
  y = point_estimate,
  ymin = lower_CI,
  ymax = upper_CI,
  shape = sex,
  color = sex
)) +
  geom_pointrange(position = position_dodge(width = 0.5), size = 0.4) +
  facet_wrap(~ marker_cohort_name, scales = "free_y") +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept = 1, linetype = 2) +
  labs(
    title = "Male vs Female Risk by NSAID and AESI",
    x = "NSAID",
    y = "Adjusted Sequence Ratio"
  ) +
  scale_shape_manual(values = c("Male" = 17, "Female" = 16)) +  # ▲ triangle for Male, ● circle for Female
  scale_color_manual(values = c("Male" = "#1f77b4", "Female" = "#d62728")) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    strip.text = element_text(face = "bold")
  )

srPlotName <- paste0("nsaids_aesi_sex_new", ".png")
png(paste0(here::here(output_folder, srPlotName)), width = 8, height = 6, units = "in", res = 1500, type="cairo")
print(p_sex_comparison, newpage = FALSE)
dev.off()