# run cohort symmetry on all index-marker pairs

##############################
# main study nsaids (index) - aesi (markers)
##############################
cli::cli_alert_info("- Generate SequenceCohortSet for nsaids-aesis")

tryCatch({
    
# generate the sequence cohorts
    cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                     name = "nsaids_aesi",
                                                     cohortDateRange = c(starting_date, ending_date),
                                                     daysPriorObservation = 365,
                                                     combinationWindow = c(0, 180),
                                                     washoutWindow = 365,
                                                     indexTable = "nsaids",
                                                     markerTable = "aesi")
    
cli::cli_alert_success("- Generated SequenceCohortSet for nsaids-aesis")


cli::cli_alert_info("- Generate SequenceRatios for nsaids-aesis")
# get the sequence ratios   
results_cs <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi"]])

cli::cli_alert_success("- Generated SequenceRatios for nsaids-aesis")
  
 }, error = function(e) {
  writeLines(as.character(e),
             here::here(output_folder, paste0("/", cdmName(cdm), "_cs_error.txt"
             )))
})

cli::cli_alert_success("- Got cohort symmetry results")

cli::cli_alert_info("- Export results for nsaids-aesis")
# export the results (summarised only)
exportSummarisedResult(results_cs, 
                       path = here::here(output_folder), 
                       fileName = paste0(db_name,"_result.csv"))

#null sequence ratio 
marker_settings <- 
  settings(cdm[["nsaids_aesi"]])

write.csv(marker_settings, here::here(output_folder, paste0("/", cdmName(cdm), "_ssa_marker_settings.csv"
)))


#attrition for outcomes 
attrition_seq_ratio <- 
  attrition(cdm[["nsaids_aesi"]])
            
write.csv(attrition_seq_ratio, here::here(output_folder, paste0("/", cdmName(cdm), "_ssa_attrition.csv"
            )))


#temporal sequence plot
summary_temp_trends_months <- summariseTemporalSymmetry(cohort = cdm[["nsaids_aesi"]]
                                                        , timescale = "month")

write.csv(summary_temp_trends_months, here::here(output_folder, paste0("/", cdmName(cdm), "_ssa_temporal_symmetry_summary.csv"
)))

# get the record trends for index and markers
record_trends_overall_index <- cdm[["nsaids"]] %>%
  filter(cohort_start_date >= starting_date,
         cohort_start_date <= ending_date) %>%
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
  filter(cohort_start_date >= starting_date,
         cohort_start_date <= ending_date) %>%
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

write.csv(record_trends_overall, 
          here::here(output_folder, paste0("/", cdmName(cdm), "_record_trend_overall.csv"
          )))


cli::cli_alert_info("- Make a pretty plot for nsaids-aesis")

# get a tidy version so you can make a pretty plot
sr_tidy <- results_cs |>
  omopgenerics::tidy() |>
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

# creates a facetted plot for all nsaids with aesi in facets

p <- visOmopResults::scatterPlot(
  sr_tidy,
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
 
srPlotName <- paste0("nsaids_aesi", ".png")
png(paste0(here::here(output_folder, srPlotName)), width = 8, height = 6, units = "in", res = 1500, type="cairo")
print(p, newpage = FALSE)
dev.off()
