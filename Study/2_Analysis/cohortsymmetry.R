# run cohort symmetry on all index-marker pairs and controls

########################
# positive controls (we know has a signal)
########################
cli::cli_alert_info("- Generate SequenceCohortSet for positive controls")

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "amiodarone_levothyroxine",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 indexTable = "amiodarone",
                                                 markerTable = "levothyroxine",
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365,
                                                 indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow - for acute effects can set this
                                                 combinationWindow = c(0, 180)) # this is where the combination window that we look to see the order

amiodarone_levothyroxin <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_levothyroxine)
amiodarone_levothyroxin_results <- CohortSymmetry::tableSequenceRatios(result = amiodarone_levothyroxin  ,
                                                                       type = "tibble")

cli::cli_alert_success("- Generated SequenceCohortSet for positive controls")

##############################
# negative controls (we know there is no signal)
##############################
cli::cli_alert_info("- Generate SequenceCohortSet for negative controls")

#Amiodarone	Allopurinol
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "amiodarone_allopurinol",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 indexTable = "amiodarone",
                                                 markerTable = "allopurinol",
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365,
                                                 indexMarkerGap = NULL, # if null it uses the second argument of the combinationWindow
                                                 combinationWindow = c(0, 180))

amiodarone_allopurinol <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_allopurinol)
amiodarone_allopurinol_results <- CohortSymmetry::tableSequenceRatios(result = amiodarone_allopurinol ,
                                                                      type = "tibble")

cli::cli_alert_success("- Generated SequenceCohortSet for negative controls")

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
# get the sequence rations   
results_cs <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi"]],
                                              minCellCount = 5)

cli::cli_alert_success("- Generated SequenceRatios for nsaids-aesis")
  
 }, error = function(e) {
  writeLines(as.character(e),
             here(output_folder, paste0("/", db_name, "_cs_error.txt"
             )))
})

cli::cli_alert_success("- Got cohort symmetry results")

cli::cli_alert_info("- Export results for nsaids-aesis")
# export the results (summarised only)
exportSummarisedResult(results_cs, path = here::here("Results", paste0(db_name)), fileName = paste0(db_name,"_result.csv"))

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


#p
 
srPlotName <- paste0("nsaids_aesi", ".png")
png(paste0(here::here(output_folder, srPlotName)), width = 8, height = 6, units = "in", res = 1500, type="cairo")
print(p, newpage = FALSE)
dev.off()
