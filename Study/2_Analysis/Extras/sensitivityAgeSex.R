sensitivity_as_folder <- file.path(output_folder, "sensitivity_age_sex")
if (!dir.exists(sensitivity_as_folder)) dir.create(sensitivity_as_folder, recursive = TRUE)

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "nsaids_ssa_sens_180",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 daysPriorObservation = 365,
                                                 combinationWindow = c(0, 180),
                                                 washoutWindow = 365,
                                                 indexTable = "nsaids_sa",
                                                 markerTable = "aesi")

results_180 <- CohortSymmetry::summariseSequenceRatios(cdm$nsaids_ssa_sens_180)

exportSummarisedResult(results_180,
                       path = here::here(sensitivity_as_folder),
                       fileName = paste0(db_name, "all_nsaids_180_result.csv"))

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "nsaids_ssa_sens_365",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 daysPriorObservation = 365,
                                                 combinationWindow = c(0, 365),
                                                 washoutWindow = 365,
                                                 indexTable = "nsaids_sa",
                                                 markerTable = "aesi")

results_365 <- CohortSymmetry::summariseSequenceRatios(cdm$nsaids_ssa_sens_365)

exportSummarisedResult(results_365,
                       path = here::here(sensitivity_as_folder),
                       fileName = paste0(db_name, "all_nsaids_365_result.csv"))

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "nsaids_ssa_sens_90",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 daysPriorObservation = 365,
                                                 combinationWindow = c(0, 90),
                                                 washoutWindow = 365,
                                                 indexTable = "nsaids_sa",
                                                 markerTable = "aesi")

results_90 <- CohortSymmetry::summariseSequenceRatios(cdm$nsaids_ssa_sens_90)

exportSummarisedResult(results_90,
                       path = here::here(sensitivity_as_folder),
                       fileName = paste0(db_name, "all_nsaids_90_result.csv"))


cdm$all_nsaids_age_sex <- cdm$all_nsaids|>
  addSex() |>
  addAge(ageGroup = list("18_to_65" = c(18,64), "65_and_over" = c(65, Inf))) |>
  stratifyCohorts(strata = list("sex", "age_group", c("sex", "age_group")), name = "all_nsaids_age_sex")


cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "all_nsaids_ssa_age_sex_180",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 daysPriorObservation = 365,
                                                 combinationWindow = c(0, 180),
                                                 washoutWindow = 365,
                                                 indexTable = "all_nsaids_age_sex",
                                                 markerTable = "aesi")

results_age_sex_180 <- CohortSymmetry::summariseSequenceRatios(cdm$all_nsaids_ssa_age_sex_180)

exportSummarisedResult(results_age_sex_180,
                       path = here::here(sensitivity_as_folder),
                       fileName = paste0(db_name, "all_nsaids_age_sex_180_result.csv"))

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "all_nsaids_ssa_age_sex_365",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 daysPriorObservation = 365,
                                                 combinationWindow = c(0, 365),
                                                 washoutWindow = 365,
                                                 indexTable = "all_nsaids_age_sex",
                                                 markerTable = "aesi")

results_age_sex_365 <- CohortSymmetry::summariseSequenceRatios(cdm$all_nsaids_ssa_age_sex_365)

exportSummarisedResult(results_age_sex_365,
                       path = here::here(sensitivity_as_folder),
                       fileName = paste0(db_name, "all_nsaids_age_sex_365_result.csv"))

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "all_nsaids_ssa_age_sex_90",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 daysPriorObservation = 365,
                                                 combinationWindow = c(0, 90),
                                                 washoutWindow = 365,
                                                 indexTable = "all_nsaids_age_sex",
                                                 markerTable = "aesi")

results_age_sex_90 <- CohortSymmetry::summariseSequenceRatios(cdm$all_nsaids_ssa_age_sex_90)

exportSummarisedResult(results_age_sex_90,
                       path = here::here(sensitivity_as_folder),
                       fileName = paste0(db_name, "all_nsaids_age_sex_90_result.csv"))


# sr_tidy_sa_age_sex <- results_sa_age_sex |>
#   omopgenerics::tidy() %>% 
#   dplyr::mutate(
#     index_cohort_name = stringr::str_replace(index_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
#     marker_cohort_name = stringr::str_replace(marker_cohort_name, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
#   ) |>
#   dplyr::filter(variable_level == "sequence_ratio" & variable_name == "adjusted") |>
#   dplyr::mutate(
#     pair = paste0(index_cohort_name, "->", marker_cohort_name)
#   ) %>% 
#   filter(point_estimate != Inf) %>% 
#   mutate(highlight = ifelse(lower_CI > 1, "Highlighted", "Not Highlighted")) %>% 
#   filter(abs(upper_CI - lower_CI) <= 10) 
# 
# labs = c("ASR", "Drug Pairs")
# custom_colors <- c("adjusted" = "black")
# 
# 
# p <- visOmopResults::scatterPlot(
#   sr_tidy_sa_age_sex,
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
#   ggplot2::ylim(c(0,10))+ # restricts the plot
#   ggplot2::coord_flip() +
#   ggplot2::theme_bw() +
#   ggplot2::geom_hline(yintercept = 1, linetype = 2) +
#   ggplot2::scale_shape_manual(values = rep(19, 5)) +
#   #ggplot2::scale_colour_manual(values = custom_colors) +
#   ggplot2::theme(panel.border = ggplot2::element_blank(),
#                  axis.line = ggplot2::element_line(),
#                  legend.position="none" ,
#                  legend.title = ggplot2::element_blank(),
#                  plot.title = ggplot2::element_text(hjust = 0.5)) 
# 
# srPlotName <- paste0("nsaids_aesi_sa_age_sex", ".png")
# png(paste0(here::here(output_folder, srPlotName)), width = 8, height = 12, units = "in", res = 300, type="cairo")
# print(p, newpage = FALSE)
# dev.off()
