# positive and negative controls ------

# this code runs the positive and negative controls for the study
if (isTRUE(run_controls)) {
  ########################
  # database positive controls (we know has a signal)
  ########################
  cli::cli_alert_info("- Generate SequenceCohortSet for positive controls")
  info(logger, "Generate SequenceCohortSet for positive controls")

  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "amiodarone_levothyroxine",
    cohortDateRange = c(starting_date, ending_date),
    indexTable = "amiodarone",
    markerTable = "levothyroxine",
    daysPriorObservation = 365,
    washoutWindow = 365,
    combinationWindow = c(0, 180)
  ) # this is where the combination window that we look to see the order

  amiodarone_levothyroxin <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_levothyroxine)


  # ace - cough
  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "ace_cough",
    cohortDateRange = c(starting_date, ending_date),
    indexTable = "ace_inh",
    markerTable = "cough",
    daysPriorObservation = 365,
    washoutWindow = 365,
    combinationWindow = c(0, 180)
  )

  ace_cough <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$ace_cough)

  cli::cli_alert_success("- Generated SequenceCohortSet for database positive controls")
  info(logger, "Generated SequenceCohortSet for positive controls")

  ##############################
  # database negative controls (we know there is no signal)
  ##############################
  cli::cli_alert_info("- Generate SequenceCohortSet for database negative controls")
  info(logger, "Generate SequenceCohortSet for database negative controls")

  # Amiodarone	Allopurinol
  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "amiodarone_allopurinol",
    cohortDateRange = c(starting_date, ending_date),
    indexTable = "amiodarone",
    markerTable = "allopurinol",
    daysPriorObservation = 365,
    washoutWindow = 365,
    combinationWindow = c(0, 180)
  )

  amiodarone_allopurinol <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_allopurinol)


  cli::cli_alert_success("- Generated SequenceCohortSet for database negative controls")
  info(logger, "Generated SequenceCohortSet for database negative controls")

  #############
  # nsaids positive controls
  #############

  cli::cli_alert_info("- Generate SequenceCohortSet for nsaids positive controls")
  info(logger, "Generate SequenceCohortSet for nsaids positive controls")

  # Edema
  cli::cli_alert_info("- Generate SequenceCohortSet for all_nsaids_edema")
  info(logger, "Generate SequenceCohortSet for all_nsaids_edema")

  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "all_nsaids_edema",
    cohortDateRange = c(starting_date, ending_date),
    indexTable = "all_nsaids",
    markerTable = "edema",
    daysPriorObservation = 365,
    washoutWindow = 365,
    combinationWindow = c(0, 180)
  )

  all_nsaids_edema <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$all_nsaids_edema)

  cli::cli_alert_info("- Generated SequenceCohortSet for all_nsaids_edema")
  info(logger, "Generated SequenceCohortSet for all_nsaids_edema")

  # Nausea
  cli::cli_alert_info("- Generate SequenceCohortSet for all_nsaids_nausea")
  info(logger, "Generate SequenceCohortSet for all_nsaids_nausea")

  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "all_nsaids_nausea",
    cohortDateRange = c(starting_date, ending_date),
    indexTable = "all_nsaids",
    markerTable = "nausea",
    daysPriorObservation = 365,
    washoutWindow = 365,
    combinationWindow = c(0, 180)
  )

  all_nsaids_nausea <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$all_nsaids_nausea)

  cli::cli_alert_info("- Generated SequenceCohortSet for all_nsaids_nausea")
  info(logger, "Generated SequenceCohortSet for all_nsaids_nausea")

  # Vomiting
  cli::cli_alert_info("- Generate SequenceCohortSet for all_nsaids_vomiting")
  info(logger, "Generate SequenceCohortSet for all_nsaids_vomiting")

  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "all_nsaids_vomiting",
    cohortDateRange = c(starting_date, ending_date),
    indexTable = "all_nsaids",
    markerTable = "vomiting",
    daysPriorObservation = 365,
    washoutWindow = 365,
    combinationWindow = c(0, 180)
  )

  all_nsaids_vomiting <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$all_nsaids_vomiting)

  cli::cli_alert_info("- Generated SequenceCohortSet for all_nsaids_vomiting")
  info(logger, "Generated SequenceCohortSet for all_nsaids_vomiting")

  # AKI
  cli::cli_alert_info("- Generate SequenceCohortSet for all_nsaids_aki")
  info(logger, "Generate SequenceCohortSet for all_nsaids_aki")

  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "all_nsaids_aki",
    cohortDateRange = c(starting_date, ending_date),
    indexTable = "all_nsaids",
    markerTable = "aki",
    daysPriorObservation = 365,
    washoutWindow = 365,
    combinationWindow = c(0, 180)
  )

  all_nsaids_aki <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$all_nsaids_aki)

  cli::cli_alert_info("- Generated SequenceCohortSet for all_nsaids_aki")
  info(logger, "Generated SequenceCohortSet for all_nsaids_aki")

  # Anemia
  cli::cli_alert_info("- Generate SequenceCohortSet for all_nsaids_anemia")
  info(logger, "Generate SequenceCohortSet for all_nsaids_anemia")

  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "all_nsaids_anemia",
    cohortDateRange = c(starting_date, ending_date),
    indexTable = "all_nsaids",
    markerTable = "anemia",
    daysPriorObservation = 365,
    washoutWindow = 365,
    combinationWindow = c(0, 180)
  )

  all_nsaids_anemia <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$all_nsaids_anemia)

  cli::cli_alert_info("- Generated SequenceCohortSet for all_nsaids_anemia")
  info(logger, "Generated SequenceCohortSet for all_nsaids_anemia")

  cli::cli_alert_info("- Generated SequenceCohortSet for nsaids positive controls")
  info(logger, "Generated SequenceCohortSet for nsaids positive controls")

  ########
  # nsaids negative control
  ########

  # Cataracts
  cli::cli_alert_info("- Generate SequenceCohortSet for all_nsaids_cataracts")
  info(logger, "Generate SequenceCohortSet for all_nsaids_cataracts")

  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "all_nsaids_cataracts",
    cohortDateRange = c(starting_date, ending_date),
    indexTable = "all_nsaids",
    markerTable = "cataracts",
    daysPriorObservation = 365,
    washoutWindow = 365,
    combinationWindow = c(0, 180)
  )

  all_nsaids_cataracts <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$all_nsaids_cataracts)

  cli::cli_alert_info("- Generated SequenceCohortSet for all_nsaids_cataracts")
  info(logger, "Generated SequenceCohortSet for all_nsaids_cataracts")

  # add positive and negative controls to one list
  summary_seq_ratio <- list(
    amiodarone_levothyroxin,
    amiodarone_allopurinol,
    ace_cough,
    all_nsaids_edema,
    all_nsaids_nausea,
    all_nsaids_vomiting,
    all_nsaids_aki,
    all_nsaids_anemia,
    all_nsaids_cataracts
  )

  cli::cli_alert_info("- Bind summary sequence ratios")

  # bind the list of sequence ratios
  summary_seq_ratio_final <- omopgenerics::bind(summary_seq_ratio)

  # write out summarised results object
  omopgenerics::exportSummarisedResult(summary_seq_ratio_final,
    minCellCount = 5,
    path = here::here(symmetry_folder),
    fileName = paste0(
      cdmName(cdm),
      "_results_ssa_estimates_controls.csv"
    )
  )



  # null sequence ratio
  marker_settings_controls <- bind_rows(
    settings(cdm[["amiodarone_levothyroxine"]]),
    settings(cdm[["ace_cough"]]),
    settings(cdm[["amiodarone_allopurinol"]]),
    settings(cdm[["all_nsaids_edema"]]),
    settings(cdm[["all_nsaids_nausea"]]),
    settings(cdm[["all_nsaids_vomiting"]]),
    settings(cdm[["all_nsaids_anemia"]]),
    settings(cdm[["all_nsaids_aki"]]),
    settings(cdm[["all_nsaids_cataracts"]])
  )


  write_csv(marker_settings_controls, here::here(symmetry_folder, paste0("/", cdmName(cdm), "_ssa_marker_settings_controls.csv")))

  cli::cli_alert_info("- Generated null sequence ratios")
  info(logger, "Generated null sequence ratios")

  # attrition for outcomes
  attrition_seq_ratio_controls <- list(
    attrition(cdm[["amiodarone_levothyroxine"]]) %>%
      mutate(cohort_name = "amiodarone_levothyroxine"),
    attrition(cdm[["ace_cough"]]) %>%
      mutate(cohort_name = "ace_cough"),
    attrition(cdm[["amiodarone_allopurinol"]]) %>%
      mutate(cohort_name = "amiodarone_allopurinol"),
    attrition(cdm[["all_nsaids_edema"]]) %>%
      mutate(cohort_name = "all_nsaids_edema"),
    attrition(cdm[["all_nsaids_nausea"]]) %>%
      mutate(cohort_name = "all_nsaids_nausea"),
    attrition(cdm[["all_nsaids_vomiting"]]) %>%
      mutate(cohort_name = "all_nsaids_vomiting"),
    attrition(cdm[["all_nsaids_aki"]]) %>%
      mutate(cohort_name = "all_nsaids_aki"),
    attrition(cdm[["all_nsaids_anemia"]]) %>%
      mutate(cohort_name = "all_nsaids_anemia"),
    attrition(cdm[["all_nsaids_cataracts"]]) %>%
      mutate(cohort_name = "all_nsaids_cataracts")
  ) %>%
    bind_rows()


  write_csv(attrition_seq_ratio_controls, here::here(symmetry_folder, paste0("/", cdmName(cdm), "_ssa_attrition_controls.csv")))

  cli::cli_alert_info("- Generated attrition")
  info(logger, "Generated attrition")

  # temporal sequence plot
  summary_temp_trends_months_controls <-
    list(
      summariseTemporalSymmetry(
        cohort = cdm[["amiodarone_levothyroxine"]],
        timescale = "month"
      ),
      summariseTemporalSymmetry(
        cohort = cdm[["ace_cough"]],
        timescale = "month"
      ),
      summariseTemporalSymmetry(
        cohort = cdm[["amiodarone_allopurinol"]],
        timescale = "month"
      ),
      summariseTemporalSymmetry(
        cohort = cdm[["all_nsaids_edema"]],
        timescale = "month"
      ),
      summariseTemporalSymmetry(
        cohort = cdm[["all_nsaids_nausea"]],
        timescale = "month"
      ),
      summariseTemporalSymmetry(
        cohort = cdm[["all_nsaids_vomiting"]],
        timescale = "month"
      ),
      summariseTemporalSymmetry(
        cohort = cdm[["all_nsaids_aki"]],
        timescale = "month"
      ),
      summariseTemporalSymmetry(
        cohort = cdm[["all_nsaids_anemia"]],
        timescale = "month"
      ),
      summariseTemporalSymmetry(
        cohort = cdm[["all_nsaids_cataracts"]],
        timescale = "month"
      )
    ) %>%
    bind()



  write_csv(summary_temp_trends_months_controls, here::here(symmetry_folder, paste0("/", cdmName(cdm), "_ssa_temporal_symmetry_summary_controls.csv")))

  cli::cli_alert_info("- Generated temporal symmetry plots")
  info(logger, "Generated temporal symmetry plots")

  # get the record trends for index and markers
  record_trends_overall_pnc1 <- cdm[["amiodarone"]] %>%
    filter(
      cohort_start_date >= starting_date,
      cohort_start_date <= ending_date
    ) %>%
    mutate(year = year(cohort_start_date)) %>%
    group_by(cohort_definition_id, year) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    collect() %>%
    left_join(
      settings(cdm$amiodarone) %>%
        select(cohort_definition_id, cohort_name),
      by = "cohort_definition_id"
    )

  record_trends_overall_pnc2 <- cdm[["levothyroxine"]] %>%
    filter(
      cohort_start_date >= starting_date,
      cohort_start_date <= ending_date
    ) %>%
    mutate(year = year(cohort_start_date)) %>%
    group_by(cohort_definition_id, year) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    collect() %>%
    left_join(
      settings(cdm$levothyroxine) %>%
        select(cohort_definition_id, cohort_name),
      by = "cohort_definition_id"
    )

  record_trends_overall_pnc3 <- cdm[["allopurinol"]] %>%
    filter(
      cohort_start_date >= starting_date,
      cohort_start_date <= ending_date
    ) %>%
    mutate(year = year(cohort_start_date)) %>%
    group_by(cohort_definition_id, year) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    collect() %>%
    left_join(
      settings(cdm$allopurinol) %>%
        select(cohort_definition_id, cohort_name),
      by = "cohort_definition_id"
    )

  record_trends_overall_pnc4 <- cdm[["cough"]] %>%
    filter(
      cohort_start_date >= starting_date,
      cohort_start_date <= ending_date
    ) %>%
    mutate(year = year(cohort_start_date)) %>%
    group_by(cohort_definition_id, year) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    collect() %>%
    left_join(
      settings(cdm$cough) %>%
        select(cohort_definition_id, cohort_name),
      by = "cohort_definition_id"
    )

  record_trends_overall_pnc5 <- cdm[["ace_inh"]] %>%
    filter(
      cohort_start_date >= starting_date,
      cohort_start_date <= ending_date
    ) %>%
    mutate(year = year(cohort_start_date)) %>%
    group_by(cohort_definition_id, year) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    collect() %>%
    left_join(
      settings(cdm$ace_inh) %>%
        select(cohort_definition_id, cohort_name),
      by = "cohort_definition_id"
    )

  record_trends_overall_pnc6 <- cdm[["all_nsaids"]] %>%
    filter(
      cohort_start_date >= starting_date,
      cohort_start_date <= ending_date
    ) %>%
    mutate(year = year(cohort_start_date)) %>%
    group_by(cohort_definition_id, year) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    collect() %>%
    left_join(
      settings(cdm$all_nsaids) %>%
        select(cohort_definition_id, cohort_name),
      by = "cohort_definition_id"
    )

  record_trends_overall_pnc6 <- cdm[["all_nsaids_edema"]] %>%
    filter(
      cohort_start_date >= starting_date,
      cohort_start_date <= ending_date
    ) %>%
    mutate(year = year(cohort_start_date)) %>%
    group_by(cohort_definition_id, year) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    collect() %>%
    left_join(
      settings(cdm$all_nsaids_edema) %>%
        select(cohort_definition_id, cohort_name),
      by = "cohort_definition_id"
    )

  record_trends_overall_pnc7 <- cdm[["all_nsaids_nausea"]] %>%
    filter(
      cohort_start_date >= starting_date,
      cohort_start_date <= ending_date
    ) %>%
    mutate(year = year(cohort_start_date)) %>%
    group_by(cohort_definition_id, year) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    collect() %>%
    left_join(
      settings(cdm$all_nsaids_nausea) %>%
        select(cohort_definition_id, cohort_name),
      by = "cohort_definition_id"
    )

  record_trends_overall_pnc8 <- cdm[["all_nsaids_anemia"]] %>%
    filter(
      cohort_start_date >= starting_date,
      cohort_start_date <= ending_date
    ) %>%
    mutate(year = year(cohort_start_date)) %>%
    group_by(cohort_definition_id, year) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    collect() %>%
    left_join(
      settings(cdm$all_nsaids_anemia) %>%
        select(cohort_definition_id, cohort_name),
      by = "cohort_definition_id"
    )

  record_trends_overall_pnc9 <- cdm[["all_nsaids_aki"]] %>%
    filter(
      cohort_start_date >= starting_date,
      cohort_start_date <= ending_date
    ) %>%
    mutate(year = year(cohort_start_date)) %>%
    group_by(cohort_definition_id, year) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    collect() %>%
    left_join(
      settings(cdm$all_nsaids_aki) %>%
        select(cohort_definition_id, cohort_name),
      by = "cohort_definition_id"
    )

  record_trends_overall_pnc10 <- cdm[["all_nsaids_cataracts"]] %>%
    filter(
      cohort_start_date >= starting_date,
      cohort_start_date <= ending_date
    ) %>%
    mutate(year = year(cohort_start_date)) %>%
    group_by(cohort_definition_id, year) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    collect() %>%
    left_join(
      settings(cdm$all_nsaids_cataracts) %>%
        select(cohort_definition_id, cohort_name),
      by = "cohort_definition_id"
    )

  record_trends_overall_pnc11 <- cdm[["all_nsaids_vomiting"]] %>%
    filter(
      cohort_start_date >= starting_date,
      cohort_start_date <= ending_date
    ) %>%
    mutate(year = year(cohort_start_date)) %>%
    group_by(cohort_definition_id, year) %>%
    summarise(n_records = n(), .groups = "drop") %>%
    collect() %>%
    left_join(
      settings(cdm$all_nsaids_vomiting) %>%
        select(cohort_definition_id, cohort_name),
      by = "cohort_definition_id"
    )

  record_trends_overall_controls <- bind_rows(
    record_trends_overall_pnc1,
    record_trends_overall_pnc2,
    record_trends_overall_pnc3,
    record_trends_overall_pnc4,
    record_trends_overall_pnc5,
    record_trends_overall_pnc6,
    record_trends_overall_pnc7,
    record_trends_overall_pnc8,
    record_trends_overall_pnc9,
    record_trends_overall_pnc10,
    record_trends_overall_pnc11
  ) %>%
    mutate(cdm_name = db_name)


  # write out the results
  write_csv(
    record_trends_overall_controls,
    here::here(symmetry_folder, paste0("/", cdmName(cdm), "_record_trend_controls.csv"))
  )

  cli::cli_alert_info("- Generated record trends")
  info(logger, "Generated record trends")
}

####

# data <- summary_seq_ratio_final |>
#   omopgenerics::splitGroup() |>
#   mutate(
#     index_cohort_name = stringr::str_to_title(index_cohort_name),
#     index_cohort_name = dplyr::case_when(
#       index_cohort_name == "All_nsaids" ~ "All NSAIDs",
#       TRUE ~ index_cohort_name
#     )
#   ) |>
#   tidyr::pivot_wider(names_from = estimate_name, values_from = estimate_value) |>
#   filter(variable_name == "adjusted") |>
#   mutate(signal = ifelse(point_estimate > 1 & lower_CI > 1, "Positive", "Negative / Null"))
#
# labs = c("Adjusted Sequence Ratio", "NSAID")
# custom_colors <- c("adjusted" = "black")
#
# controls <- ggplot(data, aes(
#   x = marker_cohort_name,
#   y = as.numeric(point_estimate),
#   ymin = as.numeric(lower_CI),
#   ymax = as.numeric(upper_CI),
#   color = signal
# )) +
#   geom_hline(yintercept = 1, linetype = 2) +
#   # Draw error bars with thicker lines
#   geom_errorbar(
#     aes(ymin = as.numeric(lower_CI), ymax = as.numeric(upper_CI)),
#     position = position_dodge(width = 0.8),
#     width = 0,
#     size = 1  # This controls the thickness of the error bar line
#   ) +
#   facet_wrap(~ index_cohort_name) +
#   # Add points separately
#   geom_point(
#     position = position_dodge(width = 0.8),
#     size = 3.5  # Controls the size of the point
#   ) +
#   coord_flip() +
#   theme_bw() +
#   labs(
#     x = "NSAID",
#     y = "Adjusted Sequence Ratio"
#   ) +
#   theme(
#     legend.position = "right",
#     legend.title = element_blank(),
#     strip.text = element_text(face = "bold", size = 16),
#     axis.text = ggplot2::element_text(size = 14),
#     axis.title = ggplot2::element_text(size = 16)
#   ) +
#   ylim(0,2)
#
# controls
