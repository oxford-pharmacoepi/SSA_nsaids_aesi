# Create output folder for symmetry analysis
symmetry_folder <- file.path(output_folder, "symmetry")
if (!dir.exists(symmetry_folder)) dir.create(symmetry_folder, recursive = TRUE)

cli::cli_alert_info("- Running main analysis cohort symmetry")
info(logger, "RUNNING MAIN ANALYSIS COHORT SYMMETRY")


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

cli::cli_alert_info("- Completed main analysis cohort symmetry")
info(logger, "COMPLETED MAIN ANALYSIS COHORT SYMMETRY")
