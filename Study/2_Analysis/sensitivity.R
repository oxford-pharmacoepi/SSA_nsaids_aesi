# run cohort symmetry on all index-marker pairs and controls (365-day version)
sensitivity_folder <- file.path(output_folder, "sensitivity")
if (!dir.exists(sensitivity_folder)) dir.create(sensitivity_folder, recursive = TRUE)

##############################
# main study nsaids (index) - aesi (markers)
##############################
cli::cli_alert_info("- Generate SequenceCohortSet (365d) for nsaids-aesis")
info(logger, "GENERATING SEQUENCE COHORT SET 365D FOR NSAIDS AESIS")

tryCatch({
  
  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "nsaids_aesi_365",
    cohortDateRange = c(starting_date, ending_date),
    daysPriorObservation = 365,
    combinationWindow = c(0, 365),
    washoutWindow = 365,
    indexTable = "nsaids",
    markerTable = "aesi"
  )
  
  cli::cli_alert_success("- Generated SequenceCohortSet for nsaids-aesis (365d)")
  info(logger, "GENERATED SEQUENCE COHORT SET 365D FOR NSAIDS AESIS")
  
  cli::cli_alert_info("- Generate SequenceRatios for nsaids-aesis (365d)")
  
  results_cs_365 <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_365"]])
  
  cli::cli_alert_success("- Generated SequenceRatios for nsaids-aesis (365d)")
  info(logger, "GENERATED SEQUENCE RATIOS 365D FOR NSAIDS AESIS")

}, error = function(e) { 
  writeLines(as.character(e), 
             here::here(sensitivity_folder, paste0(db_name, "_cs_365_error.txt"))) 
})

cli::cli_alert_success("- Got cohort symmetry results (365d)")
info(logger, "GOT COHORT SYMMETRY RESULTS FOR NSAIDS AESI 365")

cli::cli_alert_info("- Export results for nsaids-aesis (365d)")

exportSummarisedResult( results_cs_365, 
                        path = here::here(sensitivity_folder), 
                        fileName = paste0(db_name, "_result_365.csv") )

info(logger, "EXPORTED COHORT SYMMETRY RESULTS FOR NSAIDS AESI 365")

# Export marker settings
marker_settings_365 <- settings(cdm[["nsaids_aesi_365"]])
write_csv(marker_settings_365,
          here::here(sensitivity_folder, paste0(cdmName(cdm), "_ssa_marker_settings_365.csv")))
info(logger, "WROTE MARKER SETTINGS 365 CSV")

# Export attrition table
attrition_seq_ratio_365 <- attrition(cdm[["nsaids_aesi_365"]])
write_csv(attrition_seq_ratio_365,
          here::here(sensitivity_folder, paste0(cdmName(cdm), "_ssa_attrition_365.csv")))
info(logger, "WROTE ATTRITION 365 CSV")

# Temporal sequence summary
summary_temp_trends_months_365 <- summariseTemporalSymmetry(cohort = cdm[["nsaids_aesi_365"]], timescale = "month")
write_csv(summary_temp_trends_months_365,
          here::here(sensitivity_folder, paste0(cdmName(cdm), "_ssa_temporal_symmetry_summary_365.csv")))
info(logger, "WROTE TEMPORAL SEQUENCE SUMMARY 365 CSV")

#90 day analysis
cdm <- CohortSymmetry::generateSequenceCohortSet(
  cdm = cdm,
  name = "nsaids_aesi_90",
  cohortDateRange = c(starting_date, ending_date),
  daysPriorObservation = 365,
  combinationWindow = c(0, 90),
  washoutWindow = 365,
  indexTable = "nsaids",
  markerTable = "aesi"
)

results_90 <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_90"]])

exportSummarisedResult( results_90, 
                        path = here::here(sensitivity_folder), 
                        fileName = paste0(db_name, "_result_90.csv") )


cli::cli_alert_info("- Completed 365 and 90 windown sensitivity analysis")
info(logger, "COMPLETED 365 AND 90 DAY WINDOW SENSITIVITY ANALYSIS")
