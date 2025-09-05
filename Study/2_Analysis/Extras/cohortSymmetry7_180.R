cli::cli_alert_info("- Generate SequenceRatios for nsaids-aesis-7-180")
info(logger, "GENERATE SEQUENCE RATIOS FOR NSAIDS AESI")

cdm <- CohortSymmetry::generateSequenceCohortSet(
  cdm = cdm,
  name = "nsaids_aesi_7_180",
  cohortDateRange = c(starting_date, ending_date),
  daysPriorObservation = 365,
  combinationWindow = c(7, 180),
  washoutWindow = 365,
  indexTable = "nsaids",
  markerTable = "aesi"
)

results_cs_7_180 <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_7_180"]])

exportSummarisedResult(
  results_cs_7_180,
  path = here::here(symmetry_folder),
  fileName = paste0(db_name, "_result_7_180.csv")
)

marker_settings_7_180 <- settings(cdm[["nsaids_aesi_7_180"]])
write.csv(marker_settings_7_180, here::here(symmetry_folder, paste0(cdmName(cdm), "_ssa_marker_settings_7_180.csv")))

attrition_seq_ratio_7_180 <- attrition(cdm[["nsaids_aesi_7_180"]])
write.csv(attrition_seq_ratio_7_180, here::here(symmetry_folder, paste0(cdmName(cdm), "_ssa_attrition_7_180.csv")))

summary_temp_trends_months_7_180 <- summariseTemporalSymmetry(cdm[["nsaids_aesi_7_180"]], timescale = "month")
write.csv(summary_temp_trends_months_7_180, file.path(symmetry_folder, paste0(cdmName(cdm), "_ssa_temporal_symmetry_summary_7_180.csv")))