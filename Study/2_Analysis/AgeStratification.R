age_strat_folder <- file.path(output_folder, "age_stratification")
if (!dir.exists(age_strat_folder)) dir.create(age_strat_folder, recursive = TRUE)

#add age and stratifying cohort
cli::cli_alert_info("Adding age variable and stratifying cohorts ({Sys.time()})")
info(logger, "ADDING AGE VARIABLE AND STRATIFYING COHORTS")

cdm$nsaids_aesi_18_64 <- cdm$nsaids_aesi |>
  requireDemographics(indexDate = "index_date",
                      ageRange = c(18,64),
                      name = "nsaids_aesi_18_64")

info(logger, "ADDED AGE VARIABLE AND STRATIFYING COHORTS")

#run cohort symmetry for age-stratified data
cli::cli_alert_info("Running cohort symmetry for age-stratified cohorts ({Sys.time()})")
info(logger, "RUNNING COHORT SYMMETRY FOR AGE-STRATIFIED COHORTS")

tryCatch({
  cli::cli_alert_info("- Generate SequenceRatios for nsaids-aesis-age")
  results_18_64 <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_18_64"]])
  
  cli::cli_alert_success("- Generated SequenceRatios for nsaids-aesis-age")
  
}, error = function(e) {
  writeLines(as.character(e),
             here::here(age_strat_folder, paste0(db_name, "_cs_error.txt")))
})

cli::cli_alert_success("- Got cohort symmetry results")
info(logger, "RAN COHORT SYMMETRY FOR AGE-STRATIFIED COHORTS")

cli::cli_alert_info("- Export results for nsaids-aesis-age")
exportSummarisedResult(results_18_64,
                       path = here::here(age_strat_folder),
                       fileName = paste0(db_name, "_result_18_64.csv"))

info(logger, "EXPORTED AGE STRATIFIED COHORT SYMMETRY RESULTS")

#marker settings
marker_settings_18_64 <- settings(cdm[["nsaids_aesi_18_64"]])
write_csv(marker_settings_18_64, here::here(age_strat_folder, paste0(cdmName(cdm), "_ssa_marker_settings_18_64.csv")))
info(logger, "WROTE MARKER SETTINGS CSV")

#attrition
attrition_seq_ratio_18_64 <- attrition(cdm[["nsaids_aesi_18_64"]])
write_csv(attrition_seq_ratio_18_64, here::here(age_strat_folder, paste0(cdmName(cdm), "_ssa_attrition_18_64.csv")))
info(logger, "WROTE ATTRITION CSV")

#temporal plots
summary_temp_trends_months_18_64 <- summariseTemporalSymmetry(cdm[["nsaids_aesi_18_64"]], timescale = "month")
write_csv(summary_temp_trends_months_18_64, here::here(age_strat_folder, paste0(cdmName(cdm), "_ssa_temporal_symmetry_summary_18_64.csv")))
info(logger, "WROTE TEMPORAL SEQUENCE SUMMARY CSV")


#######
cdm$nsaids_aesi_65_150 <- cdm$nsaids_aesi |>
  requireDemographics(indexDate = "index_date",
                      ageRange = c(65,150),
                      name = "nsaids_aesi_65_150")

info(logger, "ADDED AGE VARIABLE AND STRATIFYING COHORTS")

#run cohort symmetry for age-stratified data
cli::cli_alert_info("Running cohort symmetry for age-stratified cohorts ({Sys.time()})")
info(logger, "RUNNING COHORT SYMMETRY FOR AGE-STRATIFIED COHORTS")

tryCatch({
  cli::cli_alert_info("- Generate SequenceRatios for nsaids-aesis-age")
  results_65_150 <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_65_150"]])
  
  cli::cli_alert_success("- Generated SequenceRatios for nsaids-aesis-age")
  
}, error = function(e) {
  writeLines(as.character(e),
             here::here(age_strat_folder, paste0(db_name, "_cs_error.txt")))
})

cli::cli_alert_success("- Got cohort symmetry results")
info(logger, "RAN COHORT SYMMETRY FOR AGE-STRATIFIED COHORTS")

cli::cli_alert_info("- Export results for nsaids-aesis-age")
exportSummarisedResult(results_65_150,
                       path = here::here(age_strat_folder),
                       fileName = paste0(db_name, "_result_65_150.csv"))

info(logger, "EXPORTED AGE STRATIFIED COHORT SYMMETRY RESULTS")

#marker settings
marker_settings_65_150 <- settings(cdm[["nsaids_aesi_65_150"]])
write_csv(marker_settings_65_150, here::here(age_strat_folder, paste0(cdmName(cdm), "_ssa_marker_settings_65_150.csv")))
info(logger, "WROTE MARKER SETTINGS CSV")

#attrition
attrition_seq_ratio_65_150 <- attrition(cdm[["nsaids_aesi_65_150"]])
write_csv(attrition_seq_ratio_65_150, here::here(age_strat_folder, paste0(cdmName(cdm), "_ssa_attrition_65_150.csv")))
info(logger, "WROTE ATTRITION CSV")

#temporal plots
summary_temp_trends_months_65_150 <- summariseTemporalSymmetry(cdm[["nsaids_aesi_65_150"]], timescale = "month")
write_csv(summary_temp_trends_months_65_150, here::here(age_strat_folder, paste0(cdmName(cdm), "_ssa_temporal_symmetry_summary_65_150.csv")))
info(logger, "WROTE TEMPORAL SEQUENCE SUMMARY CSV")

info(logger, "COMPLETED AGE STRATIFICATION ANALYSIS")
