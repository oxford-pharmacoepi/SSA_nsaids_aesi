sex_strat_folder <- file.path(output_folder, "sex_stratification")
if (!dir.exists(sex_strat_folder)) dir.create(sex_strat_folder, recursive = TRUE)

# Add sex variable and stratify cohorts
cli::cli_alert_info("Adding sex variable and stratifying cohorts ({Sys.time()})")
info(logger, "ADDING SEX VARIABLE AND STRATIFYING COHORTS")

cdm$nsaids_sex <- cdm$nsaids |>
  addSex() %>%
  stratifyCohorts(strata = "sex", name = "nsaids_sex")

info(logger, "ADDED SEX VARIABLE AND STRATIFYING COHORTS")

# Run cohort symmetry analysis for sex-stratified data
cli::cli_alert_info("Running cohort symmetry for sex stratified data ({Sys.time()})")
info(logger, "RUNNING COHORT SYMMETRY FOR SEX STRATIFIED DATA")

tryCatch({
  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "nsaids_aesi_sex",
    cohortDateRange = c(starting_date, ending_date),
    daysPriorObservation = 365,
    combinationWindow = c(0, 180),
    washoutWindow = 365,
    indexTable = "nsaids_sex",
    markerTable = "aesi"
  )
  
  cli::cli_alert_success("- Generated SequenceCohortSet for nsaids-aesis-sex")
  info(logger, "RAN COHORT SYMMETRY FOR SEX STRATIFIED DATA")
  
  results_sex <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_sex"]])
  cli::cli_alert_success("- Generated SequenceRatios for nsaids-aesis-sex")
}, error = function(e) {
  writeLines(as.character(e),
             here(sex_strat_folder, paste0(db_name, "_cs_error.txt")))
})

cli::cli_alert_success("- Got cohort symmetry results")
info(logger, "GOT COHORT SYMMETRY RESULTS")

cli::cli_alert_info("- Export results for nsaids-aesis-sex")
# export the results (summarised only)
exportSummarisedResult(results_sex, 
                       path = here::here(sex_strat_folder), 
                       fileName = paste0(db_name,"_result_sex.csv"))


info(logger, "EXPORTED SUMMARISED RESULTS")

marker_settings_sex <- settings(cdm[["nsaids_aesi_sex"]])
write.csv(marker_settings_sex, file.path(sex_strat_folder, paste0(cdmName(cdm), "_ssa_marker_settings_sex.csv")))
info(logger, "WROTE CSV FOR MARKER SETTINGS")


#attrition
attrition_seq_ratio_sex <- attrition(cdm[["nsaids_aesi_sex"]])
write.csv(attrition_seq_ratio_sex, file.path(sex_strat_folder, paste0(cdmName(cdm), "_ssa_attrition_sex.csv")))
info(logger, "WROTE CSV FOR ATTRITION SEQ RATIO SEX")

#temporal symmetry plots 
summary_temp_trends_months_sex <- summariseTemporalSymmetry(
  cohort = cdm[["nsaids_aesi_sex"]],
  timescale = "month"
)
write_csv(summary_temp_trends_months_sex, file.path(sex_strat_folder, paste0(cdmName(cdm), "_ssa_temporal_symmetry_summary_sex.csv")))
info(logger, "WROTE CSV FOR TEMPORAL SEQUENCE SUMMARY")


info(logger, "COMPLETED SEX STRATIFICATION ANALYSIS")
