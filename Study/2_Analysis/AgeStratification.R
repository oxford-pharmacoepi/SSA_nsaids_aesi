age_strat_folder <- file.path(output_folder, "age_stratification")
if (!dir.exists(age_strat_folder)) dir.create(age_strat_folder, recursive = TRUE)

#add age and stratifying cohort
cli::cli_alert_info("Adding age variable and stratifying cohorts ({Sys.time()})")
info(logger, "ADDING AGE VARIABLE AND STRATIFYING COHORTS")

cdm$nsaids_age <- cdm$nsaids |>
  addAge(ageGroup = list("18_to_65" = c(18,64), "65_and_over" = c(65, Inf))) %>%
  stratifyCohorts(strata = "age_group", name = "nsaids_age")

info(logger, "ADDED AGE VARIABLE AND STRATIFYING COHORTS")

#run cohort symmetry for age-stratified data
cli::cli_alert_info("Running cohort symmetry for age-stratified cohorts ({Sys.time()})")
info(logger, "RUNNING COHORT SYMMETRY FOR AGE-STRATIFIED COHORTS")

tryCatch({
  cdm <- CohortSymmetry::generateSequenceCohortSet(
    cdm = cdm,
    name = "nsaids_aesi_age",
    cohortDateRange = c(starting_date, ending_date),
    daysPriorObservation = 365,
    combinationWindow = c(0, 180),
    washoutWindow = 365,
    indexTable = "nsaids_age",
    markerTable = "aesi"
  )
  
  cli::cli_alert_success("- Generated SequenceCohortSet for nsaids-aesis-age")
  
  cli::cli_alert_info("- Generate SequenceRatios for nsaids-aesis-age")
  results_age <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_age"]])
  
  cli::cli_alert_success("- Generated SequenceRatios for nsaids-aesis-age")
  
}, error = function(e) {
  writeLines(as.character(e),
             here::here(age_strat_folder, paste0(db_name, "_cs_error.txt")))
})

cli::cli_alert_success("- Got cohort symmetry results")
info(logger, "RAN COHORT SYMMETRY FOR AGE-STRATIFIED COHORTS")

cli::cli_alert_info("- Export results for nsaids-aesis-age")
exportSummarisedResult(results_age,
                       path = here::here(age_strat_folder),
                       fileName = paste0(db_name, "_result_age.csv"))

info(logger, "EXPORTED AGE STRATIFIED COHORT SYMMETRY RESULTS")

#marker settings
marker_settings_age <- settings(cdm[["nsaids_aesi_age"]])
write_csv(marker_settings_age, here::here(age_strat_folder, paste0(cdmName(cdm), "_ssa_marker_settings_age.csv")))
info(logger, "WROTE MARKER SETTINGS CSV")

#attrition
attrition_seq_ratio_age <- attrition(cdm[["nsaids_aesi_age"]])
write_csv(attrition_seq_ratio_age, here::here(age_strat_folder, paste0(cdmName(cdm), "_ssa_attrition_age.csv")))
info(logger, "WROTE ATTRITION CSV")

#temporal plots
summary_temp_trends_months_age <- summariseTemporalSymmetry(cdm[["nsaids_aesi_age"]], timescale = "month")
write_csv(summary_temp_trends_months_age, here::here(age_strat_folder, paste0(cdmName(cdm), "_ssa_temporal_symmetry_summary_age.csv")))
info(logger, "WROTE TEMPORAL SEQUENCE SUMMARY CSV")

#365 day analysis
cdm <- CohortSymmetry::generateSequenceCohortSet(
  cdm = cdm,
  name = "nsaids_aesi_age_365",
  cohortDateRange = c(starting_date, ending_date),
  daysPriorObservation = 365,
  combinationWindow = c(0, 365),
  washoutWindow = 365,
  indexTable = "nsaids_age",
  markerTable = "aesi"
)

results_age_365 <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_age_365"]])

exportSummarisedResult(results_age_365, 
                       path = here::here(age_strat_folder), 
                       fileName = paste0(db_name,"_result_age_365.csv"))

#90 day analysis
cdm <- CohortSymmetry::generateSequenceCohortSet(
  cdm = cdm,
  name = "nsaids_aesi_age_90",
  cohortDateRange = c(starting_date, ending_date),
  daysPriorObservation = 365,
  combinationWindow = c(0, 90),
  washoutWindow = 365,
  indexTable = "nsaids_age",
  markerTable = "aesi"
)

results_age_90 <- CohortSymmetry::summariseSequenceRatios(cdm[["nsaids_aesi_age_90"]])

exportSummarisedResult(results_age_90, 
                       path = here::here(age_strat_folder), 
                       fileName = paste0(db_name,"_result_age_90.csv"))


info(logger, "COMPLETED AGE STRATIFICATION ANALYSIS")
