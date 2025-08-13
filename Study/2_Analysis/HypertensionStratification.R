htn_strat_folder <- file.path(output_folder, "hypertension_stratification")
if (!dir.exists(htn_strat_folder)) dir.create(htn_strat_folder, recursive = TRUE)

cli::cli_alert_success("- Running hypertension stratification analysis")
info(logger, "RUNNING HYPERTENSION STRATIFICATION ANALYSIS")


cdm$nsaids_aesi_no_hypertension <- cdm$nsaids_aesi |>
    CohortConstructor::requireTableIntersect(
      intersections = 0,
      tableName = "hypertension",
      indexDate = "cohort_start_date",
      window = c(-Inf,-1),
      name = "nsaids_aesi_no_hypertension"
    )

cdm$nsaids_aesi_prior_hypertension <- cdm$nsaids_aesi |>
  CohortConstructor::requireTableIntersect(
    tableName = "hypertension",
    indexDate = "cohort_start_date",
    window = c(-Inf,-1),
    name = "nsaids_aesi_prior_hypertension"
  )

#no prior hypertension
cli::cli_alert_success("- Running no prior hypertension analysis")
info(logger, "RUNNING NO PRIOR HYPERTENSION ANALYSIS")

res <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$nsaids_aesi_no_hypertension)

cli::cli_alert_success("- Running prior hypertension analysis")
info(logger, "RUNNING PRIOR HYPERTENSION ANALYSIS")

res_htn <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$nsaids_aesi_hypertension)

cli::cli_alert_success("- Ran prior hypertension analysis")
info(logger, "Ran PRIOR HYPERTENSION ANALYSIS")

exportSummarisedResult(res, 
                       path = here::here(htn_strat_folder), 
                       fileName = paste0(db_name,"_result_no_htn.csv"))

exportSummarisedResult(res_htn, 
                       path = here::here(htn_strat_folder), 
                       fileName = paste0(db_name,"_result_htn.csv"))



cli::cli_alert_success("- Completed hypertension stratification analysis")
info(logger, "COMPLETED HYPERTENSION STRATIFICATION ANALYSIS")