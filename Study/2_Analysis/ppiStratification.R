if(isTRUE(run_ppi_stratification)){
  cdm <- CDMConnector::cdmFromCon(con = db,
                                  cdmSchema = cdm_database_schema,
                                  writeSchema = results_database_schema,
                                  cohortTables = c( "nsaids", 
                                                    "aesi", 
                                                    "medications", 
                                                    "conditions", 
                                                    "all_nsaids", 
                                                    "cox_2", 
                                                    "non_selective", 
                                                    "nsaids_sa", 
                                                    "amiodarone", 
                                                    "levothyroxine", 
                                                    "allopurinol", 
                                                    "ace_inh", 
                                                    "cough", 
                                                    "asthma", 
                                                    "edema",
                                                    "hypertension",
                                                    "cataracts", 
                                                    "nausea", 
                                                    "vomiting",
                                                    "ppi",
                                                    "anemia", 
                                                    "aki"),
                                  
                                  writePrefix = table_stem,
                                  achillesSchema = results_database_schema,
                                  cdmName = db_name)



ppi_strat_folder <- file.path(output_folder, "ppi_stratification")
if (!dir.exists(ppi_strat_folder)) dir.create(ppi_strat_folder, recursive = TRUE)

cli::cli_alert_success("- Running ppi stratification analysis")
info(logger, "RUNNING PPI STRATIFICATION ANALYSIS")

# generate all nsaids aesi cohort
cdm <- CohortSymmetry::generateSequenceCohortSet(
  cdm = cdm,
  name = "all_nsaids_aesi",
  cohortDateRange = c(starting_date, ending_date),
  daysPriorObservation = 365,
  combinationWindow = c(0, 180),
  washoutWindow = 365,
  indexTable = "all_nsaids",
  markerTable = "aesi"
)


# create no and prior PPI use cohorts based on all nsaid and aesi cohort
# note the index date is the date of nsaid
cdm$nsaids_no_ppi <- cdm$all_nsaids_aesi |>
  CohortConstructor::requireTableIntersect(
    intersections = 0,
    tableName = "ppi",
    indexDate = "index_date",
    window = c(-Inf,-1),
    name = "nsaids_no_ppi"
  )

cdm$nsaids_prior_ppi <- cdm$all_nsaids_aesi |>
  CohortConstructor::requireTableIntersect(
    tableName = "ppi",
    indexDate = "index_date",
    window = c(-Inf,-1),
    name = "nsaids_prior_ppi"
  )


# summarise cohort sequences for no and prior ppi

#no prior ppi
cli::cli_alert_success("- Running no prior ppi analysis")
info(logger, "RUNNING NO PRIOR PPI ANALYSIS")

res_no_ppi <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$nsaids_no_ppi)

cli::cli_alert_success("- Ran no prior ppi analysis")
info(logger, "RAN NO PRIOR PPI ANALYSIS")

#prior ppi
cli::cli_alert_success("- Running prior ppi analysis")
info(logger, "RUNNING PRIOR PPI ANALYSIS")

res_ppi <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$nsaids_prior_ppi)

# all nsaids for comparison
all_nsaid_aesi_overall <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$all_nsaids_aesi)
  
cli::cli_alert_success("- Ran prior ppi analysis")
info(logger, "RAN PRIOR PPI ANALYSIS")

cli::cli_alert_success("- Export prior ppi analysis")
info(logger, "EXPORT PRIOR PPI ANALYSIS")

exportSummarisedResult(res_no_ppi, 
                       path = here::here(ppi_strat_folder), 
                       fileName = paste0(db_name,"_result_no_ppi.csv"))

exportSummarisedResult(res_ppi, 
                       path = here::here(ppi_strat_folder), 
                       fileName = paste0(db_name,"_result_ppi.csv"))

exportSummarisedResult(all_nsaid_aesi_overall, 
                       path = here::here(ppi_strat_folder), 
                       fileName = paste0(db_name,"_result_ppi_strat_overall.csv"))


cli::cli_alert_success("- Completed ppi stratification analysis")
info(logger, "COMPLETED PPI STRATIFICATION ANALYSIS")

}