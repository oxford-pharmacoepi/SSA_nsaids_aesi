if (isTRUE(run_characterisation)) {
  
  # Create output folder for characterisation analysis
  characterisation_folder <- file.path(output_folder, "characterisation")
  if (!dir.exists(characterisation_folder)) dir.create(characterisation_folder, recursive = TRUE)
  
  # demographics ----
  cli::cli_alert_info("Summarising Table One Demographics ({Sys.time()})")
  info(logger, "SUMMARISING TABLE ONE DEMOGRAPHICS")
  
  # comorbidities --------
  cli::cli_alert_info("Instantiating table one Comorbidities ({Sys.time()})")
  info(logger, "INSTANTIATING TABLE ONE COMORBIDITIES")
  
  if(isFALSE(instantiated)){
    codelistConditions <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm)
    
    cdm <- CDMConnector::generateConceptCohortSet(
      cdm = cdm,
      conceptSet = codelistConditions,
      name = "conditions",
      overwrite = TRUE
    )
  }
  
  cli::cli_alert_info("Summarising table one Comorbidities ({Sys.time()})")
  info(logger, "SUMMARISING TABLE ONE COMORBIDITIES")
  
  # medications -----
  cli::cli_alert_info("Instantiating table one Medications ({Sys.time()})")
  info(logger, "INSTANTIATING TABLE ONE MEDICATIONS")
  
  if(isFALSE(instantiated)){
    codelistMedications <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm)
    
    cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
      cdm = cdm,
      conceptSet = codelistMedications,
      name = "medications"
    )
  }
  
  cli::cli_alert_info("Summarising table one Medications ({Sys.time()})")
  info(logger, "SUMMARISING TABLE ONE MEDICATIONS")
  
  # add sex and age to cohorts ----
  cli::cli_alert_info("Add demographics to cohort ({Sys.time()})")
  info(logger, "ADDING DEMOGRAPHICS TO COHORT")
  
  # take the nsaids_aesi cohort (individual nsaids versus all markers)
  # bind to the settings and remove rows where marker is gi hem or stroke broad
  # remove broad nsaid groups (all, non_selective, cox_2)
  # take first nsaid-aesi combination for each individual
  # replace cohort_definition id with index_id (so it will run per nsaid)
cdm$nsaids_aesi_characterisation <- cdm$nsaids_aesi |>
  left_join(
    settings(cdm$nsaids_aesi),
    by = "cohort_definition_id",
    copy = TRUE) |>
  select(-c(cohort_date_range, days_prior_observation, washout_window, index_marker_gap, 
            combination_window, moving_average_restriction, nsr)) |>
 mutate(order = ifelse(as.Date(index_date) < as.Date(marker_date), "index", "marker")) |>
  filter(!marker_name %in% c("gi_hemorrhage", "stroke_broad"),
         !index_name %in% c("aspirin", "acetaminophen")) |>
  filter(!index_name %in% c("all_nsaids", "non_selective", "cox_2")) |>
  mutate(
    index_date  = as.Date(index_date),
    marker_date = as.Date(marker_date),
    # choose which date to use depending on `order`
    event_date = case_when(
      `order` %in% c("index") ~ index_date,
      `order` %in% c("marker")           ~ marker_date,
      TRUE                               ~ as.Date(NA)   # fallback for unexpected `order` values
    )
  ) |>
  filter(!is.na(event_date)) |>
  group_by(subject_id) |>
  slice_min(event_date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(cohort_definition_id = index_id) |>
  PatientProfiles::addDemographics(
    indexDate = "cohort_start_date",
    ageGroup = list(
      "age_group" = list(
        "18 to 49" = c(18, 49),
        "50 to 59" = c(50, 59),
        "60 to 69" = c(60, 69),
        "70 to 79" = c(70, 79),
        "80 to 89" = c(80, 89),
        "90+"     = c(90, 150)
      )
    )
  ) |>
  compute(name = "nsaids_aesi_characterisation", temporary = FALSE)

new_settings <- cdm$nsaids_aesi_characterisation %>%
  distinct(cohort_definition_id, index_id, index_name) %>%  # all together!
  rename(cohort_name = index_name) %>%
  collect() %>%  # collect locally if needed
  tibble::as_tibble()

# attrition
new_attrition <- tibble(
  cohort_definition_id = integer(),
  number_subjects = integer(),
  number_records = integer(),
  reason_id = integer(),
  reason = character(),
  excluded_subjects = integer(),
  excluded_records = integer()
)

# codelist ref
new_codelist_ref <- tibble(
  cohort_definition_id = integer(),
  codelist_name = character(),
  concept_id = integer(),
  codelist_type = character()
)


# add new settings, attrition and codelist ref to new cohort table
cdm$nsaids_aesi_characterisation <- omopgenerics::newCohortTable(
  table = cdm$nsaids_aesi_characterisation,
  cohortSetRef = new_settings,
  cohortAttritionRef = new_attrition,
  cohortCodelistRef = new_codelist_ref,
  .softValidation = TRUE
)

info(logger, "SUMMARISE NSAID COHORTS")

suppressWarnings({
summarycharacteristics <- cdm$nsaids_aesi_characterisation |>
  CohortCharacteristics::summariseCharacteristics(
    ageGroup = list(
      "18 to 49" = c(18, 49),
      "50 to 59" = c(50, 59),
      "60 to 69" = c(60, 69),
      "70 to 79" = c(70, 79),
      "80 to 89" = c(80, 89),
      "90+" = c(90, 150)
    ),
    cohortIntersectFlag = list(
      "Conditions prior to index date" = list(
        targetCohortTable = "conditions",
        window = c(-Inf, -1)
      ),
      "Medications 365 days prior to index date" = list(
        targetCohortTable = "medications",
        window = c(-365, -1)
      )
    )
  )
})


info(logger, "CREATE ANY NSAID CHARACTERISTICS COHORT")

suppressWarnings({
  
cdm$characterise_any_nsaid_use <- cdm$nsaids_aesi_characterisation |>
  unionCohorts(name = "characterise_any_nsaid_use")

cdm$characterise_any_nsaid_use <- cdm$characterise_any_nsaid_use |>
  renameCohort(
    cohortId = 1,
    newCohortName = c("any_nsaid")
  )

cdm$characterise_any_nsaid_use <- cdm$characterise_any_nsaid_use |>
  PatientProfiles::addDemographics(
    indexDate = "cohort_start_date",
    ageGroup = list(
      "age_group" = list(
        "18 to 49" = c(18, 49),
        "50 to 59" = c(50, 59),
        "60 to 69" = c(60, 69),
        "70 to 79" = c(70, 79),
        "80 to 89" = c(80, 89),
        "90+"     = c(90, 150)
      )
    )
  )
})

info(logger, "SUMMARISE NSAID COHORTS")

suppressWarnings({
  
set <- settings(cdm$nsaids) |>
  filter(!cohort_name %in% c("acetaminophen", "all_nsaids",
                             "cox_2", "non_selective", "aspirin")) |>
  pull(cohort_definition_id)
  
summariseAnyNsaid <- cdm$characterise_any_nsaid_use |>
  CohortCharacteristics::summariseCharacteristics(
    ageGroup = list(
      "18 to 49" = c(18, 49),
      "50 to 59" = c(50, 59),
      "60 to 69" = c(60, 69),
      "70 to 79" = c(70, 79),
      "80 to 89" = c(80, 89),
      "90+" = c(90, 150)
    ),
cohortIntersectFlag = list(
  
  "NSAID on index date" = list(
    targetCohortTable = "nsaids",
    targetCohortId = set,
    window = c(0, 0)
  ),
  "Conditions prior to index date" = list(
    targetCohortTable = "conditions",
    window = c(-Inf, -1)
  ),
  "Medications 365 days prior to index date" = list(
    targetCohortTable = "medications",
    window = c(-365, -1)
  )
)
)
})

cli::cli_alert_info("Exporting table one characteristics results ({Sys.time()})")
info(logger, "EXPORTING TABLE ONE CHARACTERISTICS RESULTS")

table_one <- rbind(summarycharacteristics, summariseAnyNsaid)

omopgenerics::exportSummarisedResult(
  table_one,
  minCellCount = 5,
  path = here::here(characterisation_folder),
  fileName = paste0(cdmName(cdm), "_summary_characteristics.csv")
)

info(logger, "EXPORTED TABLE ONE CHARACTERISTICS RESULTS")

cli::cli_alert_success("Table one Characterisation Analysis Complete ({Sys.time()})")
info(logger, "COMPLETED TABLE ONE CHARACTERISATION ANALYSIS")
}

