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
  # only take the index name all_nsaids (excluding GI hem, stroke broad as it is a positive control)
  # replace cohort_definition id with index_id (so it will run per nsaid)
  cdm$nsaids_characteristics <- cdm$nsaids_aesi %>%
    left_join(
      settings(cdm$nsaids_aesi) %>%
        select(cohort_definition_id, index_id, index_name, marker_name),
      by = "cohort_definition_id",
      copy = TRUE
    ) %>%
    filter(!marker_name %in% c("gi_hemorrhage", "stroke_broad")) %>%
    mutate(cohort_definition_id = index_id) %>%
    PatientProfiles::addDemographics(
      indexDate = "index_date",
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
    ) %>% 
    compute(name = "nsaids_characteristics", temporary = FALSE)


  # need to create new settings, attrition and codelist
  # settings
  new_settings <- cdm$nsaids_characteristics %>%
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
  cdm$nsaids_characteristics <- omopgenerics::newCohortTable(
    table = cdm$nsaids_characteristics,
    cohortSetRef = new_settings,
    cohortAttritionRef = new_attrition,
    cohortCodelistRef = new_codelist_ref,
    .softValidation = TRUE
    
  )
  
  info(logger, "ADDED DEMOGRAPHICS TO COHORT")
  
  suppressWarnings({
    summarycharacteristics <- cdm$nsaids_characteristics %>%
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
  
  cli::cli_alert_info("Exporting table one characteristics results ({Sys.time()})")
  info(logger, "EXPORTING TABLE ONE CHARACTERISTICS RESULTS")
  
  omopgenerics::exportSummarisedResult(
    summarycharacteristics,
    minCellCount = 5,
    path = here::here(characterisation_folder),
    fileName = paste0(cdmName(cdm), "_summary_characteristics.csv")
  )
  info(logger, "EXPORTED TABLE ONE CHARACTERISTICS RESULTS")
  
  cli::cli_alert_success("Table one Characterisation Analysis Complete ({Sys.time()})")
  info(logger, "COMPLETED TABLE ONE CHARACTERISATION ANALYSIS")
}
