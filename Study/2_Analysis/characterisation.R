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
  
  codelistConditions <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm)
  
  cdm <- CDMConnector::generateConceptCohortSet(
    cdm = cdm,
    conceptSet = codelistConditions,
    name = "conditions",
    overwrite = TRUE
  )
  
  cli::cli_alert_info("Summarising table one Comorbidities ({Sys.time()})")
  info(logger, "SUMMARISING TABLE ONE COMORBIDITIES")
  
  # medications -----
  cli::cli_alert_info("Instantiating table one Medications ({Sys.time()})")
  info(logger, "INSTANTIATING TABLE ONE MEDICATIONS")
  
  codelistMedications <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm)
  
  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(
    cdm = cdm,
    conceptSet = codelistMedications,
    name = "medications"
  )
  
  cli::cli_alert_info("Summarising table one Medications ({Sys.time()})")
  info(logger, "SUMMARISING TABLE ONE MEDICATIONS")
  
  # add sex and age to cohorts ----
  cli::cli_alert_info("Add demographics to cohort ({Sys.time()})")
  info(logger, "ADDING DEMOGRAPHICS TO COHORT")
  
  cdm$nsaids_characteristics <- cdm$nsaids_aesi %>%
    PatientProfiles::addDemographics()
  
  cdm$nsaids_characteristics <- cdm$nsaids_characteristics %>%
    PatientProfiles::addDemographics(ageGroup = list(
      "age_group" = list(
        "18 to 49" = c(18, 49),
        "50 to 59" = c(50, 59),
        "60 to 69" = c(60, 69),
        "70 to 79" = c(70, 79),
        "80 to 89" = c(80, 89),
        "90+" = c(90, 150)
      )
    ))
  info(logger, "ADDED DEMOGRAPHICS TO COHORT")
  
  cdm$nsaids_characteristics <- cdm$nsaids_characteristics %>%
    filter(prior_observation >= 365)
  
  cdm$nsaids_characteristics <- cdm$nsaids_characteristics %>%
    compute(name = "nsaids_characteristics", temporary = FALSE, overwrite = TRUE) %>%
    CDMConnector::recordCohortAttrition(reason = "Excluded patients with less than 365 prior history")
  
  info(logger, "EXCLUDED PATIENTS WITH LESS THAN 365 PRIOR HISTORY")
  
  cdm$nsaids_characteristics <- cdm$nsaids_characteristics %>%
    filter(sex %in% c("Male", "Female"))
  
  info(logger, "EXCLUDED PATIENTS WITH NO SEX RECORDED")

  cdm$nsaids_characteristics <- cdm$nsaids_characteristics %>%
    CohortConstructor::requireIsFirstEntry()
  
  info(logger, "CHARACTERISING AT FIRST ENTRY")
  
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
