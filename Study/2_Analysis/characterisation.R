if(isTRUE(run_characterisation)){
  
  # demographics ----
  cli::cli_alert_info("Summarising Table One Demographics")
  
  
  # comorbidities --------
  cli::cli_alert_info("Instantiating table one Comorbidities")
  
  codelistConditions <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm)
  
  cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, 
                                                conceptSet = codelistConditions,
                                                name = "conditions",
                                                overwrite = TRUE)
  
  cli::cli_alert_info("Summarising table one Comorbidities")
  
  
  # medications -----
  cli::cli_alert_info("Instantiating table one Medications")
  
  # instantiate medications
  codelistMedications <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm)
  
  cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(cdm = cdm, 
                                                           conceptSet = codelistMedications, 
                                                           name = "medications")
  
  cli::cli_alert_info("Summarising table one Medications")
  
  
  # add sex and age to cohorts ----
  cli::cli_alert_info("Add demographics to cohort")
  
  # get nsaid cohort and add in inclusion/exclusion ---
  cdm$nsaids_characteristics <- cdm$nsaids %>% 
    PatientProfiles::addDemographics()
  
  cdm$nsaids_characteristics <- cdm$nsaids_characteristics %>% 
    PatientProfiles::addDemographics(
      ageGroup = list(
        "age_group" =
          list(
            "18 to 49" = c(18, 49),
            "50 to 59" = c(50, 59),
            "60 to 69" = c(60, 69),
            "70 to 79" = c(70, 79),
            "80 to 89" = c(80, 89),
            "90+" = c(90, 150)
          )
      )) 
  

  #for those with prior history remove those with less than 365 days of prior history -------
  cdm$nsaids_characteristics <- cdm$nsaids_characteristics %>% 
    filter(prior_observation >= 365) 
  
  # make outcome a perm table and update the attrition
  cdm$nsaids_characteristics <- cdm$nsaids_characteristics %>% 
    compute(name = "nsaids_characteristics", temporary = FALSE, overwrite = TRUE) %>% 
    CDMConnector::recordCohortAttrition(reason="Excluded patients with less than 365 prior history" )
  
  #only keeping those with sex recorded
  cdm$nsaids_characteristics <- cdm$nsaids_characteristics %>% 
    filter(sex %in% c("Male", "Female"))
  
  # make outcome a perm table and update the attrition
  cdm$nsaids_characteristics <- cdm$nsaids_characteristics %>% 
    compute(name = "nsaids_characteristics", temporary = FALSE, overwrite = TRUE) %>% 
    CDMConnector::recordCohortAttrition(reason="Excluded patients with no sex recorded" )
  
  # collaspse the records in cohort
  cdm$nsaids_characteristics <- cdm$nsaids_characteristics |> 
    CohortConstructor::collapseCohorts(gap = Inf, name = "nsaids_characteristics")
  
  # characterise comorbs (any time prior) and medications (1 year prior) ot nsaid use
  suppressWarnings(
    
    summarycharacteristics <- cdm$nsaids_characteristics %>%
      CohortCharacteristics::summariseCharacteristics(
        ageGroup = list( "18 to 49" = c(18, 49),
                         "50 to 59" = c(50, 59),
                         "60 to 69" = c(60, 69),
                         "70 to 79" = c(70, 79),
                         "80 to 89" = c(80, 89),
                         "90+" = c(90, 150)),
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
    
  )
  
  cli::cli_alert_info("Exporting table one characteristics results")
  omopgenerics::exportSummarisedResult(summarycharacteristics,
                                       minCellCount = 5,
                                       path = here("Results",db_name),
                                       fileName = paste0(cdmName(cdm),
                                                         "_summary_characteristics.csv")
  )
  
  cli::cli_alert_success("Table one Characterisation Analysis Complete")
  
}