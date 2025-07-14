# positive controls -------
#log("- Getting benchmarker definitions drug - drug positive controls")

#cdm <- DrugUtilisation::generateIngredientCohortSet(
#  cdm = cdm,
#  name = "amiodarone",
#  ingredient = "amiodarone",
#  gapEra = 30
#)


#cdm <- DrugUtilisation::generateIngredientCohortSet(
#  cdm = cdm,
#  name = "levothyroxine",
#  ingredient = "levothyroxine",
#  gapEra = 30
#)

#cli::cli_alert_success("- Got benchmarker definitions drug - drug positive controls")

# negative controls -------
#log("- Getting benchmarker definitions drug - drug negative controls")

#cdm <- DrugUtilisation::generateIngredientCohortSet(
#  cdm = cdm,
#  name = "allopurinol",
#  ingredient = "allopurinol",
#  gapEra = 30
#)

#cli::cli_alert_success("- Got benchmarker definitions drug - drug negative controls")


# ace inhibitors ----
#ace_inhib <- omopgenerics::importCodelist(path = "1_InstantiateCohorts/Codelists/C09A_ace_inhibitors_plain.csv", type = "csv")
#cdm[["ace_inh"]] <- conceptCohort(cdm,
#                                  conceptSet = ace_inhib,
 #                                 name = "ace_inh",
  #                                exit = "event_end_date",
   #                               useSourceFields = FALSE,
    #                              subsetCohort = NULL,
     #                             subsetCohortId = NULL)

#cough_codes <- read.csv("1_InstantiateCohorts/Codelists/coughCodes.csv")


#cdm[["cough"]] <- conceptCohort(cdm,
#                                conceptSet = list(cough_codes = cough_codes$concept_id),
#                                name = "cough",
#                                exit = "event_end_date",
#                                useSourceFields = FALSE,
#                                subsetCohort = NULL,
#                                subsetCohortId = NULL)


#cli::cli_alert_success("- Getting nsaids")

# inclusions are nsaids with minimum count of 1000

nsaids_codelist1 <- omopgenerics::importCodelist(path = "1_InstantiateCohorts/Codelists/NSAIDs", type = "csv")

nsaids_codelist2 <- subsetToCodesInUse(nsaids_codelist1, 
                                          minimumCount = 1000,
                                          table = c("drug_exposure"),
                                          cdm = cdm)

# instantiate the nsaids using drug utilisation package function
# all ingredients will be in one table but with unique cohort_definition ids

cdm <- generateDrugUtilisationCohortSet(
      cdm = cdm,
      name = "nsaids",
      conceptSet = nsaids_codelist2 ,
      gapEra = 30
    )

# restrict to study period and age range
 cdm$nsaids %>% 
  CohortConstructor::requireInDateRange(dateRange = as.Date(c(starting_date, ending_date))) %>% 
  CohortConstructor::requireAge(indexDate = "cohort_start_date",
             ageRange = list(c(18, 150)))

# generate outcome cohorts AESI's ---------
# gi hemorrhage
# acute MI (heart attack) 
# ischemic stroke
# haemorrhagic stroke 
# stroke (is + hs)
# arrhythmia
# heart failure  
# deep vein thrombosis (DVT)
# pulmonary embolism
# heart failure

# instantiate outcome cohorts
cli::cli_alert_info("- Getting outcome definitions")
    
# get concept sets from cohorts----
# apart from the GI related ones the rest are from Darwin adverse events of special interest (aesi)
aesi_codelists <- CodelistGenerator::codesFromCohort(
  path = here::here("1_InstantiateCohorts", "cohorts"),
  cdm = cdm
)

# use cohort constructor to create cohort with age restriction and study period restriction
cdm$aesi <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = aesi_codelists,
  name = "aesi",
) %>% 
  CohortConstructor::requireInDateRange(dateRange = as.Date(c(starting_date, ending_date))) %>% 
  CohortConstructor::requireAge(indexDate = "cohort_start_date",
                                ageRange = list(c(18, 150)))
    
cli::cli_alert_success("- Got outcome definitions")

    