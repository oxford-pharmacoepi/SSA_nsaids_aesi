# positive controls -------
log("- Getting benchmarker definitions drug - drug positive controls")

cdm <- DrugUtilisation::generateIngredientCohortSet(
  cdm = cdm,
  name = "amiodarone",
  ingredient = "amiodarone",
  gapEra = 30
)


cdm <- DrugUtilisation::generateIngredientCohortSet(
  cdm = cdm,
  name = "levothyroxine",
  ingredient = "levothyroxine",
  gapEra = 30
)

cli::cli_alert_success("- Got benchmarker definitions drug - drug positive controls")

# negative controls -------
log("- Getting benchmarker definitions drug - drug negative controls")

cdm <- DrugUtilisation::generateIngredientCohortSet(
  cdm = cdm,
  name = "allopurinol",
  ingredient = "allopurinol",
  gapEra = 30
)

cli::cli_alert_success("- Got benchmarker definitions drug - drug negative controls")


# ace inhibitors ----
ace_inhib <- omopgenerics::importCodelist(path = "1_InstantiateCohorts/Codelists/C09A_ace_inhibitors_plain.csv", type = "csv")
cdm[["ace_inh"]] <- conceptCohort(cdm,
                                  conceptSet = ace_inhib,
                                 name = "ace_inh",
                                exit = "event_end_date",
                               useSourceFields = FALSE,
                              subsetCohort = NULL,
                             subsetCohortId = NULL)

cough_codes <- read.csv("1_InstantiateCohorts/Codelists/coughCodes.csv")


cdm[["cough"]] <- conceptCohort(cdm,
                                conceptSet = list(cough_codes = cough_codes$concept_id),
                                name = "cough",
                                exit = "event_end_date",
                                useSourceFields = FALSE,
                                subsetCohort = NULL,
                                subsetCohortId = NULL)


cli::cli_alert_success("- Getting nsaids")

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
      conceptSet = nsaids_codelist2,
      gapEra = 30
    )

# restrict to study period and age range
cdm$nsaids |> 
  CohortConstructor::requireAge(indexDate = "cohort_start_date",
             ageRange = list(c(18, 150))) |> 
  CohortConstructor::requireInDateRange(dateRange = as.Date(c(starting_date, ending_date))) 

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
)  |> 
  CohortConstructor::requireAge(indexDate = "cohort_start_date",
                                ageRange = list(c(18, 150))) |> 
  CohortConstructor::requireInDateRange(dateRange = as.Date(c(starting_date, ending_date))) 
    
cli::cli_alert_success("- Got outcome definitions")

###
all_nsaids <- omopgenerics::importCodelist(path = here::here("1_InstantiateCohorts", "Codelists", "all_nsaids.csv"), type = "csv")

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "all_nsaids",
  conceptSet = all_nsaids ,
  gapEra = 30
) 

#Cox2 selective cohort
cox_2_codelist <- omopgenerics::importCodelist(path = here::here("1_InstantiateCohorts", "Codelists", "cox_2.csv"), type = "csv")

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "cox_2",
  conceptSet = cox_2_codelist,
  gapEra = 30
)
 
non_selective_codelist <- omopgenerics::importCodelist(path = here::here("1_InstantiateCohorts", "Codelists", "non_selective.csv"), type = "csv")

cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "non_selective",
  conceptSet = non_selective_codelist,
  gapEra = 30
)
 
cox_2_pref_codelist <- omopgenerics::importCodelist(path = here::here("1_InstantiateCohorts", "Codelists", "cox_2_pref.csv"), type = "csv")

 #Non selective with cox 2 preference
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "cox_2_pref",
  conceptSet = cox_2_pref_codelist,
  gapEra = 30
)
 
cox_1_pref_codelist <- omopgenerics::importCodelist(path = here::here("1_InstantiateCohorts", "Codelists", "cox_1_pref.csv"), type = "csv")

 #Non selective with cox 1 preference
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "cox_1_pref",
  conceptSet = cox_1_pref_codelist,
  gapEra = 30
)
 
 cdm <- omopgenerics::bind(
   cdm$all_nsaids,
   cdm$cox_2,
   cdm$non_selective,
   cdm$cox_2_pref,
   cdm$cox_1_pref,
   name = "nsaids_sa"
 ) 
 
 cdm$nsaids_sa |> 
   CohortConstructor::requireAge(indexDate = "cohort_start_date",
                                 ageRange = list(c(18, 150))) |> 
   CohortConstructor::requireInDateRange(dateRange = as.Date(c(starting_date, ending_date))) 
 
 ###
 
 hyper_codelist <- omopgenerics::importCodelist(path = "1_InstantiateCohorts/Codelists/hypertension.csv", type = "csv")
 
 cdm$hypertension <- CohortConstructor::conceptCohort(
   cdm = cdm,
   conceptSet = hyper_codelist,
   name = "hypertension"
 )
 
 cdm$nsaids_no_hypertension <- cdm$nsaids |>
   CohortConstructor::requireCohortIntersect(
     intersections = 0,
     targetCohortTable = "hypertension",
     indexDate = "cohort_start_date",
     window = c(-Inf,0),
     name = "nsaids_no_hypertension"
   )
 
 cdm$nsaids_prior_hypertension <- cdm$nsaids |>
   CohortConstructor::requireCohortIntersect(
     intersections = c(1,Inf),
     targetCohortTable = "hypertension",
     indexDate = "cohort_start_date",
     window = c(-Inf,0),
     name = "nsaids_prior_hypertension"
   )