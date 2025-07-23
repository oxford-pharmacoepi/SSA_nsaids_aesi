# positive controls -------
cli::cli_alert_info("- Getting benchmarker definitions drug - drug positive controls")
info(logger, "GETTING BENCHMARK DEFINITIONS DRUG-DRUG POSITIVE CONTROLS")

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
info(logger, "GOT BENCHMARK DEFINITIONS DRUG-DRUG POSITIVE CONTROLS")

# negative controls -------
cli::cli_alert_info("- Getting benchmarker definitions drug - drug negative controls")
info(logger, "GETTING BENCHMARK DEFINITIONS DRUG-DRUG NEGATIVE CONTROLS")

cdm <- DrugUtilisation::generateIngredientCohortSet(
  cdm = cdm,
  name = "allopurinol",
  ingredient = "allopurinol",
  gapEra = 30
)

cli::cli_alert_success("- Got benchmarker definitions drug - drug negative controls")
info(logger, "GOT BENCHMARK DEFINITIONS DRUG-DRUG NEGATIVE CONTROLS")


# ace inhibitors ----
cli::cli_alert_info("- Creating ace inhibitor and cough cohorts")
info(logger, "CREATING ACE INHIBITOR AND COUGH COHORTS")

ace_inh <- omopgenerics::importCodelist(path = "1_InstantiateCohorts/Codelists/C09A_ace_inhibitors_plain.csv", type = "csv")

cdm[["ace_inh"]] <- conceptCohort(cdm,
                                  conceptSet = ace_inh,
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

cli::cli_alert_success("- Created ace inhibitor and cough cohorts")
info(logger, "CREATED ACE INHIBITOR AND COUGH COHORTS")

##phenotyped controls
cli::cli_alert_success("- Creating phenotyped control cohorts")
info(logger, "CREATING PHENOTYPED CONTROL COHORTS")

#acute kidney injury
cli::cli_alert_info("- Creating AKI control cohort")
info(logger, "CREATING AKI CONTROL COHORT")

aki_concept_ids <- read.csv("1_InstantiateCohorts/Controls/AKI.csv") |>
  dplyr::filter(tolower(overall) == "y") |>
  dplyr::pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

aki_codes <- list(
  aki = aki_concept_ids
)

cdm[["aki"]] <- conceptCohort(
  cdm,
  conceptSet = aki_codes,
  exit = "event_end_date",
  useSourceFields = FALSE,
  name = "aki"
)

cli::cli_alert_success("- Created AKI control cohort")
info(logger, "CREATED AKI CONTROL COHORT")

#nausea diagnosis
cli::cli_alert_success("- Creating nausea control cohort")
info(logger, "CREATING NAUSEA CONTROL COHORT")

nausea_concept_ids <- read.csv("1_InstantiateCohorts/Controls/Nausea.csv") |>
  pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

nausea_codes <- list(
  nausea = nausea_concept_ids
)

cdm[["nausea"]] <- conceptCohort(
  cdm,
  conceptSet = nausea_codes,
  exit = "event_end_date",
  useSourceFields = FALSE,
  name = "nausea"
)

cli::cli_alert_success("- Created nausea control cohort")
info(logger, "CREATED NAUSEA CONTROL COHORT")

#vomiting diagnosis
cli::cli_alert_info("- Creating vomiting control cohort")
info(logger, "CREATING VOMITING CONTROL COHORT")

vomiting_concept_ids <- read_csv("1_InstantiateCohorts/Controls/Vomit.csv") |>
  pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

vomiting_codes <- list(
  vomiting = vomiting_concept_ids
)

cdm[["vomiting"]] <- conceptCohort(
  cdm,
  conceptSet = vomiting_codes,
  exit = "event_end_date",
  useSourceFields = FALSE,
  name = "vomiting"
)

cli::cli_alert_success("- Created vomiting control cohort")
info(logger, "CREATED VOMITING CONTROL COHORT")

#anemia diagnosis
cli::cli_alert_info("- Creating anemia control cohort")
info(logger, "CREATING ANEMIA CONTROL COHORT")

anemia_concept_ids <- read.csv("1_InstantiateCohorts/Controls/Anemia.csv") |>
  pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

anemia_codes <- list(
  anemia = anemia_concept_ids
)

cdm[["anemia"]] <- conceptCohort(
  cdm,
  conceptSet = anemia_codes,
  exit = "event_end_date",
  useSourceFields = FALSE,
  name = "anemia"
)
cli::cli_alert_success("- Created anemia control cohort")
info(logger, "CREATED ANEMIA CONTROL COHORT")

#cataracts diagnosis
cli::cli_alert_info("- Creating cataracts control cohort")
info(logger, "CREATING CATARACTS CONTROL COHORT")

cataracts_concept_ids <- read.csv("1_InstantiateCohorts/Controls/Cataracts.csv") |>
  pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

cataracts_codes <- list(
  cataracts = cataracts_concept_ids
)

cdm[["cataracts"]] <- conceptCohort(
  cdm,
  conceptSet = cataracts_codes,
  exit = "event_end_date",
  useSourceFields = FALSE,
  name = "cataracts"
)
cli::cli_alert_success("- Created cataracts control cohort")
info(logger, "CREATED CATARACTS CONTROL COHORT")

#asthma diagnosis
cli::cli_alert_info("- Creating asthma control cohort")
info(logger, "CREATING ASTHMA CONTROL COHORT")

asthma_concept_ids <- read.csv("1_InstantiateCohorts/Controls/Asthma.csv") |>
  pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

asthma_codes <- list(
  asthma = asthma_concept_ids
)

cdm[["asthma"]] <- conceptCohort(
  cdm,
  conceptSet = asthma_codes,
  exit = "event_end_date",
  useSourceFields = FALSE,
  name = "asthma"
)
cli::cli_alert_success("- Created asthma control cohort")
info(logger, "CREATED ASTHMA CONTROL COHORT")

#edema diagnosis
cli::cli_alert_info("- Creating edema control cohort")
info(logger, "CREATING EDEMA CONTROL COHORT")

edema_concept_ids <- read_csv("1_InstantiateCohorts/Controls/Edema.csv") |>
  pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

edema_codes <- list(
  edema = edema_concept_ids
)

cdm[["edema"]] <- conceptCohort(
  cdm,
  conceptSet = edema_codes,
  exit = "event_end_date",
  useSourceFields = FALSE,
  name = "edema"
)

cli::cli_alert_success("- Created edema control cohort")
info(logger, "CREATED EDEMA CONTROL COHORT")

cli::cli_alert_success("- Created phenotyped control cohorts")
info(logger, "CREATED PHENOTYPED CONTROL COHORTS")

#NSAIDs
cli::cli_alert_info("- Getting nsaids")
info(logger, "GETTING NSAIDS")

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

cli::cli_alert_success("- Got nsaids")
info(logger, "GOT NSAIDS")

cli::cli_alert_info("- Getting outcome definitions")
info(logger, "GETTING OUTCOME DEFINITIONS")

# get concept sets from cohorts----
# apart from the GI related ones the rest are from Darwin adverse events of special interest (aesi)
aesi_codelists <- CodelistGenerator::codesFromCohort(
  path = here::here("1_InstantiateCohorts", "Cohorts"),
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
info(logger, "GOT OUTCOME DEFINITIONS")

cli::cli_alert_info("- Get SA cohorts")
info(logger, "GET SA COHORTS")

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
 
#cox_2_pref_codelist <- omopgenerics::importCodelist(path = here::here("1_InstantiateCohorts", "Codelists", "cox_2_pref.csv"), type = "csv")

 #Non selective with cox 2 preference
#cdm <- generateDrugUtilisationCohortSet(
#  cdm = cdm,
#  name = "cox_2_pref",
#  conceptSet = cox_2_pref_codelist,
#  gapEra = 30
#)
 
#cox_1_pref_codelist <- omopgenerics::importCodelist(path = here::here("1_InstantiateCohorts", "Codelists", "cox_1_pref.csv"), type = "csv")

 #Non selective with cox 1 preference#
#cdm <- generateDrugUtilisationCohortSet(
#  cdm = cdm,
#  name = "cox_1_pref",
#  conceptSet = cox_1_pref_codelist,
#  gapEra = 30
#)
 
 cdm <- omopgenerics::bind(
   cdm$all_nsaids,
   cdm$cox_2,
   cdm$non_selective,
   #cdm$cox_2_pref,
   #cdm$cox_1_pref,
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
   CohortConstructor::requireTableIntersect(
     intersections = 0,
     tableName = "hypertension",
     indexDate = "cohort_start_date",
     window = c(-Inf,0),
     name = "nsaids_no_hypertension"
   )
 
 cdm$nsaids_prior_hypertension <- cdm$nsaids |>
   CohortConstructor::requireTableIntersect(
     tableName = "hypertension",
     indexDate = "cohort_start_date",
     window = c(-Inf,0),
     name = "nsaids_prior_hypertension"
   )
 
## anticoagulants
 # anticoagulants <- getATCCodes(
 #   cdm,
 #   level = c("ATC 4th"),
 #   name = c("Vitamin K antagonists",
 #            "Heparin group",
 #            "Direct thrombin inhibitors",
 #            "Direct factor Xa inhibitors"),
 #   doseForm = NULL,
 #   doseUnit = NULL,
 #   routeCategory = NULL,
 #   type = "codelist"
 # )
 # 
 # cdm$anticoagulants <- CohortConstructor::conceptCohort(
 #   cdm = cdm,
 #   conceptSet = anticoagulants,
 #   name = "anticoagulants"
 # )
 # 
 # 
 # cdm$nsaids_no_anticoagulants <- cdm$nsaids |>
 #   CohortConstructor::requireTableIntersect(
 #     intersections = 0,
 #     tableName = "anticoagulants",
 #     indexDate = "cohort_start_date",
 #     window = c(-Inf,0),
 #     name = "nsaids_no_anticoagulants"
 #   )
 # 
 # cdm$nsaids_prior_anticoagulants <- cdm$nsaids |>
 #   CohortConstructor::requireTableIntersect(
 #     tableName = "anticoagulants",
 #     indexDate = "cohort_start_date",
 #     window = c(-Inf,0),
 #     name = "nsaids_prior_anticoagulants"
 #   )
 


cli::cli_alert_success("- Completed Instantiate Cohorts")
info(logger, "COMPLETED INSTANTIATE COHORTS")
