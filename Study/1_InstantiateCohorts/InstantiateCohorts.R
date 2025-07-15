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
cli::cli_alert_success("- Creating ace inhibitor and cough cohorts")
info(logger, "CREATING ACE INHIBITOR AND COUGH COHORTS")

ace_inhib <- getATCCodes(
  cdm,
  level = c("ATC 3rd"),
  name = "ACE INHIBITORS, PLAIN",
  doseForm = NULL,
  doseUnit = NULL,
  routeCategory = NULL,
  type = "codelist"
)

cdm[["ace_inh"]] <- conceptCohort(cdm,
                                  conceptSet = ace_inhib,
                                  name = "ace_inh",
                                  exit = "event_end_date",
                                  useSourceFields = FALSE,
                                  subsetCohort = NULL,
                                  subsetCohortId = NULL)

# cough diagnosis ----
cough_codes <- getDescendants(cdm, 
                              conceptId = c(254761))

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
cli::cli_alert_success("- Creating AKI control cohort")
info(logger, "CREATING AKI CONTROL COHORT")

concept_ids <- readr::read_csv("1_InstantiateCohorts/Controls/AKI.csv") |>
  dplyr::filter(tolower(overall) == "y") |>
  dplyr::pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

aki_codes <- list(
  aki = concept_ids
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

concept_ids <- read_csv("1_InstantiateCohorts/Controls/Nausea.csv") |>
  pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

nausea_codes <- list(
  nausea = concept_ids
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
cli::cli_alert_success("- Creating vomiting control cohort")
info(logger, "CREATING VOMITING CONTROL COHORT")

concept_ids <- read_csv("1_InstantiateCohorts/Controls/Vomit.csv") |>
  pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

vomiting_codes <- list(
  vomiting = concept_ids
)

cdm[["vomit"]] <- conceptCohort(
  cdm,
  conceptSet = vomiting_codes,
  exit = "event_end_date",
  useSourceFields = FALSE,
  name = "vomit"
)
cli::cli_alert_success("- Created vomiting control cohort")
info(logger, "CREATED VOMITING CONTROL COHORT")

#anemia diagnosis
cli::cli_alert_success("- Creating anemia control cohort")
info(logger, "CREATING ANEMIA CONTROL COHORT")

concept_ids <- read_csv("1_InstantiateCohorts/Controls/Anemia.csv") |>
  pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

anemia_codes <- list(
  anemia = concept_ids
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
cli::cli_alert_success("- Creating cataracts control cohort")
info(logger, "CREATING CATARACTS CONTROL COHORT")

concept_ids <- read_csv("1_InstantiateCohorts/Controls/Cataracts.csv") |>
  pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

cataracts_codes <- list(
  cataracts = concept_ids
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
cli::cli_alert_success("- Creating asthma control cohort")
info(logger, "CREATING ASTHMA CONTROL COHORT")

concept_ids <- read_csv("1_InstantiateCohorts/Controls/Asthma.csv") |>
  pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

asthma_codes <- list(
  asthma = concept_ids
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
cli::cli_alert_success("- Creating edema control cohort")
info(logger, "CREATING EDEMA CONTROL COHORT")

concept_ids <- read_csv("1_InstantiateCohorts/Controls/Edema.csv") |>
  pull(concept_id) |>
  as.numeric() |>
  na.omit() |>
  unique()

edema_codes <- list(
  edema = concept_ids
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
cli::cli_alert_success("- Getting nsaids")
info(logger, "GETTING NSAIDS")

# use codelists generator to get the ingredient levels for all nsaids -----
# extract ATC for NSAIDS at 4th level to get the ingredients

# M01AA Butylpyrazolidines
# M01AB Acetic acid derivatives and related substances
# M01AC Oxicams
# M01AE Propionic acid derivatives
# M01AG Fenamates
# M01AH Coxibs
# M01AX other anti inflammatories

nsaids_lists <- getATCCodes(
  cdm,
  level = c("ATC 4th"),
  name = c("Butylpyrazolidines",
           "Acetic acid derivatives and related substances",
           "Oxicams",
           "Propionic acid derivatives",
           "Fenamates",
           "Coxibs",
           "Other antiinflammatory and antirheumatic agents, non-steroids"),
  doseForm = NULL,
  doseUnit = NULL,
  routeCategory = NULL,
  type = "codelist_with_details"
)

# collapse the elements of the lists
nsaids_lists_ingredients <- nsaids_lists %>% 
  data.table::rbindlist()

# get the ingredients from the list by binding with concept table
nsaids_lists_ingredients <- cdm$concept %>% filter(concept_id %in% nsaids_lists_ingredients$concept_id) %>% 
  filter(concept_class_id == "Ingredient") %>% 
  collect()

# filter out the ones which are not nsaids
# exclusions these are ingredients give in combination which are not nsaids
exclusions <- c("methocarbamol",
                "carisoprodol" ,
                "magaldrate",
                "magnesium carbonate",
                "aluminum hydroxide",
                "betamethasone",
                "dexpanthenol",
                "loperamide",
                "misoprostol",
                "codeine",
                "vitamin B12",
                "dexamethasone",
                "amoxicillin",
                "ampicillin",
                "pridinol",
                "cholestyramine resin",
                "magnesium",
                "thiamine",
                "pyridoxine",
                "avocado oil",
                "polysulfated glycosaminoglycan",
                "avocado soybean unsaponifiables",
                "diacetylrhein",
                "orgotein",
                "soybean oil",
                "bumadizone",
                "benzydamine",
                "oxaceprol",
                "bufexamac",
                "chondroitin sulfates",
                "glucosamine")

# remove the exclusions from the list of nsaid ingredients
nsaids_lists_ingredients <- nsaids_lists_ingredients %>% 
  filter(!(concept_name %in% exclusions))


# other NSAIDs are caputured elsewhere in ATC classification:
nsaids_lists1 <- getATCCodes(
  cdm,
  level = c("ATC 4th"),
  name = c("Salicylic acid and derivatives",
           "Pyrazolones",
           "Other analgesics and antipyretics"),
  doseForm = NULL,
  doseUnit = NULL,
  routeCategory = NULL,
  type = "codelist_with_details"
)

nsaids_lists_ingredients1 <- nsaids_lists1 %>% 
  data.table::rbindlist()

# get the ingredients from the list by binding with concept table
nsaids_lists_ingredients1 <- cdm$concept %>% filter(concept_id %in% nsaids_lists_ingredients1$concept_id) %>% 
  filter(concept_class_id == "Ingredient") %>% 
  collect()


# filter out the ones which are nsaids
# inclusions are ingredients which are nsaids
inclusions <- c("methyl salicylate",
                "salicylic acid" ,
                "diflunisal",
                "salsalate",
                "aspirin",
                "salicylamide",
                "magnesium salicylate",
                "morpholine salicylate",
                "Guacetisal",
                "Carbasalate Calcium",
                "Salacetamide",
                "aloxiprin",
                "ethenzamide",
                "dipyrocetyl" ,
                "phenazone",
                "propyphenazone",
                "aminophenazone",
                "nifenazone",
                "floctafenine")

# keep the inclusions from the list of nsaid ingredients
nsaids_lists_ingredients1 <- nsaids_lists_ingredients1 %>% 
  filter(concept_name %in% inclusions)


# bind rows to get final list
nsaids_lists_ingredients <- bind_rows(nsaids_lists_ingredients,
                                      nsaids_lists_ingredients1)

# put the nsaids list into codelist generator and do more filtering of concepts
nsaids_codelist1 <- getDrugIngredientCodes(
  cdm,
  name = nsaids_lists_ingredients$concept_name,
  nameStyle = "{concept_name}",
  routeCategory = "oral",
  ingredientRange = c(1, 1),
  type = "codelist"
)


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

cli::cli_alert_success("- Got nsaids")
info(logger, "GOT NSAIDS")

# #create hypertension and no hypertension cohorts
# cli::cli_alert_success("- Creating hypertension and no hypertension cohorts")
# info(logger, "CREATING HYPERTENSION COHORT")
# 
# hyper_codelists <- CodelistGenerator::codesFromConceptSet(
#   path = here::here("1_InstantiateCohorts", "Conditions", "hypertension.json"),
#   cdm = cdm
# )
# 
# cdm$hypertension <- CohortConstructor::conceptCohort(
#   cdm = cdm,
#   conceptSet = hyper_codelists,
#   name = "hypertension"
# )
# 
# 
# cdm$nsaids_no_hypertension <- cdm$nsaids |>
#   CohortConstructor::requireCohortIntersect(
#     intersections = 0,
#     targetCohortTable = "hypertension",
#     indexDate = "cohort_start_date",
#     window = c(-Inf,0),
#     name = "nsaids_no_hypertension"
#   )
# 
# cdm$nsaids_prior_hypertension <- cdm$nsaids |>
#   CohortConstructor::requireCohortIntersect(
#     intersections = c(1,Inf),
#     targetCohortTable = "hypertension",
#     indexDate = "cohort_start_date",
#     window = c(-Inf,0),
#     name = "nsaids_prior_hypertension"
#   )
# 
# cli::cli_alert_success("- Created hypertension and no hypertension cohorts")
# info(logger, "CREATED HYPERTENSION COHORT")


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
info(logger, "GETTING OUTCOME DEFINITIONS")
    
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
info(logger, "GOT OUTCOME DEFINITIONS")

# #create cohorts with all nsaids and by COX mechanism
# cli::cli_alert_success("- Creating cohorts with all nsaids and by COX mechanism")
# info(logger, "CREATING COHORTS WITH ALL NSAIDS AND BY COX MECHANISM")

# #all nsaids cohort
# cdm$all_nsaids <-  cdm$nsaids %>% 
#   CohortConstructor::unionCohorts(
#     cohortName = "all_nsaids",
#     name = "all_nsaids"
#   ) # cohortId = c(6, 12, 19, 25, 30)
# 
# 
# #Cox2 selective cohort
# cdm$cox_2 <-  cdm$nsaids %>% 
#   CohortConstructor::unionCohorts(
#     cohortId = c(6, 12, 19, 25, 30),
#     cohortName = "cox_2",
#     name = "cox_2"
#   )
# 
# #Non selective cohort
# cdm$non_selective <-  cdm$nsaids %>% 
#   CohortConstructor::unionCohorts(
#     cohortId = setdiff(omopgenerics::settings(cdm$nsaids) %>% dplyr::pull("cohort_definition_id"), c(6,12,19,25,30)),
#     cohortName = "non_selective",
#     name = "non_selective"
#   )
# 
# #Non selective with cox 2 preference
# cdm$cox_2_preference <-  cdm$nsaids %>% 
#   CohortConstructor::unionCohorts(
#     cohortId = c(1, 9, 11, 21, 22, 26),
#     cohortName = "cox_2_preference",
#     name = "cox_2_preference"
#   )
# 
# #Non selective with cox 1 preference
# cdm$cox_1_preference <-  cdm$nsaids %>% 
#   CohortConstructor::unionCohorts(
#     cohortId = c(5, 15, 17, 18, 23, 24),
#     cohortName = "cox_1_preference",
#     name = "cox_1_preference"
#   )
# 
# cdm <- omopgenerics::bind(
#   cdm$nsaids,
#   cdm$all_nsaids,
#   cdm$cox_2,
#   cdm$non_selective,
#   cdm$cox_2_preference,
#   cdm$cox_1_preference,
#   name = "nsaids"
# )

cli::cli_alert_success("- Completed Instantiate Cohorts")
info(logger, "COMPLETED INSTANTIATE COHORTS")