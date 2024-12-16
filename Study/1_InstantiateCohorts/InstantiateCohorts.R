# positive controls -------
#log("- Getting benchmarker definitions drug - drug positive controls")

cdm <- DrugUtilisation::generateIngredientCohortSet(
  cdm = cdm,
  name = "amiodarone",
  ingredient = "amiodarone"
)


cdm <- DrugUtilisation::generateIngredientCohortSet(
  cdm = cdm,
  name = "levothyroxine",
  ingredient = "levothyroxine",
  gapEra = 30
)

cli::cli_alert_success("- Got benchmarker definitions drug - drug positive controls")

# negative controls -------
#log("- Getting benchmarker definitions drug - drug negative controls")

cdm <- DrugUtilisation::generateIngredientCohortSet(
  cdm = cdm,
  name = "allopurinol",
  ingredient = "allopurinol",
  gapEra = 30
)

cli::cli_alert_success("- Got benchmarker definitions drug - drug negative controls")

#log("- Getting benchmarker definitions conditions")


cli::cli_alert_success("- Getting nsaids")


# use codelists generator to get the ingredient levels for all nsaids
# extract ATC for NSAIDS at 4th level to get the ingredients

# M01AA Butylpyrazolidines
# M01AB Acetic acid derivatives and related substances
# M01AC Oxicams
# M01AE Propionic acid derivatives
# M01AG Fenamates
# M01AH Coxibs
# note did not include M01AX (other anti inflammatories)

nsaids_lists <- getATCCodes(
  cdm,
  level = c("ATC 4th"),
  name = c("Butylpyrazolidines",
           "Acetic acid derivatives and related substances",
           "Oxicams",
           "Propionic acid derivatives",
           "Fenamates",
           "Coxibs"),
  doseForm = NULL,
  doseUnit = NULL,
  routeCategory = NULL,
  type = "codelist_with_details"
)

# collapse the elements of the list
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
                "thiamine")

# remove the exclusions from the list of nsaid ingredients
nsaids_lists_ingredients <- nsaids_lists_ingredients %>% 
  filter(!(concept_name %in% exclusions))

# put the nsaids list into codelist generator and do more filtering of concepts
nsaids_lists1 <- getDrugIngredientCodes(
  cdm,
  name = nsaids_lists_ingredients$concept_name,
  nameStyle = "{concept_code}_{concept_name}",
  type = "codelist"
  
)

# remove ingredients with no record counts in database
nsaids_lists2 <- subsetToCodesInUse(nsaids_lists1, 
                                        minimumCount = 0,
                                        table = c("drug_exposure"),
                                        cdm = cdm)

# remove ingredients with <100 record counts in database
nsaids_lists2 <- subsetToCodesInUse(nsaids_lists2, 
                                          minimumCount = 100,
                                          table = c("drug_exposure"),
                                          cdm = cdm)

# instantiate the nsaids using drug utilisation package function
# all ingredients will be in one table but with unique cohort_definition ids

cdm <- generateDrugUtilisationCohortSet(
      cdm = cdm,
      name = "nsaids",
      conceptSet = nsaids_lists2 ,
      gapEra = 1
    )

# restrict to study period
cdm$nsaids %>% 
  CohortConstructor::requireInDateRange(dateRange = as.Date(c(starting_date, ending_date)))

# generate outcome cohorts AESI's ---------
# gi bleed - upper gi ulcer and generic gi hemorrhage
# MI (heart attack) 
# ischemic stroke
# haemorrhagic stroke 
# arrhythmia
# cardiomiopathy
# heart failure  
# deep vein thrombosis (DVT)
# pulmonary embolism
# heart failure

# instantiate outcome cohorts
cli::cli_alert_info("- Getting outcome definitions")
    
# get concept sets from cohorts----
aesi_codelists <- CodelistGenerator::codesFromCohort(
  path = here::here("1_InstantiateCohorts", "cohorts"),
  cdm = cdm
)

# use cohort constructor to create cohort
cdm$aesi_outcomes <- CohortConstructor::conceptCohort(
  cdm = cdm,
  conceptSet = aesi_codelists,
  name = "aesi_outcomes",
) %>% 
  CohortConstructor::requireInDateRange(dateRange = as.Date(c(starting_date, ending_date)))
    
cli::cli_alert_success("- Got outcome definitions")

    