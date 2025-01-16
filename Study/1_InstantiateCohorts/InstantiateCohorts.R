# positive controls -------
#log("- Getting benchmarker definitions drug - drug positive controls")

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
#log("- Getting benchmarker definitions drug - drug negative controls")

cdm <- DrugUtilisation::generateIngredientCohortSet(
  cdm = cdm,
  name = "allopurinol",
  ingredient = "allopurinol",
  gapEra = 30
)

cli::cli_alert_success("- Got benchmarker definitions drug - drug negative controls")

cli::cli_alert_success("- Getting nsaids")


# use codelists generator to get the ingredient levels for all nsaids -----
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
                "acetaminophen",
                "magnesium",
                "thiamine",
                "pyridoxine")

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
  nameStyle = "{concept_code}_{concept_name}",
  type = "codelist"
  
)

# remove ingredients with no record counts in database
nsaids_codelist2 <- subsetToCodesInUse(nsaids_codelist1, 
                                        minimumCount = 0,
                                        table = c("drug_exposure"),
                                        cdm = cdm)

# remove ingredients with <500 record counts in database
nsaids_codelist2 <- subsetToCodesInUse(nsaids_codelist2, 
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

    