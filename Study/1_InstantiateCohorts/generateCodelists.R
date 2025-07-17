ace_inhib <- getATCCodes(
  cdm,
  level = c("ATC 3rd"),
  name = "ACE INHIBITORS, PLAIN",
  doseForm = NULL,
  doseUnit = NULL,
  routeCategory = NULL,
  type = "codelist"
)

cough_codes <- getDescendants(cdm, conceptId = c(254761))

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
  type = "codelist")
  
omopgenerics::exportCodelist(x = nsaids_codelist1, path = "1_InstantiateCohorts/Codelists/NSAIDs", type = "csv")
omopgenerics::exportCodelist(x = ace_inhib, path = "1_InstantiateCohorts/Codelists", type = "csv")
omopgenerics::exportConceptSetExpression(x = cough_codelist, path = "1_InstantiateCohorts/Codelists", type = "csv")

write.csv(cough_codes, "1_InstantiateCohorts/Codelists/coughCodes.csv")

### Cox-2



# Sensitivity Analysis

cdm$nsaid_union <- CohortConstructor::unionCohorts(
  cohort = cdm$nsaids,
  name = "nsaid_union",
  cohortName = "all_nsaids",
  keepOriginalCohorts = FALSE
)

all_nsaids_codelist <- cohortCodelist(cdm$nsaid_union, cohortId = 1)

all_nsaids_codelist <- unlist(all_nsaids_codelist)

# 2. Convert the combined vector into a list containing only that vector
names(all_nsaids_codelist) <- NULL

# 3. Convert the combined vector into a list containing only that vector
all_nsaids <- list("all_nsaids" = all_nsaids_codelist)

omopgenerics::exportCodelist(x = all_nsaids, path = "1_InstantiateCohorts/Codelists", type = "csv")

###

hyper_codelists <- CodelistGenerator::codesFromConceptSet(
  path = here::here("1_InstantiateCohorts", "Conditions", "hypertension.json"),
  cdm = cdm
)

omopgenerics::exportCodelist(x = hyper_codelists, path = "1_InstantiateCohorts/Codelists", type = "csv")

###
cox_2_codelist <- bind(nsaids_codelist2["celecoxib"], nsaids_codelist2["etoricoxib"], 
                       nsaids_codelist2["lumiracoxib"],
                       nsaids_codelist2["rofecoxib"],
                       nsaids_codelist2["valdecoxib"])

cox_2_codelist <- unlist(cox_2_codelist)

# 2. Convert the combined vector into a list containing only that vector
names(cox_2_codelist) <- NULL

# 3. Convert the combined vector into a list containing only that vector
cox_2_codelist <- list("cox_2" = cox_2_codelist)

omopgenerics::exportCodelist(x = cox_2_codelist, path = "1_InstantiateCohorts/Codelists", type = "csv")

###

non_selective_codelist <- within(nsaids_codelist2, rm("celecoxib", "etoricoxib", "lumiracoxib", "rofecoxib", "valdecoxib"))

non_selective_codelist <- unlist(non_selective_codelist)

# 2. Convert the combined vector into a list containing only that vector
names(non_selective_codelist) <- NULL

# 3. Convert the combined vector into a list containing only that vector
non_selective_codelist <- list("non_selective" = non_selective_codelist)

omopgenerics::exportCodelist(x = non_selective_codelist, path = "1_InstantiateCohorts/Codelists", type = "csv")

###

cox_2_pref_codelist <- bind(nsaids_codelist2["aceclofenac"], nsaids_codelist2["diclofenac"], nsaids_codelist2["etodolac"], nsaids_codelist2["meloxicam"], 
                            nsaids_codelist2["nabumetone"],nsaids_codelist2["sulindac"])

cox_2_pref_codelist <- unlist(cox_2_pref_codelist)

# 2. Convert the combined vector into a list containing only that vector
names(cox_2_pref_codelist) <- NULL

# 3. Convert the combined vector into a list containing only that vector
cox_2_pref_codelist <- list("cox_2_pref" = cox_2_pref_codelist)

omopgenerics::exportCodelist(x = cox_2_pref_codelist, path = "1_InstantiateCohorts/Codelists", type = "csv")

###

cox_1_pref_codelist <- bind(nsaids_codelist2["aspirin"], nsaids_codelist2["flurbiprofen"], nsaids_codelist2["indomethacin"], nsaids_codelist2["ketoprofen"], 
                            nsaids_codelist2["naproxen"],nsaids_codelist2["piroxicam"])

cox_1_pref_codelist <- unlist(cox_1_pref_codelist)

names(cox_1_pref_codelist) <- NULL

cox_1_pref_codelist <- list("cox_1_pref" = cox_1_pref_codelist)

omopgenerics::exportCodelist(x = cox_1_pref_codelist, path = "1_InstantiateCohorts/Codelists", type = "csv")

