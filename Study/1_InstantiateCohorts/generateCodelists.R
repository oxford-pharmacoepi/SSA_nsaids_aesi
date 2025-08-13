# Code showing how codelists were generating for this study

# ace inhibitors ----
ace_inh <- getATCCodes(
  cdm,
  level = c("ATC 3rd"),
  name = "ACE INHIBITORS, PLAIN",
  doseForm = NULL,
  doseUnit = NULL,
  routeCategory = NULL,
  type = "codelist"
)

# export ace inhibitor codelist
omopgenerics::exportCodelist(x = ace_inh, path = "1_InstantiateCohorts/Codelists", type = "csv")

# cough ------
#get code and descendents and create codelist
coughCodes <- omopgenerics::newCodelist(
  list(coughCodes = getDescendants(cdm, conceptId = 254761)$concept_id)
)

# export codelist
omopgenerics::exportCodelist(x = coughCodes, path = "1_InstantiateCohorts/Codelists", type = "csv")

# NSAIDS ----------
# use codelists generator to get the ingredient levels for all nsaids
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

# export nsaids codelists
omopgenerics::exportCodelist(x = nsaids_codelist1, path = "1_InstantiateCohorts/Codelists/NSAIDs", type = "csv")

# all nsaids ----

# # Create a combined regex pattern from the inclusions and "acetaminophen" and other nsaids from non M class
exclusions4allnsaids <- c("acetaminophen", inclusions)
pattern <- paste(exclusions4allnsaids, collapse = "|")  # create regex pattern

# Filter out matching names and build the codelist
all_nsaids <- omopgenerics::newCodelist(
  list(
    all_nsaids = unlist(
      nsaids_codelist2[!grepl(pattern, names(nsaids_codelist2), ignore.case = TRUE)],
      use.names = FALSE
    )
  )
)

# export the codelist
omopgenerics::exportCodelist(x = all_nsaids, path = "1_InstantiateCohorts/Codelists", type = "csv")

### hypertension ---
hyper_codelists <- CodelistGenerator::codesFromConceptSet(
  path = here::here("1_InstantiateCohorts", "Conditions", "hypertension.json"),
  cdm = cdm
)

omopgenerics::exportCodelist(x = hyper_codelists, path = "1_InstantiateCohorts/Codelists", type = "csv")

### Cox-2
cox_2_codelist <- bind(nsaids_codelist2["celecoxib"], nsaids_codelist2["etoricoxib"], 
                       nsaids_codelist2["lumiracoxib"],
                       nsaids_codelist2["rofecoxib"],
                       nsaids_codelist2["valdecoxib"])

cox_2_codelist <- unlist(cox_2_codelist)

names(cox_2_codelist) <- NULL

cox_2_codelist <- list("cox_2" = cox_2_codelist)

omopgenerics::exportCodelist(x = cox_2_codelist, path = "1_InstantiateCohorts/Codelists", type = "csv")

### non selective nsaids -----

non_selective_codelist <- within(nsaids_codelist2, rm("celecoxib", "etoricoxib", "lumiracoxib", "rofecoxib", "valdecoxib",
                                                      "acetaminophen", "aspirin", "dilfunisal"))

non_selective_codelist <- unlist(non_selective_codelist)

names(non_selective_codelist) <- NULL

non_selective_codelist <- list("non_selective" = non_selective_codelist)

omopgenerics::exportCodelist(x = non_selective_codelist, path = "1_InstantiateCohorts/Codelists", type = "csv")


# ppi ----
# A02BC Proton pump inhibitors
ppi <- getATCCodes(
  cdm,
  level = c("ATC 4th"),
  name = "Proton pump inhibitors",
  doseForm = NULL,
  doseUnit = NULL,
  routeCategory = NULL,
  type = "codelist"
)

# export ppi codelist
omopgenerics::exportCodelist(x = ppi, path = "1_InstantiateCohorts/Codelists", type = "csv")
