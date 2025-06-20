# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
output_folder <- here("Results", db_name)

# output files ---- 
if (!file.exists(output_folder)){
  dir.create(output_folder, recursive = TRUE)}

# add start and end dates for index and marker drugs -----
starting_date <- as.Date("2013-01-01")
ending_date <- as.Date("2023-01-01")

# run analysis
run_symmetry <- TRUE
run_characterisation <- TRUE
run_sex_stratification <- TRUE
run_age_stratification <- TRUE
run_sensitivity_365 <- TRUE

# get cdm snapshot
OmopSketch::exportSummarisedResult(
  OmopSketch::summariseOmopSnapshot(cdm),
  fileName = here(output_folder, paste0("/", db_name, "_cdm_snapshot_.csv")),
  path = output_folder
)

# this is useful if you have already created the cohorts you can get them back from the database results schema

source(here("1_InstantiateCohorts","InstantiateCohorts.R"))


# run main analysis ------------
if(isTRUE(run_symmetry)){
  
  tryCatch({
    source(here("2_Analysis", "cohortsymmetry.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here(output_folder, paste0("/", db_name,
                                          
                                          "_error_cohortsymmetry.txt")))
  })
}


# characterisation analysis -----
if(isTRUE(run_characterisation)){
  #log("- Running Characterisation")
  tryCatch({
    source(here("2_Analysis", "characterisation.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here(output_folder, paste0("/", db_name,
                                          
                                          "_error_characterisation.txt")))
  })
}


# sex stratification analysis
if(isTRUE(run_sex_stratification)){
  #log("- Running Sex Stratification")
  tryCatch({
    source(here("2_Analysis", "SexStratification.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here(output_folder, paste0("/", db_name,
                                          
                                          "_error_sex_stratification.txt")))
  })
}


# age stratification analysis
if(isTRUE(run_age_stratification)){
  #log("- Running Age Stratification")
  tryCatch({
    source(here("2_Analysis", "AgeStratification.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here(output_folder, paste0("/", db_name,
                                          
                                          "_error_age_stratification.txt")))
  })
}

# sensivity analysis of 365 day combination window using unstratified population
if(isTRUE(run_sensitivity_365)){
  #log("- Running Sensitivity 365")
  tryCatch({
    source(here("2_Analysis", "sensitivity.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here(output_folder, paste0("/", db_name,
                                          
                                          "_error_sensitivity365.txt")))
  })
}


# zip results ----
#log("- Zipping Results")
# zip all results
zip::zip(
  zipfile = file.path(here(output_folder,
                           paste0("Results_new", db_name, ".zip"))),
  files = list.files(here(output_folder)),
  root = output_folder)

cli::cli_alert_success("- Study Done!")
cli::cli_alert_success("- If all has worked, there should now be a zip folder with your results in the Results folder to share")
cli::cli_alert_success("- Thank you for running the study! :)")
