# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
# use this format: output_folder <- here("Results", db_name, "_new folder name")
output_folder <- here("Results", db_name, "_11th_jul_full")

# Create subfolders for each analysis
sex_strat_folder <- file.path(output_folder, "sex_stratification")
age_strat_folder <- file.path(output_folder, "age_stratification")
htn_strat_folder <- file.path(output_folder, "hypertension_stratification")
sensitivity_folder <- file.path(output_folder, "sensitivity_365")
characterisation_folder <- file.path(output_folder, "characterisation")
symmetry_folder <- file.path(output_folder, "symmetry")

folders <- list(sex_strat_folder, age_strat_folder, htn_strat_folder, sensitivity_folder, characterisation_folder, symmetry_folder)
lapply(folders, function(f) {
  if (!dir.exists(f)) dir.create(f, recursive = TRUE)
})

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
run_hypertension_stratification <- TRUE
run_sensitivity_365 <- TRUE

# get cdm snapshot
OmopSketch::exportSummarisedResult(
  OmopSketch::summariseOmopSnapshot(cdm),
  fileName = here(output_folder, paste0("/", db_name, "_cdm_snapshot_.csv")),
  path = output_folder
)

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


# database and study postive and negative controls ----------
if(isTRUE(run_symmetry)){
  
  tryCatch({
    source(here("2_Analysis", "controls.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here(output_folder, paste0("/", db_name,
                                          
                                          "_error_controls.txt")))
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

# hypertension stratification analysis
if(isTRUE(run_hypertension_stratification)){
  #log("- Running Hypertension Stratification")
  tryCatch({
    source(here("2_Analysis", "HypertensionStratification.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here(output_folder, paste0("/", db_name,
                                          
                                          "_error_hypertension_stratification.txt")))
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
                           paste0("Results", db_name, ".zip"))),
  files = list.files(here(output_folder)),
  root = output_folder)

cli::cli_alert_success("- Study Done!")
cli::cli_alert_success("- If all has worked, there should now be a zip folder with your results in the Results folder to share")
cli::cli_alert_success("- Thank you for running the study! :)")
