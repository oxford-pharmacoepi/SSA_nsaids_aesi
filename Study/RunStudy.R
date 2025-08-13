# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
# use this format: output_folder <- here("Results", db_name, "_new folder name")
output_folder <- here("Results", db_name, Sys.Date())

# output files ----
if (!file.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# Create subfolders for each analysis
sex_strat_folder <- file.path(output_folder, "sex_stratification")
age_strat_folder <- file.path(output_folder, "age_stratification")
htn_strat_folder <- file.path(output_folder, "hypertension_stratification")
sensitivity_folder <- file.path(output_folder, "sensitivity_365")
sensitivity_as_folder <- file.path(output_folder, "sensitivity_age_sex")
characterisation_folder <- file.path(output_folder, "characterisation")
symmetry_folder <- file.path(output_folder, "symmetry")
#anticoag_strat_folder <- file.path(output_folder, "anticoagulants_stratification")

folders <- list(sex_strat_folder, 
                age_strat_folder, 
                htn_strat_folder, 
                sensitivity_folder, 
                characterisation_folder, 
                symmetry_folder)
lapply(folders, function(f) {
  if (!dir.exists(f)) dir.create(f, recursive = TRUE)
})

# log file
logger_name <- gsub(":| |-", "_", paste0("log_01_001_", Sys.time(), ".txt"))
logger <- create.logger()
logfile(logger) <- here::here("Results", db_name, Sys.Date(), logger_name)
level(logger) <- "INFO"
info(logger, "LOG CREATED")

# add start and end dates for index and marker drugs -----
starting_date <- as.Date("2013-01-01")
ending_date <- as.Date("2023-01-01")

# get cdm snapshot
info(logger, "RETRIEVING SNAPSHOT")

OmopSketch::exportSummarisedResult(
  OmopSketch::summariseOmopSnapshot(cdm),
  fileName = here(output_folder, paste0("/", db_name, "_cdm_snapshot_.csv")),
  path = output_folder
)
info(logger, "SNAPSHOT COMPLETED")

info(logger, "INSTANTIATING COHORTS")
if(isFALSE(instantiated)){
  
source(here("1_InstantiateCohorts", "InstantiateCohorts.R"))
  
}

if(isTRUE(instantiated)){
cdm <- CDMConnector::cdmFromCon(con = db,
                                cdmSchema = cdm_database_schema,
                                writeSchema = results_database_schema,
                                cohortTables = c( "nsaids", 
                                                  "aesi", 
                                                  "medications", 
                                                  "conditions", 
                                                  "all_nsaids", 
                                                  "cox_2", 
                                                  "non_selective", 
                                                  "nsaids_sa", 
                                                  "amiodarone", 
                                                  "levothyroxine", 
                                                  "allopurinol", 
                                                  "ace_inh", 
                                                  "cough", 
                                                  "asthma", 
                                                  "edema",
                                                  "hypertension",
                                                  "cataracts", 
                                                  "nausea", 
                                                  "vomiting",
                                                  "anemia", 
                                                  "aki"),
                                
                                writePrefix = table_stem,
                                achillesSchema = results_database_schema,
                                cdmName = db_name)
}

# run main analysis ------------
if(isTRUE(run_symmetry)){
  cli::cli_text("- Running cohort symmetry for main analysis ({Sys.time()})")
  
  info(logger, "RUNNING COHORT SYMMETRY MAIN ANALYSIS")
  
  tryCatch({
    source(here("2_Analysis", "cohortsymmetry.R"))
  }, error = function(e) {
    writeLines(as.character(e),
               here(output_folder, paste0("/", db_name,
                                          
                                          "_error_cohortsymmetry.txt")))
  })
  
  info(logger, "RAN COHORT SYMMETRY MAIN ANALYSIS")
}


# database and study postive and negative controls ----------
if (isTRUE(run_symmetry)) {
  cli::cli_text("- Running cohort symmetry for controls ({Sys.time()})")

  info(logger, "RUNNING COHORT SYMMETRY FOR CONTROLS")

  tryCatch(
    {
      source(here("2_Analysis", "controls.R"))
    },
    error = function(e) {
      writeLines(
        as.character(e),
        here(output_folder, paste0(
          "/", db_name,
          "_error_controls.txt"
        ))
      )
    }
  )
  info(logger, "RAN COHORT SYMMETRY FOR CONTROLS")
}


# characterisation analysis -----

if (isTRUE(run_characterisation)) {
  cli::cli_text("- Running characterisation ({Sys.time()})")

  info(logger, "RUNNING CHARACTERISATION")

  tryCatch(
    {
      source(here("2_Analysis", "characterisation.R"))
    },
    error = function(e) {
      writeLines(
        as.character(e),
        here(output_folder, paste0(
          "/", db_name,
          "_error_characterisation.txt"
        ))
      )
    }
  )
  info(logger, "RAN CHARACTERISATION")
}


# sex stratification analysis
if (isTRUE(run_sex_stratification)) {
  cli::cli_text("- Running sex stratification ({Sys.time()})")

  info(logger, "RUNNING SEX STRATIFICATION")

  tryCatch(
    {
      source(here("2_Analysis", "SexStratification.R"))
    },
    error = function(e) {
      writeLines(
        as.character(e),
        here(output_folder, paste0(
          "/", db_name,
          "_error_sex_stratification.txt"
        ))
      )
    }
  )
  info(logger, "RAN SEX STRATIFICATION")
}


# age stratification analysis
if (isTRUE(run_age_stratification)) {
  cli::cli_text("- Running age stratification ({Sys.time()})")

  info(logger, "RUNNING AGE STRATIFICATION")

  tryCatch(
    {
      source(here("2_Analysis", "AgeStratification.R"))
    },
    error = function(e) {
      writeLines(
        as.character(e),
        here(output_folder, paste0(
          "/", db_name,
          "_error_age_stratification.txt"
        ))
      )
    }
  )
  info(logger, "RAN AGE STRATIFICATION")
}

# hypertension stratification analysis
if (isTRUE(run_hypertension_stratification)) {
  cli::cli_text("- Running hypertension stratification ({Sys.time()})")

  info(logger, "RUNNING HYPERTENSION STRATIFICATION")

  tryCatch(
    {
      source(here("2_Analysis", "HypertensionStratification.R"))
    },
    error = function(e) {
      writeLines(
        as.character(e),
        here(output_folder, paste0(
          "/", db_name,
          "_error_hypertension_stratification.txt"
        ))
      )
    }
  )
  info(logger, "RAN HYPERTENSION STRATIFICATION")
}

## anticoagulants stratification analysis
# if (isTRUE(run_anticoagulants_stratification)) {
#   cli::cli_text("- Running anticoagulants stratification ({Sys.time()})")
#   
#   info(logger, "RUNNING ANTICOAGULANTS STRATIFICATION")
#   
#   tryCatch(
#     {
#       source(here("2_Analysis", "AnticoagulantsStratification.R"))
#     },
#     error = function(e) {
#       writeLines(
#         as.character(e),
#         here(output_folder, paste0(
#           "/", db_name,
#           "_error_anticoagulants_stratification.txt"
#         ))
#       )
#     }
#   )
#   info(logger, "RAN ANTICOAGULANTS STRATIFICATION")
# }

# sensitivity analysis of 365 day combination window using unstratified population
if (isTRUE(run_sensitivity_365)) {
  cli::cli_text("- Running sensitivity 365 ({Sys.time()})")

  info(logger, "RUNNING SENSITIVITY 365")

  tryCatch(
    {
      source(here("2_Analysis", "sensitivity.R"))
    },
    error = function(e) {
      writeLines(
        as.character(e),
        here(output_folder, paste0(
          "/", db_name,
          "_error_sensitivity365.txt"
        ))
      )
    }
  )

  info(logger, "RAN SENSITIVITY 365")
}

if (isTRUE(run_sensitivity_age_sex)) {
  info(logger, "SENSITIVITY AGE + SEX")
  tryCatch(
    {
      source(here("2_Analysis", "sensitivityAgeSex.R"))
    },
    error = function(e) {
      writeLines(
        as.character(e),
        here(output_folder, paste0(
          "/", db_name,
          "_error_sensitivity_age_sex.txt"
        ))
      )
    }
  )
}


if (isTRUE(run_ppi_stratification)) {
  info(logger, "PPI STRATIFICATION")
  tryCatch(
    {
      source(here("2_Analysis", "ppiStratification.R"))
    },
    error = function(e) {
      writeLines(
        as.character(e),
        here(output_folder, paste0(
          "/", db_name,
          "_error_sensitivity_ppi.txt"
        ))
      )
    }
  )
}


# zip results ----
cli::cli_text("- Zipping Results ({Sys.time()})")
info(logger, "ZIPPING RESULTS")

# zip all results
zip::zip(
  zipfile = file.path(here(
    output_folder,
    paste0("Results", db_name, ".zip")
  )),
  files = list.files(here(output_folder)),
  root = output_folder
)

cli::cli_alert_success("- Study Done!")
cli::cli_alert_success("- If all has worked, there should now be a zip folder with your results in the Results folder to share")
cli::cli_alert_success("- Thank you for running the study! :)")

info(logger, "ZIPPED RESULTS")
