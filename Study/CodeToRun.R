# Manage project dependencies ------
# the following will prompt you to install the various packages used in the study 
# install.packages("renv")
# renv::activate()
renv::restore()

# PACKAGES
library(CDMConnector)
library(DBI)
library(log4r)
library(dplyr)
library(here)
library(zip)
library(OmopSketch)
library(CodelistGenerator)
library(CohortSymmetry)
library(DrugUtilisation)
library(stringr)
library(CohortConstructor)
library(ggplot2)
library(CohortCharacteristics)
library(readr)
library(PatientProfiles)

# database metadata and connection details
# The name/ acronym for the database
db_name <-"..."

# database connection details
server_dbi <- "..."
user       <- "..."
password   <- "..."
port       <- "..."
host       <- "..."


# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below
# https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html 
# for more details.
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
db_driver <- "..."

# create the connection
db <- DBI::dbConnect(db_driver,
                     dbname = server_dbi,
                     port = port,
                     host = host, 
                     user = user, 
                     password = password)


# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "..."

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema <- "..."

# The name of the schema where results tables will be created 
results_database_schema <- "..."

# stem table description use something short and informative
# Note, if there is an existing table in your results schema with the same names it will be overwritten 
# needs to be in lower case and NOT more than 10 characters
table_stem <-"..."

# create cdm reference ---- DO NOT REMOVE "PREFIX" ARGUMENT IN THIS CODE
cdm <- CDMConnector::cdmFromCon(con = db, 
                                  cdmSchema = cdm_database_schema,
                                  writeSchema = results_database_schema, 
                                  writePrefix = table_stem,
                                  achillesSchema = results_database_schema,
                                  cdmName = db_name)

# to check whether the DBI connection is correct, 
# running the next line should give you a count of your person table
cdm$person %>% 
  dplyr::tally() %>% 
  dplyr::compute()


# Run the study ------
source(here("RunStudy.R"))

# after the study is run you should have a zip folder in your output folder to share

# Disconnect from the database
#DBI::dbDisconnect(db)
