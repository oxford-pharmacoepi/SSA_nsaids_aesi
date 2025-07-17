# postive and negative controls ------

# this code runs the postive and negative controls for the study

########################
# database positive controls (we know has a signal)
########################
cli::cli_alert_info("- Generate SequenceCohortSet for positive controls")

cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "amiodarone_levothyroxine",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 indexTable = "amiodarone",
                                                 markerTable = "levothyroxine",
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365,
                                                 combinationWindow = c(0, 180)) # this is where the combination window that we look to see the order

amiodarone_levothyroxin <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_levothyroxine)


# ace - cough
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "ace_cough",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 indexTable = "ace_inhib",
                                                 markerTable = "cough",
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365,
                                                 combinationWindow = c(0, 180))

ace_cough <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$ace_cough)

cli::cli_alert_success("- Generated SequenceCohortSet for database positive controls")

##############################
# database negative controls (we know there is no signal)
##############################
cli::cli_alert_info("- Generate SequenceCohortSet for database negative controls")

#Amiodarone	Allopurinol
cdm <- CohortSymmetry::generateSequenceCohortSet(cdm = cdm,
                                                 name = "amiodarone_allopurinol",
                                                 cohortDateRange = c(starting_date, ending_date),
                                                 indexTable = "amiodarone",
                                                 markerTable = "allopurinol",
                                                 daysPriorObservation = 365,
                                                 washoutWindow = 365,
                                                 combinationWindow = c(0, 180))

amiodarone_allopurinol <- CohortSymmetry::summariseSequenceRatios(cohort = cdm$amiodarone_allopurinol)


cli::cli_alert_success("- Generated SequenceCohortSet for database negative controls")



# add positive and negative controls to one list
summary_seq_ratio <- list(amiodarone_levothyroxin,
                          amiodarone_allopurinol,
                          ace_cough
                          )

cli::cli_alert_info("- Bind summary sequence ratios")

# bind the list of sequence ratios
summary_seq_ratio_final <- omopgenerics::bind(summary_seq_ratio)

# write out summarised results object
omopgenerics::exportSummarisedResult(summary_seq_ratio_final,
                                     minCellCount = 5,
                                     path = here::here(symmetry_folder),
                                     fileName = paste0(cdmName(cdm),
                                                       "_results_ssa_estimates_controls.csv")
)



#null sequence ratio 
marker_settings_controls <- bind_rows(
  settings(cdm[["amiodarone_levothyroxine"]]),
  settings(cdm[["ace_cough"]]),
  settings(cdm[["amiodarone_allopurinol"]])
)
  

write_csv(marker_settings_controls, here::here(symmetry_folder, paste0("/", cdmName(cdm), "_ssa_marker_settings_controls.csv"
)))


#attrition for outcomes 
attrition_seq_ratio_controls <- list(attrition(cdm[["amiodarone_levothyroxine"]]) %>% 
                                               mutate(cohort_name = "amiodarone_levothyroxine"),   
                                     attrition(cdm[["ace_cough"]]) %>% 
                                                 mutate(cohort_name = "ace_cough"),  
                                     attrition(cdm[["amiodarone_allopurinol"]]) %>% 
                                                 mutate(cohort_name = "amiodarone_allopurinol")) %>% 
  bind_rows()


write_csv(attrition_seq_ratio_controls, here::here(symmetry_folder, paste0("/", cdmName(cdm), "_ssa_attrition_controls.csv"
)))


#temporal sequence plot
summary_temp_trends_months_controls <- 
  list(summariseTemporalSymmetry(cohort = cdm[["amiodarone_levothyroxine"]]
                                 , timescale = "month"),
       summariseTemporalSymmetry(cohort = cdm[["ace_cough"]]
                                 , timescale = "month"),
       summariseTemporalSymmetry(cohort = cdm[["amiodarone_allopurinol"]]
                                 , timescale = "month")
  ) %>% 
  bind()
       
       

write_csv(summary_temp_trends_months_controls, here::here(symmetry_folder, paste0("/", cdmName(cdm), "_ssa_temporal_symmetry_summary_controls.csv"
)))



# get the record trends for index and markers
record_trends_overall_pnc1 <- cdm[["amiodarone"]] %>%
  filter(cohort_start_date >= starting_date,
         cohort_start_date <= ending_date) %>%
  mutate(year = year(cohort_start_date)) %>%
  group_by(cohort_definition_id, year) %>%
  summarise(n_records = n(), .groups = "drop") %>%
  collect() %>%
  left_join(
    settings(cdm$amiodarone) %>%
      select(cohort_definition_id, cohort_name),
    by = "cohort_definition_id"
  )

record_trends_overall_pnc2 <- cdm[["levothyroxine"]] %>%
  filter(cohort_start_date >= starting_date,
         cohort_start_date <= ending_date) %>%
  mutate(year = year(cohort_start_date)) %>%
  group_by(cohort_definition_id, year) %>%
  summarise(n_records = n(), .groups = "drop") %>%
  collect() %>%
  left_join(
    settings(cdm$levothyroxine) %>%
      select(cohort_definition_id, cohort_name),
    by = "cohort_definition_id"
  )

record_trends_overall_pnc3 <- cdm[["allopurinol"]] %>%
  filter(cohort_start_date >= starting_date,
         cohort_start_date <= ending_date) %>%
  mutate(year = year(cohort_start_date)) %>%
  group_by(cohort_definition_id, year) %>%
  summarise(n_records = n(), .groups = "drop") %>%
  collect() %>%
  left_join(
    settings(cdm$allopurinol) %>%
      select(cohort_definition_id, cohort_name),
    by = "cohort_definition_id"
  )

record_trends_overall_pnc4 <- cdm[["cough"]] %>%
  filter(cohort_start_date >= starting_date,
         cohort_start_date <= ending_date) %>%
  mutate(year = year(cohort_start_date)) %>%
  group_by(cohort_definition_id, year) %>%
  summarise(n_records = n(), .groups = "drop") %>%
  collect() %>%
  left_join(
    settings(cdm$cough) %>%
      select(cohort_definition_id, cohort_name),
    by = "cohort_definition_id"
  )

record_trends_overall_pnc5 <- cdm[["ace_inhib"]] %>%
  filter(cohort_start_date >= starting_date,
         cohort_start_date <= ending_date) %>%
  mutate(year = year(cohort_start_date)) %>%
  group_by(cohort_definition_id, year) %>%
  summarise(n_records = n(), .groups = "drop") %>%
  collect() %>%
  left_join(
    settings(cdm$ace_inhib) %>%
      select(cohort_definition_id, cohort_name),
    by = "cohort_definition_id"
  )

record_trends_overall_controls <- bind_rows(
  record_trends_overall_pnc1,
  record_trends_overall_pnc2,
  record_trends_overall_pnc3,
  record_trends_overall_pnc4,
  record_trends_overall_pnc5) %>% 
  mutate(cdm_name = db_name)


# write out the results
write_csv(record_trends_overall_controls, 
          here::here(symmetry_folder, paste0("/", cdmName(cdm), "_record_trend_controls.csv"
          )))
