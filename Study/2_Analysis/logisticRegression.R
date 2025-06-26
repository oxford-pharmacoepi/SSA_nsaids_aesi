# logistic regression

# SEX
# take the output CohortSymmetry::generateSequenceCohortSet and add a binary value for index marker order
cdm$nsaids_aesi_lr <- cdm$nsaids_aesi %>% 
  mutate(order = if_else(index_date < marker_date, "Index first", "Marker first")) %>% 
  mutate(order_binary = if_else(index_date < marker_date, 1, 0)) %>% 
  addSex() %>% 
  compute(name = "nsaids_aesi_lr", temporary = FALSE, overwrite = TRUE)

# set up logistic regression model

# Get distinct cohort_definition_ids
cohort_ids <- cdm$nsaids_aesi_lr %>%
  select(cohort_definition_id) %>%
  distinct() %>%
  collect() %>%
  pull(cohort_definition_id)

# Run logistic regression model per cohort_definition_id ie nsaid/cvd outcome
  lr_results_sex <- map_dfr(cohort_ids, function(cid) {
    # Safely collect just that cohort's data
    data_subset <- cdm$nsaids_aesi_lr %>%
      filter(cohort_definition_id == cid) %>%
      select(order_binary, sex) %>%
      collect()
    
    # Check if model is valid
    if (nrow(data_subset) > 1 && length(unique(data_subset$sex)) > 1) {
      model <- glm(order_binary ~ sex, data = data_subset, family = binomial)
      broom::tidy(model) %>%
        mutate(cohort_definition_id = cid)
    } else {
      # Return placeholder with cohort_definition_id and NA model results
      tibble(
        term = NA_character_,
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        cohort_definition_id = cid
      )
    }
  })
  
  # join back the cohort_name 
  lr_results_sex <-   lr_results_sex %>%
    left_join(
      settings(cdm$nsaids_aesi) %>%
        select(cohort_definition_id, cohort_name),
      by = "cohort_definition_id"
    )

  # write out the results
  write_csv(lr_results_sex, 
            here::here("Results", paste0(db_name, "/", cdmName(cdm), "_logistic_regression_sex.csv"
            )))
  