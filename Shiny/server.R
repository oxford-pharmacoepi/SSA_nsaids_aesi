#### SERVER ------
server <-	function(input, output, session) {
  
  # cdm snapshot------
  output$tbl_cdm_snaphot <- renderText(kable(snapshotcdm) %>%
                                         kable_styling("striped", full_width = F) )
  
  
  output$gt_cdm_snaphot_word <- downloadHandler(
    filename = function() {
      "cdm_snapshot.docx"
    },
    content = function(file) {
      x <- gt(snapshotcdm)
      gtsave(x, file)
    }
  )
  
  
  
  # overall ssa -----
  get_ssa_estimates <-reactive({
    
    table <- ssa_estimates %>% 
      select(-c(
        asr   ,            
        asr_lower    ,
        asr_upper 
        
      )) %>% 
      filter(signal %in% input$overall_ssa_signal_selector) %>% 
      filter(`Index cohort name` %in% input$overall_ssa_index_name_selector) %>% 
      filter(`Marker cohort name` %in% input$overall_ssa_marker_name_selector)
    
    table
    
  }) 
  
  output$tbl_overall_ssa <- renderText(kable(get_ssa_estimates()) %>%
                                           kable_styling("striped", full_width = F) )
  
  
  
  output$gt_overall_ssa_word <- downloadHandler(
    filename = function() {
      "overall_ssa_estimates.docx"
    },
    content = function(file) {
      x <- gt(get_ssa_estimates())
      gtsave(x, file)
    }
  )
  
  ## controls
  get_ssa_estimates_pnc <-reactive({

    table_pnc <- ssa_estimates_controls %>%
      select(-c(
        asr   ,
        asr_lower    ,
        asr_upper

      )) %>%
      filter(signal %in% input$overall_ssa_signal_selector_pnc) %>%
      filter(`Index cohort name` %in% input$overall_ssa_index_name_selector_pnc) %>%
      filter(`Marker cohort name` %in% input$overall_ssa_marker_name_selector_pnc)

    table_pnc

  })

  output$tbl_overall_ssa_pnc <- renderText(kable(get_ssa_estimates_pnc()) %>%
                                         kable_styling("striped", full_width = F) )



  output$gt_overall_ssa_word_pnc <- downloadHandler(
    filename = function() {
      "overall_ssa_estimates_pnc.docx"
    },
    content = function(file) {
      x <- gt(get_ssa_estimates_pnc())
      gtsave(x, file)
    }
  )

  # forest plot OVERALL
  get_overall_forest_plot <-reactive({
    
    
    get_data <- ssa_estimates |>
      filter(`Index cohort name` %in% input$forest_plot_index_selector) %>%
      filter(`Marker cohort name` %in% input$forest_plot_marker_selector) %>%
      dplyr::mutate(
        `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
      ) %>% 
      filter(asr != Inf) %>% 
      mutate(highlight = ifelse(asr_lower > 1, "Highlighted", "Not Highlighted")) %>% 
      rename(index_cohort_name = `Index cohort name`,
             marker_cohort_name = `Marker cohort name`)  %>%  
      mutate(
        index_cohort_name = stringr::str_to_title(index_cohort_name),
        marker_cohort_name = dplyr::case_when(
          marker_cohort_name == "pe" ~ "Pulmonary Embolism",
          marker_cohort_name == "gi_hemorrhage" ~ "GI Hemorrhage",
          marker_cohort_name == "heart_failure" ~ "Heart Failure",
          marker_cohort_name == "dvt" ~ "Deep Vein Thrombosis",
          marker_cohort_name == "stroke_broad" ~ "Stroke",
          marker_cohort_name == "isbroad" ~ "Ischemic Stroke",
          marker_cohort_name == "acute_mi" ~ "Myocardial Infarction",
          marker_cohort_name == "hem_stroke" ~ "Hemorrhagic Stroke",
          
          TRUE ~ stringr::str_to_title(marker_cohort_name)),
          index_cohort_name = dplyr::case_when(
            index_cohort_name == "All_nsaids" ~ "Any NSAIDs (excl. Aspirin)",
            index_cohort_name == "Non_selective" ~ "Non-selective",
            index_cohort_name == "Cox_2" ~ "Cox-2",
            TRUE ~ index_cohort_name
          )
        ) |>
      mutate(index_cohort_name = factor(index_cohort_name, 
                                         levels = rev(c("Any NSAIDs (excl. Aspirin)", "Cox-2", "Celecoxib", "Etoricoxib", "Non-selective", "Aspirin", 
                                                    "Diclofenac", "Etodolac", "Ibuprofen", "Indomethacin", "Mefenamate", "Meloxicam", "Naproxen",
                                                    "Piroxicam", "Acetaminophen")),
                                         ordered = TRUE),
             signal = factor(signal, levels = c("Positive", "Negative", "Null"), ordered = TRUE))
    
    
    labs = c("Adjusted Sequence Ratio", "NSAID")
    custom_colors <- c("adjusted" = "black")
    
    plot_data <- ggplot(get_data, aes(
      x = index_cohort_name,
      y = asr,
      ymin = asr_lower,
      ymax = asr_upper,
      color = signal
    )) +
      geom_hline(yintercept = 1, linetype = 2) +
      # Draw error bars with thicker lines
      geom_errorbar(
        aes(ymin = asr_lower, ymax = asr_upper),
        position = position_dodge(width = 0.8),
        width = 0,
        size = 1  # This controls the thickness of the error bar line
      ) +
      # Add points separately
      geom_point(
        position = position_dodge(width = 0.8),
        size = 3.5  # Controls the size of the point
      ) +
      facet_wrap(~ marker_cohort_name) +
      coord_flip() +
      theme_bw() +
      labs(
        x = "NSAID",
        y = "Adjusted Sequence Ratio"
      ) +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold", size = 16),
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 16)
      ) +
      scale_colour_manual(values = c("Positive" = "#1f77b4", "Null" = "#d62728", "Negative" = "#7b48c1"))

    plot_data

  })

  output$forestPlot_overall <- renderPlot(
    get_overall_forest_plot()
  )
  
  #forest plot controls
  get_overall_forest_plot_pnc <-reactive({

    get_data_pnc <- ssa_estimates_controls |>
      dplyr::mutate(
        `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
      ) %>%
      filter(asr != Inf) %>%
      mutate(highlight = ifelse(asr_lower > 1, "Highlighted", "Not Highlighted")) %>%
      rename(index_cohort_name = `Index cohort name`,
             marker_cohort_name = `Marker cohort name`)  %>%
      mutate(
        index_cohort_name = stringr::str_to_title(index_cohort_name),
        marker_cohort_name = dplyr::case_when(
          marker_cohort_name == "aki" ~ "Acute Kidney Injury",
          marker_cohort_name == "cough_codes" ~ "Cough",
          TRUE ~ stringr::str_to_title(marker_cohort_name)),
        index_cohort_name = dplyr::case_when(
          index_cohort_name == "All_nsaids" ~ "Any NSAIDs (excl. Aspirin)",
          index_cohort_name == "Ace_inhibitors_plain" ~ "ACE Inhibitors",
          TRUE ~ stringr::str_to_title(index_cohort_name)
        )
      ) |>
      filter(`index_cohort_name` %in% input$forest_plot_index_selector_pnc) %>%
      filter(`marker_cohort_name` %in% input$forest_plot_marker_selector_pnc)


    labs = c("Adjusted Sequence Ratio", "NSAID")
    custom_colors <- c("adjusted" = "black")

    plot_data_pnc <- ggplot(get_data_pnc, aes(
      x = marker_cohort_name,
      y = asr,
      ymin = asr_lower,
      ymax = asr_upper,
      color = signal
    )) +
      geom_hline(yintercept = 1, linetype = 2) +
      # Draw error bars with thicker lines
      geom_errorbar(
        aes(ymin = asr_lower, ymax = asr_upper),
        position = position_dodge(width = 0.8),
        width = 0,
        size = 1  # This controls the thickness of the error bar line
      ) +
      # Add points separately
      geom_point(
        position = position_dodge(width = 0.8),
        size = 3.5  # Controls the size of the point
      ) +
      facet_wrap(~ index_cohort_name, scales = "free") +
      coord_flip() +
      theme_bw() +
      labs(
        x = "Index",
        y = "Adjusted Sequence Ratio"
      ) +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold", size = 16),
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 16)
      )

    plot_data_pnc

  })

  output$forestPlot_overall_pnc <- renderPlot(
    get_overall_forest_plot_pnc()
  )
  
  # forest plot SEX
  get_sex_forest_plot <-reactive({
    
    
    get_data <- ssa_estimates_sex |>
      dplyr::mutate(
         `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
         `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
      ) |>
      dplyr::mutate(
        # Extract sex BEFORE cleaning names
        sex = dplyr::case_when(
          stringr::str_detect(`Index cohort name`, "_female$") ~ "Female",
          stringr::str_detect(`Index cohort name`, "_male$") ~ "Male",
          TRUE ~ "Unspecified"
        ) ) %>% 
      
      dplyr::mutate(
        # Clean cohort names AFTER extracting sex
        `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        `Index cohort name` = stringr::str_remove(`Index cohort name`, "_(female|male)$"),
        `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        pair = paste0(`Index cohort name`, "->", `Marker cohort name`
        )) %>% 
      
      filter(asr != Inf) %>% 
      rename(index_cohort_name = `Index cohort name`,
             marker_cohort_name = `Marker cohort name`)  %>%  
      mutate(
        index_cohort_name = stringr::str_to_title(index_cohort_name),
        marker_cohort_name = dplyr::case_when(
          marker_cohort_name == "pe" ~ "Pulmonary Embolism",
          marker_cohort_name == "gi_hemorrhage" ~ "GI Hemorrhage",
          marker_cohort_name == "heart_failure" ~ "Heart Failure",
          marker_cohort_name == "dvt" ~ "Deep Vein Thrombosis",
          marker_cohort_name == "stroke_broad" ~ "Stroke",
          marker_cohort_name == "isbroad" ~ "Ischemic Stroke",
          marker_cohort_name == "acute_mi" ~ "Myocardial Infarction",
          marker_cohort_name == "hem_stroke" ~ "Hemorrhagic Stroke",
          
          TRUE ~ stringr::str_to_title(marker_cohort_name)  # Default capitalization
        ),
        index_cohort_name = dplyr::case_when(
          index_cohort_name == "All_nsaids" ~ "Any NSAIDs (excl. Aspirin)",
          index_cohort_name == "Non_selective" ~ "Non-selective",
          index_cohort_name == "Cox_2" ~ "COX-2",
          TRUE ~ index_cohort_name
        ),
        signal = dplyr::case_when(
          signal == "Null" ~ "Negative / Null",
          signal == "Negative" ~ "Negative / Null",
          TRUE ~ signal
        )
      ) |>
      mutate(index_cohort_name = factor(index_cohort_name, 
                                        levels = rev(c("Any NSAIDs (excl. Aspirin)", "COX-2", "Celecoxib", "Etoricoxib", "Non-selective", "Aspirin", 
                                                       "Diclofenac", "Etodolac", "Ibuprofen", "Indomethacin", "Mefenamate", "Meloxicam", "Naproxen",
                                                       "Piroxicam", "Acetaminophen")),
                                        ordered = TRUE))
    
    p_sex_comparison <- ggplot(get_data, aes(
      x = index_cohort_name,
      y = asr,
      ymin = asr_lower,
      ymax = asr_upper,
      shape = sex,
      color = signal
    )) +
      geom_hline(yintercept = 1, linetype = 2) +
      # Draw error bars with thicker lines
      geom_errorbar(
        aes(ymin = asr_lower, ymax = asr_upper),
        position = position_dodge(width = 0.8),
        width = 0,
        size = 1  # This controls the thickness of the error bar line
      ) +
      # Add points separately
      geom_point(
        position = position_dodge(width = 0.8),
        size = 3.5  # Controls the size of the point
      ) +
      facet_wrap(~ marker_cohort_name) +
      coord_flip() +
      theme_bw() +
      labs(
        x = "NSAID",
        y = "Adjusted Sequence Ratio"
      ) +
      scale_shape_manual(values = c("Male" = 17, "Female" = 16)) +
      scale_color_manual(values = c("Negative / Null" = "#1f77b4", "Positive" = "#d62728")) +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold", size = 16),
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 16)
      )
    
    
    
    p_sex_comparison
    
    
    
  })
  
  output$forestPlot_sex <- renderPlot(
    get_sex_forest_plot()
  )
  
  # forest plot AGE
  get_age_forest_plot <-reactive({
    
    
    get_data <- ssa_estimates_age |>
      dplyr::mutate(
        `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
      ) %>% 
      dplyr::mutate(
        # Extract sex BEFORE cleaning names
        age_group = dplyr::case_when(
          age_group == "18_64" ~ "Under 65",
          age_group == "65_150" ~ "65 and over",
          TRUE ~ age_group
        ) ) %>% 
      dplyr::mutate(
        # Clean cohort names AFTER extracting age
        `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        `Index cohort name` = stringr::str_remove(`Index cohort name`, "_(65_and_over|18_to_65)$"),
        `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        pair = paste0(`Index cohort name`, "->", `Marker cohort name`
        )) %>% 
      
      filter(asr != Inf) %>% 
      mutate(highlight = ifelse(asr_lower > 1, "Highlighted", "Not Highlighted")) |>
      rename(index_cohort_name = `Index cohort name`,
             marker_cohort_name = `Marker cohort name`)  |>
      mutate(
        index_cohort_name = stringr::str_to_title(index_cohort_name),
        marker_cohort_name = dplyr::case_when(
          marker_cohort_name == "pe" ~ "Pulmonary Embolism",
          marker_cohort_name == "gi_hemorrhage" ~ "GI Hemorrhage",
          marker_cohort_name == "heart_failure" ~ "Heart Failure",
          marker_cohort_name == "dvt" ~ "Deep Vein Thrombosis",
          marker_cohort_name == "stroke_broad" ~ "Stroke",
          marker_cohort_name == "isbroad" ~ "Ischemic Stroke",
          marker_cohort_name == "acute_mi" ~ "Myocardial Infarction",
          marker_cohort_name == "hem_stroke" ~ "Hemorrhagic Stroke",
          
          TRUE ~ stringr::str_to_title(marker_cohort_name)),
        index_cohort_name = dplyr::case_when(
          index_cohort_name == "All_nsaids" ~ "Any NSAIDs (excl. Aspirin)",
          index_cohort_name == "Non_selective" ~ "Non-selective",
          TRUE ~ index_cohort_name),
          signal = dplyr::case_when(
            signal == "Null" ~ "Negative / Null",
            signal == "Negative" ~ "Negative / Null",
            TRUE ~ signal
          )
        ) |>
          mutate(index_cohort_name = factor(index_cohort_name, 
                                            levels = rev(c("Any NSAIDs (excl. Aspirin)", "Cox-2", "Celecoxib", "Etoricoxib", "Non-selective", "Aspirin", 
                                                           "Diclofenac", "Etodolac", "Ibuprofen", "Indomethacin", "Mefenamate", "Meloxicam", "Naproxen",
                                                           "Piroxicam", "Acetaminophen")),
                                            ordered = TRUE))
    
    
    p_age_comparison <- ggplot(get_data, aes(
      x = index_cohort_name,
      y = asr,
      ymin = asr_lower,
      ymax = asr_upper,
      shape = age_group,
      color = signal
    )) +
      geom_hline(yintercept = 1, linetype = 2) +
      # Draw error bars with thicker lines
      geom_errorbar(
        aes(ymin = asr_lower, ymax = asr_upper),
        position = position_dodge(width = 0.8),
        width = 0,
        size = 1  # This controls the thickness of the error bar line
      ) +
      # Add points separately
      geom_point(
        position = position_dodge(width = 0.8),
        size = 3.5  # Controls the size of the point
      ) +
      facet_wrap(~ marker_cohort_name) +
      coord_flip() +
      theme_bw() +
      labs(
        x = "NSAID",
        y = "Adjusted Sequence Ratio"
      ) +
      scale_shape_manual(values = c("Under 65" = 17, "65 and over" = 16)) +
      scale_color_manual(values = c("Negative / Null" = "#1f77b4", "Positive" = "#d62728")) +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold", size = 16),
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 16)
      )
    
    
    
    p_age_comparison
    
    
    
  })
  
  output$forestPlot_age <- renderPlot(
    get_age_forest_plot()
  )
  
  
  # sex ssa -----
  get_ssa_sex_estimates <-reactive({
    
    table <- ssa_estimates_sex %>% 
      select(-c(
        asr   ,            
        asr_lower    ,
        asr_upper 
        
      )) %>% 
      filter(signal %in% input$sex_ssa_signal_selector) %>% 
      filter(`Index cohort name` %in% input$sex_ssa_index_name_selector) %>% 
      filter(`Marker cohort name` %in% input$sex_ssa_marker_name_selector)
    
    table
    
  }) 
  
  output$tbl_sex_ssa <- renderText(kable(get_ssa_sex_estimates()) %>%
                                         kable_styling("striped", full_width = F) )
  
  
  
  output$gt_sex_ssa_word <- downloadHandler(
    filename = function() {
      "sex_ssa_estimates.docx"
    },
    content = function(file) {
      x <- gt(get_ssa_sex_estimates())
      gtsave(x, file)
    }
  )
  
  
  # age ssa -----
  get_ssa_age_estimates <-reactive({
    
    table <- ssa_estimates_age %>% 
      select(-c(
        asr   ,            
        asr_lower    ,
        asr_upper 
        
      )) %>% 
      filter(signal %in% input$age_ssa_signal_selector) %>% 
      filter(`Index cohort name` %in% input$age_ssa_index_name_selector) %>% 
      filter(`Marker cohort name` %in% input$age_ssa_marker_name_selector)
    
    table
    
  }) 
  
  output$tbl_age_ssa <- renderText(kable(get_ssa_age_estimates()) %>%
                                     kable_styling("striped", full_width = F) )
  
  
  
  output$gt_age_ssa_word <- downloadHandler(
    filename = function() {
      "age_ssa_estimates.docx"
    },
    content = function(file) {
      x <- gt(get_ssa_age_estimates())
      gtsave(x, file)
    }
  )
  
 
  # im settings -----
  get_ssa_settings <-reactive({
    
    table <- im_settings %>% 
      filter(marker_name %in% input$settings_marker_selector) %>% 
      filter(index_name %in% input$settings_cohort_selector) %>% 
      select(-c(cohort_definition_id))
    
    table
    
  }) 
  
  output$tbl_im_settings <- renderText(kable(get_ssa_settings()) %>%
                                     kable_styling("striped", full_width = F) )
  
  
  
  output$gt_im_settings_word <- downloadHandler(
    filename = function() {
      "im_ssa_settings.docx"
    },
    content = function(file) {
      x <- gt(get_ssa_settings())
      gtsave(x, file)
    }
  )
  
  
  # im attrition -----
  get_im_attrition <-reactive({
    
    table <- im_attrition %>% 
    filter(marker_name %in% input$im_attrition_marker_selector) %>% 
      filter(index_name %in% input$im_attrition_cohort_selector) 

    
    table
    
  }) 
  
  output$tbl_im_attrition <- renderText(kable(get_im_attrition()) %>%
                                         kable_styling("striped", full_width = F) )
  
  
  
  output$gt_im_attrition_word <- downloadHandler(
    filename = function() {
      "im_attrition.docx"
    },
    content = function(file) {
      x <- gt(get_im_attrition())
      gtsave(x, file)
    }
  )
  
  
  
  
  # im temporal -----
  get_im_temporal <-reactive({
    
    table <- im_temporal %>%
      filter(group_level %in% input$im_temporal_cohort_selector ) %>% 
      select(-c(result_id,
                estimate_type,
                group_name,
                strata_name,
                strata_level,
                additional_name,
                additional_level)) %>% 
      mutate(estimate_value = if_else(estimate_value < 5, "<5", as.character(estimate_value)))

    
  }) 
  
  output$tbl_im_temporal <- renderText(kable(get_im_temporal()) %>%
                                          kable_styling("striped", full_width = F) )
  
  
  
  output$gt_im_temporal_word <- downloadHandler(
    filename = function() {
      "im_temporal.docx"
    },
    content = function(file) {
      x <- gt(get_im_temporal())
      gtsave(x, file)
    }
  )
  
  
  #patient_demographics ----
  get_demo_characteristics <- reactive({

    demo_characteristics <- demo_characteristics %>%
      filter(strata_name %in% input$demographics_strata_selector )

    demo_characteristics
    
    
  })


  output$gt_demo_characteristics  <- render_gt({
    CohortCharacteristics::tableCharacteristics(get_demo_characteristics(),
                                          header = c("cdm_name", "cohort_name")
                                          )
  })


  output$gt_demo_characteristics_word <- downloadHandler(
    filename = function() {
      "demographics_characteristics.docx"
    },
    content = function(file) {
      gtsave(CohortCharacteristics::tableCharacteristics(get_demo_characteristics(),
                                                   header = c("group", "cdm_name", "strata")), file)
    }
  )

# temporal sequence plot
  
  get_ts_plot <- reactive({
    
    validate(need(input$ts_plot_marker_selector != "", "Please select a Marker"))
    validate(need(input$ts_plot_index_selector != "", "Please select an Index"))
    
    
    plot_data <- im_temporal_test %>%
      filter(index_name %in% input$ts_plot_index_selector,
             marker_name %in% input$ts_plot_marker_selector,
             combination_window %in% input$ts_plot_window_selector)  
    
    plot <- plot_data %>%
      plotTemporalSymmetry1(
        labs = c("Time (Months)", "Individuals (N)"),
        xlim = c(-6, 6),
        colours = c( "#1f77b4", "#ff7f0e")
        )

    plot 
    
  })
  
  output$ts_plot <- renderPlot(
    get_ts_plot()
  )
  
  output$ts_plot_download_plot <- downloadHandler(
    filename = function() {
      "Temporal_sequence_plot.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_ts_plot(),
        width = as.numeric(input$ts_plot_download_width),
        height = as.numeric(input$ts_plot_download_height),
        dpi = as.numeric(input$ts_plot_download_dpi),
        units = "cm"
      )
    }
  )
  
  # temporal sequence controls plot
  
  get_ts_plot_pnc <- reactive({

    validate(need(input$ts_plot_marker_selector_pnc != "", "Please select a Marker"))
    validate(need(input$ts_plot_index_selector_pnc != "", "Please select an Index"))


    plot_data_pnc <- im_temporal_controls_test %>%
      filter(index_name %in% input$ts_plot_index_selector_pnc,
             marker_name %in% input$ts_plot_marker_selector_pnc)

    plot_controls <- plot_data_pnc %>%
      plotTemporalSymmetry1(
        labs = c("Time (Months)", "Individuals (N)"),
        xlim = c(-6, 6),
        colours = c( "#1f77b4", "#ff7f0e")
      )

    plot_controls

  })

  output$ts_plots_pnc <- renderPlot(
    get_ts_plot_pnc()
  )

  output$ts_plot_download_plot_pnc <- downloadHandler(
    filename = function() {
      "Temporal_sequence_plot_controls.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_ts_plot_pnc(),
        width = as.numeric(input$ts_plot_download_width_pnc),
        height = as.numeric(input$ts_plot_download_height_pnc),
        dpi = as.numeric(input$ts_plot_download_dpi_pnc),
        units = "cm"
      )
    }
  )

  # temporal sequence plot sex
  
  get_ts_plot_sex <- reactive({
    
    validate(need(input$ts_plot_index_selector_sex != "", "Please select a Index"))
    validate(need(input$ts_plot_marker_selector_sex != "", "Please select a Marker"))
    validate(need(input$ts_plot_sex_selector_sex != "", "Please select sex"))
    
    
    plot_data_sex <- im_temporal_test_sex %>%
      filter(index_name %in% input$ts_plot_index_selector_sex) %>%
      filter(marker_name %in% input$ts_plot_marker_selector_sex) %>%
      filter(sex %in% input$ts_plot_sex_selector_sex)
    
    plot_sex <- plot_data_sex %>%
      plotTemporalSymmetry1_sex(
        labs = c("Time (Months)", "Individuals (N)"),
        xlim = c(-6, 6),
        colours = c( "#1f77b4", "#ff7f0e")
      )
    
    plot_sex 
    
  })
  
  output$ts_plot_sex <- renderPlot(
    get_ts_plot_sex()
  )
  
  output$ts_plot_download_plot_sex <- downloadHandler(
    filename = function() {
      "Temporal_sequence_plot_sex.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_ts_plot_sex(),
        width = as.numeric(input$ts_plot_download_width_sex),
        height = as.numeric(input$ts_plot_download_height_sex),
        dpi = as.numeric(input$ts_plot_download_dpi_sex),
        units = "cm"
      )
    }
  )
  
  # temporal sequence plot age
  
  get_ts_plot_age <- reactive({
    
    validate(need(input$ts_plot_index_selector_age != "", "Please select a Index"))
    validate(need(input$ts_plot_marker_selector_age != "", "Please select a Marker"))
    
    
    plot_data_age <- im_temporal_test_age %>%
      filter(index_name %in% input$ts_plot_index_selector_age)%>%
      filter(marker_name %in% input$ts_plot_marker_selector_age)
    
    plot_age <- plot_data_age %>%
      plotTemporalSymmetry1_age(
        labs = c("Time (Months)", "Individuals (N)"),
        xlim = c(-6, 6),
        colours = c( "#1f77b4", "#ff7f0e")
      )
    
    plot_age 
    
  })
  
  output$ts_plot_age <- renderPlot(
    get_ts_plot_age()
  )
  
  output$ts_plot_download_plot_age <- downloadHandler(
    filename = function() {
      "Temporal_sequence_plot_age.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_ts_plot_age(),
        width = as.numeric(input$ts_plot_download_width_age),
        height = as.numeric(input$ts_plot_download_height_age),
        dpi = as.numeric(input$ts_plot_download_dpi_age),
        units = "cm"
      )
    }
  )

  get_sa_forest_plot <-reactive({


    get_data_sa <- ssa_estimates_sa |>
      filter(combination_window == "(0, 90)",
             `Index cohort name` %in% input$ssa_index_selector_sa,
             `Marker cohort name` %in% input$ssa_marker_selector_sa
             ) |>
      dplyr::mutate(
        `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
      )  %>%
      dplyr::mutate(
        # Clean cohort names AFTER extracting age
        `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        pair = paste0(`Index cohort name`, "->", `Marker cohort name`
        )) %>%
      filter(asr != Inf) %>%
      mutate(highlight = ifelse(asr_lower > 1, "Highlighted", "Not Highlighted")) %>%
      rename(index_cohort_name = `Index cohort name`,
             marker_cohort_name = `Marker cohort name`)  %>%
      mutate(
        index_cohort_name = stringr::str_to_title(index_cohort_name),
        marker_cohort_name = dplyr::case_when(
          marker_cohort_name == "pe" ~ "Pulmonary Embolism",
          marker_cohort_name == "gi_hemorrhage" ~ "GI Hemorrhage",
          marker_cohort_name == "heart_failure" ~ "Heart Failure",
          marker_cohort_name == "dvt" ~ "Deep Vein Thrombosis",
          marker_cohort_name == "stroke_broad" ~ "Stroke",
          marker_cohort_name == "isbroad" ~ "Ischemic Stroke",
          marker_cohort_name == "acute_mi" ~ "Myocardial Infarction",
          marker_cohort_name == "hem_stroke" ~ "Hemorrhagic Stroke",

          TRUE ~ stringr::str_to_title(marker_cohort_name)),
        index_cohort_name = dplyr::case_when(
          index_cohort_name == "All_nsaids" ~ "Any NSAIDs (excl. Aspirin)",
          index_cohort_name == "Non_selective" ~ "Non-selective",
          index_cohort_name == "Cox_2" ~ "COX-2",
          TRUE ~ index_cohort_name
        )
      ) |>
      mutate(index_cohort_name = factor(index_cohort_name,
                                        levels = rev(c("Any NSAIDs (excl. Aspirin)", "COX-2", "Celecoxib", "Etoricoxib", "Non-selective", "Aspirin",
                                                       "Diclofenac", "Etodolac", "Ibuprofen", "Indomethacin", "Mefenamate", "Meloxicam", "Naproxen",
                                                       "Piroxicam","Acetaminophen")),
                                        ordered = TRUE))


    p_sa_comparison <- ggplot(get_data_sa, aes(
      x = index_cohort_name,
      y = asr,
      ymin = asr_lower,
      ymax = asr_upper,
      color = signal
    )) +
      geom_hline(yintercept = 1, linetype = 2) +
      geom_errorbar(
        aes(ymin = asr_lower, ymax = asr_upper),
        position = position_dodge(width = 0.8),
        width = 0,
        size = 1
      ) +
      geom_point(
        position = position_dodge(width = 0.8),
        size = 3.5
      ) +
      facet_wrap(~ marker_cohort_name) +
      coord_flip() +
      theme_bw() +
      labs(
        x = "NSAID",
        y = "Adjusted Sequence Ratio"
      ) +
      scale_y_discrete(
        labels = c(
          "<b>Any NSAIDs (excl. Aspirin)</b>",
          "<b>COX-2</b>",
          "Celecoxib",
          "Etoricoxib",
          "<b>Non-selective</b>",
          "Aspirin",
          "Diclofenac",
          "Etodolac",
          "Ibuprofen",
          "Indomethacin",
          "Mefenamate",
          "Meloxicam",
          "Naproxen",
          "Piroxicam"
        )
      ) +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold", size = 16),
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 16)
      )




    p_sa_comparison



  })

  output$forestPlot_sa <- renderPlot(
    get_sa_forest_plot()
  )

  get_sa_forest_plot_365 <-reactive({


    get_data_sa_365 <- ssa_estimates_sa |>
      filter(combination_window == "(0, 365)",
             `Index cohort name` %in% input$ssa_index_selector_sa,
             `Marker cohort name` %in% input$ssa_marker_selector_sa) |>
      dplyr::mutate(
        `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
      )  %>%
      dplyr::mutate(
        # Clean cohort names AFTER extracting age
        `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        pair = paste0(`Index cohort name`, "->", `Marker cohort name`
        )) %>%
      filter(asr != Inf) %>%
      mutate(highlight = ifelse(asr_lower > 1, "Highlighted", "Not Highlighted")) %>%
      rename(index_cohort_name = `Index cohort name`,
             marker_cohort_name = `Marker cohort name`)  %>%
      mutate(
        index_cohort_name = stringr::str_to_title(index_cohort_name),
        marker_cohort_name = dplyr::case_when(
          marker_cohort_name == "pe" ~ "Pulmonary Embolism",
          marker_cohort_name == "gi_hemorrhage" ~ "GI Hemorrhage",
          marker_cohort_name == "heart_failure" ~ "Heart Failure",
          marker_cohort_name == "dvt" ~ "Deep Vein Thrombosis",
          marker_cohort_name == "stroke_broad" ~ "Stroke",
          marker_cohort_name == "isbroad" ~ "Ischemic Stroke",
          marker_cohort_name == "acute_mi" ~ "Myocardial Infarction",
          marker_cohort_name == "hem_stroke" ~ "Hemorrhagic Stroke",

          TRUE ~ stringr::str_to_title(marker_cohort_name)),
        index_cohort_name = dplyr::case_when(
          index_cohort_name == "All_nsaids" ~ "Any NSAIDs (excl. Aspirin)",
          index_cohort_name == "Non_selective" ~ "Non-selective",
          index_cohort_name == "Cox_2" ~ "COX-2",
          TRUE ~ index_cohort_name
        ),
        sex = dplyr::case_when(
          sex == "Both" ~ "Both sexes",
          TRUE ~ sex
        )
      ) |>
       mutate(index_cohort_name = factor(index_cohort_name,
                                         levels = rev(c("Any NSAIDs (excl. Aspirin)", "COX-2", "Celecoxib", "Etoricoxib", "Non-selective", "Aspirin",
                                                        "Diclofenac", "Etodolac", "Ibuprofen", "Indomethacin", "Ketoprofen", "Mefenamate", "Meloxicam", "Nabumetone",
                                                        "Naproxen", "Piroxicam", "Acetaminophen")),
                                         ordered = TRUE))


    p_sa_comparison_365 <- ggplot(get_data_sa_365, aes(
      x = index_cohort_name,
      y = asr,
      ymin = asr_lower,
      ymax = asr_upper,
      color = signal
    )) +
      geom_hline(yintercept = 1, linetype = 2) +
      # Draw error bars with thicker lines
      geom_errorbar(
        aes(ymin = asr_lower, ymax = asr_upper),
        position = position_dodge(width = 0.8),
        width = 0,
        size = 1  # This controls the thickness of the error bar line
      ) +
      # Add points separately
      geom_point(
        position = position_dodge(width = 0.8),
        size = 3.5  # Controls the size of the point
      ) +
      facet_wrap(~ marker_cohort_name) +
      coord_flip() +
      theme_bw() +
      labs(
        x = "NSAID",
        y = "Adjusted Sequence Ratio"
      ) +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold", size = 16),
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 16)
      )



    p_sa_comparison_365



  })

  output$forestPlot_sa_365 <- renderPlot(
    get_sa_forest_plot_365()
  )

  # sa ssa -----
  get_ssa_sa_estimates <-reactive({

    table <- ssa_estimates_sa %>%
      select(-c(
        asr   ,
        asr_lower    ,
        asr_upper

      )) %>%
      filter(signal %in% input$ssa_signal_selector_sa) %>%
      filter(`Index cohort name` %in% input$ssa_index_name_selector_sa) %>%
      filter(`Marker cohort name` %in% input$ssa_marker_name_selector_sa) %>%
      filter(combination_window %in% input$ssa_window_name_selector_sa)

    table

  })

  output$tbl_ssa_sa <- renderText(kable(get_ssa_sa_estimates()) %>%
                                     kable_styling("striped", full_width = F) )



  output$gt_sa_ssa_word <- downloadHandler(
    filename = function() {
      "ssa_estimates_sa.docx"
    },
    content = function(file) {
      x <- gt(get_ssa_sa_estimates())
      gtsave(x, file)
    }
  )
  
  # forest plot PPI
  get_ppi_forest_plot <-reactive({
    
    
    get_data <- ssa_estimates_ppi_bind  |>
      filter(`Index cohort name` %in% input$ssa_index_selector_ppi,
             `Marker cohort name` %in% input$ssa_marker_selector_ppi) |>
      dplyr::mutate(
        `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        pair = paste0(`Index cohort name`, "->", `Marker cohort name`
        )) |>
      filter(asr != Inf) %>% 
      mutate(highlight = ifelse(asr_lower > 1, "Highlighted", "Not Highlighted")) |>
      rename(index_cohort_name = `Index cohort name`,
             marker_cohort_name = `Marker cohort name`)  |>
      mutate(
        index_cohort_name = stringr::str_to_title(index_cohort_name),
        index_cohort_name = dplyr::case_when(
          index_cohort_name == "All_nsaids" ~ "Any NSAIDs (excl. Aspirin)",
          TRUE ~ index_cohort_name
        ),
        marker_cohort_name = dplyr::case_when(
          marker_cohort_name == "pe" ~ "Pulmonary Embolism",
          marker_cohort_name == "gi_hemorrhage" ~ "GI Hemorrhage",
          marker_cohort_name == "heart_failure" ~ "Heart Failure",
          marker_cohort_name == "dvt" ~ "Deep Vein Thrombosis",
          marker_cohort_name == "stroke_broad" ~ "Stroke",
          marker_cohort_name == "isbroad" ~ "Ischemic Stroke",
          marker_cohort_name == "acute_mi" ~ "Myocardial Infarction",
          marker_cohort_name == "hem_stroke" ~ "Hemorrhagic Stroke",
          TRUE ~ stringr::str_to_title(marker_cohort_name)),
        signal = dplyr::case_when(
          signal == "Null" ~ "Negative / Null",
          signal == "Negative" ~ "Negative / Null",
          TRUE ~ signal
        )
      )
    
    
    p_ppi_comparison <- ggplot(get_data, aes(
      x = marker_cohort_name,
      y = asr,
      ymin = asr_lower,
      ymax = asr_upper,
      shape = type,
      color = signal
    )) +
      geom_hline(yintercept = 1, linetype = 2) +
      # Draw error bars with thicker lines
      geom_errorbar(
        aes(ymin = asr_lower, ymax = asr_upper),
        position = position_dodge(width = 0.8),
        width = 0,
        size = 1  # This controls the thickness of the error bar line
      ) +
      # Add points separately
      geom_point(
        position = position_dodge(width = 0.8),
        size = 3.5  # Controls the size of the point
      ) +
      facet_wrap(~ index_cohort_name) +
      coord_flip() +
      theme_bw() +
      labs(
        x = "NSAID",
        y = "Adjusted Sequence Ratio"
      ) +
      scale_shape_manual(values = c("PPI" = 17, "No PPI" = 16)) +
      scale_color_manual(values = c("Negative / Null" = "#1f77b4", "Positive" = "#d62728")) +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        strip.text = element_text(face = "bold", size = 16),
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 16)
      )
    
    
    
    p_ppi_comparison
    
    
    
  })
  
  output$forestPlot_ppi <- renderPlot(
    get_ppi_forest_plot()
  )
  
  # ppi ssa -----
  get_ssa_ppi_estimates <-reactive({
    
    table <- ssa_estimates_ppi_bind %>% 
      select(-c(
        asr,            
        asr_lower,
        asr_upper 
      )) %>% 
      filter(signal %in% input$ppi_ssa_signal_selector) %>% 
      filter(`Index cohort name` %in% input$ppi_ssa_index_name_selector) %>% 
      filter(`Marker cohort name` %in% input$ppi_ssa_marker_name_selector)
    
    table
    
  }) 
  
  output$tbl_ppi_ssa <- renderText(kable(get_ssa_ppi_estimates()) %>%
                                     kable_styling("striped", full_width = F) )
  
  
  
  output$gt_ppi_ssa_word <- downloadHandler(
    filename = function() {
      "ppi_ssa_estimates.docx"
    },
    content = function(file) {
      x <- gt(get_ssa_ppi_estimates())
      gtsave(x, file)
    }
  )
}