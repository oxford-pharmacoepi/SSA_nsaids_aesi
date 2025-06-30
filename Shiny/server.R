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
      filter(`Database name` %in% input$overall_ssa_database_name_selector) %>% 
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

  # forest plot OVERALL
  get_overall_forest_plot <-reactive({
    
    
    get_data <- ssa_estimates |>
      filter(`Database name` %in% input$forest_plot_database_selector) %>% 
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
          
          TRUE ~ stringr::str_to_title(marker_cohort_name)  # Default capitalization
        )
      )
    
    
    labs = c("Adjusted Sequence Ratio", "NSAID")
    custom_colors <- c("adjusted" = "black")
    
    plot_data <- visOmopResults::scatterPlot(
      get_data,
      x = "index_cohort_name",
      y = "asr",
      line = FALSE,
      point = TRUE,
      ribbon = FALSE,
      ymin = "asr_lower",
      ymax = "asr_upper",
      facet = "marker_cohort_name",
      colour = "highlight"
    ) +
      ggplot2::ylab(labs[1]) +
      ggplot2::xlab(labs[2]) +
      ggplot2::coord_flip() +
      #ggplot2::facet_wrap(~marker_cohort_name, scales = "free_x") +  # facet with free x-scale
      ggplot2::theme_bw(base_size = 16) +  # base font size
      ggplot2::geom_hline(yintercept = 1, linetype = 2, linewidth = 1) +
      ggplot2::scale_shape_manual(values = rep(19, 5)) +
      ggplot2::theme(
        # KEY PART: box around each facet
        panel.border = ggplot2::element_rect(color = "black", fill = NA, size = 0.5),

        # Optional: make facets more visible
        strip.background = ggplot2::element_rect(fill = "gray90", color = "black"),
        strip.text = ggplot2::element_text(size = 16),

        # Fonts
        axis.text = ggplot2::element_text(size = 14),
        axis.title = ggplot2::element_text(size = 16),
        plot.title = ggplot2::element_text(hjust = 0.5, size = 18),

        # Legend
        legend.position = "none",
        legend.title = ggplot2::element_blank()
      )
    
    plot_data <-  plot_data + ggplot2::geom_point(size = 4)+  # bigger points
      ggplot2::geom_errorbar(         # thicker lines from ymin to ymax
        ggplot2::aes(ymin = asr_lower, ymax = asr_upper),
        width = 0,
        linewidth = 1
      )

    plot_data

    

    
    


  })

  output$forestPlot_overall <- renderPlot(
    get_overall_forest_plot()
  )
  
  
  # forest plot SEX
  get_sex_forest_plot <-reactive({
    
    
    get_data <- ssa_estimates_sex |>
      dplyr::mutate(
        `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
      ) %>% 
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
          
          TRUE ~ stringr::str_to_title(marker_cohort_name)  # Default capitalization
        )
      )
    
    
    p_sex_comparison <- ggplot(get_data, aes(
      x = index_cohort_name,
      y = asr,
      ymin = asr_lower,
      ymax = asr_upper,
      shape = sex,
      color = sex
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
      facet_wrap(~ marker_cohort_name, scales = "free_x") +
      coord_flip() +
      theme_bw() +
      labs(
        x = "NSAID",
        y = "Adjusted Sequence Ratio"
      ) +
      scale_shape_manual(values = c("Male" = 17, "Female" = 16)) +
      scale_color_manual(values = c("Male" = "#1f77b4", "Female" = "#d62728")) +
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
        age = dplyr::case_when(
          stringr::str_detect(`Index cohort name`, "_18_to_65$") ~ "Under 65",
          stringr::str_detect(`Index cohort name`, "_65_and_over$") ~ "Over 65",
          TRUE ~ "Unspecified"
        ) ) %>% 
      
      dplyr::mutate(
        # Clean cohort names AFTER extracting age
        `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
        `Index cohort name` = stringr::str_remove(`Index cohort name`, "_(65_and_over|18_to_65)$"),
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
          
          TRUE ~ stringr::str_to_title(marker_cohort_name)  # Default capitalization
        )
      )
    
    
    p_age_comparison <- ggplot(get_data, aes(
      x = index_cohort_name,
      y = asr,
      ymin = asr_lower,
      ymax = asr_upper,
      shape = age,
      color = age
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
      facet_wrap(~ marker_cohort_name, scales = "free_x") +
      coord_flip() +
      theme_bw() +
      labs(
        x = "NSAID",
        y = "Adjusted Sequence Ratio"
      ) +
      scale_shape_manual(values = c("Under 65" = 17, "Over 65" = 16)) +
      scale_color_manual(values = c("Under 65" = "#1f77b4", "Over 65" = "#d62728")) +
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
      filter(`Database name` %in% input$sex_ssa_database_name_selector) %>% 
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
      filter(`Database name` %in% input$age_ssa_database_name_selector) %>% 
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
      filter(cdm_name %in% input$im_temporal_database_name_selector) %>%
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

    
    validate(
      need(input$demographics_database_name_selector != "", "Please select a database")
    )

    demo_characteristics <- demo_characteristics %>%
      filter(cdm_name %in% input$demographics_database_name_selector) %>% 
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
    validate(need(input$ts_plot_database_selector != "", "Please select a database"))
    validate(need(input$ts_plot_facet != "", "Please select a group to facet by"))
    
    
    plot_data <- im_temporal_test %>%
      filter(cdm_name %in% input$ts_plot_database_selector)  %>%
      filter(group_level %in% input$ts_plot_marker_selector)  
    
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

   
}