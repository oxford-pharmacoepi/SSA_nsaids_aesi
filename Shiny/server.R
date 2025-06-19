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
  
  
  
  # atc ssa -----
  get_atc_ssa <-reactive({
    
    table <- atc_ssa %>% 
      select(-c(
        asr   ,            
        asr_lower    ,
        asr_upper 
        
      )) %>% 
      filter(`Database name` %in% input$atc_ssa_database_name_selector) %>% 
      filter(signal %in% input$atc_ssa_signal_selector) %>% 
      filter(index_marker_name %in% input$atc_ssa_cohort_name_selector) %>% 
      select(-c(index_marker_name,
                first_level
                )) %>% 
      relocate(NSR, .before = signal)
      
      
      
    
    table
    
  }) 
  
  output$tbl_atc_ssa <- renderText(kable(get_atc_ssa()) %>%
                                           kable_styling("striped", full_width = F) )
  
  
  
  output$gt_atc_ssa_word <- downloadHandler(
    filename = function() {
      "atc_class_ssa_estimates.docx"
    },
    content = function(file) {
      x <- gt(get_atc_ssa())
      gtsave(x, file)
    }
  )

  # forest plot for atc class
  get_atc_forest_plot <-reactive({
    
    plot_data <- atc_ssa %>%
      filter(signal == "Positive") %>% 
      filter(`Marker cohort name` != "N06DX") %>% 
      filter(`Marker cohort name` != "MEMANTINE") %>% 
      filter(`Marker cohort name` != "LEVOTHYROXINE") %>% 
      arrange((ATC_Class)) %>%   
      mutate(ATC_Class = str_replace_all(ATC_Class, "_", " ")) %>%  # Remove underscores
      mutate(ATC_Class = paste0(ATC_Class, "  ")) %>% 
      dplyr::select(ATC_Class,
                    `Index N (%)`,    
                    `Marker N (%)`,
                    `ASR (99% CI)`, 
                    `CSR (99% CI)`, 
                    NSR,
                    asr, 
                    asr_lower, 
                    asr_upper) %>%
      # Add a blank column (no name) to be used as the 4th column in the plot
      mutate(` ` = paste(rep(" ", 35), collapse = " ")) %>%
      relocate(` `, .before = `ASR (99% CI)`) %>% 
      rename("ATC 4th Level" = "ATC_Class")
    
    
    tm <- forest_theme(
      base_size = 8,
      refline_col = "black",
      refline_lty = 3,
      ci_pch = 15,
      ci_col = "black",
      ci_lty = 1,
      ci_alpha = 1,
      vertline_col = "grey60",
      arrow_type = "closed",
      ticks_gp = gpar(cex = 0.9),
      lineheight = 0.2 ,
      zero_line_col = "#e31a1c",
      zero_line_lty = 2
    )
    
    # Now you don't need to bind the header anymore
    # Just use the table_data directly
    forest_plot <- forest(
      plot_data[,c(1:7)],
      est = plot_data$asr,
      lower = plot_data$asr_lower,
      upper = plot_data$asr_upper,
      ci_column = 4,  # ASR [99% CI] column (it's 4th here)
      ref_line = 1,
      xlim = c(1, 10),
      xlab = "ASR (99% CI)",
      theme = tm
    )
    
    forest_plot
    
    
  })
  
  output$forestPlot_atc <- renderPlot(
    get_atc_forest_plot()
  )
  
 
  # im settings -----
  get_ssa_settings <-reactive({
    
    table <- im_settings %>% 
      filter(cdm_name %in% input$atc_ssa_database_name_selector) %>% 
      filter(marker_type %in% input$settings_marker_type_selector) %>% 
      filter(cohort_name %in% input$settings_cohort_selector) 
    
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
      filter(cdm_name %in% input$im_attrition_database_name_selector) %>% 
      filter(cohort_name %in% input$im_attrition_cohort_name_selector) %>% 
      filter(marker_type %in% input$im_attrition_marker_type_selector)

    
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
      filter(timescale %in% input$im_temporal_time_selector ) %>% 
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
      filter(cdm_name %in% input$demographics_database_name_selector) 

    demo_characteristics
    
    
  })


  output$gt_demo_characteristics  <- render_gt({
    CohortCharacteristics::tableCharacteristics(get_demo_characteristics(),
                                          header = c("cdm_name"),
                                          hide = c("group_name", "cohort_name", "table_name",
                                                   "table", "window", "value")
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
        colours = c( "#c994c7", "#dd1c77"))
    
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