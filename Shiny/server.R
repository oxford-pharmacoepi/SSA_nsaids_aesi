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
  
  # forest plot for drug ingredient
  get_ing_forest_plot <-reactive({
    
    plot_data <- atc_ssa_ing %>%
      filter(signal == "Positive") %>% 
      arrange(desc(asr)) %>%   
      dplyr::select(name,
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
      rename("Drug Ingredient" = "name") %>% 
      mutate(`Drug Ingredient` = str_replace_all(`Drug Ingredient`, "_", " ")) %>%  # replace underscores with spaces
      mutate(`Drug Ingredient` = str_to_title(`Drug Ingredient`)) %>%
      mutate(`Drug Ingredient` = if_else(`Drug Ingredient` == "Sodium Lauryl Sulfoacetate", 
                                         "SLSA", 
                                         `Drug Ingredient`))
    
    
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
    forest_plot_ing <- forest(
      plot_data[,c(1:7)],
      est = plot_data$asr,
      lower = plot_data$asr_lower,
      upper = plot_data$asr_upper,
      ci_column = 4,  # ASR [99% CI] column (it's 4th here)
      ref_line = 1,
      xlim = c(1, 14),
      xlab = "ASR (99% CI)",
      theme = tm
    )
    
    forest_plot_ing
    
    
  })
  
  output$forestPlot_ing <- renderPlot(
    get_ing_forest_plot()
  )
  

  # atc ssa ingredient -----
  get_ing_ssa <-reactive({
    
    table <- atc_ssa_ing %>% 
      filter(`Database name` %in% input$ing_ssa_database_name_selector) %>% 
      filter(signal %in% input$ing_ssa_signal_selector) %>% 
      filter(index_marker_name %in% input$ing_ssa_cohort_name_selector) %>% 
      relocate(NSR, .after = `ASR (99% CI)`) %>% 
      relocate(name, .after = `Marker cohort name`) %>% 
      select(-c(index_marker_name, Attrition_Reason,
                asr, asr_lower, asr_upper, 
                ingredient, concept_id, signal)) %>% 
      distinct()
    
    
    
    
    table
    
  }) 
  
  output$tbl_ing_ssa <- renderText(kable(get_ing_ssa()) %>%
                                     kable_styling("striped", full_width = F) )
  
  
  output$gt_ing_ssa_word <- downloadHandler(
    filename = function() {
      "ing_ssa_estimates.docx"
    },
    content = function(file) {
      x <- gt(get_ing_ssa())
      gtsave(x, file)
    }
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
  
  
  # atc concepts -----
  get_atc_concepts <-reactive({
    
    table <- atc_concepts %>% 
      filter(cdm_name %in% input$atc_concepts_database_name_selector) %>% 
      filter(ATC_class %in% input$atc_concept_cohort_name_selector) %>% 
      filter(vocabulary_id %in% input$atc_concepts_vocab_selector) 
    
    table
    
  }) 
  
  output$tbl_atc_concepts <- renderText(kable(get_atc_concepts()) %>%
                                         kable_styling("striped", full_width = F) )
  
  
  
  output$gt_atc_concepts_word <- downloadHandler(
    filename = function() {
      "atc_concepts.docx"
    },
    content = function(file) {
      x <- gt(get_atc_concepts())
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

 
  #comorbidities_demographics ----
  get_comorb_characteristics <- reactive({


    
    validate(
      need(input$comorb_database_name_selector != "", "Please select a database")
    )
    
    comorb_characteristics <- comorb_characteristics %>%
      filter(cdm_name %in% input$comorb_database_name_selector)

    
    comorb_characteristics
    
  })
  
  
  output$gt_comorb_characteristics  <- render_gt({
    CohortCharacteristics::tableCharacteristics(get_comorb_characteristics(),
                                          header = c( "cdm_name"),
                                          hide = c("group_name", "cohort_name", "table_name",
                                                   "table", "window", "value")
                                          )
  })
  
  
  output$gt_comorb_characteristics_word <- downloadHandler(
    filename = function() {
      "comorbidities_characteristics.docx"
    },
    content = function(file) {
      
      gtsave(CohortCharacteristics::tableCharacteristics(get_comorb_characteristics(),
                                                       
                                                       header = c("group", "cdm_name", "strata"), file))
    }
  )
  
  
  
  #medications_demographics ----
  get_med_characteristics <- reactive({
    
    
    validate(
      need(input$med_database_name_selector != "", "Please select a database")
    )
    
    med_characteristics <- med_characteristics %>%
      filter(cdm_name %in% input$med_database_name_selector) 
    
    med_characteristics
    
  })
  
  
  output$gt_med_characteristics  <- render_gt({
    CohortCharacteristics::tableCharacteristics(get_med_characteristics(),
                                                header = c("cdm_name"),
                                                hide = c("group_name", "cohort_name", "table_name",
                                                         "table", "window", "value")
    )
  })
  
  
  output$gt_med_characteristics_word <- downloadHandler(
    filename = function() {
      "medications_characteristics.docx"
    },
    content = function(file) {
      
      gtsave(CohortCharacteristics::tableCharacteristics(get_med_characteristics(),
                                                       header = c("group", "cdm_name", "strata")
      ), file)
    }
  )
  
  
   

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
  
  # temporal sequence plot ingredient level
  get_ts_ing_plot <- reactive({
    
    validate(need(input$ts_ing_plot_marker_selector != "", "Please select a Marker"))
    validate(need(input$ts_ing_plot_database_selector != "", "Please select a database"))
    validate(need(input$ts_ing_plot_facet != "", "Please select a group to facet by"))
    
    
    plot_data <- im_temporal_test_ing %>%
      filter(cdm_name %in% input$ts_ing_plot_database_selector)  %>%
      filter(group_level %in% input$ts_ing_plot_marker_selector)  
    
    
      plot <- plot_data %>%
        plotTemporalSymmetry1(
          labs = c("Time (Months)", "Individuals (N)"),
          xlim = c(-6, 6),
          colours = c( "#c994c7", "#dd1c77"))
      
      plot 

    
  })
  
  output$ts_ing_plot <- renderPlot(
    get_ts_ing_plot()
  )
  
  output$ts_plot_download_plot <- downloadHandler(
    filename = function() {
      "Temporal_sequence_plot_ingredients.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_ts_ing_plot(),
        width = as.numeric(input$ts_ing_plot_download_width),
        height = as.numeric(input$ts_ing_plot_download_height),
        dpi = as.numeric(input$ts_ing_plot_download_dpi),
        units = "cm"
      )
    }
  )
  
  
  get_pssa_plot <- reactive({

    validate(need(input$pssa_database_selector != "", "Please select a database"))
    validate(need(input$pssa_firstatc_level_selector != "", "Please select ATC class group"))
    

    plot_data <- atc_ssa %>%
      filter(`Database name` %in% input$pssa_database_selector)  %>%
      filter(first_level %in% input$pssa_firstatc_level_selector) 
    
    max_x_data <- plot_data %>%
      group_by(`first_level`) %>%
      summarise(max_x = max(`Marker cohort name`))


    # Calculate the maximum values and identify the last group
    max_x <- max_x_data %>%
      slice_head(n = -1)

    # Get the maximum `Marker` value of the last group for the solid line
    last_max_x <- max_x_data %>%
      slice_tail(n = 1) %>%
      pull(max_x)
    

    plot <- plot_data %>%
      #unite("facet_var", c(all_of(input$pssa_plot_facet)), remove = FALSE, sep = "; ") %>%
      ggplot( mapping = aes(x = `Marker cohort name`, 
                            text = ATC_Class,  
                            y = asr)) +
      
      # Define colors
      scale_color_manual(
        values = c(
          "Positive" = "#800080",  # Dark purple for "Positive"
          "Null" = "#c994c7",     # Light purple for other values
          "Negative" = "#c994c7"  # Light purple for other values
        )
      ) +
      scale_y_continuous(breaks = seq(0, max(plot_data$asr, na.rm = TRUE), by = 1)) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major = element_blank(),
            axis.line.x = element_line(color = 'black'),
            axis.line.y = element_line(color = 'black'),
            panel.spacing = unit(0, "lines"),
            panel.background = element_blank(),
            strip.background = element_blank(),
            strip.clip = "off") +
      # labs(x = "ATC Class Group",
      #      y = "Adjusted Sequence Ratio") +
      labs(x = " ",
           y = "Adjusted Sequence Ratio") +
      facet_grid(. ~ first_level,
                 scales = "free_x",
                 space = "free_x",
                 switch = "x") +
    
      geom_vline(data = max_x, aes(xintercept = max_x), color = "black", linetype = "dashed") +
      geom_hline(yintercept = -Inf, color = "black", linewidth = 1) +
      geom_hline(yintercept = Inf, color = "black", linewidth = 1) +
      # Plot non-positive points first
      geom_point(data = plot_data %>% filter(signal != "Positive"),
                 alpha = 1, size = 2, aes(color = signal)) +
      # Plot positive points on top
      geom_point(data = plot_data %>% filter(signal == "Positive"),
                 alpha = 1, size = 3, aes(color = signal)) +
      geom_vline(xintercept = last_max_x, color = "black", linetype = "solid") +  # Add the solid black vline
      guides(color = "none")
    
      
      plot <- plotly::ggplotly(plot, tooltip = "text") %>%
        layout(xaxis = list(autorange = TRUE), yaxis = list(autorange = TRUE))
      
      
      for (i in seq_along(plot$x$layout$annotations)) {
        if (plot$x$layout$annotations[[i]]$text %in% unique(plot_data$first_level)) {
          plot$x$layout$annotations[[i]]$y <- -0.04  # Position annotations just above x-axis title
          
          
        }
      }

      
      
      plot <- plot %>%
        layout(
          margin = list(t = 10, r = 50, b = 70, l = 50),
          annotations = list(
            x = 0.5,  # Place annotation centered horizontally
            y = -0.08,  # Position it above the x-axis title
            text = "ATC Class Group",  # Replace with your actual annotation text
            showarrow = FALSE,
            xref = "paper",  # Relative to the entire plot width
            yref = "paper",  # Relative to the entire plot height
            xanchor = "center",  # Align the annotation text center
            yanchor = "bottom",  # Align it above the x-axis title
            align = "center"  # Center align text within the annotation box
          )
        )
      

      
  

  })
  
  
  output$pssaPlot <- renderPlotly(
    get_pssa_plot()
  )


  get_pssa_plot1 <- reactive({

    validate(need(input$pssa_database_selector != "", "Please select a database"))
    validate(need(input$pssa_firstatc_level_selector != "", "Please select ATC class group"))


    plot_data <- atc_ssa %>%
      filter(`Database name` %in% input$pssa_database_selector)  %>%
      filter(first_level %in% input$pssa_firstatc_level_selector)

    max_x_data <- plot_data %>%
      group_by(`first_level`) %>%
      summarise(max_x = max(`Marker cohort name`))


    # Calculate the maximum values and identify the last group
    max_x <- max_x_data %>%
      slice_head(n = -1)

    # Get the maximum `Marker` value of the last group for the solid line
    last_max_x <- max_x_data %>%
      slice_tail(n = 1) %>%
      pull(max_x)


    plot <- plot_data %>%
      ggplot( mapping = aes(x = `Marker cohort name`,
                            y = asr)) +

      # Define colors
      scale_color_manual(
        values = c(
          "Positive" = "#800080",  # Dark purple for "Positive"
          "Null" = "#c994c7",     # Light purple for other values
          "Negative" = "#c994c7"  # Light purple for other values
        )
      ) +
      scale_y_continuous(breaks = seq(0, max(plot_data$asr, na.rm = TRUE), by = 1)) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major = element_blank(),
            axis.line.y = element_line(color = 'black'),
            panel.spacing = unit(0, "lines"),
            panel.background = element_blank(),
            strip.background = element_blank(),
            strip.clip = "off") +
      labs(x = "ATC Class Group",
           y = "Adjusted Sequence Ratio") +
      facet_grid(. ~ first_level,
                 scales = "free_x",
                 space = "free_x",
                 switch = "x") +

      geom_vline(data = max_x, aes(xintercept = max_x), color = "black", linetype = "dashed") +
      geom_hline(yintercept = -Inf, color = "black", linewidth = 1) +
      geom_hline(yintercept = Inf, color = "black", linewidth = 1) +
      # Plot non-positive points first
      geom_point(data = plot_data %>% filter(signal != "Positive"),
                 alpha = 1, size = 2, aes(color = signal)) +
      # Plot positive points on top
      geom_point(data = plot_data %>% filter(signal == "Positive"),
                 alpha = 1, size = 3, aes(color = signal)) +
      geom_vline(xintercept = last_max_x, color = "black", linetype = "solid") +  # Add the solid black vline
      guides(color = "none")



  })
  
  

  output$pssa_download_plot <- downloadHandler(
    filename = function() {
      "pssa_plot.png"
    },
    content = function(file) {
      ggsave(
        file,
        get_pssa_plot1(),
        width = as.numeric(input$pssa_plot_download_width),
        height = as.numeric(input$pssa_plot_download_height),
        dpi = as.numeric(input$pssa_plot_download_dpi),
        units = "cm"
      )
    }
  )
  

# box plot for sensitivity analysis
  
get_box_sensitivity_plot <- reactive({
    
    # for plot
    plot_data <- wide_atc %>%
      select(`Marker cohort name`, signal_180, signal_365, `Database name`) %>%
      pivot_longer(
        cols = starts_with("signal_"),
        names_to = "window",
        values_to = "signal") %>% 
      mutate(
        window = recode(window, signal_180 = "180", signal_365 = "365")
      )
    
    # Define your colors
    signal_colors <- c(
      "Positive" = "gold",
      "Negative" = "steelblue",
      "Null"     = "grey50"  # darker grey for "Null"
    )
    
    
    plot <- ggplot(plot_data, aes(x = window, y = `Marker cohort name`, fill = signal)) +
      geom_tile(color = "white", width = 0.9, height = 0.9) +  # control tile size
      scale_fill_manual(
        values = signal_colors,
        na.value = "white"  # lighter grey for actual NA values
      ) +
      labs(
        x = "Time window (Days)",
        y = "ATC class",
        fill = "Signal"
      ) +
      coord_fixed(ratio = 1) +  # forces square aspect ratio
      theme_minimal(base_size = 10) +
      facet_wrap(~ `Database name`) +  # facet by cdm_name
      theme(
        panel.grid = element_blank(),
        strip.clip = "off",
        axis.text.y = element_text(size = 10, hjust = 1.5),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.5)  # adjust here
      ) 
    
    plot
    
    
    
  })
  
  
  output$sensitivityPlot <- renderPlot(
    get_box_sensitivity_plot()
  )
  
  
  


   
}