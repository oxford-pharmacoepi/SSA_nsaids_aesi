# #### UI -----

# ui shiny ----
ui <- dashboardPage(

  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center;",  # Align items horizontally and vertically centered
      tags$img(src = "CSHex.png", height = "50px", style = "margin-right: 10px;"),  # Logo image with adjusted height
      tags$span("Menu", style = "font-size: 24px;")  # Menu text with font size adjustment
    ),
    titleWidth = 250  # Adjust title width as needed
  ),
  
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background",
        icon = shiny::icon("book")
      ),

      menuItem(
        text = "Database",
        tabName = "dbs",
        icon = shiny::icon("database"),
        menuSubItem(
          text = "Snapshot",
          tabName = "snapshotcdm"
        ),
        
        menuSubItem(
          text = "ATC concepts",
          tabName = "atc_concepts"
        )
        
      ),
     
      menuItem(
        text = "Characteristics",
        tabName = "char",
        icon = shiny::icon("hospital-user"),
        menuSubItem(
          text = "Demographics",
          tabName = "demographics"
        ),
        menuSubItem(
          text = "Medications",
          tabName = "medications"
        ),
        menuSubItem(
          text = "Comorbidities",
          tabName = "comorbidities"
        )
        ),

      
      menuItem(
        text = "Class Cascades",
        tabName = "cs",
        icon = shiny::icon("star-half-stroke") ,
        menuSubItem(
          text = "Plots",
          tabName = "cs_plots"
        ),
        
        menuSubItem(
          text = "Forest plot",
          tabName = "forest_plots"
        ),
        
        menuSubItem(
          text = "Estimates",
          tabName = "cs_results"
        ) ,
        
        menuSubItem(
          text = "Temporal Sequence Plots",
          tabName = "ts_plots"
        ),
        menuSubItem(
          text = "Sensitivity Analysis",
          tabName = "sens_atc"
        )
        
      ),
      
      menuItem(
        text = "Ingredient Cascades",
        tabName = "cs_results_ing",
        icon = shiny::icon("star-half-stroke") ,
        
        menuSubItem(
          text = "Plots",
          tabName = "cs_plots_ing"
        ),
        
        menuSubItem(
          text = "Estimates",
          tabName = "cs_results_ing"
        ) ,
        
        menuSubItem(
          text = "Temporal Sequence Plots",
          tabName = "ts_ing_plots"
        ) ,
        menuSubItem(
          text = "Sensitivity Analysis",
          tabName = "sens_ing"
        )
        
      ),
      
      menuItem(
        text = "Attrition",
        tabName = "cs",
        icon = shiny::icon("arrow-down-wide-short") ,
        
      
      menuSubItem(
        text = "Index-Marker Attrition",
        tabName = "im_attrition"
      )
      
      ),
      
      menuItem(
        text = "Settings",
        icon = shiny::icon("screwdriver-wrench") ,
      menuSubItem(
        text = "Marker Settings",
        tabName = "study_settings"
      )
      
    ),
    
    # Logo HDS
    tags$div(
      style = "position: relative; margin-top: 20px; text-align: center; margin-bottom: 0;",
      a(img(
        src = "EHDEN_Logo_JPG.jpg",  # Replace with the correct file name and extension
        height = "60px",  # Adjust the height as needed
        width = "auto"     # Let the width adjust proportionally
      ),
      href = "https://www.ehden.eu/",
      target = "_blank"
      )
    ) ,
      
    
    tags$div(
      style = "position: relative; margin-top: 20px; text-align: center; margin-bottom: 0;",
      a(img(
        src = "Logo_HDS.png",  # Replace with the correct file name and extension
        height = "150px",  # Adjust the height as needed
        width = "auto"     # Let the width adjust proportionally
      ),
      href = "https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology",
      target = "_blank"
      )
    ) ,
    
    # Logo 
    tags$div(
      style = "position: relative; margin-top: -20px; text-align: center; margin-bottom: 0;",
      a(img(
        src = "logoOxford.png",  # Replace with the correct file name and extension
        height = "150px",  # Adjust the height as needed
        width = "auto"     # Let the width adjust proportionally
      ),
      href = "https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology",
      target = "_blank"
      )
    )
    
    
    )
  ),
  
  ## body ----
  dashboardBody(
    
    use_theme(mytheme),
    
    tabItems(
      # background  ------
      tabItem(
        tabName = "background",
        h3("Using Prescription Sequence Symmetry Analysis to Detect Prescription Cascades of Dementia Medications"),
        tags$h4(tags$strong("Please note, the results presented here should be considered as
                                                preliminary and subject to change.")),
        
        tags$h5(
          tags$span("Background:", style = "font-weight: bold;"),
          "There is no cure for dementia and approved drugs have shown inconclusive effectiveness and numerous side effects. The side effects of dementia drug treatments can sometimes be misinterpreted as new medical conditions and additional drugs are prescribed which leads to unnecessary drug therapy leading to a prescription cascade. Persons living with dementia are more vulnerable to prescribing cascades due to increased multimorbidity. In-depth characterization of those on dementia drugs will give crucial insights into the management of disease to healthcare providers, regulatory authorities and patients. The aim of this project is to characterize the treatments of patients taking dementia drugs and determine potential prescription cascades"
        ),

        tags$h5(
          tags$span(" Methods:", style = "font-weight: bold;"),
          "We carried out Prescription Sequence Symmetry Analysis (PSSA) using the CohortSymmetry R package. Index drugs were Acetylcholinesterase inhibitors (AChE inhibitors) (donepezil, rivastigmine, and galantamine).
          Marker drugs were all ATC (4th level) drug classes. We tested two positive controls (Amiodarone > levothyroxine and AChE inhibitorsr > Memantine) and one negative control (Amiodarone > Allopurinol). 
          Both crude and adjusted sequence ratio's were calculated as well as temporal sequence of all index marker pairs. We also ran all drug ingredients in addition to see which individual drug ingredients were driving potential signals. We carried out this study using primary care records from the United Kingdom (CPRD GOLD) and THIN. The study period from 1st Jan 2002 to 1st Jan 2022 for CPRD GOLD with different start dates for the THIN databases. Those patients 18 years and older with at least one year of prior history who initiated both the index and marker within 180 days (365 days as sensitivity analysis) were included in the study"
          ),
        
        tags$h5(
          tags$span(" Results:", style = "font-weight: bold;"),
          "These results will need to be checked by those with clinical and pharmacological expertise and assumptions of self controlled case series design will need to be checked"
          
        ),
        
        tags$h5(
          tags$span("Funding:" , style = "font-weight: bold;"),
          "This research was funded by the European Health Data and Evidence Network (EHDEN) (grant number 806968), and the Oxford NIHR Biomedical Research Centre."
        ),
        
        tags$h5("The results of this study are published in the following journal:"
        ),
        tags$ol(
          tags$li(strong("TBC"),"(",tags$a(href="https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology","Paper Link"),")" )),
        
        tags$h5("The analysis code used to generate these results can be found",
                tags$a(href="https://github.com/oxford-pharmacoepi/", "here")
                
        ),
        
        tags$h5("Any questions regarding this shiny app please contact",
                tags$a(href="mailto:danielle.newby@ndorms.ox.ac.uk", "Danielle Newby"),
                "and any questions regarding this study please contact the corresponding author",
                tags$a(href="mailto:daniel.prietoalhambra@ndorms.ox.ac.uk", "Professor Daniel Prieto Alhambra"),
                "This study was developed using the",
                tags$a(href="https://oxford-pharmacoepi.github.io/CohortSymmetry/", "CohortSymmetry"),
                "R package. Questions regarding this package please contact",
                tags$a(href="mailto:xihang.chen@ndorms.ox.ac.uk", "Xihang Chen")

        
      ),
      
      tags$hr()
      
      ),
      
      # cdm snapshot ------
      tabItem(
        tags$h5("Snapshot of the cdm from database"),
        tabName = "snapshotcdm",
        htmlOutput('tbl_cdm_snaphot'),
        tags$hr(),
        div(
          style = "display:inline-block",
          downloadButton(
            outputId = "gt_cdm_snaphot_word",
            label = "Download table as word"
          ),
          style = "display:inline-block; float:right"
        )
      
      ),
      
      
      tabItem(
        tabName = "atc_concepts",
        tags$h5("These are the concept ids with a description that were used for each ATC class."),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "atc_concepts_database_name_selector",
            label = "Database",
            choices = unique(atc_concepts$cdm_name),
            selected = unique(atc_concepts$cdm_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "atc_concepts_vocab_selector",
            label = "Vocabulary",
            choices = unique(atc_concepts$vocabulary_id),
            selected = unique(atc_concepts$vocabulary_id),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "atc_concept_cohort_name_selector",
            label = "Index Marker Name",
            choices = unique(atc_concepts$ATC_class),
            selected = unique(atc_concepts$ATC_class)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3",  liveSearch = TRUE ),
            multiple = TRUE,
           
          )
        ),
        
        
        htmlOutput('tbl_atc_concepts'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_atc_concepts_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")
        
      )  ,
      
      
      

      
      tabItem(
        tabName = "cs_results",
        tags$h5("Results from PSSA analysis showing crude and adjusted sequence ratio's as well as percentages of order of index and marker."),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "atc_ssa_database_name_selector",
            label = "Database",
            choices = unique(atc_ssa$`Database name`),
            selected = unique(atc_ssa$`Database name`)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "atc_ssa_signal_selector",
            label = "Signal Type",
            choices = unique(atc_ssa$signal),
            selected = "Positive",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "atc_ssa_cohort_name_selector",
            label = "Index Marker Name",
            choices = unique(atc_ssa$index_marker_name),
            selected = unique(atc_ssa$index_marker_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        

        htmlOutput('tbl_atc_ssa'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_atc_ssa_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")
        
      )  ,
      
      tabItem(
        tabName = "cs_results_ing",
        tags$h5("Results from PSSA analysis showing crude and adjusted sequence ratio's as well as percentages of order of index and marker for drug ingredients from positive signals for drug ATC classes."),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "ing_ssa_database_name_selector",
            label = "Database",
            choices = unique(atc_ssa_ing$`Database name`),
            selected = unique(atc_ssa_ing$`Database name`)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "ing_ssa_signal_selector",
            label = "Signal Type",
            choices = unique(atc_ssa_ing$signal),
            selected = "Positive",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "ing_ssa_cohort_name_selector",
            label = "Index Marker Name",
            choices = unique(atc_ssa_ing$index_marker_name),
            selected = unique(atc_ssa_ing$index_marker_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        htmlOutput('tbl_ing_ssa'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_ing_ssa_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")
        
      )  ,
      
      tabItem(
        tabName = "study_settings",
        
        tags$h5("Settings for index-markers used for PSSA analysis"),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "settings_database_name_selector",
            label = "Database",
            choices = unique(im_settings$cdm_name),
            selected = unique(im_settings$cdm_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "settings_cohort_selector",
            label = "Index Marker Name",
            choices = unique(im_settings$cohort_name),
            selected = c(
              "index_amiodarone_marker_levothyroxine",
              "index_amiodarone_marker_allopurinol",
              "index_ache_inhibitors_marker_memantine"
            )
              ,
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "settings_marker_type_selector",
            label = "Marker Type",
            choices = unique(im_settings$marker_type),
            selected = c("Positive Control" ,
                         "Negative Control"),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        
        
        
        
        
        htmlOutput('tbl_im_settings'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_im_settings_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")
        
      )  ,
      
      
      # im attrition
      
      tabItem(
        tabName = "im_attrition",
        tags$h5("Attrition for index-markers for PSSA study"),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "im_attrition_database_name_selector",
            label = "Database",
            choices = unique(im_attrition$cdm_name),
            selected = unique(im_attrition$cdm_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "im_attrition_cohort_name_selector",
            label = "Index-Marker Name",
            choices = unique(im_attrition$cohort_name),
            selected = c("amiodarone_levothyroxine",
                         "amiodarone_allopurinol" ,
                         "ache_inhibitors_memantine"),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "im_attrition_marker_type_selector",
            label = "Marker Type",
            choices = unique(im_attrition$marker_type),
            selected = c("Positive Control", "Negative Control"),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        
        htmlOutput('tbl_im_attrition'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_im_attrition_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")
        
      )  ,
      
      tabItem(
        tabName = "ts_estimates",
        tags$h5("Estimates counts for temporal sequence symmetry crude plots to assess asymmetry."),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "im_temporal_database_name_selector",
            label = "Database",
            choices = unique(im_temporal$cdm_name),
            selected = unique(im_temporal$cdm_name)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "im_temporal_cohort_selector",
            label = "Index Marker Name",
            choices = unique(im_temporal$group_level),
            selected = c(
              "amiodarone &&& allopurinol" ,
              "ache_inhibitors &&& memantine" ,
              "amiodarone &&& levothyroxine" 
            ),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "im_temporal_time_selector",
            label = "Time",
            choices = unique(im_temporal$timescale),
            selected = unique(im_temporal$timescale)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        htmlOutput('tbl_im_temporal'),
        
        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_im_temporal_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")
        
      ) ,
      
      
      #temporal sequence plot atc class
      
      tabItem(
        tabName = "ts_plots",
        tags$h5("Temporal sequence symmetry crude plots to assess asymmetry. If there is no signal we would expect plots to be symmetrical. If there is a potential signal we might expect more counts of the marker after the index (dashed line)"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "ts_plot_database_selector",
            label = "Database",
            choices = unique(im_temporal_test$cdm_name),
            selected = unique(im_temporal_test$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),

        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "ts_plot_marker_selector",
            label = "Index Marker Pair",
            choices = unique(im_temporal_test$group_level),
            selected = c(
              
              "Amiodarone &&& Levothyroxine",
              "Amiodarone &&& Allopurinol",
              "AChE Inhibitors &&& Memantine"
              
            ),
              
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "ts_plot_time_selector",
            label = "Timescale",
            choices = unique(im_temporal_test$timescale),
            selected = "month",
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = FALSE
          )
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "ts_plot_facet",
                        label = "Facet by",
                        choices = c("cdm_name", 
                                    "group_level"),
                        selected = c("group_level"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
        ),
        
        
        div(
          style = "width: 80%; height: 90%;",  # Set width to 100% for responsive design
          plotOutput("ts_plot",
                     height = "800px"
          ) %>%
            withSpinner(),
          h4("Download Figure"),
          div("Height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("ts_plot_download_height", "", 30, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("Width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("ts_plot_download_width", "", 35, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block; margin-right:",
            textInput("ts_plot_download_dpi", "", 600, width = "50px")
          ),
          downloadButton("ts_plot_download_plot", "Download plot")
        )
        
        
      ) ,
      
      
      
      tabItem(
        tabName = "ts_ing_plots",
        tags$h5("Temporal sequence symmetry crude plots to assess asymmetry. If there is no signal we would expect plots to be symmetrical. If there is a potential signal we might expect more counts of the marker after the index (dashed line)"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "ts_ing_plot_database_selector",
            label = "Database",
            choices = unique(im_temporal_test_ing$cdm_name),
            selected = unique(im_temporal_test_ing$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "ts_ing_plot_marker_selector",
            label = "Index Marker Pair",
            choices = unique(im_temporal_test_ing$group_level),
            selected = unique(im_temporal_test_ing$group_level)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "ts_ing_plot_facet",
                        label = "Facet by",
                        choices = c("cdm_name", 
                                    "group_level"),
                        selected = c("group_level"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
        ),
        
        
        div(
          style = "width: 80%; height: 90%;",  # Set width to 100% for responsive design
          plotOutput("ts_ing_plot",
                     height = "800px"
          ) %>%
            withSpinner(),
          h4("Download Figure"),
          div("Height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("ts_ing_plot_download_height", "", 30, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("Width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("ts_ing_plot_download_width", "", 35, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block; margin-right:",
            textInput("ts_ing_plot_download_dpi", "", 600, width = "50px")
          ),
          downloadButton("ts_ing_plot_download_plot", "Download plot")
        )
        
        
      ) ,
      

      tabItem(
        tabName = "forest_plots",
        
        div(
          style = "width: 80%; height: 90%; margin: auto;",
          withSpinner(
            plotOutput("forestPlot_atc", height = "800px")
          )
        )
          
        
      ) ,
      
      
      tabItem(
        tabName = "cs_plots_ing",
        
        div(
          style = "width: 80%; height: 90%; margin: auto;",
          withSpinner(
            plotOutput("forestPlot_ing", height = "800px")
          )
        )
        
        
      ) ,
      
      
      tabItem(
        tabName = "sens_atc",
        
        div(
          style = "width: 80%; height: 90%; margin: auto;",
          withSpinner(
            plotOutput("sensitivityPlot", height = "800px")
          )
        )
        
        
      ) ,
      
      #pssa plots atc class
      tabItem(
        tabName = "cs_plots",
        tags$h5("Results from PSSA analysis grouped by ATC class level 1. Larger darker dots indicate a positive signal between index and marker i.e. more patients are prescribed these marker drugs after the initiation of the index drugs. Hover over dots to see ATC class drug level 4."),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "pssa_database_selector",
            label = "Database",
            choices = unique(atc_ssa$`Database name`),
            selected = unique(atc_ssa$`Database name`)[1],
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "pssa_firstatc_level_selector",
            label = "ATC first level",
            choices = sort(unique(atc_ssa$first_level)),  # Sort the levels alphabetically
            selected = sort(unique(atc_ssa$first_level)),  # Also sort the selected values
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(inputId = "pssa_plot_facet",
                        label = "Facet by",
                        choices = c("cdm_name", 
                                    "first_level"),
                        selected = c("first_level"),
                        options = list(
                          `actions-box` = TRUE,
                          size = 10,
                          `selected-text-format` = "count > 3"),
                        multiple = TRUE,)
        ),
        
        
        div(
          style = "width: 80%; height: 90%;",  # Set width to 100% for responsive design
          plotlyOutput("pssaPlot", 
                       height = "800px")  %>%
            withSpinner(),
          
          
          
          
          h4("Download Figure"),
          div("Height:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("pssa_plot_download_height", "", 15, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("Width:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block;",
            textInput("pssa_plot_download_width", "", 35, width = "50px")
          ),
          div("cm", style = "display: inline-block; margin-right: 25px;"),
          div("dpi:", style = "display: inline-block; font-weight: bold; margin-right: 5px;"),
          div(
            style = "display: inline-block; margin-right:",
            textInput("pssa_plot_download_dpi", "", 600, width = "50px")
          ),
          
          div(style="display:inline-block",
              downloadButton(
                outputId = "pssa_download_plot",
                label = "Download plot"
              ),
              style="display:inline-block")
          
          
        )
          
        
      ) ,
      
      
      
      

      tabItem(
        tabName = "demographics",
        tags$h5("Demographics for eligible patients with prescription for Acetylcholinesterase inhibitors (AChEIs)"),

        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "demographics_database_name_selector",
            label = "Database",
            choices = unique(demo_characteristics$cdm_name),
            selected = unique(demo_characteristics$cdm_name),
            options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
            multiple = TRUE
          )
        ),



       # tags$hr(),
        gt_output("gt_demo_characteristics") %>%
          withSpinner() ,


        div(style="display:inline-block",
            downloadButton(
              outputId = "gt_demo_characteristics_word",
              label = "Download table as word"
            ),
            style="display:inline-block; float:right")

      ) ,

    tabItem(
      tabName = "comorbidities",
      tags$h5("Comorbidities any time prior for eligible patients with prescription for Acetylcholinesterase inhibitors (AChEIs)"),

      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        pickerInput(
          inputId = "comorb_database_name_selector",
          label = "Database",
          choices = unique(comorb_characteristics$cdm_name),
          selected = unique(comorb_characteristics$cdm_name),
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
          multiple = TRUE
        )
      ),


      # tags$hr(),
      gt_output("gt_comorb_characteristics") %>%
        withSpinner() ,


      div(style="display:inline-block",
          downloadButton(
            outputId = "gt_comorb_characteristics_word",
            label = "Download table as word"
          ),
          style="display:inline-block; float:right")

    ) ,

    tabItem(
      tabName = "medications",
      tags$h5("Medications up to 1 year prior for eligible patients with prescription for Acetylcholinesterase inhibitors (AChEIs)"),

      div(
        style = "display: inline-block;vertical-align:top; width: 150px;",
        pickerInput(
          inputId = "med_database_name_selector",
          label = "Database",
          choices = unique(med_characteristics$cdm_name),
          selected = unique(med_characteristics$cdm_name),
          options = list(`actions-box` = TRUE, size = 10, `selected-text-format` = "count > 3"),
          multiple = TRUE
        )
      ),


      # tags$hr(),
      gt_output("gt_med_characteristics") %>%
        withSpinner() ,


      div(style="display:inline-block",
          downloadButton(
            outputId = "gt_med_characteristics_word",
            label = "Download table as word"
          ),
          style="display:inline-block; float:right")

    ) 

  
  
)

)

)
