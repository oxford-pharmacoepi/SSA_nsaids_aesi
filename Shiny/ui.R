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
        )
      ),
     
      menuItem(
        text = "Characteristics",
        tabName = "char",
        icon = shiny::icon("hospital-user"),
        menuSubItem(
          text = "Demographics",
          tabName = "demographics"
        )
      ),
      
      
      menuItem(
        text = "Overall Analysis",
        tabName = "cs",
        icon = shiny::icon("star-half-stroke") ,
        
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
        text = "Sex Stratifed Analysis",
        tabName = "cs_sex",
        icon = shiny::icon("star-half-stroke") ,
        
        menuSubItem(
          text = "Forest plot",
          tabName = "forest_plots_sex"
        ),
        
        menuSubItem(
          text = "Estimates",
          tabName = "cs_results_sex"
        ) ,
        
        menuSubItem(
          text = "Temporal Sequence Plots",
          tabName = "ts_plots_sex"
        ),
        menuSubItem(
          text = "Sensitivity Analysis",
          tabName = "sens_sex"
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
        src = "uab.svg",  # Replace with the correct file name and extension
        height = "60px",  # Adjust the height as needed
        width = "auto"     # Let the width adjust proportionally
      ),
      href = "https://www.uab.edu/home/",
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
        h3("Determining Cardiovascular Safety Signals of Non-Steroidal Anti-Inflammatory Drugs: A Sequence Symmetry Study"),
        tags$h4(tags$strong("Please note, the results presented here should be considered as
                                                preliminary and subject to change.")),
        
        tags$h5(
          tags$span("Background:", style = "font-weight: bold;"),
          "TBC"
        ),

        tags$h5(
          tags$span(" Methods:", style = "font-weight: bold;"),
          "TBC"
          ),
        
        tags$h5(
          tags$span(" Results:", style = "font-weight: bold;"),
          "TBC"
          
        ),
        
        tags$h5(
          tags$span("Funding:" , style = "font-weight: bold;"),
          "This research was funded by XXX, and the Oxford NIHR Biomedical Research Centre."
        ),
        
        tags$h5("The results of this study are published in the following journal:"
        ),
        tags$ol(
          tags$li(strong("TBC"),"(",tags$a(href="https://www.ndorms.ox.ac.uk/research/research-groups/Musculoskeletal-Pharmacoepidemiology","Paper Link"),")" )),
        
        tags$h5("The analysis code used to generate these results can be found",
                tags$a(href="https://github.com/oxford-pharmacoepi/SSA_nsaids_aesi", "here")
                
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
      
      
      #temporal sequence plot
      
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
        tabName = "forest_plots",
        
        div(
          style = "width: 80%; height: 90%; margin: auto;",
          withSpinner(
            plotOutput("forestPlot_atc", height = "800px")
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

      )  

  
  
)

)

)
