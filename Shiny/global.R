#### PACKAGES -----
renv::restore()

library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(readr)
library(here)
library(stringr)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(gt)
library(scales)
library(kableExtra)
library(tidyr)
library(stringr)
library(ggplot2)
library(fresh)
library(plotly)
library(bslib)
library(PatientProfiles)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(CirceR)
library(rclipboard)
library(CodelistGenerator)
library(CohortSymmetry)
library(plotly)
library(forestploter)
library(grid)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#A35FA6"
  ),
  adminlte_sidebar(
    dark_bg = "#A35FA6",  # Use your color for sidebar background
    dark_hover_bg = "#3B9AB2",  # Keep this as is or adjust
    dark_color = "white",  # Keep this as is
    dark_submenu_bg = "#58B1C8"  # Keep this as is or adjust
  ),
  adminlte_global(
    content_bg = "#eaebea"  # Keep this as is or adjust
  ),
  adminlte_vars(
    border_color = "black",  # Keep this as is
    active_link_hover_bg = "#A35FA6",  # Use your color for hover background
    active_link_hover_color = "white",  # Change to white for contrast
    active_link_hover_border_color = "#A35FA6",  # Use your color for border
    link_hover_border_color = "#A35FA6",  # Use your color for border
    table_border_color = "black"  # Keep this as is
  )
)



# Data prep functions -----
# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
# printing numbers with 4 decimal place and commas 
nice.num4<-function(x) {
  trimws(format(round(x,4),
                big.mark=",", nsmall = 4, digits=4, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}

# format markdown
formatMarkdown <- function(x) {
  lines <- strsplit(x, "\r\n\r\n") |> unlist()
  getFormat <- function(line) {
    if (grepl("###", line)) {return(h3(gsub("###", "", line)))} 
    else {h4(line)} 
  }
  purrr::map(lines, ~ getFormat(.))
}


# Load, prepare, and merge results -----
results <-list.files(here("data"), full.names = TRUE,
                     recursive = TRUE,
                     include.dirs = TRUE,
                     pattern = ".zip")

#unzip data
for (i in (1:length(results))) {
  utils::unzip(zipfile = results[[i]],
               exdir = here("data"))
}

#grab the results from the folders
results <- list.files(
  path = here("data"),
  pattern = ".csv",
  full.names = TRUE,
  recursive = TRUE,
  include.dirs = TRUE
)

# cdm snapshot ------
snapshot_files <- results[stringr::str_detect(results, ".csv")]
snapshot_files <- results[stringr::str_detect(results, "cdm_snapshot")]
snapshotcdm <- list()
for(i in seq_along(snapshot_files)){
  snapshotcdm[[i]] <- readr::read_csv(snapshot_files[[i]],
                                      show_col_types = FALSE) %>% 
    mutate_all(as.character)
  
}

snapshotcdm <- bind_rows(snapshotcdm) %>% 
  select("cdm_name", "person_count", "observation_period_count" ,
         "vocabulary_version", "cdm_version", "cdm_description",) %>% 
  mutate(person_count = nice.num.count(person_count), 
         observation_period_count = nice.num.count(observation_period_count)) %>% 
  mutate(cdm_name = str_replace_all(cdm_name, "_", " ")) %>% 
  rename("Database name" = "cdm_name",
         "Persons in the database" = "person_count",
         "Number of observation periods" = "observation_period_count",
         "OMOP CDM vocabulary version" = "vocabulary_version",
         "Database CDM Version" = "cdm_version",
         "Database Description" = "cdm_description" )  


# pssa class settings ------
im_settings_files <- results[stringr::str_detect(results, ".csv")]
im_settings_files <- results[stringr::str_detect(results, "ssa_marker_settings")]
im_settings <- list()

for(i in seq_along(im_settings_files)){
  im_settings[[i]] <- readr::read_csv(im_settings_files[[i]],
                                      show_col_types = FALSE)
  
}

im_settings <- dplyr::bind_rows(im_settings) %>% 
  select(c(
    cohort_name    ,
    index_name       ,
    marker_name,
    days_prior_observation,
    cohort_date_range,
    moving_average_restriction,
    washout_window,
    index_marker_gap,
    combination_window,
    nsr,
    marker_type,
    cdm_name
    
  )) %>% 
  mutate(marker_name = toupper(marker_name))


# pssa results OVERALL ------
# pssa results for all markers and controls
atc_ssa_files <- results[stringr::str_detect(results, ".csv")]
atc_ssa_files <- results[
  stringr::str_detect(results, "ssa_estimates") &
    !stringr::str_detect(results, "365_window")
]

atc_ssa <- list()
for(i in seq_along(atc_ssa_files)){
  
  atc_ssa[[i]] <- omopgenerics::importSummarisedResult(atc_ssa_files[[i]])
  
}

# bind the results for the class result
atc_ssa <- omopgenerics::bind(atc_ssa) %>% 
  visOmopResults::visOmopTable(
    estimateName = c("N (%)" = "<count> (<percentage>%)",
                     "SR [CI 99%]" = "<point_estimate> [<lower_CI> - <upper_CI>]"),
    header = c("Variable name", "Estimate name"),
    rename = c("Database name" = "cdm_name"),
    groupColumn = "cdm_name",
    type = "tibble",
    hide = "variable_level"
  ) %>% 
  rename_with(
    ~ if_else(str_detect(., "crude"), "CSR (99% CI)", .) ) %>%
  rename_with(
    ~ if_else(str_detect(., "adjusted"), "ASR (99% CI)", .) ) %>% 
  rename_with(
    ~ if_else(str_detect(., "index"), "Index N (%)", .)) %>% 
  rename_with(
    ~ if_else(str_detect(., "marker"), "Marker N (%)", .)) %>% 
  mutate(
    `CSR (99% CI)` = if_else(
      `CSR (99% CI)` == "Inf [Inf - Inf]" | `Index N (%)` == "<5" | `Marker N (%)` == "<5",
      NA_character_,
      `CSR (99% CI)`
    ),
    `ASR (99% CI)` = if_else(
      `ASR (99% CI)` == "Inf [Inf - Inf]" | `Index N (%)` == "<5" | `Marker N (%)` == "<5",
      NA_character_,
      `ASR (99% CI)`
    )
  ) %>%
  extract(`ASR (99% CI)`, into = c("asr", "asr_lower", "asr_upper"),
          regex = "(\\d+\\.?\\d*) \\[\\s*(\\d+\\.?\\d*)\\s*-\\s*(\\d+\\.?\\d*)\\s*\\]",
          convert = TRUE, remove = FALSE) %>%
  mutate(signal = case_when(
    asr_lower > 1 ~ "Positive",    # If the lower ASR is greater than 1
    asr_upper < 1 ~ "Negative", 
    asr == 0 ~ NA_character_,
    is.na(asr) ~ NA_character_,
    `Index N (%)` == "<5" ~ NA_character_,
    `Marker N (%)` == "<5" ~ NA_character_,
    TRUE ~ "Null"                  # All other cases
  )) %>%
  mutate(
    `Index cohort name` = str_replace(`Index cohort name`, "ache_inhibitors", "AChE Inhibitors"),
    `Marker cohort name` = toupper(`Marker cohort name`)) %>% 
  mutate(index_marker_name = paste0(`Index cohort name`, "_", `Marker cohort name`)) %>% 
  left_join(select(atc_class, fourth_level, first_level, ATC_Class), 
            by = c("Marker cohort name" = "fourth_level")) %>% 
  left_join(select(im_settings, marker_name, nsr), 
            by = c("Marker cohort name" = "marker_name")) %>% 
  rename("NSR" = "nsr") %>%
  mutate(NSR = round(NSR, 3))


# read in estimates from sensitivity analysis (365 windows)
atc_ssa_sens_files <- results[stringr::str_detect(results, ".csv")]
atc_ssa_sens_files <- results[stringr::str_detect(results, "ssa_estimates_365_window")]

atc_ssa_sens <- list()
for(i in seq_along(atc_ssa_sens_files)){
  
  atc_ssa_sens[[i]] <- omopgenerics::importSummarisedResult(atc_ssa_sens_files[[i]])
  
}

atc_ssa_sens <- omopgenerics::bind(atc_ssa_sens) %>% 
  visOmopResults::visOmopTable(
    estimateName = c("N (%)" = "<count> (<percentage>%)",
                     "SR [CI 99%]" = "<point_estimate> [<lower_CI> - <upper_CI>]"),
    header = c("Variable name", "Estimate name"),
    rename = c("Database name" = "cdm_name"),
    groupColumn = "cdm_name",
    type = "tibble",
    hide = "variable_level"
  ) %>% 
  rename_with(
    ~ if_else(str_detect(., "crude"), "CSR (99% CI)", .) ) %>%
  rename_with(
    ~ if_else(str_detect(., "adjusted"), "ASR (99% CI)", .) ) %>% 
  rename_with(
    ~ if_else(str_detect(., "index"), "Index N (%)", .)) %>% 
  rename_with(
    ~ if_else(str_detect(., "marker"), "Marker N (%)", .)) %>% 
  mutate(
    `CSR (99% CI)` = if_else(
      `CSR (99% CI)` == "Inf [Inf - Inf]" | `Index N (%)` == "<5" | `Marker N (%)` == "<5",
      NA_character_,
      `CSR (99% CI)`
    ),
    `ASR (99% CI)` = if_else(
      `ASR (99% CI)` == "Inf [Inf - Inf]" | `Index N (%)` == "<5" | `Marker N (%)` == "<5",
      NA_character_,
      `ASR (99% CI)`
    )
  ) %>%
  extract(`ASR (99% CI)`, into = c("asr", "asr_lower", "asr_upper"),
          regex = "(\\d+\\.?\\d*) \\[\\s*(\\d+\\.?\\d*)\\s*-\\s*(\\d+\\.?\\d*)\\s*\\]",
          convert = TRUE, remove = FALSE) %>%
  mutate(signal = case_when(
    asr_lower > 1 ~ "Positive",    # If the lower ASR is greater than 1
    asr_upper < 1 ~ "Negative", 
    asr == 0 ~ NA_character_,
    is.na(asr) ~ NA_character_,
    `Index N (%)` == "<5" ~ NA_character_,
    `Marker N (%)` == "<5" ~ NA_character_,
    TRUE ~ "Null"                  # All other cases
  )) %>%
  mutate(
    `Index cohort name` = str_replace(`Index cohort name`, "ache_inhibitors", "AChE Inhibitors"),
    `Marker cohort name` = toupper(`Marker cohort name`)) %>% 
  mutate(index_marker_name = paste0(`Index cohort name`, "_", `Marker cohort name`)) %>% 
  left_join(select(atc_class, fourth_level, first_level, ATC_Class), 
            by = c("Marker cohort name" = "fourth_level"))

atc_ssa_sens1 <- atc_ssa_sens %>% 
  select(c(
    "Database name" ,
    "Marker cohort name",
    "ASR (99% CI)"    ,
    "asr" ,
    "asr_lower"    ,
    "asr_upper"  ,
    "signal"
  )) %>% 
  mutate(window = 365)

atc_ssa1 <- atc_ssa %>% 
  select(c(
    "Database name" ,
    "Marker cohort name",
    "ASR (99% CI)"    ,
    "asr" ,
    "asr_lower"    ,
    "asr_upper"  ,
    "signal"
  )) %>% 
  mutate(window = 180)


combined_atc <- bind_rows(atc_ssa_sens1, atc_ssa1)

# Step 2: Pivot wider using the 'window' value as suffix
# Step 2: Pivot wider using the 'window' value as suffix
wide_atc <- combined_atc %>%
  pivot_wider(
    id_cols = c(`Marker cohort name`, `Database name`),
    names_from = window,
    values_from = c(asr, asr_lower, asr_upper, `ASR (99% CI)`, signal),
    names_glue = "{.value}_{window}"
  ) %>% 
  filter(!(is.na(signal_180) & is.na(signal_365))) %>% 
  filter(signal_180 == "Positive" | signal_365 == "Positive") %>% 
  filter(`Marker cohort name` != "MEMANTINE") %>% 
  filter(`Marker cohort name` != "LEVOTHYROXINE")



# attrition index - markers ----
im_attrition_files <- results[stringr::str_detect(results, ".csv")]
im_attrition_files <- results[stringr::str_detect(results, "attrition")]
im_attrition <- list()
for(i in seq_along(im_attrition_files)){
  im_attrition[[i]] <- readr::read_csv(im_attrition_files[[i]],
                                  show_col_types = FALSE)
  
}

im_attrition <- dplyr::bind_rows(im_attrition) %>% 
  select(-c(cohort_definition_id)) %>% 
  relocate(cohort_name)


# temporal symmetry class -----
im_temporal_files <- results[stringr::str_detect(results, ".csv")]
im_temporal_files <- results[stringr::str_detect(results, "temporal_symmetry_summary")]
im_temporal <- list()
for(i in seq_along(im_temporal_files)){
  im_temporal[[i]] <- readr::read_csv(im_temporal_files[[i]],
                                       show_col_types = FALSE)
  
}

im_temporal <- dplyr::bind_rows(im_temporal)

im_temporal_test <- im_temporal %>%
  mutate(
    group_level = str_replace(group_level, "ache_inhibitors", "AChE Inhibitors"),
    group_level = str_replace(group_level, "amiodarone", "Amiodarone"),
    group_level = str_replace(group_level, "(?<=&&& )memantine$", "Memantine"),
    group_level = str_replace(group_level, "(?<=&&& )allopurinol$", "Allopurinol"),
    group_level = str_replace(group_level, "(?<=&&& )levothyroxine$", "Levothyroxine"),
    group_level = str_replace(group_level, "(?<=&&& )[a-z0-9]+$", function(x) toupper(x))
  )


#tweaking plotting function
plotTemporalSymmetry1 <- function(result,
                                 plotTitle = NULL,
                                  labs = c("Time (months)", "Individuals (N)"),
                                  xlim = c(-12, 12),
                                  colours = c("blue", "red"),
                                  scales = "free") {

  
   plot_data <- result |>
     omopgenerics::splitGroup() |>
     dplyr::select(.data$index_name, .data$marker_name, .data$variable_name, .data$variable_level, .data$estimate_name, .data$estimate_value, .data$additional_level, .data$additional_name) |>
     dplyr::group_by(.data$estimate_name) |>
     dplyr::mutate(row = dplyr::row_number()) |>
     tidyr::pivot_wider(names_from = "variable_name",
                        values_from = "variable_level") |>
     tidyr::pivot_wider(names_from = "estimate_name",
                        values_from = "estimate_value") |>
     dplyr::select(-"row") |>
     dplyr::ungroup() |>
     dplyr::rename("time" = "temporal_symmetry") |>
     dplyr::filter(.data$time != 0) |>
     dplyr::mutate(colour = dplyr::if_else(.data$time > 0, "B", "A")) |>
     dplyr::mutate(index_name = paste0("Index: ", .data$index_name),
                   marker_name = paste0("Marker: ", .data$marker_name)) |>
     dplyr::mutate(count = as.integer(.data$count),
                   time = as.integer(.data$time)) |>
     dplyr::compute()
   
   colours = c("A" = colours[1], "B" = colours[2])
   
   width_range <- (xlim[2] - xlim[1])/2
   
   timescale_breaks <- if (grepl("months", labs[1], ignore.case = TRUE)) {
     seq(xlim[1], xlim[2], by = 1)
   } else if (grepl("days", labs[1], ignore.case = TRUE)) {
     seq(xlim[1], xlim[2], by = 52)
   } else {
     seq(xlim[1], xlim[2], by = 1)  # Default fallback
   }
   
   ggplot2::ggplot(data = plot_data, ggplot2::aes(
     x = .data$time, y = .data$count, fill = .data$colour)) +
     ggplot2::geom_col(width = 0.01*width_range) +
     ggplot2::geom_point(ggplot2::aes(colour = .data$colour), size = 4) +
     ggplot2::coord_cartesian(xlim = c(xlim[1], xlim[2])) +
     ggplot2::labs(title = plotTitle, x = labs[1], y = labs[2]) +
     scale_y_continuous(expand = expansion(mult = c(0, .1))) +
     ggplot2::scale_x_continuous(breaks = timescale_breaks) + 
     ggplot2::theme_minimal() +  # Use a minimal theme with a white background
     ggplot2::theme(legend.position = "none",
                    axis.line = ggplot2::element_line(colour = "black"),  # Make axis lines black
                    axis.ticks = ggplot2::element_line(colour = "black"),  # Make axis ticks black
                    axis.text = ggplot2::element_text(colour = "black", size = 20) ,   # Make axis text black
                    panel.grid.minor = ggplot2::element_blank() ,  # Remove minor grid lines
                    panel.grid.major.x = ggplot2::element_blank(),  # Remove vertical grid lines
                    panel.grid.major.y = ggplot2::element_line(colour = "grey96"),  # Keep horizontal grid lines
                    plot.title = ggplot2::element_text(hjust = 0.5),
                    strip.text = ggplot2::element_text(size = 20),  # Increase the facet strip labels' text size
                    axis.title = ggplot2::element_text(size = 20)
                    ) +
     ggplot2::facet_wrap(~ index_name + marker_name, scales = scales) +
     ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
     ggplot2::scale_fill_manual(values = colours) +
     ggplot2::scale_colour_manual(values = colours)

}



# table one demographics------
tableone_demo_files <- results[stringr::str_detect(results, ".csv")]
tableone_demo_files <- results[stringr::str_detect(results, "characteristics")]

if(length(tableone_demo_files > 0)){
  
  tableone_demo <- list()
  
  for(i in seq_along(tableone_demo_files)){
    #read in the files
    tableone_demo[[i]] <- omopgenerics::importSummarisedResult(tableone_demo_files[[i]])
    
  }
  


demo_characteristics <- omopgenerics::bind(tableone_demo) %>%
  mutate(cdm_name = str_replace_all(cdm_name, "_", " "))

rm(tableone_demo)

}


# to add -----------

#sex stratification
#age stratification
#comorbs stratification


