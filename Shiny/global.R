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
library(CohortCharacteristics)


# this changes the colour scheme of the shiny
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#007A33"  # Replace 'light_blue' with UAB green
  ),
  adminlte_sidebar(
    dark_bg = "#007A33",             # UAB Green
    dark_hover_bg = "#FDBB30",       # UAB Gold
    dark_color = "white",
    dark_submenu_bg = "#004C1D"      # Optional darker green
  ),
  adminlte_global(
    content_bg = "#f4f4f4"           # Light neutral background
  ),
  adminlte_vars(
    border_color = "black",
    active_link_hover_bg = "#007A33",
    active_link_hover_color = "white",
    active_link_hover_border_color = "#FDBB30",
    link_hover_border_color = "#FDBB30",
    table_border_color = "black"
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
  
  snapshotcdm[[i]] <- omopgenerics::importSummarisedResult(snapshot_files) 

  
}

snapshotcdm <- omopgenerics::bind(snapshotcdm) %>%
  omopgenerics::addSettings() |>
    omopgenerics::splitAll() %>% 
  select(c(cdm_name,
           estimate_name,
           estimate_value
           )) %>% 
  rename("Database" = "cdm_name",
         "Name" = "estimate_name",
         "Value" = "estimate_value" )  


# pssa settings ------
im_settings_files <- results[stringr::str_detect(results, "marker_settings")]

im_settings <- list()

for(i in seq_along(im_settings_files)){
  im_settings[[i]] <- readr::read_csv(im_settings_files[[i]],
                                      show_col_types = FALSE)
  
}

im_settings <- dplyr::bind_rows(im_settings) %>% 
  select(c(
    index_name       ,
    marker_name,
    days_prior_observation,
    cohort_date_range,
    moving_average_restriction,
    washout_window,
    index_marker_gap,
    combination_window,
    cohort_definition_id,
    nsr
  )) 


# pssa results OVERALL ------
# pssa results for all markers
ssa_estimates_files <- results[stringr::str_detect(results, "result")]
ssa_estimates_files <- ssa_estimates_files[!stringr::str_detect(ssa_estimates_files, "sex")]
ssa_estimates_files <- ssa_estimates_files[!stringr::str_detect(ssa_estimates_files, "_age")]
ssa_estimates_files <- ssa_estimates_files[!stringr::str_detect(ssa_estimates_files, "controls")]
ssa_estimates <- list()

for(i in seq_along(ssa_estimates_files)){
  
  ssa_estimates[[i]] <- omopgenerics::importSummarisedResult(ssa_estimates_files[[i]])
  
}

all_nsaids_ssa_files <- results[stringr::str_detect(results, "all_nsaids")]
all_nsaids_ssa_files <- all_nsaids_ssa_files[!stringr::str_detect(all_nsaids_ssa_files, "age_sex_sa")]

all_nsaids_estimates <- list()

for(i in seq_along(all_nsaids_ssa_files)){
  all_nsaids_estimates[[i]] <- omopgenerics::importSummarisedResult(all_nsaids_ssa_files[[i]])
}

# bind the results for the class result
ssa_estimates_bind <- omopgenerics::bind(ssa_estimates) 
all_nsaids_bind <- omopgenerics::bind(all_nsaids_estimates)

ssa_estimates_bind <- omopgenerics::bind(ssa_estimates_bind, all_nsaids_bind)

ssa_estimates <- ssa_estimates_bind %>% 
  visOmopResults::visOmopTable(
    estimateName = c("N (%)" = "<count> (<percentage>%)",
                     "SR [CI 99%]" = "<point_estimate> [<lower_CI> - <upper_CI>]"),
    header = c("variable_name", "estimate_name"),
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
  group_by(
    `CDM name`,
    `Index cohort name`,
    `Marker cohort name`,
    #`Combination window`
  ) %>%
  summarise(
    `Index N (%)` = na.omit(`Index N (%)`)[1],
    `Marker N (%)` = na.omit(`Marker N (%)`)[1],
    `CSR (99% CI)` = na.omit(`CSR (99% CI)`)[1],
    `ASR (99% CI)` = na.omit(`ASR (99% CI)`)[1],
    #`Variable level` = paste(unique(na.omit(`Variable level`)), collapse = ", "),
    .groups = "drop"
  ) %>%
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
          regex = "(\\d+\\.?\\d*)\\s*\\[\\s*(\\d+\\.?\\d*)\\s*-\\s*(\\d+\\.?\\d*)\\s*\\]", # Adjusted regex for potential spaces
          convert = TRUE, remove = FALSE) %>%
  rename("Database name" = "CDM name") %>%
  mutate(signal = case_when(
    asr_lower > 1 ~ "Positive",    # If the lower ASR is greater than 1
    asr_upper < 1 ~ "Negative", 
    asr == 0 ~ NA_character_,
    is.na(asr) ~ NA_character_,
    `Index N (%)` == "<5" ~ NA_character_,
    `Marker N (%)` == "<5" ~ NA_character_,
    TRUE ~ "Null"                  # All other cases
  )) 

# pssa results SEX ------
# pssa results for all markers
ssa_estimates_sex_files <- results[stringr::str_detect(results, ".csv")]
ssa_estimates_sex_files <- results[
  stringr::str_detect(results, "result") &
    stringr::str_detect(results, "sex") &
    !stringr::str_detect(results, "_sa_") 
]

ssa_estimates_sex <- list()

for(i in seq_along(ssa_estimates_sex_files)){
  
  ssa_estimates_sex[[i]] <- omopgenerics::importSummarisedResult(ssa_estimates_sex_files[[i]])
  
}

ssa_estimates_sex <- omopgenerics::bind(ssa_estimates_sex)

all_nsaids_sex_files <- results[
  stringr::str_detect(results, "all_nsaids") &
    stringr::str_detect(results, "_age_sex_")]

all_nsaids_sex <- list()

for(i in seq_along(all_nsaids_sex_files)){
  
  all_nsaids_sex[[i]] <- omopgenerics::importSummarisedResult(all_nsaids_sex_files[[i]])
  
}

all_nsaids_sex <- omopgenerics::bind(all_nsaids_sex) %>%
  omopgenerics::splitGroup() %>%
  dplyr::filter(!str_detect(index_cohort_name, "18_to_65")) %>%
  dplyr::filter(!str_detect(index_cohort_name, "65_and_over")) %>%
  omopgenerics::uniteGroup(c("index_cohort_name", "marker_cohort_name"))

# bind the results for the class result
ssa_estimates_sex <- omopgenerics::bind(ssa_estimates_sex, all_nsaids_sex) %>% 
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
  )) 

# # pssa results AGE ------
# # pssa results for all markers
ssa_estimates_age_files <- results[
  stringr::str_detect(results, "result") &
    stringr::str_detect(results, "_age") &
    !stringr::str_detect(results, "_sa_") 
]

ssa_estimates_age <- list()

for(i in seq_along(ssa_estimates_age_files)){

  ssa_estimates_age[[i]] <- omopgenerics::importSummarisedResult(ssa_estimates_age_files[[i]])

}

ssa_estimates_age <- omopgenerics::bind(ssa_estimates_age)

all_nsaids_age_files <- results[
  stringr::str_detect(results, "all_nsaids") &
    stringr::str_detect(results, "_age_sex_")]

all_nsaids_age <- list()

for(i in seq_along(all_nsaids_age_files)){
  
  all_nsaids_age[[i]] <- omopgenerics::importSummarisedResult(all_nsaids_age_files[[i]])
  
}

all_nsaids_age <- omopgenerics::bind(all_nsaids_age) %>%
  omopgenerics::splitGroup() %>%
  dplyr::filter(!str_detect(index_cohort_name, "male")) %>%
  dplyr::filter(!str_detect(index_cohort_name, "female")) %>%
  omopgenerics::uniteGroup(c("index_cohort_name", "marker_cohort_name"))

# bind the results for the class result
ssa_estimates_age <- omopgenerics::bind(ssa_estimates_age, all_nsaids_age) %>%
  visOmopResults::visOmopTable(
    estimateName = c("N (%)" = "<count> (<percentage>%)",
                     "SR [CI 99%]" = "<point_estimate> [<lower_CI> - <upper_CI>]"),
    header = c("variable_name", "estimate_name"),
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
  group_by(
    `CDM name`,
    `Index cohort name`,
    `Marker cohort name`,
  ) %>%
  summarise(
    `Index N (%)` = na.omit(`Index N (%)`)[1],
    `Marker N (%)` = na.omit(`Marker N (%)`)[1],
    `CSR (99% CI)` = na.omit(`CSR (99% CI)`)[1],
    `ASR (99% CI)` = na.omit(`ASR (99% CI)`)[1],
    .groups = "drop"
  ) %>%
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
          regex = "(\\d+\\.?\\d*)\\s*\\[\\s*(\\d+\\.?\\d*)\\s*-\\s*(\\d+\\.?\\d*)\\s*\\]", # Adjusted regex for potential spaces
          convert = TRUE, remove = FALSE) %>%
  rename("Database name" = "CDM name") %>%
  mutate(signal = case_when(
    asr_lower > 1 ~ "Positive",    # If the lower ASR is greater than 1
    asr_upper < 1 ~ "Negative", 
    asr == 0 ~ NA_character_,
    is.na(asr) ~ NA_character_,
    `Index N (%)` == "<5" ~ NA_character_,
    `Marker N (%)` == "<5" ~ NA_character_,
    TRUE ~ "Null"                  # All other cases
  )) 


# attrition index - markers ----
im_attrition_files <- results[stringr::str_detect(results, ".csv")]
im_attrition_files <- results[
  stringr::str_detect(results, "attrition") &
    !stringr::str_detect(results, "sex") &
    !stringr::str_detect(results, "_age")
]

im_attrition <- list()
for(i in seq_along(im_attrition_files)){
  im_attrition[[i]] <- readr::read_csv(im_attrition_files[[i]],
                                  show_col_types = FALSE)
  
}


im_attrition <- dplyr::bind_rows(im_attrition) %>%
  left_join(
    im_settings %>%
      select(cohort_definition_id, marker_name, index_name),
    by = "cohort_definition_id"
  ) %>%
  select(marker_name, index_name, everything(), -cohort_definition_id, -reason_id)




# temporal symmetry class -----
im_temporal_files <- results[stringr::str_detect(results, ".csv")]
im_temporal_files <- results[
  stringr::str_detect(results, "temporal_symmetry_summary") &
    !stringr::str_detect(results, "sex") &
    !stringr::str_detect(results, "_age")
]

im_temporal <- list()
for(i in seq_along(im_temporal_files)){
  im_temporal[[i]] <- readr::read_csv(im_temporal_files[[i]],
                                       show_col_types = FALSE)
  
}

im_temporal <- dplyr::bind_rows(im_temporal)

im_temporal_test <- im_temporal %>%
  mutate(
    group_level = str_replace(group_level, "amiodarone", "Amiodarone"),
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

### Controls
ssa_estimates_controls_files <- results[
  stringr::str_detect(results, "result") &
    stringr::str_detect(results, "control")
]

ssa_estimates_controls <- importSummarisedResult(ssa_estimates_controls_files) |>
visOmopResults::visOmopTable(
  estimateName = c("N (%)" = "<count> (<percentage>%)",
                   "SR [CI 99%]" = "<point_estimate> [<lower_CI> - <upper_CI>]"),
  header = c("variable_name", "estimate_name"),
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
  group_by(
    `CDM name`,
    `Index cohort name`,
    `Marker cohort name`,
  ) %>%
  summarise(
    `Index N (%)` = na.omit(`Index N (%)`)[1],
    `Marker N (%)` = na.omit(`Marker N (%)`)[1],
    `CSR (99% CI)` = na.omit(`CSR (99% CI)`)[1],
    `ASR (99% CI)` = na.omit(`ASR (99% CI)`)[1],
    .groups = "drop"
  ) %>%
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
          regex = "(\\d+\\.?\\d*)\\s*\\[\\s*(\\d+\\.?\\d*)\\s*-\\s*(\\d+\\.?\\d*)\\s*\\]", # Adjusted regex for potential spaces
          convert = TRUE, remove = FALSE) %>%
  rename("Database name" = "CDM name") %>%
  mutate(signal = case_when(
    asr_lower > 1 ~ "Positive",    # If the lower ASR is greater than 1
    asr_upper < 1 ~ "Negative", 
    asr == 0 ~ NA_character_,
    is.na(asr) ~ NA_character_,
    `Index N (%)` == "<5" ~ NA_character_,
    `Marker N (%)` == "<5" ~ NA_character_,
    TRUE ~ "Null"                  # All other cases
  )) 


# to add -----------
#sensitivity analysis
#comorbs stratification
