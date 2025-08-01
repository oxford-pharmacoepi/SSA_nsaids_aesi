### Figure 1

figure1_data <- ssa_estimates |>
  dplyr::filter(`Combination window` == "0, 180") |>
  dplyr::mutate(
    `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
  ) |>
  filter(asr != Inf) |>
  mutate(highlight = ifelse(asr_lower > 1, "Highlighted", "Not Highlighted")) |>
  rename(index_cohort_name = `Index cohort name`,
         marker_cohort_name = `Marker cohort name`
         #,
        #combination_window = `Combination window`
        )  |> 
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
      index_cohort_name == "All_nsaids" ~ "All NSAIDs",
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
  filter(#!index_cohort_name %in% c("All NSAIDs","Non-selective","COX-2", "Acetaminophen"),
         !marker_cohort_name %in% c("Hemorrhagic Stroke", "Ischemic Stroke", "GI Hemorrhage")) |>
  mutate(marker_cohort_name = factor(marker_cohort_name, 
                                     levels = c("Arrythmia", "Deep Vein Thrombosis",
                                                "Heart Failure", "Stroke", "Myocardial Infarction",
                                                "Pulmonary Embolism"),
                                     ordered = TRUE))


labs = c("Adjusted Sequence Ratio", "NSAID")
custom_colors <- c("adjusted" = "black")

figure1 <- ggplot(figure1_data, aes(
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
    linewidth = 1  # This controls the thickness of the error bar line
  ) +
  # Add points separately
  geom_point(
    position = position_dodge(width = 0.8),
    size = 3.5  # Controls the size of the point
  ) +
  facet_wrap(~ marker_cohort_name, ncol = 2) +
  coord_flip() +
  theme_minimal() +
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

figure1

### Figure 2

figure2_data <- ssa_estimates |>
  #dplyr::filter(`Combination window` == "0, 180") |>
  dplyr::mutate(
    `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
  ) |>
  filter(asr != Inf) |>
  mutate(highlight = ifelse(asr_lower > 1, "Highlighted", "Not Highlighted")) |>
  rename(index_cohort_name = `Index cohort name`,
         marker_cohort_name = `Marker cohort name`
         #,
         #combination_window = `Combination window`
         )  |> 
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
      index_cohort_name == "All_nsaids" ~ "All NSAIDs",
      index_cohort_name == "Non_selective" ~ "Non-selective",
      index_cohort_name == "Cox_2" ~ "COX-2",
      TRUE ~ index_cohort_name
    )
  ) |>
  filter(index_cohort_name %in% c("All NSAIDs","Non-selective","COX-2"))

labs = c("Adjusted Sequence Ratio", "NSAID")
custom_colors <- c("adjusted" = "black")

figure2 <- ggplot(figure2_data, aes(
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

figure2

### Figure 3

# figure3_data <- ssa_estimates |>
#   dplyr::filter(`Combination window` == "0, 365") |>
#   dplyr::mutate(
#     `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
#     `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
#   ) |>
#   filter(asr != Inf) |>
#   mutate(highlight = ifelse(asr_lower > 1, "Highlighted", "Not Highlighted")) |>
#   rename(index_cohort_name = `Index cohort name`,
#          marker_cohort_name = `Marker cohort name`,
#          combination_window = `Combination window`)  |> 
#   mutate(
#     index_cohort_name = stringr::str_to_title(index_cohort_name),
#     marker_cohort_name = dplyr::case_when(
#       marker_cohort_name == "pe" ~ "Pulmonary Embolism",
#       marker_cohort_name == "gi_hemorrhage" ~ "GI Hemorrhage",
#       marker_cohort_name == "heart_failure" ~ "Heart Failure",
#       marker_cohort_name == "dvt" ~ "Deep Vein Thrombosis",
#       marker_cohort_name == "stroke_broad" ~ "Stroke",
#       marker_cohort_name == "isbroad" ~ "Ischemic Stroke",
#       marker_cohort_name == "acute_mi" ~ "Myocardial Infarction",
#       marker_cohort_name == "hem_stroke" ~ "Hemorrhagic Stroke",
#       
#       TRUE ~ stringr::str_to_title(marker_cohort_name)),
#     index_cohort_name = dplyr::case_when(
#       index_cohort_name == "All_nsaids" ~ "All NSAIDs",
#       index_cohort_name == "Non_selective" ~ "Non-selective",
#       index_cohort_name == "Cox_2" ~ "COX-2",
#       TRUE ~ index_cohort_name
#     )
#   ) |>
#   filter(!index_cohort_name %in% c("All NSAIDs","Non-selective","COX-2"),
#          marker_cohort_name != "GI Hemorrhage")
# 
# labs = c("Adjusted Sequence Ratio", "NSAID")
# custom_colors <- c("adjusted" = "black")
# 
# figure3 <- ggplot(figure3_data, aes(
#   x = index_cohort_name,
#   y = asr,
#   ymin = asr_lower,
#   ymax = asr_upper,
#   color = signal
# )) +
#   geom_hline(yintercept = 1, linetype = 2) +
#   # Draw error bars with thicker lines
#   geom_errorbar(
#     aes(ymin = asr_lower, ymax = asr_upper),
#     position = position_dodge(width = 0.8),
#     width = 0,
#     size = 1  # This controls the thickness of the error bar line
#   ) +
#   # Add points separately
#   geom_point(
#     position = position_dodge(width = 0.8),
#     size = 3.5  # Controls the size of the point
#   ) +
#   facet_wrap(~ marker_cohort_name, scales = "free_x") +
#   coord_flip() +
#   theme_bw() +
#   labs(
#     x = "NSAID",
#     y = "Adjusted Sequence Ratio"
#   ) +
#   theme(
#     legend.position = "right",
#     legend.title = element_blank(),
#     strip.text = element_text(face = "bold", size = 16),
#     axis.text = ggplot2::element_text(size = 14),
#     axis.title = ggplot2::element_text(size = 16)
#   )
# 
# figure3

### Figure 4

figure4_data <- ssa_estimates_controls |>
  dplyr::mutate(
    `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
  ) |>
  filter(asr != Inf) |>
  mutate(highlight = ifelse(asr_lower > 1, "Highlighted", "Not Highlighted")) |>
  rename(index_cohort_name = `Index cohort name`,
         marker_cohort_name = `Marker cohort name`)  |> 
  mutate(
    index_cohort_name = stringr::str_to_title(index_cohort_name),
    index_cohort_name = dplyr::case_when(
      index_cohort_name == "All_nsaids" ~ "All NSAIDs",
      TRUE ~ index_cohort_name
    )
  )

labs = c("Adjusted Sequence Ratio", "NSAID")
custom_colors <- c("adjusted" = "black")

figure4 <- ggplot(figure4_data, aes(
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
  facet_wrap(~ index_cohort_name, scales = "free_y") +
  # Add points separately
  geom_point(
    position = position_dodge(width = 0.8),
    size = 3.5  # Controls the size of the point
  ) +
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

figure4
### Figure 5

figure5_data <- ssa_estimates_sex |>
  dplyr::mutate(
    sex = dplyr::case_when(
      stringr::str_detect(`Index cohort name`, "_female$") ~ "Female",
      stringr::str_detect(`Index cohort name`, "_male$") ~ "Male",
      TRUE ~ "Unspecified"
    )) |>
  dplyr::mutate(
    `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    `Index cohort name` = stringr::str_remove(`Index cohort name`, "_(female|male)$"),
    `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
  ) |>
  filter(asr != Inf) |>
  mutate(highlight = ifelse(asr_lower > 1, "Highlighted", "Not Highlighted")) |>
  rename(index_cohort_name = `Index cohort name`,
         marker_cohort_name = `Marker cohort name`) |> 
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
      index_cohort_name == "All_nsaids" ~ "All NSAIDs",
      index_cohort_name == "Non_selective" ~ "Non-selective",
      index_cohort_name == "Cox_2" ~ "COX-2",
      TRUE ~ index_cohort_name
    )
  ) |>
  filter(!index_cohort_name %in% c("All NSAIDs","Non-selective","COX-2"))

labs = c("Adjusted Sequence Ratio", "NSAID")
custom_colors <- c("adjusted" = "black")

figure5 <- ggplot(figure5_data, aes(
  x = index_cohort_name,
  y = asr,
  ymin = asr_lower,
  ymax = asr_upper,
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
  facet_wrap(~ marker_cohort_name, scales = "free") +
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

figure5

### Figure 6

figure6_data <- ssa_estimates_age |>
  dplyr::mutate(
    # Extract sex BEFORE cleaning names
    age = dplyr::case_when(
      stringr::str_detect(`Index cohort name`, "_18_to_65$") ~ "Under 65",
      stringr::str_detect(`Index cohort name`, "_65_and_over$") ~ "Over 65",
      TRUE ~ "Unspecified"
    )) |>
  dplyr::mutate(
    `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    `Index cohort name` = stringr::str_remove(`Index cohort name`, "_(65_and_over|18_to_65)$"),
    `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
  ) |>
  filter(asr != Inf) |>
  mutate(highlight = ifelse(asr_lower > 1, "Highlighted", "Not Highlighted")) |>
  rename(index_cohort_name = `Index cohort name`,
         marker_cohort_name = `Marker cohort name`) |> 
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
      index_cohort_name == "All_nsaids" ~ "All NSAIDs",
      index_cohort_name == "Non_selective" ~ "Non-selective",
      index_cohort_name == "Cox_2" ~ "COX-2",
      TRUE ~ index_cohort_name
    )
  ) |>
  filter(!index_cohort_name %in% c("All NSAIDs","Non-selective","COX-2"),
         marker_cohort_name != "GI Hemorrhage")

labs = c("Adjusted Sequence Ratio", "NSAID")
custom_colors <- c("adjusted" = "black")

figure6 <- ggplot(figure6_data, aes(
  x = index_cohort_name,
  y = asr,
  ymin = asr_lower,
  ymax = asr_upper,
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
  facet_wrap(~ marker_cohort_name, scales = "free") +
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

figure6

### Figure 7

figure7_data <- rbind(ssa_estimates_age,ssa_estimates_sex) |>
  dplyr::mutate(
    # Extract sex BEFORE cleaning names
    age = dplyr::case_when(
      stringr::str_detect(`Index cohort name`, "_18_to_65$") ~ "Under 65",
      stringr::str_detect(`Index cohort name`, "_65_and_over$") ~ "Over 65",
      TRUE ~ "Unspecified"
    ),
    sex = dplyr::case_when(
      stringr::str_detect(`Index cohort name`, "_female$") ~ "Female",
      stringr::str_detect(`Index cohort name`, "_male$") ~ "Male",
      TRUE ~ "Unspecified"
    )) |>
  dplyr::mutate(
    `Index cohort name` = stringr::str_replace(`Index cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", ""),
    `Index cohort name` = stringr::str_remove(`Index cohort name`, "_(65_and_over|18_to_65)$"),
    `Index cohort name` = stringr::str_remove(`Index cohort name`, "_(female|male)$"),
    `Marker cohort name` = stringr::str_replace(`Marker cohort name`, "^(?:[A-Za-z][0-9]|[0-9])[^_]*_", "")
  ) |>
  filter(asr != Inf) |>
  mutate(highlight = ifelse(asr_lower > 1, "Highlighted", "Not Highlighted")) |>
  rename(index_cohort_name = `Index cohort name`,
         marker_cohort_name = `Marker cohort name`) |> 
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
      index_cohort_name == "All_nsaids" ~ "All NSAIDs",
      index_cohort_name == "Non_selective" ~ "Non-selective",
      index_cohort_name == "Cox_2" ~ "COX-2",
      TRUE ~ index_cohort_name
    )
  ) |>
  filter(index_cohort_name %in% c("All NSAIDs","Non-selective","COX-2"),
         marker_cohort_name != "GI Hemorrhage")

labs = c("Adjusted Sequence Ratio", "NSAID")
custom_colors <- c("adjusted" = "black")

figure7 <- ggplot(figure7_data, aes(
  x = marker_cohort_name,
  y = asr,
  ymin = asr_lower,
  ymax = asr_upper,
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
  facet_wrap(~ index_cohort_name + sex, scales = "free") +
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

figure7

### Figure 8

figure8 <- ggplot(figure1_data, aes(
  x = index_cohort_name,
  y = asr,
  ymin = asr_lower,
  ymax = asr_upper,
  color = marker_cohort_name,
  shape = highlight
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

figure8

ggsave("combined_plot.png", plot = figure8,
       height = 16, width = 8)
