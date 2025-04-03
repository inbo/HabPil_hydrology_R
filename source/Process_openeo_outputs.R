library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)


# ---- Define the study site ----
study_site <- "Demervallei"  # Change this to process a different region

# Load the data
mcfeeters_path <- file.path("output/NDWI_McFeeters", study_site, "masked", "timeseries.json")
xu_path        <- file.path("output/NDWI_Xu",        study_site, "masked", "timeseries.json")
gao_path       <- file.path("output/NDWI_Gao",       study_site, "masked", "timeseries.json")



ndwi_mcfeeters <- fromJSON(mcfeeters_path)
ndwi_xu <- fromJSON(xu_path)
ndwi_gao <- fromJSON(gao_path)

ndwi_mcfeeters_df <- do.call(rbind, lapply(names(ndwi_mcfeeters), function(date) {
  data.frame(
    date = as.Date(date),
    point_id = seq_along(ndwi_mcfeeters[[date]]),  # Assign unique point IDs
    NDWI_McFeeters = unlist(ndwi_mcfeeters[[date]])  # Extract values
  )
}))

ndwi_xu_df <- do.call(rbind, lapply(names(ndwi_xu), function(date) {
  data.frame(
    date = as.Date(date),
    point_id = seq_along(ndwi_xu[[date]]),
    NDWI_Xu = unlist(ndwi_xu[[date]])
  )
}))

ndwi_gao_df <- do.call(rbind, lapply(names(ndwi_gao), function(date) {
  data.frame(
    date = as.Date(date),
    point_id = seq_along(ndwi_gao[[date]]),
    NDWI_Gao = unlist(ndwi_gao[[date]])
  )
}))


ndwi_df <- reduce(list(ndwi_mcfeeters_df, ndwi_xu_df, ndwi_gao_df), full_join, by = c("date", "point_id"))

# Ensure date column is in Date format
ndwi_df$date <- as.Date(ndwi_df$date)

# Ensure the output directory exists
output_dir <- file.path("output/NDWI_time_series", study_site)
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)  # Create directory if it doesn't exist
}

# Save the CSV file
write.csv(ndwi_df, file.path(output_dir, "Demervallei_NDWI_all.csv"), row.names = FALSE)


# Loop over each unique point_id
unique_ids <- unique(ndwi_df$point_id)

for (id in unique_ids) {
  df_subset <- ndwi_df %>% filter(point_id == id)

  # Reshape to long format
  df_long <- df_subset %>%
    pivot_longer(
      cols = starts_with("NDWI_"),
      names_to = "Index",
      values_to = "Value"
    )

  # Create faceted plot with points only
  p <- ggplot(df_long, aes(x = date, y = Value, color = Index)) +  # color by Index
    geom_point() +
    facet_wrap(~Index, ncol = 1, scales = "free_y") +
    labs(
      title = paste("NDWI Time Series -", study_site, "- Point", id),
      x = "Date",
      y = "NDWI Value"
    ) +
    theme(
      strip.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(face = "bold"),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      plot.title = element_text(size = 14, face = "bold"),
      legend.position = "none"
    )

  # Save the plot
  plot_file <- file.path(output_dir, paste0("NDWI_Timeseries_Point_", id, ".png"))
  ggsave(plot_file, plot = p, width = 8, height = 9)
}

