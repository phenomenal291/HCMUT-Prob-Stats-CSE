source("00_config.R")
source("03_process.R")

if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

for (nm in names(processed_pipelines)) {

  df <- processed_pipelines[[nm]]$analysis

  # Histogram: Width
  p1 <- ggplot(df, aes(x = Width, fill = IsAds)) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    labs(
      title = paste("Histogram of Width -", nm),
      x = "Width",
      y = "Count"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOT_DIR, paste0(nm, "_Width_hist.png")),
    plot = p1,
    width = 8,
    height = 5
  )

  # Histogram: Height
  p2 <- ggplot(df, aes(x = Height, fill = IsAds)) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    labs(
      title = paste("Histogram of Height -", nm),
      x = "Height",
      y = "Count"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOT_DIR, paste0(nm, "_hist_height.png")),
    plot = p2,
    width = 8,
    height = 5
  )

  # Histogram: Aspect Ratio
  p3 <- ggplot(df, aes(x = Aspect_Ratio, fill = IsAds)) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    labs(
      title = paste("Histogram of Aspect Ratio -", nm),
      x = "Aspect Ratio",
      y = "Count"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOT_DIR, paste0(nm, "_Aspect_Ratio_hist.png")),
    plot = p3,
    width = 8,
    height = 5
  )

  # Scatter plot: Width vs Height
  p4 <- ggplot(df, aes(x = Width, y = Height, color = IsAds)) +
    geom_point(alpha = 0.7) +
    labs(
      title = paste("Width vs Height -", nm),
      x = "Width",
      y = "Height"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOT_DIR, paste0(nm, "_Width_Height_Scatter.png")),
    plot = p4,
    width = 8,
    height = 5
  )

  # Scatter plot: Width vs Aspect Ratio
  p5 <- ggplot(df, aes(x = Width, y = Aspect_Ratio, color = IsAds)) +
    geom_point(alpha = 0.7) +
    labs(
      title = paste("Scatter Plot: Width vs Aspect Ratio -", nm),
      x = "Width",
      y = "Aspect Ratio"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOT_DIR, paste0(nm, "_Width_Aspect_Ratio_Scatter.png")),
    plot = p5,
    width = 8,
    height = 5
  )

  # Boxplot: Width by class
  p6 <- ggplot(df, aes(x = IsAds, y = Width, fill = IsAds)) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = paste("Boxplot of Width by Class -", nm),
      x = "Class",
      y = "Width"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOT_DIR, paste0(nm, "_Width_box.png")),
    plot = p6,
    width = 8,
    height = 5
  )

  # -------------------------
  # Boxplot: Height by class
  # -------------------------
  p7 <- ggplot(df, aes(x = IsAds, y = Height, fill = IsAds)) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = paste("Boxplot of Height by Class -", nm),
      x = "Class",
      y = "Height"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOT_DIR, paste0(nm, "_Height_box.png")),
    plot = p7,
    width = 8,
    height = 5
  )

  # Boxplot: Aspect Ratio by class
  p8 <- ggplot(df, aes(x = IsAds, y = Aspect_Ratio, fill = IsAds)) +
    geom_boxplot(alpha = 0.7) +
    labs(
      title = paste("Boxplot of Aspect Ratio by Class -", nm),
      x = "Class",
      y = "Aspect Ratio"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOT_DIR, paste0(nm, "_Aspect_Ratio_box.png")),
    plot = p8,
    width = 8,
    height = 5
  )

  # Class count
  class_df <- data.frame(table(df$IsAds))
  names(class_df) <- c("IsAds", "Count")

  p9 <- ggplot(class_df, aes(x = IsAds, y = Count, fill = IsAds)) +
    geom_col() +
    labs(
      title = paste("Class Count -", nm),
      x = "Class",
      y = "Count"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(PLOT_DIR, paste0(nm, "_class_count.png")),
    plot = p9,
    width = 7,
    height = 5
  )
}

cat("Visualization completed.\n")
cat("Plots saved in:", PLOT_DIR, "\n")