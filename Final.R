# -------------------------
# 1) CONFIGURATION
# -------------------------
DATA_FILE <- "add.csv"
OUTPUT_DIR <- "outputs"
PLOT_DIR <- file.path(OUTPUT_DIR, "plots")

NUMERIC_FEATURES <- c("Width", "Height", "Aspect_Ratio")
IQR_K <- 1.5
MIN_PREVALENCE <- 0.01
MAX_PREVALENCE <- 0.99
TOP_BINARY_PLOTS <- 20L

# -------------------------
# 2) GENERAL HELPERS
# -------------------------
ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}

save_plot <- function(filename, plot_code, width = 1600, height = 1600, res = 200) {
  png(file.path(PLOT_DIR, filename), width = width, height = height, res = res)
  on.exit(dev.off(), add = TRUE)
  plot_code()
}

class_colors <- function(labels) {
  ifelse(as.character(labels) == "ads", "red", "blue")
}

# -------------------------
# 3) DATA CONVERSION HELPERS
# -------------------------
convert_ads_label <- function(x) {
  x <- trimws(as.character(x))
  out <- ifelse(x == "ad.", "ads",
         ifelse(x == "nonad.", "non-ads", NA_character_))
  factor(out, levels = c("non-ads", "ads"))
}

convert_binary_value <- function(x) {
  x <- trimws(as.character(x))
  ifelse(x == "1", TRUE,
         ifelse(x == "0", FALSE, NA))
}

safe_as_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

binary_to_integer_df <- function(df) {
  if (ncol(df) == 0) return(df)

  out <- df
  for (nm in names(out)) {
    out[[nm]] <- as.integer(as.logical(out[[nm]]))
  }
  out
}

# -------------------------
# 4) CLEANING HELPERS
# -------------------------
drop_na_rows <- function(df, cols) {
  df[complete.cases(df[, cols, drop = FALSE]), , drop = FALSE]
}

median_impute <- function(df, cols) {
  out <- df
  for (col_name in cols) {
    med <- median(out[[col_name]], na.rm = TRUE)
    out[[col_name]][is.na(out[[col_name]])] <- med
  }
  out
}

clean_binary_rows <- function(df, binary_cols) {
  out <- df
  out$IsAds <- convert_ads_label(out$IsAds)

  if (length(binary_cols) > 0) {
    out[, binary_cols] <- lapply(out[, binary_cols, drop = FALSE], convert_binary_value)
  }

  required_cols <- c(binary_cols, "IsAds")
  keep <- complete.cases(out[, required_cols, drop = FALSE])

  list(
    data = out[keep, , drop = FALSE],
    keep_row_id = out$.row_id[keep]
  )
}

shared_ids_in_order <- function(primary_ids, secondary_ids) {
  primary_ids[primary_ids %in% secondary_ids]
}

align_to_row_ids <- function(df, ids) {
  df[match(ids, df$.row_id), , drop = FALSE]
}

# -------------------------
# 5) PROCESSING HELPERS
# -------------------------
iqr_remove_rows_by_group <- function(df, cols, group_col = "IsAds", k = IQR_K) {
  keep <- rep(TRUE, nrow(df))
  bounds_list <- list()
  groups <- unique(as.character(df[[group_col]]))

  for (group_name in groups) {
    group_rows <- which(as.character(df[[group_col]]) == group_name)
    if (length(group_rows) == 0) next

    keep_group <- rep(TRUE, length(group_rows))

    for (col_name in cols) {
      values <- df[group_rows, col_name]

      q1 <- as.numeric(quantile(values, 0.25, na.rm = TRUE, type = 7))
      q3 <- as.numeric(quantile(values, 0.75, na.rm = TRUE, type = 7))
      iqr_value <- q3 - q1
      lower <- q1 - (k * iqr_value)
      upper <- q3 + (k * iqr_value)

      keep_group <- keep_group & !is.na(values) & values >= lower & values <= upper

      bounds_list[[length(bounds_list) + 1L]] <- data.frame(
        Group = group_name,
        Variable = col_name,
        Lower = lower,
        Upper = upper,
        stringsAsFactors = FALSE
      )
    }

    keep[group_rows] <- keep_group
  }

  list(
    data = df[keep, , drop = FALSE],
    keep_row_id = df$.row_id[keep],
    bounds = if (length(bounds_list) > 0) do.call(rbind, bounds_list) else data.frame()
  )
}

prevalence_filter <- function(df, min_prev = MIN_PREVALENCE, max_prev = MAX_PREVALENCE) {
  if (ncol(df) == 0) {
    return(list(
      data = df,
      kept = character(0),
      stats = data.frame(),
      removed_constant = character(0),
      removed_prevalence = character(0)
    ))
  }

  count_ones <- colSums(df == 1L, na.rm = TRUE)
  prevalence <- colMeans(df == 1L, na.rm = TRUE)

  non_constant <- count_ones > 0L & count_ones < nrow(df)
  in_range <- prevalence >= min_prev & prevalence <= max_prev
  keep <- non_constant & in_range

  stats <- data.frame(
    Feature = names(df),
    Count_1 = count_ones,
    Prevalence = prevalence,
    Kept = keep,
    stringsAsFactors = FALSE
  )

  list(
    data = df[, keep, drop = FALSE],
    kept = names(df)[keep],
    stats = stats,
    removed_constant = names(df)[!non_constant],
    removed_prevalence = names(df)[non_constant & !in_range]
  )
}

build_analysis_table <- function(num_df, bin_df) {
  stopifnot(identical(num_df$.row_id, bin_df$.row_id))

  binary_cols <- setdiff(names(bin_df), c(".row_id", "IsAds"))

  if (length(binary_cols) > 0) {
    binary_int <- binary_to_integer_df(bin_df[, binary_cols, drop = FALSE])
  } else {
    binary_int <- data.frame(row.names = seq_len(nrow(bin_df)))
  }

  out <- cbind(
    num_df[, c(".row_id", NUMERIC_FEATURES), drop = FALSE],
    binary_int,
    IsAds = num_df$IsAds,
    stringsAsFactors = FALSE
  )

  out$y <- as.integer(out$IsAds == "ads")
  out
}

process_one_pipeline <- function(name, pipeline) {
  num_df <- pipeline$num
  bin_df <- pipeline$bin

  stopifnot(identical(num_df$.row_id, bin_df$.row_id))

  # A. remove numeric outlier rows with IQR by class
  num_iqr <- iqr_remove_rows_by_group(num_df, NUMERIC_FEATURES)

  # B. filter binary columns by prevalence
  binary_cols <- setdiff(names(bin_df), c(".row_id", "IsAds"))

  if (length(binary_cols) > 0) {
    binary_int <- binary_to_integer_df(bin_df[, binary_cols, drop = FALSE])
    prev_res <- prevalence_filter(binary_int)
  } else {
    prev_res <- list(
      data = data.frame(row.names = seq_len(nrow(bin_df))),
      kept = character(0),
      stats = data.frame(),
      removed_constant = character(0),
      removed_prevalence = character(0)
    )
  }

  bin_processed <- cbind(
    bin_df[, ".row_id", drop = FALSE],
    prev_res$data,
    IsAds = bin_df$IsAds,
    stringsAsFactors = FALSE
  )

  # C. align rows after IQR removal
  final_ids <- shared_ids_in_order(num_iqr$keep_row_id, bin_processed$.row_id)
  num_final <- align_to_row_ids(num_iqr$data, final_ids)
  bin_final <- align_to_row_ids(bin_processed, final_ids)

  stopifnot(identical(num_final$.row_id, bin_final$.row_id))

  analysis_df <- build_analysis_table(num_final, bin_final)

  write.csv(
    bin_final,
    file.path(OUTPUT_DIR, paste0(name, "_binary_processed.csv")),
    row.names = FALSE
  )

  info <- data.frame(
    Pipeline = name,
    Rows = nrow(analysis_df),
    Numeric_Features = length(NUMERIC_FEATURES),
    Binary_Before = length(binary_cols),
    Binary_After_Prevalence = ncol(prev_res$data),
    Removed_Constant = length(prev_res$removed_constant),
    Removed_Prevalence = length(prev_res$removed_prevalence),
    Ads = sum(analysis_df$IsAds == "ads"),
    NonAds = sum(analysis_df$IsAds == "non-ads"),
    stringsAsFactors = FALSE
  )

  list(
    name = name,
    num = num_final,
    bin = bin_final,
    analysis = analysis_df,
    prevalence_stats = prev_res$stats,
    iqr_bounds = num_iqr$bounds,
    process_info = info
  )
}

# -------------------------
# 6) PLOTTING HELPERS
# -------------------------
plot_class_count <- function(df, filename, title_text) {
  counts <- table(df$IsAds)

  save_plot(filename, function() {
    barplot(
      counts,
      main = title_text,
      xlab = "IsAds",
      ylab = "Count",
      col = c("lightblue", "salmon")
    )
  })
}

plot_histogram <- function(values, filename, title_text, x_label, fill_color,
                           width = 1600, height = 1600, res = 200,
                           xlim = NULL, ylim = NULL) {
  save_plot(filename, function() {
    hist_args <- list(
      x = values,
      main = title_text,
      xlab = x_label,
      ylab = "Frequency",
      col = fill_color,
      border = "black"
    )

    if (!is.null(xlim)) hist_args$xlim <- xlim
    if (!is.null(ylim)) hist_args$ylim <- ylim

    do.call(hist, hist_args)
  }, width = width, height = height, res = res)
}

plot_scatter_by_class <- function(x, y, labels, filename, title_text, x_label, y_label) {
    save_plot(filename, function() {
    plot(
        x, y,
        col = class_colors(labels),
        pch = 19,
        xlab = x_label,
        ylab = y_label,
        main = title_text
        )
    legend(
        "topright",
        legend = c("ads", "non-ads"),
        col = c("red", "blue"),
        pch = 19
    )
    })
}

plot_boxplot_by_class <- function(df, feature, filename) {
  save_plot(filename, function() {
    boxplot(
      df[[feature]] ~ df$IsAds,
      main = paste("Boxplot of", feature, "by IsAds"),
      xlab = "IsAds",
      ylab = feature,
      col = c("lightblue", "salmon")
    )
  }, width = 1800, height = 1600, res = 250)
}

plot_logistic_curve <- function(df, feature, filename) {
    formula_text <- as.formula(paste("y ~", feature))
    model <- glm(formula_text, data = df, family = "binomial")

    x_values <- sort(df[[feature]])
    new_data <- data.frame(x_values)
    names(new_data) <- feature
    predicted_y <- predict(model, newdata = new_data, type = "response")

    save_plot(filename, function() {
        plot(
        df[[feature]], df$y,
        col = class_colors(df$IsAds),
        pch = 20,
        xlab = feature,
        ylab = "isAds",
        main = paste(feature, "vs isAds with Logistic Fit")
        )
        lines(x_values, predicted_y, col = "darkgreen", lwd = 2)
        legend(
        "topright",
        legend = c("ads", "non-ads"),
        col = c("red", "blue"),
        pch = 20
        )
    })
}

plot_correlation_heatmap <- function(df, filename) {
  cor_mat <- cor(df[, NUMERIC_FEATURES, drop = FALSE], use = "complete.obs")

  save_plot(filename, function() {
    corrplot::corrplot(
      cor_mat,
      method = "color",
      type = "upper",
      addCoef.col = "black",
      tl.col = "black",
      number.cex = 0.8,
      title = "Correlation Matrix Heatmap for Numeric Features",
      mar = c(0, 0, 1, 0)
    )
  }, width = 1200, height = 1600, res = 200)
}

plot_top_binary_prevalence <- function(df, filename) {
  binary_cols <- setdiff(names(df), c(".row_id", NUMERIC_FEATURES, "IsAds", "y"))
  if (length(binary_cols) == 0) return(invisible(NULL))

  prevalence <- sapply(df[, binary_cols, drop = FALSE], function(col) mean(col == 1, na.rm = TRUE))
  prevalence <- sort(prevalence, decreasing = TRUE)
  prevalence <- prevalence[seq_len(min(TOP_BINARY_PLOTS, length(prevalence)))]

  save_plot(filename, function() {
    barplot(
      prevalence,
      names.arg = names(prevalence),
      main = "Top Binary Indicators",
      xlab = "Binary Features",
      ylab = "Prevalence",
      las = 2,
      cex.names = 0.8,
      ylim = c(0, max(prevalence) * 1.1),
      col = "lightgreen"
    )
  })
}

plot_top_binary_differences <- function(df, ads_filename, nonads_filename) {
  binary_cols <- setdiff(names(df), c(".row_id", NUMERIC_FEATURES, "IsAds", "y"))
  if (length(binary_cols) == 0) return(invisible(NULL))

  ads_data <- df[df$IsAds == "ads", , drop = FALSE]
  nonads_data <- df[df$IsAds == "non-ads", , drop = FALSE]

  diff_props <- sapply(binary_cols, function(col_name) {
    mean(ads_data[[col_name]] == 1, na.rm = TRUE) -
      mean(nonads_data[[col_name]] == 1, na.rm = TRUE)
  })

  ads_diff <- sort(diff_props[diff_props > 0], decreasing = TRUE)
  nonads_diff <- sort(diff_props[diff_props < 0], decreasing = FALSE)

  if (length(ads_diff) > 0) {
    top_ads_diff <- ads_diff[seq_len(min(TOP_BINARY_PLOTS, length(ads_diff)))]

    save_plot(ads_filename, function() {
      par(mar = c(5, 10, 4, 2))
      barplot(
        rev(top_ads_diff),
        horiz = TRUE,
        las = 1,
        col = "salmon",
        xlab = "Difference",
        main = "Top Indicators Most Common in Ads"
      )
      abline(v = 0, lwd = 2)
    }, width = 1800, height = 1600, res = 220)
  }

  if (length(nonads_diff) > 0) {
    top_nonads_diff <- nonads_diff[seq_len(min(TOP_BINARY_PLOTS, length(nonads_diff)))]

    save_plot(nonads_filename, function() {
      par(mar = c(5, 10, 4, 2))
      barplot(
        rev(top_nonads_diff),
        horiz = TRUE,
        las = 1,
        col = "skyblue",
        xlab = "Difference",
        main = "Top Indicators Most Common in Non-Ads"
      )
      abline(v = 0, lwd = 2)
    }, width = 1800, height = 1600, res = 220)
  }
}

# -------------------------
# 7) LOAD RAW DATA
# -------------------------
ensure_dir(OUTPUT_DIR)
ensure_dir(PLOT_DIR)

raw_table <- read.csv(DATA_FILE, na.strings = c("?", ""), check.names = FALSE)
raw_table[] <- lapply(raw_table, function(col) {
  if (is.character(col)) trimws(col) else col
})

original_rows <- nrow(raw_table)
raw_table <- raw_table[!duplicated(raw_table), , drop = FALSE]
duplicates_removed <- original_rows - nrow(raw_table)

raw <- data.frame(.row_id = seq_len(nrow(raw_table)), raw_table, check.names = FALSE)

# Assumption kept from your original project:
# - last column = target label
# - first non-target column = original row id
# - next 3 columns = Width, Height, Aspect_Ratio
ads_col_name <- names(raw)[ncol(raw)]
other_cols <- setdiff(names(raw), c(".row_id", ads_col_name))

if (length(other_cols) < 4L) {
  stop("Input file does not have the expected row-id + 3 numeric columns layout.")
}

original_row_id_col <- other_cols[1]
raw_numeric_cols <- other_cols[2:4]
raw_binary_cols <- if (length(other_cols) > 4L) other_cols[5:length(other_cols)] else character(0)

# -------------------------
# 8) SPLIT INTO NUMERIC AND BINARY TABLES
# -------------------------
num_table <- raw[, c(".row_id", original_row_id_col, raw_numeric_cols, ads_col_name), drop = FALSE]
names(num_table) <- c(".row_id", "row_id", "Width", "Height", "Aspect_Ratio", "IsAds")

num_table$Width <- safe_as_numeric(num_table$Width)
num_table$Height <- safe_as_numeric(num_table$Height)
num_table$Aspect_Ratio <- safe_as_numeric(num_table$Aspect_Ratio)
num_table$IsAds <- convert_ads_label(num_table$IsAds)

write.csv(num_table, file.path(OUTPUT_DIR, "numeric_table_aftertypecasting.csv"), row.names = FALSE)

if (length(raw_binary_cols) > 0) {
  bin_table <- raw[, c(".row_id", raw_binary_cols, ads_col_name), drop = FALSE]
} else {
  bin_table <- raw[, c(".row_id", ads_col_name), drop = FALSE]
}
names(bin_table)[names(bin_table) == ads_col_name] <- "IsAds"

binary_cols <- setdiff(names(bin_table), c(".row_id", "IsAds"))

# -------------------------
# 9) SAVE NA SUMMARY FOR NUMERIC FEATURES
# -------------------------
ads_table <- num_table[num_table$IsAds == "ads", , drop = FALSE]

numeric_na_summary <- do.call(
  rbind,
  lapply(NUMERIC_FEATURES, function(feature_name) {
    total_count <- nrow(num_table)
    ads_count <- nrow(ads_table)
    na_count <- sum(is.na(num_table[[feature_name]]))
    na_ads_count <- sum(is.na(ads_table[[feature_name]]))

    row <- data.frame(
      Feature = feature_name,
      Total = total_count,
      isAds = ads_count,
      NA_count = na_count,
      NA_isAds = na_ads_count,
      Percent_NA_isAds = if (ads_count > 0) na_ads_count / ads_count * 100 else NA_real_,
      stringsAsFactors = FALSE
    )
    names(row) <- c("Feature", "Total", "isAds", "NA", "NA_isAds", "%NA_isAds")
    row
  })
)

write.csv(numeric_na_summary, file.path(OUTPUT_DIR, "numeric_na_count.csv"), row.names = FALSE)

# -------------------------
# 10) CLEAN THE DATA
# -------------------------
# Pipeline A: drop numeric rows with NA
numeric_drop <- drop_na_rows(num_table, NUMERIC_FEATURES)

# Pipeline B: median impute numeric rows with NA
numeric_impute <- median_impute(num_table, NUMERIC_FEATURES)

# Clean binary rows (remove rows with NA in binary variables or class)
binary_clean_res <- clean_binary_rows(bin_table, binary_cols)
binary_clean <- binary_clean_res$data

# Align rows for drop pipeline
ids_drop <- shared_ids_in_order(numeric_drop$.row_id, binary_clean$.row_id)
num_pipeline_drop <- align_to_row_ids(numeric_drop, ids_drop)
bin_pipeline_drop <- align_to_row_ids(binary_clean, ids_drop)
stopifnot(identical(num_pipeline_drop$.row_id, bin_pipeline_drop$.row_id))

# Align rows for impute pipeline
ids_impute <- shared_ids_in_order(numeric_impute$.row_id, binary_clean$.row_id)
num_pipeline_impute <- align_to_row_ids(numeric_impute, ids_impute)
bin_pipeline_impute <- align_to_row_ids(binary_clean, ids_impute)
stopifnot(identical(num_pipeline_impute$.row_id, bin_pipeline_impute$.row_id))

after_cleaning_list <- list(
  drop = list(num = num_pipeline_drop, bin = bin_pipeline_drop),
  impute = list(num = num_pipeline_impute, bin = bin_pipeline_impute)
)

write.csv(after_cleaning_list$impute$num,
          file.path(OUTPUT_DIR, "numeric_impute_aftercleaning.csv"),
          row.names = FALSE)

cleaning_summary <- data.frame(
  Stage = c(
    "raw_original",
    "after_duplicate_removal",
    "after_binary_cleaning",
    "after_median_imputed"
  ),
  NumberofRows = c(
    original_rows,
    nrow(raw),
    nrow(binary_clean),
    nrow(num_pipeline_impute)
  ),
  stringsAsFactors = FALSE
)

write.csv(cleaning_summary, file.path(OUTPUT_DIR, "Rows_Stage_Summary.csv"), row.names = FALSE)

# -------------------------
# 11) PROCESS THE CLEANED DATA
# -------------------------
after_processed_list <- list(
  drop = process_one_pipeline("drop", after_cleaning_list$drop),
  impute = process_one_pipeline("impute", after_cleaning_list$impute)
)

process_summary <- do.call(
  rbind,
  lapply(after_processed_list, function(x) x$process_info)
)

write.csv(process_summary, file.path(OUTPUT_DIR, "after_processed_summary.csv"), row.names = FALSE)

# -------------------------
# 12) PLOTS BEFORE PROCESSING
# -------------------------
plot_class_count(num_table, "00_isAds_Count_Before_Processing.png", "Number of Ads and Non-Ads")
plot_histogram(num_table$Width,  "01_Width_before_processing_hist.png",        "Histogram of Width",         "Width",         "salmon")
plot_histogram(num_table$Height, "02_Height_before_processing_hist.png",       "Histogram of Height",        "Height",        "skyblue")
plot_histogram(num_table$Aspect_Ratio, "03_Aspect_Ratio_before_processing_hist.png", "Histogram of Aspect Ratio", "Aspect Ratio", "lightgreen")
plot_scatter_by_class(num_table$Width, num_table$Height, num_table$IsAds,
                      "04_Scatter_Width_Height_before_processing.png",
                      "Width vs Height", "Width", "Height")

# -------------------------
# 13) PLOTS AFTER PROCESSING
# -------------------------

# MEDIAN IMPUTED PIPELINE
add_data <- after_processed_list$impute$analysis

plot_class_count(add_data, "10_isAds_Count_After_Processing.png", "Number of Ads vs Non-Ads")
plot_histogram(add_data$Width,  "11_Width_after_processing_hist.png",  "Histogram of Width",        "Width",         "salmon",
               width = 1600, height = 1600, res = 250, xlim = c(0, 150), ylim = c(0, 1200))
plot_histogram(add_data$Height, "12_Height_after_processing_hist.png", "Histogram of Height",       "Height",        "skyblue")
plot_histogram(add_data$Aspect_Ratio, "13_Aspect_Ratio_after_processing_hist.png", "Histogram of Aspect Ratio", "Aspect Ratio", "lightgreen")
plot_correlation_heatmap(add_data, "14_Correlation_Heatmap_after_processing.png")
plot_scatter_by_class(add_data$Width, add_data$Height, add_data$IsAds,
                      "15_Width_Height_Scatter.png",
                      "Width vs Height", "Width", "Height")
plot_boxplot_by_class(add_data, "Width",        "16_Boxplot_Width_after_processing_by_IsAds.png")
plot_boxplot_by_class(add_data, "Height",       "17_Boxplot_Height_after_processing_by_IsAds.png")
plot_boxplot_by_class(add_data, "Aspect_Ratio", "19_Boxplot_Aspect_Ratio_after_processing_by_IsAds.png")
plot_logistic_curve(add_data, "Height",       "110_Scatter_Height_isAds.png")
plot_logistic_curve(add_data, "Width",        "111_Scatter_Width_isAds.png")
plot_top_binary_prevalence(add_data,           "111_Binary_Feature_Distribution_after_processing.png")
plot_logistic_curve(add_data, "Aspect_Ratio", "112_Scatter_Aspect_Ratio_isAds.png")
plot_top_binary_differences(add_data,
                            "113_Top_Indicators_More_Common_in_Ads.png",
                            "114_Top_Indicators_More_Common_in_NonAds.png")

cat("Summary of numeric variables\n")
print(summary(num_table[, c("Width", "Height", "Aspect_Ratio")]))

# DROP NA PIPELINE
drop_data <- after_processed_list$drop$analysis

plot_class_count(drop_data, "210_isAds_Count_After_Processing_DropNA.png", 
                 "Number of Ads vs Non-Ads (Drop NA)")

plot_histogram(drop_data$Width,
               "211_Width_after_processing_hist_DropNA.png",
               "Histogram of Width (Drop NA)",
               "Width",
               "salmon",
               width = 1600, height = 1600, res = 250,
               xlim = c(0, 150), ylim = c(0, 1200))

plot_histogram(drop_data$Height,
               "212_Height_after_processing_hist_DropNA.png",
               "Histogram of Height (Drop NA)",
               "Height",
               "skyblue")

plot_histogram(drop_data$Aspect_Ratio,
               "213_Aspect_Ratio_after_processing_hist_DropNA.png",
               "Histogram of Aspect Ratio (Drop NA)",
               "Aspect Ratio",
               "lightgreen")

plot_correlation_heatmap(drop_data,
                         "214_Correlation_Heatmap_after_processing_DropNA.png")

plot_scatter_by_class(drop_data$Width, drop_data$Height, drop_data$IsAds,
                      "215_Width_Height_Scatter_DropNA.png",
                      "Width vs Height (Drop NA)", "Width", "Height")

plot_boxplot_by_class(drop_data, "Width",
                      "216_Boxplot_Width_after_processing_by_IsAds_DropNA.png")

plot_boxplot_by_class(drop_data, "Height",
                      "217_Boxplot_Height_after_processing_by_IsAds_DropNA.png")

plot_boxplot_by_class(drop_data, "Aspect_Ratio",
                      "219_Boxplot_Aspect_Ratio_after_processing_by_IsAds_DropNA.png")

plot_logistic_curve(drop_data, "Height",
                    "220_Scatter_Height_isAds_DropNA.png")

plot_logistic_curve(drop_data, "Width",
                    "221_Scatter_Width_isAds_DropNA.png")

plot_logistic_curve(drop_data, "Aspect_Ratio",
                    "222_Scatter_Aspect_Ratio_isAds_DropNA.png")

plot_top_binary_prevalence(drop_data,
                           "223_Binary_Feature_Distribution_after_processing_DropNA.png")

plot_top_binary_differences(drop_data,
                            "224_Top_Indicators_More_Common_in_Ads_DropNA.png",
                            "225_Top_Indicators_More_Common_in_NonAds_DropNA.png")

# -------------------------
# 14) FINAL MESSAGE
# -------------------------
cat("Project workflow completed.\n")
cat("Rows removed as duplicates:", duplicates_removed, "\n")
cat("Outputs folder:", OUTPUT_DIR, "\n")
cat("Plots folder:", PLOT_DIR, "\n")
cat("Available pipelines:", paste(names(after_cleaning_list), collapse = ", "), "\n")
cat("DONE\n")
