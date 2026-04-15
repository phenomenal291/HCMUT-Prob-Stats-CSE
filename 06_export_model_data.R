source("03_process.R")

if (!exists("after_processed_list")) {
  stop("after_processed_list not found. Ensure processing scripts run correctly.")
}

if (!all(c("impute", "drop") %in% names(after_processed_list))) {
  stop("Expected pipelines 'impute' and 'drop' in after_processed_list.")
}

impute_data <- after_processed_list$impute$analysis
drop_data <- after_processed_list$drop$analysis

if (!is.data.frame(impute_data) || !is.data.frame(drop_data)) {
  stop("Processed analysis data is not available as data frames.")
}

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

impute_path <- file.path(OUTPUT_DIR, "cleaned_data_impute.csv")
drop_path <- file.path(OUTPUT_DIR, "cleaned_data_drop.csv")
default_path <- file.path(OUTPUT_DIR, "cleaned_data.csv")

write.csv(impute_data, impute_path, row.names = FALSE)
write.csv(drop_data, drop_path, row.names = FALSE)

# Backward-compatible default for existing scripts.
write.csv(impute_data, default_path, row.names = FALSE)

cat("Export completed.\n")
cat("-", impute_path, "rows:", nrow(impute_data), "cols:", ncol(impute_data), "\n")
cat("-", drop_path, "rows:", nrow(drop_data), "cols:", ncol(drop_data), "\n")
cat("-", default_path, "rows:", nrow(impute_data), "cols:", ncol(impute_data), "(alias -> impute)\n")
