source("00_config.R")
source("01_helper.R")

make_output_dir()

numeric_feature_names <- c("Width", "Height", "Aspect_Ratio")

# 1. LOAD RAW DATA
raw_table <- read.csv(DATA_FILE, na.strings = c("?", ""), check.names = FALSE)
raw_table[] <- lapply(raw_table, function(col) {
  if (is.character(col)) trimws(col) else col
})

original_rows <- nrow(raw_table)
raw_table <- raw_table[!duplicated(raw_table), , drop = FALSE]
duplicates_removed <- original_rows - nrow(raw_table)

# add internal tracking id
raw <- data.frame(.row_id = seq_len(nrow(raw_table)), raw_table, check.names = FALSE)

# identify columns
ads_col_name <- names(raw)[ncol(raw)]
other_cols <- setdiff(names(raw), c(".row_id", ads_col_name))

# original dataset layout:
# other_cols[1] = original row_id
# other_cols[2:4] = Width, Height, Aspect_Ratio
if (length(other_cols) < 4L) {
  stop("Input file does not have the expected row-id + 3 numeric columns layout.")
}

orig_row_id_col <- other_cols[1]
numeric_cols <- other_cols[2:4]
binary_cols  <- other_cols[5:length(other_cols)]

# 2. SPLIT INTO 2 TABLES
# numeric table
num_table <- raw[, c(".row_id", orig_row_id_col, numeric_cols, ads_col_name), drop = FALSE]
names(num_table) <- c(".row_id", "row_id", "Width", "Height", "Aspect_Ratio", "IsAds")

num_table$Width <- suppressWarnings(as.numeric(num_table$Width))
num_table$Height <- suppressWarnings(as.numeric(num_table$Height))
num_table$Aspect_Ratio <- suppressWarnings(as.numeric(num_table$Aspect_Ratio))
num_table$IsAds <- ads_convert(num_table$IsAds)

# na count table
num_na_count <- sapply(num_table[, numeric_feature_names, drop = FALSE], function(col) sum(is.na(col)))
num_na_count <- data.frame(
  Feature = names(num_na_count),
  NA_Count = num_na_count,
  stringsAsFactors = FALSE
)
write.csv(num_na_count, file.path(OUTPUT_DIR, "01_numeric_na_count.csv"), row.names = FALSE)

# binary table
bin_table <- raw[, c(".row_id", binary_cols, ads_col_name), drop = FALSE]
names(bin_table)[ncol(bin_table)] <- "IsAds"

# 3. CLEAN EACH SIDE
# numeric version 1: drop NA
num_drop <- drop_na_rows(num_table, numeric_feature_names)

# numeric version 2: median impute
num_impute <- median_impute(num_table, numeric_feature_names)

# binary: drop rows with NA in binary set
bin_clean_res <- clean_binary_rows(bin_table, binary_cols)
bin_clean <- bin_clean_res$data

# 4. ALIGN BY .row_id
# Pipeline 1: numeric drop + binary clean
ids_drop <- shared_ids_in_order(num_drop$.row_id, bin_clean$.row_id)

num_pipeline_drop <- align_to_row_ids(num_drop, ids_drop)
bin_pipeline_drop <- align_to_row_ids(bin_clean, ids_drop)

stopifnot(identical(num_pipeline_drop$.row_id, bin_pipeline_drop$.row_id))

# Pipeline 2: numeric impute + binary clean
ids_impute <- shared_ids_in_order(num_impute$.row_id, bin_clean$.row_id)

num_pipeline_impute <- align_to_row_ids(num_impute, ids_impute)
bin_pipeline_impute <- align_to_row_ids(bin_clean, ids_impute)

stopifnot(identical(num_pipeline_impute$.row_id, bin_pipeline_impute$.row_id))

# 5. STORE CLEANED PIPELINES
cleaned_pipelines <- list(
  drop = list(
    num = num_pipeline_drop,
    bin = bin_pipeline_drop
  ),
  impute = list(
    num = num_pipeline_impute,
    bin = bin_pipeline_impute
  )
)

# 6. CLEANING SUMMARY
cleaning_summary <- data.frame(
  Stage = c(
    "raw_original",
    "after_duplicate_removal",
    "binary_clean",
    "pipeline_drop_aligned",
    "pipeline_impute_aligned"
  ),
  Rows = c(
    original_rows,
    nrow(raw),
    nrow(bin_clean),
    nrow(num_pipeline_drop),
    nrow(num_pipeline_impute)
  ),
  stringsAsFactors = FALSE
)

write.csv(cleaning_summary, file.path(OUTPUT_DIR, "01_cleaning_summary.csv"), row.names = FALSE)
saveRDS(cleaned_pipelines, file.path(OUTPUT_DIR, "01_cleaned_pipelines.rds"))
