source("02_loadClean.R")

make_output_dir()

process_one_pipeline <- function(name, pipe, min_prev = MIN_PREVALENCE, max_prev = MAX_PREVALENCE) {
  num_df <- pipe$num
  bin_df <- pipe$bin

  stopifnot(identical(num_df$.row_id, bin_df$.row_id))

  # 1. numeric processing
  # row outlier removal with IQR
  num_iqr <- iqr_remove_rows(num_df, c("Width", "Height", "Aspect_Ratio"))

  # 2. binary processing
  # column filtering by prevalence
  binary_cols <- setdiff(names(bin_df), c(".row_id", "IsAds"))

  if (length(binary_cols) > 0) {
    bin_int <- binary_to_integer_df(bin_df[, binary_cols, drop = FALSE])
    prev_res <- prevalence_filter(bin_int, min_prev = min_prev, max_prev = max_prev)
  } else {
    bin_int <- data.frame(row.names = seq_len(nrow(bin_df)))
    prev_res <- list(
      data = bin_int,
      kept = character(0),
      stats = data.frame(),
      removed_constant = character(0),
      removed_prevalence = character(0)
    )
  }

  bin_prev <- cbind(
    bin_df[, ".row_id", drop = FALSE],
    prev_res$data,
    IsAds = bin_df$IsAds,
    stringsAsFactors = FALSE
  )

  # 3. sync rows again
  # because numeric IQR removed rows
  final_ids <- shared_ids_in_order(num_iqr$keep_row_id, bin_prev$.row_id)

  num_final <- align_to_row_ids(num_iqr$data, final_ids)
  bin_final <- align_to_row_ids(bin_prev, final_ids)


  # 4. build analysis table
  analysis_df <- build_model_frame(num_final, bin_final)

  info <- data.frame(
    Pipeline = name,
    Rows = nrow(analysis_df),
    Numeric_Features = 3L,
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
# correlation matrix building fo numeric features after processing
# cor_mat <- cor(after_processed_list$drop$num[, c("Width", "Height", "Aspect_Ratio")], use = "complete.obs")


after_processed_list <- lapply(names(after_cleaning_list), function(nm) {
  process_one_pipeline(nm, after_cleaning_list[[nm]])
})
names(after_processed_list) <- names(after_cleaning_list)

process_summary <- do.call(
  rbind,
  lapply(after_processed_list, function(x) x$process_info)
)

write.csv(process_summary, file.path(OUTPUT_DIR, "after_processed_summary.csv"), row.names = FALSE)
# saveRDS(after_processed_list, file.path(OUTPUT_DIR, "03_processed_pipelines.rds"))
