source("00_config.R")

# Data type converting
# isAds
ads_convert <- function(x) {
  x <- trimws(as.character(x))
  out <- ifelse(x == "ad.", "ads", ifelse(x == "nonad.", "non-ads", NA_character_))
  factor(out, levels = c("non-ads", "ads"))
}
# Terms
to_binary_logical <- function(b) {
  b <- trimws(as.character(b))
  ifelse(b == "1", TRUE, ifelse(b == "0", FALSE, NA))
}

# Cleaning helpers
# drop na rows
drop_na_rows <- function(x, cols) {
  x[complete.cases(x[, cols, drop = FALSE]), , drop = FALSE]
}
# Median impute
median_impute <- function(x, cols) {
  for (i in cols) {
    med <- median(x[[i]], na.rm = TRUE)
    x[[i]][is.na(x[[i]])] <- med
  }
  x
}
# Cleaning binary rows
clean_binary_rows <- function(x, binary_cols) {
  out <- x
  out$IsAds <- ads_convert(out$IsAds)
  out[, binary_cols] <- lapply(out[, binary_cols, drop = FALSE], to_binary_logical)
  keep <- complete.cases(out[, c(binary_cols, "IsAds"), drop = FALSE])

  list(
    data = out[keep, , drop = FALSE],
    keep_row_id = out$.row_id[keep]
  )
}

#iqr trimming
iqr_remove_rows <- function(x, cols, k = IQR_K) {
  keep <- rep(TRUE, nrow(x))
  bounds <- list()
  groups <- unique(as.character(x$IsAds))

  for (g in groups) {
    rows_g <- which(as.character(x$IsAds) == g)
    if (length(rows_g) == 0) next

    keep_g <- rep(TRUE, length(rows_g))

    for (col in cols) {
      vals <- x[rows_g, col]
      q1 <- as.numeric(quantile(vals, 0.25, na.rm = TRUE, type = 7))
      q3 <- as.numeric(quantile(vals, 0.75, na.rm = TRUE, type = 7))
      iqr <- q3 - q1
      lower <- q1 - k * iqr
      upper <- q3 + k * iqr

      keep_g <- keep_g & !is.na(vals) & vals >= lower & vals <= upper

      bounds[[length(bounds) + 1L]] <- data.frame(
        Group = g,
        Variable = col,
        Lower = lower,
        Upper = upper,
        stringsAsFactors = FALSE
      )
    }

    keep[rows_g] <- keep_g
  }

  list(
    data = x[keep, , drop = FALSE],
    keep_row_id = x$.row_id[keep],
    bounds = if (length(bounds) > 0) do.call(rbind, bounds) else NULL
  )
}

# Alignment
align_to_row_ids <- function(x, ids) {
  x[match(ids, x$.row_id), , drop = FALSE]
}

shared_ids_in_order <- function(primary_ids, secondary_ids) {
  primary_ids[primary_ids %in% secondary_ids]
}

binary_to_integer_df <- function(x) {
  out <- x
  for (nm in names(out)) {
    out[[nm]] <- as.integer(as.logical(out[[nm]]))
  }
  out
}

prevalence_filter <- function(x, min_prev = MIN_PREVALENCE, max_prev = MAX_PREVALENCE) {
  if (ncol(x) == 0) {
    return(list(
      data = x,
      kept = character(0),
      stats = data.frame(),
      removed_constant = character(0),
      removed_prevalence = character(0)
    ))
  }

  counts <- colSums(x == 1L, na.rm = TRUE)
  prev <- colMeans(x == 1L, na.rm = TRUE)
  nonconstant <- counts > 0L & counts < nrow(x)
  in_range <- prev >= min_prev & prev <= max_prev
  keep <- nonconstant & in_range

  stats <- data.frame(
    Feature = names(x),
    Count_1 = counts,
    Prevalence = prev,
    Kept = keep,
    stringsAsFactors = FALSE
  )

  list(
    data = x[, keep, drop = FALSE],
    kept = names(x)[keep],
    stats = stats,
    removed_constant = names(x)[!nonconstant],
    removed_prevalence = names(x)[nonconstant & !in_range | (!nonconstant & !keep)]
  )
}

dup_remove_cols <- function(x) {
  if (ncol(x) == 0) {
    return(list(data = x, kept = character(0), removed = character(0)))
  }

  keys <- vapply(
    x,
    function(col) paste(ifelse(is.na(col), "NA", as.integer(col)), collapse = "|"),
    character(1)
  )
  keep <- !duplicated(keys)

  list(
    data = x[, keep, drop = FALSE],
    kept = names(x)[keep],
    removed = names(x)[!keep]
  )
}

build_model_frame <- function(num_df, bin_df) {
  stopifnot(identical(num_df$.row_id, bin_df$.row_id))
  bin_cols <- setdiff(names(bin_df), c(".row_id", "IsAds"))
  numeric_cols <- c("Width", "Height", "Aspect_Ratio")

  if (length(bin_cols) > 0) {
    bin_int <- binary_to_integer_df(bin_df[, bin_cols, drop = FALSE])
  } else {
    bin_int <- data.frame(row.names = seq_len(nrow(bin_df)))
  }

  out <- cbind(
    num_df[, c(".row_id", numeric_cols), drop = FALSE],
    bin_int,
    IsAds = num_df$IsAds,
    stringsAsFactors = FALSE
  )

  out$y <- as.integer(out$IsAds == "ads")
  out
}

make_output_dir <- function(path = OUTPUT_DIR) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  invisible(path)
}
