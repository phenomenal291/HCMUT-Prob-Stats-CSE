if (!requireNamespace("randomForest", quietly = TRUE)) {
  install.packages("randomForest", repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages(library(randomForest))

K_FOLDS <- 10L
HOLDOUT_RATIO <- 0.7
RF_NTREE <- 100L
PLOT_DIR <- "outputs/plots"
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

safe_div <- function(num, den) {
  if (is.na(den) || den == 0) return(NA_real_)
  num / den
}

compute_metrics <- function(cm, positive_label) {
  labels <- colnames(cm)
  negative_label <- setdiff(labels, positive_label)

  tp <- as.numeric(cm[positive_label, positive_label])
  fp <- as.numeric(cm[positive_label, negative_label])
  fn <- as.numeric(cm[negative_label, positive_label])
  tn <- as.numeric(cm[negative_label, negative_label])

  precision <- safe_div(tp, tp + fp)
  recall <- safe_div(tp, tp + fn)
  f1 <- safe_div(2 * precision * recall, precision + recall)
  specificity <- safe_div(tn, tn + fp)
  balanced_accuracy <- mean(c(recall, specificity), na.rm = TRUE)
  accuracy <- safe_div(tp + tn, tp + tn + fp + fn)

  list(
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1 = f1,
    Specificity = specificity,
    Balanced_Accuracy = balanced_accuracy,
    TP = tp,
    FP = fp,
    FN = fn,
    TN = tn
  )
}

choose_positive_label <- function(class_levels) {
  if ("ads" %in% class_levels) return("ads")
  class_levels[2]
}

print_metrics <- function(prefix, metrics) {
  cat(prefix, "Accuracy:", round(metrics$Accuracy, 4), "\n")
  cat(prefix, "Precision:", round(metrics$Precision, 4), "\n")
  cat(prefix, "Recall:", round(metrics$Recall, 4), "\n")
  cat(prefix, "F1:", round(metrics$F1, 4), "\n")
  cat(prefix, "Specificity:", round(metrics$Specificity, 4), "\n")
  cat(prefix, "Balanced Accuracy:", round(metrics$Balanced_Accuracy, 4), "\n")
}

ensure_binary_levels <- function(actual, predicted, class_levels) {
  actual <- factor(actual, levels = class_levels)
  predicted <- factor(predicted, levels = class_levels)
  list(actual = actual, predicted = predicted)
}

evaluate_holdout <- function(data, target_col, positive_label, seed = 42, train_ratio = HOLDOUT_RATIO) {
  set.seed(seed)

  sample_indices <- sample(seq_len(nrow(data)), size = floor(train_ratio * nrow(data)))
  train_data <- data[sample_indices, , drop = FALSE]
  test_data <- data[-sample_indices, , drop = FALSE]

  model_formula <- as.formula(paste(target_col, "~ ."))
  class_levels <- levels(data[[target_col]])

  log_model <- glm(model_formula, data = train_data, family = "binomial")
  log_probabilities <- predict(log_model, newdata = test_data, type = "response")
  log_predictions <- ifelse(log_probabilities > 0.5, class_levels[2], class_levels[1])

  rf_model <- randomForest(model_formula, data = train_data, ntree = RF_NTREE, importance = TRUE)
  rf_predictions <- predict(rf_model, newdata = test_data)

  log_pairs <- ensure_binary_levels(test_data[[target_col]], log_predictions, class_levels)
  rf_pairs <- ensure_binary_levels(test_data[[target_col]], rf_predictions, class_levels)

  log_cm <- table(Predicted = log_pairs$predicted, Actual = log_pairs$actual)
  rf_cm <- table(Predicted = rf_pairs$predicted, Actual = rf_pairs$actual)

  log_metrics <- compute_metrics(log_cm, positive_label)
  rf_metrics <- compute_metrics(rf_cm, positive_label)

  list(
    train_rows = nrow(train_data),
    test_rows = nrow(test_data),
    logistic_cm = log_cm,
    rf_cm = rf_cm,
    logistic_metrics = log_metrics,
    rf_metrics = rf_metrics,
    log_model = log_model,
    rf_model = rf_model
  )
}

make_stratified_folds <- function(y, k = K_FOLDS, seed = 42) {
  set.seed(seed)
  folds <- vector("list", k)
  for (i in seq_len(k)) folds[[i]] <- integer(0)

  classes <- levels(y)
  for (cls in classes) {
    idx <- which(y == cls)
    idx <- sample(idx)
    bucket <- rep(seq_len(k), length.out = length(idx))
    for (i in seq_len(k)) {
      folds[[i]] <- c(folds[[i]], idx[bucket == i])
    }
  }

  lapply(folds, sort)
}

format_res <- function(mean_val, std_val) {
  sprintf("%.2f ± %.2f%%", mean_val * 100, std_val * 100)
}

evaluate_cv <- function(data, target_col, positive_label, k = K_FOLDS, seed = 42) {
  model_formula <- as.formula(paste(target_col, "~ ."))
  class_levels <- levels(data[[target_col]])
  folds <- make_stratified_folds(data[[target_col]], k = k, seed = seed)

  fold_rows <- list()

  for (fold_id in seq_len(k)) {
    test_idx <- folds[[fold_id]]
    train_idx <- setdiff(seq_len(nrow(data)), test_idx)

    train_data <- data[train_idx, , drop = FALSE]
    test_data <- data[test_idx, , drop = FALSE]

    # Logistic Regression
    suppressWarnings({
      log_model <- glm(model_formula, data = train_data, family = "binomial")
    })
    log_probabilities <- predict(log_model, newdata = test_data, type = "response")
    # second level is treated as 1 in GLM
    log_predictions <- ifelse(log_probabilities > 0.5, class_levels[2], class_levels[1])

    # Random Forest
    rf_model <- randomForest(model_formula, data = train_data, ntree = RF_NTREE, importance = TRUE)
    rf_predictions <- predict(rf_model, newdata = test_data)

    log_pairs <- ensure_binary_levels(test_data[[target_col]], log_predictions, class_levels)
    rf_pairs <- ensure_binary_levels(test_data[[target_col]], rf_predictions, class_levels)

    log_cm <- table(Predicted = log_pairs$predicted, Actual = log_pairs$actual)
    rf_cm <- table(Predicted = rf_pairs$predicted, Actual = rf_pairs$actual)

    log_metrics <- compute_metrics(log_cm, positive_label)
    rf_metrics <- compute_metrics(rf_cm, positive_label)

    fold_rows[[length(fold_rows) + 1L]] <- data.frame(
      Fold = fold_id,
      Model = "Logistic",
      Accuracy = log_metrics$Accuracy,
      Precision = log_metrics$Precision,
      Recall = log_metrics$Recall,
      F1 = log_metrics$F1,
      Specificity = log_metrics$Specificity,
      Balanced_Accuracy = log_metrics$Balanced_Accuracy,
      TP = log_metrics$TP,
      FP = log_metrics$FP,
      FN = log_metrics$FN,
      TN = log_metrics$TN,
      stringsAsFactors = FALSE
    )

    fold_rows[[length(fold_rows) + 1L]] <- data.frame(
      Fold = fold_id,
      Model = "RandomForest",
      Accuracy = rf_metrics$Accuracy,
      Precision = rf_metrics$Precision,
      Recall = rf_metrics$Recall,
      F1 = rf_metrics$F1,
      Specificity = rf_metrics$Specificity,
      Balanced_Accuracy = rf_metrics$Balanced_Accuracy,
      TP = rf_metrics$TP,
      FP = rf_metrics$FP,
      FN = rf_metrics$FN,
      TN = rf_metrics$TN,
      stringsAsFactors = FALSE
    )
  }

  folds_table <- do.call(rbind, fold_rows)

  summarize_model <- function(model_name) {
    rows <- folds_table[folds_table$Model == model_name, , drop = FALSE]
    data.frame(
      Model = model_name,
      K = k,
      Accuracy_Mean = mean(rows$Accuracy, na.rm = TRUE),
      Accuracy_SD = sd(rows$Accuracy, na.rm = TRUE),
      Precision_Mean = mean(rows$Precision, na.rm = TRUE),
      Precision_SD = sd(rows$Precision, na.rm = TRUE),
      Recall_Mean = mean(rows$Recall, na.rm = TRUE),
      Recall_SD = sd(rows$Recall, na.rm = TRUE),
      F1_Mean = mean(rows$F1, na.rm = TRUE),
      F1_SD = sd(rows$F1, na.rm = TRUE),
      Specificity_Mean = mean(rows$Specificity, na.rm = TRUE),
      Specificity_SD = sd(rows$Specificity, na.rm = TRUE),
      Balanced_Accuracy_Mean = mean(rows$Balanced_Accuracy, na.rm = TRUE),
      Balanced_Accuracy_SD = sd(rows$Balanced_Accuracy, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  summary_table <- rbind(summarize_model("Logistic"), summarize_model("RandomForest"))

  list(folds = folds_table, summary = summary_table)
}

build_model_table <- function(data) {
  target_col <- if ("IsAds" %in% names(data)) "IsAds" else names(data)[ncol(data)]

  data[[target_col]] <- as.factor(data[[target_col]])

  if (length(levels(data[[target_col]])) != 2) {
    stop("Target variable must have exactly 2 classes for this workflow.")
  }

  # Ensure consistent level ordering: first level is "negative", second is "positive"
  # If we have "non-ads" and "ads", we want "non-ads" as level 1, "ads" as level 2.
  if (all(c("ads", "non-ads") %in% levels(data[[target_col]]))) {
    data[[target_col]] <- factor(data[[target_col]], levels = c("non-ads", "ads"))
  }

  exclude_cols <- c(".row_id")

  if (target_col == "IsAds" && "y" %in% names(data)) {
    exclude_cols <- c(exclude_cols, "y")
  }

  if (target_col == "y" && "IsAds" %in% names(data)) {
    exclude_cols <- c(exclude_cols, "IsAds")
  }

  predictor_cols <- setdiff(names(data), c(target_col, exclude_cols))

  if (length(predictor_cols) == 0) {
    stop("No predictors available after excluding ID/target leakage columns.")
  }

  out <- data[, c(predictor_cols, target_col), drop = FALSE]
  list(data = out, target = target_col)
}

run_for_dataset <- function(label, csv_path, seed = 42) {
  if (!file.exists(csv_path)) {
    stop(paste("Dataset file not found:", csv_path))
  }

  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  model_ready <- build_model_table(data)
  data <- model_ready$data
  target_col <- model_ready$target

  class_levels <- levels(data[[target_col]])
  positive_label <- choose_positive_label(class_levels)

  holdout_res <- evaluate_holdout(data, target_col, positive_label, seed = seed)
  cv_res <- evaluate_cv(data, target_col, positive_label, k = K_FOLDS, seed = seed)

  # --- Generate Importance Plots ---
  # 1. Random Forest Importance Plot
  rf_plot_path <- file.path(PLOT_DIR, paste0(label, "_rf_importance.png"))
  cat("Saving RF importance plot to:", rf_plot_path, "\n")
  png(rf_plot_path, width = 1200, height = 1000, res = 150)
  varImpPlot(holdout_res$rf_model, main = paste("RF Feature Importance:", label), n.var = 15)
  dev.off()

  # 2. Logistic Regression Coefficient Plot (Magnitude)
  log_coefs <- summary(holdout_res$log_model)$coefficients[-1, 1]
  # Filter out NA coefficients (common in rank-deficient models)
  log_coefs <- log_coefs[!is.na(log_coefs)]
  log_importance <- sort(abs(log_coefs), decreasing = TRUE)
  top_log <- head(log_importance, 15)

  log_plot_path <- file.path(PLOT_DIR, paste0(label, "_log_importance.png"))
  cat("Saving Logistic importance plot to:", log_plot_path, "\n")
  png(log_plot_path, width = 1200, height = 1000, res = 150)
  par(mar = c(5, 12, 4, 2))
  if (length(top_log) > 0) {
    barplot(rev(top_log), horiz = TRUE, las = 1, col = "skyblue",
            main = paste("Top Logistic Coefficients (Abs):", label),
            xlab = "Absolute Coefficient Magnitude")
  } else {
    plot.new()
    text(0.5, 0.5, "No valid coefficients to plot")
  }
  dev.off()

  log_metrics <- holdout_res$logistic_metrics
  rf_metrics <- holdout_res$rf_metrics

  cat("\n========================================\n")
  cat("Dataset:", label, "\n")
  cat("Source:", csv_path, "\n")
  cat("Rows:", nrow(data), "Cols:", ncol(data), "Target:", target_col, "\n")
  cat("Positive class:", positive_label, "\n")
  cat("Holdout split: train", holdout_res$train_rows, "test", holdout_res$test_rows, "\n")
  cat("----------------------------------------\n")
  cat("Holdout Logistic Regression Confusion Matrix\n")
  print(holdout_res$logistic_cm)
  print_metrics("Logistic", log_metrics)
  cat("----------------------------------------\n")
  cat("Holdout Random Forest Confusion Matrix\n")
  print(holdout_res$rf_cm)
  print_metrics("RF", rf_metrics)
  cat("----------------------------------------\n")
  cat("Stratified", K_FOLDS, "-Fold CV Summary (Mean ± SD)\n")
  
  cv_summary_print <- data.frame(
    Model = cv_res$summary$Model,
    Accuracy = mapply(format_res, cv_res$summary$Accuracy_Mean, cv_res$summary$Accuracy_SD),
    Precision = mapply(format_res, cv_res$summary$Precision_Mean, cv_res$summary$Precision_SD),
    Recall = mapply(format_res, cv_res$summary$Recall_Mean, cv_res$summary$Recall_SD),
    F1 = mapply(format_res, cv_res$summary$F1_Mean, cv_res$summary$F1_SD),
    Specificity = mapply(format_res, cv_res$summary$Specificity_Mean, cv_res$summary$Specificity_SD),
    Bal_Acc = mapply(format_res, cv_res$summary$Balanced_Accuracy_Mean, cv_res$summary$Balanced_Accuracy_SD)
  )
  print(cv_summary_print)

  cat("\nSummary of Pooled CV Confusion Matrices\n")
  
  cat("\n--- Logistic Regression (Pooled CM) ---\n")
  folds_log <- cv_res$folds[cv_res$folds$Model == "Logistic", ]
  cm_log <- matrix(
    c(sum(folds_log$TP), sum(folds_log$FN), 
      sum(folds_log$FP), sum(folds_log$TN)), 
    nrow = 2, byrow = FALSE,
    dimnames = list(
      Predicted = c(positive_label, paste0("Non-", positive_label)),
      Actual = c(positive_label, paste0("Non-", positive_label))
    )
  )
  print(as.table(cm_log))

  cat("\n--- Random Forest (Pooled CM) ---\n")
  folds_rf <- cv_res$folds[cv_res$folds$Model == "RandomForest", ]
  cm_rf <- matrix(
    c(sum(folds_rf$TP), sum(folds_rf$FN), 
      sum(folds_rf$FP), sum(folds_rf$TN)), 
    nrow = 2, byrow = FALSE,
    dimnames = list(
      Predicted = c(positive_label, paste0("Non-", positive_label)),
      Actual = c(positive_label, paste0("Non-", positive_label))
    )
  )
  print(as.table(cm_rf))

  holdout_summary_row <- data.frame(
    Dataset = label,
    File = csv_path,
    Rows = nrow(data),
    Cols = ncol(data),
    Target = target_col,
    Positive_Class = positive_label,
    Logistic_Accuracy = log_metrics$Accuracy,
    Logistic_Precision = log_metrics$Precision,
    Logistic_Recall = log_metrics$Recall,
    Logistic_F1 = log_metrics$F1,
    Logistic_Specificity = log_metrics$Specificity,
    Logistic_Balanced_Accuracy = log_metrics$Balanced_Accuracy,
    RF_Accuracy = rf_metrics$Accuracy,
    RF_Precision = rf_metrics$Precision,
    RF_Recall = rf_metrics$Recall,
    RF_F1 = rf_metrics$F1,
    RF_Specificity = rf_metrics$Specificity,
    RF_Balanced_Accuracy = rf_metrics$Balanced_Accuracy,
    stringsAsFactors = FALSE
  )


  holdout_metrics_rows <- data.frame(
    Dataset = c(label, label),
    File = c(csv_path, csv_path),
    Model = c("Logistic", "RandomForest"),
    Target = c(target_col, target_col),
    Positive_Class = c(positive_label, positive_label),
    Accuracy = c(log_metrics$Accuracy, rf_metrics$Accuracy),
    Precision = c(log_metrics$Precision, rf_metrics$Precision),
    Recall = c(log_metrics$Recall, rf_metrics$Recall),
    F1 = c(log_metrics$F1, rf_metrics$F1),
    Specificity = c(log_metrics$Specificity, rf_metrics$Specificity),
    Balanced_Accuracy = c(log_metrics$Balanced_Accuracy, rf_metrics$Balanced_Accuracy),
    TP = c(log_metrics$TP, rf_metrics$TP),
    FP = c(log_metrics$FP, rf_metrics$FP),
    FN = c(log_metrics$FN, rf_metrics$FN),
    TN = c(log_metrics$TN, rf_metrics$TN),
    stringsAsFactors = FALSE
  )

  cv_summary_rows <- data.frame(
    Dataset = label,
    File = csv_path,
    Rows = nrow(data),
    Cols = ncol(data),
    Target = target_col,
    Positive_Class = positive_label,
    Model = cv_res$summary$Model,
    K = cv_res$summary$K,
    Accuracy_Mean = cv_res$summary$Accuracy_Mean,
    Accuracy_SD = cv_res$summary$Accuracy_SD,
    Precision_Mean = cv_res$summary$Precision_Mean,
    Precision_SD = cv_res$summary$Precision_SD,
    Recall_Mean = cv_res$summary$Recall_Mean,
    Recall_SD = cv_res$summary$Recall_SD,
    F1_Mean = cv_res$summary$F1_Mean,
    F1_SD = cv_res$summary$F1_SD,
    stringsAsFactors = FALSE
  )

  cv_fold_rows <- data.frame(
    Dataset = rep(label, nrow(cv_res$folds)),
    File = rep(csv_path, nrow(cv_res$folds)),
    Target = rep(target_col, nrow(cv_res$folds)),
    Positive_Class = rep(positive_label, nrow(cv_res$folds)),
    cv_res$folds,
    stringsAsFactors = FALSE
  )

  list(
    holdout_summary = holdout_summary_row,
    holdout_metrics = holdout_metrics_rows,
    cv_summary = cv_summary_rows,
    cv_folds = cv_fold_rows
  )
}

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) >= 1) tolower(args[1]) else "both"

mode_paths <- list(
  impute = "outputs/cleaned_data_impute.csv",
  drop = "outputs/cleaned_data_drop.csv"
)

jobs <- list()

if (mode %in% c("impute", "drop")) {
  jobs[[mode]] <- mode_paths[[mode]]
} else if (mode == "both") {
  jobs <- mode_paths
} else if (mode == "custom") {
  if (length(args) < 2) {
    stop("For custom mode, use: Rscript run_models_compare.R custom <path_to_csv>")
  }
  jobs[[basename(args[2])]] <- args[2]
} else if (file.exists(args[1])) {
  jobs[[basename(args[1])]] <- args[1]
} else {
  stop("Usage: Rscript run_models_compare.R [impute|drop|both|custom <csv_path>|<csv_path>]")
}

run_outputs <- lapply(names(jobs), function(label) run_for_dataset(label, jobs[[label]]))

holdout_summary <- do.call(rbind, lapply(run_outputs, function(x) x$holdout_summary))
holdout_metrics_table <- do.call(rbind, lapply(run_outputs, function(x) x$holdout_metrics))
cv_summary_table <- do.call(rbind, lapply(run_outputs, function(x) x$cv_summary))
cv_fold_table <- do.call(rbind, lapply(run_outputs, function(x) x$cv_folds))

if (!dir.exists("outputs")) {
  dir.create("outputs", recursive = TRUE)
}

summary_path <- "outputs/model_compare_summary.csv"
write.csv(holdout_summary, summary_path, row.names = FALSE)

metrics_path <- "outputs/model_metrics_table.csv"
write.csv(holdout_metrics_table, metrics_path, row.names = FALSE)

cv_summary_path <- "outputs/model_cv_summary.csv"
write.csv(cv_summary_table, cv_summary_path, row.names = FALSE)

cv_folds_path <- "outputs/model_cv_folds.csv"
write.csv(cv_fold_table, cv_folds_path, row.names = FALSE)

cat("\nSaved comparison summary to:", summary_path, "\n")
cat("Saved model metrics table to:", metrics_path, "\n")
cat("Saved CV summary to:", cv_summary_path, "\n")
cat("Saved CV fold-level table to:", cv_folds_path, "\n")