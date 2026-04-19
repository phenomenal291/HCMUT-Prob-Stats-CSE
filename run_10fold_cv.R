# run_10fold_cv.R
suppressPackageStartupMessages(library(randomForest))

# Helper function to format output as [XX.XX] ± [X.XX]
format_res <- function(mean_val, std_val) {
  sprintf("%.2f ± %.2f", mean_val * 100, std_val * 100)
}

# 10-Fold CV Function
run_10_fold_cv <- function(csv_path, seed = 42) {
  if (!file.exists(csv_path)) {
    stop(paste("Dataset file not found:", csv_path))
  }
  
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # 1. Setup target and predictors (matching your existing logic)
  target_col <- if ("IsAds" %in% names(data)) "IsAds" else names(data)[ncol(data)]
  data[[target_col]] <- as.factor(data[[target_col]])
  
  # Ensure "non-ads" is the positive class (Level 2) for standard GLM behavior
  if (all(c("ads", "non-ads") %in% levels(data[[target_col]]))) {
    data[[target_col]] <- factor(data[[target_col]], levels = c("ads", "non-ads"))
  }
  positive_label <- "non-ads"
  
  # Drop ID and leakage columns
  exclude_cols <- c(".row_id", "y")
  predictor_cols <- setdiff(names(data), c(target_col, exclude_cols))
  data <- data[, c(predictor_cols, target_col), drop = FALSE]
  
  # 2. Create Stratified Folds (preserves class imbalance ratio)
  set.seed(seed)
  k <- 10
  folds <- integer(nrow(data))
  for (cls in levels(data[[target_col]])) {
    idx <- which(data[[target_col]] == cls)
    folds[idx] <- sample(rep(1:k, length.out = length(idx)))
  }
  
  # 3. Initialize storage for metrics
  res_log <- data.frame(Accuracy=numeric(k), Precision=numeric(k), Recall=numeric(k), F1=numeric(k))
  res_rf  <- data.frame(Accuracy=numeric(k), Precision=numeric(k), Recall=numeric(k), F1=numeric(k))
  
  # Helper to compute core metrics
  calc_metrics <- function(pred, actual, pos_class) {
    tp <- sum(pred == pos_class & actual == pos_class)
    fp <- sum(pred == pos_class & actual != pos_class)
    fn <- sum(pred != pos_class & actual == pos_class)
    tn <- sum(pred != pos_class & actual != pos_class)
    
    acc <- (tp + tn) / (tp + tn + fp + fn)
    prec <- ifelse((tp + fp) == 0, 0, tp / (tp + fp))
    rec <- ifelse((tp + fn) == 0, 0, tp / (tp + fn))
    f1 <- ifelse((prec + rec) == 0, 0, 2 * prec * rec / (prec + rec))
    
    c(Accuracy = acc, Precision = prec, Recall = rec, F1 = f1)
  }
  
  # 4. Cross-Validation Loop
  for (i in 1:k) {
    test_idx <- which(folds == i)
    train_data <- data[-test_idx, , drop = FALSE]
    test_data <- data[test_idx, , drop = FALSE]
    
    model_formula <- as.formula(paste(target_col, "~ ."))
    
    # --- Logistic Regression ---
    # suppressWarnings hides glm convergence warnings on smaller folds
    suppressWarnings({
      log_model <- glm(model_formula, data = train_data, family = "binomial")
    })
    log_prob <- predict(log_model, newdata = test_data, type = "response")
    # > 0.5 maps to level 2 ("non-ads")
    log_pred <- ifelse(log_prob > 0.5, levels(data[[target_col]])[2], levels(data[[target_col]])[1])
    
    # --- Random Forest ---
    rf_model <- randomForest(model_formula, data = train_data, ntree = 100)
    rf_pred <- predict(rf_model, newdata = test_data)
    
    actual <- test_data[[target_col]]
    
    # Store metrics for this fold
    res_log[i, ] <- calc_metrics(log_pred, actual, pos_class = positive_label)
    res_rf[i, ]  <- calc_metrics(rf_pred, actual, pos_class = positive_label)
  }
  
  # 5. Output formatted results
  cat("\n========================================\n")
  cat("10-Fold CV Results for Dataset:", basename(csv_path), "\n")
  cat("Format: Mean ± Std (%)\n")
  cat("========================================\n")
  
  cat("--- Logistic Regression ---\n")
  cat(sprintf("Accuracy:  %s\n", format_res(mean(res_log$Accuracy), sd(res_log$Accuracy))))
  cat(sprintf("Precision: %s\n", format_res(mean(res_log$Precision), sd(res_log$Precision))))
  cat(sprintf("Recall:    %s\n", format_res(mean(res_log$Recall), sd(res_log$Recall))))
  cat(sprintf("F1-Score:  %s\n", format_res(mean(res_log$F1), sd(res_log$F1))))
  
  cat("\n--- Random Forest ---\n")
  cat(sprintf("Accuracy:  %s\n", format_res(mean(res_rf$Accuracy), sd(res_rf$Accuracy))))
  cat(sprintf("Precision: %s\n", format_res(mean(res_rf$Precision), sd(res_rf$Precision))))
  cat(sprintf("Recall:    %s\n", format_res(mean(res_rf$Recall), sd(res_rf$Recall))))
  cat(sprintf("F1-Score:  %s\n", format_res(mean(res_rf$F1), sd(res_rf$F1))))
  cat("\n")
}

# Run the 10-fold CV on both cleaned pipeline files
run_10_fold_cv("outputs/cleaned_data_impute.csv")
run_10_fold_cv("outputs/cleaned_data_drop.csv")