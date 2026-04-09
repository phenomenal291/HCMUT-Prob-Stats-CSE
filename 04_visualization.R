source("02_loadClean.R")
source("03_process.R")
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

# Histogram for na and non-ads count
png(file.path(PLOT_DIR, "00_isAds_Count_Before_Processing.png"), width = 1600, height = 1600, res = 200)
barplot(table(num_table$IsAds),
      main = "Number of Ads and Non-Ads",
      xlab = "isAds",
      ylab = "Count",
      ylim = c(0, 3000),
      col = c("lightblue", "salmon"))
dev.off()

# preprocessing visualization of numeric features
png(file.path(PLOT_DIR, "01_Width_before_processing_hist.png"), width = 1600, height = 1600, res = 200)
hist(num_table$Width,
      main = "Histogram of Width",
      xlab = "Width",
      col = "salmon",
      border = "black")
dev.off()

png (file.path(PLOT_DIR, "02_Height_before_processing_hist.png"), width = 1600, height = 1600, res = 200)
hist(num_table$Height,
      main = "Histogram of Height",
      xlab = "Height",
      col = "skyblue",
      border = "black")
dev.off()

png(file.path(PLOT_DIR, "03_Aspect_Ratio_before_processing_hist.png"), width = 1600, height = 1600, res = 200)
hist(num_table$Aspect_Ratio,
      main = "Histogram of Aspect Ratio",
      xlab = "Aspect Ratio",
      col = "lightgreen",
      border = "black")
dev.off()

png(file.path(PLOT_DIR, "04_Scatter_Width_Height_before_processing.png"), width = 1600, height = 1600, res = 200)
plot(num_table$Width, num_table$Height,
      col = ifelse(num_table$IsAds == "ads", "red", "blue"),
      pch = 19,
      xlab = "Width",
      ylab = "Height",
      main = "Width vs Height")
legend("topright",
      legend = c("ads", "non-ads"),
      col = c("red", "blue"),
      pch = 19)
dev.off()




# After processing visualization of numeric features
add_data <- after_processed_list$impute$analysis

# Histogram for ads and non-ads count after processing
png(file.path(PLOT_DIR, "10_isAds_Count_After_Processing.png"), width = 1600, height = 1600, res = 200)
barplot(table(add_data$IsAds),
      main = "Number of Ads vs Non-Ads",
      xlab = "isAds",
      ylab = "Count",
      ylim = c(0, 3000),
      col = c("lightblue", "salmon"))
dev.off()


png(file.path(PLOT_DIR, "11_Width_after_processing_hist.png"), width = 1600, height = 1600, res = 250)
hist(add_data$Width,
      main = "Histogram of Width",
      xlab = "Width",
      xlim = c(0, 150),
      ylab = "Frequency",
      ylim = c(0, 1200),
      col = "salmon",
      border = "black")
dev.off()

png(file.path(PLOT_DIR, "12_Height_after_processing_hist.png"), width = 1600, height = 1600, res = 200)
hist(add_data$Height,
      main = "Histogram of Height",
      xlab = "Height",
      col = "skyblue",
      border = "black")
dev.off()

png(file.path(PLOT_DIR, "13_Aspect_Ratio_after_processing_hist.png"), width = 1600, height = 1600, res = 200)
hist(add_data$Aspect_Ratio,
      main = "Histogram of Aspect Ratio",
      xlab = "Aspect Ratio",
      col = "lightgreen",
      border = "black")
dev.off()

num_data <- add_data[, c("Width", "Height", "Aspect_Ratio")]
cor_mat <- cor(num_data, use = "complete.obs")

png(file.path(PLOT_DIR, "14_Correlation_Heatmap_after_processing.png"), height = 1600, width = 1200, res = 200)
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
dev.off()

png(file.path(PLOT_DIR, "15_Width_Height_Scatter.png"), width = 1600, height = 1600, res = 200)
plot(add_data$Width, add_data$Height,
      col = ifelse(add_data$IsAds == "ads", "red", "blue"),
      pch = 19,
      xlab = "Width",
      ylab = "Height",
      main = "Width vs Height")
legend("topright",
      legend = c("ads", "non-ads"),
      col = c("red", "blue"),
      pch = 19)
dev.off()

# Binary feature distribution
model_height <- glm(y ~ Height, data = add_data, family = "binomial")
model_width <- glm(y ~ Width, data = add_data, family = "binomial")
model_aspect_ratio <- glm(y ~ Aspect_Ratio, data = add_data, family = "binomial")

# Boxplot for numeric features by IsAds
png(file.path(PLOT_DIR, "16_Boxplot_Width_after_processing_by_IsAds.png"), width = 1800, height = 1600, res = 250)
boxplot(Width ~ IsAds, data = add_data,
            main = "Boxplot of Width by IsAds",
            xlab = "IsAds",
            ylab = "Width",
            col = c("lightblue", "salmon"))
dev.off()

png(file.path(PLOT_DIR, "17_Boxplot_Height_after_processing_by_IsAds.png"), width = 1800, height = 1600, res = 250)
boxplot(Height ~ IsAds, data = add_data,
            main = "Boxplot of Height by IsAds",
            xlab = "IsAds",
            ylab = "Height",
            col = c("lightblue", "salmon"))
dev.off()

# Scatter plot + Logistic regression line
png(file.path(PLOT_DIR, "18_Scatter_Height_isAds.png"), width = 1600, height = 1600, res = 200)
plot(
      add_data$Height, add_data$y,
      col = ifelse(add_data$IsAds == "ads", "red", "blue"),
      pch = 20,
      xlab = "Height",
      ylab = "isAds",
      main = "Height vs isAds with Logistic Fit"
) 
curve(predict(model_height, data.frame(Height = x), type = "response"),
      add = TRUE,
      col = "darkgreen",
      lwd = 2)
legend("topright",
      legend = c("ads", "non-ads"),
      col = c("red", "blue"),
      pch = 20)
dev.off()
png(file.path(PLOT_DIR, "19_Scatter_Width_isAds.png"), width = 1600, height = 1600, res = 200)
plot(
      add_data$Width, add_data$y,
      col = ifelse(add_data$IsAds == "ads", "red", "blue"),
      pch = 20,
      xlab = "Width",
      ylab = "isAds",
      main = "Width vs isAds with Logistic Fit"
) 
curve(predict(model_width, data.frame(Width = x), type = "response"),
      add = TRUE,
      col = "darkgreen",
      lwd = 2)
legend("topright",
      legend = c("ads", "non-ads"),
      col = c("red", "blue"),
      pch = 20)
dev.off()
png(file.path(PLOT_DIR, "110_Scatter_Aspect_Ratio_isAds.png"), width = 1600, height = 1600, res = 200)
plot(
      add_data$Aspect_Ratio, add_data$y,
      col = ifelse(add_data$IsAds == "ads", "red", "blue"),
      pch = 20,
      xlab = "Aspect Ratio",
      ylab = "isAds",
      main = "Aspect Ratio vs isAds with Logistic Fit"
) 
curve(predict(model_aspect_ratio, data.frame(Aspect_Ratio = x), type = "response"),
      add = TRUE,
      col = "darkgreen",
      lwd = 2)
legend("topright",
      legend = c("ads", "non-ads"),
      col = c("red", "blue"),
      pch = 20)
dev.off()

# Top 20 binary features by prevalence after processing
binary_cols_after <- setdiff(names(add_data), c(".row_id", "Width", "Height", "Aspect_Ratio", "IsAds", "y"))
binary_prevalence <- sapply(add_data[, binary_cols_after, drop = FALSE], function(col) mean(col == 1, na.rm = TRUE))
top_binary_cols <- names(sort(binary_prevalence, decreasing = TRUE))[1:min(TOP_BINARY_PLOTS, length(binary_prevalence))]
top_binary_prevalence <- sort(binary_prevalence, decreasing = TRUE)[1:min(TOP_BINARY_PLOTS, length(binary_prevalence))]

png(file.path(PLOT_DIR, "111_Binary_Feature_Distribution_after_processing.png"), width = 1600, height = 1600, res = 200)
barplot(top_binary_prevalence,
      names.arg = top_binary_cols,
      main = "Top Binary Indicators",
      xlab = "Binary Features",
      ylab = "Prevalence",
      las = 2,
      cex.names = 0.8,
      ylim = c(0, max(top_binary_prevalence) * 1.1),
      col = "lightgreen")
dev.off()
