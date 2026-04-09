source("02_loadClean.R")
source("03_process.R")
if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

# Circular plotting for na count
png(file.path(PLOT_DIR, "NA_Count_Circular.png"), width = 1600, height = 1800, res = 200)
barplot(
  num_na_count[["NA"]],
  main = "NA Count for Numeric Features",
  xlab = "Numeric Features",
  ylab = "NA Count",
  col = "lightcoral",
  border = "black",
  names.arg = num_na_count$Feature,
  las = 2
)
dev.off()

# After processing visualization of numeric features
add_data <- after_processed_list$impute$analysis

png(file.path(PLOT_DIR, "Width_hist.png"), width = 1600, height = 1600, res = 200)
hist(add_data$Width,
      main = "Histogram of Width",
      xlab = "Width",
      col = "salmon",
      border = "black")
dev.off()

png(file.path(PLOT_DIR, "Height_hist.png"), width = 1600, height = 1600, res = 200)
hist(add_data$Height,
      main = "Histogram of Height",
      xlab = "Height",
      col = "skyblue",
      border = "black")
dev.off()

png(file.path(PLOT_DIR, "Aspect_Ratio_hist.png"), width = 1600, height = 1600, res = 200)
hist(add_data$Aspect_Ratio,
      main = "Histogram of Aspect Ratio",
      xlab = "Aspect Ratio",
      col = "lightgreen",
      border = "black")
dev.off()

num_data <- add_data[, c("Width", "Height", "Aspect_Ratio")]
cor_mat <- cor(num_data, use = "complete.obs")

if (!requireNamespace("corrplot", quietly = TRUE)) {
  stop("Package 'corrplot' is required. Run install.packages('corrplot') first.")
}

png(file.path(PLOT_DIR, "Correlation_Heatmap.png"), height = 1600, width = 1200, res = 200)
corrplot::corrplot(
      cor_mat,
      method = "color",
      type = "upper",
      addCoef.col = "black",
      tl.col = "black",
      number.cex = 0.8,
      title = "Correlation Matrix",
      mar = c(0, 0, 1, 0)
)
dev.off()

png(file.path(PLOT_DIR, "Width_Height_Scatter.png"), width = 1600, height = 1600, res = 200)
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

# Boxplot for numeric features by IsAds
png(file.path(PLOT_DIR, "Boxplot_Width_by_IsAds.png"), width = 1800, height = 1600, res = 250)
boxplot(Width ~ IsAds, data = add_data,
            main = "Boxplot of Width by IsAds",
            xlab = "IsAds",
            ylab = "Width",
            col = c("lightblue", "salmon"))
dev.off()

png(file.path(PLOT_DIR, "Boxplot_Height_by_IsAds.png"))
boxplot(Height ~ IsAds, data = add_data,
            main = "Boxplot of Height by IsAds",
            xlab = "IsAds",
            ylab = "Height",
            col = c("lightblue", "salmon"))
dev.off()

# Scatter plot + Logistic regression line
png(file.path(PLOT_DIR, "Scatter_Height_isAds.png"), width = 1600, height = 1600, res = 200)
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
