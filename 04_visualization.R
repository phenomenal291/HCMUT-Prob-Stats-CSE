
source("03_process.R")

if (!dir.exists(PLOT_DIR)) dir.create(PLOT_DIR, recursive = TRUE)

add_data <- processed_pipelines$impute$analysis

png(file.path(PLOT_DIR, "Width_hist.png"))
hist(add_data$Width,
      main = "Histogram of Width",
      xlab = "Width",
      col = "salmon",
      border = "black")
dev.off()

png(file.path(PLOT_DIR, "Height_hist.png"))
hist(add_data$Height,
      main = "Histogram of Height",
      breaks = 20,
      xlab = "Height",
      col = "skyblue",
      border = "black")
dev.off()

png(file.path(PLOT_DIR, "Aspect_Ratio_hist.png"))
hist(add_data$Aspect_Ratio,
      main = "Histogram of Aspect Ratio",
      xlab = "Aspect Ratio",
      col = "lightgreen",
      border = "black")
dev.off()

num_data <- add_data[, c("Width", "Height", "Aspect_Ratio")]
cor_mat <- cor(num_data, use = "complete.obs")

png(file.path(PLOT_DIR, "Correlation_Heatmap.png"))
heatmap(cor_mat,
        Rowv = NA,
        Colv = NA,
        scale = "none",
        main = "Correlation Heatmap")
dev.off()

png(file.path(PLOT_DIR, "Width_Height_Scatter.png"))
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
model_height <- glm(isAds ~ Height, data = add_data, family = "binomial")
plot(add_data$Height, add_data$isAds,
      col = ifelse(add_data$IsAds == "ads", "red", "blue"),
      pch = 19,
      xlab = "Height",
      ylab = "isAds",
      main = "Height vs isAds")
      col = rgb(1, 0, 0, add_data$Height / max(add_data$Height)),
      pch = 19)
