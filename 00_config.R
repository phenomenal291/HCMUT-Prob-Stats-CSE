DATA_FILE <- "add.csv"
OUTPUT_DIR <- "outputs"
PLOT_DIR <- file.path(OUTPUT_DIR, "plots")
IQR_K <- 1.5
MIN_PREVALENCE <- 0.01
MAX_PREVALENCE <- 0.99
TOP_BINARY_PLOTS <- 15L

suppressPackageStartupMessages(library(ggplot2))
