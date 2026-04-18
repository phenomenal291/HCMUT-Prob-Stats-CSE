source('outputs/tmp_preplots.R')
run_plot <- function(label, expr) {
  cat('RUNNING ', label, '...\n', sep='')
  tryCatch({
    eval(expr)
    cat('OK ', label, '\n', sep='')
  }, error = function(e) {
    cat('FAIL ', label, ': ', conditionMessage(e), '\n', sep='')
    quit(status = 1)
  })
}
add_data <- after_processed_list$impute$analysis
run_plot('before class count', quote(plot_class_count(num_table, '00_isAds_Count_Before_Processing.png', 'Number of Ads and Non-Ads')))
run_plot('before width hist', quote(plot_histogram(num_table$Width, '01_Width_before_processing_hist.png', 'Histogram of Width', 'Width', 'salmon')))
run_plot('before height hist', quote(plot_histogram(num_table$Height, '02_Height_before_processing_hist.png', 'Histogram of Height', 'Height', 'skyblue')))
run_plot('before aspect hist', quote(plot_histogram(num_table$Aspect_Ratio, '03_Aspect_Ratio_before_processing_hist.png', 'Histogram of Aspect Ratio', 'Aspect Ratio', 'lightgreen')))
run_plot('after class count', quote(plot_class_count(add_data, '10_isAds_Count_After_Processing.png', 'Number of Ads vs Non-Ads')))
run_plot('after width hist', quote(plot_histogram(add_data$Width, '11_Width_after_processing_hist.png', 'Histogram of Width', 'Width', 'salmon', width = 1600, height = 1600, res = 250, xlim = c(0, 150), ylim = c(0, 1200))))
run_plot('after height hist', quote(plot_histogram(add_data$Height, '12_Height_after_processing_hist.png', 'Histogram of Height', 'Height', 'skyblue')))
run_plot('after aspect hist', quote(plot_histogram(add_data$Aspect_Ratio, '13_Aspect_Ratio_after_processing_hist.png', 'Histogram of Aspect Ratio', 'Aspect Ratio', 'lightgreen')))
