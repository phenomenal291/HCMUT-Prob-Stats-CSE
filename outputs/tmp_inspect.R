exprs <- parse('Final.R')
env <- new.env(parent = globalenv())
for (i in seq_along(exprs)) {
  txt <- paste(deparse(exprs[[i]]), collapse=' ')
  if (grepl('plot_class_count\(num_table', txt, fixed = FALSE)) break
  eval(exprs[[i]], envir = env)
}
add_data <- env$after_processed_list$impute$analysis
cat('ROWS=', nrow(add_data), '\n', sep='')
cat('WIDTH_CLASS=', class(add_data$Width), '\n', sep='')
cat('WIDTH_FINITE=', sum(is.finite(add_data$Width)), '\n', sep='')
cat('WIDTH_RANGE=', paste(range(add_data$Width, na.rm = TRUE), collapse=', '), '\n', sep='')
cat('HEIGHT_RANGE=', paste(range(add_data$Height, na.rm = TRUE), collapse=', '), '\n', sep='')
cat('ASPECT_RANGE=', paste(range(add_data$Aspect_Ratio, na.rm = TRUE), collapse=', '), '\n', sep='')
