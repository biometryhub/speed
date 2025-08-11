calculate_incidence <- function(df, row_col = "row", col_col = "col", var_col = "treatment") {

  # Input validation
  if (!all(c(row_col, col_col, var_col) %in% names(df))) {
    stop("Specified columns not found in dataframe")
  }

  # Create contingency tables
  row_incidence <- table(df[[row_col]], df[[var_col]])
  col_incidence <- table(df[[col_col]], df[[var_col]])

  # Convert to data frames for easier manipulation
  row_df <- as.data.frame.matrix(row_incidence)
  col_df <- as.data.frame.matrix(col_incidence)

  # Add row/col identifiers
  row_df$position <- as.numeric(rownames(row_df))
  col_df$position <- as.numeric(rownames(col_df))

  # Reshape to long format for easier analysis
  row_long <- reshape2::melt(row_df, id.vars = "position",
                            variable.name = "variable",
                            value.name = "count")
  row_long$type <- "row"

  col_long <- reshape2::melt(col_df, id.vars = "position",
                            variable.name = "variable",
                            value.name = "count")
  col_long$type <- "col"

  # Combine results
  result <- rbind(row_long, col_long)

  # Return list with multiple formats
  return(list(
    row_incidence = row_incidence,
    col_incidence = col_incidence,
    row_matrix = row_df,
    col_matrix = col_df,
    combined_long = result,
    summary = list(
      total_by_row = rowSums(row_incidence),
      total_by_col = rowSums(col_incidence),
      total_by_variable = colSums(row_incidence)
    )
  ))
}

# Alternative function for proportions instead of counts
calculate_incidence_prop <- function(df, row_col = "row", col_col = "col", var_col = "treatment") {
  result <- calculate_incidence(df, row_col, col_col, var_col)

  # Convert counts to proportions
  row_prop <- prop.table(result$row_incidence, margin = 1)
  col_prop <- prop.table(result$col_incidence, margin = 1)

  return(list(
    row_proportions = row_prop,
    col_proportions = col_prop,
    row_counts = result$row_incidence,
    col_counts = result$col_incidence
  ))
}

# # Example usage with your data
# df <- data.frame(
#   row = rep(1:10, times = 6),
#   col = rep(1:6, each = 10),
#   treatment = rep(LETTERS[1:10], 6)
# )
#
# # Calculate incidence
# incidence_result <- calculate_incidence(df)
#
# # Display results
# print("Row incidence (treatments by row):")
# print(incidence_result$row_incidence)
#
# print("\nColumn incidence (treatments by column):")
# print(incidence_result$col_incidence)
#
# print("\nSummary statistics:")
# print(incidence_result$summary)
#
# # Calculate proportions
# prop_result <- calculate_incidence_prop(df)
# print("\nRow proportions:")
# print(prop_result$row_proportions)

# Visualization function (requires ggplot2)
plot_incidence <- function(incidence_result, type = "both") {
  if (!require(ggplot2, quietly = TRUE)) {
    stop("ggplot2 package required for plotting")
  }

  library(ggplot2)

  if (type == "row" || type == "both") {
    row_plot <- ggplot(incidence_result$combined_long[incidence_result$combined_long$type == "row",],
                       aes(x = position, y = count, fill = variable)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Variable Incidence by Row", x = "Row", y = "Count") +
      theme_minimal()

    if (type == "row") return(row_plot)
  }

  if (type == "col" || type == "both") {
    col_plot <- ggplot(incidence_result$combined_long[incidence_result$combined_long$type == "col",],
                       aes(x = position, y = count, fill = variable)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Variable Incidence by Column", x = "Column", y = "Count") +
      theme_minimal()

    if (type == "col") return(col_plot)
  }

  if (type == "both") {
    if (require(gridExtra, quietly = TRUE)) {
      return(gridExtra::grid.arrange(row_plot, col_plot, ncol = 1))
    } else {
      print(row_plot)
      print(col_plot)
    }
  }
}
