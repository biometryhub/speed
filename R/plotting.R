#' Plot Experimental Design
#'
#' @description
#' Creates a visual representation of an experimental design layout, with options to highlight blocks
#' and customize the appearance.
#'
#' @param design_result Either the result from speed() function or a design matrix
#' @param treatment_var The name of the treatment variable (required if design_result is a dataframe)
#' @param block_var Optional name of the blocking variable to highlight with boundaries
#' @param title Plot title (default: "Experimental Design")
#' @param row_var Name of the row coordinate variable (default: "row")
#' @param col_var Name of the column coordinate variable (default: "col")
#'
#' @return A ggplot object displaying the experimental design
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_viridis_d labs theme_minimal
#' @importFrom ggplot2 theme element_text coord_equal scale_x_continuous scale_y_continuous
#'
#' @examples
#' # Create a simple design
#' df <- data.frame(
#'   row = rep(1:4, each = 3),
#'   col = rep(1:3, times = 4),
#'   treatment = rep(LETTERS[1:4], 3)
#' )
#'
#' # Optimize the design
#' result <- speed(df, "treatment")
#'
#' # Plot the optimized design
#' plot_design(result)
#'
#' # Plot with custom treatment variable name
#' plot_design(result$design_df, treatment_var = "treatment")
#'
#' @export
plot_design <- function(design_result, treatment_var = NULL, block_var = NULL, 
                        title = "Experimental Design", row_var = "row", col_var = "col") {
  
  # Handle different input types
  if (is.list(design_result) && "design_df" %in% names(design_result)) {
    # Input is the result of speed() function
    design_df <- design_result$design_df
    
    # Try to determine treatment variable if not specified
    if (is.null(treatment_var)) {
      # Look for variables other than row, col, and block variables
      possible_treatments <- setdiff(names(design_df), c(row_var, col_var, block_var))
      if (length(possible_treatments) == 1) {
        treatment_var <- possible_treatments[1]
      } else if ("permute" %in% names(design_result)) {
        # Extract treatment variable from the permute formula
        treatment_var <- all.vars(design_result$permute)[1]
      } else {
        stop("Treatment variable cannot be automatically determined. Please specify treatment_var.")
      }
    }
  } else if (is.matrix(design_result)) {
    # Input is a design matrix
    nrows <- nrow(design_result)
    ncols <- ncol(design_result)
    design_df <- expand.grid(row = 1:nrows, col = 1:ncols)
    names(design_df)[names(design_df) == "row"] <- row_var
    names(design_df)[names(design_df) == "col"] <- col_var
    design_df[[treatment_var]] <- as.vector(design_result)
  } else if (is.data.frame(design_result)) {
    # Input is a data frame
    design_df <- design_result
    if (is.null(treatment_var)) {
      stop("Treatment variable must be specified when input is a data frame.")
    }
  } else {
    stop("design_result must be a speed() result, a matrix, or a data frame.")
  }
  
  # Verify required columns exist
  if (!all(c(row_var, col_var, treatment_var) %in% names(design_df))) {
    stop("Required columns not found in the data frame.")
  }
  
  # Convert row and column to factors for proper ordering in plot
  max_row <- max(design_df[[row_var]])
  max_col <- max(design_df[[col_var]])
  
  p <- ggplot2::ggplot(design_df, ggplot2::aes(x = .data[[col_var]], 
                                               y = -as.numeric(.data[[row_var]]), 
                                               fill = factor(.data[[treatment_var]]))) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = .data[[treatment_var]]), size = 3) +
    ggplot2::scale_fill_viridis_d() +
    ggplot2::labs(title = title, fill = treatment_var) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 10),
                   axis.title = ggplot2::element_text(size = 12),
                   plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
                   legend.text = ggplot2::element_text(size = 10),
                   legend.title = ggplot2::element_text(size = 12)) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous(breaks = 1:max_col) +
    ggplot2::scale_y_continuous(breaks = -1:-max_row,
                                labels = 1:max_row)
  
  # Add block outlines if block variable is provided
  if (!is.null(block_var) && block_var %in% names(design_df)) {
    # Create a matrix of blocks for boundary detection
    block_matrix <- matrix(NA, nrow = max_row, ncol = max_col)
    
    for (i in 1:nrow(design_df)) {
      r <- design_df[[row_var]][i]
      c <- design_df[[col_var]][i]
      block_matrix[r, c] <- design_df[[block_var]][i]
    }
    
    # Find boundaries between blocks
    block_boundaries <- data.frame()
    
    # Check horizontal boundaries
    for (i in 1:max_row) {
      for (j in 1:(max_col-1)) {
        if (!is.na(block_matrix[i, j]) && !is.na(block_matrix[i, j+1]) && 
            block_matrix[i, j] != block_matrix[i, j+1]) {
          block_boundaries <- rbind(block_boundaries,
                                   data.frame(x = j + 0.5, y = -i,
                                             xend = j + 0.5, yend = -(i-1)))
        }
      }
    }
    
    # Check vertical boundaries
    for (i in 1:(max_row-1)) {
      for (j in 1:max_col) {
        if (!is.na(block_matrix[i, j]) && !is.na(block_matrix[i+1, j]) && 
            block_matrix[i, j] != block_matrix[i+1, j]) {
          block_boundaries <- rbind(block_boundaries,
                                   data.frame(x = j, y = -(i + 0.5),
                                             xend = j + 1, yend = -(i + 0.5)))
        }
      }
    }
    
    # Add boundaries to plot
    if (nrow(block_boundaries) > 0) {
      p <- p + ggplot2::geom_segment(data = block_boundaries,
                                     ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                                     inherit.aes = FALSE,
                                     linewidth = 1.5, color = "red")
    }
  }
  
  return(p)
}




#' Plot Optimization Progress
#'
#' @description
#' Creates two plots showing the progression of the optimization:
#' 1. Objective score over iterations
#' 2. Temperature decay over iterations
#'
#' @param result A list containing the optimization results with the following elements:
#'   \itemize{
#'     \item scores - Numeric vector of objective scores for each iteration
#'     \item temperatures - Numeric vector of temperatures for each iteration
#'   }
#'
#' @return No return value; prints two ggplot objects showing:
#'   \itemize{
#'     \item Progress of the objective score
#'     \item Cooling schedule (temperature decay)
#'   }
#'
#' @examples
#' # Create a simple design
#' df <- data.frame(
#'   row = rep(1:3, each = 3),
#'   col = rep(1:3, times = 3),
#'   treatment = rep(LETTERS[1:3], 3)
#' )
#'
#' # Optimize the design
#' result <- speed(df, "treatment")
#'
#' # Plot optimization progress
#' plot_progress(result)
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
#' @export
plot_progress <- function(result) {
    # ... existing code ...
}
plot_progress <- function(result) {
  df <- data.frame(
    iteration = 1:length(result$scores),
    score = result$scores,
    temperature = result$temperatures
  )

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = iteration, y = score)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Objective Score Over Iterations",
         x = "Iteration", y = "Score") +
    ggplot2::theme_minimal()

  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = iteration, y = temperature)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Temperature Over Iterations",
         x = "Iteration", y = "Temperature") +
    ggplot2::theme_minimal()

  print(p1)
  print(p2)
}
