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
#'   Treatment = rep(LETTERS[1:4], 3)
#' )
#'
#' # Optimize the design
#' result <- speed(df, permute = ~Treatment)
#'
#' # Plot the optimized design
#' plot_design(result)
#'
#' # Plot with custom treatment variable name
#' plot_design(result$design_df, treatment_var = "Treatment")
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




#' Generate plots for designs generated in speed
#'
#' @param object An object to create a plot for. Currently objects from the [multiple_comparisons()] or [design()] functions with class "mct" or "design" respectively are supported.
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param rotation Rotate the x axis labels and the treatment group labels within the plot. Allows for easier reading of long axis or treatment labels. Number between 0 and 360 (inclusive) - default 0
#' @param margin Logical (default `FALSE`). A value of `FALSE` will expand the plot to the edges of the plotting area i.e. remove white space between plot and axes.
#' @param palette A string specifying the colour scheme to use for plotting or a vector of custom colours to use as the palette. Default is equivalent to "Spectral". Colour blind friendly palettes can also be provided via options `"colour blind"` (or `"color blind"`, both equivalent to `"viridis"`), `"magma"`, `"inferno"`, `"plasma"`, `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`. Other palettes from [scales::brewer_pal()] are also possible.
#' @param buffer A string specifying the buffer plots to include for plotting. Default is `NULL` (no buffers plotted). Other options are "edge" (outer edge of trial area), "rows" (between rows), "columns" (between columns), "double row" (a buffer row each side of a treatment row) or "double column" (a buffer row each side of a treatment column). "blocks" (a buffer around each treatment block) will be implemented in a future release.
#' @param row A variable to plot a column from `object` as rows.
#' @param column A variable to plot a column from `object` as columns.
#' @param block A variable to plot a column from `object` as blocks.
#' @param treatments A variable to plot a column from `object` as treatments.
#' @inheritParams rlang::args_dots_used
#'
#' @name autoplot
#'
#' @returns A `ggplot2` object.
#' @seealso [speed()]
#'
NULL

#' @rdname autoplot
#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot



#' @rdname autoplot
#'
#' @importFrom farver decode_colour
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 ggplot geom_tile aes geom_text theme_bw scale_fill_manual scale_x_continuous scale_y_continuous scale_y_reverse
#' @importFrom scales brewer_pal reverse_trans viridis_pal
#' @importFrom stringi stri_sort
#' @importFrom rlang check_dots_used enquo sym quo_is_null quo_name
#' @export
#' @examples
#' des.out <- design(type = "crd", treatments = c(1, 5, 10, 20),
#'                   reps = 5, nrows = 4, ncols = 5, seed = 42, plot = FALSE)
#' autoplot(des.out)
#'
#' # Colour blind friendly colours
#' autoplot(des.out, palette = "colour-blind")
#'
#' # Alternative colour scheme
#' autoplot(des.out, palette = "plasma")
#'
#' # Custom colour palette
#' autoplot(des.out, palette = c("#ef746a", "#3fbfc5", "#81ae00", "#c37cff"))
#'
#' # Visualise different components of a split plot design
#' des.out <- design(type = "split", treatments = c("A", "B"), sub_treatments = 1:4,
#' reps = 4, nrows = 8, ncols = 4, brows = 4, bcols = 2, seed = 42)
#'
#' # Show the wholeplot components
#' autoplot(des.out, treatments = wholeplots)
#'
#' # Display block level
#' autoplot(des.out, treatments = block)
autoplot.design <- function(object, rotation = 0, size = 4, margin = FALSE, palette = "default", buffer = NULL, row = NULL, column = NULL, block = NULL, treatments = NULL, ...) {
    stopifnot(inherits(object, "design"))
    rlang::check_dots_used()

    if(inherits(object, "list")) {
        object <- object$design
    }

    row_expr <- rlang::enquo(row)
    column_expr <- rlang::enquo(column)
    block_expr <- rlang::enquo(block)
    trt_expr <- rlang::enquo(treatments)

    # If row and column are not provided, set default values
    if(rlang::quo_is_null(row_expr)) {
        row_expr <- rlang::sym("row")  # Default to the row column
    }
    if(rlang::quo_is_null(column_expr)) {
        column_expr <- rlang::sym("col")  # Default to the col column
    }
    if(rlang::quo_is_null(block_expr)) {
        block_expr <- rlang::sym("block")  # Default to the block column
    }
    if(rlang::quo_is_null(trt_expr)) {
        trt_expr <- rlang::sym("treatments")  # Default to the treatments column
    }

    row_expr <- rlang::quo_name(row_expr)
    column_expr <- rlang::quo_name(column_expr)
    block_expr <- rlang::quo_name(block_expr)
    trt_expr <- rlang::quo_name(trt_expr)

    object[[trt_expr]] <- factor(as.character(object[[trt_expr]]), levels = unique(stringi::stri_sort(as.character(object[[trt_expr]]), numeric = TRUE)))
    ntrt <- nlevels(object[[trt_expr]])

    # create the colours for the graph
    if(length(palette) > 1) {
        # Assume custom palette colours are being passed in
        if(length(palette) != ntrt) {
            stop("palette needs to be a single string to choose a predefined palette, or ", ntrt, " custom colours.")
        }
        colour_palette <- palette
    }
    else {
        if(palette == "default") {
            colour_palette <- grDevices::colorRampPalette(scales::brewer_pal(palette = "Spectral")(11))(ntrt)
        }
        else if(palette %in% c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy",
                               "RdYlBu", "RdYlGn", "Spectral", "Set3", "Paired")) {
            colour_palette <- grDevices::colorRampPalette(scales::brewer_pal(palette = palette)(11))(ntrt)
        }
        else if(any(grepl("(colou?r([[:punct:]]|[[:space:]]?)blind)|cb|viridis", palette, ignore.case = T))) {
            colour_palette <- scales::viridis_pal(option = "viridis")(ntrt)
        }
        else if(tolower(trimws(palette)) %in% c("magma", "inferno", "cividis", "plasma", "rocket", "mako", "turbo")) {
            colour_palette <- scales::viridis_pal(option = palette)(ntrt)
        }
        else {
            stop("Invalid value for palette.", call. = FALSE)
        }
    }

    hcl <- farver::decode_colour(colour_palette, "rgb", "hcl")
    colours <- data.frame(treatments = levels(object[[trt_expr]]),
                          text_col = ifelse(hcl[, "l"] > 50, "black", "white"))
    colnames(colours)[1] <- trt_expr
    object <- merge(object, colours)

    if(!any(grepl("block", tolower(names(object))))) {
        if(!missing(buffer)) {
            object <- create_buffers(object, type = buffer)
            if("buffer" %in% levels(object[[trt_expr]])) {
                colour_palette <- c(colour_palette, "white")
            }
        }

        # create the graph
        plt <- ggplot2::ggplot() +
            ggplot2::geom_tile(data = object, mapping = ggplot2::aes(x = .data[[column_expr]], y = .data[[row_expr]], fill = .data[[trt_expr]]), colour = "black") +
            ggplot2::geom_text(data = object, mapping = ggplot2::aes(x = .data[[column_expr]], y = .data[[row_expr]], label = .data[[trt_expr]]), colour = object$text_col, angle = rotation, size = size, ...) +
            ggplot2::theme_bw()
    }
    else {
        # Set up dataframe with coordinates for drawing the blocks
        blkdf <- data.frame(
            block = sort(unique(object[[block_expr]])),
            xmin = 0, xmax = 0, ymin = 0, ymax = 0
        )
        if(!missing(buffer)) {
            object <- create_buffers(object, type = buffer, blocks = TRUE)
            if("buffer" %in% levels(object[[trt_expr]])) {
                colour_palette <- c(colour_palette, "white")
            }
        }
        for (i in 1:nrow(blkdf)) {
            tmp <- object[object[[block_expr]] == blkdf$block[i], ]
            blkdf[i, "ymin"] <- (min(tmp$row) - 0.5)
            blkdf[i, "ymax"] <- (max(tmp$row) + 0.5)
            blkdf[i, "xmin"] <- (min(tmp$col) - 0.5)
            blkdf[i, "xmax"] <- (max(tmp$col) + 0.5)
        }

        plt <- ggplot2::ggplot(...) +
            ggplot2::geom_tile(data = object, mapping = ggplot2::aes(x = .data[[column_expr]], y = .data[[row_expr]], fill = .data[[trt_expr]]), colour = "black") +
            ggplot2::geom_text(data = object, mapping = ggplot2::aes(x = .data[[column_expr]], y = .data[[row_expr]], label = .data[[trt_expr]]), colour = object$text_col, angle = rotation, size = size, ...) +
            ggplot2::geom_rect(
                data = blkdf,
                mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                linewidth = 1.8, colour = "black", fill = NA
            ) +
            ggplot2::geom_rect(
                data = blkdf,
                mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                linewidth = 0.6, colour = "white", fill = NA
            ) +
            ggplot2::theme_bw()
    }

    plt <- plt + scale_fill_manual(values = colour_palette, name = tools::toTitleCase(trt_expr))

    if(!margin) {
        plt <- plt + ggplot2::scale_x_continuous(expand = c(0, 0), breaks = seq(1, max(object[[column_expr]]), 1)) + ggplot2::scale_y_continuous(expand = c(0, 0), trans = scales::reverse_trans(), breaks = seq(1, max(object[[row_expr]]), 1))
    }
    else {
        plt <- plt + ggplot2::scale_x_continuous(breaks = seq(1, max(object[[column_expr]]), 1))+ ggplot2::scale_y_continuous(trans = scales::reverse_trans(), breaks = seq(1, max(object[[row_expr]]), 1))
    }

    return(plt)
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
#'   Treatment = rep(LETTERS[1:3], 3)
#' )
#'
#' # Optimize the design
#' result <- speed(df, permute = ~Treatment)
#'
#' # Plot optimization progress
#' plot_progress(result)
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
#' @export
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
