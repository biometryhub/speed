#' Generate plots for designs generated in speed
#'
#' @param object An experimental design object generated from [speed()].
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param rotation Rotate the x axis labels and the treatment group labels within the plot. Allows for easier reading of long axis or treatment labels. Number between 0 and 360 (inclusive) - default 0
#' @param margin Logical (default `FALSE`). A value of `FALSE` will expand the plot to the edges of the plotting area i.e. remove white space between plot and axes.
#' @param palette A string specifying the colour scheme to use for plotting or a vector of custom colours to use as the palette. Default is equivalent to "Spectral". Colour blind friendly palettes can also be provided via options `"colour blind"` (or `"color blind"`, both equivalent to `"viridis"`), `"magma"`, `"inferno"`, `"plasma"`, `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`. Other palettes from [scales::brewer_pal()] are also possible.
#' @param buffer A string specifying the buffer plots to include for plotting. Default is `NULL` (no buffers plotted). Other options are "edge" (outer edge of trial area), "rows" (between rows), "columns" (between columns), "double row" (a buffer row each side of a treatment row) or "double column" (a buffer row each side of a treatment column). "blocks" (a buffer around each treatment block) will be implemented in a future release.
#' @param block A variable to plot a column from `object` as blocks.
#' @param row A variable to plot a column from `object` as rows.
#' @param column A variable to plot a column from `object` as columns.
#' @param treatments A variable to plot a column from `object` as treatments.
#' @param legend Logical (default `FALSE`). If `TRUE`, displays the legend for treatment colors.
#' @inheritParams rlang::args_dots_used
#'
#' @name autoplot
#'
#' @return A `ggplot2` object.
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
#' @importFrom ggplot2 ggplot geom_tile aes geom_text theme_bw scale_fill_manual scale_x_continuous scale_y_continuous scale_y_reverse labs
#' @importFrom scales brewer_pal reverse_trans viridis_pal
#' @importFrom stringi stri_sort
#' @importFrom rlang check_dots_used enquo sym quo_is_null quo_name
#' @export
#' @examples
#' # Create a design with blocks
#' df <- data.frame(
#'      row = rep(1:6, each = 4),
#'      col = rep(1:4, times = 6),
#'      treatment = rep(LETTERS[1:8], 3),
#'      block = rep(1:3, each = 8))
#'
#' # Optimise while respecting blocks
#' result <- speed(df,
#'                 "treatment",
#'                 swap_within = "block",
#'                 seed = 42)
#'
#' # Plot the design with block boundaries
#' autoplot(result)
#'
#' # Show legend
#' autoplot(result, legend = TRUE)
#'
#' # Colour blind friendly colours
#' autoplot(result, palette = "colour-blind")
#'
#' # Alternative colour scheme
#' autoplot(result, palette = "plasma")
#'
#' # Custom colour palette
#' autoplot(result, palette = c("#ef746a", "#3fbfc5", "#81ae00", "#c37cff",
#'                              "#304702", "#dde024", "#630380ff", "#df7700"))
autoplot.design <- function(object,
                            rotation = 0,
                            size = 4,
                            margin = FALSE,
                            palette = "default",
                            row = NULL,
                            column = NULL,
                            block = NULL,
                            treatments = NULL,
                            legend = FALSE, ...) {
  stopifnot(inherits(object, "design"))
  rlang::check_dots_used()

  if(inherits(object, "list")) {
    object <- object$design_df
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
    trt_expr <- rlang::sym("treatment")  # Default to the treatments column
  }

  row_expr <- rlang::quo_name(row_expr)
  column_expr <- rlang::quo_name(column_expr)
  block_expr <- rlang::quo_name(block_expr)
  trt_expr <- rlang::quo_name(trt_expr)

  # Verify that required columns exist in the data
  verify_column_exists(row_expr, object, suffix = "Please specify the appropriate column using the 'row' argument.")
  verify_column_exists(column_expr, object, suffix = "Please specify the appropriate column using the 'column' argument.")
  verify_column_exists(trt_expr, object, suffix = "Please specify the appropriate column using the 'treatments' argument.")

  # Only verify block column if it's being used (i.e., if blocks exist in data)
  if(any(grepl("block", tolower(names(object))))) {
    verify_column_exists(block_expr, object, suffix = "Please specify the appropriate column using the 'block' argument.")
  }

  # Set up treatments and colours
  if("buffer" %in% as.character(object[[trt_expr]])) {
    # Separate treatments and buffers for proper ordering
    treatments_only <- unique(as.character(object[[trt_expr]]))
    treatments_only <- treatments_only[treatments_only != "buffer"]
    treatments_sorted <- stringi::stri_sort(treatments_only, numeric = TRUE)

    # Set factor levels with treatments first, then buffer at the end
    factor_levels <- c(treatments_sorted, "buffer")
    object[[trt_expr]] <- factor(as.character(object[[trt_expr]]), levels = factor_levels)
    ntrt <- length(treatments_sorted)  # Number of actual treatments (excluding buffer)
  } else {
    # Logic for designs without buffers
    object[[trt_expr]] <- factor(as.character(object[[trt_expr]]),
                                 levels = unique(stringi::stri_sort(as.character(object[[trt_expr]]), numeric = TRUE)))
    ntrt <- nlevels(object[[trt_expr]])
  }

  # Colour palette setup
  colour_palette <- .setup_colour_palette(palette, ntrt)

  # Check if buffers exist and adjust palette
  if("buffer" %in% levels(object[[trt_expr]])) {
    colour_palette <- c(colour_palette, "white")
  }

  # Text colour setup
  colours <- data.frame(treatments = levels(object[[trt_expr]]),
                        text_col = ifelse(.is_light_colour(colour_palette), "black", "white"))
  colnames(colours)[1] <- trt_expr
  object <- merge(object, colours)


  # Create plot based on whether blocks exist
  if(!any(grepl("block", tolower(names(object))))) {
    plt <- create_basic_plot(object, row_expr, column_expr, trt_expr, rotation, size, ...)
  } else {
    plt <- create_blocked_plot(object, row_expr, column_expr, block_expr, trt_expr, rotation, size, ...)
  }

  # Apply styling
  plt <- plt + scale_fill_manual(values = colour_palette, name = tools::toTitleCase(trt_expr))

  # Control legend visibility
  if (!legend) {
    plt <- plt + ggplot2::theme(legend.position = "none")
  }

  plt <- apply_axis_styling(plt, margin, object, row_expr, column_expr)

  return(plt)
}

#' @keywords internal
.setup_colour_palette <- function(palette, ntrt) {
  # Handle custom colour palettes (vector of colours)
  if(length(palette) > 1) {
    if(length(palette) != ntrt) {
      stop("palette needs to be a single string to choose a predefined palette, or ",
           ntrt, " custom colours.")
    }
    return(palette)
  }

  # Handle single string palette names
  palette <- tolower(trimws(palette))

  # Default viridis palette
  if(palette == "default") {
    return(scales::viridis_pal(option = "viridis")(ntrt))
  }

  # colourBrewer palettes
  brewer_palettes <- c("brbg", "piyg", "prgn", "puor", "rdbu", "rdgy",
                       "rdylbu", "rdylgn", "spectral", "set3", "paired")
  if(palette %in% brewer_palettes) {
    # Convert to proper case for scales::brewer_pal
    palette_proper <- switch(palette,
                             "brbg" = "BrBG",
                             "piyg" = "PiYG",
                             "prgn" = "PRGn",
                             "puor" = "PuOr",
                             "rdbu" = "RdBu",
                             "rdgy" = "RdGy",
                             "rdylbu" = "RdYlBu",
                             "rdylgn" = "RdYlGn",
                             "spectral" = "Spectral",
                             "set3" = "Set3",
                             "paired" = "Paired"
    )
    return(grDevices::colorRampPalette(scales::brewer_pal(palette = palette_proper)(11))(ntrt))
  }

  # colour blind friendly palettes (viridis family)
  viridis_patterns <- c("colou?r([[:punct:]]|[[:space:]]?)blind", "cb", "viridis")
  if(any(sapply(viridis_patterns, function(pattern) grepl(pattern, palette, ignore.case = TRUE)))) {
    return(scales::viridis_pal(option = "viridis")(ntrt))
  }

  # Other viridis options
  viridis_options <- c("magma", "inferno", "cividis", "plasma", "rocket", "mako", "turbo")
  if(palette %in% viridis_options) {
    return(scales::viridis_pal(option = palette)(ntrt))
  }

  # If we get here, the palette name is invalid
  valid_options <- c("default", brewer_palettes, "colour blind", "colour blind",
                     "cb", viridis_options)
  stop("Invalid value for palette. Valid options are: ",
       paste(valid_options, collapse = ", "),
       ", or a vector of ", ntrt, " custom colours.", call. = FALSE)
}


create_basic_plot <- function(object, row_expr, column_expr, trt_expr, rotation, size, ...) {
  # Separate buffer plots from treatment plots
  buffer_plots <- object[object[[trt_expr]] == "buffer", ]
  treatment_plots <- object[object[[trt_expr]] != "buffer", ]

  ggplot2::ggplot() +
    ggplot2::geom_tile(data = object,
                       mapping = ggplot2::aes(x = .data[[column_expr]],
                                              y = .data[[row_expr]],
                                              fill = .data[[trt_expr]]),
                       colour = "black") +
    # Only add text to non-buffer plots
    ggplot2::geom_text(data = treatment_plots,
                       mapping = ggplot2::aes(x = .data[[column_expr]],
                                              y = .data[[row_expr]],
                                              label = .data[[trt_expr]]),
                       colour = treatment_plots$text_col, angle = rotation, size = size, ...) +
    ggplot2::theme_bw()
}

create_blocked_plot <- function(object, row_expr, column_expr, block_expr, trt_expr, rotation, size, ...) {
  # Block boundary calculation
  blkdf <- calculate_block_boundaries(object, block_expr)

  # Separate buffer plots from treatment plots
  buffer_plots <- object[object[[trt_expr]] == "buffer", ]
  treatment_plots <- object[object[[trt_expr]] != "buffer", ]

  ggplot2::ggplot() +
    ggplot2::geom_tile(data = object,
                       mapping = ggplot2::aes(x = .data[[column_expr]],
                                              y = .data[[row_expr]],
                                              fill = .data[[trt_expr]]),
                       colour = "black") +
    # Only add text to non-buffer plots
    ggplot2::geom_text(data = treatment_plots,
                       mapping = ggplot2::aes(x = .data[[column_expr]],
                                              y = .data[[row_expr]],
                                              label = .data[[trt_expr]]),
                       colour = treatment_plots$text_col, angle = rotation, size = size, ...) +
    ggplot2::geom_rect(data = blkdf,
                       mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                       linewidth = 1.8, colour = "black", fill = NA) +
    ggplot2::geom_rect(data = blkdf,
                       mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                       linewidth = 0.6, colour = "white", fill = NA) +
    ggplot2::theme_bw()
}

apply_axis_styling <- function(plot, margin, object, row_expr, column_expr) {
  if(!margin) {
    # No margin - expand plot to edges with no white space
    plot <- plot + ggplot2::scale_x_continuous(expand = c(0, 0),
                                               breaks = seq(1, max(as_numeric_factor(object[[column_expr]])), 1)) +
      ggplot2::scale_y_continuous(expand = c(0, 0),
                                  trans = scales::reverse_trans(),
                                  breaks = seq(1, max(as_numeric_factor(object[[row_expr]])), 1))
  } else {
    # With margin - default ggplot spacing
    plot <- plot + ggplot2::scale_x_continuous(breaks = seq(1, max(as_numeric_factor(object[[column_expr]])), 1)) +
      ggplot2::scale_y_continuous(trans = scales::reverse_trans(),
                                  breaks = seq(1, max(as_numeric_factor(object[[row_expr]])), 1))
  }
  return(plot)
}

calculate_block_boundaries <- function(object, block_expr) {
  blkdf <- data.frame(
    block = sort(unique(object[[block_expr]])),
    xmin = 0, xmax = 0, ymin = 0, ymax = 0
  )

  for (i in 1:nrow(blkdf)) {
    tmp <- object[object[[block_expr]] == blkdf$block[i], ]
    blkdf[i, "ymin"] <- (min(as_numeric_factor(tmp$row)) - 0.5)
    blkdf[i, "ymax"] <- (max(as_numeric_factor(tmp$row)) + 0.5)
    blkdf[i, "xmin"] <- (min(as_numeric_factor(tmp$col)) - 0.5)
    blkdf[i, "xmax"] <- (max(as_numeric_factor(tmp$col)) + 0.5)
  }

  return(blkdf)
}


#' Determine if a colour is Light
#'
#' Internal helper function to determine whether a colour is light or dark
#' for appropriate font colour selection (black text on light backgrounds,
#' white text on dark backgrounds).
#'
#' @param colour A colour specification (hex code, named colour, etc.)
#' @return Logical. TRUE if the colour is light (luminance > 0.5), FALSE if dark.
#'
#' @details Uses standard luminance calculation: 0.299*R + 0.587*G + 0.114*B,
#'   normalized to 0-1 scale. Coefficients reflect human eye sensitivity to
#'   different colours (green > red > blue).
#'
#' @keywords internal
.is_light_colour <- function(colour) {
  # Convert vector of colours to RGB matrix (columns = colours)
  rgb_vals <- grDevices::col2rgb(colour)
  # Calculate luminance for each colour
  luminance <- (0.299 * rgb_vals[1, ] + 0.587 * rgb_vals[2, ] + 0.114 * rgb_vals[3, ]) / 255
  return(luminance > 0.5)
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
#' # Optimise the design
#' result <- speed(df, "treatment")
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

