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
#' @importFrom ggplot2 ggplot geom_tile aes geom_text theme_bw scale_fill_manual scale_x_continuous scale_y_continuous scale_y_reverse
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
#' # Set seed for reproducibility
#' set.seed(42)
#'
#' # Optimise while respecting blocks
#' result <- speed(df,
#'                 "treatment",
#'                 swap_within = "block",
#'                 iterations = 5000)
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
#' df <- data.frame(
#'       row = rep(1:4, each = 3),
#'       col = rep(1:3, times = 4),
#'       treatment = rep(LETTERS[1:4], 3))
#'
#' # Set seed for reproducibility
#' set.seed(42)
#'
#' # Optimise while respecting blocks
#' result <- speed(df,
#'                 "treatment",
#'                 iterations = 5000)
#'
#' # Custom colour palette
#' autoplot(result, palette = c("#ef746a", "#3fbfc5", "#81ae00", "#c37cff"))
autoplot.design <- function(object, rotation = 0, size = 4, margin = FALSE, palette = "default", buffer = NULL, row = NULL, column = NULL, block = NULL, treatments = NULL, legend = FALSE, ...) {
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
            colour_palette <- scales::viridis_pal(option = "viridis")(ntrt)
            # colour_palette <- grDevices::colorRampPalette(scales::brewer_pal(palette = "Spectral")(11))(ntrt)
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

    # Control legend visibility
    if (!legend) {
        plt <- plt + ggplot2::theme(legend.position = "none")
    }

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

