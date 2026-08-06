#' Create buffers for design plots
#'
#' @param design The data frame of the design.
#' @param type The type of buffer. One of edge, row, column, double row, double column, or block (coming soon).
#' @param blocks Does the design data frame contain blocks?
#' @param treatment_cols Character vector of treatment column names to fill with "buffer".
#'   If NULL (default), uses "treatment" column if it exists.
#'
#' @importFrom stats setNames aggregate
#'
#' @returns The original data frame, updated to include buffers
#' @keywords internal
create_buffers <- function(
  design,
  type,
  blocks = FALSE,
  treatment_cols = NULL
) {
  nrow <- max(as_numeric_factor(design$row))
  ncol <- max(as_numeric_factor(design$col))

  # Each branch displaces the real plots' coordinates to make room for the
  # buffers, as `scale * coord + shift`. `tf` records that displacement next to
  # the line applying it so the two cannot drift apart; `add_buffers()` stores
  # it so `.drop_buffer_rows()` can invert it and recover the design's own
  # coordinates. Default is the identity, for the axis a buffer type leaves
  # alone.
  tf <- list(row = c(scale = 1, shift = 0), col = c(scale = 1, shift = 0))

  # Match edge, edges or e
  if (grepl("(^edges?$|^e$)", tolower(type))) {
    design$row <- design$row + 1
    design$col <- design$col + 1
    tf$row <- c(scale = 1, shift = 1)
    tf$col <- c(scale = 1, shift = 1)

    min_row <- min(design$row)
    min_col <- min(design$col)

    row <- c(rep(1, ncol + 2), rep(nrow + 2, ncol + 2), rep(2:(nrow + 1), 2))
    col <- c(rep(1:(ncol + 2), 2), rep(1, nrow), rep(ncol + 2, nrow))
    n_brow <- length(row) # Number of rows to create in the buffer dataframe
    treatment <- rep("buffer", n_brow)
  } else if (grepl("(^rows?$|^r$)", tolower(type))) {
    # Match row, rows, r
    design$row <- 2 * design$row
    tf$row <- c(scale = 2, shift = 0)

    min_row <- min(design$row)
    min_col <- min(design$col)

    row <- rep(seq(min_row - 1, (2 * nrow) + 1, by = 2), each = ncol)
    col <- rep(seq(1, ncol), times = nrow + 1)
    n_brow <- length(row) # Number of rows to create in the buffer dataframe
    treatment <- rep("buffer", n_brow)
  } else if (grepl("(^col(umn)?s?$|^c$)", tolower(type))) {
    # Match col, cols, column, columns or c
    design$col <- 2 * design$col
    tf$col <- c(scale = 2, shift = 0)

    min_row <- min(design$row)
    min_col <- min(design$col)

    row <- rep(seq(1, nrow), times = ncol + 1)
    col <- rep(seq(min_col - 1, (2 * ncol) + 1, by = 2), each = nrow)
    n_brow <- length(row) # Number of rows to create in the buffer dataframe
    treatment <- rep("buffer", n_brow)
  } else if (grepl("(^double rows?$|^dr$)", tolower(type))) {
    # Match double row, double rows, or dr
    design$row <- (3 * design$row) - 1
    tf$row <- c(scale = 3, shift = -1)

    min_row <- min(design$row)
    min_col <- min(design$col)

    row <- c(
      rep(seq(min_row - 1, (3 * nrow) - 2, by = 3), each = ncol),
      rep(seq(min_row + 1, (3 * nrow), by = 3), each = ncol)
    )
    col <- seq(min_col, ncol)
    n_brow <- length(row) # Number of rows to create in the buffer dataframe
    treatment <- rep("buffer", n_brow)
  } else if (grepl("(^double col(umn)?s?$|^dc$)", tolower(type))) {
    # Match double col, double cols, double column, double columns, dc
    design$col <- (3 * design$col) - 1
    tf$col <- c(scale = 3, shift = -1)

    min_row <- min(design$row)
    min_col <- min(design$col)

    row <- seq(min_row, nrow)
    col <- c(
      rep(seq(min_col - 1, (3 * ncol) - 2, by = 3), each = nrow),
      rep(seq(min_col + 1, (3 * ncol), by = 3), each = nrow)
    )
    n_brow <- length(col) # Number of rows to create in the buffer dataframe
    treatment <- rep("buffer", n_brow)
  } else if (grepl("(^blocks?$|^b$)", tolower(type))) {
    # Match block, blocks, or b
    stop("Block buffers are not yet supported.", call. = FALSE)
  } else {
    stop("Invalid buffer option: ", type, call. = FALSE)
  }

  buffers <- data.frame(matrix(NA, nrow = n_brow, ncol = ncol(design)))
  buffers <- stats::setNames(buffers, names(design))
  buffers$row <- row
  buffers$col <- col

  # Determine which treatment columns to fill with "buffer"
  if (is.null(treatment_cols)) {
    # Default: look for a column named "treatment"
    if ("treatment" %in% names(design)) {
      treatment_cols <- "treatment"
    } else {
      # If no treatment column specified and none found, don't add buffer values
      treatment_cols <- character(0)
    }
  }

  # Set buffer values for all treatment columns
  for (col_name in treatment_cols) {
    if (col_name %in% names(buffers)) {
      buffers[[col_name]] <- factor("buffer")
    }
  }

  if (blocks) {
    blocks_df <- stats::aggregate(
      cbind(row, col) ~ block,
      data = design,
      FUN = max
    )
    blocks_df$row[blocks_df$row == max(blocks_df$row)] <- max(blocks_df$row) + 1
    blocks_df$col[blocks_df$col == max(blocks_df$col)] <- max(blocks_df$col) + 1
    for (i in max(as.numeric(blocks_df$block)):1) {
      buffers[
        buffers$row <= blocks_df$row[i] & buffers$col <= blocks_df$col[i],
        "block"
      ] <- blocks_df$block[i]
    }
  }

  design <- rbind(design, buffers)

  # Carried as an attribute rather than a list return so the existing callers
  # keep receiving a plain data frame; `add_buffers()` strips it.
  attr(design, "buffer_transform") <- tf
  return(design)
}

#' Compose two buffer coordinate displacements
#'
#' Each [add_buffers()] call displaces the coordinates it is given, so stacking
#' calls composes the displacements: applying `new` on top of `old` gives
#' `scale = new$scale * old$scale` and `shift = new$scale * old$shift +
#' new$shift`. Composing (rather than overwriting) is what lets
#' `.drop_buffer_rows()` recover the original coordinates from a design that has
#' been buffered more than once, e.g. `add_buffers(add_buffers(d, "row"), "col")`.
#'
#' @param old,new Transform records, each a list of `row` and `col` named
#'   `c(scale, shift)` vectors. `old` may be `NULL`, for a first call.
#'
#' @returns A transform record of the same shape.
#' @keywords internal
.compose_buffer_transform <- function(old, new) {
  if (is.null(old)) {
    return(new)
  }
  compose1 <- function(o, n) {
    return(c(
      scale = unname(n[["scale"]] * o[["scale"]]),
      shift = unname(n[["scale"]] * o[["shift"]] + n[["shift"]])
    ))
  }
  return(list(
    row = compose1(old$row, new$row),
    col = compose1(old$col, new$col)
  ))
}

#' Add buffers to an existing design
#'
#' @param design_obj A design object (with class "design") from the design() function
#' @param type The type of buffer to add
#' @returns The modified design object with buffers added
#' @export
add_buffers <- function(design_obj, type) {
  stopifnot(inherits(design_obj, "design"))

  # Determine if design has blocks
  has_blocks <- any(grepl("block", tolower(names(design_obj$design_df))))

  # Extract treatment column names from the design object
  # For hierarchical designs, treatments is a named list
  # For simple designs, treatments is a character vector (but we need the column name, not values)
  treatment_cols <- NULL

  if (!is.null(design_obj$treatments)) {
    if (is.list(design_obj$treatments)) {
      # Hierarchical design: extract all treatment column names from the list names
      # These correspond to the column names in the design dataframe
      # We need to get the actual column names from the swap parameter used
      # The treatments list stores treatment values, but we need column names
      # Look for columns ending in "treatment" or matching common patterns
      all_cols <- names(design_obj$design_df)
      treatment_cols <- all_cols[grepl(
        "treatment",
        all_cols,
        ignore.case = TRUE
      )]
    } else {
      # Simple design: look for a "treatment" column
      if ("treatment" %in% names(design_obj$design_df)) {
        treatment_cols <- "treatment"
      }
    }
  }

  # Create buffers and update the design dataframe
  buffered <- create_buffers(
    design_obj$design_df,
    type,
    blocks = has_blocks,
    treatment_cols = treatment_cols
  )

  # Buffers are a field-layout convenience: they must not change any statistical
  # property of the design. Making room for them does displace the real plots'
  # row/col coordinates, so record the displacement (composed with any earlier
  # one) and let `.drop_buffer_rows()` undo it before anything is computed.
  # Recording it here, rather than compensating in each metric, keeps the
  # displacement a presentation detail that never reaches the statistics.
  tf <- attr(buffered, "buffer_transform")
  attr(buffered, "buffer_transform") <- NULL
  design_obj$design_df <- buffered
  design_obj$metadata$buffer <- list(
    types = c(design_obj$metadata$buffer$types, type),
    transform = .compose_buffer_transform(
      design_obj$metadata$buffer$transform,
      tf
    )
  )

  return(design_obj)
}
