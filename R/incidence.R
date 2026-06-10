#' Calculate Treatment Pair Incidence
#'
#' @description
#' Returns a symmetric matrix counting how many times each pair of treatments
#' appears as neighbours in the design. Two plots are neighbours if they share
#' an edge on the `row × col` grid (orthogonal / rook adjacency; diagonals
#' excluded). Horizontal and vertical adjacencies are pooled.
#'
#' The diagonal holds self-adjacency counts — the quantity that
#' [calculate_adjacency_score()] penalises. A well-optimised design will have
#' zeros (or near-zeros) on the diagonal. Off-diagonal entry \eqn{(i, j)}
#' is the total number of edges where treatment \eqn{i} and treatment \eqn{j}
#' are immediate neighbours.
#'
#' @param design A `design` object returned by [speed()], or a plain data
#'   frame containing at least the columns named by `swap`, `row_col`, and
#'   `col_col`.
#' @param swap Column name of the treatment variable (default `"treatment"`).
#' @param row_col Column name of the row position variable (default `"row"`).
#' @param col_col Column name of the column position variable (default `"col"`).
#' @param as_list If `TRUE`, returns a named list instead of a matrix: one
#'   entry per treatment, each a named integer vector of neighbour counts with
#'   every other treatment (including itself on the diagonal). This is the
#'   matrix split by row — identical data, different container.
#'
#' @return When `as_list = FALSE` (default): a symmetric integer matrix of
#'   dimension \eqn{t \times t} where \eqn{t} is the number of unique
#'   treatments, with treatment names as row and column names.
#'
#'   When `as_list = TRUE`: a named list of length \eqn{t}, where each element
#'   is a named integer vector of neighbour counts for that treatment.
#'
#' @examples
#' # 3x3 Latin square — all pairs equally adjacent, zero self-adjacency
#' df <- initialise_design_df(
#'   items = c("A", "B", "C", "B", "C", "A", "C", "A", "B"),
#'   nrows = 3,
#'   ncols = 3
#' )
#' calculate_pair_incidence(df)
#'
#' # as named list
#' calculate_pair_incidence(df, as_list = TRUE)
#'
#' @seealso [calculate_adjacency_score()], [calculate_position_incidence()]
#'
#' @export
calculate_pair_incidence <- function(design,
                                     swap = "treatment",
                                     row_col = "row",
                                     col_col = "col",
                                     as_list = FALSE) {
  df <- if (inherits(design, "design")) design$design_df else design
  missing_cols <- setdiff(c(swap, row_col, col_col), names(df))
  if (length(missing_cols)) {
    stop(
      "Column(s) not found in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  m <- build_design_matrix(df, swap, row_col, col_col)
  nr <- nrow(m)
  nc <- ncol(m)

  lvls <- stringi::stri_sort(
    unique(as.character(df[[swap]][!is.na(df[[swap]])])),
    numeric = TRUE
  )

  horiz <- if (nc >= 2) {
    cbind(
      as.vector(m[, seq_len(nc - 1L)]),
      as.vector(m[, seq.int(2L, nc)])
    )
  } else {
    matrix(character(0L), nrow = 0L, ncol = 2L)
  }
  vert <- if (nr >= 2) {
    cbind(
      as.vector(m[seq_len(nr - 1L), ]),
      as.vector(m[seq.int(2L, nr), ])
    )
  } else {
    matrix(character(0L), nrow = 0L, ncol = 2L)
  }

  edges <- rbind(horiz, vert)
  keep <- !is.na(edges[, 1L]) & !is.na(edges[, 2L])
  edges <- edges[keep, , drop = FALSE]

  a_fac <- factor(edges[, 1L], levels = lvls)
  b_fac <- factor(edges[, 2L], levels = lvls)
  raw <- matrix(
    as.integer(table(a_fac, b_fac)),
    nrow = length(lvls),
    dimnames = list(lvls, lvls)
  )

  M <- raw + t(raw)
  diag(M) <- diag(raw)

  if (as_list) {
    return(lapply(setNames(lvls, lvls), function(trt) M[trt, ]))
  }
  M
}


#' Calculate Positional Incidence of Treatments
#'
#' @description
#' Returns how many times each treatment appears in each row position and each
#' column position of the design. This is the human-readable decomposition of
#' what [calculate_balance_score()] collapses to a scalar: uneven row or column
#' counts reveal where spatial imbalance lies.
#'
#' @inheritParams calculate_pair_incidence
#'
#' @return A named list with two integer matrices, both with treatments as rows
#'   and positions as columns:
#'   \describe{
#'     \item{`row`}{Treatment × row-position count matrix.}
#'     \item{`col`}{Treatment × column-position count matrix.}
#'   }
#'   Indexing by treatment name is natural: `result$row["A", ]` gives the row
#'   distribution of treatment A.
#'
#' @examples
#' df <- initialise_design_df(
#'   items = c("A", "B", "C", "B", "C", "A", "C", "A", "B"),
#'   nrows = 3,
#'   ncols = 3
#' )
#' res <- calculate_position_incidence(df)
#' res$row  # treatment × row counts
#' res$col  # treatment × column counts
#'
#' @seealso [calculate_balance_score()], [calculate_pair_incidence()]
#'
#' @export
calculate_position_incidence <- function(design,
                                         swap = "treatment",
                                         row_col = "row",
                                         col_col = "col") {
  df <- if (inherits(design, "design")) design$design_df else design
  missing_cols <- setdiff(c(swap, row_col, col_col), names(df))
  if (length(missing_cols)) {
    stop(
      "Column(s) not found in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  lvls <- stringi::stri_sort(
    unique(as.character(df[[swap]][!is.na(df[[swap]])])),
    numeric = TRUE
  )
  trt_fac <- factor(df[[swap]], levels = lvls)

  row_tbl <- table(trt_fac, df[[row_col]])
  col_tbl <- table(trt_fac, df[[col_col]])

  row_mat <- matrix(
    as.integer(row_tbl),
    nrow = length(lvls),
    dimnames = list(lvls, colnames(row_tbl))
  )
  col_mat <- matrix(
    as.integer(col_tbl),
    nrow = length(lvls),
    dimnames = list(lvls, colnames(col_tbl))
  )

  list(row = row_mat, col = col_mat)
}
