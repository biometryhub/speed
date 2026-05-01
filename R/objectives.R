#' Objective function using the treatment information matrix
#'
#' Creates an objective function that optimises experimental designs using the
#' Fisher information for treatment contrasts after adjusting for nuisance
#' effects, and takes an optional spatial covariance structure for spatial
#' optimisation.
#'
#' @param layout_df Data frame representing the spatial layout of the
#'   experiment.
#' @param swap Column name to swap, usually the treatment.
#' @param spatial_cols Column name of the spatial factors.
#' @param criterion Either \code{"A"} or \code{"D"}, representing A or D
#'   optimality.
#'     - A-optimality: Minimises \eqn{\mathrm{tr} \left( \mathcal{I}^-
#'       \right)}{tr(I⁻)}.
#'     - D-optimality: Minimises \eqn{-\log \left| \mathcal{I} \right|}{-
#'       log(|I|)}
#' @param L_matrix Precomputed projection matrix. Use
#'   \code{precompute_projection} to generate it. If \code{NULL}, then the
#'   identity covariance structure will be assumed, and the projection will be
#'   computed using the structure of the design's blocks.
#' @param block_column Column name of the design's block factor. Used when
#'   \code{L_matrix} isn't supplied.
#' @param ... Extra parameters passed from \code{speed}.
#'
#' @details
#' This function computes the treatment information matrix:
#' \deqn{I = X_1^\intercal L X_1}{I = X₁ᵀ L X₁}
#' Where \eqn{X_1}{X₁} is the treatment design matrix, and \eqn{L} is the
#' projection that removes nuisance fixed effects from the GLS's inverse
#' covariance:
#' \deqn{L = \Sigma^{-1} - \Sigma^{-1} X_2 \left( X_2^\intercal \Sigma^{-1} X_2
#' \right)^{-1}}{L = Σ⁻¹ - Σ⁻¹ X₂ (X₂ᵀ Σ⁻¹ X₂)⁻¹}
#' The user specifies the spatial covariance \eqn{\Sigma}{Σ} they intend to use
#' at the time of analysis, e.g. lag-1 autoregressive \eqn{\text{AR}_1 \otimes
#' \text{AR}_1}{AR₁ ⊗ AR₁}, and this objective will optimise under that
#' structure.
#'
#' @examples
#' # Small RCBD layout: 6 treatments in 4 blocks of 6 plots
#' df <- initialise_design_df(
#'   items = 6, nrows = 4, ncols = 6,
#'   block_nrows = 1, block_ncols = 6
#' )
#'
#' # Non-spatial: identity covariance
#' result <- speed(
#'   df,
#'   swap = "treatment",
#'   swap_within = "block",
#'   obj_function = objective_function_info,
#'   criterion = "A",
#'   seed = 42,
#'   quiet = TRUE
#' )
#'
#' # Spatial: AR(1) x AR(1)
#' Sigma <- cor_ar1_ar1(
#'   n_rows = 4, n_cols = 6,
#'   rho_row = 0.6, rho_col = 0.3
#' )
#' L <- compute_L_projection(df, Sigma, block_column = "block")
#' result_spatial <- speed(
#'   df,
#'   swap = "treatment",
#'   swap_within = "block",
#'   obj_function = objective_function_info,
#'   L_matrix = L,
#'   criterion = "A",
#'   optimise_params = optim_params(random_initialisation = TRUE),
#'   seed = 42,
#'   quiet = TRUE
#' )
#'
#' # Inspect the optimised design
#' calc_info_matrix(result_spatial$design_df, L_matrix = L)
#'
#' @return A named list with \code{score}, \code{info_matrix},
#'   \code{eigenvalues}, \code{criterion}.
#'
#' @export
objective_function_info <- function(
  layout_df,
  swap,
  spatial_cols,
  criterion = c("A", "D"),
  L_matrix = NULL,
  block_column = "block",
  ...
) {
  criterion <- match.arg(criterion)

  ci <- .compute_info(layout_df, swap, L_matrix, block_column)
  info <- ci$info
  v <- ci$v

  ## get positive eigenvals from info matrix
  eig <- eigen(info, symmetric = TRUE, only.values = TRUE)$values
  max_eig <- max(eig)
  if (max_eig <= 0) {
    score <- 1e10
    pos_eig <- numeric(0)
  } else {
    pos_eig <- eig[eig > max_eig * 1e-10]
    ## if design is disconnected, penalise greatly
    ## otherwise A or D
    if (length(pos_eig) < v - 1) {
      score <- 1e10
    } else {
      score <- switch(criterion,
        A = sum(1 / pos_eig),
        D = -sum(log(pos_eig))
      )
      if (!is.finite(score)) score <- 1e10
    }
  }

  return(list(
    score = score,
    info_matrix = info,
    eigenvalues = sort(pos_eig, decreasing = TRUE),
    criterion = criterion
  ))
}

#' Build the treatment indicator matrix X1
#' @noRd
.build_treatment_matrix <- function(treatments, trt_levels, n, v) {
  X1 <- matrix(0, n, v)
  trt_idx <- match(treatments, trt_levels)
  for (i in seq_len(n)) {
    X1[i, trt_idx[i]] <- 1
  }
  return(X1)
}

#' Function to precompute \eqn{L}
#'
#' \eqn{L} is the projection matrix that removes nuisance effects under a
#' spatial correlation structure, given an \eqn{n \times n}{n * n} covariance
#' \eqn{\Sigma}{Sigma} and nuisance effects design \eqn{X_2}.
#'
#' This only depends on lambda and the block structure, so precompute this.
#' @noRd
.build_L_from_df <- function(layout_df, block_column, n) {
  if (!block_column %in% names(layout_df)) {
    ## L = I - (1/n) ones_matrix
    return(diag(n) - matrix(1 / n, n, n))
  }

  blocks <- factor(layout_df[[block_column]])
  b <- nlevels(blocks)

  X2 <- matrix(0, n, b)
  for (j in seq_len(b)) {
    X2[blocks == levels(blocks)[j], j] <- 1
  }

  ## L = I - X2 (X2t X2)^{-1} X2t
  X2tX2_inv <- diag(1 / colSums(X2), nrow = b)
  diag(n) - X2 %*% X2tX2_inv %*% t(X2)
}

#' Function for computing the information matrix
#' @noRd
.compute_info <- function(
  layout_df,
  swap,
  L_matrix,
  block_column
) {
  ## a lot of the args come from the design now
  treatments <- layout_df[[swap]]
  trt_levels <- levels(factor(treatments))
  v <- length(trt_levels)
  n <- nrow(layout_df)

  ## L
  ### projection matrix
  if (is.null(L_matrix)) {
    L_matrix <- .build_L_from_df(layout_df, block_column, n)
  }

  ## X1
  ### treatment design matrix.
  X1 <- .build_treatment_matrix(treatments, trt_levels, n, v)
  ## information matrix I = X1t L X1
  info <- t(X1) %*% L_matrix %*% X1

  list(info = info, v = v, trt_levels = trt_levels)
}

# Helper functions

#' Compute L projection
#'
#' This is the projection matrix that removes nuisance effects from the GLS's
#' covariance
#'
#' @param layout_df Data frame representing the spatial layout of the
#'   experiment.
#' @param Sigma Covariance structure to use.
#' @param block_column Column name of the design's block factor in
#'   \code{layout_df}.
#'
#' @return \eqn{(n \times n)} numeric matrix.
#'
#' @export
compute_L_projection <- function(
  layout_df,
  Sigma,
  block_column = "block"
) {
  n <- nrow(layout_df)

  ## arg checks
  if (!is.matrix(Sigma)) stop("Sigma must be a matrix")
  if (nrow(Sigma) != n) {
    stop(
      "Sigma matrix must have dimension equal ",
      "to dimension of layout_df dataframe")
  }
  if (ncol(Sigma) != n) {
    stop(
      "Sigma matrix must have dimension equal ",
      "to dimension of layout_df dataframe"
    )
  }
  if (!isSymmetric(Sigma, tol = 1e-8)) stop("Sigma must be symmetric")
  if (!(block_column %in% names(layout_df))) {
    stop("Block column name not in layout_df dataframe")
  }

  blocks <- factor(layout_df[[block_column]])
  b <- nlevels(blocks)

  ## X2
  ### block indicator matrix with the intercept removed
  X2 <- matrix(0, n, b)
  for (j in seq_len(b)) {
    X2[blocks == levels(blocks)[j], j] <- 1
  }

  Sigma_inv <- solve(Sigma)
  SiX2 <- Sigma_inv %*% X2
  return(Sigma_inv - SiX2 %*% solve(t(X2) %*% SiX2) %*% t(SiX2))
}

#' Calculate info matrix
#'
#' Calculate the Fisher information in the experimental design
#'
#' @param layout_df Data frame representing the spatial layout of the
#'   experiment.
#' @param treatment_column Column name containing the design's treatments in
#'   \code{layout_df}.
#' @param L_matrix Precomputed projection matrix from
#'   \code{compute_L_projection}.
#' @param block_column Column name containing the designs block in
#'   \code{layout_df}.
#'
#' @export
calc_info_matrix <- function(
  layout_df,
  treatment_column = "treatment",
  L_matrix = NULL,
  block_column = "block"
) {
  ci <- .compute_info(layout_df, treatment_column, L_matrix, block_column)
  info <- ci$info
  v <- ci$v
  rownames(info) <- ci$trt_levels
  colnames(info) <- ci$trt_levels

  ## get positive eigenvals from info matrix
  eig <- eigen(info, symmetric = TRUE, only.values = TRUE)$values
  max_eig <- max(eig)
  if (max_eig <= 0) {
    pos_eig <- numeric(0)
  } else {
    pos_eig <- eig[eig > max_eig * 1e-10]
  }

  if (length(pos_eig) < v - 1) {
    A_val <- Inf
    D_val <- Inf
  } else {
    A_val = sum(1 / pos_eig)
    D_val = -sum(log(pos_eig))
    if (!is.finite(A_val)) A_val <- Inf
    if (!is.finite(D_val)) D_val <- Inf
  }

  return(list(
    info_matrix = info,
    eigenvalues = sort(pos_eig, decreasing = TRUE),
    rank = length(pos_eig),
    A_value = A_val,
    D_value = D_val
  ))
}

#' Calculate incidence matrix
#'
#' @param layout_df Data frame representing the spatial layout of the
#'   experiment.
#' @param treatment_column Column name containing the design's treatments in
#'   \code{layout_df}.
#' @param block_column Column name containing the designs block in
#'   \code{layout_df}.
#'
#' @return \eqn{v \times b} matrix of integers, with treatment levels as row
#'   names, and block levels as column names.
#'
#' @export
calc_incidence_matrix <- function(
  layout_df,
  treatment_column = "treatment",
  block_column = "block"
) {
  treatments <- factor(layout_df[[treatment_column]])
  blocks <- factor(layout_df[[block_column]])

  ## make a matrix of integers
  N <- table(treatments, blocks)
  class(N) <- "matrix"
  storage.mode(N) <- "integer"

  return(N)
}

#' Calculate concurrence matrix
#'
#' @param layout_df Data frame representing the spatial layout of the
#'   experiment.
#' @param treatment_column Column name containing the design's treatments in
#'   \code{layout_df}.
#' @param block_column Column name containing the designs block in
#'   \code{layout_df}.
#'
#' @return Symmetric \eqn{N N^\intercal}{N Nᵀ} matrix.
#'
#' @export
calc_concurrence_matrix <- function(
  layout_df,
  treatment_column = "treatment",
  block_column = "block"
) {
  N <- calc_incidence_matrix(layout_df, treatment_column, block_column)
  C <- N %*% t(N)
  storage.mode(C) <- "integer"
  return(C)
}

#' Calculate canonical efficiency factors
#'
#' @param layout_df Data frame representing the spatial layout of the
#'   experiment.
#' @param treatment_column Column name containing the design's treatments in
#'   \code{layout_df}.
#' @param L_matrix Precomputed projection matrix from
#'   \code{compute_L_projection}.
#' @param block_column Column name containing the designs block in
#'   \code{layout_df}.
#'
#' @return A list with \code{efficiency_factors}, \code{E}, \code{replication}.
#'
#' @export
calculate_efficiency_factors <- function(
  layout_df,
  treatment_column = "treatment",
  L_matrix = NULL,
  block_column = "block"
) {
  info <- calc_info_matrix(
    layout_df,
    treatment_column,
    L_matrix,
    block_column
  )

  treatments <- factor(layout_df[[treatment_column]])
  reps <- table(treatments)
  r_bar <- mean(reps)

  eff_factors <- info$eigenvalues / r_bar

  v <- nlevels(treatments)
  E <- if (info$rank >= v - 1) {
    (v - 1) / sum(1 / eff_factors)
  } else {
    NA_real_
  }

  list(
    efficiency_factors = eff_factors,
    E = E,
    replication = setNames(as.integer(reps), levels(treatments))
  )
}


# Convenience correlation structure constructors

#' Construct a 1 dimensional lag-1 autoregressive covariance
#'
#' @param n Size of the covariance vector to generate
#' @param rho Correlation parameter
#'
#' @return A symmetric \eqn{(n \times n)} correlation matrix.
#'
#' @export
cor_ar1 <- function(n, rho) {
  rho^abs(outer(seq_len(n), seq_len(n), "-"))
}


#' Construct a 2 dimensional lag-1 autoregressive covariance
#'
#' @param n_rows Number of rows in the design.
#' @param n_cols Number of columns in the design.
#' @param rho_row Correlation parameter along the row direction.
#' @param rho_col Correlation parameter along the column direction.
#'
#' @return A symmetric \eqn{(\text{n_rows} \cdot \text{n_cols}) \times
#'   (\text{n_rows} \cdot \text{n_cols})} correlation matrix.
#'
#' @export
cor_ar1_ar1 <- function(n_rows, n_cols, rho_row, rho_col) {
  kronecker(
    cor_ar1(n_rows, rho_row),
    cor_ar1(n_cols, rho_col)
  )
}
