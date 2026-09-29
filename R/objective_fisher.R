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
#' @param L_matrix Optional precomputed projection matrix. This is an advanced
#'   alternative to `Sigma`; the two cannot be supplied together.
#' @param Sigma Optional covariance matrix whose rows and columns follow the row
#'   order of `layout_df`. The projection is computed on the first objective
#'   call and reused during optimisation. If both `Sigma` and `L_matrix` are
#'   `NULL`, identity covariance is assumed.
#' @param contrast_matrix Numeric contrast matrix with one column per treatment
#'   level. Column names, when supplied, must match the treatment levels. Only
#'   these contrasts are required to be estimable and contribute to the
#'   optimality criterion. The default targets the complete treatment-contrast
#'   space.
#' @param block_column Column name of the design's block factor. Used when
#'   \code{L_matrix} isn't supplied.
#' @param nuisance_formula Optional one-sided formula describing nuisance
#'   effects. Used when \code{L_matrix} is not supplied. If omitted,
#'   \code{block_column} is treated as a factor.
#' @param tolerance Relative numerical tolerance for rank and estimability
#'   checks.
#' @param current_score_obj The current objective result supplied by [speed()]
#'   after the initial call. Used to reuse invariant matrix calculations.
#' @param swapped_items The treatments swapped by [speed()]. Changed plot rows
#'   are identified from the cached and proposed allocations so repeated
#'   treatments and multi-swaps are handled exactly.
#' @param ... Extra parameters passed from \code{speed}.
#'
#' @details
#' This function computes the treatment information matrix:
#' \deqn{I = X_1^\intercal L X_1}{I = X₁ᵀ L X₁}
#' Where \eqn{X_1}{X₁} is the treatment design matrix, and \eqn{L} is the
#' projection that removes nuisance fixed effects from the GLS's inverse
#' covariance:
#' \deqn{L = \Sigma^{-1} - \Sigma^{-1} X_2 \left( X_2^\intercal \Sigma^{-1} X_2
#' \right)^{-1} X_2^\intercal \Sigma^{-1}}{L = Σ⁻¹ - Σ⁻¹ X₂
#' (X₂ᵀ Σ⁻¹ X₂)⁻¹ X₂ᵀ Σ⁻¹}
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
#' result_spatial <- speed(
#'   df,
#'   swap = "treatment",
#'   swap_within = "block",
#'   obj_function = objective_function_info,
#'   Sigma = Sigma,
#'   criterion = "A",
#'   optimise_params = optim_params(random_initialisation = TRUE),
#'   seed = 42,
#'   quiet = TRUE
#' )
#'
#' @return A named list with \code{score}, \code{info_matrix},
#'   \code{eigenvalues}, contrast diagnostics, and \code{criterion}.
#'
#' @export
objective_function_info <- function(
  layout_df,
  swap,
  spatial_cols,
  criterion = c("A", "D"),
  L_matrix = NULL,
  Sigma = NULL,
  contrast_matrix = NULL,
  block_column = "block",
  nuisance_formula = NULL,
  tolerance = 1e-9,
  current_score_obj = NULL,
  swapped_items = NULL,
  ...
) {
  criterion <- match.arg(criterion)

  fisher_cache <- NULL
  if (!is.null(swapped_items)) {
    fisher_cache <- attr(current_score_obj, "fisher_cache", exact = TRUE)
  }
  ci <- .compute_info(
    layout_df,
    swap,
    Sigma,
    L_matrix,
    block_column,
    nuisance_formula,
    tolerance,
    fisher_cache
  )
  diagnostics <- .information_criteria(
    ci$info, ci$trt_levels, contrast_matrix, tolerance
  )
  score <- diagnostics[[paste0(criterion, "_value")]]
  if (!is.finite(score)) score <- 1e10

  result <- list(
    score = score,
    info_matrix = ci$info,
    eigenvalues = diagnostics$eigenvalues,
    contrast_matrix = diagnostics$contrast_matrix,
    contrast_covariance = diagnostics$contrast_covariance,
    estimable = diagnostics$estimable,
    rank = diagnostics$rank,
    criterion = criterion,
    A_value = diagnostics$A_value,
    D_value = diagnostics$D_value
  )
  attr(result, "fisher_cache") <- ci$cache
  return(result)
}

#' Build the treatment indicator matrix X1
#' @noRd
.build_treatment_matrix <- function(treatments, trt_levels, n, v) {
  X1 <- matrix(0, n, v)
  trt_idx <- match(treatments, trt_levels)
  if (anyNA(trt_idx)) stop("Treatment allocations cannot contain missing values.", call. = FALSE)
  X1[cbind(seq_len(n), trt_idx)] <- 1
  return(X1)
}

#' Build an orthonormal factorial contrast matrix
#'
#' Constructs a contrast basis for selected factorial effects while treatments
#' remain encoded as atomic combinations during optimisation. The returned rows
#' are orthonormal, so A-optimality is invariant to the particular contrast
#' coding used by [stats::model.matrix()].
#'
#' @param treatment_df One row per treatment combination, containing the
#'   component factor columns.
#' @param formula One-sided formula selecting factorial effects, for example
#'   \code{~ (stage + cultivar + inoculum)^2}.
#' @param treatment_column Column containing the atomic treatment labels.
#' @param tolerance Numerical rank tolerance.
#'
#' @return A numeric contrast matrix whose columns are named by treatment level.
#'
#' @export
factorial_contrast_matrix <- function(
  treatment_df,
  formula,
  treatment_column = "treatment",
  tolerance = 1e-10
) {
  if (!is.data.frame(treatment_df)) {
    stop("`treatment_df` must be a data frame.", call. = FALSE)
  }
  if (!inherits(formula, "formula") || length(formula) != 2L) {
    stop("`formula` must be a one-sided formula.", call. = FALSE)
  }
  if (!treatment_column %in% names(treatment_df)) {
    stop("Treatment column `", treatment_column, "` not found.", call. = FALSE)
  }

  treatment_levels <- as.character(treatment_df[[treatment_column]])
  if (anyNA(treatment_levels) || anyDuplicated(treatment_levels)) {
    stop("`treatment_df` must contain one non-missing row per treatment.", call. = FALSE)
  }

  formula_vars <- all.vars(formula)
  missing_vars <- setdiff(formula_vars, names(treatment_df))
  if (length(missing_vars) > 0L) {
    stop(
      "Formula variable(s) not found: ", paste(missing_vars, collapse = ", "),
      call. = FALSE
    )
  }

  model_data <- treatment_df
  for (column in formula_vars) {
    if (is.character(model_data[[column]])) {
      model_data[[column]] <- factor(model_data[[column]])
    }
  }

  effect_matrix <- stats::model.matrix(formula, model_data)
  effect_matrix <- sweep(effect_matrix, 2L, colMeans(effect_matrix), "-")
  decomposition <- qr(effect_matrix, tol = tolerance)
  effect_rank <- decomposition$rank
  if (effect_rank == 0L) {
    stop("`formula` does not define any treatment contrasts.", call. = FALSE)
  }

  basis <- qr.Q(decomposition, complete = FALSE)[, seq_len(effect_rank), drop = FALSE]
  contrasts <- t(basis)
  rownames(contrasts) <- paste0("contrast_", seq_len(nrow(contrasts)))
  colnames(contrasts) <- treatment_levels
  return(contrasts)
}

#' Prepare a requested contrast space
#' @noRd
.prepare_contrast_matrix <- function(contrast_matrix, trt_levels, tolerance) {
  v <- length(trt_levels)
  if (is.null(contrast_matrix)) {
    if (v < 2L) {
      return(matrix(numeric(0), nrow = 0L, ncol = v))
    }
    basis <- qr.Q(qr(stats::contr.helmert(v)), complete = FALSE)
    contrast_matrix <- t(basis)
    colnames(contrast_matrix) <- trt_levels
  } else {
    if (!is.matrix(contrast_matrix) || !is.numeric(contrast_matrix)) {
      stop("`contrast_matrix` must be a numeric matrix.", call. = FALSE)
    }
    if (any(!is.finite(contrast_matrix))) {
      stop("`contrast_matrix` must contain only finite values.", call. = FALSE)
    }
    if (is.null(colnames(contrast_matrix))) {
      if (ncol(contrast_matrix) != v) {
        stop("`contrast_matrix` must have one column per treatment.", call. = FALSE)
      }
      colnames(contrast_matrix) <- trt_levels
    } else {
      missing <- setdiff(trt_levels, colnames(contrast_matrix))
      extra <- setdiff(colnames(contrast_matrix), trt_levels)
      if (length(missing) > 0L || length(extra) > 0L) {
        stop("Contrast columns must match the treatment levels.", call. = FALSE)
      }
      contrast_matrix <- contrast_matrix[, trt_levels, drop = FALSE]
    }
  }

  if (nrow(contrast_matrix) > 0L && qr(t(contrast_matrix), tol = tolerance)$rank < nrow(contrast_matrix)) {
    stop("Rows of `contrast_matrix` must be linearly independent.", call. = FALSE)
  }
  return(contrast_matrix)
}

#' Calculate information criteria for a requested contrast space
#' @noRd
.information_criteria <- function(info, trt_levels, contrast_matrix, tolerance) {
  info <- (info + t(info)) / 2
  eig <- eigen(info, symmetric = TRUE)
  scale <- max(abs(eig$values))
  keep <- if (scale == 0) rep(FALSE, length(eig$values)) else eig$values > scale * tolerance
  rank <- sum(keep)
  pos_eig <- sort(eig$values[keep], decreasing = TRUE)

  info_inverse <- matrix(0, nrow(info), ncol(info))
  if (rank > 0L) {
    vectors <- eig$vectors[, keep, drop = FALSE]
    info_inverse <- vectors %*% diag(1 / eig$values[keep], nrow = rank) %*% t(vectors)
  }

  contrasts <- .prepare_contrast_matrix(contrast_matrix, trt_levels, tolerance)
  projection <- info_inverse %*% info
  residual <- contrasts - contrasts %*% projection
  estimable <- nrow(contrasts) == 0L ||
    max(abs(residual)) <= tolerance * (1 + max(abs(contrasts)))

  contrast_covariance <- matrix(numeric(0), nrow = 0L, ncol = 0L)
  A_value <- Inf
  D_value <- Inf
  if (estimable) {
    contrast_covariance <- contrasts %*% info_inverse %*% t(contrasts)
    contrast_covariance <- (contrast_covariance + t(contrast_covariance)) / 2
    if (nrow(contrasts) == 0L) {
      A_value <- 0
      D_value <- 0
    } else {
      contrast_eig <- eigen(contrast_covariance, symmetric = TRUE, only.values = TRUE)$values
      contrast_scale <- max(abs(contrast_eig))
      if (all(contrast_eig > contrast_scale * tolerance)) {
        A_value <- sum(diag(contrast_covariance))
        D_value <- sum(log(contrast_eig))
      }
    }
  }

  return(list(
    eigenvalues = pos_eig,
    rank = rank,
    contrast_matrix = contrasts,
    contrast_covariance = contrast_covariance,
    estimable = estimable,
    A_value = A_value,
    D_value = D_value
  ))
}

#' Function to precompute \eqn{L}
#'
#' \eqn{L} is the projection matrix that removes nuisance effects under a
#' spatial correlation structure, given an \eqn{n \times n}{n * n} covariance
#' \eqn{\Sigma}{Sigma} and nuisance effects design \eqn{X_2}.
#'
#' This only depends on lambda and the block structure, so precompute this.
#' @noRd
.build_nuisance_matrix <- function(layout_df, block_column, nuisance_formula) {
  if (is.null(nuisance_formula)) {
    if (!block_column %in% names(layout_df)) {
      return(matrix(1, nrow(layout_df), 1L))
    }
    return(stats::model.matrix(~ 0 + block, data.frame(block = factor(layout_df[[block_column]]))))
  }
  if (!inherits(nuisance_formula, "formula") || length(nuisance_formula) != 2L) {
    stop("`nuisance_formula` must be a one-sided formula.", call. = FALSE)
  }
  return(stats::model.matrix(nuisance_formula, layout_df))
}

#' Build a projection under identity covariance
#' @noRd
.build_L_from_df <- function(layout_df, block_column, n, nuisance_formula = NULL, tolerance = 1e-10) {
  X2 <- .build_nuisance_matrix(layout_df, block_column, nuisance_formula)
  crossprod_inverse <- pseudo_inverse(crossprod(X2), tolerance)
  return(diag(n) - X2 %*% crossprod_inverse %*% t(X2))
}

#' Check whether a cached projection matches the requested projection
#' @noRd
.fisher_projection_matches <- function(
  cache,
  source,
  input,
  block_column,
  nuisance_formula,
  tolerance,
  n
) {
  matches <- is.list(cache) &&
    identical(cache$projection_source, source) &&
    identical(cache$projection_input, input) &&
    identical(cache$block_column, block_column) &&
    identical(cache$nuisance_formula, nuisance_formula) &&
    identical(cache$tolerance, tolerance) &&
    is.matrix(cache$L_matrix) &&
    identical(dim(cache$L_matrix), c(n, n))
  return(isTRUE(matches))
}

#' Check whether an information-matrix cache can be updated
#' @noRd
.fisher_cache_is_valid <- function(cache, swap, trt_levels, L_matrix, n, v) {
  valid <- is.list(cache) &&
    identical(cache$swap, swap) &&
    identical(cache$trt_levels, trt_levels) &&
    length(cache$treatments) == n &&
    all(cache$treatments %in% trt_levels) &&
    is.matrix(cache$LX_matrix) &&
    identical(dim(cache$LX_matrix), c(n, v)) &&
    identical(cache$L_matrix, L_matrix)
  return(isTRUE(valid))
}

#' Function for computing and caching the information matrix
#' @noRd
.compute_info <- function(
  layout_df,
  swap,
  Sigma,
  L_matrix,
  block_column,
  nuisance_formula = NULL,
  tolerance = 1e-10,
  cache = NULL
) {
  treatments <- layout_df[[swap]]
  trt_levels <- levels(factor(treatments))
  v <- length(trt_levels)
  n <- nrow(layout_df)
  treatment_labels <- as.character(treatments)

  if (!is.null(Sigma) && !is.null(L_matrix)) {
    stop("Only one of `Sigma` and `L_matrix` can be supplied.", call. = FALSE)
  }

  if (!is.null(L_matrix)) {
    projection_source <- "L_matrix"
    projection_input <- L_matrix
  } else if (!is.null(Sigma)) {
    projection_source <- "Sigma"
    projection_input <- Sigma
    if (.fisher_projection_matches(
      cache,
      projection_source,
      projection_input,
      block_column,
      nuisance_formula,
      tolerance,
      n
    )) {
      L_matrix <- cache$L_matrix
    } else {
      L_matrix <- compute_L_projection(
        layout_df,
        Sigma,
        block_column,
        nuisance_formula,
        tolerance
      )
    }
  } else {
    projection_source <- "identity"
    projection_input <- NULL
    if (.fisher_projection_matches(
      cache,
      projection_source,
      projection_input,
      block_column,
      nuisance_formula,
      tolerance,
      n
    )) {
      L_matrix <- cache$L_matrix
    } else {
      L_matrix <- .build_L_from_df(
        layout_df,
        block_column,
        n,
        nuisance_formula,
        tolerance
      )
    }
  }

  if (!is.matrix(L_matrix) || !identical(dim(L_matrix), c(n, n))) {
    stop("`L_matrix` must be an n by n matrix for `layout_df`.", call. = FALSE)
  }

  X1 <- .build_treatment_matrix(treatment_labels, trt_levels, n, v)
  if (.fisher_cache_is_valid(cache, swap, trt_levels, L_matrix, n, v)) {
    changed <- which(cache$treatments != treatment_labels)
    LX <- cache$LX_matrix
    if (length(changed) > 0L) {
      old_index <- match(cache$treatments[changed], trt_levels)
      new_index <- match(treatment_labels[changed], trt_levels)
      delta <- matrix(0, nrow = length(changed), ncol = v)
      delta[cbind(seq_along(changed), old_index)] <- -1
      delta[cbind(seq_along(changed), new_index)] <- 1
      LX <- LX + L_matrix[, changed, drop = FALSE] %*% delta
    }
  } else {
    LX <- L_matrix %*% X1
  }

  info <- crossprod(X1, LX)
  info <- (info + t(info)) / 2
  dimnames(info) <- list(trt_levels, trt_levels)

  cache <- list(
    swap = swap,
    treatments = treatment_labels,
    trt_levels = trt_levels,
    L_matrix = L_matrix,
    LX_matrix = LX,
    projection_source = projection_source,
    projection_input = projection_input,
    block_column = block_column,
    nuisance_formula = nuisance_formula,
    tolerance = tolerance
  )

  return(list(
    info = info,
    v = v,
    trt_levels = trt_levels,
    cache = cache
  ))
}

# Covariance utilities

#' Compute L projection
#'
#' This is the projection matrix that removes nuisance effects from the GLS's
#' covariance
#'
#' @param layout_df Data frame representing the spatial layout of the
#'   experiment.
#' @param Sigma Covariance structure to use. Its rows and columns must follow
#'   the row order of `layout_df`.
#' @param block_column Column name of the design's block factor in
#'   \code{layout_df}.
#' @param nuisance_formula Optional one-sided formula describing nuisance
#'   effects. If omitted, \code{block_column} is treated as a factor.
#' @param tolerance Numerical tolerance used for generalized inverses.
#'
#' @return \eqn{(n \times n)} numeric matrix.
#'
#' @export
compute_L_projection <- function(
  layout_df,
  Sigma,
  block_column = "block",
  nuisance_formula = NULL,
  tolerance = 1e-10
) {
  n <- nrow(layout_df)

  if (!is.matrix(Sigma)) {
    stop("Sigma must be a matrix")
  }
  if (!identical(dim(Sigma), c(n, n))) {
    stop(
      "Sigma matrix must have dimension equal ",
      "to dimension of layout_df dataframe"
    )
  }
  if (!isSymmetric(Sigma, tol = 1e-8)) stop("Sigma must be symmetric")
  X2 <- .build_nuisance_matrix(layout_df, block_column, nuisance_formula)

  Sigma_inv <- solve(Sigma)
  SiX2 <- Sigma_inv %*% X2
  nuisance_inverse <- pseudo_inverse(crossprod(X2, SiX2), tolerance)
  L <- Sigma_inv - SiX2 %*% nuisance_inverse %*% t(SiX2)
  return((L + t(L)) / 2)
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
  return(rho^abs(outer(seq_len(n), seq_len(n), "-")))
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
  return(kronecker(
    cor_ar1(n_rows, rho_row),
    cor_ar1(n_cols, rho_col)
  ))
}
