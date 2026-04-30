#' Objective function using the treatment information matrix
#'
#' Creates an objective function that optimises experimental designs using the
#' Fisher information for treatment contrasts after adjusting for nuisance
#' effects, and takes an optional spatial covariance structure for spatial
#' optimisation.
#'
#' @param design Data frame representing the spatial layout of the experiment.
#' @param swap Column name to swap, usually the treatment.
#' @param spatial_cols Column name of the spatial factors.
#' @param criterion Either \code{"A"} or \code{"D"}, representing A or D
#'   optimality.
#'     - A-optimality: Minimises \eqn{\mathrm{tr} \left( \mathcal{I}^- \right)}{tr(I⁻)}.
#'     - D-optimality: Minimises \eqn{-\log \left| \mathcal{I} \right|}{-log(|I|)}
#' @param L_matrix Precomputed projection matrix. Use
#'   \code{precompute_projection} to generate it. If \code{NULL}, then the
#'   identity covariance structure will be assumed, and the projection will be
#'   computed using the structure of the design's blocks.
#' @param block_col Column name of the design's block factor. Used when
#'   \code{L_matrix} isn't supplied.
#'
#' @details
#' This function computes the treatment information matrix:
#' \deqn{I = X_1^\intercal L X_1}{I = X₁ᵀ L X₁}
#' Where \eqn{X_1}{X₁} is the treatment design matrix, and \eqn{L} is the
#' projection that removes nuisance fixed effects from the GLS's inverse
#' covariance:
#' \deqn{L = \Sigma^{-1} - \Sigma^{-1} X_2 \left( X_2^\intercal \Sigma^{-1} X_2 \right)^{-1}}{L = Σ⁻¹ - Σ⁻¹ X₂ (X₂ᵀ Σ⁻¹ X₂)⁻¹}
#' The user specifies the spatial covariance \eqn{\Sigma}{Σ} they intend to use
#' at the time of analysis, e.g. autoregressive-lag-1 \eqn{\text{AR}_1 \otimes \text{AR}_1}{AR₁ ⊗ AR₁},
#' and this objective will optimise under that structure.
#'
#' @export
objective_function_info <- function(
    design,
    swap,
    spatial_cols,
    row_col = "row",
    range_col = "range",
    criterion = c("A", "D"),
    L_matrix = NULL,
    block_col = "block",
    ...
) {
    criterion <- match.arg(criterion)

    ## a lot of the args come from the design now
    treatments <- design[[swap]]
    trt_levels <- levels(factor(treatments))
    v <- length(trt_levels)
    n <- nrow(design)

    ## L
    ### projection matrix
    if (is.null(L_matrix)) {
        L_matrix <- .build_L_from_df(design, block_col, n)
    }

    ## X1
    ### treatment design matrix.
    X1 <- .build_treatment_matrix(treatments, trt_levels, n, v)

    ## information matrix I = X1t L X1
    info <- t(X1) %*% L_matrix %*% X1

    ## get positive eigenvals from info matrix
    eig <- eigen(info, symmetric = TRUE, only.values = TRUE)$values
    pos_eig <- eig[eig > 1e-10]

    ## if design is disconnected, penalise greatly
    ## otherwise A or D
    if (length(pos_eig) < v - 1) {
        score <- Inf
    } else {
        score <- switch(criterion,
            A = sum(1 / pos_eig),
            D = -sum(log(pos_eig))
        )
    }

    return(list(
        score = score,
        info_matrix = info,
        eigenvalues = sort(pos_eig, decreasing = TRUE),
        criterion = criterion
    ))
}

#' Build the treatment indicator matrix X1
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
.build_L_from_df <- function(design, block_col, n) {
    if (!block_col %in% names(design)) {
        ## L = I - (1/n) ones_matrix
        return(diag(n) - matrix(1 / n, n, n))
    }

    blocks <- factor(design[[block_col]])
    b <- nlevels(blocks)

    X2 <- matrix(0, n, b)
    for (j in seq_len(b)) {
        X2[blocks == levels(blocks)[j], j] <- 1
    }

    ## L = I - X2 (X2t X2)^{-1} X2t
    X2tX2_inv <- diag(1 / colSums(X2), nrow = b)
    diag(n) - X2 %*% X2tX2_inv %*% t(X2)
}


# Helper functions

#' Compute L projection
#'
#' This is the projection matrix that removes nuisance effects from the GLS's
#' covariance
#'
#' @param design Data frame representing the spatial layout of the experiment.
#' @param Sigma Covariance structure to use.
#' @param block_col Column name of the design's block factor in \code{design}.
#'
#' @export
compute_L_projection <- function(design, Sigma, block_col = "block") {
    n <- nrow(design)

    ## arg checks
    if (!is.matrix(Sigma)) stop("Sigma must be a matrix")
    if (nrow(Sigma) != n) stop("Sigma matrix must have dimension equal to dimension of design dataframe")
    if (ncol(Sigma) != n) stop("Sigma matrix must have dimension equal to dimension of design dataframe")
    if (!isSymmetric(Sigma, tol = 1e-8)) stop("Sigma must be symmetric")
    if (!(block_col %in% names(design))) stop("Block column name not in design dataframe")

    blocks <- factor(design[[block_col]])
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
#' @param design Data frame representing the spatial layout of the experiment.
#' @param treatment_col Column name containing the design's treatments in
#'   \code{design}.
#' @param L_matrix Precomputed projection matrix from
#'   \code{compute_L_projection}.
#' @param block_col Column name containing the designs block in \code{design}.
#'
#' @export
calc_info_matrix <- function(
    design,
    treatment_col = "treatment",
    L_matrix = NULL,
    block_col = "block"
) {
    treatments <- design[[treatment_col]]
    trt_levels <- levels(factor(treatments))
    v <- length(trt_levels)
    n <- nrow(design)

    # TODO: Duplicated from above. Probably try and simplify later
    ## L
    ### projection matrix
    if (is.null(L_matrix)) {
        L_matrix <- .build_L_from_df(design, block_col, n)
    }

    ## X1
    ### treatment design matrix.
    X1 <- .build_treatment_matrix(treatments, trt_levels, n, v)

    ## information matrix I = X1t L X1
    info <- t(X1) %*% L_matrix %*% X1

    rownames(info) <- trt_levels
    colnames(info) <- trt_levels

    ## get positive eigenvals from info matrix
    eig <- eigen(info, symmetric = TRUE, only.values = TRUE)$values
    pos_eig <- eig[eig > 1e-10]

    ## if design is disconnected, penalise greatly
    ## otherwise A or D
    if (length(pos_eig) < v - 1) {
        A_val <- Inf
        D_val <- Inf
    } else {
        A_val = sum(1 / pos_eig)
        D_val = -sum(log(pos_eig))
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
#' @param design Data frame representing the spatial layout of the experiment.
#' @param treatment_col Column name containing the design's treatments in
#'   \code{design}.
#' @param block_col Column name containing the designs block in \code{design}.
#'
#' @export
calc_incidence_matrix <- function(
    design,
    treatment_col = "treatment",
    block_col = "block"
) {
    treatments <- factor(design[[treatment_col]])
    blocks <- factor(design[[block_col]])

    ## make a matrix of integers
    N <- table(treatments, blocks)
    class(N) <- "matrix"
    storage.mode(N) <- "integer"

    return(N)
}

#' Calculate concurrence matrix
#'
#' @param design Data frame representing the spatial layout of the experiment.
#' @param treatment_col Column name containing the design's treatments in
#'   \code{design}.
#' @param block_col Column name containing the designs block in \code{design}.
#'
#' @export
calc_concurrence_matrix <- function(
    design,
    treatment_col = "treatment",
    block_col = "block"
) {
    N <- calc_incidence_matrix(design, treatment_col, block_col)
    C <- N %*% t(N)
    storage.mode(C) <- "integer"
    return(C)
}

#' Calculate canonical efficiency factors
#'
#' @param design Data frame representing the spatial layout of the experiment.
#' @param treatment_col Column name containing the design's treatments in
#'   \code{design}.
#' @param L_matrix Precomputed projection matrix from
#'   \code{compute_L_projection}.
#' @param block_col Column name containing the designs block in \code{design}.
#'
#' @export
calculate_efficiency_factors <- function(
    design,
    treatment_col = "treatment",
    L_matrix = NULL,
    block_col = "block"
) {
    info <- calc_info_matrix(
        design,
        treatment_col,
        L_matrix,
        block_col
    )

    treatments <- factor(design[[treatment_col]])
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
#' @export
cor_ar1_ar1 <- function(n_rows, n_cols, rho_row, rho_col) {
    kronecker(
        cor_ar1(n_rows, rho_row),
        cor_ar1(n_cols, rho_col)
    )
}


## Testing
set.seed(1)
n_treatments <- 4
n_blocks <- 3

df <- data.frame(
    row       = rep(1:n_blocks, each = n_treatments),
    col       = rep(1:n_treatments, times = n_blocks),
    block     = rep(1:n_blocks, each = n_treatments),
    treatment = NA_character_
)

scores <- numeric(20)
for (i in seq_len(20)) {
    for (b in 1:n_blocks) {
        idx <- df$block == b
        df$treatment[idx] <- paste0("T", sample(n_treatments))
    }
    result <- objective_function_info(
        df, swap = "treatment", spatial_cols = c("row", "col"),
        criterion = "A"
    )
    scores[i] <- result$score
}
