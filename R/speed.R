
speed <- function(df,
                  permute,
                  swap = ~ 1,
                  spatialFactors = ~ Row + Col,
                  swap_count = 1,
                  swap_all_blocks = FALSE,
                  adaptive_swaps = FALSE,
                  iterations = 10000,
                  early_stop_iterations = 2000,
                  quiet = FALSE,
                  start_temp = 100,
                  cooling_rate = 0.99,
                  adj_weight = 1,
                  bal_weight = 1) {

    if (!is.data.frame(df))
        stop("df must be an initial data frame of the design")
    layout_df <- df
    if(!inherits(permute, "formula"))
        stop("permute must be a one sided formula")
    permute_var <- deparse(permute[[2]])
    if (!(permute_var %in% names(layout_df)))
        stop("permute column not found in data frame")
    permute_vals <- layout_df[[permute_var]]
    if(!inherits(swap, "formula"))
        stop("swap must be a one sided formula")
    swap_var <- deparse(swap[[2]])
    if(swap_var == "1")
        swap_vals <- factor(rep(1, nrow(df)))
    else if (!(swap_var %in% names(layout_df)))
        stop("swap column not found in data frame")
    else swap_vals <- layout_df[[swap_var]]
    if(!inherits(spatialFactors, "formula"))
        stop("spatialFactors must be a one sided formula")
    spatial_fac <- all.vars(spatialFactors)
    if (!all(spatial_fac %in% names(layout_df)))
        stop("One or more spatialFactors not found in data frame")

    # Derive dimensions
    row <- as.integer(as.character(layout_df$Row))
    col <- as.integer(as.character(layout_df$Col))
    nrows <- max(row)
    ncols <- max(col)
    permute_mat <- matrix(permute_vals, nrow = nrows, ncol = ncols, byrow = FALSE)
    swap_mat <- matrix(swap_vals, nrow = nrows, ncol = ncols, byrow = FALSE)
    treatments <- unique(as.vector(permute_mat))

    perm_design <- function(permute_mat, swap_mat) {
        design <- matrix(NA, nrow = nrows, ncol = ncols)
        if (is.null(swap_mat)) {
            design[] <- sample(as.vector(permute_mat))
        } else {
            for (level in unique(as.vector(swap_mat))) {
                mask <- which(swap_mat == level, arr.ind = TRUE)
                vals <- permute_mat[mask]
                design[mask] <- sample(vals)
            }
        }
        return(design)
    }
    current_design <- perm_design(permute_mat, swap_mat)
    best_design <- current_design

    calculate_adjacency_score <- function(design) {
        row_adjacencies <- rowSums(design[, -ncol(design)] == design[, -1], na.rm = TRUE)
        col_adjacencies <- colSums(design[-nrow(design), ] == design[-1, ], na.rm = TRUE)
        sum(row_adjacencies) + sum(col_adjacencies)
    }

    calculate_balance_score <- function(layout_df, permute_var, spatial_fac) {
        bscore <- sapply(spatial_fac, function(el)
            sum(apply(table(layout_df[[el]], layout_df[[permute_var]]), 1, var)))
        sum(bscore)
    }

    calculate_objective <- function(design, permute_var, layout_df, spatial_fac) {
        layout_df[[permute_var]] <- as.vector(design)
        adj <- calculate_adjacency_score(design)
        bal <- calculate_balance_score(layout_df, permute_var, spatial_fac)
        adj_weight * adj + bal_weight * bal
    }

    generate_neighbor <- function(design, swap_mat, swap_count, swap_all_blocks) {
        new_design <- design
        swap_levels <- unique(as.vector(swap_mat))
        if(!swap_all_blocks) swap_levels <- sample(swap_levels, 1)
        for (level in swap_levels) {
            for (i in 1:swap_count) {
                level <- sample(swap_levels, 1)
                positions <- which(swap_mat == level, arr.ind = TRUE)
                if (nrow(positions) < 2) next
                idx <- sample(1:nrow(positions), 2)
                pos1 <- positions[idx[1], ]; pos2 <- positions[idx[2], ]
                tmp <- new_design[pos1[1], pos1[2]]
                new_design[pos1[1], pos1[2]] <- new_design[pos2[1], pos2[2]]
                new_design[pos2[1], pos2[2]] <- tmp
            }
        }
        return(new_design)
    }

    current_score <- calculate_objective(current_design, permute_var, layout_df, spatial_fac)
    best_score <- current_score
    temp <- start_temp
    scores <- numeric(iterations)
    temperatures <- numeric(iterations)
    last_improvement_iter <- 0
    for (iter in 1:iterations) {
        scores[iter] <- current_score
        temperatures[iter] <- temp
        if (adaptive_swaps) {
            current_swap_count <- max(1, round(swap_count * temp / start_temp))
            current_swap_all_blocks <- runif(1) < (temp / start_temp) && swap_all_blocks
        } else {
            current_swap_count <- swap_count
            current_swap_all_blocks <- swap_all_blocks
        }
        new_design <- generate_neighbor(current_design, swap_mat, current_swap_count, current_swap_all_blocks)
        new_score <- calculate_objective(new_design, permute_var, layout_df, spatial_fac)
        if (new_score < current_score || runif(1) < exp((current_score - new_score) / temp)) {
            current_design <- new_design
            current_score <- new_score
            if (new_score < best_score) {
                best_design <- new_design
                best_score <- new_score
                last_improvement_iter <- iter
            }
        }
        temp <- temp * cooling_rate
        if (!quiet && iter %% 1000 == 0) {
            cat("Iteration:", iter, "Score:", current_score, "Best:", best_score,
                "Since Improvement:", iter - last_improvement_iter, "\n")
        }
        if (iter - last_improvement_iter >= early_stop_iterations) {
            if (!quiet) cat("Early stopping at iteration", iter, "\n")
            scores <- scores[1:iter]
            temperatures <- temperatures[1:iter]
            break
        }
    }
    design_df <- layout_df
    design_df[[permute_var]] <- as.vector(best_design)
    return(list(
        design = best_design,
        design_df = design_df,
        score = best_score,
        adjacency_score = calculate_adjacency_score(best_design),
        balance_score = calculate_balance_score(design_df, permute_var, spatial_fac),
        scores = scores,
        temperatures = temperatures,
        iterations_run = length(scores),
        stopped_early = length(scores) < iterations,
        treatments = treatments
    ))
}
