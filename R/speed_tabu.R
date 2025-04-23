speed_tabu <- function(data,
                       permute,
                       swap = ~ 1,
                       spatialFactors = ~ Row + Col,
                       swap_count = 1,
                       swap_all_blocks = FALSE,
                       adaptive_swaps = FALSE,
                       iterations = 10000,
                       early_stop_iterations = 2000,
                       quiet = FALSE,
                       tabu_size = 20,           # Size of tabu list
                       tabu_tenure_modifier = 1,  # Multiplier for tabu tenure
                       candidate_moves = 5,       # Number of candidate moves to evaluate
                       diversification_freq = 50, # How often to diversify search
                       adj_weight = 1,            # Weight for adjacency score in objective function
                       bal_weight = 1) {          # Weight for balance score in objective function

    # Input validation
    if (!is.data.frame(data))
        stop("data must be an initial data frame of the design")
    layout_df <- data
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
        swap_vals <- factor(rep(1, nrow(data)))
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

    # Initialize design
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

    # Scoring functions
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

    # Generate a candidate move
    generate_move <- function(design, swap_mat, swap_count, swap_all_blocks) {
        swap_levels <- unique(as.vector(swap_mat))
        if(!swap_all_blocks) swap_levels <- sample(swap_levels, 1)

        # Generate and return move positions
        moves <- list()
        for (level in swap_levels) {
            for (i in 1:swap_count) {
                level <- sample(swap_levels, 1)
                positions <- which(swap_mat == level, arr.ind = TRUE)
                if (nrow(positions) < 2) next
                idx <- sample(1:nrow(positions), 2)
                pos1 <- positions[idx[1], ]
                pos2 <- positions[idx[2], ]
                moves <- c(moves, list(list(pos1 = pos1, pos2 = pos2)))
            }
        }
        return(moves)
    }

    # Apply move to design
    apply_move <- function(design, move) {
        new_design <- design
        for (swap in move) {
            pos1 <- swap$pos1
            pos2 <- swap$pos2
            tmp <- new_design[pos1[1], pos1[2]]
            new_design[pos1[1], pos1[2]] <- new_design[pos2[1], pos2[2]]
            new_design[pos2[1], pos2[2]] <- tmp
        }
        return(new_design)
    }

    # Create move key for tabu list
    create_move_key <- function(move) {
        keys <- character(length(move))
        for (i in seq_along(move)) {
            swap <- move[[i]]
            keys[i] <- paste(
                paste0(swap$pos1[1], "-", swap$pos1[2], "-", current_design[swap$pos1[1], swap$pos1[2]]),
                paste0(swap$pos2[1], "-", swap$pos2[2], "-", current_design[swap$pos2[1], swap$pos2[2]]),
                sep = "_"
            )
        }
        paste(keys, collapse = "|")
    }

    # Initialize tabu list - will store iteration when move was made
    tabu_list <- new.env(hash = TRUE)

    # Initialize tracking variables
    current_score <- calculate_objective(current_design, permute_var, layout_df, spatial_fac)
    best_score <- current_score
    scores <- numeric(iterations)
    last_improvement_iter <- 0

    # Main tabu search loop
    for (iter in 1:iterations) {
        scores[iter] <- current_score

        # Adjust parameters if adaptive
        if (adaptive_swaps) {
            freq_factor <- min(1, (iter - last_improvement_iter) / diversification_freq)
            current_swap_count <- max(1, round(swap_count * (1 + freq_factor)))
            current_swap_all_blocks <- freq_factor > 0.7 && swap_all_blocks
        } else {
            current_swap_count <- swap_count
            current_swap_all_blocks <- swap_all_blocks
        }

        # Generate candidate moves
        best_candidate <- NULL
        best_candidate_score <- Inf

        for (c in 1:candidate_moves) {
            # Generate a candidate move
            move <- generate_move(current_design, swap_mat, current_swap_count, current_swap_all_blocks)
            if (length(move) == 0) next

            # Check if move is tabu
            move_key <- create_move_key(move)
            is_tabu <- exists(move_key, envir = tabu_list) &&
                (iter - get(move_key, envir = tabu_list) <= tabu_tenure_modifier * tabu_size)

            # Apply move to get new design
            candidate_design <- apply_move(current_design, move)
            candidate_score <- calculate_objective(candidate_design, permute_var, layout_df, spatial_fac)

            # Aspiration criterion: accept tabu move if it gives the best solution found so far
            if (!is_tabu || candidate_score < best_score) {
                if (candidate_score < best_candidate_score) {
                    best_candidate <- list(design = candidate_design, score = candidate_score, move = move)
                    best_candidate_score <- candidate_score
                }
            }
        }

        # If no valid move found (all were tabu), generate a random non-tabu move
        if (is.null(best_candidate)) {
            repeat {
                move <- generate_move(current_design, swap_mat, current_swap_count, TRUE)
                if (length(move) == 0) next
                move_key <- create_move_key(move)
                if (!exists(move_key, envir = tabu_list) ||
                    (iter - get(move_key, envir = tabu_list) > tabu_tenure_modifier * tabu_size)) {
                    candidate_design <- apply_move(current_design, move)
                    candidate_score <- calculate_objective(candidate_design, permute_var, layout_df, spatial_fac)
                    best_candidate <- list(design = candidate_design, score = candidate_score, move = move)
                    break
                }
            }
        }

        # Update current solution
        current_design <- best_candidate$design
        current_score <- best_candidate$score

        # Add move to tabu list
        move_key <- create_move_key(best_candidate$move)
        assign(move_key, iter, envir = tabu_list)

        # Maintain tabu list size (optional - can just let the tenure expire)
        if (iter %% 100 == 0) {
            tabu_keys <- ls(tabu_list)
            for (key in tabu_keys) {
                if (iter - get(key, envir = tabu_list) > tabu_tenure_modifier * tabu_size * 2) {
                    rm(list = key, envir = tabu_list)
                }
            }
        }

        # Update best solution if improved
        if (current_score < best_score) {
            best_design <- current_design
            best_score <- current_score
            last_improvement_iter <- iter
        }

        # Implement diversification if no improvement for a while
        if (iter - last_improvement_iter == diversification_freq) {
            if (!quiet) cat("Diversifying search at iteration", iter, "\n")

            # Increase swap count temporarily for greater diversification
            temp_swap_count <- min(5, swap_count * 3)
            diversify_moves <- generate_move(current_design, swap_mat, temp_swap_count, TRUE)
            current_design <- apply_move(current_design, diversify_moves)
            current_score <- calculate_objective(current_design, permute_var, layout_df, spatial_fac)
        }

        # Reporting
        if (!quiet && iter %% 1000 == 0) {
            cat("Iteration:", iter, "Score:", current_score, "Best:", best_score,
                "Since Improvement:", iter - last_improvement_iter,
                "Tabu Size:", length(ls(tabu_list)), "\n")
        }

        # Early stopping
        if (iter - last_improvement_iter >= early_stop_iterations) {
            if (!quiet) cat("Early stopping at iteration", iter, "\n")
            scores <- scores[1:iter]
            break
        }
    }

    # Prepare output
    design_df <- layout_df
    design_df[[permute_var]] <- as.vector(best_design)

    return(list(
        design = best_design,
        design_df = design_df,
        score = best_score,
        adjacency_score = calculate_adjacency_score(best_design),
        balance_score = calculate_balance_score(design_df, permute_var, spatial_fac),
        scores = scores,
        iterations_run = length(scores),
        stopped_early = length(scores) < iterations,
        treatments = treatments,
        final_tabu_size = length(ls(tabu_list))
    ))
}
