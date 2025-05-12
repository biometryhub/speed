#' Default objective functions
#'
#' @description
#' Default Objective Function for Design Optimization
#'
#' @rdname objective_functions
#'
#' @param layout_df A data frame representing the spatial information of the design
#' @param swap A column name of the items to be swapped
#' @param spatial_cols Column name(s) of the spatial factors
#'
#' @examples
#' layout_df <- data.frame(
#'   row = rep(1:3, each = 3),
#'   col = rep(1:3, times = 3),
#'   treatment = rep(letters[1:3], 3)
#' )
#' objective_function(layout_df, "treatment", c("row", "col"))
#'
#' @return A numeric value representing the score of the design (lower is better)
#' @export
objective_function_signature <- function(
        layout_df,
        swap,
        spatial_cols,
        ...) {
    stop("This is a dummy fucntion for documentation purposes only")
}


#' Default Objective Function for Design Optimization
#'
#' @inheritParams objective_function_signature
#' @param adj_weight Weight for adjacency score (default: 0)
#' @param bal_weight Weight for balance score (default: 1)
#'
#' @rdname objective_functions
#' @export
objective_function <- function(layout_df, swap, spatial_cols,
                               adj_weight = getOption("speed.adj_weight", 0),
                               bal_weight = getOption("speed.bal_weight", 1)) {

    adj_score <- ifelse(adj_weight != 0,
                        calculate_adjacency_score(layout_df, swap),
                        0)

    bal_score <- ifelse(bal_weight != 0,
                        calculate_balance_score(layout_df, swap, spatial_cols),
                        0)

    return(round(adj_weight * adj_score + bal_weight * bal_score, 10))
}



#' Calculate Balance Score for Experimental Design
#'
#' @description
#' Calculates a balance score that measures how evenly treatments are distributed
#'   across spatial factors in an experimental design. Lower scores indicate better balance.
#'
#' @inheritParams objective_function_signature
#'
#' @return Numeric value representing the total balance score. Lower values indicate
#'   better balance of treatments across spatial factors.
#'
#' @examples
#' layout_df <- data.frame(
#'   row = rep(1:3, each = 3),
#'   col = rep(1:3, times = 3),
#'   treatment = rep(letters[1:3], 3)
#' )
#' calculate_balance_score(layout_df, "treatment", c("row", "col"))
#'
#' @export
calculate_balance_score <- function(layout_df, swap, spatial_cols) {
    score <- vapply(spatial_cols, function(el) {
                    sum(matrixStats::rowVars(table(layout_df[[el]], layout_df[[swap]]), na.rm = TRUE), na.rm = TRUE)
    }, numeric(1))
    return(sum(score))
}

#' Calculate Adjacency Score for Design
#'
#' @description
#' Calculates the adjacency score for a given experimental design. The adjacency score
#' represents the number of adjacent plots with the same treatment. Lower scores indicate
#' better separation of treatments.
#'
#' @inheritParams objective_function_signature
#'
#' @return Numeric score for treatment adjacencies (lower is better)
#'
#' @examples
#' # Example 1: Design with no adjacencies
#' design_no_adj <- data.frame(
#'   row = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   col = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   treatment = c("A", "B", "A", "B", "A", "B", "A", "B", "A")
#' )
#'
#' # Gives 0
#' calculate_adjacency_score(design_no_adj, "treatment")
#'
#' # Example 2: Design with adjacencies
#' design_with_adj <- data.frame(
#'   row = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'   col = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
#'   treatment = c("A", "A", "A", "B", "B", "B", "A", "A", "A")
#' )
#'
#' # Gives value 6
#' calculate_adjacency_score(design_with_adj, "treatment")
#' @export
calculate_adjacency_score <- function(layout_df, swap, spatial_cols) {
    layout_df <- matrix(layout_df[[swap]],
                     nrow = max(as.numeric(as.character(layout_df$row)), na.rm = TRUE),
                     ncol = max(as.numeric(as.character(layout_df$col)), na.rm = TRUE),
                     byrow = FALSE)

    row_adjacencies <- sum(layout_df[, -ncol(layout_df)] == layout_df[, -1], na.rm = TRUE)
    col_adjacencies <- sum(layout_df[-nrow(layout_df), ] == layout_df[-1, ], na.rm = TRUE)
    return(row_adjacencies + col_adjacencies)
}











#' Objective Function with Metric from Piepho
#'
#' @description
#' Create an objective function including even distribution and neighbor balance introduced by Piepho 2018.
#'
#' @inheritParams calculate_nb
#'
#' @examples
#' design_df <- speed::initialize_design_df(
#'   items = c(1, 2, 2, 1, 3, 3, 1, 3, 3),
#'   nrows = 3,
#'   ncols = 3
#' )
#'
#' pair_mapping <- create_pair_mapping(design_df$treatment)
#' objective_function_piepho(design_df, "treatment", c("row", "col"), pair_mapping = pair_mapping)
#'
#' @return A function which returns a named list of numeric values with one required name `score` representing
#'   the score of the design (lower is better) with a signature
#'   `function(design_matrix, layout_df, swap, spatial_cols, previous_score, swapped_items)`. See signature
#'   details in [objective_function_signature].
#'
#' @references Piepho, H. P., Michel, V., & Williams, E. (2018). Neighbor balance and evenness of distribution of treatment replications in row-column designs. Biometrical journal. Biometrische Zeitschrift, 60(6), 1172–1189. <https://doi.org/10.1002/bimj.201800013>
#'
#' @seealso [objective_function()], [create_pair_mapping()]
#'
#' @export
objective_function_piepho <- function(layout_df, swap, spatial_cols, previous_score = NULL, swapped_items = NULL, pair_mapping = NULL) {
    design_matrix <- matrix(layout_df[[swap]], nrow = max(layout_df$row), ncol = max(layout_df$col))

    ed <- calculate_ed(design_matrix, previous_score$ed, swapped_items)
    ed_score <- -sum(vapply(ed, function(ed_rep) ed_rep$min_mst, numeric(1)))
    nb_score <- calculate_nb(design_matrix, pair_mapping)$max_nb

    layout_df[[swap]] <- as.vector(design_matrix)
    bal_score <- calculate_balance_score(layout_df, swap, spatial_cols)

    return(list(score = nb_score + ed_score + bal_score, ed = ed))
}

#' Neighbour Balance Calculation
#'
#' @description
#' A metric that counts the occurrence of the same adjacent pairs. Only horizontal and vertical pairs are
#'   counted.
#'
#' @inheritParams objective_function_signature
#' @param pair_mapping A named vector of pairs generated from [create_pair_mapping]
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
#' calculate_nb(design_matrix)
#'
#' @return Named list containing:
#' \itemize{
#'   \item nb - Table of pairs of items and their number of occurrence
#'   \item max_nb - The highest number of occurrence
#'   \item max_pairs - Vector of pairs of items with the highest number of occurrence
#' }
#'
#' @seealso [objective_function_piepho()]
#'
#' @export
calculate_nb <- function(design_matrix, pair_mapping = NULL) {
    if (is.null(pair_mapping)) {
        return(.calculate_nb(design_matrix))
    }

    lefts <- design_matrix[, -ncol(design_matrix)]
    rights <- design_matrix[, -1]
    tops <- design_matrix[-nrow(design_matrix), ]
    bottoms <- design_matrix[-1, ]
    lr_pairs <- paste(lefts, rights, sep = ",")
    tb_pairs <- paste(tops, bottoms, sep = ",")

    pairs <- c(lr_pairs, tb_pairs)
    is_sorted <- pairs %in% pair_mapping
    sorted_pairs <- c(pairs[is_sorted], pair_mapping[pairs[!is_sorted]])

    nb <- table(sorted_pairs)
    max_nb <- max(nb)
    max_pairs <- names(nb[nb == max_nb])
    return(list(
        nb = nb,
        max_nb = max_nb,
        max_pairs = max_pairs
    ))
}

#' Neighbor Balance Calculation without Pair Mapping
#'
#' @description
#' A metric that counts the occurrence of the same adjacent pairs. Only horizontal and vertical pairs are
#'   counted.
#'
#' @inheritParams objective_function_signature
#'
#' @return Named list containing:
#' \itemize{
#'   \item nb - Named list of pairs of items and their number of occurrence
#'   \item max_nb - The highest number of occurrence
#'   \item max_pairs - Vector of pairs of items with the highest number of occurrence
#' }
#'
#' @keywords internal
.calculate_nb <- function(design_matrix) {
    # TODO: check function overloading
    n_rows <- dim(design_matrix)[1]
    n_cols <- dim(design_matrix)[2]
    # env is faster than list
    nb <- new.env()

    for (row_ in 1:n_rows) {
        for (col_ in 1:n_cols) {
            node <- design_matrix[row_, col_]
            if (row_ < n_rows) {
                bottom <- design_matrix[row_ + 1, col_]
                if (node < bottom) {
                    pair_str <- paste0(node, ",", bottom)
                } else {
                    pair_str <- paste0(bottom, ",", node)
                }

                env_add_one(nb, pair_str)
            }

            if (col_ < n_cols) {
                right <- design_matrix[row_, col_ + 1]
                if (node < right) {
                    pair_str <- paste0(node, ",", right)
                } else {
                    pair_str <- paste0(right, ",", node)
                }

                env_add_one(nb, pair_str)
            }
        }
    }

    nb <- as.list(nb)
    max_nb <- max(unlist(nb))
    max_pairs <- names(nb[nb == max_nb])
    return(list(
        nb = nb,
        max_nb = max_nb,
        max_pairs = max_pairs
    ))
}

#' Even Distribution Calculation
#'
#' @description
#' A metric that represents the even distribution of each item with their minimum spanning tree (mst).
#'
#' @inheritParams objective_function_signature
#' @param previous_ed Named list of the previous ed calculation
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
#' calculate_ed(design_matrix)
#'
#' @return Named list containing:
#' \itemize{
#'   \item <number of replications> - Named list containing:
#'     \itemize{
#'       \item msts - Named list of items and their mst
#'       \item min_mst - The lowest mst
#'       \item min_items - Pairs of items with the lowest mst
#'     }
#' }
#'
#' @seealso [objective_function_piepho()]
#'
#' @export
calculate_ed <- function(design_matrix, previous_ed = NULL, swapped_items = NULL) {
    if (!is.null(swapped_items)) {
        design_matrix[!(design_matrix %in% swapped_items)] <- NA
        msts <- lapply(previous_ed, function(ed_by_rep) ed_by_rep$msts)
    } else {
        msts <- list()
    }

    vertices <- get_vertices(design_matrix)
    edges <- get_edges(vertices)

    edges_3_reps <- list()
    sub_graph <- list()

    for (item in names(vertices)) {
        reps <- length(vertices[[item]])
        reps_char <- as.character(reps)
        if (reps == 2) {
            # distance between 2 nodes for 2 reps
            msts[[reps_char]][[item]] <- edges[[item]]
        } else if (reps == 3) {
            # 3 reps will be calculated with .calculate_ed_3_reps
            edges_3_reps[[item]] <- edges[[item]]
        } else if (reps > 3) {
            # blanket igraph for 4+ reps
            if (is.null(sub_graph[[reps_char]])) {
                # initialize a fully-connected graph without weights
                # 1--2, 1--3, ..., 1--n-1, 1--n, 2--3, 2--4, ..., n-1--n
                edge_table <- t(combn(1:reps, 2))
                sub_graph[[reps_char]] <- igraph::graph_from_edgelist(edge_table, directed = FALSE)
            }

            igraph::E(sub_graph[[reps_char]])$weight <- edges[[item]]
            msts[[reps_char]][[item]] <- sum(igraph::E(igraph::mst(sub_graph[[reps_char]]))$weight)
        }
    }

    # summarize mst for each reps
    ed <- lapply(msts, function(msts_by_reps) {
        min_mst <- min(unlist(msts_by_reps))
        min_items <- names(msts_by_reps[msts_by_reps == min_mst])

        return(list(
            msts = msts_by_reps,
            min_mst = min_mst,
            min_items = min_items
        ))
    })

    if (length(edges_3_reps) > 0) {
        ed$`3` <- .calculate_ed_3_reps(edges_3_reps, previous_ed)
    }

    return(ed)
}

#' Get Vertices of Each Item
#'
#' @description
#' Get the vertices of each item in a design matrix.
#'
#' @inheritParams objective_function_signature
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 2, 1, 3, 3), nrow = 3, ncol = 3)
#' vertices <- get_vertices(design_matrix)
#'
#' @return Named list containing:
#'   \itemize{
#'     \item <item> - A list of (vertex 1, vertex 2, ...)
#'   }
#'
#' @seealso [get_edges()]
#'
#' @export
get_vertices <- function(design_matrix) {
    # Create vectors of row, col indices and corresponding values
    rows <- row(design_matrix)
    cols <- col(design_matrix)
    items <- as.character(design_matrix)

    # Combine row and col into coordinates
    coords <- Map(c, as.vector(rows), as.vector(cols))

    # Use split to group coordinates by item
    return(split(coords, items))
}

#' Get Weighted Edges
#'
#' @description
#' Calculate the weight of edges from vertices.
#'
#' @param vertices Named list of vertices containing:
#'   \itemize{
#'     \item <item> - A list of (vertex 1, vertex 2, ...)
#'   }
#'
#' @examples
#' design_matrix <- matrix(c(1, 2, 2, 1, 3, 2, 1, 3, 3), nrow = 3, ncol = 3)
#' vertices <- get_vertices(design_matrix)
#' edges <- get_edges(vertices)
#'
#' @return Named list containing:
#'   \itemize{
#'     \item <item> - A vector of edge weights
#'   }
#'
#' @seealso [get_vertices()]
#'
#' @export
get_edges <- function(vertices) {
    edges <- vector("list", length(vertices))
    names(edges) <- names(vertices)

    for (item in names(vertices)) {
        coords <- vertices[[item]]
        n_vertices <- length(coords)
        if (n_vertices < 2) {
            edges[[item]] <- c()
            next
        }

        # Preallocate vector to hold all edges
        item_edges <- numeric(n_vertices * (n_vertices - 1) / 2)
        idx <- 1

        for (i in 1:(n_vertices - 1)) {
            for (j in (i + 1):n_vertices) {
                item_edges[[idx]] <- sqrt(sum((coords[[i]] - coords[[j]])^2))
                idx <- idx + 1
            }
        }

        edges[[item]] <- item_edges
    }

    return(edges)
}

#' Even Distribution Calculation for 3 Replications
#'
#' @description
#' A metric that represents the even distribution of items with 3 replications with their minimum spanning tree
#'   (mst).
#'
#' @param edges A list of vectors of edge weights
#'
#' @importFrom utils modifyList
#'
#' @return Named list containing:
#' \itemize{
#'   \item msts - Named list of pairs of items and their mst
#'   \item min_mst - The lowest mst
#'   \item min_items - Pairs of items with the lowest mst
#' }
#'
#' @seealso [get_edges()]
#'
#' @keywords internal
.calculate_ed_3_reps <- function(edges, previous_ed = NULL) {
    # pick 2 shortest connections for 3 reps
    ed <- lapply(
        edges, function(weights) {
            sum(weights) - max(weights)
        }
    )

    if (!is.null(previous_ed)) {
        ed <- modifyList(previous_ed$`3`$msts, ed)
    }

    min_mst <- min(unlist(ed))
    min_items <- names(ed[ed == min_mst])
    return(list(
        msts = ed,
        min_mst = min_mst,
        min_items = min_items
    ))
}


#' Create Pair Mapping
#'
#' @description
#' Create an item pair mapping for [calculate_nb].
#'
#' @param items Vector of items for the design
#'
#' @importFrom stats setNames
#'
#' @examples
#' treatments <- c(rep(1:10, 4), rep(11:16, 3), rep(17:27, 2))
#' create_pair_mapping(treatments)
#'
#' @return Named vector of item pairs as a character separated by `","`:
#' \itemize{
#'   \item "<item 2>,<item 1>" - "<item 1>,<item 2>"
#'   \item "<item 3>,<item 1>" - "<item 1>,<item 3>"
#'   \item ...
#'   \item "<item n-1>,<item 1>" - "<item 1>,<item n-1>"
#'   \item "<item n>,<item 1>" - "<item 1>,<item n>"
#'   \item "<item 3>,<item 2>" - "<item 2>,<item 3>"
#'   \item "<item 4>,<item 2>" - "<item 2>,<item 4>"
#'   \item ...
#'   \item "<item n-1>,<item 2>" - "<item 2>,<item n-1>"
#'   \item "<item n>,<item 2>" - "<item 2>,<item n>"
#'   \item ...
#'   \item "<item n>,<item n-1>" - "<item n-1>,<item n>"
#'   \item "<item 1>,<item 1>" - "<item 1>,<item 1>"
#'   \item "<item 2>,<item 2>" - "<item 2>,<item 2>"
#'   \item ...
#'   \item "<item n-1>,<item n-1>" - "<item n-1>,<item n-1>"
#'   \item "<item n>,<item n>" - "<item n>,<item n>"
#' }
#'
#' @export
create_pair_mapping <- function(items) {
    items <- unique(items)
    combinations <- combn(sort(items), 2)

    identical_pairs <- paste(items, items, sep = ",")
    pairs <- paste(combinations[1, ], combinations[2, ], sep = ",")
    pairs_r <- sapply(pairs, function(k) paste(rev(strsplit(k, ",")[[1]]), collapse = ","))

    pair_mapping <- setNames(c(pairs, identical_pairs), c(pairs_r, identical_pairs))
    return(pair_mapping)
}




