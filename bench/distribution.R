# Distribution of optimal designs: 4 treatments, 6 reps, 6 rows, 4 columns.
# Optimal = no like-treatment neighbours, every row holds all 4 treatments and
# every column holds two treatments twice and two once.
n_treatments_dist <- 4
n_rows_dist <- 6
n_cols_dist <- 4

# all permutations of 1..n, one per row
permutations <- function(n) {
  if (n == 1) {
    return(matrix(1L))
  }

  sub <- permutations(n - 1)
  out <- lapply(seq_len(n), function(i) {
    return(cbind(i, matrix(setdiff(seq_len(n), i)[sub], nrow = nrow(sub))))
  })

  return(do.call(rbind, out))
}

# every optimal design as a row of treatments, plots in row-major order
enumerate_dist <- function() {
  rows <- permutations(n_treatments_dist)
  n_perms <- nrow(rows)
  # rows sharing no column entry can be stacked without vertical neighbours
  compat <- outer(
    seq_len(n_perms),
    seq_len(n_perms),
    Vectorize(function(i, j) return(all(rows[i, ] != rows[j, ])))
  )
  # column x treatment one-hot of each row
  onehot <- t(apply(rows, 1, function(r) {
    x <- integer(n_cols_dist * n_treatments_dist)
    x[(seq_len(n_cols_dist) - 1) * n_treatments_dist + r] <- 1L
    return(x)
  }))
  cap <- ceiling(n_rows_dist / n_treatments_dist)

  seqs <- matrix(seq_len(n_perms), ncol = 1)
  counts <- onehot
  for (k in seq_len(n_rows_dist)[-1]) {
    nxt <- which(compat[seqs[, k - 1], , drop = FALSE], arr.ind = TRUE)
    new_counts <- counts[nxt[, 1], , drop = FALSE] +
      onehot[nxt[, 2], , drop = FALSE]
    keep <- rowSums(new_counts > cap) == 0
    seqs <- cbind(seqs[nxt[keep, 1], , drop = FALSE], nxt[keep, 2])
    counts <- new_counts[keep, , drop = FALSE]
  }
  # most even split also leaves no treatment absent from a column
  keep <- rowSums(counts < floor(n_rows_dist / n_treatments_dist)) == 0
  seqs <- seqs[keep, , drop = FALSE]

  return(do.call(
    cbind,
    lapply(seq_len(n_rows_dist), function(k) {
      return(rows[seqs[, k], , drop = FALSE])
    })
  ))
}

# key shared by designs equal up to treatment relabelling, column permutation
# and vertical flip; assumes optimal designs (first row holds every treatment)
canonical_key <- function(designs) {
  n <- nrow(designs)
  col_perms <- permutations(n_cols_dist)
  weights <- n_treatments_dist^(rev(seq_len(ncol(designs))) - 1)
  best <- rep(Inf, n)
  for (row_order in list(seq_len(n_rows_dist), rev(seq_len(n_rows_dist)))) {
    for (p in seq_len(nrow(col_perms))) {
      idx <- as.vector(t(outer(
        (row_order - 1) * n_cols_dist,
        col_perms[p, ],
        "+"
      )))
      d <- designs[, idx, drop = FALSE]
      # relabel so the first row reads 1..t
      inv <- matrix(0L, n, n_treatments_dist)
      inv[cbind(rep(seq_len(n), n_cols_dist), as.vector(d[, seq_len(n_cols_dist)]))] <-
        rep(seq_len(n_cols_dist), each = n)
      relabelled <- matrix(inv[cbind(rep(seq_len(n), ncol(d)), as.vector(d))], n)
      # base-t number, exact in a double as t^24 < 2^53
      best <- pmin(best, as.vector((relabelled - 1) %*% weights))
    }
  }

  return(best)
}

df_initial_dist <- speed::initialise_design_df(
  items = n_treatments_dist,
  nrows = n_rows_dist,
  ncols = n_cols_dist
)

# design data frame -> row-major treatment vector
as_design_vector <- function(df) {
  row <- as.integer(as.character(df$row))
  col <- as.integer(as.character(df$col))
  trt <- as.integer(as.factor(df$treatment))

  return(trt[order(row, col)])
}

# vector -> design data frame, for scoring with speed
as_design_df <- function(x) {
  df <- df_initial_dist[order(df_initial_dist$row, df_initial_dist$col), ]
  df$treatment <- factor(x)

  return(df)
}

is_optimal_dist <- function(df, tol = 1e-8) {
  adj <- speed::calculate_adjacency_score(df, "treatment")
  bal <- speed::calculate_balance_score(df, "treatment", c("row", "col"))
  bal_min <- speed:::.balance_score_min(df, "treatment", c("row", "col"))

  return(adj == 0 && bal - bal_min < tol)
}

bench_speed_dist <- function(seed = 112) {
  speed::speed(
    data = df_initial_dist,
    swap = "treatment",
    spatial_factors = ~ row + col,
    quiet = TRUE,
    seed = seed
  )
}

# one row per seed: seed, optimal flag and the design's plots p1..p24
run_speed_dist <- function(
  seeds = seq_len(10000),
  cores = parallel::detectCores(),
  csv_path = bench_out("distribution-speed.csv")
) {
  runs <- parallel::mclapply(
    seeds,
    function(seed) {
      df <- bench_speed_dist(seed)$design_df
      return(c(
        seed = seed,
        optimal = is_optimal_dist(df),
        stats::setNames(as_design_vector(df), paste0("p", seq_len(nrow(df))))
      ))
    },
    mc.cores = cores
  )
  out <- as.data.frame(do.call(rbind, runs))
  utils::write.csv(out, csv_path, row.names = FALSE)

  return(out)
}

# labelled designs as exact keys, for counting repeats
design_key <- function(designs) {
  weights <- n_treatments_dist^(rev(seq_len(ncol(designs))) - 1)

  return(as.vector((designs - 1) %*% weights))
}

# plot x treatment counts against 1/t
marginal_stat <- function(designs) {
  counts <- vapply(
    seq_len(n_treatments_dist),
    function(v) return(colSums(designs == v)),
    numeric(ncol(designs))
  )
  expected <- nrow(designs) / n_treatments_dist

  return(sum((counts - expected)^2 / expected))
}

# pairs of identical labelled designs
repeat_stat <- function(designs) {
  return(sum(choose(table(design_key(designs)), 2)))
}

# p-values of speed's optimal designs against uniform draws from the
# enumeration; the marginal and repeat nulls are simulated with `B` draws
test_dist <- function(speed_runs, enum = explore_dist(), B = 1000) {
  plots <- paste0("p", seq_len(n_rows_dist * n_cols_dist))
  optimal <- speed_runs$optimal == 1
  designs <- as.matrix(speed_runs[optimal, plots])
  n <- nrow(designs)
  n_enum <- nrow(enum$designs)

  class_sizes <- table(enum$keys)
  observed <- table(factor(canonical_key(designs), levels = names(class_sizes)))
  class_test <- stats::chisq.test(
    as.vector(observed),
    p = as.vector(class_sizes) / n_enum
  )

  set.seed(112)
  null <- replicate(B, {
    draw <- enum$designs[sample(n_enum, n, replace = TRUE), , drop = FALSE]
    c(marginal = marginal_stat(draw), repeats = repeat_stat(draw))
  })
  # add-one so a simulated p-value is never 0
  mc_p <- function(stat, null_stats, greater = TRUE) {
    extreme <- if (greater) null_stats >= stat else null_stats <= stat
    return((sum(extreme) + 1) / (length(null_stats) + 1))
  }
  marginal <- marginal_stat(designs)
  repeats <- repeat_stat(designs)

  return(data.frame(
    test = c("optimal_rate", "class", "marginal", "repeats"),
    statistic = c(mean(optimal), class_test$statistic, marginal, repeats),
    expected = c(1, class_test$parameter, mean(null["marginal", ]),
                 choose(n, 2) / n_enum),
    p_value = c(NA, class_test$p.value, mc_p(marginal, null["marginal", ]),
                mc_p(repeats, null["repeats", ]))
  ))
}

explore_dist <- function() {
  designs <- enumerate_dist()
  keys <- canonical_key(designs)
  class_sizes <- table(keys)
  cat(sprintf(
    "optimal designs: %d\nclasses: %d\nclass sizes: %s\n",
    nrow(designs),
    length(class_sizes),
    paste(names(table(class_sizes)), table(class_sizes), sep = "x", collapse = " ")
  ))

  # enumeration agrees with speed's own scores
  set.seed(112)
  checked <- sample(nrow(designs), min(200, nrow(designs)))
  stopifnot(all(vapply(
    checked,
    function(i) return(is_optimal_dist(as_design_df(designs[i, ]))),
    logical(1)
  )))

  return(invisible(list(designs = designs, keys = keys)))
}
