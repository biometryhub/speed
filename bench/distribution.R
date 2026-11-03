# Distribution of optimal designs: t treatments in t rows x `n_cols` columns,
# swapped within columns. Optimal = no like-treatment neighbours, every column
# holds all t treatments and every row is as even as possible.
n_cores <- parallel::detectCores()

# layout of t treatments, t rows (each column a complete replicate) and
# `n_cols` columns
dist_spec <- function(n_treatments = 4, n_cols = 5) {
  return(list(
    name = sprintf("%dx%d", n_treatments, n_cols),
    n_treatments = n_treatments,
    n_cols = n_cols,
    df_initial = speed::initialise_design_df(
      items = n_treatments,
      nrows = n_treatments,
      ncols = n_cols
    )
  ))
}

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

# row x treatment one-hot of each column of `cols`
column_onehot <- function(cols) {
  n <- ncol(cols)

  return(t(apply(cols, 1, function(r) {
    x <- integer(n * n)
    x[(seq_len(n) - 1) * n + r] <- 1L
    return(x)
  })))
}

# every optimal design as a row of treatments, plots in column-major order
enumerate_dist <- function(spec) {
  cols <- permutations(spec$n_treatments)
  onehot <- column_onehot(cols)
  # columns sharing no row entry can sit side by side without like neighbours
  compat <- tcrossprod(onehot) == 0
  cap <- ceiling(spec$n_cols / spec$n_treatments)

  seqs <- matrix(seq_len(nrow(cols)), ncol = 1)
  counts <- onehot
  for (k in seq_len(spec$n_cols)[-1]) {
    nxt <- which(compat[seqs[, k - 1], , drop = FALSE], arr.ind = TRUE)
    new_counts <- counts[nxt[, 1], , drop = FALSE] +
      onehot[nxt[, 2], , drop = FALSE]
    keep <- rowSums(new_counts > cap) == 0
    seqs <- cbind(seqs[nxt[keep, 1], , drop = FALSE], nxt[keep, 2])
    counts <- new_counts[keep, , drop = FALSE]
  }
  # most even split also leaves no treatment absent from a row
  keep <- rowSums(counts < floor(spec$n_cols / spec$n_treatments)) == 0
  seqs <- seqs[keep, , drop = FALSE]

  return(matrix(t(cols[t(seqs), ]), nrow(seqs), byrow = TRUE))
}

# number of balance-only designs (complete columns, most even rows, like
# neighbours allowed), counted over row x treatment states as there are too
# many to enumerate
count_balanced_dist <- function(spec) {
  onehot <- column_onehot(permutations(spec$n_treatments))
  cap <- ceiling(spec$n_cols / spec$n_treatments)

  states <- unique(onehot)
  ways <- rep(1, nrow(states))
  for (k in seq_len(spec$n_cols)[-1]) {
    nxt <- states[rep(seq_len(nrow(states)), each = nrow(onehot)), ] +
      onehot[rep(seq_len(nrow(onehot)), nrow(states)), ]
    nxt_ways <- rep(ways, each = nrow(onehot))
    keep <- rowSums(nxt > cap) == 0
    key <- apply(nxt[keep, , drop = FALSE], 1, paste, collapse = "")
    ways <- as.vector(tapply(nxt_ways[keep], key, sum))
    states <- nxt[keep, , drop = FALSE][match(sort(unique(key)), key), ]
  }
  keep <- rowSums(states < floor(spec$n_cols / spec$n_treatments)) == 0

  return(sum(ways[keep]))
}

# labelled designs as exact keys, for counting repeats; base-t number, exact
# in a double while t^plots < 2^53
design_key <- function(designs, n_treatments) {
  weights <- n_treatments^(rev(seq_len(ncol(designs))) - 1)

  return(as.vector((designs - 1) %*% weights))
}

# design data frame -> column-major treatment vector
as_design_vector <- function(df) {
  row <- as.integer(as.character(df$row))
  col <- as.integer(as.character(df$col))
  trt <- as.integer(as.factor(df$treatment))

  return(trt[order(col, row)])
}

# vector -> design data frame, for scoring with speed
as_design_df <- function(x, spec) {
  df <- spec$df_initial[order(spec$df_initial$col, spec$df_initial$row), ]
  df$treatment <- factor(x)

  return(df)
}

is_optimal_dist <- function(df, tol = 1e-8) {
  adj <- speed::calculate_adjacency_score(df, "treatment")
  bal <- speed::calculate_balance_score(df, "treatment", c("row", "col"))
  bal_min <- speed:::.balance_score_min(df, "treatment", c("row", "col"))

  return(adj == 0 && bal - bal_min < tol)
}

bench_speed_dist <- function(
  spec,
  seed = 112,
  optimise_params = speed::optim_params()
) {
  speed::speed(
    data = spec$df_initial,
    swap = "treatment",
    swap_within = "col",
    spatial_factors = ~row,
    optimise_params = optimise_params,
    quiet = TRUE,
    seed = seed
  )
}

# digger: each column a replicate, rows as blocks
bench_digger_dist <- function(spec, seed = 112) {
  DiGGer::ibDiGGer(
    numberOfTreatments = spec$n_treatments,
    rowsInDesign = spec$n_treatments,
    columnsInDesign = spec$n_cols,
    rowsInReplicate = spec$n_treatments,
    columnsInReplicate = 1,
    rowsInBlock = 1,
    columnsInBlock = spec$n_cols,
    # same designs as the default 1e5 on 4x5, about 20x faster
    maxInterchanges = 1000,
    # DiGGer caps its two seeds at 31328 and 30081; this keeps seeds distinct
    rngSeeds = c(seed %% 31329, seed %/% 31329)
  )
}

digger_dist_df <- function(spec, seed = 112) {
  df <- spec$df_initial
  df$treatment <- c(DiGGer::getDesign(bench_digger_dist(spec, seed)))

  return(df)
}

# odw, with the row variance started high
bench_odw_dist <- function(spec) {
  data <- to_factor(spec$df_initial, c("treatment", "row", "col"))
  odw_args <- list(
    random = ~ treatment + row,
    data = data,
    permute = ~treatment,
    swap = ~col,
    search = "tabu+rw"
  )
  params <- do.call(odw::odw, c(odw_args, start.values = TRUE))$vparameters.table
  params[params[[1]] %in% "row", 2] <- 100

  return(do.call(
    odw::odw,
    c(odw_args, list(G.param = params, R.param = params, maxit = 3))
  ))
}

# tool name -> function of a seed returning a design data frame
dist_tools <- function(spec) {
  speed_tool <- function(...) {
    params <- speed::optim_params(...)
    return(function(seed) {
      return(bench_speed_dist(spec, seed, params)$design_df)
    })
  }

  return(list(
    speed = speed_tool(),
    speed_ts0 = speed_tool(target_swaps = 0),
    speed_ri1 = speed_tool(random_initialisation = 1),
    speed_ts0_ri1 = speed_tool(target_swaps = 0, random_initialisation = 1),
    digger = function(seed) return(digger_dist_df(spec, seed)),
    odw = function(seed) return(bench_odw_dist(spec)$design)
  ))
}

# one row per seed: seed, optimal flag and the design's plots p1..pN in
# column-major order
run_dist <- function(
  tool,
  spec,
  seeds = seq_len(10000),
  cores = n_cores,
  csv_path = bench_out(sprintf("distribution-%s-%s.csv", spec$name, tool))
) {
  run_tool <- dist_tools(spec)[[tool]]
  runs <- parallel::mclapply(
    seeds,
    function(seed) {
      df <- run_tool(seed)
      return(c(
        seed = seed,
        optimal = is_optimal_dist(df),
        stats::setNames(as_design_vector(df), paste0("p", seq_len(nrow(df))))
      ))
    },
    mc.cores = cores
  )
  failed <- vapply(runs, inherits, logical(1), "try-error")
  if (any(failed)) {
    stop(sprintf(
      "%s failed on %d seeds, first: %s",
      tool,
      sum(failed),
      runs[[which(failed)[1]]]
    ))
  }
  out <- as.data.frame(do.call(rbind, runs))
  utils::write.csv(out, csv_path, row.names = FALSE)

  return(out)
}

# plot x treatment counts against 1/t
marginal_stat <- function(designs, n_treatments) {
  counts <- vapply(
    seq_len(n_treatments),
    function(v) return(colSums(designs == v)),
    numeric(ncol(designs))
  )
  expected <- nrow(designs) / n_treatments

  return(sum((counts - expected)^2 / expected))
}

# pairs of identical labelled designs
repeat_stat <- function(designs, n_treatments) {
  return(sum(choose(table(design_key(designs, n_treatments)), 2)))
}

# columns p1..pN of `runs` as a design matrix, optionally optimal rows only
run_designs <- function(runs, optimal_only = TRUE) {
  if (optimal_only) {
    runs <- runs[runs$optimal == 1, ]
  }

  return(as.matrix(runs[, grep("^p[0-9]+$", names(runs))]))
}

# p-values of a tool's optimal designs (from `run_dist()`) against uniform draws
# from the enumeration; the marginal and repeat nulls are simulated with `B` draws
test_dist <- function(runs, enum, B = 1000) {
  t <- enum$spec$n_treatments
  designs <- run_designs(runs)
  n <- nrow(designs)
  n_enum <- nrow(enum$designs)
  # nothing optimal to compare
  if (n == 0) {
    return(data.frame(
      test = "optimal_rate",
      statistic = 0,
      expected = 1,
      p_value = NA
    ))
  }

  set.seed(112)
  # hits per optimal design vs equal hits; expected counts can be tiny, so the
  # p-value is simulated
  observed <- tabulate(match(design_key(designs, t), enum$keys), n_enum)
  design_test <- stats::chisq.test(observed, simulate.p.value = TRUE, B = B)

  null <- replicate(B, {
    draw <- enum$designs[sample(n_enum, n, replace = TRUE), , drop = FALSE]
    c(marginal = marginal_stat(draw, t), repeats = repeat_stat(draw, t))
  })
  # add-one so a simulated p-value is never 0
  mc_p <- function(stat, null_stats) {
    return((sum(null_stats >= stat) + 1) / (length(null_stats) + 1))
  }
  marginal <- marginal_stat(designs, t)
  repeats <- repeat_stat(designs, t)

  return(data.frame(
    test = c("optimal_rate", "design", "marginal", "repeats"),
    statistic = c(mean(runs$optimal), design_test$statistic, marginal, repeats),
    expected = c(
      1,
      n_enum - 1,
      mean(null["marginal", ]),
      choose(n, 2) / n_enum
    ),
    p_value = c(
      NA,
      design_test$p.value,
      mc_p(marginal, null["marginal", ]),
      mc_p(repeats, null["repeats", ])
    )
  ))
}

# caption lines for the speed settings named in `tools`
settings_caption <- function(tools) {
  settings <- c(
    ts0 = "ts0: target_swaps = 0",
    ri1 = "ri1: random_initialisation = 1"
  )
  used <- vapply(
    names(settings),
    function(s) return(any(grepl(s, tools))),
    logical(1)
  )
  if (!any(used)) {
    return(NULL)
  }

  return(paste(settings[used], collapse = "; "))
}

scale_dist <- function(...) {
  return(scale_colour_viridis_d(option = "inferno", end = 0.85, ...))
}

# % of speed-optimal designs hit k times per tool and for uniform random draws
# from the enumeration, against the Poisson expectation under uniform sampling;
# `max_hits` defaults to the 99.9th percentile of the largest expectation;
# `lines = FALSE` drops the observed lines, for layouts with few designs
plot_hits <- function(
  runs,
  enum,
  seed = 112,
  max_hits = NULL,
  B = 2000,
  lines = TRUE,
  alpha = 0.5
) {
  t <- enum$spec$n_treatments
  n_enum <- nrow(enum$designs)

  keys <- lapply(runs, function(r) return(design_key(run_designs(r), t)))
  keys <- keys[lengths(keys) > 0]
  set.seed(seed)
  n_random <- max(lengths(keys))
  keys$random <- enum$keys[sample(n_enum, n_random, replace = TRUE)]
  if (is.null(max_hits)) {
    max_hits <- stats::qpois(0.999, n_random / n_enum)
  }

  k <- 0:max_hits
  k_labels <- c(k[-length(k)], paste0(max_hits, "+"))
  hits <- do.call(
    rbind,
    lapply(names(keys), function(tool) {
      n <- length(keys[[tool]])
      observed <- tabulate(match(keys[[tool]], enum$keys), n_enum)
      # expected counts can be tiny, so the p-value is simulated
      test <- stats::chisq.test(observed, simulate.p.value = TRUE, B = B)
      p_value <- format.pval(test$p.value, digits = 2)
      lambda <- n / n_enum
      expected <- c(
        stats::dpois(k[-length(k)], lambda),
        stats::ppois(max_hits - 1, lambda, lower.tail = FALSE)
      )
      label <- sprintf(
        "%s\nn = %d, chi² = %.0f, p %s",
        tool,
        n,
        test$statistic,
        if (startsWith(p_value, "<")) p_value else paste("=", p_value)
      )
      return(data.frame(
        tool = label,
        hits = k,
        observed = 100 *
          tabulate(pmin(observed, max_hits) + 1, max_hits + 1) /
          n_enum,
        expected = 100 * expected
      ))
    })
  )
  hits$tool <- factor(hits$tool, levels = unique(hits$tool))
  caption <- paste(
    c(
      "Expected (uniform): Poisson under uniform sampling of optimal designs, at each tool's n.",
      "chi²: hits per design vs equal hits across all optimal designs.",
      settings_caption(names(runs))
    ),
    collapse = "\n"
  )

  # about a dozen labelled ticks however many hit counts there are
  step <- ceiling(length(k) / 12)
  breaks <- c(k[k %% step == 0 & k <= max_hits - step], max_hits)

  return(
    ggplot(hits, aes(hits, group = tool)) +
      geom_line(
        aes(y = expected, linetype = "expected (uniform)"),
        colour = "grey40",
        linewidth = 0.25,
        alpha = alpha
      ) +
      (if (lines) {
        geom_line(
          aes(y = observed, colour = tool),
          linewidth = 0.25,
          alpha = alpha
        )
      }) +
      geom_point(
        aes(y = observed, colour = tool, shape = tool),
        size = 2,
        alpha = alpha
      ) +
      scale_x_continuous(breaks = breaks, labels = k_labels[breaks + 1]) +
      scale_dist() +
      scale_shape_manual(values = c(16, 17, 15, 18, 8, 4, 3)) +
      scale_linetype_manual(values = c(`expected (uniform)` = "dashed")) +
      guides(
        colour = guide_legend(order = 1, override.aes = list(alpha = 1)),
        shape = guide_legend(order = 1),
        linetype = guide_legend(order = 2)
      ) +
      labs(
        title = "Speed-optimal designs",
        x = "Hits per design",
        y = "Designs (%)",
        caption = caption,
        colour = NULL,
        shape = NULL,
        linetype = NULL
      ) +
      theme_bw() +
      theme(
        plot.caption = element_text(hjust = 0),
        panel.grid.minor.x = element_blank(),
        legend.key.spacing.y = unit(4, "pt")
      )
  )
}

# distinct designs found as runs accumulate, against uniform sampling of the
# speed-optimal designs and of balance-only designs
plot_coverage <- function(runs, enum) {
  t <- enum$spec$n_treatments
  n_enum <- nrow(enum$designs)
  n_balanced <- count_balanced_dist(enum$spec)

  coverage <- do.call(
    rbind,
    lapply(names(runs), function(tool) {
      keys <- design_key(run_designs(runs[[tool]], optimal_only = FALSE), t)
      return(data.frame(
        tool = tool,
        runs = seq_along(keys),
        distinct = cumsum(!duplicated(keys))
      ))
    })
  )
  coverage$tool <- factor(coverage$tool, levels = names(runs))
  n_runs <- seq_len(max(coverage$runs))
  ref_labels <- c(
    sprintf("uniform random (speed-optimal, %d)", n_enum),
    sprintf("uniform random (balance only, %d)", n_balanced)
  )
  uniform <- function(n) return(n * (1 - (1 - 1 / n)^n_runs))
  reference <- data.frame(
    ref = factor(rep(ref_labels, each = length(n_runs)), levels = ref_labels),
    runs = n_runs,
    distinct = c(uniform(n_enum), uniform(n_balanced))
  )

  return(
    ggplot(coverage, aes(runs, distinct, colour = tool)) +
      geom_line(
        data = reference,
        aes(runs, distinct, linetype = ref),
        colour = "grey40",
        linewidth = 0.25,
        alpha = 0.5,
        inherit.aes = FALSE
      ) +
      geom_line(linewidth = 0.25, alpha = 0.5) +
      scale_dist() +
      scale_linetype_manual(values = c("dashed", "dotted")) +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) +
      coord_cartesian(ylim = c(0, max(coverage$distinct))) +
      labs(
        title = "All designs",
        x = "Runs",
        y = "Distinct designs",
        caption = settings_caption(names(runs)),
        colour = NULL,
        linetype = NULL
      ) +
      theme_bw() +
      theme(plot.caption = element_text(hjust = 0))
  )
}

# optimal designs of `spec`, checked against speed's own scores
explore_dist <- function(spec) {
  designs <- enumerate_dist(spec)
  cat(sprintf("%s optimal designs: %d\n", spec$name, nrow(designs)))

  set.seed(112)
  checked <- sample(nrow(designs), min(200, nrow(designs)))
  stopifnot(all(vapply(
    checked,
    function(i) return(is_optimal_dist(as_design_df(designs[i, ], spec))),
    logical(1)
  )))

  return(invisible(list(
    spec = spec,
    designs = designs,
    keys = design_key(designs, spec$n_treatments)
  )))
}
