library(dae)
library(dplyr)
library(odw)
library(speed)
library(ggplot2)
library(patchwork)

bench_out_root <- "./bench-out"
dir.create(bench_out_root, showWarnings = FALSE, recursive = TRUE)

# every csv and png written by ./bench/ goes through here
bench_out <- function(...) {
  return(file.path(bench_out_root, ...))
}

efficiency <- function(design, treatment, units = ~ row * col) {
  design <- as.data.frame(design)
  cols <- unique(c(all.vars(units), treatment))
  design[cols] <- lapply(design[cols], factor)
  anatomy <- dae::designAnatomy(
    list(units = units, treatments = stats::reformulate(treatment)),
    data = design
  )
  return(summary(anatomy))
}

# odw needs factors
to_factor <- function(df, cols) {
  df[cols] <- lapply(df[cols], as.factor)
  return(df)
}

# plots beyond the ideal max of each treatment per level of `col`
count_dupes <- function(df, col, treatment = "treatment") {
  tab <- table(df[[treatment]], df[[col]])
  cap <- ceiling(colSums(tab) / nrow(tab))

  return(sum(pmax(sweep(tab, 2, cap), 0)))
}

get_metrics <- function(df, cols, skip_adj = FALSE) {
  metrics <- if (!skip_adj) {
    list(adjacency = speed::calculate_adjacency_score(df, "treatment"))
  } else {
    list()
  }
  for (col in cols) {
    metrics[[paste0(col, "_dupes")]] <- count_dupes(df, col)
  }
  for (name in names(metrics)) {
    cat(sprintf("%s: %s\n", name, metrics[[name]]))
  }

  return(invisible(metrics))
}

# aefficiency / eefficiency of the bottom line
bottom_stratum_eff <- function(anatomy_summary) {
  d <- anatomy_summary$decomp
  keep <- which(!is.na(d$Source.treatments) & d$Source.treatments != "Residual")
  if (!length(keep)) {
    return(list(stratum = NA_character_, aeff = NA_real_, eeff = NA_real_))
  }

  i <- keep[length(keep)]
  return(list(
    stratum = d$Source.units[i],
    aeff = d$aefficiency[i],
    eeff = d$eefficiency[i]
  ))
}

# tag a bare design data frame so speed::autoplot() accepts it
as_design <- function(df) {
  class(df) <- unique(c(class(df), "design"))
  return(df)
}

# lightened inferno, skipping its near-black end
bench_palette <- function(n) {
  return(colorspace::lighten(
    # to not use too dark and too light colors
    viridisLite::inferno(n, begin = 0.15, end = 0.95),
    0.6
  ))
}

scale_fill_bench <- function(values, ...) {
  if (is.numeric(values)) {
    return(scale_fill_gradientn(colours = bench_palette(256), ...))
  }

  lvls <- levels(as.factor(values))
  return(scale_fill_manual(
    values = stats::setNames(bench_palette(length(lvls)), lvls),
    ...
  ))
}

# speed::autoplot() with bench_palette()
autoplot_bench <- function(object, treatments = "treatment", ...) {
  df <- if (is.data.frame(object)) object else object$design_df
  ntrt <- length(setdiff(unique(as.character(df[[treatments]])), "buffer"))
  return(
    speed::autoplot(
      object,
      treatments = !!treatments,
      palette = bench_palette(ntrt),
      ...
    ) +
      theme(plot.title = element_text(hjust = 0.5))
  )
}

# plots with a like-treatment neighbour outside their own `unit`
flag_adjacent <- function(df, treatment, unit = NULL) {
  row <- as.integer(as.character(df$row))
  col <- as.integer(as.character(df$col))
  key <- paste(row, col)
  trt <- as.character(df[[treatment]])
  grp <- if (is.null(unit)) seq_len(nrow(df)) else as.character(df[[unit]])

  flags <- logical(nrow(df))
  for (offset in list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))) {
    j <- match(paste(row + offset[1], col + offset[2]), key)
    flags <- flags | (!is.na(j) & trt[j] == trt & grp[j] != grp)
  }

  return(flags)
}

# plots whose treatment exceeds its ideal count in a level of any of `factors`,
# counting each `unit` once
flag_dupes <- function(df, treatment, factors, unit = NULL) {
  units <- if (is.null(unit)) df else df[!duplicated(df[[unit]]), ]

  flags <- logical(nrow(df))
  for (col in factors) {
    tab <- table(units[[treatment]], units[[col]])
    over <- sweep(tab, 2, ceiling(colSums(tab) / nrow(tab))) > 0
    idx <- cbind(as.character(df[[treatment]]), as.character(df[[col]]))
    flags <- flags | over[idx]
  }

  return(flags)
}

# plots that are adjacent or off-balance in any of `factors`
flag_hl <- function(df, treatment, factors, unit = NULL) {
  return(
    flag_adjacent(df, treatment, unit) |
      flag_dupes(df, treatment, factors, unit)
  )
}

# dim tiles and labels of unflagged plots
hl_tiles <- function(plot, df, flags, dim_alpha = 0.38) {
  key <- paste(df$row, df$col)
  for (layer in plot$layers) {
    if (!inherits(layer$geom, c("GeomText", "GeomTile"))) {
      next
    }
    # layers without their own data inherit the plot's
    if (!is.data.frame(layer$data)) {
      layer$data <- plot$data
    }
    hl <- flags[match(paste(layer$data$row, layer$data$col), key)] %in% TRUE
    layer$aes_params$alpha <- ifelse(hl, 1, dim_alpha)
    if (inherits(layer$geom, "GeomText")) {
      layer$data$.fontface <- ifelse(hl, "bold", "plain")
      layer$mapping$fontface <- rlang::quo(.data$.fontface)
    }
  }

  return(plot + theme(panel.grid = element_blank()))
}

# autoplot_bench() dimming plots that are neither adjacent nor off-balance in
# any of `factors`
autoplot_hl <- function(df, factors, hl_dupes = TRUE) {
  plot <- autoplot_bench(df)
  if (hl_dupes) {
    design_df <- if (is.data.frame(df)) df else df$design_df
    plot <- hl_tiles(plot, design_df, flag_hl(design_df, "treatment", factors))
  }

  return(plot)
}

bench_objects_dir <- function() {
  return(bench_out("objects"))
}

object_path <- function(design_name, tool_name, seed) {
  return(file.path(
    bench_objects_dir(),
    sprintf("%s-%s-%s.RData", design_name, tool_name, seed)
  ))
}

# NULL on failure, so the caller still writes a row of NAs
execute_run <- function(run_tool, design_name, tool_name, seed) {
  return(tryCatch(
    {
      elapsed <- system.time(design_df <- run_tool(seed))[["elapsed"]]
      list(
        design = design_name,
        tool = tool_name,
        seed = seed,
        elapsed = elapsed,
        design_df = design_df
      )
    },
    error = function(e) {
      warning(sprintf(
        "%s/%s/seed=%s failed: %s",
        design_name,
        tool_name,
        seed,
        conditionMessage(e)
      ))
      NULL
    }
  ))
}

save_run <- function(run) {
  path <- object_path(run$design, run$tool, run$seed)
  save(run, file = path)

  return(path)
}

load_run <- function(design_name, tool_name, seed) {
  path <- object_path(design_name, tool_name, seed)
  if (!file.exists(path)) {
    warning(sprintf("object not found: %s", path))
    return(NULL)
  }

  env <- new.env()
  load(path, envir = env)

  return(env$run)
}

# One row of results for a run; a NULL run scores as NAs
score_run <- function(
  run,
  spec,
  design_name,
  tool_name,
  seed,
  skip_metrics = FALSE
) {
  if (skip_metrics) {
    return(data.frame(
      tool = tool_name,
      design = design_name,
      seed = seed,
      run_time = if (is.null(run)) NA_real_ else run$elapsed
    ))
  }

  metrics <- list(conv = NA, aeff = NA_real_, eeff = NA_real_)
  if (!is.null(run)) {
    metrics <- tryCatch(
      {
        eff <- efficiency(run$design_df, spec$treatment, spec$units) |>
          bottom_stratum_eff()
        list(
          conv = isTRUE(spec$is_converged(run$design_df)),
          aeff = eff$aeff,
          eeff = eff$eeff
        )
      },
      error = function(e) {
        warning(sprintf(
          "%s/%s/seed=%s metrics failed: %s",
          design_name,
          tool_name,
          seed,
          conditionMessage(e)
        ))
        list(conv = NA, aeff = NA_real_, eeff = NA_real_)
      }
    )
  }
  row <- data.frame(
    tool = tool_name,
    design = design_name,
    seed = seed,
    run_time = if (is.null(run)) NA_real_ else run$elapsed,
    is_converged = metrics$conv,
    aefficiency = metrics$aeff,
    eefficiency = metrics$eeff
  )

  # Design-specific custom columns, appended to the right
  if (!is.null(run) && is.function(spec$custom_metrics)) {
    custom <- tryCatch(
      as.data.frame(as.list(spec$custom_metrics(run$design_df))),
      error = function(e) {
        warning(sprintf(
          "%s/%s/seed=%s custom metrics failed: %s",
          design_name,
          tool_name,
          seed,
          conditionMessage(e)
        ))
        NULL
      }
    )
    if (!is.null(custom)) {
      row <- cbind(row, custom)
    }
  }

  return(row)
}

# Writes one CSV per design (`<csv_prefix>-<design>.csv`) and returns a named
# list of per-design result data frames. A design spec may supply
# `custom_metrics`, a function of the design data frame returning a named list
# of scalars; these are appended as the right-most columns of each bench run.
#
# `save_objects` runs the tools and saves each run to
# `<bench_objects_dir()>/<design>-<tool>-<seed>.RData` without scoring or
# writing a CSV; it returns the saved paths. `from_objects` scores the runs
# saved that way instead of running the tools, warning about missing ones.
#
# `skip_metrics` records only run times, skipping all efficiency and custom
# metrics.
#
# `tools` limits the run to the named tools.
run_benchmarks <- function(
  designs,
  seeds,
  csv_prefix = "benchmark",
  save_objects = FALSE,
  from_objects = FALSE,
  skip_metrics = FALSE,
  tools = c("speed", "odw", "digger")
) {
  if (save_objects && from_objects) {
    stop("`save_objects` and `from_objects` cannot both be TRUE.")
  }
  if (save_objects) {
    dir.create(bench_objects_dir(), showWarnings = FALSE, recursive = TRUE)
  }

  results <- list()
  saved <- character()
  for (design_name in names(designs)) {
    spec <- designs[[design_name]]
    csv_path <- bench_out(sprintf("%s-%s.csv", csv_prefix, design_name))
    rows <- list()
    design_results <- NULL
    for (tool_name in intersect(names(spec$tools), tools)) {
      run_tool <- spec$tools[[tool_name]]
      for (seed in seeds) {
        run <- if (from_objects) {
          load_run(design_name, tool_name, seed)
        } else {
          execute_run(run_tool, design_name, tool_name, seed)
        }

        if (save_objects) {
          if (!is.null(run)) {
            saved <- c(saved, save_run(run))
          }
          next
        }

        rows[[length(rows) + 1L]] <- score_run(
          run,
          spec,
          design_name,
          tool_name,
          seed,
          skip_metrics
        )
        # rewrite every row each run, cheap
        design_results <- dplyr::bind_rows(rows)
        utils::write.csv(design_results, csv_path, row.names = FALSE)
      }
    }
    if (!save_objects) {
      results[[design_name]] <- design_results
    }
  }

  if (save_objects) {
    return(invisible(saved))
  }

  return(results)
}

contrast_colour <- function(fills) {
  luminance <- apply(grDevices::col2rgb(fills) / 255, 2, function(channel) {
    linear <- ifelse(
      channel <= 0.03928,
      channel / 12.92,
      ((channel + 0.055) / 1.055)^2.4
    )
    return(sum(c(0.2126, 0.7152, 0.0722) * linear))
  })

  return(ifelse(luminance > 0.179, "black", "white"))
}

tile_colour <- function(values, na_colour = "grey") {
  if (is.numeric(values)) {
    colours <- scales::col_numeric(
      bench_palette(256),
      range(values, na.rm = TRUE)
    )(values)
  } else {
    values <- as.factor(values)
    colours <- bench_palette(nlevels(values))[as.integer(values)]
  }
  colours[is.na(colours)] <- na_colour

  return(colours)
}

plot_layout_irr <- function(df, fill, title = NULL) {
  scale_args <- list(df[[fill]], na.value = "grey")
  if (!is.numeric(df[[fill]])) {
    scale_args$drop <- FALSE
  }
  df$label_colour <- contrast_colour(
    tile_colour(df[[fill]], scale_args$na.value)
  )

  return(
    ggplot(df, aes(col, row, fill = get(fill))) +
      geom_tile(color = "black") +
      geom_text(aes(label = treatment, colour = label_colour), size = 5) +
      scale_colour_identity() +
      do.call(scale_fill_bench, scale_args) +
      scale_x_continuous(
        expand = c(0, 0),
        breaks = 1:max(df$col),
        position = "top"
      ) +
      scale_y_continuous(expand = c(0, 0), breaks = 1:max(df$row)) +
      labs(title = title) +
      theme_bw(base_size = 23) +
      theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  )
}

# `hl_dupes` dims plots that are neither adjacent nor off-balance
autoplot_irr <- function(df, hl_dupes = TRUE) {
  is_sw <- df$site == "sw"
  sw <- plot_layout_irr(df[is_sw, ], "block", "SW")
  se <- plot_layout_irr(df[!is_sw, ], "block", "SE")
  if (hl_dupes) {
    planted <- !is.na(df$block)
    flags <- logical(nrow(df))
    # adjacency per site, the two share (row, col) coordinates
    for (in_site in list(planted & is_sw, planted & !is_sw)) {
      flags[in_site] <- flag_adjacent(df[in_site, ], "treatment")
    }
    flags[planted] <- flags[planted] |
      flag_dupes(df[planted, ], "treatment", c("site_row", "site_col", "block"))
    sw <- hl_tiles(sw, df[is_sw, ], flags[is_sw])
    se <- hl_tiles(se, df[!is_sw, ], flags[!is_sw])
  }

  sw_x <- sw$scales$get_scales("x")
  sw_x$trans <- scales::reverse_trans()

  se_y <- se$scales$get_scales("y")
  se_y$position <- "right"
  return(sw + se + patchwork::plot_layout(ncol = 2))
}

highlight_plot <- function(
  df,
  fill,
  hl = NULL,
  label_hl = TRUE,
  label_non_hl = FALSE
) {
  # non-highlighted tiles fall back to the grey na.value
  df$col <- as.integer(df$col)
  df$row <- as.integer(df$row)
  df$.fill <- df[[fill]]
  labels <- NULL
  if (!is.null(hl)) {
    is_hl <- df[[fill]] %in% hl
    df$.fill[!is_hl] <- NA
    df$.label <- as.character(df[[fill]])
    labels <- list(
      if (label_non_hl) {
        geom_text(
          aes(label = .data$.label),
          data = df[!is_hl, ],
          size = 2,
          color = "black",
          na.rm = TRUE
        )
      },
      if (label_hl) {
        geom_text(
          aes(label = .data$.label),
          data = df[is_hl, ],
          size = 2,
          color = "black",
          na.rm = TRUE,
          fontface = "bold"
        )
      }
    )
  }

  return(
    ggplot(df, aes(col, row, fill = .data$.fill)) +
      geom_tile(color = "darkgray") +
      labels +
      scale_fill_bench(df[[fill]], na.value = "grey") +
      scale_x_continuous(expand = c(0, 0), breaks = 1:max(as.integer(df$col))) +
      scale_y_continuous(
        expand = c(0, 0),
        breaks = 1:max(as.integer(df$row)),
        trans = scales::reverse_trans()
      ) +
      theme_bw() +
      labs(fill = fill) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  )
}
