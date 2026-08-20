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

get_metrics <- function(df, cols, skip_adj = FALSE) {
  metrics <- if (!skip_adj) {
    list(adjacency = speed::calculate_adjacency_score(df, "treatment"))
  } else {
    list()
  }
  for (col in cols) {
    uniques <- unique(table(df$treatment, df[[col]]))
    name <- paste0(col, "_unique_occurence")
    metrics[[name]] <- paste(uniques, collapse = ",")
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

# Writes one CSV per design (`<csv_prefix>-<design>.csv`) and returns a named
# list of per-design result data frames. A design spec may supply
# `custom_metrics`, a function of the design data frame returning a named list
# of scalars; these are appended as the right-most columns of each bench run.
run_benchmarks <- function(designs, seeds, csv_prefix = "benchmark") {
  results <- list()
  for (design_name in names(designs)) {
    spec <- designs[[design_name]]
    csv_path <- bench_out(sprintf("%s-%s.csv", csv_prefix, design_name))
    rows <- list()
    for (tool_name in names(spec$tools)) {
      run_tool <- spec$tools[[tool_name]]
      for (seed in seeds) {
        run <- tryCatch(
          {
            elapsed <- system.time(design_df <- run_tool(seed))[["elapsed"]]
            list(elapsed = elapsed, design_df = design_df)
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
        )
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

        rows[[length(rows) + 1L]] <- row
        # rewrite every row each run, cheap
        design_results <- dplyr::bind_rows(rows)
        utils::write.csv(design_results, csv_path, row.names = FALSE)
      }
    }
    results[[design_name]] <- design_results
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
      viridisLite::viridis(256),
      range(values, na.rm = TRUE)
    )(values)
  } else {
    values <- as.factor(values)
    colours <- viridisLite::viridis(nlevels(values))[as.integer(values)]
  }
  colours[is.na(colours)] <- na_colour

  return(colours)
}

plot_layout_irr <- function(df, fill, title = NULL) {
  scale_args <- list(na.value = "grey")
  if (is.numeric(df[[fill]])) {
    scale_fill <- scale_fill_viridis_c
  } else {
    scale_fill <- scale_fill_viridis_d
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
      do.call(scale_fill, scale_args) +
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

autoplot_irr <- function(df) {
  sw <- plot_layout_irr(df[df$site == "sw", ], "block", "SW")
  sw_x <- sw$scales$get_scales("x")
  sw_x$trans <- scales::reverse_trans()

  se <- plot_layout_irr(df[df$site == "se", ], "block", "SE")
  se_y <- se$scales$get_scales("y")
  se_y$position <- "right"
  return(sw + se + patchwork::plot_layout(ncol = 2))
}
