# Plot helpers shared by the design scripts.

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

autoplot_split <- function(df) {
  whole <- speed::autoplot(df, treatments = "wholeplot_treatment")
  sub <- speed::autoplot(
    df,
    treatments = "subplot_treatment",
    block = "wholeplot"
  )
  return(whole + sub + patchwork::plot_layout(ncol = 2))
}

# One facet per metric found in the csv written by run_benchmarks().
plot_comparison <- function(
  csv_path,
  title,
  subtitle = "10 seeds per tool; points are individual runs"
) {
  results <- utils::read.csv(csv_path)
  metrics <- c(
    run_time = "Run time (s) - lower better",
    aefficiency = "A-efficiency - higher better",
    eefficiency = "E-efficiency - higher better",
    adjacency = "Adjacency - lower better"
  )
  metrics <- metrics[names(metrics) %in% names(results)]

  long <- do.call(
    rbind,
    lapply(names(metrics), function(m) {
      data.frame(
        tool = results$tool,
        metric = unname(metrics[m]),
        value = results[[m]]
      )
    })
  )
  long$tool <- factor(long$tool, levels = c("speed", "digger", "odw"))
  long$metric <- factor(long$metric, levels = unname(metrics))

  return(
    ggplot(long, aes(tool, value, fill = tool)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.55, width = 0.6) +
      geom_jitter(width = 0.12, height = 0, size = 1.6, alpha = 0.8) +
      facet_wrap(~metric, scales = "free_y", nrow = 2) +
      scale_fill_brewer(palette = "Set2", guide = "none") +
      labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
      theme_bw(base_size = 23) +
      theme(strip.text = element_text(face = "bold"))
  )
}
