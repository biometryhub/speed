# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project overview

`speed` is an R package (CRAN-style, MIT) that produces spatially
efficient experimental designs by rearranging treatments via simulated
annealing. The score being minimised is a weighted sum of an **adjacency
score** (penalises like-treatment neighbours) and a **balance score**
(rewards even spread across spatial factors such as row/col/block).

Designs can be **simple** (one swap variable) or **hierarchical**
(e.g. split-plot, strip-plot, MET) - in the hierarchical case the
optimiser runs sequentially, one level at a time, with potentially
different parameters per level.

## Common commands

This is a standard R package; development uses `devtools` from an R
session.

R, `devtools`, and the test deps are installed inside the devcontainer,
**not** on the host. Run any R command through the
`devcontainer-exec-here` wrapper (alias for
`devcontainer exec --workspace-folder .`) - it executes inside the
container with the workspace mounted at `/workspaces/speed`:

``` sh
devcontainer-exec-here R -e "devtools::test()"
devcontainer-exec-here R -e "devtools::check()"
# Quiet, scriptable test summary (surfaces failing tests; use devtools::test, NOT
# pkgload::load_all + testthat::test_dir - that combo makes vdiffr mark _snaps/*.svg
# as orphaned and delete them):
devcontainer-exec-here Rscript -e '
  res <- devtools::test("/workspaces/speed", reporter = "silent")
  df  <- as.data.frame(res)
  cat(sprintf("pass=%d fail=%d warn=%d skip=%d\n",
              sum(df$passed), sum(df$failed), sum(df$warning), sum(df$skipped)))
  fails <- subset(df, failed > 0); if (nrow(fails)) print(fails[, c("file","test","failed")])
'
```

Inside an R session (e.g. `devcontainer-exec-here R`):

``` r

devtools::install_dev_deps()         # one-time: install Suggests + Imports
devtools::load_all()                 # load package for interactive iteration
devtools::document()                 # regenerate man/*.Rd and NAMESPACE from roxygen
devtools::test()                     # run all testthat tests
devtools::test(filter = "speed")     # run only tests/testthat/test-speed.R
testthat::test_file("tests/testthat/test-metrics.R")  # run a single file
devtools::check()                    # full R CMD check (must pass before PRs)
devtools::build_readme()             # regenerate README.md from README.Rmd
quarto::quarto_render("vignettes/speed.qmd")  # render a vignette (Quarto, not knitr)
```

`R CMD check` is also enforced in CI on macOS / Windows / Ubuntu against
R devel, release, and oldrel-1 (`.github/workflows/R-CMD-check.yaml`).

Code formatting is handled by [Air](https://posit-dev.github.io/air/) -
see `air.toml` (80-col, 2-space indent). Per `CONTRIBUTING.md`, do
**not** restyle code that is unrelated to your PR.

User-facing changes should add a bullet to the top of `NEWS.md`.

## Architecture

### Entry point and control flow

[`speed()`](https://biometryhub.github.io/speed/reference/speed.md)
(R/speed.R) is the only top-level optimisation entry point. Its job is
to validate inputs, normalise them into a per-level `optimise` list,
then delegate to
[`speed_hierarchical()`](https://biometryhub.github.io/speed/reference/speed_hierarchical.md)
which runs the simulated-annealing loop.

The `optimise` list is the canonical internal representation regardless
of whether the user passed a “simple” or “hierarchical” call. There are
three input shapes that all collapse into it:

1.  **Simple**: scalar `swap`, `swap_within`, etc. → wrapped into a
    single-level list.
2.  **Legacy hierarchical**: `swap = list(wp = ..., sp = ...)` and
    matching named lists for `swap_within` / `iterations` /
    `obj_function`. Detected by
    `is.list(swap) && !is.null(names(swap))`.
3.  **New `optimise = list(level1 = list(...), level2 = list(...))`**
    argument, which lets each level override any field including
    `spatial_factors` and `optimise_params`. See the MET example in
    [`?speed`](https://biometryhub.github.io/speed/reference/speed.md).

[`create_speed_input()`](https://biometryhub.github.io/speed/reference/create_speed_input.md)
(in R/design_utils.R) is what merges these into the per-level structure
used by the SA loop.

### The SA loop (`speed_hierarchical` in R/speed.R)

For each level in order:

1.  [`random_initialise()`](https://biometryhub.github.io/speed/reference/random_initialise.md)
    (R/design_utils.R) optionally shuffles the starting design
    `optim_params(random_initialisation = N)` times and keeps the best.
2.  Each iteration calls
    [`generate_neighbour()`](https://biometryhub.github.io/speed/reference/generate_neighbour.md)
    (R/design_utils.R) to swap treatments - single-swap or multi-swap
    depending on `swap_all`.
3.  The objective function (default `objective_function`, R/metrics.R)
    returns a `list(score = ..., ...)`. The returned list is fed back as
    `current_score_obj` on the next call so objective functions can
    incrementally update internal state from `swapped_items` instead of
    recomputing from scratch - **any custom objective function must
    honour this contract** (see `objective_function_signature` and the
    custom-objective vignette).
4.  Acceptance is the standard Metropolis criterion; temperature decays
    by `cooling_rate` each iteration.
5.  Early stop fires when
    `iter - last_improvement_iter >= early_stop_iterations` or when the
    score reaches `.Machine$double.eps`.

The result is an S3 object of class `"design"` with `print.design` and
`autoplot.design` methods. For single-level designs the `scores` /
`temperatures` / `stopped_early` fields are flat; for multi-level they
are lists keyed by level name - `print.design` and downstream code
branch on `is.list(x$treatments)`.

### Source layout (R/)

- **speed.R** -
  [`speed()`](https://biometryhub.github.io/speed/reference/speed.md),
  [`speed_hierarchical()`](https://biometryhub.github.io/speed/reference/speed_hierarchical.md),
  `print.design`.
- **metrics.R** - exported objective functions (`objective_function`,
  `objective_function_factorial`, `objective_function_piepho`) plus
  their building blocks (`calculate_adjacency_score`,
  `calculate_balance_score`, `calculate_ed`, `calculate_nb`,
  `calculate_efficiency_factor`, `get_vertices`, `get_edges`,
  `create_pair_mapping`).
- **design_utils.R** - neighbour generation, `infer_row_col`
  (auto-detects row/col columns from `grid_factors`),
  `initialise_design_df` (and `initialize_design_df` US-spelling alias),
  `random_initialise`, `create_speed_input`.
- **optim_params.R** -
  [`optim_params()`](https://biometryhub.github.io/speed/reference/optim_params.md)
  constructor for SA hyperparameters; reads legacy `options(speed.*)`
  for backwards compatibility and emits a deprecation warning if any are
  set.
- **options.R** - roxygen-only file documenting the deprecated `speed.*`
  options.
- **constants.R** - `.DEFAULT` list of fallback values for each level
  when fields are missing from `optimise`.
- **buffers.R** -
  [`add_buffers()`](https://biometryhub.github.io/speed/reference/add_buffers.md)
  adds edge/row/column/block buffer plots to a design.
- **plotting.R** - `autoplot.design` and `plot_progress`
  (ggplot2-based).
- **verify_utils.R** - internal `.verify_*` input-validation helpers;
  called from
  [`speed()`](https://biometryhub.github.io/speed/reference/speed.md)
  before any work begins.
- **utils.R** - small helpers: `to_factor` / `to_types` (round-trip
  column types because the SA loop requires factors), `pseudo_inverse`,
  `env_add_one`, etc.
- **zzz.R** - `.onAttach` checks GitHub for a newer version; wrapped in
  `tryCatch` and silently ignores network failures.
- **speed-package.R** - package doc and
  [`utils::globalVariables`](https://rdrr.io/r/utils/globalVariables.html)
  declarations for NSE columns.

### Conventions to preserve

- **British spelling in user-facing names** (`initialise_design_df`,
  `random_initialisation`, `optimise`). The few US-spelt aliases
  (`initialize_design_df`) exist only for back-compat - do not remove
  without a deprecation cycle.
- **roxygen2 with markdown** is the documentation source of truth -
  never edit `man/*.Rd` directly; run `devtools::document()` instead.
- **Vignettes are Quarto (`.qmd`)**, not Rmd - `DESCRIPTION` declares
  `VignetteBuilder: quarto`.
- **`# fmt: skip`** comments are intentional Air-formatter overrides;
  leave them in place when editing nearby code.
- The `dummy_<timestamp>` column inside
  [`speed()`](https://biometryhub.github.io/speed/reference/speed.md) is
  a temporary level used to represent “no `swap_within` boundary” (`"1"`
  / `"none"`); it is added before the SA loop and removed before
  returning.
