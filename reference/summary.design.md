# Summarise a speed design

Produces a richer, statistically meaningful evaluation of a design than
[print()](https://biometryhub.github.io/speed/reference/print.design.md).
Where [`print()`](https://rdrr.io/r/base/print.html) is a compact
output, [`summary()`](https://rdrr.io/r/base/summary.html) decomposes
the optimised score and reports structural and evaluation metrics that
let you interrogate and defend a design.

## Usage

``` r
# S3 method for class 'design'
summary(
  object,
  efficiency = FALSE,
  connectedness = NULL,
  concurrence = NULL,
  neighbour = NULL,
  ...
)
```

## Arguments

- object:

  A `"design"` object returned by
  [`speed()`](https://biometryhub.github.io/speed/reference/speed.md).

- efficiency:

  Logical (default `FALSE`); if `TRUE`, compute the A-efficiency factor.
  Returns `NA` with a reason when its assumptions are not met (columns
  named `row` and `col`, and at least 3 treatments). See Details for
  more information.

- connectedness:

  `NULL` (default) checks whether the design is connected, but skips the
  check for very large designs where the model fit would be expensive;
  `TRUE` forces it regardless of size; `FALSE` skips it. See Details for
  more information.

- concurrence:

  `NULL` (default) computes within-block treatment concurrence only when
  an *incomplete* block factor is present; `TRUE` forces it even for
  complete blocks, `FALSE` skips it. See Details for more information.

- neighbour:

  `NULL` (default) or `TRUE` reports neighbour-balance diagnostics
  whenever the design has a row/column grid; `FALSE` skips them. See
  Details for more information.

- ...:

  Unused; for S3 compatibility.

## Value

A list of class `"summary.design"`:

- **hierarchical** - `TRUE` for a multi-level (e.g. split-plot) design.

- **layout** - `n_plots`, `nrow`, `ncol`, `row_column`, `col_column`,
  `has_grid` (`TRUE` when the design is reportable as a single grid),
  and `grid_reason` (why not, or `NA`). `nrow`/`ncol` count the rows and
  columns the design *occupies*, so a design with a gap in its
  coordinates (a missing plot, or a removed buffer) reports fewer than
  the coordinates span. Both are `NA` unless `has_grid`.

- **levels** - character vector of level names (e.g. `"wp"`/`"sp"`; a
  single name for a simple design).

- **per_level** - one element per level (named by `levels`), each a list
  with:

  - `swap`, `n_treatments`, `treatments`.

  - `replication` - `counts`, `min`, `mean`, `max`, `equal`,
    `distribution`.

  - `spatial_factors` - named vector of the number of levels of each
    spatial factor.

  - `evaluation` - `replicate_span`, `connectedness`, `concurrence`,
    `block_spread`, `efficiency`, `neighbour`; each a list with an
    `available` flag and either its value(s) or a `reason` it wasn't
    computed.

  - `score` - `initial`, `final`, `components`.

  - `optim` - `objective`, `start_temp`, `cooling_rate`,
    `iterations_requested`, `iterations_run`, `stopped_early`.

- **score** - the overall optimised score (summed across levels for a
  hierarchical design).

- **seed** - the seed used for reproducibility.

- **flags** - `hit_iteration_cap`, `unequal_replication`,
  `disconnected`.

- **call** - the captured
  [`speed()`](https://biometryhub.github.io/speed/reference/speed.md)
  call.

## Details

The returned object is a list of class `"summary.design"`; it can be
assigned and queried programmatically (e.g.
`s <- summary(d); s$per_level[[1]]$score`). Printing it is handled by
[`print.summary.design()`](https://biometryhub.github.io/speed/reference/print.summary.design.md).

What the evaluation metrics report:

- **Connectedness:** whether every pairwise treatment difference is
  *estimable* (statistically distinguishable) once row, column and any
  block effects are accounted for. A disconnected design is usually not
  desirable as it confounds some comparisons with the layout itself, no
  matter how many replicates there are. Adjusts for the design's actual
  input spatial factors (row, column, and any block or site factor), not
  just columns literally named `row`/`col`.

- **Concurrence:** how many blocks each pair of treatments shares
  (`lambda`); equal lambda across all pairs indicates a balanced
  incomplete-block design. Uninformative for complete blocks such as
  RCBD/split-plot, where every pair always shares every block.

- **Replicate spread:** how many distinct blocks each treatment's
  replicates reach, and how many treatments land more than once in a
  single block. The block counterpart of replicate span: a block factor
  is nominal, so there is no distance to measure along, but the number
  of blocks a treatment reaches is still well defined. Reported whenever
  a block factor is present, including for complete blocks (where
  concurrence is skipped) - there it confirms every treatment reaches
  every block.

- **Replicate span:** how far apart a treatment's own replicates are
  placed along the two grid axes (whichever columns `grid_factors`
  resolved to, reported by name). For each replicated treatment, the
  closest its replicates come to each other, counted inclusively - so a
  span of 1 means two replicates share a row, 2 means they are in
  adjacent rows, and larger is better. The printed figure is the worst
  case across all replicated treatments; the two axes are minimised
  independently, so they may come from different treatments. Only the
  grid axes are measured: a span is a distance, and a non-grid spatial
  factor such as block or site has no ordering to measure along.

- **Efficiency:** the A-efficiency factor, a row–column model metric
  (see
  [`calculate_efficiency_factor()`](https://biometryhub.github.io/speed/reference/calculate_efficiency_factor.md));
  the heaviest of these metrics to compute, hence opt-in.

- **Neighbour balance:** how often treatment pairs end up side by side,
  using rook adjacency (left–right and up–down, not diagonal). Reported
  in two parts, because they mean opposite things. *Self-adjacency*
  counts treatments placed beside another plot of themselves; zero is
  the desirable outcome and is what the optimiser works towards. *Pair
  balance* covers the distinct treatment pairs: the min, max and
  variance of their adjacency counts, and how many never end up adjacent
  at all. Every possible pair is counted, so pairs that never neighbour
  register as zero rather than being omitted.

## See also

[`print.design()`](https://biometryhub.github.io/speed/reference/print.design.md),
[`speed()`](https://biometryhub.github.io/speed/reference/speed.md)

## Examples

``` r
df <- data.frame(
  row = rep(1:4, times = 3),
  col = rep(1:3, each = 4),
  treatment = rep(LETTERS[1:3], 4)
)
design <- speed(df, swap = "treatment", swap_within = "1",
                spatial_factors = ~ row + col, iterations = 100, seed = 1)
#> row and col are used as row and column, respectively.
#> Optimising level: single treatment within whole design 
summary(design)
#> Design Summary
#> ==============
#> 
#> Flags
#> -----
#> ! Ran to iteration cap - may not have converged
#> 
#> Structure
#> ---------
#> Layout:       4 rows x 3 cols (12 plots)
#> Treatments:   3
#> Replication:  4 each
#> Spatial:      row (4), col (3)
#> 
#> Optimisation
#> ------------
#> Seed:         1
#> Objective:    objective_function
#> Score:        1  (initial 1 -> final 1)
#>               adjacency  0
#>               balance    1
#> Iterations:   100 / 100 (ran to cap)
#> Temperature:  start 100, cooling 0.99
#> 
#> Evaluation
#> ----------
#> Connected:    connected - treatment estimable given row + col [model (row + col)]
#> Concurrence:  no block factor
#> Blk. spread:  no block factor
#> Repl. span:   worst-case 2 (row), 1 (col) across 3 replicated treatment(s)
#> Efficiency:   not requested (set efficiency = TRUE)
#> Self-adj.:    none
#> Neighbour:    min 5, max 6 over 3 pairs (variance 0.3333)

# Opt in to the (heavier) A-efficiency factor
summary(design, efficiency = TRUE)
#> Design Summary
#> ==============
#> 
#> Flags
#> -----
#> ! Ran to iteration cap - may not have converged
#> 
#> Structure
#> ---------
#> Layout:       4 rows x 3 cols (12 plots)
#> Treatments:   3
#> Replication:  4 each
#> Spatial:      row (4), col (3)
#> 
#> Optimisation
#> ------------
#> Seed:         1
#> Objective:    objective_function
#> Score:        1  (initial 1 -> final 1)
#>               adjacency  0
#>               balance    1
#> Iterations:   100 / 100 (ran to cap)
#> Temperature:  start 100, cooling 0.99
#> 
#> Evaluation
#> ----------
#> Connected:    connected - treatment estimable given row + col [model (row + col)]
#> Concurrence:  no block factor
#> Blk. spread:  no block factor
#> Repl. span:   worst-case 2 (row), 1 (col) across 3 replicated treatment(s)
#> Efficiency:   0.9375 (A-efficiency, row-column model)
#> Self-adj.:    none
#> Neighbour:    min 5, max 6 over 3 pairs (variance 0.3333)
```
