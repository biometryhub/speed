# Optimise Experimental Design Layout Using Simulated Annealing

Optimises the spatial layout of experimental designs using simulated
annealing to minimise treatment adjacency and maintain treatment balance
across spatial factors. Works with regular or irregular spatial designs.

## Usage

``` r
speed(
  data,
  swap,
  swap_within = "1",
  spatial_factors = ~row + col,
  grid_factors = list(dim1 = "row", dim2 = "col"),
  iterations = 10000,
  early_stop_iterations = 2000,
  obj_function = objective_function,
  swap_all = FALSE,
  linked_cols = NULL,
  optimise = NULL,
  optimise_params = optim_params(),
  quiet = FALSE,
  seed = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing the experimental design with spatial
  coordinates

- swap:

  A column name of the items to be swapped (e.g., `treatment`,
  `variety`, `genotype`, etc). For hierarchical designs, provide a named
  list where each name corresponds to a hierarchy level (e.g.,
  `list(wp = "wholeplot_treatment", sp = "subplot_treatment")`). See
  details for more information.

- swap_within:

  A string specifying the variable that defines a boundary within which
  to swap treatments. Specify `"1"` or `"none"` for no boundary
  (default: `"1"`). Other examples might be `"block"` or `"replicate"`
  or even `"site"`. For hierarchical designs, provide a named list with
  names matching `swap` to optimise a hierarchical design such as a
  split-plot. See details for more information.

- spatial_factors:

  A one-sided formula specifying spatial factors to consider for balance
  (default: `~row + col`).

- grid_factors:

  A named list specifying grid factors to construct a matrix for
  calculating adjacency score, `dim1` for row and `dim2` for column.
  (default: `list(dim1 = "row", dim2 = "col")`). The axes apply to the
  whole design, so unlike `swap` they cannot be set per level of a
  hierarchical design.

  An optional third element, `by`, names a column that groups plots into
  *separate* grids - a multi-environment trial, where each site reuses
  the same `row`/`col` numbering. Each grid is then scored on its own
  and the adjacency counts summed, so no adjacency is counted between
  plots at different sites, e.g.
  `list(dim1 = "row", dim2 = "col", by = "site")`. Without it, a design
  whose sites share coordinates is refused rather than silently pooled.

- iterations:

  Maximum number of iterations for the simulated annealing algorithm
  (default: 10000). For hierarchical designs, can be a named list with
  names matching `swap`.

- early_stop_iterations:

  Number of iterations without improvement before early stopping
  (default: 2000). For hierarchical designs, can be a named list with
  names matching `swap`. Optimisation also stops as soon as a level
  reaches the lowest score its layout allows, which is only applicable
  for the default
  [`objective_function()`](https://biometryhub.github.io/speed/reference/objective_functions.md);
  see
  [summary()](https://biometryhub.github.io/speed/reference/summary.design.md).

- obj_function:

  Objective function used to calculate score (lower is better) (default:
  [`objective_function()`](https://biometryhub.github.io/speed/reference/objective_functions.md)).
  For hierarchical designs, can be a named list with names matching
  `swap`.

- swap_all:

  Logical; Whether to swap all matching items or a single item at a time
  (default: FALSE)

- linked_cols:

  Character vector of column names that travel with the `swap` column,
  for example a `variety_name` label belonging to a numeric `variety`
  code (default: `NULL`). For hierarchical designs, can be a named list
  with names matching `swap`. See details for more information.

- optimise:

  A list of named arguments describing optimising parameters; see more
  in example.

- optimise_params:

  Parameters used to control the behaviour of simulated annealing
  algorithm. See
  [`optim_params()`](https://biometryhub.github.io/speed/reference/optim_params.md)
  for more details.

- quiet:

  Logical; if TRUE, suppresses progress messages (default: FALSE)

- seed:

  A numeric value for random seed. If provided, it ensures
  reproducibility of results (default: `NULL`).

- ...:

  Other arguments passed to objective functions. For hierarchical
  designs, set an `obj_args` field in `optimise`. See details for more
  information.

## Value

A list containing:

- **design_df** - Data frame of optimised design

- **score** - Final optimisation score

- **scores** - Vector of scores across iterations (for simple designs)
  or named list of score vectors (for hierarchical designs)

- **temperatures** - Vector of temperatures across iterations

- **iterations_run** - Total number of iterations performed

- **stopped_early** - Logical indicating if optimisation stopped early
  (for simple designs) or named logical vector (for hierarchical
  designs)

- **treatments** - Vector of unique treatments (for simple designs) or
  named list of treatment vectors (for hierarchical designs)

- **seed** - Random seed used for reproducibility of the design. If not
  set in the function, the seed is set to the third element of
  `.Random.seed`, or drawn at random if the RNG has not been used yet.

- **metadata** - A list describing how the design was produced: the
  captured `call`, the ordered `levels`, the resolved `row_column` /
  `col_column` names, and a `per_level` list recording each level's swap
  variable, spatial factors, adjacency/balance weights, requested
  iterations, starting temperature, cooling rate, objective function,
  achieved score and the `stop_reason` that ended the level. Used by
  [summary()](https://biometryhub.github.io/speed/reference/summary.design.md)
  to recompute per-level evaluation metrics.

## Details

This function provides a very general interface for producing
experimental designs of different types. For hierarchical designs such
as split-plots, strip plots, split-split plots and similar nested
structures, the key arguments (`swap`, `swap_within`, `iterations`,
`early_stop_iterations` and `obj_function`) can be provided as named
lists where each name corresponds to a level in the hierarchy (e.g.,
"wholeplot", "subplot"). When these arguments are provided as named
lists with matching names, the optimisation is applied sequentially at
each hierarchical level, starting with the first level and progressing
through the hierarchy. This allows for different optimisation parameters
and objective functions to be applied at different levels of the design
structure. For simple (non-hierarchical) designs, these arguments can be
provided as single values. For more examples and detailed usage, see the
package vignettes.

`linked_cols` names columns that describe the treatment rather than the
plot, so that a value paired with a treatment stays paired with it
wherever the search moves it. They take no part in scoring, and are
returned in their input class and position. Columns used as
`swap_within`, spatial factors or grid factors cannot be linked, since
they describe where a plot is rather than what is on it.

A named list links different columns at different levels, e.g.
`list(wp = "wholeplot_label", sp = "subplot_label")`. A bare character
vector applies to every level, which in a hierarchical design is only
valid when every level swaps the same column, as in a multi-environment
trial - a column can only travel with one `swap` column.

A later level's `swap` column may be linked to an earlier one, which
carries a child treatment along when its parent moves: linking `sp_trt`
at the whole-plot level of a split-plot moves each sub-plot treatment
with its whole-plot treatment, before the sub-plot level then optimises
it. The carrying level must come first, or it would undo the child
level's work.

On a level with `swap_all = TRUE` whole treatment groups move at once,
so a linked column with more than one value per treatment travels with
its treatment group rather than being paired plot for plot.

Arguments for the objective function, such as `adj_weight`, `bal_weight`
or `relationship`, can be passed with `...`, or set per level with an
`obj_args` field of a level in `optimise`, e.g.,
`optimise = list(wp = list(..., obj_args = list(adj_weight = 0)))`. A
level's `obj_args` override `...` argument by argument. Setting
`adj_weight` and `bal_weight` through
[`optim_params()`](https://biometryhub.github.io/speed/reference/optim_params.md)
is deprecated.

## Examples

``` r
# Create a simple design with 3 replicates of 4 treatments in a 4x3 layout
df <- data.frame(
  row = rep(1:4, times = 5),
  col = rep(1:5, each = 4),
  treatment = rep(LETTERS[1:4], 5)
)

# Optimise the design
result <- speed(df, swap = "treatment", seed = 42)
#> row and col are used as row and column, respectively.
#> Optimising level: single treatment within whole design 
#> Optimal score reached at iteration 726 for level single treatment within whole design 
autoplot(result)


# Keep a label column travelling with its treatment
df$treatment_name <- paste("Variety", df$treatment)
result <- speed(df, swap = "treatment", linked_cols = "treatment_name", seed = 42)
#> row and col are used as row and column, respectively.
#> Optimising level: single treatment within whole design 
#> Optimal score reached at iteration 726 for level single treatment within whole design 
head(result$design_df)
#>   row col treatment treatment_name
#> 1   1   1         B      Variety B
#> 2   1   2         A      Variety A
#> 3   1   3         B      Variety B
#> 4   1   4         C      Variety C
#> 5   1   5         D      Variety D
#> 6   2   1         A      Variety A

# Hierarchical split-plot design
df_split <- data.frame(
  row = rep(1:12, each = 4),
  col = rep(1:4, times = 12),
  block = rep(1:4, each = 12),
  wholeplot = rep(1:12, each = 4),
  wholeplot_treatment = rep(rep(LETTERS[1:3], each = 4), times = 4),
  subplot_treatment = rep(letters[1:4], 12)
)

result <- speed(df_split,
                swap = list(wp = "wholeplot_treatment",
                            sp = "subplot_treatment"),
                swap_within = list(wp = "block", sp = "wholeplot"),
                swap_all = TRUE,
                seed = 42)
#> row and col are used as row and column, respectively.
#> Optimising level: wp 
#> Level: wp Iteration: 1000 Score: 100 Best: 100 Since Improvement: 1000 
#> Level: wp Iteration: 2000 Score: 100 Best: 100 Since Improvement: 2000 
#> Early stopping at iteration 2000 for level wp 
#> Optimising level: sp 
#> Optimal score reached at iteration 570 for level sp 

# Plot wholeplot allocations within blocks
autoplot(result, treatments = "wholeplot_treatment")

# Plot subplot allocations within wholeplots
autoplot(result, treatments = "subplot_treatment", block = "wholeplot")


# Using optimise parameter
# Multi-Environment Trial (MET) design
# With 7 replicates of 100 lines in this design, each line will appear
# twice at two different sites and once at the rest of the sites.
lines <- rep(1:100, 7)
df_site <- initialise_design_df(1, 28, 5, 14, 5)
df_initial <- rbind(df_site, df_site, df_site, df_site, df_site)
df_initial$lines <- lines
df_initial$site <- rep(c("a", "b", "c", "d", "e"), each = 140)

df_initial$site_row <- paste(df_initial$site, df_initial$row, sep = "_")
df_initial$site_col <- paste(df_initial$site, df_initial$col, sep = "_")
df_initial$site_block <- paste(df_initial$site, df_initial$block, sep = "_")

# Low iterations keeps the example quick; raise it for a real design.
optimise <- list(
  connectivity = list(
    spatial_factors = ~site,
    iterations = 500,
    early_stop_iterations = 100
  ),
  balance = list(
    swap_within = "site",
    spatial_factors = ~ site_col + site_block,
    iterations = 500,
    early_stop_iterations = 100
  )
)

result <- speed(
  data = df_initial,
  swap = "lines",
  optimise = optimise,
  optimise_params = optim_params(random_initialisation = TRUE),
  adj_weight = 0,
  seed = 112,
  quiet = TRUE
)

head(table(result$design_df$lines, result$design_df$site))
#>    
#>     a b c d e
#>   1 4 0 0 3 0
#>   2 2 0 2 3 0
#>   3 1 2 1 1 2
#>   4 0 1 1 3 2
#>   5 2 1 1 2 1
#>   6 0 1 3 0 3

# Plot the MET design with facets
autoplot(result, treatments = "lines") +
ggplot2::facet_wrap(~site)

```
