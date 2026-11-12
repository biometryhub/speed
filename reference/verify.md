# Verify Inputs for `speed`

Validates the arguments as the user passed them, dispatching to the
checker for the input shape given and running the checks that apply to
all three. Checks needing the resolved per-level list cannot run here;
they are documented under
[`.verify_level_columns()`](https://biometryhub.github.io/speed/reference/verify_resolved.md)
and are called from
[`speed()`](https://biometryhub.github.io/speed/reference/speed.md) once
[`create_speed_input()`](https://biometryhub.github.io/speed/reference/create_speed_input.md)
has built it.

`grid_factors` must be a single list naming the two grid axes, since
[`infer_row_col()`](https://biometryhub.github.io/speed/reference/infer_row_col.md)
resolves one pair of axes for the whole design. The axes cannot vary
between levels of a hierarchical design; `by` is what splits a design
into separate grids.

`grid_factors` is a plain list, so a mistyped `by` would be ignored and
every grid silently pooled. Checked before any optimisation happens.

## Usage

``` r
.verify_inputs(
  data,
  swap,
  swap_within,
  spatial_factors,
  grid_factors,
  iterations,
  early_stop_iterations,
  obj_function,
  quiet,
  seed,
  optimise = NULL
)

.verify_speed_inputs(
  data,
  swap,
  swap_within,
  spatial_factors,
  iterations,
  early_stop_iterations,
  quiet,
  seed
)

.verify_hierarchical_inputs(
  data,
  swap,
  swap_within,
  spatial_factors,
  iterations,
  early_stop_iterations,
  obj_function,
  quiet,
  seed
)

.verify_obj_args(obj_args)

.verify_optim_params(
  swap_count,
  swap_all_blocks,
  adaptive_swaps,
  start_temp,
  cooling_rate,
  random_initialisation,
  adj_weight,
  bal_weight,
  stop_at_optimal
)

.verify_grid_factors(grid_factors)

.verify_grid_by(data, grid_factors)
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

- quiet:

  Logical; if TRUE, suppresses progress messages (default: FALSE)

- seed:

  A numeric value for random seed. If provided, it ensures
  reproducibility of results (default: `NULL`).

- optimise:

  The `optimise` argument as passed to
  [`speed()`](https://biometryhub.github.io/speed/reference/speed.md).

- obj_args:

  One level's `obj_args`.

- swap_count:

  Number of treatment swaps per iteration (default: 1).

- swap_all_blocks:

  Logical; if `TRUE`, performs swaps in all blocks at each iteration
  (default: `FALSE`).

- adaptive_swaps:

  Logical; if `TRUE`, adjusts swap parameters based on temperature
  (default: `FALSE`).

- start_temp:

  Starting temperature for simulated annealing (default: 100). A higher
  start temperature allows the algorithm to accept worse solutions early
  on, encouraging exploration of the solution space and helping to avoid
  local optima. Lower values make the algorithm greedier from the start,
  which can speed up convergence but increases the risk of getting stuck
  in a poor solution. A good starting temperature allows moderately
  worse solutions to be accepted with a probability of 70-90% at the
  beginning of the optimisation.

- cooling_rate:

  Rate at which temperature decreases for simulated annealing (default:
  0.99). This controls how quickly the algorithm shifts from exploration
  to exploitation. The temperature is updated at each iteration by
  multiplying it by this rate: `T_i = start_temp * cooling_rate^i`. A
  higher cooling rate (e.g. 0.995-0.999) results in slower cooling and a
  longer exploration phase, which is generally better for complex or
  noisy optimisation landscapes. Lower values (e.g. 0.95-0.98) cool
  quickly, leading to faster convergence but greater risk of premature
  convergence to a suboptimal design.

- random_initialisation:

  Number of times to randomly shuffle items within `swap_within`; the
  design with the best score is used as an initial design (default: 0).

- adj_weight, bal_weight:

  Deprecated. Weights for the adjacency and balance scores. Pass them to
  [`speed()`](https://biometryhub.github.io/speed/reference/speed.md)
  directly or through a level's `obj_args` in `optimise` instead.

- stop_at_optimal:

  Logical; if `TRUE`, stops the level as soon as the best score reaches
  the lowest score its layout allows (default: `TRUE`). The bound is
  only derivable for the default
  [`objective_function()`](https://biometryhub.github.io/speed/reference/objective_functions.md)
  with non-negative weights and no `relationship` matrix; otherwise the
  level runs to its usual stopping rules regardless of this setting.
