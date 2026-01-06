# Verify Inputs for `speed`

Verify inputs for the `speed` function.

## Usage

``` r
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

.verify_optim_params(
  swap_count,
  swap_all_blocks,
  adaptive_swaps,
  start_temp,
  cooling_rate,
  random_initialisation,
  adj_weight,
  bal_weight
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

- iterations:

  Maximum number of iterations for the simulated annealing algorithm
  (default: 10000). For hierarchical designs, can be a named list with
  names matching `swap`.

- early_stop_iterations:

  Number of iterations without improvement before early stopping
  (default: 2000). For hierarchical designs, can be a named list with
  names matching `swap`.

- quiet:

  Logical; if TRUE, suppresses progress messages (default: FALSE)

- seed:

  A numeric value for random seed. If provided, it ensures
  reproducibility of results (default: `NULL`).

- obj_function:

  Objective function used to calculate score (lower is better) (default:
  [`objective_function()`](https://biometryhub.github.io/speed/reference/objective_functions.md)).
  For hierarchical designs, can be a named list with names matching
  `swap`.
