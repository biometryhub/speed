# Verify Inputs for `speed`

Verify inputs for the `speed` function.

`swap_all = TRUE` proposes a move by exchanging *every* plot holding one
treatment with *every* plot holding another. That is a rearrangement of
the design only when both treatments occupy the same number of plots
within the swap group; when they do not, the two treatments exchange
replication counts and the design that comes back is not the design that
went in. Error before any optimisation happens rather than silently
altering replication.

Called on the resolved `optimise` list, so it covers simple, legacy
hierarchical and `optimise = ` calls alike, including levels that set
`swap_all` individually.

`grid_factors` is a plain list, so a mistyped `by` would be ignored and
every grid silently pooled. Checked before any optimisation happens.

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

.verify_swap_all_replication(data, optimise, dummy_group = NULL)

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

- optimise:

  A list of named arguments describing optimising parameters; see more
  in example.

- dummy_group:

  Name of the internal placeholder column used for a level with no
  `swap_within` boundary, so it can be described as the whole design.

- grid_factors:

  A named list specifying grid factors to construct a matrix for
  calculating adjacency score, `dim1` for row and `dim2` for column.
  (default: `list(dim1 = "row", dim2 = "col")`).

  An optional third element, `by`, names a column that groups plots into
  *separate* grids - a multi-environment trial, where each site reuses
  the same `row`/`col` numbering. Each grid is then scored on its own
  and the adjacency counts summed, so no adjacency is counted between
  plots at different sites, e.g.
  `list(dim1 = "row", dim2 = "col", by = "site")`. Without it, a design
  whose sites share coordinates is refused rather than silently pooled.
