# Create Input for Internal speed Function

Create Input for Internal speed Function

## Usage

``` r
create_speed_input(
  swap,
  swap_within,
  spatial_factors,
  grid_factors,
  iterations,
  early_stop_iterations,
  obj_function,
  swap_all,
  optimize = NULL
)
```

## Arguments

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
  (default: `list(dim1 = "row", dim2 = "col")`).

- iterations:

  Maximum number of iterations for the simulated annealing algorithm
  (default: 10000). For hierarchical designs, can be a named list with
  names matching `swap`.

- early_stop_iterations:

  Number of iterations without improvement before early stopping
  (default: 2000). For hierarchical designs, can be a named list with
  names matching `swap`.

- obj_function:

  Objective function used to calculate score (lower is better) (default:
  [`objective_function()`](https://biometryhub.github.io/speed/reference/objective_functions.md)).
  For hierarchical designs, can be a named list with names matching
  `swap`.

- swap_all:

  Logical; Whether to swap all matching items or a single item at a time
  (default: FALSE)
