# Validate a Design's Coordinates and Build its Grid Index

Coerces and validates the `row_column`/`col_column` coordinates,
returning everything
[`build_design_matrix()`](https://biometryhub.github.io/speed/reference/build_design_matrix.md)
needs to place plots on a grid: the two-column matrix index and the
grid's dimensions.

Split out from
[`build_design_matrix()`](https://biometryhub.github.io/speed/reference/build_design_matrix.md)
because it is the expensive half and the *invariant* half: during
annealing only the treatment column changes, so the index can be built
once per
[`speed()`](https://biometryhub.github.io/speed/reference/speed.md) run
and reused every iteration.

## Usage

``` r
grid_index(df, row_column = "row", col_column = "col")
```

## Arguments

- df:

  A data frame with columns named by `row_column` and `col_column`.

- row_column:

  Column name of the row position variable (default `"row"`).

- col_column:

  Column name of the column position variable (default `"col"`).

## Value

A list with `idx` (an `nrow(df)` x 2 integer matrix of grid positions),
`nrow` and `ncol` (the grid's dimensions), and `n` (the number of plots
the index was built for, used to detect a stale index).
