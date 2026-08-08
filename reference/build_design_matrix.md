# Build a Spatial Design Matrix from a Data Frame

Places each treatment value at the grid position given by its
`row_column` and `col_column` coordinates, returning a character matrix
of dimensions `max(row)` by `max(col)`. Cells with no corresponding row
in `df` are `NA`.

Each plot's position comes from its own coordinates, so the row ordering
of `df` is irrelevant, as is the level order of factor coordinate
columns.

Coordinates are used as-is, never renumbered: a gap in the coordinates
is a real gap in the field (a missing plot, or a buffer that was
removed), so collapsing it would make non-adjacent plots into
neighbours. Callers must therefore cope with `NA` cells.

## Usage

``` r
build_design_matrix(
  df,
  swap,
  row_column = "row",
  col_column = "col",
  index = NULL
)
```

## Arguments

- df:

  A data frame with columns named by `swap`, `row_column`, `col_column`.

- swap:

  Column name of the treatment variable.

- row_column:

  Column name of the row position variable (default `"row"`).

- col_column:

  Column name of the column position variable (default `"col"`).

- index:

  Optional pre-built index from
  [`grid_index()`](https://biometryhub.github.io/speed/reference/grid_index.md).
  Supplying one skips coordinate coercion and validation, which is the
  bulk of the work and is invariant during annealing.
  [`speed()`](https://biometryhub.github.io/speed/reference/speed.md)
  builds one per run; anything calling this once should leave it `NULL`.

## Value

A character matrix of dimensions `max(row)` by `max(col)`.
