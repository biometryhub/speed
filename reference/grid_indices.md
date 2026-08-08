# Split a Design into One Grid Index per Grid

The multi-grid counterpart of
[`grid_index()`](https://biometryhub.github.io/speed/reference/grid_index.md).
A multi-environment trial is several grids that share a treatment set
and never share an edge, so it cannot be one matrix: sites reuse
`row`/`col`, and pooling them either silently overwrites plots or
invents adjacencies between sites.

With `by = NULL` this returns a one-element list, so callers have a
single code path whether or not the design spans grids.

## Usage

``` r
grid_indices(df, row_column = "row", col_column = "col", by = NULL)
```

## Arguments

- df:

  A data frame with columns named by `row_column` and `col_column`.

- row_column:

  Column name of the row position variable (default `"row"`).

- col_column:

  Column name of the column position variable (default `"col"`).

- by:

  Optional column name grouping plots into grids (e.g. `"site"`). `NULL`
  treats the design as one grid.

## Value

A named list with one element per grid, each a list of `rows` (the
positions in `df` belonging to that grid) and `index` (that grid's
[`grid_index()`](https://biometryhub.github.io/speed/reference/grid_index.md)).
Named `"1"` when `by` is `NULL`.
