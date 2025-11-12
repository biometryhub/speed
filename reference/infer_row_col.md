# Infer 'row' and 'col' with Patterns

Infer data frame names with patterns to determine if variations of 'row'
and 'col' columns exist.

## Usage

``` r
infer_row_col(
  layout_df,
  grid_factors = list(dim1 = "row", dim2 = "col"),
  quiet = FALSE
)
```

## Arguments

- layout_df:

  A data frame representing the current design

- grid_factors:

  A named list specifying grid factors to construct a matrix for
  calculating adjacency score, `dim1` for row and `dim2` for column.
  (default: `list(dim1 = "row", dim2 = "col")`).

- quiet:

  Logical (default: FALSE). If TRUE, output will be suppressed.

## Value

A list containing:

- **inferred** - Logical; if TRUE, row and column columns were inferred
  from the data frame

- **row** - Name of the row column

- **col** - Name of the column column
