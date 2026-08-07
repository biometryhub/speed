# Initialise Design Data Frame

Initialise a design data frame with or without blocking.

## Usage

``` r
initialise_design_df(
  items,
  nrows,
  ncols,
  block_nrows = NULL,
  block_ncols = NULL
)

initialize_design_df(
  items,
  nrows,
  ncols,
  block_nrows = NULL,
  block_ncols = NULL
)
```

## Arguments

- items:

  Items to be placed in the design. Either a single numeric value (the
  number of equally replicated items), or a vector of items.

- nrows:

  Number of rows in the design

- ncols:

  Number of columns in the design

- block_nrows:

  Number of rows in each block

- block_ncols:

  Number of columns in each block

## Value

A data frame containing the design

## Examples

``` r
initialise_design_df(
  items = c(1, 2, 2, 1, 3, 3, 1, 3, 3),
  nrows = 3,
  ncols = 3
)
#>   row col treatment
#> 1   1   1         1
#> 2   2   1         2
#> 3   3   1         2
#> 4   1   2         1
#> 5   2   2         3
#> 6   3   2         3
#> 7   1   3         1
#> 8   2   3         3
#> 9   3   3         3

# blocking
initialise_design_df(rep(1:8, 4), 8, 4, 2, 2)
#>    row col treatment row_block col_block block
#> 1    1   1         1         1         1     1
#> 2    2   1         2         1         1     1
#> 3    3   1         3         2         1     2
#> 4    4   1         4         2         1     2
#> 5    5   1         5         3         1     3
#> 6    6   1         6         3         1     3
#> 7    7   1         7         4         1     4
#> 8    8   1         8         4         1     4
#> 9    1   2         1         1         1     1
#> 10   2   2         2         1         1     1
#> 11   3   2         3         2         1     2
#> 12   4   2         4         2         1     2
#> 13   5   2         5         3         1     3
#> 14   6   2         6         3         1     3
#> 15   7   2         7         4         1     4
#> 16   8   2         8         4         1     4
#> 17   1   3         1         1         2     5
#> 18   2   3         2         1         2     5
#> 19   3   3         3         2         2     6
#> 20   4   3         4         2         2     6
#> 21   5   3         5         3         2     7
#> 22   6   3         6         3         2     7
#> 23   7   3         7         4         2     8
#> 24   8   3         8         4         2     8
#> 25   1   4         1         1         2     5
#> 26   2   4         2         1         2     5
#> 27   3   4         3         2         2     6
#> 28   4   4         4         2         2     6
#> 29   5   4         5         3         2     7
#> 30   6   4         6         3         2     7
#> 31   7   4         7         4         2     8
#> 32   8   4         8         4         2     8
```
