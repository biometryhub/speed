# Objective Function with Metric from Piepho

Create an objective function including even distribution and neighbor
balance introduced by Piepho 2018.

## Usage

``` r
objective_function_piepho(
  design,
  swap,
  spatial_cols,
  current_score_obj = NULL,
  swapped_items = NULL,
  pair_mapping = NULL,
  row_column = "row",
  col_column = "col",
  by = NULL,
  grid_index = NULL,
  ...
)
```

## Arguments

- design:

  A data frame representing the spatial information of the design

- swap:

  A column name of the items to be swapped

- spatial_cols:

  Column name(s) of the spatial factors

- current_score_obj:

  A named list containing the current score

- swapped_items:

  The items that had just been swapped

- pair_mapping:

  A named vector of pairs generated from
  [create_pair_mapping](https://biometryhub.github.io/speed/reference/create_pair_mapping.md)

- row_column:

  Name of column representing the row of the design (default: "row")

- col_column:

  Name of column representing the column of the design (default: "col")

- by, grid_index:

  Optional grouping of plots into separate grids; see
  [`calculate_adjacency_score()`](https://biometryhub.github.io/speed/reference/calculate_adjacency_score.md).
  Neighbour balance sums across grids, while evenness of distribution is
  scored **per grid** and the scores summed, reported both in total and
  per grid. A grid with no treatment replicated inside it contributes
  `0`.

- ...:

  Extra parameters passed from
  [speed](https://biometryhub.github.io/speed/reference/speed.md)

## Value

A function which returns a named list of numeric values with one
required name `score` representing the score of the design (lower is
better) with a signature `function(design_df, swap, spatial_cols, ...)`.
An objective may optionally return a `components` element: a named
numeric vector of the additive pieces that sum to `score` (e.g.
`c(adjacency = ..., balance = ...)`). When present,
[`summary.design()`](https://biometryhub.github.io/speed/reference/summary.design.md)
reports this as a faithful score decomposition. See signature details in
[objective_function_signature](https://biometryhub.github.io/speed/reference/objective_functions.md).

## References

Piepho, H. P., Michel, V., & Williams, E. (2018). Neighbor balance and
evenness of distribution of treatment replications in row-column
designs. Biometrical journal. Biometrische Zeitschrift, 60(6),
1172-1189. <https://doi.org/10.1002/bimj.201800013>

## See also

[`objective_function()`](https://biometryhub.github.io/speed/reference/objective_functions.md),
[`create_pair_mapping()`](https://biometryhub.github.io/speed/reference/create_pair_mapping.md)

## Examples

``` r
design_df <- initialise_design_df(
  items = c(1, 2, 2, 1, 3, 3, 1, 3, 3),
  nrows = 3,
  ncols = 3
)

pair_mapping <- create_pair_mapping(design_df$treatment)
objective_function_piepho(design_df, "treatment", c("row", "col"), pair_mapping = pair_mapping)
#> $score
#> [1] 16.36667
#> 
#> $ed
#> $ed$`1`
#> $ed$`1`$`2`
#> $ed$`1`$`2`$msts
#> $ed$`1`$`2`$msts$`2`
#> [1] 1
#> 
#> 
#> $ed$`1`$`2`$min_mst
#> [1] 1
#> 
#> $ed$`1`$`2`$min_items
#> [1] "2"
#> 
#> 
#> $ed$`1`$`4`
#> $ed$`1`$`4`$msts
#> $ed$`1`$`4`$msts$`3`
#> [1] 3
#> 
#> 
#> $ed$`1`$`4`$min_mst
#> [1] 3
#> 
#> $ed$`1`$`4`$min_items
#> [1] "3"
#> 
#> 
#> $ed$`1`$`3`
#> $ed$`1`$`3`$msts
#> $ed$`1`$`3`$msts$`1`
#> [1] 2
#> 
#> 
#> $ed$`1`$`3`$min_mst
#> [1] 2
#> 
#> $ed$`1`$`3`$min_items
#> [1] "1"
#> 
#> 
#> 
#> 
#> $ed_per_grid
#>         1 
#> 0.1666667 
#> 
#> $bal
#> [1] 8
#> 
#> $adj
#> [1] 7
#> 
#> $nb
#> $nb$nb
#> 1,1 1,2 1,3 2,2 2,3 3,3 
#>   2   1   2   1   2   4 
#> 
#> $nb$max_nb
#> [1] 4
#> 
#> $nb$max_pairs
#> [1] "3,3"
#> 
#> $nb$var
#> [1] 1.2
#> 
#> 
#> $components
#> neighbour_balance even_distribution           balance         adjacency 
#>         1.2000000         0.1666667         8.0000000         7.0000000 
#> 
# usage in speed, speed(..., obj_function = objective_function_piepho, pair_mapping = pair_mapping)
```
