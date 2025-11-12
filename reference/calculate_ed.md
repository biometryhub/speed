# Even Distribution Calculation

A metric that represents the even distribution of each item with their
minimum spanning tree (mst).

## Usage

``` r
calculate_ed(design_matrix, current_ed = NULL, swapped_items = NULL)
```

## Arguments

- design_matrix:

  A matrix representing the design

- current_ed:

  Named list of the current ed calculation

- swapped_items:

  The items that had just been swapped

## Value

Named list containing:

- \- Named list containing:

  - msts - Named list of items and their mst

  - min_mst - The lowest mst

  - min_items - Pairs of items with the lowest mst

## See also

[`objective_function_piepho()`](https://biometryhub.github.io/speed/reference/objective_function_piepho.md)

## Examples

``` r
design_matrix <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
calculate_ed(design_matrix)
#> $`2`
#> $`2`$msts
#> $`2`$msts$`2`
#> [1] 1
#> 
#> 
#> $`2`$min_mst
#> [1] 1
#> 
#> $`2`$min_items
#> [1] "2"
#> 
#> 
#> $`4`
#> $`4`$msts
#> $`4`$msts$`3`
#> [1] 3
#> 
#> 
#> $`4`$min_mst
#> [1] 3
#> 
#> $`4`$min_items
#> [1] "3"
#> 
#> 
#> $`3`
#> $`3`$msts
#> $`3`$msts$`1`
#> [1] 2
#> 
#> 
#> $`3`$min_mst
#> [1] 2
#> 
#> $`3`$min_items
#> [1] "1"
#> 
#> 
```
