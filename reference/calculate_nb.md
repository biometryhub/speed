# Neighbour Balance Calculation

A metric that counts the occurrence of the same adjacent pairs. Only
horizontal and vertical pairs are counted.

## Usage

``` r
calculate_nb(design_matrix, pair_mapping = NULL)
```

## Arguments

- design_matrix:

  A matrix representing the design

- pair_mapping:

  A named vector of pairs generated from
  [create_pair_mapping](https://biometryhub.github.io/speed/reference/create_pair_mapping.md)

## Value

Named list containing:

- nb - Table of pairs of items and their number of occurrence

- max_nb - The highest number of occurrence

- max_pairs - Vector of pairs of items with the highest number of
  occurrence

## See also

[`objective_function_piepho()`](https://biometryhub.github.io/speed/reference/objective_function_piepho.md)

## Examples

``` r
design_matrix <- matrix(c(1, 2, 2, 1, 3, 3, 1, 3, 3), nrow = 3, ncol = 3)
calculate_nb(design_matrix)
#> $nb
#> $nb$`3,3`
#> [1] 4
#> 
#> $nb$`2,2`
#> [1] 1
#> 
#> $nb$`2,3`
#> [1] 2
#> 
#> $nb$`1,1`
#> [1] 2
#> 
#> $nb$`1,2`
#> [1] 1
#> 
#> $nb$`1,3`
#> [1] 2
#> 
#> 
#> $max_nb
#> [1] 4
#> 
#> $max_pairs
#> [1] "3,3"
#> 
#> $var
#> [1] 1.2
#> 
```
