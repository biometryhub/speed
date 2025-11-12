# Neighbor Balance Calculation without Pair Mapping

A metric that counts the occurrence of the same adjacent pairs. Only
horizontal and vertical pairs are counted.

## Usage

``` r
.calculate_nb(design_matrix)
```

## Arguments

- design_matrix:

  A matrix representing the design

## Value

Named list containing:

- nb - Named list of pairs of items and their number of occurrence

- max_nb - The highest number of occurrence

- max_pairs - Vector of pairs of items with the highest number of
  occurrence
