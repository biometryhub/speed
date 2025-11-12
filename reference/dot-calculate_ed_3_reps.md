# Even Distribution Calculation for 3 Replications

A metric that represents the even distribution of items with 3
replications with their minimum spanning tree (mst).

## Usage

``` r
.calculate_ed_3_reps(edges, current_ed = NULL)
```

## Arguments

- edges:

  A list of vectors of edge weights

## Value

Named list containing:

- msts - Named list of pairs of items and their mst

- min_mst - The lowest mst

- min_items - Pairs of items with the lowest mst

## See also

[`get_edges()`](https://biometryhub.github.io/speed/reference/get_edges.md)
