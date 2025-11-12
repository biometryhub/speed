# Get Weighted Edges

Calculate the weight of edges from vertices.

## Usage

``` r
get_edges(vertices)
```

## Arguments

- vertices:

  Named list of vertices containing:

  - \- A list of (vertex 1, vertex 2, ...)

## Value

Named list containing:

- \- A vector of edge weights

## See also

[`get_vertices()`](https://biometryhub.github.io/speed/reference/get_vertices.md)

## Examples

``` r
design_matrix <- matrix(c(1, 2, 2, 1, 3, 2, 1, 3, 3), nrow = 3, ncol = 3)
vertices <- get_vertices(design_matrix)
edges <- get_edges(vertices)
```
