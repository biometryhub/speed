# Get Vertices of Each Item

Get the vertices of each item in a design matrix.

## Usage

``` r
get_vertices(design_matrix)
```

## Arguments

- design_matrix:

  A matrix representing the design

## Value

Named list containing:

- \- A list of (vertex 1, vertex 2, ...)

## See also

[`get_edges()`](https://biometryhub.github.io/speed/reference/get_edges.md)

## Examples

``` r
design_matrix <- matrix(c(1, 2, 2, 1, 3, 2, 1, 3, 3), nrow = 3, ncol = 3)
vertices <- get_vertices(design_matrix)
```
