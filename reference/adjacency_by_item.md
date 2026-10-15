# Same-Item Adjacency per Item

Weighted same-item edge count per level of `items`, counted from
`rescored_rows` holding rows of the items to be counted.

## Usage

``` r
adjacency_by_item(items, neighbours, rescored_rows)
```

## Arguments

- items:

  Factor of items, one per row.

- neighbours:

  Neighbour index from
  [`adjacency_neighbours()`](https://biometryhub.github.io/speed/reference/adjacency_neighbours.md).

- rescored_rows:

  Row positions to count from.

## Value

Numeric vector with one count per level of `items`.
