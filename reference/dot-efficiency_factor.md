# A-efficiency factor (opt-in wrapper)

Thin guarded wrapper over
[`calculate_efficiency_factor()`](https://biometryhub.github.io/speed/reference/calculate_efficiency_factor.md)
(a row–column model metric), using the design's resolved row/column
columns. Returns `NA` with a reason rather than erroring when its
assumptions are not met.

## Usage

``` r
.efficiency_factor(df, swap, rc, cc, grid)
```

## Arguments

- rc, cc:

  Row and column column names.

- grid:

  A
  [`grid_index()`](https://biometryhub.github.io/speed/reference/grid_index.md)
  list, or a character reason there is no grid.
