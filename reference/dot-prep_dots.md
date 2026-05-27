# Prep `dots$relationship` once with the union of treatments seen at every swap level.

Prep `dots$relationship` once with the union of treatments seen at every
swap level.

## Usage

``` r
.prep_dots(dots, optimise, data)
```

## Arguments

- dots:

  A named list captured from `...`.

- optimise:

  Per-level `optimise` list as built by
  [`create_speed_input()`](https://biometryhub.github.io/speed/reference/create_speed_input.md).

- data:

  The (factor-converted) design data frame.

## Value

`dots`, with `relationship` replaced by the prepped form when present.
