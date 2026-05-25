# Shift a Matrix With NA Padding

Translates a matrix `m` by (`dx` columns, `dy` rows), padding cells that
fall off the source with `fill`. Positive `dx` shifts right; positive
`dy` shifts down.

## Usage

``` r
shift_pad(m, dx, dy, fill = NA)
```

## Arguments

- m:

  A matrix.

- dx, dy:

  Integer column (x-axis) and row (y-axis) offsets.

- fill:

  Value used to pad cells with no source (default `NA`).

## Value

A matrix of the same dimensions as `m`.
