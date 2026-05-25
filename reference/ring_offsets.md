# Offsets for a Chebyshev or Manhattan Ring

Returns the `(dx, dy)` offsets of cells at exact distance `d` from the
origin under the chosen ring shape. The origin itself is excluded.

## Usage

``` r
ring_offsets(d, ring_type = c("manhattan", "chebyshev"))
```

## Arguments

- d:

  Positive integer ring radius.

- ring_type:

  Ring shape: `"manhattan"` (default; diamond ring) or `"chebyshev"`
  (square ring). See `ring_offsets()`.

## Value

Integer matrix with two columns - column 1 holds `dx`, column 2 holds
`dy` - one row per cell on the ring.
