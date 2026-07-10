# Base Type of a Column for Round-Tripping

Maps a column to a single base type name (usable as `as.<type>()`) so
its type can be restored after the SA loop. Columns with an exotic or
multi-class [`class()`](https://rdrr.io/r/base/class.html) (e.g.
vctrs-backed columns such as those in an edibble design) cannot be
reconstructed with an `as.<class>()` function, so they are restored as
`character`.

## Usage

``` r
base_type(x)
```

## Arguments

- x:

  A vector (data frame column)

## Value

A length-1 character string naming a base type.
