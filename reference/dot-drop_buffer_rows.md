# Drop buffer plots from a design data frame

[`add_buffers()`](https://biometryhub.github.io/speed/reference/add_buffers.md)
appends rows with the treatment column(s) set to `"buffer"`. Buffers are
a practical field-layout convenience, not part of the statistical
design, so [`print()`](https://rdrr.io/r/base/print.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) exclude them from
every computation. Removes the buffer rows and the now-unused `"buffer"`
factor level. A no-op when there is no metadata or no buffers.

## Usage

``` r
.drop_buffer_rows(df, meta)
```

## Arguments

- df:

  A design data frame.

- meta:

  The design's `metadata` (for the per-level swap columns).

## Value

`df` with any buffer rows removed.
