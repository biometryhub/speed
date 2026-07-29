# Replicate spatial spans

For each treatment, the minimum Manhattan separation between its
replicate plots along rows and along columns (`+ 1` so the span counts
plots inclusively), plus the worst-case (minimum) span across replicated
treatments. Adapted from a colleague's `sommario.duplicates.span.doe`. A
small worst-case span flags replicates that sit close together.

## Usage

``` r
.replicate_spans(df, swap, rc, cc)
```

## Arguments

- df:

  Design data frame.

- swap:

  Treatment column name.

- rc, cc:

  Row and column column names.

## Details

Measured only along the two grid axes (`rc`/`cc`, resolved from
`grid_factors` by
[`infer_row_col()`](https://biometryhub.github.io/speed/reference/infer_row_col.md)),
not the design's other spatial factors. A span is a distance, and only
the grid axes are ordered: the separation between block 1 and block 3 is
not 2. For non-grid factors the equivalent question ("do a treatment's
replicates land in the same block?") is a count, and is answered by the
concurrence matrix diagonal instead.
