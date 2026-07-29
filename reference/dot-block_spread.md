# Replicate spread across blocks

How many distinct blocks each treatment's replicates are spread over,
and how many treatments land more than once in a single block. This is
the block equivalent of
[`.replicate_spans()`](https://biometryhub.github.io/speed/reference/dot-replicate_spans.md):
a block factor is nominal, so there is no distance to measure along, but
"how many different blocks do a treatment's replicates reach" is still
well defined. It reads the same treatment-by-block incidence `M` as
[`.design_concurrence()`](https://biometryhub.github.io/speed/reference/dot-design_concurrence.md) -
the per-treatment figures come from `M`'s rows, where concurrence uses
the off-diagonals of `M M'`.

## Usage

``` r
.block_spread(df, swap, block)
```

## Arguments

- block:

  Block factor column name.

## Details

Computed whenever a block factor is present, independent of the
`concurrence` argument: unlike concurrence it remains informative for
complete blocks, where it confirms every treatment reaches every block.
