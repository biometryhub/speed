# Treatment concurrences within blocks

From the treatment-by-block incidence `M`, the concurrence matrix is
`C = M M'`: off-diagonals are pairwise concurrences (how often two
treatments share a block), the diagonal is replication.

## Usage

``` r
.design_concurrence(df, swap, block, force = FALSE)
```

## Arguments

- force:

  Compute even when blocks are complete.

## Details

Concurrences only carry information for *incomplete* blocks (block size
\< number of treatments). For complete blocks (RCBD, split-plot, ...)
every pair co-occurs in every block, so every concurrence equals the
replication - it merely restates the design and is skipped unless
`force = TRUE`.
