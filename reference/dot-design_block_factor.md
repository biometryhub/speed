# Detect a block-type factor for one level of a design

Among *that level's* spatial factors that are not the row or column
factor, prefer one named like `block`; otherwise take the first such
factor. Failing that, fall back to a column literally named `block`. The
chosen factor is surfaced in the concurrence output (`[block: ...]`), so
the choice is visible to the user.

## Usage

``` r
.design_block_factor(df, spatial_cols, rc, cc)
```

## Arguments

- spatial_cols:

  Character vector of this level's spatial factor columns.

## Details

Resolved per level (rather than once from the union of every level's
spatial factors) so that a hierarchical design whose levels are blocked
by different factors doesn't have one level's block column applied to
another.
