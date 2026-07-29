# Design connectedness (base R, no lme4)

A design is connected if every treatment contrast is estimable after
adjusting for the factors the design is stratified by - its spatial
factors (row, col, ...) **and** any block factor. We fit
`lm(dummy ~ <nuisance> + treatment)` with `treatment` last, so that any
confounding aliases the treatment coefficients (which we count) rather
than the nuisance ones. Zero aliased treatment coefficients implies
treatment is fully estimable.

## Usage

``` r
.design_connectedness(df, swap, block, spatial_cols, force = FALSE)
```

## Arguments

- spatial_cols:

  Character vector of the level's spatial factor columns.

- force:

  Fit the model even for very large designs (where it is skipped by
  default because the dense `lm` fit is expensive).

## Details

The response is a dummy: estimability is a rank property of the design
matrix, independent of the data. Counting aliasing only among treatment
terms avoids false positives when nuisance factors are themselves
collinear (e.g. a block factor that coincides with rows in a resolvable
design).
