# Apply Nested Splits to a Design Data Frame

Adds nested-unit ID columns (and optional treatment columns) to a design
data frame produced by
[`initialise_design_df()`](https://biometryhub.github.io/speed/reference/initialise_design_df.md).
Each split level subdivides the parent unit (block, or the whole grid if
there are no blocks) into smaller rectangular units of size `nrows` by
`ncols`. Subsequent levels subdivide the units of the previous level,
with the same column-major numbering that
[`initialise_design_df()`](https://biometryhub.github.io/speed/reference/initialise_design_df.md)
uses for blocks.

## Usage

``` r
apply_splits(df, splits, nrows, ncols, block_nrows, block_ncols)
```

## Arguments

- df:

  A data frame with `row` and `col` columns (and `block` if blocking is
  in use), as built by
  [`initialise_design_df()`](https://biometryhub.github.io/speed/reference/initialise_design_df.md).

- splits:

  Deprecated; use
  [`initialise_split_design_df()`](https://biometryhub.github.io/speed/reference/initialise_split_design_df.md)
  instead. A named list of nested-unit specifications, ordered from the
  outermost level to the innermost. Each entry is itself a list with
  `nrows` and `ncols` (the dimensions of one unit at that level, in
  cells) and an optional `items` (treatments to allocate across the
  units at that level, one item per unit, ordered by parent then
  within-parent ID). For each level, `<name>` and `<name>_treatment`
  columns are added (the latter only if `items` is provided). Used to
  build hierarchical layouts such as split-plot, split-split-plot, and
  strip-plot designs. (default: `NULL`)

- nrows:

  Number of rows in the design (default: `NULL`)

- ncols:

  Number of columns in the design (default: `NULL`)

- block_nrows:

  Number of rows in each block (default: `NULL`)

- block_ncols:

  Number of columns in each block (default: `NULL`)

## Value

The data frame with one ID column per split (named after the split) and
one `<name>_treatment` column per split that supplies `items`.
