# speed 0.0.11

## Major Changes

- `speed()` now stops as soon as a design reaches the lowest score its layout allows, applicable only to the
  default `objective_function()`. `summary()` now reports the lower bound score alongside the achieved one.

# speed 0.0.10

## Major Changes

- Added a `summary()` method for `"design"` objects, reporting structure and replication, a
  decomposed optimisation score, and design-quality diagnostics (connectedness, concurrence,
  replicate spans and spread across blocks, neighbour balance, and opt-in efficiency).
  ([#73](https://github.com/biometryhub/speed/issues/73))

# speed 0.0.9

## Major Changes

- Deprecated the `splits` argument of `initialise_design_df()` in favor of `initialise_split_design_df()`.
  Passing `splits` now warns with the equivalent suggested call.

## Bug Fixes

- `speed()` now errors when `swap_all = TRUE` is used on a design with unequal within-group
  replication, instead of silently swapping treatments with different replication counts.
- `speed()` no longer returns numeric/integer columns (e.g. `treatment`, `row`, `col`) as their
  internal factor level codes instead of their original values.
- `speed()` no longer emits a "Setting row names on a tibble is deprecated" warning when passed a tibble.
- `speed()` now accepts designs with `vctrs`-backed multi-class columns (e.g. from the `edibble`
  package) instead of erroring; such columns are now returned as `character`.

# speed 0.0.8

## Major Changes

- Added `ring_dists`, `ring_weights`, and `ring_type` arguments to `calculate_adjacency_score` for weighting
  matches at larger adjacent ring radii; can be passed via `speed()`.

# speed 0.0.7

## Major Changes

- Added `splits` argument to `initialise_design_df` to support split-plot designs (#92).
- Added `main_weight` and `interaction_weight` arguments to `objective_function_factorial` to tune the trade-off 
  between main-treatment and interaction balance (#90).

## Minor Changes

- Fixed `autoplot.design` where `'block'` column was required when providing another column for `block` (#88).

# speed 0.0.6

## Major Changes

- Extended `random_initialise` to handle hierarchical (multi-level) `optimise` lists by shuffling within each
  level's grouping.

# speed 0.0.5

## Major Changes

- Added `objective_function_factorial` for factorial designs, combining main-treatment and interaction balance
  scores (#78).

# speed 0.0.4

## Major Changes

- Added vignettes for MET (#70) and factorial (#71) designs.

# speed 0.0.3

## Major Changes

- Optimisation parameters were changed from options to arguments to enable better reproducibility of designs (#65
- Enabled one stage MET designs
- Added contributing guide and code of conduct (#59)

See changelog for further details.

# speed 0.0.2

## Major Changes

- Enabled more complex designs and added some vignettes with examples and detailed use.

See changelog for further details.


# speed 0.0.1

First version. 
