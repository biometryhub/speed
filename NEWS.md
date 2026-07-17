# speed 0.0.10

## Major Changes

- Added a `summary()` method for `"design"` objects that reports design-evaluation
  metrics: structure and replication, the optimised score decomposed into
  adjacency and balance components, connectedness, within-block concurrences (for
  incomplete blocks), replicate spatial spans, an opt-in A-efficiency factor, and
  neighbour balance (for neighbour-balance designs).

## Minor Changes

- `speed()` results now carry a `metadata` field recording each level's swap
  variable, spatial factors, weights and optimisation settings.

# speed 0.0.9

## Major Changes

- Deprecated the `splits` argument of `initialise_design_df()` in favor of `initialise_split_design_df()`.
  Passing `splits` now warns with the equivalent suggested call.

## Bug Fixes

- `speed()` no longer emits a "Setting row names on a tibble is deprecated" warning when passed a tibble;
  row labels are now only reset for base data frames.
- `speed()` now accepts designs with `vctrs`-backed columns that report a multi-class `class()` (such as the
  tables produced by the `edibble` package). Previously these failed with
  "first argument has length > 1" when restoring column types; such columns are now restored as `character`.

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
