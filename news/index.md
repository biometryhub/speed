# Changelog

## speed 0.0.9

### Major Changes

- Deprecated the `splits` argument of
  [`initialise_design_df()`](https://biometryhub.github.io/speed/reference/initialise_design_df.md)
  in favor of
  [`initialise_split_design_df()`](https://biometryhub.github.io/speed/reference/initialise_split_design_df.md).
  Passing `splits` now warns with the equivalent suggested call.

## speed 0.0.8

### Major Changes

- Added `ring_dists`, `ring_weights`, and `ring_type` arguments to
  `calculate_adjacency_score` for weighting matches at larger adjacent
  ring radii; can be passed via
  [`speed()`](https://biometryhub.github.io/speed/reference/speed.md).

## speed 0.0.7

### Major Changes

- Added `splits` argument to `initialise_design_df` to support
  split-plot designs
  ([\#92](https://github.com/biometryhub/speed/issues/92)).
- Added `main_weight` and `interaction_weight` arguments to
  `objective_function_factorial` to tune the trade-off between
  main-treatment and interaction balance
  ([\#90](https://github.com/biometryhub/speed/issues/90)).

### Minor Changes

- Fixed `autoplot.design` where `'block'` column was required when
  providing another column for `block`
  ([\#88](https://github.com/biometryhub/speed/issues/88)).

## speed 0.0.6

### Major Changes

- Extended `random_initialise` to handle hierarchical (multi-level)
  `optimise` lists by shuffling within each level’s grouping.

## speed 0.0.5

### Major Changes

- Added `objective_function_factorial` for factorial designs, combining
  main-treatment and interaction balance scores
  ([\#78](https://github.com/biometryhub/speed/issues/78)).

## speed 0.0.4

### Major Changes

- Added vignettes for MET
  ([\#70](https://github.com/biometryhub/speed/issues/70)) and factorial
  ([\#71](https://github.com/biometryhub/speed/issues/71)) designs.

## speed 0.0.3

### Major Changes

- Optimisation parameters were changed from options to arguments to
  enable better reproducibility of designs
  ([\#65](https://github.com/biometryhub/speed/issues/65)
- Enabled one stage MET designs
- Added contributing guide and code of conduct
  ([\#59](https://github.com/biometryhub/speed/issues/59))

See changelog for further details.

## speed 0.0.2

### Major Changes

- Enabled more complex designs and added some vignettes with examples
  and detailed use.

See changelog for further details.

## speed 0.0.1

First version.
