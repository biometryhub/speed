# Changelog

## speed 0.0.11

### Major Changes

- Added the `linked_cols` argument to
  [`speed()`](https://biometryhub.github.io/speed/reference/speed.md),
  naming columns that should be rearranged along with the `swap` column,
  for example a `variety_name` label belonging to a numeric `variety`
  code. For hierarchical designs, pass a named list to link different
  columns at different levels.
  ([\#105](https://github.com/biometryhub/speed/issues/105))
- [`speed()`](https://biometryhub.github.io/speed/reference/speed.md)
  now stops as soon as a design reaches the lowest score its layout
  allows, applicable only to the default
  [`objective_function()`](https://biometryhub.github.io/speed/reference/objective_functions.md).
  This can be turned off per level with
  `optim_params(stop_at_optimal = FALSE)`.
  [`summary()`](https://rdrr.io/r/base/summary.html) now reports the
  lower bound score alongside the achieved one.

### Minor Changes

- The startup version check now only runs in interactive sessions. Set
  the `SPEED_NO_VERSION_CHECK` environment variable to disable it
  entirely.
- [`summary()`](https://rdrr.io/r/base/summary.html) now reports why
  each level stopped - the optimum was reached, no further improvement
  was found, no swap was possible, or the iteration cap was hit. The
  reason is also recorded as `stop_reason` in each level’s metadata.
- [`speed()`](https://biometryhub.github.io/speed/reference/speed.md)
  now warns when a `swap_all = TRUE` group holds no two treatments of
  equal replication, and stops a level immediately when no group in it
  can be swapped.

### Bug Fixes

- `optim_params(random_initialisation = )` no longer fails with
  “Duplicate (row, col) coordinates” on multi-site designs with
  `grid_factors$by`.
- [`speed()`](https://biometryhub.github.io/speed/reference/speed.md) no
  longer fails with “supplied seed is not a valid integer” when `seed`
  is left unset in a session that has not yet used the random number
  generator and `random_initialisation` is toggled on.
- `optim_params(random_initialisation = )` no longer moves plots holding
  no treatment.
- `optim_params(random_initialisation = )` now respects
  `swap_all = TRUE`, so it no longer breaks up the wholeplots or strips
  of a split-plot or strip-plot design.
- [`speed()`](https://biometryhub.github.io/speed/reference/speed.md)
  now gives a clear error when `grid_factors` is malformed, instead of
  failing with “missing value where TRUE/FALSE needed”.
- A two sided formula passed to `spatial_factors` is now rejected
  instead of silently accepted, and hierarchical designs now run the
  same `spatial_factors`, `iterations` and `seed` checks as simple ones.
- Named lists for hierarchical arguments such as `iterations` are now
  split per level under the `optimise` argument, as they already were
  when `swap` was a named list. A level the list leaves out now takes
  that argument’s own default rather than the default for
  `spatial_factors`.
- Columns that take no part in the optimisation are no longer converted
  to factors and back, so a class that cannot be rebuilt with
  `as.<class>()`, such as `Date`, is now returned unchanged instead of
  as `character`.
  ([\#122](https://github.com/biometryhub/speed/issues/122))
- Each level of a hierarchical design now starts from the best design
  found so far, rather than whichever design the previous level’s search
  last accepted. A level could otherwise be optimised, and reported on,
  against a design that was never returned, or replace an earlier
  level’s better result with a worse one.
- A design that ties the best score found now replaces it, so a search
  that ends on a score plateau returns a random arrangement from that
  plateau rather than the systematic input it was given. Early stopping
  still counts only strict improvements.
- The swapped treatments passed to an objective function are now labels
  rather than factor codes, so
  [`objective_function_piepho()`](https://biometryhub.github.io/speed/reference/objective_function_piepho.md)
  updates the plots that actually moved.

## speed 0.0.10

### Major Changes

- Added a [`summary()`](https://rdrr.io/r/base/summary.html) method for
  `"design"` objects, reporting structure and replication, a decomposed
  optimisation score, and design-quality diagnostics.
  ([\#73](https://github.com/biometryhub/speed/issues/73))
- `grid_factors` gains an optional `by` element naming the column that
  separates a design into several grids,
  e.g. `list(dim1 = "row", dim2 = "col", by = "site")` for a
  multi-environment trial. Each grid is scored on its own.

### Minor Changes

- Designs whose `row`/`col` columns are not numeric, or where two plots
  share a coordinate, now fail with a message naming the problem.

### Bug Fixes

- Design metrics are now built from each plot’s `row`/`col` coordinates
  rather than the order of the rows in the data frame. Designs generated
  with
  [`objective_function_piepho()`](https://biometryhub.github.io/speed/reference/objective_function_piepho.md)
  should be regenerated.
- Multi-site designs are no longer scored as one pooled grid, which
  discarded plots whose coordinates collided and counted adjacencies
  between sites. Use `grid_factors$by` to name the grouping column.
- [`objective_function_piepho()`](https://biometryhub.github.io/speed/reference/objective_function_piepho.md)
  now scores evenness of distribution per grid and reports each grid
  separately. A grid with no treatment replicated within it contributes
  `0` rather than `Inf`.
- [`calculate_efficiency_factor()`](https://biometryhub.github.io/speed/reference/calculate_efficiency_factor.md)
  now errors for a design whose treatment contrasts are not estimable,
  instead of returning an impossible value above 1. The row-column model
  gained an intercept, which does not change results that were already
  valid.
- [`summary()`](https://rdrr.io/r/base/summary.html) no longer errors on
  designs that cannot be placed on a single grid; the affected
  diagnostics report why they are unavailable instead.
- [`calculate_nb()`](https://biometryhub.github.io/speed/reference/calculate_nb.md)
  no longer errors on designs with missing plots when `pair_mapping` is
  not supplied.
- [`calculate_adjacency_score()`](https://biometryhub.github.io/speed/reference/calculate_adjacency_score.md)
  now recycles a single `ring_weights` value across every entry of
  `ring_dists`, so the default is usable with more than one ring.
- `swap_all = TRUE` no longer changes the replication of a design when
  an earlier level has unbalanced a swap group mid-search. Only
  treatments with matching replication are exchanged.

## speed 0.0.9

### Major Changes

- Deprecated the `splits` argument of
  [`initialise_design_df()`](https://biometryhub.github.io/speed/reference/initialise_design_df.md)
  in favor of
  [`initialise_split_design_df()`](https://biometryhub.github.io/speed/reference/initialise_split_design_df.md).
  Passing `splits` now warns with the equivalent suggested call.

### Bug Fixes

- [`speed()`](https://biometryhub.github.io/speed/reference/speed.md)
  now errors when `swap_all = TRUE` is used on a design with unequal
  within-group replication, instead of silently swapping treatments with
  different replication counts.
- [`speed()`](https://biometryhub.github.io/speed/reference/speed.md) no
  longer returns numeric/integer columns (e.g. `treatment`, `row`,
  `col`) as their internal factor level codes instead of their original
  values.
- [`speed()`](https://biometryhub.github.io/speed/reference/speed.md) no
  longer emits a “Setting row names on a tibble is deprecated” warning
  when passed a tibble.
- [`speed()`](https://biometryhub.github.io/speed/reference/speed.md)
  now accepts designs with `vctrs`-backed multi-class columns (e.g. from
  the `edibble` package) instead of erroring; such columns are now
  returned as `character`.

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
