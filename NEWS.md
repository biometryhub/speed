# speed 0.0.10

## Major Changes

- `objective_function_piepho()`, `calculate_ed()` and `calculate_nb()` now implement the ED and NB criteria as
  defined by Piepho et al. (2018, 2021), verified against the published statistics for Figure 1 of the 2018
  paper. Scores are not comparable with earlier versions.
- Neighbour balance is now counted along rows for designs with more columns than rows, and along columns when
  there are more rows, following Piepho et al. (2021); previously both directions were always counted. Override
  with `directions` in `calculate_nb()` or `nb_directions` in `objective_function_piepho()`.
- `calculate_nb()` additionally returns Piepho's `s2` neighbour balance score and a `self_adjacencies` count;
  `objective_function_piepho()` gains `self_adj_weight` to penalise the latter.
- `calculate_ed()` is substantially faster: minimum spanning trees over fewer than 20 points are computed
  directly rather than via `igraph`, taking one iteration of `objective_function_piepho()` on a 25-treatment
  design from roughly 18 ms to 1 ms.

## Bug Fixes

- `calculate_ed()` no longer fails with `object 'trt_groups' not found`, which had made every call to it and to
  `objective_function_piepho()` error.
- `calculate_ed()` now reports `MST_i` as the mean rather than the total edge length of the minimum spanning
  tree, so that it is comparable across treatments with different numbers of replications.
- `calculate_nb()` now counts treatment pairs that never occur as neighbours, and excludes self-adjacencies from
  the neighbour balance variance. Previously a design could improve its score by making pairs disappear rather
  than by balancing them.
- `calculate_nb()` no longer returns `NA` for `var` on a two-treatment design, generates a `pair_mapping` when
  none is supplied, and ignores adjacencies involving plots with no treatment.
- Minimum spanning trees are now correct when two plots holding the same item share a position; `igraph` reads a
  distance of 0 as an absent edge.
- The neighbour generators no longer report empty strings as swapped items when a swap has to be skipped.

# speed 0.0.9

## Major Changes

- Deprecated the `splits` argument of `initialise_design_df()` in favor of `initialise_split_design_df()`.
  Passing `splits` now warns with the equivalent suggested call.

## Bug Fixes

- `speed()` now errors when `swap_all = TRUE` is used on a design whose treatments are unequally replicated
  within a swap group, instead of silently changing the replication. Such a swap exchanges every plot of one
  treatment with every plot of another, so when the two treatments occupy different numbers of plots they
  exchange replication counts and the returned design is not a rearrangement of the input. Designs with equal
  within-group replication, which is what `swap_all` is intended for, are unaffected.
- `speed()` no longer returns numeric and integer columns as their factor level codes; a `treatment` column of
  `c(10, 100, 30, 9)` was previously returned as `c(2, 4, 3, 1)`. Numeric `row` and `col` values other than
  `1:n` were affected in the same way.
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

- Optimisation parameters were changed from options to arguments to enable better reproducibility of designs (#65)
- Enabled one stage MET designs
- Added contributing guide and code of conduct (#59)

See changelog for further details.

# speed 0.0.2

## Major Changes

- Enabled more complex designs and added some vignettes with examples and detailed use.

See changelog for further details.


# speed 0.0.1

First version. 
