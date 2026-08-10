# speed 0.0.11

## Major Changes

- `speed()` now stops as soon as a design reaches the lowest score its layout allows, applicable only to the
  default `objective_function()`. This can be turned off per level with `optim_params(stop_at_optimal =
  FALSE)`. `summary()` now reports the lower bound score alongside the achieved one.
- `objective_function_piepho()`, `calculate_ed()` and `calculate_nb()` now implement the ED and NB criteria as
  defined by Piepho et al. (2018, 2021), verified against the published statistics for Figure 1 of the 2018
  paper. Scores are not comparable with earlier versions.
- `objective_function_piepho()` no longer adds the adjacency and balance scores to its score, which is now the
  ED and NB criteria alone plus a penalty on same-item adjacencies. Use `objective_function()` for adjacency
  and balance.
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
- `calculate_nb()` no longer returns `NA` for `var` on a two-treatment design, and ignores adjacencies involving
  plots with no treatment.
- Minimum spanning trees are now correct when two plots holding the same item share a position; `igraph` reads a
  distance of 0 as an absent edge.
- The neighbour generators no longer report empty strings as swapped items when a swap has to be skipped.

# speed 0.0.10

## Major Changes

- Added a `summary()` method for `"design"` objects, reporting structure and replication, a
  decomposed optimisation score, and design-quality diagnostics.
  ([#73](https://github.com/biometryhub/speed/issues/73))
- `grid_factors` gains an optional `by` element naming the column that separates a design into
  several grids, e.g. `list(dim1 = "row", dim2 = "col", by = "site")` for a multi-environment trial.
  Each grid is scored on its own.

## Minor Changes

- Designs whose `row`/`col` columns are not numeric, or where two plots share a coordinate, now fail
  with a message naming the problem.

## Bug Fixes

- Design metrics are now built from each plot's `row`/`col` coordinates rather than the order of the
  rows in the data frame. Designs generated with `objective_function_piepho()` should be regenerated.
- Multi-site designs are no longer scored as one pooled grid, which discarded plots whose coordinates
  collided and counted adjacencies between sites. Use `grid_factors$by` to name the grouping column.
- `objective_function_piepho()` now scores evenness of distribution per grid and reports each grid
  separately. A grid with no treatment replicated within it contributes `0` rather than `Inf`.
- `calculate_efficiency_factor()` now errors for a design whose treatment contrasts are not
  estimable, instead of returning an impossible value above 1. The row-column model gained an
  intercept, which does not change results that were already valid.
- `summary()` no longer errors on designs that cannot be placed on a single grid; the affected
  diagnostics report why they are unavailable instead.
- `calculate_nb()` no longer errors on designs with missing plots when `pair_mapping` is not
  supplied.
- `calculate_adjacency_score()` now recycles a single `ring_weights` value across every entry of
  `ring_dists`, so the default is usable with more than one ring.
- `swap_all = TRUE` no longer changes the replication of a design when an earlier level has
  unbalanced a swap group mid-search. Only treatments with matching replication are exchanged.

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
