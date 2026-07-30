# Test coverage gaps

Handoff document for a follow-up branch. Everything listed here is **outside** the scope of
`feature/summary` — the summary method and its periphery are now at 100%.

Generated on 2026-07-29 against `feature/summary` at `c22c17a` + the coverage tests added on that
branch.

## How to reproduce these numbers

`covr` is not in `Suggests`, so install it first:

```r
install.packages("covr")
```

Then:

```r
cov <- covr::package_coverage(".")
covr::percent_coverage(cov)
covr::report(cov)          # interactive, browsable
covr::zero_coverage(cov)   # just the uncovered lines
```

Caveat when running on a host R rather than the devcontainer: `devtools::test()` may report
`Deleting unused snapshots` and remove `tests/testthat/_snaps/initialise_design_df/*.svg`, and
rewrite `_snaps/speed.md` line endings. That is a vdiffr/ggplot2 version artefact of the host, not a
real change — `git checkout -- tests/testthat/_snaps/` afterwards.

## Current state

Overall: **98.79%** (was 97.16% before the `feature/summary` coverage work).

| File | Expressions | Uncovered | Coverage |
| --- | --- | --- | --- |
| R/design_utils.R | 292 | 14 | 95.21% |
| R/utils.R | 85 | 1 | 98.82% |
| R/plotting.R | 101 | 1 | 99.01% |
| R/metrics.R | 178 | 1 | 99.44% |
| R/buffers.R | 76 | 0 | 100% |
| R/calculate_adjacency_score.R | 56 | 0 | 100% |
| R/optim_params.R | 11 | 0 | 100% |
| R/speed.R | 146 | 0 | 100% |
| R/summary.R | 272 | 0 | 100% |
| R/verify_utils.R | 138 | 0 | 100% |
| R/zzz.R | 12 | 0 | 100% |

17 uncovered expressions remain. Every trigger condition below was verified empirically against the
package before being written down — the "verified" note records what actually happened, so these are
recipes rather than guesses.

---

## 1. `R/design_utils.R` — neighbour generation (3 expressions)

These are the "nothing to swap" defensive branches. All three are reachable by calling the internal
generators directly, which is the cheapest way to test them deterministically — driving them through
`speed()` would depend on which group `sample()` happens to pick.

Suggested new file: `tests/testthat/test-generate_neighbour.R` (there is no test file for these
functions at present, which is why the gaps clustered here).

### 1a. Line 74 — `to_be_swapped <- NULL` (single swap, group holds one treatment)

`generate_single_swap_neighbour()` picks two random plots in a group. If they hold the same treatment
it looks for a plot with a *different* treatment; when the group is uniform there is none, so the
swap is abandoned.

Trigger: every group holds exactly one distinct treatment (so the branch fires whichever group
`sample()` picks), with at least 2 plots per group.

```r
d <- data.frame(
  row = rep(1:4, each = 2),
  col = rep(1:2, 4),
  block = factor(rep(1:2, each = 4)),
  treatment = factor(rep(c("A", "B"), each = 4))
)
res <- generate_single_swap_neighbour(d, "treatment", "block", 1, FALSE)
expect_identical(res$design$treatment, d$treatment)   # design unchanged
expect_true(all(res$swapped_items == ""))             # nothing recorded as swapped
```

*Verified:* design unchanged, `swapped_items` all empty.

Worth asserting the design is returned **unchanged** rather than just that no error occurs — the
point of the branch is that a uniform group is a no-op, not a corrupted swap.

### 1b. Line 102 — `groups_to_swap <- groups` (multi swap, all blocks)

The `swap_all_blocks = TRUE` arm of `generate_multi_swap_neighbour()`. The equivalent line in
`generate_single_swap_neighbour()` (line 43) *is* covered, so only the multi-swap variant is missing.

```r
d <- data.frame(
  row = rep(1:4, each = 2),
  col = rep(1:2, 4),
  block = factor(rep(1:2, each = 4)),
  treatment = factor(rep(c("A", "B"), 4))
)
res <- generate_multi_swap_neighbour(d, "treatment", "block", 1, TRUE)
expect_equal(sum(res$swapped_items != ""), 4)   # both blocks swapped, 2 items each
```

*Verified:* 4 non-empty entries in `swapped_items`, i.e. both blocks were touched — which is the
actual claim (contrast with `swap_all_blocks = FALSE`, where only one block is).

Note `swap_all_blocks` reaches these functions from the deprecated `options(speed.swap_all_blocks =)`
via `generate_neighbour()`'s default. Testing the generator directly sidesteps the deprecation
warning.

### 1c. Line 123 — `next` (multi swap, group has fewer than 2 treatments)

Same shape as 1a but on the multi-swap path: the group has ≥2 plots but <2 *distinct* treatments.
Reuses the uniform-group frame from 1a:

```r
res <- generate_multi_swap_neighbour(d_uniform, "treatment", "block", 1, TRUE)
expect_true(all(res$swapped_items == ""))
expect_identical(res$design$treatment, d_uniform$treatment)
```

*Verified:* no swaps performed, design unchanged.

---

## 2. `R/design_utils.R` — `infer_row_col()` (1 expression)

### Line 199 — the "used as row and column" message on the *pattern-inference* path

`infer_row_col()` has two `message()` calls saying the same thing: line 171 for the explicit
`grid_factors` path (covered) and line 199 for the fallback that greps column names against
`^row(s|)$` / `^(col(umn|)|range)(s|)$` (uncovered). The gap is that every non-quiet test happens to
supply `grid_factors`.

Trigger: `quiet = FALSE`, and the default `grid_factors` must *not* match, so at least one axis has
to be found by pattern instead. A `row`/`range` layout is the realistic case:

```r
expect_message(
  res <- infer_row_col(data.frame(row = 1, range = 1), quiet = FALSE),
  "row and range are used as row and column"
)
expect_true(res$inferred)
expect_equal(res$col, "range")
```

*Verified:* emits `row and range are used as row and column, respectively.` and returns
`list(inferred = TRUE, row = "row", col = "range")`.

Do **not** pass `grid_factors = NULL` to reach this — `infer_row_col()` dereferences
`grid_factors$dim1` unguarded and errors on `NULL`. See §6.

`grep`-inferring `col` from `range` is worth an assertion in its own right: it means a column called
`range` is silently treated as the design's column axis.

---

## 3. `R/design_utils.R` — `apply_splits()` and the deprecated `splits` argument (8 expressions)

Lines 351–353, 379, 387–395. All sit behind the **deprecated** `splits` argument of
`initialise_design_df()`, which emits a deprecation warning and directs users to
`initialise_split_design_df()`. Tests need `suppressWarnings()` or `expect_warning()`.

> Judgement call for whoever picks this up: this is deprecated code on the way out. Covering it is
> cheap (recipes below, all verified), but an equally defensible outcome is to wrap the deprecated
> `splits` path in `# nocov start` / `# nocov end` and let the coverage number reflect only supported
> code. Recommend covering it while it still ships, since it is still reachable by users.

### 3a. Lines 351–353 — `apply_splits()` with no block structure

The `else` arm taken when `block_nrows` is `NULL`; the covered arm is the blocked one.

```r
suppressWarnings(
  df <- initialise_design_df(
    items = paste0("T", 1:4), nrows = 4, ncols = 4,
    splits = list(wp = list(nrows = 2, ncols = 2))
  )
)
expect_true("wp" %in% names(df))
expect_equal(length(unique(df$wp)), 4)   # 4x4 grid / 2x2 units = 4 whole plots
```

*Verified:* returns columns `row, col, treatment, wp`.

### 3b. Line 379 — numeric scalar `items` in a split expands to `T1..Tn`

```r
suppressWarnings(
  df <- initialise_design_df(
    items = paste0("T", 1:4), nrows = 4, ncols = 4,
    splits = list(wp = list(nrows = 2, ncols = 2, items = 4))
  )
)
expect_setequal(unique(df$wp_treatment), paste0("T", 1:4))
```

*Verified:* `wp_treatment` takes values `T1,T2,T3,T4`.

### 3c. Lines 387–395 — `items` length neither equals nor divides the child count

```r
expect_error(
  suppressWarnings(initialise_design_df(
    items = paste0("T", 1:4), nrows = 4, ncols = 4,
    splits = list(wp = list(nrows = 2, ncols = 2, items = c("a", "b", "c")))
  )),
  "must have length 4 \\(or divide it\\); got 3"
)
```

*Verified:* errors with `` `items` for split `wp` must have length 4 (or divide it); got 3 ``.

Also worth adding a *passing* case for the recycling branch immediately above it (a divisor length,
e.g. `items = c("a", "b")` against 4 children) to pin down that recycling happens once per parent
unit — that line is currently covered but the behaviour is untested.

---

## 4. `R/design_utils.R` — `random_initialise()` (1 expression)

### Line 638 — early return when a random shuffle scores exactly 0

`random_initialise()` shuffles up to `random_initialisation` times and keeps the best; a score of
exactly 0 is optimal, so it returns immediately without trying the rest.

The reliable, deterministic trigger is to make *every* score 0 by zeroing both weights, rather than
hoping a shuffle finds a perfect layout:

```r
r <- speed(
  data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  ),
  swap = "treatment", spatial_factors = ~ row + col,
  iterations = 10, seed = 1, quiet = TRUE,
  optimise_params = optim_params(
    random_initialisation = 3, adj_weight = 0, bal_weight = 0
  )
)
expect_equal(r$score, 0)
```

*Verified:* `speed()` returns a design with score 0.

A caveat to note in the test comment: zero weights make the whole objective identically 0, so this
exercises the early-return mechanism rather than a genuinely perfect design. If a test that shows a
*real* perfect design is preferred, a small design where a shuffle can reach 0 under normal weights
would be better — but it will be seed-sensitive, so pin the seed and assert `r$score == 0`.

---

## 5. `R/design_utils.R` — `.verify_initialise_design_df()` (4 expressions)

Four validation errors, all one-liners. Add to `tests/testthat/test-initialise_design_df.R`.

| Line | Condition | Message |
| --- | --- | --- |
| 663 | no `nrows`/`ncols` **and** no `designs` | `Either `nrows` and `ncols` or `designs` must be provided` |
| 671 | `items` `NULL` and `splits` `NULL` | `` `items` must be provided when `splits` is `NULL` `` |
| 729 | unrecognised argument inside `splits$<name>` | `` `bogus` is an invalid argument in `splits$wp` `` |
| 733 | a split missing `nrows`/`ncols` | `` `nrows` and `ncols` must be provided for split `wp` `` |

```r
expect_error(
  initialise_design_df(items = 1:4),
  "Either `nrows` and `ncols` or `designs` must be provided"
)
expect_error(
  initialise_design_df(nrows = 2, ncols = 2),
  "`items` must be provided when `splits` is `NULL`"
)
expect_error(
  initialise_design_df(
    items = paste0("T", 1:4), nrows = 4, ncols = 4,
    splits = list(wp = list(nrows = 2, ncols = 2, bogus = 1))
  ),
  "`bogus` is an invalid argument in `splits\\$wp`"
)
expect_error(
  initialise_design_df(
    items = paste0("T", 1:4), nrows = 4, ncols = 4,
    splits = list(wp = list(nrows = 2))
  ),
  "`nrows` and `ncols` must be provided for split `wp`"
)
```

*Verified:* all four produce exactly the messages above. Note validation runs before the `splits`
deprecation warning, so the last two need no `suppressWarnings()`.

---

## 6. `R/utils.R` — `create_speed_input()` (1 expression) — **latent bug, not just a coverage gap**

### Line 214 — per-level `grid_factors` in a legacy hierarchical call

```r
grid_factors = if (is.list(grid_factors[[1]])) {
  grid_factors[[optimise_name]] %||% .DEFAULT$grid_factors   # <- line 214
} else {
  grid_factors
}
```

This branch exists to let a legacy hierarchical call (`swap = list(wp = ..., sp = ...)`) give each
level its own `grid_factors`, e.g.:

```r
grid_factors = list(
  wp = list(dim1 = "range", dim2 = "plot"),
  sp = list(dim1 = "range", dim2 = "plot")
)
```

**It cannot be reached through `speed()`.** `speed()` calls `infer_row_col(data, grid_factors, quiet)`
at [R/speed.R:187](R/speed.R#L187) *before* `create_speed_input()`, and `infer_row_col()`
dereferences `grid_factors$dim1` at [R/design_utils.R:167](R/design_utils.R#L167). For a
list-of-lists, `grid_factors$dim1` is `NULL`, so `NULL %in% names(layout_df)` is `logical(0)` and the
`&&` fails.

*Verified:* the call above fails with `missing value where TRUE/FALSE needed` — an opaque internal
error, not a helpful validation message.

*Verified:* calling `create_speed_input()` directly with the same list-of-lists works correctly —
each level gets its own `grid_factors`, and a level omitted from the list falls back to
`.DEFAULT$grid_factors` (`row`/`col`) via the `%||%`.

So the feature is implemented one layer down but unreachable from the public entry point. **Decide
first, then test** — the coverage gap is a symptom:

1. **Support it** (the line's evident intent): teach `speed()` to resolve per-level `grid_factors`
   before `infer_row_col()`, or make `infer_row_col()` handle a list-of-lists. Then the test is an
   end-to-end `speed()` call asserting each level used its own axes. Note the new `optimise =`
   argument already supports per-level `grid_factors`, so this only matters for the legacy
   `swap = list(...)` shape.
2. **Reject it cleanly**: add a `.verify_*` check that errors with a clear message pointing at
   `optimise =`, test that message, and mark line 214 `# nocov`.

Either way `infer_row_col()` should not fall over with `missing value where TRUE/FALSE needed`; a
guard there is worth adding regardless of which route is chosen.

---

## 7. `R/plotting.R` — `autoplot.design()` (1 expression)

### Lines 130–134 — `verify_column_exists()` for an explicitly supplied `block` column

The block column is only validated when the user passes `block` *and* no column name matches it.

```r
r <- speed(
  data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    treatment = rep(LETTERS[1:3], 4)
  ),
  swap = "treatment", spatial_factors = ~ row + col,
  iterations = 20, seed = 1, quiet = TRUE
)
expect_error(
  autoplot(r, block = "nope"),
  "'nope' not found in row, col, treatment"
)
```

*Verified:* errors with `'nope' not found in row, col, treatment. Please specify the appropriate
column using the 'block' argument.`

Add to `tests/testthat/test-plotting.R`. The error is raised eagerly by `autoplot()` itself, so the
plot does not need to be printed.

---

## 8. `R/metrics.R` — `calculate_efficiency_factor()` (1 expression) — **effectively unreachable**

### Line 719 — `ZtZ_inv <- pseudo_inverse(ZtZ)`

```r
condition_number <- kappa(ZtZ)
if (condition_number > 1e12) {
  ZtZ_inv <- pseudo_inverse(ZtZ)   # <- line 719
} else {
  ZtZ_inv <- solve(ZtZ)
}
```

This is the near-singular fallback, and **no valid input appears to reach it**:

- `Z`'s columns are grid-row indicators (`1..n_rows-1`) and grid-column indicators
  (`1..n_cols-1`), which are linearly independent for any `n_rows`, `n_cols` ≥ 1. So `ZtZ` is
  positive definite whenever the function gets that far.
- The `Z`-filling loop iterates `n_rows * n_cols` times and indexes rows of `Z` sequentially, so any
  input with `n_rows * n_cols > n_plots` errors with a subscript-out-of-bounds *before* reaching
  `kappa()`. Rank deficiency by dimension is impossible: `(n_rows-1) + (n_cols-1) < n_rows * n_cols`
  always holds.
- *Verified* `kappa(ZtZ)` growth: 9.3 for 3×4, 81 for 10×10, 1.48e3 for 50×50, 5.1e3 for 100×100 —
  roughly `0.5 * n_plots`. Reaching the `1e12` threshold would need on the order of 1e12 plots.
  A 1×1 grid errors (`'a' is 0-diml`) rather than producing an ill-conditioned matrix.

This function's signature was changed on `feature/summary` (added `row_column`/`col_column`), which
is why it is mentioned here, but the uncovered line predates that and is not summary-related.

Recommended action — pick one, both are code changes rather than tests:

1. Mark it `# nocov` with a comment recording why (a defensive guard against a condition the current
   `Z` construction cannot produce).
2. Or drop the branch and call `pseudo_inverse(ZtZ)` unconditionally. `pseudo_inverse()` is already
   used unconditionally on `A_RC` twelve lines later, and it agrees with `solve()` on a
   well-conditioned matrix, so this simplifies the function and removes the untestable branch. Needs
   a regression check that `calculate_efficiency_factor()` values are unchanged across the existing
   tests — including the snapshot tests in `tests/testthat/_snaps/`.

Option 2 is the tidier outcome; option 1 is the safe minimum.

---

## Suggested ordering

1. **§6 first** — it is a genuine bug with a design decision attached, and the decision determines
   whether a test or a `# nocov` is the right answer.
2. **§8** — also a decision (`# nocov` vs. removing the branch) rather than a test.
3. **§1, §5, §7** — plain missing tests, no code changes, ~30 lines total. Biggest coverage return
   for the effort (8 of the 17 expressions).
4. **§2, §4** — one test each, both verified above.
5. **§3 last** — deprecated surface; decide cover-vs-`nocov` before writing anything.

Clearing §1–§7 takes the package to ~99.4%; §8 as well takes it to 100%.

## Housekeeping

Delete this file (or add it to `.Rbuildignore`) before release — it is not in `.Rbuildignore`, and
root-level `.md` files ship in the built package. Previous plan documents on this repo followed the
same convention of living at the root and being removed once actioned.
