# Implementation Plan: `keep_with` — Keep Companion Columns Linked During Randomisation

## Feature request

Users often have a treatment column plus one or more **companion columns** that are
attributes of that treatment — e.g. `trt` holds `1:N` while `trtname` holds
`"Variety 1", "Variety 2", ..., "Variety N"`. Currently `speed()` rearranges only the
`swap` column, so companion columns stay put by row position and the linkage silently
breaks. This adds a `keep_with` argument so companion columns follow their treatment.

## Key insight

A companion column like `trtname` is a **pure function of** the swap column
(`7 -> "Variety 7"` everywhere), and the objective functions only read the `swap`
column plus the spatial factors — **companion columns never enter scoring**.

Two consequences:

1. Swapping a companion column in lockstep during SA produces *exactly* the same design
   as re-deriving it once at the end. There is no optimisation difference.
2. Therefore we do **not** touch the hot SA loop. We strip companion columns before the
   loop and re-derive them from the final `swap` values on the way out. This is provably
   equivalent, cheaper, and lower-risk.

## Decisions (confirmed)

- **Approach:** post-hoc reattachment (not lockstep swapping, not composite key).
- **API:** new `keep_with` argument on `speed()`.
- **Non-functional mapping** (same `swap` value maps to two different companion values):
  **error out** with a clear message, matching the existing `.verify_*` style.

## API

Add `keep_with = NULL` to `speed()`. Accepts:

- A character vector of column names (simple designs): `keep_with = c("trtname")`.
- A named list keyed by level, mirroring `swap`, for hierarchical designs:
  `keep_with = list(wp = "wp_name", sp = "sp_name")`.
- Also settable per-level inside the `optimise` list.

Each named entry ties its companion columns to that level's `swap` column.

## Steps

### 1. `R/verify_utils.R` — new `.verify_keep_with(data, swap, keep_with)`
- Each named group's columns exist in `data` (reuse `verify_column_exists`).
- Companion columns are disjoint from every `swap`, `swap_within`, and spatial-factor
  column (cannot keep-with a column that SA moves or scores).
- **Functional-dependence check:** for each `(swap_col, companion)` verify every `swap`
  value maps to exactly one companion value, e.g.
  `tapply(companion, swap_col, \(x) length(unique(x)))` all `== 1`. On failure `stop()`
  naming the offending swap value(s).
- Call from both `.verify_speed_inputs` and `.verify_hierarchical_inputs`, and validate
  when `optimise` is supplied.

### 2. Normalise `keep_with` into the per-level structure
- In `create_speed_input` (`R/utils.R`), add `keep_with` to `speed_args` so it merges
  per-level exactly like `swap_within` etc. A bare character vector attaches to the
  single/first level; a named list distributes by name.

### 3. `speed()` — build the mapping and strip columns (`R/speed.R`)
- After `to_factor`/sorting, collect all companion columns across levels into one lookup
  keyed by the relevant `swap` column (distinct `swap` value -> companion value(s), plus
  original type from `factored$input_types`).
- Remove companion columns from `data` before `create_speed_input` / `speed_hierarchical`
  so they never enter SA, `random_initialise`, or the objective functions.

### 4. Re-attach after optimisation (before `to_types`, ~`speed.R:220-221`)
- For each level, left-join the stored lookup onto `design$design_df` by the `swap`
  column to reconstruct each companion column in its optimised position.
- Restore original types — fold the companion columns' original classes back into
  `factored$input_types` so the existing `to_types` call handles them. Preserve column
  order so output matches input.

### 5. Docs
- roxygen `@param keep_with` on `speed()` with the `trt`/`trtname` example; run
  `devtools::document()`.
- New bullet at top of `NEWS.md`.
- Optionally a short section in the relevant vignette.

### 6. Tests — `tests/testthat/test-keep-with.R`
- Simple design: every row's `trtname` matches the `trt -> trtname` map; column present,
  correct type, original order.
- Companion column is genuinely permuted (not left in place) relative to input.
- Hierarchical design with per-level `keep_with`.
- `keep_with` supplied via the `optimise` list.
- Error cases: non-existent column; companion overlapping a swap/spatial column;
  **non-functional mapping** errors with a clear message.
- Equivalence check: result with `keep_with` is identical (same seed) to manually
  re-joining after a plain `speed()` run — proves no effect on optimisation.

## Files touched

- `R/speed.R` — argument, strip, re-attach
- `R/utils.R` — `create_speed_input` merge
- `R/verify_utils.R` — `.verify_keep_with` + call sites
- `man/speed.Rd` — regenerated via `devtools::document()`
- `NEWS.md`
- `tests/testthat/test-keep-with.R`

## Edge cases

- **`NA` swap values:** companion becomes `NA` on re-join — matches current buffer/empty
  cell handling; add an explicit test.
- **MET where a swap value legitimately differs per site:** not non-functional if the
  companion is truly an attribute; if a user hits the error, the real fix is a
  composite/site-scoped key — surface this hint in the error message.
- **No changes** to the neighbour generators, objective-function contract, or
  `optim_params` — keeps the risk surface minimal.
