# Implementation Plan: `.origin` index + `keep_with` — Keep Companion Columns Linked During Randomisation

## Feature request

Users often have a treatment column plus one or more **companion columns** that belong
with that treatment — e.g. `trt` holds `1:N` while `trtname` holds
`"Variety 1", ..., "Variety N"`. Currently `speed()` rearranges only the `swap` column,
so companion columns stay put by row position and the linkage silently breaks. This adds:

1. An always-present **`.origin`** index column recording where each final plot's unit
   came from, and
2. A **`keep_with`** convenience argument that uses `.origin` to bring named companion
   columns forward automatically.

## Mental model

In speed the rows never move — `row`/`col` stay put and *treatment values* migrate
between rows during swapping. So the useful thing to record is, per plot, **which
original unit landed here**.

## Key insight

Companion columns don't affect scoring (the objective functions read only the `swap`
column plus spatial factors). So we never touch the scoring path. We instead track a
single internal index through the swaps and use it to reconstruct anything afterward.

Because the index lives *inside* `design_df`, it rides along with accept/reject and the
best-design snapshot for free — no separate state threading.

## Prior art

`odw::odw()` (David Butler, Univ. of Wollongong) has the same concept as its `reorder`
argument:

> *A numeric or character vector identifying any columns in `data` that are to be permuted
> (at the termination of the search) in design order, parallel to the objective factor
> given in `permute`.*

This validates our approach — reorder companion columns **at the end** of the search to
line up with the optimised objective factor, rather than dragging them through it. Naming
note: odw's *objective factor* is `permute` (speed's is `swap`), and odw *also* has a
separate `swap` argument meaning something else, so the surrounding names can't fully
align. We deliberately use the speed-native name **`keep_with`** for intra-package
consistency (`swap` / `swap_within` take column names with no suffix), accepting the loss
of cross-package familiarity with `reorder`.

## Decisions (confirmed)

- **Primitive:** an internal integer index column, carried in lockstep with the `swap`
  value through every swap. Not user-supplied; our own bookkeeping.
- **Column name:** `.origin`.
- **Direction:** *source* — for each final plot, `.origin` is the **input row index** of
  the unit now occupying that plot. Makes rematch a one-line index: `original[.origin, ]`.
  **This must be stated explicitly in the documentation.**
- **Input-order referenced:** stamp `.origin` from the user's input row order *before*
  speed's internal row/col sort, so it refers to rows as the user passed them.
- **Always returned:** `.origin` is included in the returned `design_df` as the general
  escape hatch, whether or not `keep_with` is used.
- **`keep_with`:** ergonomic layer on top; takes a **vector of column names**; speed does
  the `original[.origin, cols]` rejoin and returns those columns already in place.
- **No non-functional error:** indexing original rows works for functional *and*
  per-instance columns, so `keep_with` needs no functional-dependence check. Validate only
  that named columns exist and do not overlap swap / swap_within / spatial-factor columns.

## Worked example

Input (with the internal `.origin` stamped `1:N`):

```
.origin  row col  trt  trtname
1        1   1    A    Variety A
2        2   1    B    Variety B
3        1   2    C    Variety C
4        2   2    D    Variety D
```

`.origin` is carried in lockstep with `trt` through every swap. After optimisation:

```
row col  trt  .origin
1   1    C    3
2   1    A    1
1   2    D    4
2   2    B    2
```

Row 1: the plot at (row 1, col 1) now holds the unit that started at input row **3**
(C / "Variety C"). Rematch any original column with a single index:

```r
final$trtname <- original$trtname[final$.origin]
# keep_with does exactly this, for the named columns, automatically.
```

## Steps

### 1. `R/speed.R` — stamp, strip, re-attach
- **Stamp** `.origin <- seq_len(nrow(data))` immediately, *before* the row/col sort at
  `speed.R:190`, so it references the user's input order.
- Strip any `keep_with` companion columns from `data` before the SA loop so they never
  enter SA / `random_initialise` / objective functions. (`.origin` stays in and is
  carried through the swaps.)
- **Re-attach** before the `to_types` return (~`speed.R:220-221`): for each `keep_with`
  column, `design_df[[col]] <- original[[col]][design_df$.origin]`. Restore original
  types (fold companion classes into `factored$input_types`). Preserve column order.
- Keep `.origin` in the returned `design_df`.

### 2. `R/design_utils.R` — carry `.origin` in lockstep
- In `generate_single_swap_neighbour` (`design_utils.R:34`) and
  `generate_multi_swap_neighbour` (`design_utils.R:94`), whenever the `swap` column value
  moves between positions, move `.origin` the same way.
- In `shuffle_items` (`design_utils.R:539`), apply the same permutation to `.origin` that
  is applied to the `swap` column.
- These are the only hot-loop edits. They must be provably score-neutral and seed-stable.

### 3. `R/utils.R` — merge `keep_with` per level
- Add `keep_with` to `speed_args` in `create_speed_input` (`utils.R:130`) so it merges
  per-level like `swap_within` etc. A bare character vector attaches to the single/first
  level.

### 4. `R/verify_utils.R` — `.verify_keep_with(data, swap, swap_within, spatial_factors, keep_with)`
- Named columns exist (reuse `verify_column_exists`).
- Companion columns disjoint from every `swap`, `swap_within`, and spatial-factor column.
- Also guard against a user column literally named `.origin` (collision) — error or
  reserve the name.
- Call from `.verify_speed_inputs` and `.verify_hierarchical_inputs`, and validate when
  `optimise` is supplied.
- No functional-dependence check.

### 5. Docs
- roxygen `@param keep_with` on `speed()` with the `trt`/`trtname` example.
- Document the new `.origin` column in `@returns`, **explicitly stating the source
  direction** (final plot -> input row it came from) and how to use it to rematch
  arbitrary columns.
- Run `devtools::document()`.
- New bullet at top of `NEWS.md`.
- Optionally a short section in the relevant vignette.

### 6. Tests — `tests/testthat/test-keep-with.R`
- `.origin` present in output; values are a valid permutation of `1:N`.
- `original[final$.origin, ]` reconstructs the pre-optimisation unit for each plot.
- `keep_with`: every companion value matches its treatment after optimisation; correct
  type; original column order.
- Companion column is genuinely permuted (not left in place) relative to input.
- **Per-instance (non-functional) column** is carried correctly via `.origin` — the case
  a plain join could not handle.
- Hierarchical design: `.origin` consistent across levels; per-level `keep_with`.
- `keep_with` supplied via the `optimise` list.
- Error cases: non-existent column; companion overlapping a swap/spatial column; user
  column named `.origin`.
- **Score-neutrality / reproducibility:** same seed with vs without `keep_with` yields an
  identical design and identical scores (proves the lockstep edits don't perturb SA).

## Files touched

- `R/speed.R` — stamp `.origin`, strip, re-attach, keep in output
- `R/design_utils.R` — carry `.origin` in the two neighbour generators + `shuffle_items`
- `R/utils.R` — `create_speed_input` merge of `keep_with`
- `R/verify_utils.R` — `.verify_keep_with` + call sites
- `man/speed.Rd` — regenerated via `devtools::document()`
- `NEWS.md`
- `tests/testthat/test-keep-with.R`

## Edge cases

- **`NA` swap values / buffers:** the row keeps its own `.origin`; companion becomes the
  original value for that row. Add an explicit test.
- **`.origin` name collision:** reserve the name; error if the user already has a column
  called `.origin`.
- **Hierarchical / multi-level:** a single `.origin` tracks physical-unit provenance
  across all levels, since all swaps move values within the same `design_df`.
- **No changes** to the objective-function contract or `optim_params`; hot-loop edits are
  limited to moving one extra column in lockstep.
