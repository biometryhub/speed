# Implementation Plan: `linked_cols` — Keep Companion Columns Linked During Randomisation

## Feature request

Users often have a treatment column plus one or more **companion columns** that belong
with that treatment — e.g. `trt` holds `1:N` while `trtname` holds
`"Variety 1", ..., "Variety N"`. Currently `speed()` rearranges only the `swap` column,
so companion columns stay put by row position and the linkage silently breaks.

`linked_cols` takes a vector of column names. Those columns are **silently swapped
along with the `swap` column** and returned in the updated order, in their original
position and type. No new columns appear in the output.

## Mental model

In speed the rows never move — `row`/`col` stay put and *treatment values* migrate
between rows during swapping. So the useful thing to record is, per plot, **which
original unit landed here**. That is tracked by an internal index column, which is
**stripped before returning** — it is bookkeeping, not output.

## Key insight

Companion columns don't affect scoring (the objective functions read only the `swap`
column plus spatial factors). So we never touch the scoring path. We instead track a
single internal index through the swaps and use it to reorder the companion columns
afterward.

Because the index lives *inside* `design_df`, it rides along with accept/reject and the
best-design snapshot for free — no separate state threading.

## Prior art

`odw::odw()` (David Butler, Univ. of Wollongong) has the same concept as its `reorder`
argument:

> *A numeric or character vector identifying any columns in `data` that are to be permuted
> (at the termination of the search) in design order, parallel to the objective factor
> given in `permute`.*

This validates the approach — permute companion columns **at the termination of the
search**, rather than dragging them through it. odw's *objective factor* is `permute`
(speed's is `swap`), and odw *also* has a separate `swap` argument meaning something
else, so the surrounding names can't align. We use **`linked_cols`**, which says what
the argument does (these columns are linked to `swap`) without colliding with speed's
existing `swap` / `swap_within` vocabulary.

## Decisions (confirmed)

- **Primitive:** an internal integer index column, carried in lockstep with the `swap`
  value through every swap. Not user-supplied; our own bookkeeping.
- **One index per level, not one per design.** Each level's SA run only ever modifies
  *its own* `swap` column, so each level applies an independent permutation. A single
  shared index can only follow one of them. See "Hierarchical designs" below.
- **Only for levels that use it.** An index column is created only for a level that has
  `linked_cols`. A call without `linked_cols` adds no columns and executes exactly the
  code path it does today, so score-neutrality for existing users is structural rather
  than something to be argued.
- **Internal only — never returned.** Index columns are stripped from `design_df`
  before the function returns, exactly like the existing `dummy_<timestamp>` column
  (`speed.R:220`). Users see only their own columns, reordered.
- **Name:** generated at runtime as `.origin_<level>_<timestamp>`, following the
  `dummy_<timestamp>` convention already used in `speed()`. Because the names are unique
  per call and never surface, **there is no collision case to validate and no reserved
  name** — this removes an error path the earlier plan needed.
- **Direction (internal semantics):** *source* — for each final plot the index holds the
  **input row number** of the unit now occupying that plot, so the rejoin is a one-line
  index: `companions[[col]][idx]`.
- **Input-order referenced:** stamp the index from the user's input row order *before*
  speed's internal row/col sort (`speed.R:190`), so it refers to rows as the user passed
  them.
- **`linked_cols`:** takes a **vector of column names**. Columns are stripped before the
  SA loop, reordered by the index afterward, restored to their original type, and put
  back in their original column position.
- **Validation:** named columns exist, and are disjoint from every `swap`, `swap_within`,
  and spatial-factor column.

## Worked example

Input:

```
row col  trt  trtname
1   1    A    Variety A
2   1    B    Variety B
1   2    C    Variety C
2   2    D    Variety D
```

`speed(data, swap = "trt", linked_cols = "trtname")` returns:

```
row col  trt  trtname
1   1    C    Variety C
2   1    A    Variety A
1   2    D    Variety D
2   2    B    Variety B
```

`trtname` has travelled with `trt`. Column order, column names, and `trtname`'s type are
unchanged from the input; no index column is present.

## Hierarchical designs (split-plot, strip-plot, MET)

`linked_cols` is **per level**, and takes the same three input shapes as `swap` /
`swap_within` — `create_speed_input` already merges arguments this way, so no new
mechanism is needed.

Split-plot with a companion for each level:

```r
speed(
  data,
  swap        = list(wp = "irrigation",  sp = "variety"),
  swap_within = list(wp = "block",       sp = "wholeplot"),
  linked_cols = list(wp = "irr_label",   sp = "variety_name")
)
```

Only the sub-plot level needs a companion — omit the other name entirely:

```r
linked_cols = list(sp = "variety_name")
```

Several companions on one level — the per-level value is itself a vector:

```r
linked_cols = list(wp = "irr_label", sp = c("variety_name", "seed_lot"))
```

Via the `optimise` argument, `linked_cols` sits inside the level like any other field:

```r
optimise = list(
  wp = list(swap = "irrigation", swap_within = "block",     linked_cols = "irr_label"),
  sp = list(swap = "variety",    swap_within = "wholeplot", linked_cols = "variety_name")
)
```

A bare character vector in a hierarchical call attaches to the first level, matching how
the other scalar arguments already behave.

**Why per-level, and why the level names must match.** `speed_hierarchical` runs one SA
pass per level, and `generate_neighbour` only ever assigns to `design[[opt$swap]]`. So
the whole-plot pass permutes `irrigation` and nothing else; the sub-plot pass permutes
`variety` and nothing else. These are two independent permutations of the same data
frame. `irr_label` must follow the first, `variety_name` the second — one shared index
would be overwritten by the second pass and would silently mis-order the whole-plot
companion. Hence one index column per level with `linked_cols`, each swapped only during
its own level's pass.

Validation should therefore also reject `linked_cols` level names that don't appear in
`swap` / `optimise`, rather than silently dropping them.

## Steps

### 1. `R/speed.R` — strip, stamp, reorder, re-attach, drop index

Ordering matters here; the sequence is:

1. Validate (including `.verify_linked_cols`).
2. `infer_row_col()` — unchanged.
3. **Strip** companions *before* `to_factor()`: with `all_linked` the union of every
   level's `linked_cols`, keep `companions <- data[all_linked]` aside, then
   `data[all_linked] <- NULL`. Stripping before `to_factor` keeps them out of
   `factored$input_types` and avoids a pointless factor round-trip. Also record
   `input_col_order <- names(data)` (pre-strip) for step 7.
4. `to_factor(data)` — unchanged.
5. **Stamp one index per level that has `linked_cols`**, *after* `to_factor` (so they
   stay integer and never enter `input_types`) but *before* the row/col sort at
   `speed.R:190` (so they reference input order). The sort then carries them along:
   ```r
   for (level in names(optimise)) {
     if (length(optimise[[level]]$linked_cols)) {
       origin_col <- paste0(".origin_", level, "_", as.integer(Sys.time()))
       optimise[[level]]$origin_col <- origin_col
       data[[origin_col]] <- seq_len(nrow(data))
     }
   }
   ```
   This has to run after `create_speed_input` (`speed.R:199`) so the per-level
   `linked_cols` are resolved, but the stamp must precede the sort — so either move the
   sort down or compute the level set before sorting. Flagging it because the current
   ordering in `speed()` puts the sort at `:190` and `create_speed_input` at `:199`.
6. SA loop — unchanged apart from step 2 below.
7. **Re-attach** after the loop, before the `to_types` call at `speed.R:221`: for each
   level, and each of that level's companions,
   `design_df[[col]] <- companions[[col]][design_df[[origin_col]]]`, using **that level's**
   index. Companion values keep their original type because they never went through
   `to_factor`. Then reorder columns to `input_col_order` — companions are re-appended at
   the end otherwise.
8. **Drop the indices**: `design$design_df[[origin_col]] <- NULL` for every level,
   alongside the existing `dummy_group` strip at `speed.R:220`.

Stripping (rather than carrying companions through the loop) matters for speed: the hot
loop does `new_design <- design` on every iteration, so every extra column is copied per
iteration.

### 2. `R/design_utils.R` — carry the index in lockstep

The active level's index column has to reach the swap functions. Thread `origin_col`
through `generate_neighbour` → `generate_single_swap_neighbour` /
`generate_multi_swap_neighbour`, and through `shuffle_items`, as an extra argument
alongside `swap` / `swap_within`, defaulting to `NULL`. When it is `NULL` every one of
these functions behaves exactly as it does today. In `random_initialise`
(`design_utils.R:563`) the per-level loop already iterates `optimise`, so each
`shuffle_items` call passes `opt$origin_col` and each index follows its own swap column.

- `generate_single_swap_neighbour` (`design_utils.R:34`): the swap is
  `new_design[[swap]][rev(swap_pair)] <- to_be_swapped` (`:80`) — a clean 2-element
  positional exchange. Apply the identical `rev(swap_pair)` exchange to the index column.
  Well-defined and exact.
- `generate_multi_swap_neighbour` (`design_utils.R:94`): not a positional exchange — it
  moves whole treatment-label sets (`:131-136`). Exchange the index values between the
  two sets position-wise, recycling with `rep_len()` when the sets differ in length:
  ```r
  o1 <- new_design[[origin_col]][plots_1]
  o2 <- new_design[[origin_col]][plots_2]
  new_design[[origin_col]][plots_1] <- rep_len(o2, length(plots_1))
  new_design[[origin_col]][plots_2] <- rep_len(o1, length(plots_2))
  ```
  Recycling is arbitrary at the unit level, which is why multi-swap levels are guarded by
  the functional-dependence check in step 4 — see "Multi-swap semantics" below.
- `shuffle_items` (`design_utils.R:539`): currently `design[...][[swap]] <- sample(items)`.
  Change to capture the permutation and apply it to both columns:
  ```r
  perm  <- sample(seq_along(items))
  items <- items[perm]
  ```
  **RNG-stability:** `sample(x)` for a length-*n* vector is `x[sample.int(n)]`, so
  `sample(seq_along(items))` consumes exactly the same random numbers as
  `sample(items)`. Existing seeds therefore reproduce bit-for-bit. This must be asserted
  by a test, not assumed.
- These are the only hot-loop edits. They must be provably score-neutral and seed-stable.

### 3. `R/utils.R` — merge `linked_cols` per level
- Add `linked_cols` to `speed_args` in `create_speed_input` (`utils.R:130`) so it merges
  per-level like `swap_within` etc. A bare character vector attaches to the single/first
  level.
- Per-level values are **vectors**, not scalars, unlike `swap` / `swap_within`. Check
  that the merge logic in `create_speed_input` doesn't assume length-1 per level — if it
  does (e.g. via `[[1]]` or a `vapply` over levels), `linked_cols` needs handling that
  preserves the vector.
- Add `linked_cols = NULL` and an internal `origin_col` slot to `.DEFAULT`
  (`R/constants.R`) so levels without companions have well-defined fields.
- `speed()` needs the **union** of all levels' `linked_cols` for the strip in step 1.3,
  since the strip happens once for the whole call. The same column may legitimately be
  listed on only one level; it must not be double-stripped or double-attached.

### 4. `R/verify_utils.R` — `.verify_linked_cols(data, swap, swap_within, spatial_factors, linked_cols)`
- Named columns exist (reuse `verify_column_exists`).
- Companion columns disjoint from every `swap`, `swap_within`, and spatial-factor column
  — across *all* levels, not just the level the companion is declared on.
- In a hierarchical call, `linked_cols` level names must match the `swap` / `optimise`
  level names; error on an unknown level rather than silently ignoring it.
- The same column listed on two different levels is an error — two levels would reorder
  it independently and the second would win.
- Call from `.verify_speed_inputs` and `.verify_hierarchical_inputs`, and validate when
  `optimise` is supplied.
- No `.origin` collision check needed — the index name is generated per call and never
  returned.
- **Functional-dependence check, multi-swap levels only.** For any level where
  `swap_all` selects `generate_multi_swap_neighbour`, each of that level's companions
  must map one-to-one from the swap value: for every `swap` value, exactly one distinct
  companion value. Error otherwise, naming the offending column and an example swap value
  with its conflicting companion values. Single-swap levels are not checked — per-instance
  companions are legitimate there. See "Multi-swap semantics" below for why.
- The check reads only the input data, so it costs one `tapply`-style pass per companion
  and runs before any optimisation work.

### 5. Docs
- roxygen `@param linked_cols` on `speed()` with the `trt`/`trtname` example. State that
  the columns are reordered to travel with `swap`, keep their type and position, and that
  they take no part in scoring.
- `@returns` needs **no** new column documented — that's the point of keeping the index
  internal.
- Document the multi-swap restriction: on a multi-swap level, a companion column must be
  functionally dependent on that level's `swap` column (one companion value per treatment
  value). Frame it as the natural case — `irrigation` / `irr_label` — not as a limitation,
  and note that per-plot companions are supported on single-swap levels.
- Run `devtools::document()`.
- New bullet at top of `NEWS.md`.
- Optionally a short section in the relevant vignette.

### 6. Tests — `tests/testthat/test-linked-cols.R`
- Companion value matches its treatment for every plot after optimisation.
- Companion column keeps its original **type** and its original **column position**.
- No index column leaks into the output — assert
  `!any(grepl("^\\.origin", names(result$design_df)))` and that
  `names(design_df) == names(input)`.
- Companion column is genuinely permuted (not left in place) relative to input.
- Multiple `linked_cols` at once.
- **Split-plot, companion on both levels** — the test that would have caught the
  single-index bug: `linked_cols = list(wp = "irr_label", sp = "variety_name")`, then
  assert *both* companions still match their own treatment column. A shared index passes
  the sub-plot assertion and fails the whole-plot one.
- **Split-plot, companion on one level only** (`linked_cols = list(sp = ...)`), and the
  reverse (`wp` only).
- `linked_cols` supplied via the `optimise` list.
- Error cases: non-existent column; companion overlapping a swap/`swap_within`/spatial
  column; unknown level name; same column listed on two levels.
- **Score-neutrality / reproducibility:** same seed with vs without `linked_cols` yields
  an identical design and identical scores — this is the test that guards the
  `shuffle_items` and neighbour-generator edits.
- **`NA` swap values / buffers:** a row with `NA` treatment keeps its own index, so its
  companion is its own original value. Explicit test.
- **Per-instance (non-functional) companion on a single-swap level** — carried correctly;
  the case a value-lookup could not handle.
- **Per-instance companion on a multi-swap level** — errors, with a message naming the
  column and the conflicting values.
- **Functionally-dependent companion on a multi-swap level** (whole-plot `irr_label`) —
  accepted, and correct regardless of how `rep_len` recycles.

## Multi-swap semantics (decided: recycle + guard)

`generate_multi_swap_neighbour` does **not** exchange two units. It exchanges two
treatment *labels* across whole sets of plots (`design_utils.R:131-136`):

```r
plots_1 <- which(group_filter & new_design[[swap]] == swap_pair[1])
plots_2 <- which(group_filter & new_design[[swap]] == swap_pair[2])
new_design[[swap]][plots_1] <- swap_pair[2]
new_design[[swap]][plots_2] <- swap_pair[1]
```

`plots_1` and `plots_2` can differ in length (nothing enforces equal replication within
a group). When they do, **there is no unit-level bijection**, so a per-unit provenance
index cannot be carried in lockstep — the concept isn't defined for that swap. The
earlier plan's claim that per-instance companion columns are supported everywhere does
not hold for multi-swap levels.

This is not a corner case: the whole-plot level of a split-plot is exactly this. A
whole-plot treatment spans every sub-plot row beneath it, so `plots_1` is a set of rows,
not a single row. The saving grace is that whole-plot companions are functionally
dependent by nature — every row with `irrigation == "drip"` has the same `irr_label` —
so the *useful* case is always well-defined even though the general one isn't.

**Resolution.** Exchange index values position-wise with `rep_len()` recycling (step 2),
and have `.verify_linked_cols` require functional dependence on multi-swap levels only
(step 4). Functionally-dependent companions are then correct no matter how recycling
lands, because every row in `plots_1` shares one treatment value and therefore one
companion value. Per-instance companions keep full support on single-swap levels, where
provenance is exact. The combination means there is no input that produces a
plausible-looking but arbitrary result — it is either exact or a clear error.

Rejected alternatives: recycling without the guard (silently arbitrary values for a
per-plot companion on a whole-plot level), and dropping the index for a pure
`swap`-value → companion-value lookup (simpler and hot-loop-free, but gives up
per-instance companions everywhere).

Three ways to resolve it:

- **A. Recycle, document the limit.** Exchange index values position-wise, using
  `rep_len()` when the sets differ in length. Functionally-dependent companions (the
  motivating `trt`/`trtname` case) are always correct. Per-instance companions are
  documented as guaranteed only under single-swap. Cheapest; one soft caveat in the docs.
- **B. Recycle + guard.** As A, but `.verify_linked_cols` additionally checks functional
  dependence (`swap` value → exactly one companion value) *for multi-swap levels only*,
  and errors otherwise. No silent wrong answers; costs one validation pass and a clearer
  error message.
- **C. Value-lookup only.** Drop the index entirely. Build a `swap` value → companion
  value map from the input and apply it after the search. Zero hot-loop edits, trivially
  seed-stable, works for every swap mode — but requires functional dependence
  *everywhere*, so per-instance companions are not supported at all.

**Recommendation: B.** It keeps per-instance support where it is genuinely well-defined
(single-swap, the common case), and turns the ill-defined case into a clear error rather
than a plausible-looking wrong answer.

## Files touched

- `R/speed.R` — strip companions, stamp index, reorder + re-attach, drop index
- `R/design_utils.R` — thread `origin_col` through `generate_neighbour`, the two
  neighbour generators, `shuffle_items` and `random_initialise`
- `R/utils.R` — `create_speed_input` merge of `linked_cols`
- `R/constants.R` — `linked_cols` / `origin_col` defaults
- `R/verify_utils.R` — `.verify_linked_cols` + call sites
- `man/speed.Rd` — regenerated via `devtools::document()`
- `NEWS.md`
- `tests/testthat/test-linked-cols.R`

## Edge cases

- **`NA` swap values / buffers:** the row keeps its own index; companion becomes the
  original value for that row.
- **Index column type:** it is stamped after `to_factor`, so it is the only non-factor
  column in `design_df` during the loop. Verify nothing in the SA path assumes all
  columns are factors — the swap generators only touch `design[[swap]]` /
  `design[[swap_within]]`, and objective functions read `swap` plus spatial factors, so
  this should hold, but confirm before relying on it.
- **Column order:** companions are stripped and re-appended, so the original column order
  must be restored explicitly or they end up at the right-hand end.
- **Hierarchical / multi-level:** one index **per level**. Each level's SA pass writes
  only its own `swap` column, so the permutations are independent and a shared index
  would be clobbered by the later level. (The earlier version of this plan asserted the
  opposite; it was wrong.)
- **No changes** to the objective-function contract or `optim_params`; hot-loop edits are
  limited to moving one extra column in lockstep.
