# Review notes: grid construction and core metrics

**Scope:** `R/design_utils.R` (`build_design_matrix()`), `R/calculate_adjacency_score.R`,
`R/metrics.R`, and `R/summary.R` where it consumes a grid. Branch **`bugfix/grid-orientation`** off `main`.

G12 and G13 touch `R/summary.R` but belong **here, not in `REVIEW-NOTES-SUMMARY.md`** (Sam, 2026-08-06):
this branch made the grid contract strict, so it owns the consequences of that strictness.

**Companion files** — one per workstream:

| File | Workstream |
|---|---|
| `REVIEW-NOTES.md` | `feature/incidence` (PR #97) — `R/incidence.R` |
| `REVIEW-NOTES-SUMMARY.md` | the merged `summary()` work — `R/summary.R` |
| `REVIEW-NOTES-EFFICIENCY.md` | efficiency-factor statistics — needs a new branch |
| `REVIEW-NOTES-PR91.md` | PR #91 `info-objective` |
| **this file** | grid construction / core metrics |

Anything needing a branch of its own has been moved out: the A-efficiency upper bound and the missing
intercept to `REVIEW-NOTES-EFFICIENCY.md`; the S3 class collision, the `initialise_design_df()` fill
order and the now-redundant row-major sort to `KNOWN_ISSUES.md` (#2, #3, #4).

**Last verified:** 2026-08-06, R 4.6.1, `pkgload::load_all()`, branch at `f5d68f5`. All numbers
measured, not inferred. Resolved findings are deleted rather than annotated — see git history and the
`NEWS.md` entries for what they were.

> ✅ **All grid-ordering work has landed.** `build_design_matrix()` is wired into
> `calculate_adjacency_score()`, `objective_function_piepho()` and `.neighbour_balance()`;
> `calculate_efficiency_factor()` indexes its own indicator matrices by coordinate. Full suite:
> **1668 pass, 0 fail, 0 warn.**
>
> ✅ **G10 is withdrawn, not fixed.** It said `build_design_matrix()` must **rank** its coordinates to
> satisfy D6. That was the wrong lever: ranking is nothing more than the inverse of the displacement
> `add_buffers()` applies, and inverting it downstream by inference also collapses genuine holes. D6's
> *rationale* stands; its mechanism moved to `feature/buffers`, which undoes the displacement where it
> is created. `build_design_matrix()` stays **raw** — see D6 and A5.
>
> 📦 **`feature/buffers` merges into this branch** (branched off `f5d68f5`). It carries the coordinate
> restoration that makes raw correct, plus the `add_buffers()` deprecation. See A2.1 — nothing here
> should be actioned without reading it, because two items in this file are already done there.
>
> 🔴 **Blocker: G13 — nothing in speed represents a design that occupies more than one grid, so MET is
> broken.** `initialise_design_df(designs = )` reuses `row`/`col` per site, so every MET design has
> duplicate coordinates. `main` coped by silently discarding **30 of 80** plots; this branch errors
> instead. Neither works, and coordinate construction did not cause it — see A3. Grid metrics need a
> grouping dimension.
>
> ✅ **G12 has landed.** `summary()` now reports a grid metric as unavailable with a reason instead of
> erroring, via `.single_grid()` over `grid_index()`'s classed conditions. It does **not** make MET work —
> it stops `summary()` dying and stops `.efficiency_factor()` reporting `1.855529` — so G13 stands. See
> A1.2.
>
> ✅ **G11 has landed.** The grid-construction hot-loop cost this branch introduced is gone: coordinate
> validation is split into `grid_index()` and hoisted once per run, grid build is back to parity with the
> `matrix()` reshape it replaced, and whole `speed()` runs are 10-28% faster with bit-identical scores.
> See A1.1.
>
> ⬜ **Moved out, each needing its own branch:** the A-efficiency upper bound and the missing intercept
> (`REVIEW-NOTES-EFFICIENCY.md`); the S3 class collision, the `initialise_design_df()` fill order, the
> redundant row-major sort and the incremental-grid-mutation option (`KNOWN_ISSUES.md` #2, #3, #4, #6).

---

## A1. Landed on this branch

Kept as a one-line inventory for the PR description. The full write-ups are in git history.

| Was | Now |
|---|---|
| **G1** four functions each assumed a data ordering, two row-major and two column-major | all four read coordinates via `build_design_matrix()` or a coordinate-indexed fill |
| **G2** `objective_function_piepho()` wrote a column-major flattened grid back over the treatment column | write-back deleted; all four score components computed on the real layout, and piepho is order-invariant |
| **G3** `build_design_matrix()` didn't validate coordinates | explicit non-numeric / non-positive-integer / duplicate-coordinate errors |
| **G4** `.calculate_nb()` errored on sparse grids, the default path | `NA` neighbours are skipped, matching the `pair_mapping` path |
| **G5** a scalar `ring_weights` errored against multi-ring `ring_dists` | recycled across every ring |
| **G6** `calculate_efficiency_factor()` couldn't compute for a buffered design (tracked as `KNOWN_ISSUES` #1b, since removed) | resolved as a side effect of G7 — coordinate indexing absorbs the offset `add_buffers()` introduces, and a genuinely holed grid computes too; verified 2026-08-06 |
| **G7** `calculate_efficiency_factor()` filled `Z` positionally, returning a different value per row ordering (0.111 vs 0.625 on a 2×6, and values `> 1`) | `Z` is indexed by each plot's own coordinates |
| **S1** `.neighbour_balance()` reported self-adjacencies that didn't exist (6 where the truth was 0) | reads coordinates; the 4×3 fixture now returns the hand-derived truth (self 0, pair min/max 5/6) |
| **A5** lexical factor levels (`1, 10, 11, 2, …`) defeated the row-major sort, so grid metrics scored a layout that wasn't the design | coordinate construction is immune; the sort is no longer load-bearing (`KNOWN_ISSUES.md` #4) |
| **G11** coordinate validation ran on every iteration, making grid construction 16× the `matrix()` reshape it replaced | validation split into `grid_index()`, hoisted once per `speed()` run; grid build back to parity with `matrix()`, whole runs 10-28% faster — see A1.1 |
| **G12** `summary()` died outright on any design that couldn't be gridded — a MET design or non-numeric `row`/`col` labels — because `.neighbour_balance()` let `build_design_matrix()`'s error escape | `.single_grid()` gates `.neighbour_balance()`, `.efficiency_factor()` and `.replicate_spans()`, each reporting a reason; `has_grid` now means "reportable as one grid", so `layout` stops claiming an `nrow` x `ncol` that holds fewer plots than the design — see A1.2 |

### A1.1 G11 — validation hoisted out of the annealing loop

`build_design_matrix()` gained an optional `index =` argument; the coercion and validation half moved
into `grid_index()`, which returns the index and grid dimensions. `speed_hierarchical()` builds one per
run and threads it to the objective functions, `calculate_adjacency_score()` and
`objective_function_piepho()`.

**Built lazily, on purpose.** `grid_index()` is wrapped in `tryCatch` so a design whose coordinates
cannot form a grid — MET duplicates (G13), non-numeric labels, or no grid columns at all — still runs if
its objective never needs a grid, and still raises the same error from the same place if it does.
`index = NULL` reproduces the old behaviour exactly. Verified against a pre-fix worktree: a two-site
duplicate-coordinate design with `adj_weight = 0` runs and scores 4.000 both before and after.

**Measured 2026-08-06**, grid build on 700 plots (28×25), 2000 reps:

| | µs/build | vs `matrix()` |
|---|---|---|
| `matrix()` — what `main` did | 15 | 1.00× |
| `build_design_matrix()`, no index | 240 | 16.00× |
| `build_design_matrix()`, index supplied | **15** | **1.00×** |

End-to-end, benchmarked against a clean worktree at `69c516d`. **Scores are bit-identical in every
case** — this is a performance change only:

| | before | after | |
|---|---|---|---|
| `objective_function`, 700 plots, 2000 iters | 1.58 s | **1.14 s** | −28% |
| `objective_function`, 700 plots, 5000 iters | 3.89 s | **2.86 s** | −26% |
| `objective_function`, 120 plots, 5000 iters | 2.46 s | **2.17 s** | −12% |
| `objective_function_piepho`, 120 plots, 1000 iters | 1.64 s | **1.47 s** | −10% |

Larger designs gain most, since grid construction is a bigger share of each iteration. The earlier
prediction that hoisting would land *below* `matrix()` did not hold — it lands at parity, because the
`as.character()` coercion of the treatment column stays per-iteration and is what remains of the cost.
That coercion is *not* hoistable the way validation was: the swap column is the one thing annealing
mutates, so each call re-does the level lookup and allocates a fresh length-`n` character vector. The
below-parity figure came from a micro-benchmark that pre-coerced the column outside the timing loop, which
only holds if nothing is being optimised. Removing that last cost means carrying a mutable grid across
iterations — recorded as `KNOWN_ISSUES.md` #6, deliberately not bundled here because it changes the
objective-function contract this work stayed additive to.

`grid_index()` also gained a missing-column check, found by this work: a design with no grid columns
reached `max()` with empty vectors and produced `-Inf` dimensions plus two warnings.

### A1.2 G12 — `summary()` reports a reason instead of dying

Before: `summary()` on a MET design, or on one with non-numeric `row`/`col` labels, errored outright —
both optimise fine with `adj_weight = 0`, so the design object was valid but not summarisable. `main`
returned numbers for both, wrong ones via the truncation in G13, so this branch had narrowed what
`summary()` accepted.

`.single_grid(df, rc, cc)` returns `TRUE` or a short reason, and gates `.neighbour_balance()`,
`.efficiency_factor()` and `.replicate_spans()`. Each now reports `available = FALSE` with that reason,
which the print method already handled.

**It delegates rather than re-implementing the coordinate rules.** The obvious build was a second copy of
`grid_index()`'s four checks, which is exactly the drift risk worth avoiding — G11 had just made
`grid_index()` the single place that decides what a valid grid is. Instead `grid_index()` signals *which*
rule failed by condition class (`speed_grid_missing` / `_nonnumeric` / `_notinteger` / `_duplicate`, all
inheriting `speed_grid_error`, via `.grid_stop()`), and `.single_grid()` maps the class to a short reason.
No message-text matching, no duplicated rules, and the happy path is untouched — conditions are only
constructed on failure.

Three things fell out that were not in the G12 write-up:

| | |
|---|---|
| `.efficiency_factor()` needed the gate for correctness, not just to avoid an error | it does not error on duplicate coordinates — it pools the grids and returns **1.855529**, impossible for an efficiency factor. Pinned by a test that asserts the underlying `> 1` behaviour, so the reason the gate exists cannot quietly disappear |
| `.replicate_spans()` was also wrong for MET, not merely noisy | it pooled sites, making two sites' row 3 one plot apart. Gating it fixes that and removes the two leaked `NAs introduced by coercion` warnings at the same time |
| `layout` claimed an impossible shape | `has_grid` now means "reportable as **one** grid", so a MET design reports `80 plots` rather than `10 rows x 5 cols (80 plots)` — a grid holding 50. New `layout$grid_reason` carries why. `nrow`/`ncol` are `NA` unless `has_grid`, which is what the existing non-grid test already asserted |

One test changed rather than being added: the `.efficiency_factor()` "computation fails" case used a 1×1
grid holding three plots, which the new gate catches earlier as duplicate coordinates. Its `tryCatch`
backstop is now covered by `local_mocked_bindings()` instead, which tests the intent directly.

Reasons are phrased as facts, not interpretations — `duplicate row/col coordinates (e.g. a multi-site
design)` rather than `spans multiple grids`, because a malformed design looks identical to a MET one.

**Coverage added** (`test-summary.R`), where there was none — `grep site tests/testthat/test-summary.R`
previously returned nothing, which is why the suite passed while `summary()` was broken for MET:
`.single_grid()` unit cases including a genuine hole and lexical factor levels, which must *not* be
refused; a MET design through `speed()` → `summary(efficiency = TRUE)`; the `> 1` efficiency pin;
non-numeric labels asserted warning-free; and a split-plot asserting the gate does **not** withhold
metrics from a legitimate hierarchical design at either level.

Suite after: **1728 pass, 0 fail, 0 error, 0 warn.**

---

## A2. Decisions

### 🔷 D6. Do buffers break adjacency? — **settled: buffers never reach the metrics** (2026-08-06)

**Answer: a buffered design must score exactly as the same design unbuffered.** Plots either side of a
buffer **are** neighbours.

**Rationale (Sam):** when a buffered trial is *analysed*, the buffer plots are excluded and the model is
fitted on the remaining plots, which treats them as a contiguous grid — adjacent even where they are
physically separated. A design metric should describe the layout the analysis will see, not the physical
field.

**Mechanism — decided twice, and the second answer is the one to keep.** The question was originally
framed as raw-vs-ranked coordinates inside `build_design_matrix()`, and answered "ranked". That framing
was wrong. `add_buffers()` displaces the real plots' coordinates to make room (`row + 1` for `"edge"`,
`row * 2` for `"row"`, `3 * row - 1` for `"double row"`, …) and never undoes it. Ranking is *exactly the
inverse of that displacement* — verified for all five buffer types, ranking the de-buffered coordinates
restores the original `1..n` precisely. So ranking was never a statistical position on buffers; it was
an undo, applied by inference, in the wrong place.

Inferring it downstream also cannot tell a buffer from a real hole, so it collapses genuine physical
gaps — a road, an irregular trial edge — for no benefit.

**So: the displacement is undone where it is created.** `add_buffers()` records what it did in
`metadata$buffer`, and `.drop_buffer_rows()` inverts it before any metric runs (`feature/buffers`, A2.1).
`build_design_matrix()` keeps **raw** coordinates. That satisfies D6 *and* preserves real gaps —
strictly better than ranking, which bought the first at the cost of the second.

Measured on a 4×3 design with the restoration in place: `"edge"`, `"row"`, `"col"`, `"double row"` and
`"double col"`, and stacked combinations, all reproduce the unbuffered design's neighbour balance,
replicate span and efficiency exactly.

**Longer term this stops being speed's problem at all.** `add_buffers()` is deprecated as of 0.0.10 and
moving to \pkg{biometryassist} (see `BUFFERS-HANDOFF.md` in that repo). Once it is gone, speed never creates a
displacement, so the restoration goes too and raw coordinates are simply correct with nothing to undo.

This also settles S3 in `REVIEW-NOTES-SUMMARY.md`: `main`'s `length(unique(...))` behaviour was the
right *behaviour*; the defect was that the choice had never been stated.

G4's `NA` tolerance stays necessary regardless — a design with a genuine partial hole still produces a
sparse grid under raw coordinates.

### 🔶 D7. What should the grid metrics report for a multi-grid (MET) design? — **open**

Blocks the last part of G13. Adjacency and neighbour balance answer themselves — they count edges, edges
never cross a grid boundary, and summing per grid is exact (measured: 20 + 30 = 50). Two components do
not follow:

| Component | Question |
|---|---|
| `calculate_efficiency_factor()` | An efficiency factor is a property of one experiment's information matrix; there is no meaningful sum. Options: (a) report per-site values, (b) fit one model with site effects added, (c) declare it unavailable for multi-grid designs. |
| `objective_function_piepho()`'s ED | NB sums like any edge count. ED measures evenness of a distribution, so per-grid-then-averaged and pooled are different quantities and the paper's definition assumes a single trial. |

Note (c) is not merely the conservative option — it is currently *required* as an interim state either
way, because the alternative is continuing to return `1.855529` for a quantity bounded above by 1.

Whatever is chosen, the same answer should apply to `summary()`'s `efficiency` entry and to
`.neighbour_balance()`, so the two never disagree about what a MET design's diagnostics mean.

### A2.1 What arrives when `feature/buffers` merges

Branched off `f5d68f5`, so it applies cleanly. Two items below are already done there — **do not action
them again**:

| From `feature/buffers` | Effect here |
|---|---|
| `metadata$buffer` transform record in `add_buffers()`, inverted by `.drop_buffer_rows()` / `.restore_buffer_coords()` | makes D6 true without touching `build_design_matrix()` |
| `test-summary.R` buffer test rewritten | **fixes the stale comment at [test-summary.R:304](tests/testthat/test-summary.R#L304)**, which still claims a `"row"` buffer should change the counts. Now asserts every buffer type and stacked combinations match the unbuffered design |
| `add_buffers()` deprecation warning + `## Deprecations` NEWS section | buffers are leaving speed; the biometryassist repo's `BUFFERS-HANDOFF.md` specifies that side |
| `.warn_if_buffers()` in `calculate_adjacency_score()`, `calculate_balance_score()`, `calculate_efficiency_factor()` | a direct metric call on a buffered frame bypasses `.drop_buffer_rows()`, so it warns rather than silently scoring the displaced layout |
| `helper-buffers.R` with `add_buffers_quiet()`, and 45 rewritten test call sites | keeps the deprecation warning out of tests that are about layout |

One caveat carried forward: the `metadata$buffer` record is an affine `scale`/`shift` pair, which covers
speed's buffer types but **cannot** represent biometryassist's `by =` block buffers, where gaps appear
only at group boundaries. It would need to become a per-axis `new -> old` lookup if speed ever had to
invert one of those. Under the handoff plan it never does.

---

## A3. Open findings

### G13 🔴 There is no representation of a design occupying more than one grid, so MET is broken

**One root cause, four symptoms.** `build_design_matrix()` — and `matrix()` before it — models a design
as *a* grid. A multi-environment trial is several grids that share a treatment set and must never share
an edge. `initialise_multiple_designs_df()` ([design_utils.R:539](R/design_utils.R#L539)) reuses `row`/`col`
per site, so **every** MET design built the documented way has duplicate coordinates, and nothing
anywhere records which column separates the grids.

**Measured 2026-08-06** on `initialise_design_df(items = c(rep(1:10, 6), rep(11:20, 8)), designs =
list(a = list(nrows = 10, ncols = 3), b = list(nrows = 10, ncols = 5)))` — 80 plots, 10 unique rows,
5 unique cols:

| Symptom | `main` | this branch, before G12 | now |
|---|---|---|---|
| `.neighbour_balance()` | 50-cell grid from 80 plots: **30 plots silently discarded**, one `data length differs from size of matrix` warning | errors, taking all of `summary()` with it | reported unavailable, with a reason |
| `calculate_adjacency_score()` | garbage from the same truncation | **errors** | **errors** — correct for a direct call, but there is still no way to get the right number |
| `calculate_efficiency_factor()` | pools sites into one row/col model | returns `1.855529`, silently — a value `> 1` is impossible | still `1.855529` on a direct call; withheld in `summary()` |
| sites laid side by side in one grid (`col + 3` for site b, so coordinates *are* unique) | **60** adjacencies vs **50** summing per site — 10 phantom cross-site edges | **identical, 60** | **still 60** |

Read the last two rows carefully. This is **not** a regression this branch introduced, and it is **not
fixed by validation or by G12's gate**: duplicate coordinates don't break coordinate *indexing*, they just
quietly pool, and the side-by-side case has no duplicates at all, so no error can fire and no gate can
catch it. Both are silent wrong answers on `main` and on this branch. What the branch changed is the first
two rows, from silently wrong to loudly wrong; what G12 changed is that `summary()` survives saying so.
The duplicate-coordinate error is doing its job — it is the only reason any of this is visible — but it is
a diagnostic, not the fix.

**Adjacency and neighbour balance are summable over grids.** Measured: per-site adjacency 20 + 30 = **50**,
which is the correct whole-design figure. Both count edges, and edges never cross a grid boundary, so
summing per grid is exact rather than an approximation. That makes the fix tractable.

**Implementation sketch.**

1. **Carry the grouping column.** Extend `grid_factors` to `list(dim1 = "row", dim2 = "col", by = "site")`.
   Verified backwards compatible — `infer_row_col()` reads only `$dim1`/`$dim2`, and `speed()` already
   accepts the three-element list without complaint. That tolerance is itself a trap: a mistyped `by` is
   silently ignored today, so this needs validation added at the same time.
2. **Record it.** `metadata` currently holds only `row_column` / `col_column`
   ([speed.R:401-406](R/speed.R#L401-L406)), which is why `summary()` cannot recover the grouping on its
   own. Add `grid_by`.
3. **A list-of-grids primitive.** `build_design_matrices(df, swap, rc, cc, by = NULL)` returning a named
   list, length 1 when `by` is `NULL`. `build_design_matrix()` stays exactly as it is — the single-grid
   primitive, still strict. Sum `adjacency_score_vec()` and the `calculate_nb()` pair tables across the
   list. Note this wants a *list of indices* from `grid_index()` per grid, so it composes with G11's
   hoisting rather than reintroducing per-iteration validation.
4. **Auto-detection is the wrong instinct.** Duplicate coordinates with no `by` should keep erroring, and
   the current message already names the remedy. Inferring the grouping from a `"site"`-like column name
   is how the ordering bugs in G1 happened. If it should be automatic, have
   `initialise_design_df(designs = )` record `design_col` as an attribute so it is *transported* rather
   than guessed.
5. **Efficiency is not summable** — see D7. G12 already withholds it inside `summary()`; a direct
   `calculate_efficiency_factor()` call still returns `1.855529`, so the refusal needs to move into the
   function itself once D7 says what the right answer is.
6. **Then relax G12's gate.** `.single_grid()` is deliberately the *only* place `summary()` decides a
   design isn't griddable, so once the metrics take a grouping factor, MET designs stop reaching it and it
   covers only genuinely un-griddable input. The split-plot test added with G12 is what guards against the
   gate over-reaching in the meantime.

Scope check: items 1-4 are mechanical given the summability result. Item 5 and `objective_function_piepho()`'s
ED component are the parts that need a statistical answer first, and both can be gated to "unavailable"
in the meantime so MET adjacency and neighbour balance land without waiting on them.

---

## A4. Corrections that still matter

Corrections to superseded findings have been dropped along with the findings. These bear on open items.

| Earlier claim | Corrected |
|---|---|
| **Rank** the coordinates and you destroy real physical gaps, so validate instead | **Right, and it survived a detour.** Briefly overruled in favour of ranking; reinstated once it was clear that ranking is only the inverse of `add_buffers()`' displacement. Undo the displacement at its source and raw coordinates preserve genuine gaps at no cost. See D6. |
| D6 is a statistical question about whether buffers separate plots | **Not really.** It looked like one, but the only thing making a buffered design score differently was `add_buffers()` rewriting the real plots' coordinates. Verified: ranking the de-buffered coordinates restores the original `1..n` exactly, for all five buffer types. The statistical question is settled trivially — buffers must not change anything — and the rest was an implementation leak. |
| **G10** — `build_design_matrix()` must rank its coordinates to satisfy D6 | **Withdrawn, do not implement.** Ranking in `build_design_matrix()` would infer the undo in the wrong place and collapse real holes as collateral. `feature/buffers` records the displacement in `metadata$buffer` and inverts it in `.drop_buffer_rows()` instead; `build_design_matrix()` stays raw. G10's other three sub-items are also resolved: the `test-summary.R` comment is rewritten on that branch, the NEWS sentence has already been removed here, and `calculate_efficiency_factor()` needs no change (G6). |
| An efficiency factor `> 1` is a canary for the ordering bug (G7) | **Too narrow.** `> 1` signals rank deficiency however it arises. Measured: degenerate fixtures where treatment is confounded with row — which is what `initialise_design_df(rep(LETTERS[1:k], m), ...)` produces, see `KNOWN_ISSUES.md` #3 — return values `> 1` in **row-major** order too, on `main`. It is a canary for "something is wrong", not for ordering specifically. |
| MET only needs a gate in `summary()`; the grid code itself is fine | **Wrong, and too narrow twice over.** The gate (G12) is real but it only stops `summary()` crashing — it does not make MET work, which is the actual requirement. And two silent wrong answers survive any amount of gating: `calculate_efficiency_factor()` returns **1.855529** on a MET frame because duplicate coordinates pool rather than error, and a MET design laid side by side in one grid has *no* duplicate coordinates yet still counts **10 phantom cross-site edges** (60 vs 50). Validation cannot catch either. Grid metrics need a grouping dimension — see G13. |
| `main`'s MET behaviour was "garbage, with a warning" | **Quantified:** the `matrix()` reshape built 50 cells from 80 plots, **silently discarding 30**, with one `data length differs from size of matrix` warning. Worth stating precisely because it is the reason this branch's hard error is an improvement even though it is not the fix. |
| Hot-loop cost of `build_design_matrix()` is **2.84×** (415 → 1180 µs/build) | **Superseded by a cleaner measurement.** On the same 28×25 fixture it is **11.5×** (20 → 230 µs/build); the earlier figure bundled other work into both arms. The ratio is worse than thought and the absolute cost lower, but the actionable finding held up: 87% was loop-invariant validation, and hoisting it recovered parity. See A1.1. |
