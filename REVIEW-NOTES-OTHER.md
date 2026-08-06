# Review notes: grid construction and core metrics

**Scope:** `R/design_utils.R` (`build_design_matrix()`, `grid_index()`), `R/calculate_adjacency_score.R`,
`R/metrics.R`, and `R/summary.R` where it consumes a grid. Branch **`bugfix/grid-orientation`** off `main`.

G13 touches `R/summary.R` but belongs **here, not in `REVIEW-NOTES-SUMMARY.md`** (Sam, 2026-08-06): this
branch made the grid contract strict, so it owns the consequences of that strictness.

**Companion files** — one per workstream:

| File | Workstream |
|---|---|
| `REVIEW-NOTES.md` | `feature/incidence` (PR #97) — `R/incidence.R` |
| `REVIEW-NOTES-SUMMARY.md` | the merged `summary()` work — `R/summary.R` |
| `REVIEW-NOTES-EFFICIENCY.md` | efficiency-factor statistics — branch `feature/a-optimality` exists |
| `REVIEW-NOTES-PR91.md` | PR #91 `info-objective` |
| **this file** | grid construction / core metrics |

Moved out, each needing its own branch: the A-efficiency upper bound and the missing intercept
(`REVIEW-NOTES-EFFICIENCY.md`); the S3 class collision, the `initialise_design_df()` fill order and the
now-redundant row-major sort (`KNOWN_ISSUES.md` #2, #3, #4). The buffer coordinate convention — settled,
and the reason `build_design_matrix()` keeps coordinates **raw** — is `KNOWN_ISSUES.md` #1.

**Last verified:** 2026-08-06, R 4.6.1, `pkgload::load_all()`. All numbers measured, not inferred.
Resolved findings and settled decisions are deleted rather than annotated — see git history and `NEWS.md`.

> ✅ **The branch's original scope is closed.** The plan as committed at `b1d7adc` listed D6, D1 and
> G1-G6, plus the hot-loop cost recorded as out of scope; all are done, the last as G11. G7, S1, A5, G11
> and G12 were found during the work. See A1. Full suite: **1739 pass, 0 fail, 0 warn.**
>
> ⬜ **Deliberately still open from the original plan:** removing the row-major sort, which A4.7 recorded
> as out of scope on purpose (`KNOWN_ISSUES.md` #4).
>
> 📦 **`feature/buffers` merges into this branch** (branched off `f5d68f5`), carrying the coordinate
> restoration that makes raw coordinates correct plus the `add_buffers()` deprecation. See A2 — two items
> are already done there, so read it before actioning anything.
>
> 🔴 **Blocker: G13 — nothing in speed represents a design occupying more than one grid, so MET is
> broken.** `initialise_design_df(designs = )` reuses `row`/`col` per site, so every MET design has
> duplicate coordinates. `main` silently discarded **30 of 80** plots; this branch errors instead. Neither
> works, and coordinate construction did not cause it. Grid metrics need a grouping dimension — see A3.
>
> 🔶 **D7 is the one open statistical question** and it blocks the last part of G13: what a grid metric
> should report per site versus pooled. Adjacency and neighbour balance are settled (they sum exactly);
> efficiency and piepho's ED are not. See A4.
>
> 🟠 **G14 — `summary()` reports an efficiency factor above 1 for a rank-deficient *single* grid.** Measured
> 1.61 on an ordinary unreplicated 12-entry 4×3 trial, identical on `main`. **No D7 decision needed** — a
> single grid has no pooling question — but it is fixed by the same rank gate as D7 recommendation 2, so
> build that gate once. Pre-existing, not branch-introduced. See A5.

---

## A1. Landed on this branch

One-line inventory for the PR description. Full write-ups are in git history.

| Was | Now |
|---|---|
| **G1** four functions each assumed a data ordering, two row-major and two column-major | all four read coordinates via `build_design_matrix()` or a coordinate-indexed fill |
| **G2** `objective_function_piepho()` wrote a column-major flattened grid back over the treatment column | write-back deleted; all four score components computed on the real layout, and piepho is order-invariant |
| **G3** `build_design_matrix()` didn't validate coordinates | explicit missing-column / non-numeric / non-positive-integer / duplicate-coordinate errors, each its own condition class |
| **G4** `.calculate_nb()` errored on sparse grids, the default path | `NA` neighbours are skipped, matching the `pair_mapping` path |
| **G5** a scalar `ring_weights` errored against multi-ring `ring_dists` | recycled across every ring |
| **G6** `calculate_efficiency_factor()` couldn't compute for a buffered design | resolved as a side effect of G7 — coordinate indexing absorbs the offset `add_buffers()` introduces, and a genuinely holed grid computes too; both cases now pinned in `test-grid-orientation.R` — the fix was closed with no coverage until then |
| **G7** `calculate_efficiency_factor()` filled `Z` positionally, returning a different value per row ordering (0.111 vs 0.625 on a 2×6, and values `> 1`) | `Z` is indexed by each plot's own coordinates |
| **S1** `.neighbour_balance()` reported self-adjacencies that didn't exist (6 where the truth was 0) | reads coordinates; the 4×3 fixture returns the hand-derived truth (self 0, pair min/max 5/6) |
| **A5** lexical factor levels (`1, 10, 11, 2, …`) defeated the row-major sort, so grid metrics scored a layout that wasn't the design | coordinate construction is immune; the sort is no longer load-bearing (`KNOWN_ISSUES.md` #4) |
| **G11** coordinate validation ran on every iteration, making grid construction 16× the `matrix()` reshape it replaced | validation split into `grid_index()`, hoisted once per `speed()` run and built lazily so a design that cannot be gridded still optimises if its objective never needs a grid; build back to parity — see A1.1 |
| **G12** `summary()` died outright on any design that couldn't be gridded — MET, or non-numeric `row`/`col` labels | `summary.design()` calls `grid_index()` once and keeps either the index or the condition's `reason` as `grid`, which it passes to `.neighbour_balance()`, `.efficiency_factor()` and `.replicate_spans()`; each reports the reason instead of computing. `has_grid` now means "reportable as one grid", so `layout` no longer describes a MET as a single grid. `nrow`/`ncol` count *occupied* rows and columns (settled, Sam), so on a design with gaps they are deliberately fewer than the coordinates span — documented in `?summary.design` |

### A1.1 G11 measurements

Kept because they are the PR's evidence and not recoverable from the code. Grid build on 700 plots
(28×25), 2000 reps:

| | µs/build | vs `matrix()` |
|---|---|---|
| `matrix()` — what `main` did | 15 | 1.00× |
| `build_design_matrix()`, no index | 240 | 16.00× |
| `build_design_matrix()`, index supplied | **15** | **1.00×** |

End-to-end against a clean worktree at `69c516d`. **Scores are bit-identical in every case** — this is a
performance change only:

| | before | after | |
|---|---|---|---|
| `objective_function`, 700 plots, 2000 iters | 1.58 s | **1.14 s** | −28% |
| `objective_function`, 700 plots, 5000 iters | 3.89 s | **2.86 s** | −26% |
| `objective_function`, 120 plots, 5000 iters | 2.46 s | **2.17 s** | −12% |
| `objective_function_piepho`, 120 plots, 1000 iters | 1.64 s | **1.47 s** | −10% |

What remains of the cost is the per-iteration `as.character()` coercion of the swap column, which is not
hoistable the way validation was: the swap column is the one thing annealing mutates. Parity is therefore the
floor for a rebuild-per-iteration grid, and it is accepted. Carrying a mutable grid across iterations instead
was **ruled out** (Sam, 2026-08-06): it needs a contract change to the objective-function signature and is
capped at under 3% of a run, since the build is ~15 µs of a ~570 µs iteration.

---

## A2. What arrives when `feature/buffers` merges

Branched off `f5d68f5`, so it applies cleanly. Two items are already done there — **do not action them
again**:

| From `feature/buffers` | Effect here |
|---|---|
| `metadata$buffer` transform record in `add_buffers()`, inverted by `.drop_buffer_rows()` / `.restore_buffer_coords()` | satisfies the `KNOWN_ISSUES.md` #1 convention without touching `build_design_matrix()` |
| `test-summary.R` buffer test rewritten | **fixes the stale comment at [test-summary.R:299-305](tests/testthat/test-summary.R#L299-L305)**, which still claims a `"row"` buffer should change the counts — the opposite of the `KNOWN_ISSUES.md` #1 convention. Now asserts every buffer type and stacked combinations match the unbuffered design |
| `add_buffers()` deprecation warning + `## Deprecations` NEWS section | buffers are leaving speed; the biometryassist repo's `BUFFERS-HANDOFF.md` specifies that side |
| `.warn_if_buffers()` in `calculate_adjacency_score()`, `calculate_balance_score()`, `calculate_efficiency_factor()` | a direct metric call on a buffered frame bypasses `.drop_buffer_rows()`, so it warns rather than silently scoring the displaced layout |
| `helper-buffers.R` with `add_buffers_quiet()`, and 45 rewritten test call sites | keeps the deprecation warning out of tests that are about layout |

One caveat carried forward: the `metadata$buffer` record is an affine `scale`/`shift` pair, which covers
speed's buffer types but **cannot** represent biometryassist's `by =` block buffers, where gaps appear only
at group boundaries. It would need to become a per-axis `new -> old` lookup if speed ever had to invert one
of those. Under the handoff plan it never does.

### A2.1 Merge order: PR #97 depends on this branch

This branch exists because the grid work was extracted out of `feature/incidence` (D1 in the original
plan). **Verified 2026-08-06** — the extraction is complete and the dependency now runs one way:

- `feature/incidence` touches only `R/incidence.R`, docs and tests. It no longer carries any of
  `R/design_utils.R`, `R/calculate_adjacency_score.R` or `R/metrics.R`, so there is nothing to conflict.
- But `incidence.R:69` calls `build_design_matrix()`, and that function **does not exist on `main`**.

So **PR #97 cannot merge until this branch does**, and `feature/incidence` has not been rebased onto it
(`git merge-base --is-ancestor bugfix/grid-orientation feature/incidence` → false). Rebase it after this
branch lands. It calls `build_design_matrix()` without an `index`, which is correct — incidence is a
one-off diagnostic, not in the annealing loop — but it does re-implement its own `missing_cols` check,
the same duplication G12 avoided by delegating to `grid_index()`'s condition classes. Worth collapsing
when it rebases.

---

## A3. G13 🔴 There is no representation of a design occupying more than one grid, so MET is broken

**One root cause, four symptoms.** `build_design_matrix()` — and `matrix()` before it — models a design as
*a* grid. A multi-environment trial is several grids that share a treatment set and must never share an
edge. `initialise_multiple_designs_df()` ([design_utils.R:520](R/design_utils.R#L520)) reuses `row`/`col`
per site, so **every** MET design built the documented way has duplicate coordinates, and nothing anywhere
records which column separates the grids.

**Measured 2026-08-06** on `initialise_design_df(items = c(rep(1:10, 6), rep(11:20, 8)), designs =
list(a = list(nrows = 10, ncols = 3), b = list(nrows = 10, ncols = 5)))` — 80 plots, 10 unique rows,
5 unique cols:

| Symptom | `main` | this branch |
|---|---|---|
| `.neighbour_balance()` | 50-cell grid from 80 plots: **30 plots silently discarded**, one `data length differs from size of matrix` warning | reported unavailable, with a reason (G12) |
| `calculate_adjacency_score()` | garbage from the same truncation | **errors** — correct for a direct call, but there is still no way to get the right number |
| `calculate_efficiency_factor()` | pools sites into one row/col model | still `1.855529` on a direct call — a value `> 1` is impossible; withheld inside `summary()` only |
| sites laid side by side in one grid (`col + 3` for site b, so coordinates *are* unique) | **60** adjacencies vs **50** summing per site — 10 phantom cross-site edges | **still 60** |

Read the last two rows carefully. This is **not** a regression this branch introduced and it is **not fixed
by validation or by G12's gate**: duplicate coordinates don't break coordinate *indexing*, they just quietly
pool, and the side-by-side case has no duplicates at all, so no error can fire and no gate can catch it.
The duplicate-coordinate error is doing its job — it is the only reason any of this is visible — but it is a
diagnostic, not the fix.

**Adjacency and neighbour balance are summable over grids.** Measured: per-site adjacency 20 + 30 = **50**,
the correct whole-design figure. Both count edges, and edges never cross a grid boundary, so summing per
grid is exact rather than an approximation. That makes most of the fix tractable.

**Implementation sketch.**

1. **Carry the grouping column.** Extend `grid_factors` to `list(dim1 = "row", dim2 = "col", by = "site")`.
   Verified backwards compatible — `infer_row_col()` reads only `$dim1`/`$dim2`, and `speed()` already
   accepts the three-element list without complaint. That tolerance is itself a trap: a mistyped `by` is
   silently ignored today, so this needs validation added at the same time.
2. **Record it.** `metadata` currently holds only `levels` / `row_column` / `col_column` / `per_level`
   ([speed.R:418-423](R/speed.R#L418-L423)), which is why `summary()` cannot recover the grouping on its
   own. Add `grid_by`.
3. **A list-of-grids primitive.** `build_design_matrices(df, swap, rc, cc, by = NULL)` returning a named
   list, length 1 when `by` is `NULL`. `build_design_matrix()` stays exactly as it is — the single-grid
   primitive, still strict. Sum `adjacency_score_vec()` and the `calculate_nb()` pair tables across the
   list. This wants a *list of indices* from `grid_index()`, one per grid, so it composes with G11's
   hoisting rather than reintroducing per-iteration validation.
4. **Auto-detection is the wrong instinct.** Duplicate coordinates with no `by` should keep erroring, and
   the current message already names the remedy. Inferring the grouping from a `"site"`-like column name is
   how the ordering bugs in G1 happened. If it should be automatic, have
   `initialise_design_df(designs = )` record `design_col` as an attribute so it is *transported* rather
   than guessed.
5. **Efficiency and ED are not summable** — see D7. `calculate_efficiency_factor()` needs to refuse a
   multi-grid frame rather than return `1.855529`; G12 withholds it inside `summary()`, but a direct call
   still doesn't. Note the gate is specifically on *duplicate coordinates*: it does **not** catch a
   rank-deficient single grid, which `summary()` still reports — see A5.
6. **Then relax G12's gate.** The single `grid_index()` call in `summary.design()` is deliberately the
   *only* place `summary()` decides a design isn't griddable, so once the metrics take a grouping factor,
   MET designs stop reaching it and it covers only genuinely un-griddable input. The split-plot test added
   with G12 guards against the gate over-reaching in the meantime.

Scope check: items 1-4 are mechanical given the summability result. Item 5 needs D7 first, and can stay
gated to "unavailable" in the meantime so MET adjacency and neighbour balance land without waiting on it.

---

## A4. 🔶 D7. What should the grid metrics report for a multi-grid (MET) design? — **open**

Adjacency and neighbour balance answer themselves: they count edges, edges never cross a grid boundary, and
summing per grid is exact (measured, 20 + 30 = 50). Efficiency and `objective_function_piepho()`'s ED do
not follow, and for the same reason — they are properties of an assumed *model*, not counts.

**An efficiency factor is relative to a model, and speed's implied model is `y ~ trt + row + col`.**
`calculate_efficiency_factor()` eliminates a row-effect and column-effect nuisance space from the treatment
information matrix. A MET is not analysed that way: the residual structure is separate per site (a `dsum()`
term in `asreml()`), and row/column effects are nested within site rather than shared across sites. So the
pooled number speed currently produces corresponds to no model anyone fits.

**Measured 2026-08-06** — two sites, 8 treatments × 3 reps per site, 4×6 grids. The reference
implementation reproduces `calculate_efficiency_factor()` to the digit wherever the design is full rank, so
the only thing varying below is the nuisance space:

| | value |
|---|---|
| per site A / site B | 0.547 / 0.427 |
| pooled, row/col nested within site (the `dsum`-shaped model) | **0.566** |
| pooled, plain `row + col` — **what speed does now** | **0.807** |

Two conclusions. The current pooled value is **inflated** — 0.807 against 0.566 — because pooling makes
"row 3" one factor level across both sites, borrowing strength that does not exist in the field. And
per-site-then-averaged is a different quantity again: (0.547 + 0.427)/2 = 0.487, not 0.566. There is no
aggregation shortcut.

**Per site is the right unit, but it cannot be unconditional.** Measured on the commonest MET shape — 12
entries, each appearing **once per site**, 4×3 grids — the per-site value is **1.833**, impossible for an
efficiency factor. With `r = 1` inside a site there are not enough residual degrees of freedom to estimate
the treatment contrasts after eliminating row and column effects, so the information matrix is singular and
the pseudo-inverse returns a meaningless number. (The pooled site-nested model for that design gives 0.105
— low, but at least defined, because replication *across* sites is real replication.) So reporting per-site
values by default would print garbage for exactly the designs MET support exists for.

**Recommendation, in build order.**

1. **Refuse for multi-grid designs, with a reason.** Required as an interim state either way — the
   alternative is continuing to return `1.855529` for a quantity bounded above by 1. G12 already does this
   inside `summary()`; G13 item 5 moves it into `calculate_efficiency_factor()`.
2. **Then per-site values, gated on per-site estimability** — that site's own information matrix must have
   rank `k - 1`, i.e. every treatment contrast estimable within the site. Labelled per site, never summed
   or averaged. This is the design-actionable quantity, because the layout that can be changed is the one
   within a site. A site failing the rank test reports unavailable individually rather than poisoning the
   whole vector.
3. **The combined-analysis number only as an explicit opt-in.** It is computable (0.566 above) but needs
   row/col nested within site *and* an assumption of equal residual variance across sites — precisely what
   `dsum()` denies. Under unequal variances the combined efficiency depends on variance ratios that are
   unknown at design time, so no single design-time number exists. Never the default, and never the current
   plain `row + col` pooling.

Item 2 is not misleading provided it is labelled per site and no aggregate is offered alongside it; the
misleading options are the pooled 0.807 and any average of the per-site values.

**`objective_function_piepho()`'s ED needs the same treatment.** NB sums like any edge count, but ED
measures evenness of a distribution, so per-grid-then-averaged and pooled are different quantities and the
paper's definition assumes a single trial. Whatever is decided, the same answer must apply to `summary()`'s
`efficiency` entry and to `.neighbour_balance()`, so the two never disagree about what a MET design's
diagnostics mean.

**The rank gate in recommendation 2 is needed whether or not D7 is settled.** A value `> 1` signals rank
deficiency **however it arises** — a MET site with `r = 1` (above), a single grid that exhausts its residual
degrees of freedom, or one where treatment is aliased with row despite having residual df to spare
(`KNOWN_ISSUES.md` #3). The last two are single-grid designs that `summary()` reports today: see A5. So build
the gate as a rank test on *one* information matrix rather than inside the MET path, and D7 recommendation 2
and G14 are covered by one implementation.

---

## A5. G14 🟠 `summary()` reports an efficiency factor above 1 for a rank-deficient single grid

**No decision needed — this one does not ride on D7.** D7 is open because a *multi-grid* design has a real
statistical question (per site, pooled, or nested). A single grid has no such question: an A-efficiency
factor is bounded above by 1, so a value above it is wrong under any reading. What G14 shares with D7 is the
**fix**, not the decision — see the closing paragraph of A4.

**Pre-existing, and this branch changed neither the values nor the reporting.** Measured 2026-08-06 on both
`bugfix/grid-orientation` and a clean `main` worktree. Residual df is `n − 1 − (k−1) − (r−1) − (c−1)`:

| Design | plots | residual df | value | surfaced by | `main` |
|---|---|---|---|---|---|
| 1×6 grid, 3 treatments | 6 | −2 | **1.5** | `summary()` | **1.5** |
| 6×1 grid, 3 treatments | 6 | −2 | **1.5** | `summary()` | **1.5** |
| 4×3 grid, 12 entries unreplicated | 12 | −5 | **1.61** | `summary()` | **1.61** |
| 3×4 grid, 3 treatments confounded with row | 12 | **+4** | **1.5** | `.efficiency_factor()` | — |
| 2×3 grid, 3 treatments | 6 | 0 | 0.75 | `summary()` | — |
| 2×6 grid, 3 treatments | 12 | 3 | 0.75 | `summary()` | — |

**There are two independent routes to a value above 1, so residual df is not a sufficient test.** Rows 1-3
exhaust the residual degrees of freedom. Row 4 has *four* residual df and still returns 1.5, because
`rep(LETTERS[1:3], length.out = 12)` over `expand.grid(row = 1:3, col = 1:4)` puts each treatment in exactly
one row — treatment is aliased with the row effect, so eliminating the row space eliminates the treatment
contrasts with it (`KNOWN_ISSUES.md` #3). **The gate must therefore test the rank of the information matrix,
not count degrees of freedom.** The last two rows are included because they are the boundary a gate must not
reject: residual df 0 is still estimable.

Reachability differs between the two routes. Rows 1-3 come straight out of `summary()`. Row 4 is reported by
`.efficiency_factor()` when handed such a frame, but `speed()` breaks the confounding within a single
iteration (measured: still confounded `FALSE`, `summary()` then reports 0.4891), so a design that has been
through `speed()` does not normally surface it. Route 1 is the one users hit.

**Why G12's gate does not catch either.** `has_grid` is `TRUE` throughout: the coordinates are unique, so
`grid_index()` is satisfied and there is nothing for `.efficiency_factor()` to refuse. G12 gates on *can this
be one grid*, a coordinate property; this is a *rank* property of the model fitted on that grid. Different
question, so no amount of coordinate validation reaches it. The `< 3 treatments` guard already in
`.efficiency_factor()` is the only rank-adjacent check today, and it is far too weak.

**Why it matters more than the MET case.** The 4×3-with-12-unreplicated-entries row is not a pathological
fixture — it is an ordinary early-generation trial, and it is the *same shape* D7 measures at 1.833 per site
inside a MET. So the impossible value is reachable from a completely routine single-site call, not only from
the MET path everyone already knows is broken.

**Fix.** Gate `calculate_efficiency_factor()` (or `.efficiency_factor()`) on the treatment information matrix
having rank `k − 1` after eliminating the row and column space, and report unavailable with a reason when it
does not. A rank test covers both routes above; a residual-df test covers only the first. This is exactly D7
recommendation 2's check applied to a single grid, which is why A4 now says to build it as a property of one
information matrix rather than inside the MET path. Landing it here first is the cheaper order: G14 needs no
grouping column and no D7 answer, and D7 recommendation 2 then inherits the gate instead of introducing it.

Two details for whoever implements it. The exact spot is
[metrics.R:749](R/metrics.R#L749) — `V <- pseudo_inverse(A_RC)`, applied to the treatment information matrix
**unconditionally**, with no rank check. (Contrast [metrics.R:733-740](R/metrics.R#L733-L740), where the
nuisance space `ZtZ` *is* guarded by a `kappa()` test before choosing `pseudo_inverse()` over `solve()`.) So
the test is `rank(A_RC) == k − 1`, placed before line 749; sanity-checking the number that comes out is the
wrong shape. Second, clamping or `NA`-ing anything above 1 would also be wrong: it hides the confounded case
(row 4) behind a plausible value instead of reporting that the design cannot support the estimate.

**Not in scope for this branch** — it is pre-existing, it is not caused by coordinate construction, and
`REVIEW-NOTES-EFFICIENCY.md` owns the efficiency statistics (branch `feature/a-optimality` exists). Recorded
here because G13 item 5 and D7 both assume the only bad efficiency value is the MET one, and that is not
true.

### A5.1 Also found while probing — belongs in `REVIEW-NOTES-SUMMARY.md`

`summary()` **errors outright** on a single-row or single-column design:

```
summary(<1x6 design>, efficiency = TRUE)
#> Error: contrasts can be applied only to factors with 2 or more levels
```

Thrown from `.design_connectedness()` via `model.matrix()`, because a spatial factor with one level has no
contrasts. Unrelated to the grid work — this branch does not touch `.design_connectedness()` — and it is why
the table above passes `connectedness = FALSE`. Noted so it is not lost; it needs the same treatment as G12,
i.e. report unavailable with a reason rather than propagating the error.
