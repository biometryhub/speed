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

> ✅ **The branch's original scope is closed, and the findings it turned up with it.** The plan as
> committed at `b1d7adc` listed D6, D1 and G1-G6, plus the hot-loop cost recorded as out of scope; all are
> done, the last as G11. G7, S1, A5, G11, G12, G13 and G14 were found during the work and are also done.
> See A1. Full suite: **1774 pass, 0 fail, 0 warn.**
>
> ⬜ **Deliberately still open from the original plan:** removing the row-major sort, which A4.7 recorded
> as out of scope on purpose (`KNOWN_ISSUES.md` #4).
>
> 📦 **`feature/buffers` merges into this branch** (branched off `f5d68f5`), carrying the coordinate
> restoration that makes raw coordinates correct plus the `add_buffers()` deprecation. See A2 — two items
> are already done there, so read it before actioning anything.
>
> ✅ **G13 has landed.** `grid_factors` gains an optional `by`, so a design can occupy several grids.
> Adjacency and neighbour balance are summed per grid, efficiency is reported per grid, and nothing is
> counted between plots at different sites. See A3.
>
> ✅ **D7 is decided (Sam, 2026-08-07): per site, gated on per-site rank, with no combined figure.**
> Adjacency and neighbour balance sum exactly; efficiency is reported one value per site, each withheld
> with a reason if that site's contrasts aren't estimable. A combined `dsum`-shaped number needs no
> `asreml` to compute but is **not identified** — measured, the design ranking flips with the assumed
> variance ratio and the value passes 1. G13 is no longer blocked on a decision. See A4.
>
> ✅ **G14 has landed.** `calculate_efficiency_factor()` refuses a design whose treatment contrasts are
> not estimable instead of returning an impossible value, and the row-column model now carries an
> intercept, which is what makes the rank test exact. See A5.
>
> ✅ **piepho's ED is settled too** (Sam, 2026-08-07): scored **per grid and summed**, each grid's value
> reported alongside the total. A grid with nothing replicated inside it contributes `0` — which also
> fixes an `Inf` that single-grid unreplicated designs scored on `main`. See the end of A4.

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
| **G13** nothing represented a design occupying more than one grid, so every MET design was scored as one pooled grid — discarding plots whose coordinates collided, or inventing adjacencies between sites | `grid_factors` gains an optional `by`; `grid_indices()` returns one validated index per grid; adjacency and neighbour balance sum per grid, efficiency is reported per grid, and `metadata$grid_by` records the grouping — see A3 |
| **G14** `calculate_efficiency_factor()` returned a plausible-looking value, usually above 1, for a design whose treatment contrasts are not estimable | refuses with a `speed_efficiency_rank` condition, which `summary()` reports as a reason; the row-column model gained an intercept, which is what makes the rank test exact and changes no existing value — see A5 |
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

## A3. G13 ✅ A design can now occupy more than one grid

**Landed 2026-08-07.** `grid_factors` gained an optional `by` naming the column that separates grids;
`grid_indices()` returns one validated index per grid (a one-element list when `by` is `NULL`, so callers
have a single code path); `calculate_adjacency_score()` and `.neighbour_balance()` sum per grid;
`summary()` reports one efficiency per grid; `metadata$grid_by` records the grouping so `summary()` never
has to guess it. A mistyped `by` is rejected rather than silently ignored. `objective_function_piepho()`
refuses a multi-grid design, because its ED component has no agreed multi-grid form.

Measured on two 4×3 sites sharing coordinates: `by = "site"` scores **3**, exactly the sum of the
per-site scores, where pooling them side by side scores higher on account of adjacencies across the join.
The record below is what the fix had to account for.

### The problem it fixed

**One root cause, four symptoms.** `build_design_matrix()` — and `matrix()` before it — models a design as
*a* grid. A multi-environment trial is several grids that share a treatment set and must never share an
edge. `initialise_multiple_designs_df()` ([design_utils.R:520](R/design_utils.R#L520)) reuses `row`/`col`
per site, so **every** MET design built the documented way has duplicate coordinates, and nothing anywhere
records which column separates the grids.

**Measured 2026-08-06** on `initialise_design_df(items = c(rep(1:10, 6), rep(11:20, 8)), designs =
list(a = list(nrows = 10, ncols = 3), b = list(nrows = 10, ncols = 5)))` — 80 plots, 10 unique rows,
5 unique cols:

| Symptom | `main` | now |
|---|---|---|
| `.neighbour_balance()` | 50-cell grid from 80 plots: **30 plots silently discarded**, one `data length differs from size of matrix` warning | summed per grid |
| `calculate_adjacency_score()` | garbage from the same truncation | summed per grid with `by`; still errors without it, which is correct |
| `calculate_efficiency_factor()` | pools sites into one row/col model, returning `1.855529` — a value `> 1` is impossible | refuses a multi-grid frame; `summary()` reports one value per site |
| sites laid side by side in one grid (`col + 3` for site b, so coordinates *are* unique) | **60** adjacencies vs **50** summing per site — 10 phantom cross-site edges | `by` gives **50**; without it, still 60 |

The last row is the one that mattered most: it has no duplicate coordinates, so **no error can fire and no
gate can catch it** — it is a silently wrong number on `main` and would have stayed one under any amount of
coordinate validation. Only carrying the grouping fixes it, which is why the duplicate-coordinate error was
a diagnostic rather than the fix. It is pinned by a test that lays two sites side by side and asserts the
pooled score exceeds the per-grid one.

**Adjacency and neighbour balance are summable over grids.** Measured: per-site adjacency 20 + 30 = **50**,
the correct whole-design figure. Both count edges, and edges never cross a grid boundary, so summing per
grid is exact rather than an approximation. That makes most of the fix tractable.

### Decisions taken while building it

- **Auto-detection was rejected.** Duplicate coordinates with no `by` still error, and the message names
  the remedy. Inferring the grouping from a `"site"`-like column name is how the ordering bugs in G1
  happened. If it should ever be automatic, have `initialise_design_df(designs = )` record `design_col`
  as an attribute so the grouping is *transported* rather than guessed.
- **`by` is validated, because `grid_factors` is a plain list.** A mistyped element would otherwise be
  silently dropped and every site pooled — the failure mode is a wrong number, not an error.
- **`build_design_matrix()` was left untouched**, still the strict single-grid primitive.
  `grid_indices()` sits above it and returns one index per grid, so the G11 hoisting composes: the
  annealing loop still validates once per run, not once per iteration.
- **`.replicate_spans()` stays withheld for multi-grid designs.** Spans are distances within one grid;
  two sites' row 3 are not one plot apart, and there is no combined span to report.
- **`objective_function_piepho()` scores ED per grid and sums.** Its NB component sums like any edge
  count; ED is a within-grid measure and is reported per grid as well as in total. See the end of A4.

---

## A4. ✅ D7. What should the grid metrics report for a multi-grid (MET) design? — **decided 2026-08-07**

**Per site, gated on per-site rank; no combined figure.** The reasoning and measurements are kept because
they are what justify refusing the combined number, and that refusal will be questioned again otherwise.

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

**Decided (Sam, 2026-08-07): report per-site values, gated on per-site rank.**

1. **Refuse for multi-grid designs, with a reason** — the interim state, and required either way, since
   the alternative is returning `1.855529` for a quantity bounded above by 1. G12 already does this inside
   `summary()`; G13 item 5 moves it into `calculate_efficiency_factor()`.
2. **Then one value per site, each gated on that site's own rank** — the site's information matrix must
   have rank `k - 1`, i.e. every treatment contrast estimable within the site. A site failing the test
   reports unavailable with a reason rather than poisoning the vector or being silently dropped. Labelled
   per site, **never summed or averaged**: per-site-then-averaged is a different quantity from the
   combined analysis (0.487 vs 0.566 above). This is also the design-actionable quantity, because the
   layout that can be changed is the one within a site.
3. **No combined number, not even as an opt-in** — see below. This reverses the earlier suggestion that it
   could be offered with a stated assumption.

### Why there is no reliable combined-analysis figure — measured 2026-08-07

**The computation needs no `asreml`.** With the residual variances treated as known it is generalised
least squares in closed form — `C = X'WX - X'WZ (Z'WZ)⁻ Z'WX`, `W = diag(1/v_i)` — reachable with
`model.matrix()` and `pseudo_inverse()` alone. `asreml` is only needed to *estimate* variance components
from data, and at design time there is no data, so there is nothing to estimate. The obstacle is
statistical, not computational.

Three MET designs (two sites, 8 treatments × 3 reps, 4×6 each), combined efficiency against the assumed
residual variance ratio B:A, with `v` normalised to plot-weighted mean 1:

| design | ratio 1 | ratio 2 | ratio 4 | ratio 10 | ratio 100 |
|---|---|---|---|---|---|
| seed 1 | 0.4814 | 0.5244 | 0.6839 | 1.2102 | 9.1689 |
| seed 2 | 0.5660 | 0.6454 | 0.8901 | 1.6932 | 13.9839 |
| seed 3 | 0.5834 | 0.6535 | 0.8810 | 1.6317 | 13.1163 |

Two independent reasons to refuse, either of which is sufficient:

- **The ranking flips.** At ratio ≤ 2 seed 3 is the best design; from ratio 4 on, seed 2 is. So the
  assumption is not a harmless caveat attached to a number — it changes which design you would choose.
  Nothing at design time tells you which ratio to use.
- **The quantity is not identified under heterogeneity.** The values exceed 1 from ratio 10 — the rank
  test passes, so this is not rank deficiency. An A-efficiency factor is defined *relative to an
  orthogonal reference design with a single error variance*; once the variances differ there is no
  canonical reference to normalise against, and a different normalisation gives a different number. The
  `> 1` values are the symptom of that, not an arithmetic slip.

Under **equal** variances (ratio 1) the quantity is well defined and exact — but that is precisely the
assumption `dsum()` exists to deny, so a MET reported that way would carry a figure computed under a model
its own analysis contradicts. Hence: per site, and no combined figure.

### ED: scored per grid and summed — decided 2026-08-07

NB sums like any edge count. ED does not: it measures how far apart a treatment's replicates are, and
there is no distance between plots at different sites. Pooling either treats two sites' `(1, 1)` as the
same point or invents a distance across the join — measured on a p-rep MET, **five of nine treatments**
had pooled replication exceeding their within-site maximum, so their spanning trees were built largely
from non-distances.

**Each grid is scored on its own and the scores are summed**, with each grid's value reported alongside
the total. Three things settle the form:

- **Summing is not a compromise.** A swap moves plots within one grid, so it changes only that grid's
  term; minimising the sum is identical, move for move, to optimising each grid independently.
- **Per-grid scores must not be compared with each other.** Measured on a p-rep MET: site a scores 1.00
  with replication class `{2}`, site b scores 0.50 with class `{3}` — a 3-replicate spanning tree is
  longer than a 2-replicate one whatever the spread. So "min across grids" and "mean of per-grid scores"
  would both systematically flag whichever site had lower replication. They are reported side by side,
  never ranked or averaged, exactly like per-site efficiency.
- **Summing the per-grid scores beats one pooled reciprocal.** Both optimise identically, but adjacency
  sums counts across grids, so it grows with site count; under a pooled `1 / sum(all MSTs)` ED *shrinks*
  as sites are added (the two sites above: 1.00 and 0.50 individually, 0.33 pooled) and quietly loses
  weight against adjacency and balance. Summing gives 1.50 and tracks the other components.

**A grid with nothing replicated inside it contributes `0`**, not `1/0`. This also fixes a pre-existing
single-grid defect: `objective_function_piepho()` returned **`Inf`** for a fully unreplicated design — an
ordinary early-generation trial — leaving every candidate scoring `Inf` and the optimiser with nothing to
compare.

The same answer applies to `summary()`'s `efficiency` entry and `.neighbour_balance()`, so the three never
disagree about what a MET design's diagnostics mean.

**The rank gate in recommendation 2 is needed whether or not D7 is settled.** A value `> 1` signals rank
deficiency **however it arises** — a MET site with `r = 1` (above), a single grid that exhausts its residual
degrees of freedom, or one where treatment is aliased with row despite having residual df to spare
(`KNOWN_ISSUES.md` #3). The last two are single-grid designs that `summary()` reports today: see A5. So build
the gate as a rank test on *one* information matrix rather than inside the MET path, and D7 recommendation 2
and G14 are covered by one implementation.

---

## A5. G14 ✅ An efficiency factor above 1 for a rank-deficient single grid

**Landed 2026-08-07.** `calculate_efficiency_factor()` now errors with a `speed_efficiency_rank` condition
when the treatment contrasts are not estimable, and `summary()` turns that into a reason. Two findings from
building it are worth keeping, because both contradict what this section originally proposed.

**The row-column model now includes an intercept (Sam's suggestion), and that is what makes the test
exact.** The proposed gate was `rank(A_RC) == k - 1` on the existing matrix. That is wrong without an
intercept: `X`'s rows sum to 1, so the treatment mean is estimable, a sound design gives rank `k`, and an
equality test rejects valid designs. Testing the contrast space instead is not a fix either — for a PSD
matrix, "no contrast lies in the null space" is weaker than "every contrast is estimable", so it passes
designs whose contrasts are not estimable. Putting the intercept in the nuisance space makes the null space
exactly the all-ones direction, at which point `rank(A_RC) == k - 1` means precisely what it should.
**Verified not to change any value**: both published designs still return 0.834 and 0.827, matching the
paper and matching pairwise contrast variances taken from the full model's Moore-Penrose inverse. This also
closes E2 in `REVIEW-NOTES-EFFICIENCY.md`.

**`qr()$rank` is unusable here** — its default tolerance is relative, and it reports rank 3 for a matrix
whose eigenvalues are `2, 8.7e-16, 6.3e-16`. The gate uses `svd()` with the same absolute tolerance as
`pseudo_inverse()`, so the gate and the inverse cannot disagree about which directions are null.

**Confirmed against base R.** Every design below was cross-checked by fitting
`y ~ factor(row) + factor(col) + treatment` with `lm()` and asking whether it aliases a treatment
coefficient. `lm()` agrees with the gate on all of them — including the four that existing tests asserted
should return a number. Note the term order matters: `lm()` pivots in formula order, so putting `treatment`
first lets it absorb the confounding and alias the row terms instead.

**Pre-existing, and this branch changed neither the values nor the reporting.** Measured 2026-08-06 on both
`bugfix/grid-orientation` and a clean `main` worktree, with `connectedness = FALSE` (S6 in
`REVIEW-NOTES-SUMMARY.md` — the single-row shapes error under the default). Residual df is
`n − 1 − (k−1) − (r−1) − (c−1)`:

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

**Why clamping would have been wrong.** Capping or `NA`-ing anything above 1 hides the confounded case
(row 4) behind a plausible value instead of reporting that the design cannot support the estimate. The
refusal is the point, not the bound.

**Fallout in the existing tests, all of it real.** Four tests asserted a finite, positive value for designs
`lm()` also calls non-estimable, and one `@examples` block used such a design — it would have failed
`R CMD check` once the gate existed. Each was replaced with an estimable fixture, keeping the test's
original intent, plus new tests pinning the refusals and the `residual df 0` boundary that must **not** be
rejected. The comparator in "provides better result for an optimised design" is the clearest case: it was
comparing against a number that does not exist.
