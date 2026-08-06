# Review notes: grid construction and core metrics

**Scope:** `R/design_utils.R` (`build_design_matrix()`), `R/calculate_adjacency_score.R`,
`R/metrics.R`. Branch **`bugfix/grid-orientation`** off `main`.

**Companion files** — one per workstream:

| File | Workstream |
|---|---|
| `REVIEW-NOTES.md` | `feature/incidence` (PR #97) — `R/incidence.R` |
| `REVIEW-NOTES-SUMMARY.md` | the merged `summary()` work — `R/summary.R` |
| `REVIEW-NOTES-PR91.md` | PR #91 `info-objective` |
| **this file** | grid construction / core metrics |

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
> ⬜ **Deferred, each its own PR: G8, G9, and the S3 class collision (A3.1).** None is an ordering bug.

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
| **G6** `calculate_efficiency_factor()` couldn't compute for a buffered design (`KNOWN_ISSUES` #1b) | **resolved as a side effect of G7** — see A1.1 |
| **G7** `calculate_efficiency_factor()` filled `Z` positionally, returning a different value per row ordering (0.111 vs 0.625 on a 2×6, and values `> 1`) | `Z` is indexed by each plot's own coordinates |
| **S1** `.neighbour_balance()` reported self-adjacencies that didn't exist (6 where the truth was 0) | reads coordinates; the 4×3 fixture now returns the hand-derived truth (self 0, pair min/max 5/6) |
| **A5** lexical factor levels (`1, 10, 11, 2, …`) defeated the row-major sort, so grid metrics scored a layout that wasn't the design | coordinate construction is immune; the sort is no longer load-bearing (see A4) |

### A1.1 G6 fell out of the G7 fix — verify before closing `KNOWN_ISSUES` #1b

Unplanned, so it is worth confirming rather than assuming. `calculate_efficiency_factor()` now indexes
`Z_row`/`Z_col` by `rows`/`cols` directly, which tolerates the coordinate offset `add_buffers()`
introduces: the empty leading indicator column makes `ZtZ` singular, the existing `kappa()` check
routes to `pseudo_inverse()`, and the empty column contributes nothing. **Measured** on a 4×3 design:

| | efficiency |
|---|---|
| unbuffered | 0.9375 |
| `add_buffers("edge")` — rows 2-5, cols 2-4 | **0.9375** |
| `add_buffers("row")` — rows 2, 4, 6, 8 | **0.9375** |
| 4×6 with one plot removed mid-grid (a genuine hole) | **0.7009**, no error |

All correct: buffers are not part of the statistical design, so the value should equal the unbuffered
design's, and it does. `#1b` recorded this as uncomputable, and `.efficiency_factor()`'s `tryCatch` no
longer has a known failure to absorb.

Note the buffered rows of this table become moot once `add_buffers()` leaves speed (D6), but the holed-grid
row does not — a genuine missing plot is a real input class, and it is the reason `#1b` can be closed on
its merits rather than by removing the feature that exposed it.

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
moving to \pkg{biometryassist} (see `BUFFERS-HANDOFF.md`). Once it is gone, speed never creates a
displacement, so the restoration goes too and raw coordinates are simply correct with nothing to undo.

This also settles S3 in `REVIEW-NOTES-SUMMARY.md`: `main`'s `length(unique(...))` behaviour was the
right *behaviour*; the defect was that the choice had never been stated.

G4's `NA` tolerance stays necessary regardless — a design with a genuine partial hole still produces a
sparse grid under raw coordinates.

### A2.1 What arrives when `feature/buffers` merges

Branched off `f5d68f5`, so it applies cleanly. Two items below are already done there — **do not action
them again**:

| From `feature/buffers` | Effect here |
|---|---|
| `metadata$buffer` transform record in `add_buffers()`, inverted by `.drop_buffer_rows()` / `.restore_buffer_coords()` | makes D6 true without touching `build_design_matrix()` |
| `test-summary.R` buffer test rewritten | **fixes the stale comment at [test-summary.R:304](tests/testthat/test-summary.R#L304)**, which still claims a `"row"` buffer should change the counts. Now asserts every buffer type and stacked combinations match the unbuffered design |
| `add_buffers()` deprecation warning + `## Deprecations` NEWS section | buffers are leaving speed; `BUFFERS-HANDOFF.md` specifies the biometryassist side |
| `.warn_if_buffers()` in `calculate_adjacency_score()`, `calculate_balance_score()`, `calculate_efficiency_factor()` | a direct metric call on a buffered frame bypasses `.drop_buffer_rows()`, so it warns rather than silently scoring the displaced layout |
| `helper-buffers.R` with `add_buffers_quiet()`, and 45 rewritten test call sites | keeps the deprecation warning out of tests that are about layout |

One caveat carried forward: the `metadata$buffer` record is an affine `scale`/`shift` pair, which covers
speed's buffer types but **cannot** represent biometryassist's `by =` block buffers, where gaps appear
only at group boundaries. It would need to become a per-axis `new -> old` lookup if speed ever had to
invert one of those. Under the handoff plan it never does.

---

## A3. Open findings

### A3.1 🟠 Both packages register S3 methods on class `"design"`

Not an ordering bug and not buffer-specific, but it surfaced from this work and needs its own branch.
speed and \pkg{biometryassist} both use `class(x) == c("design", "list")`, and speed registers methods on
a class name it does not own. **Measured** with both loaded, calling on a **biometryassist** design:

| Call | Result |
|---|---|
| `summary(des)` | speed's `summary.design` runs → `"This design has no metadata; ... Re-run speed()"` |
| `print(des)` | speed's `print.design` runs → prints `"Optimised Experimental Design"`; wrong, and silent |
| `autoplot(des)` | last package loaded wins (`Registered S3 method overwritten by ...`) |

biometryassist defines neither `print.design` nor `summary.design`, so speed's capture its objects
unopposed. `print()` is the worst of the three: no error, just plausible wrong output.

**Fix:** one line at [R/speed.R:441](R/speed.R#L441) —
`class(output) <- c("speed_design", "design", class(output))` — then rename speed's four registrations to
`autoplot.speed_design`, `print.speed_design`, `summary.speed_design`, `print.summary.speed_design`.
Keep `"design"` in the vector so `inherits(x, "design")` still works. This also gives the biometryassist
adapter its discriminator (`inherits(x, "speed_design")`), replacing a `[["design_df"]]` sniff — see
`BUFFERS-HANDOFF.md` change 5.

### G9 🟠 `initialise_design_df()` fills `items` down columns, and nothing says so

Found while fixing G7, because it is what made the paper-comparison test fail.

`initialise_design_df()` builds its grid with `expand.grid(row = 1:nrows, col = 1:ncols)`
([R/design_utils.R:294](R/design_utils.R#L294)), which varies `row` fastest, then assigns
`df$treatment <- items` positionally. So `items` is read **down columns**. Nothing in
`?initialise_design_df` says so, and the natural way to write a design out — one grid row per source
line — produces a *different design* from the one on the page.

The package's own test suite fell into it. `test-calculate_efficiency_factor.R` writes four published
designs visually, 4 rows of 9, and asserted the paper's values. **Measured:**

| Design | as written | grid actually matching the paper | paper's value |
|---|---|---|---|
| 1 | 0.644 | **0.834** | 0.834 |
| 2 | 0.683 | **0.783** | 0.783 |
| 3 | 0.540 | **0.827** | 0.827 |

It passed on `main` only because **two conventions cancelled**: `initialise_design_df()` stored the
design column-major and `calculate_efficiency_factor()` read it back positionally row-major (G7),
recovering the design the author wrote. Fixing G7 broke the cancellation and exposed both. Supplying
the items column-major — `as.vector(matrix(items, nrow, ncol, byrow = TRUE))` — reproduces every
published value exactly, which is the confirmation that the G7 fix is right and the storage was wrong.

**Worked around in the tests** via a local `by_row()` helper, so the literals stay readable against the
paper. **Not fixed in the package** — that is a user-facing API question:

- At minimum, document the fill order in `?initialise_design_df` with a worked example.
- Better, add `byrow = FALSE`, mirroring `matrix()`. Anyone transcribing a published design wants
  `byrow = TRUE` and silently gets a different design today.

Check the other tests and the vignettes for the same latent transposition before that lands.

### G8 🔵 `Z` omits the intercept

`calculate_efficiency_factor()` builds `Z` from row indicators `1..R-1` and column indicators `1..C-1`
with **no column of ones**. Its column space has dimension `R+C-2` and does not contain the intercept,
where the row + column model space has dimension `R+C-1`, so `A_RC` is not the mean-adjusted treatment
information matrix.

**Measured:** for equireplicate designs it cancels exactly — the returned value matched the harmonic
mean of the canonical efficiency factors to machine precision on five non-square designs (3×8, 4×6,
6×4, 2×10, 5×6), re-verified on a properly randomised 3×8 (0.7040535). Under **unequal replication** it
does not: on the 25×12 p-rep example the function returns **0.267052** where a properly adjusted `C`
gives **0.268757**.

Statistical, not orientation. Pair it with the upper-bound work (A4).

---

## A4. Deferred, recorded

- **A-efficiency upper bound in `summary()`.** 🔷 **Decided 2026-08-06: its own branch.** A closed-form
  bound on the average efficiency factor, depending only on `(replication, nrow, ncol)` — no matrices,
  essentially free — so `summary()` can report how close a design gets to the best achievable
  A-efficiency:

  `UB = (1/(t-1)) * sum_i [ 1 - minSumSq(r_i, nrow)/(ncol*r_i) - minSumSq(r_i, ncol)/(nrow*r_i) + r_i/n ]`

  where `minSumSq(r, K)` is the even-split minimum of `sum n_ik^2`. **Measured:** holds as a bound on
  every design tested, equals exactly 1.000 for 4×4 and 5×5 Latin squares (which are A-optimal), and
  tracks the optimiser — a 5×6 10-treatment design moves 0.000 → 0.620 → 0.774 against a bound of 0.815
  as iterations go 0 → 200 → 5000.

  Report it as **"% of upper bound"**, never "% of optimal": `A/UB = 1` proves A-optimality, `A/UB < 1`
  does not prove sub-optimality, because the bound may be unattainable.

  **Explicitly declined:** reporting the raw A-value (average pairwise variance). Already computed and
  discarded inside `calculate_efficiency_factor()`, but it is in σ² units, only comparable across
  designs with identical replication, and actively misleading when the design is disconnected —
  measured, an unoptimised 5×6 reports 0.503 against the optimised design's 0.862, which looks better
  and is a `ginv` artefact of the rank deficiency.

  **Reuse check:** PR #91 (`REVIEW-NOTES-PR91.md`) already computes canonical efficiency factors from
  the information matrix via `eigen()`, in a function confusingly named `calculate_efficiency_factors`
  (plural). Both the bound and G8 want that machinery. Coordinate the two before writing a third
  implementation.
- **Removing the sort at [R/speed.R:195](R/speed.R#L195).** With grids coordinate-based it is no longer
  needed for correctness — the order-invariance tests are what prove that. But `generate_neighbour()`,
  `random_initialise()`, `print.design()` and `autoplot()` may rely on row order. Leave it; note as a
  later simplification, not bundled with a bug fix.
- **Hot-loop performance.** Measured on a 700-plot design (28×25), 2000 builds: `matrix()`
  **415 µs/build** vs `build_design_matrix()` **1180 µs/build** — **2.84×**, about 7.6 s extra per
  10,000 iterations per level. Real but not disqualifying, and avoidable: the row/col vectors never
  change during the SA loop, only `swap` does, so the validated `cbind(rows, cols)` index can be built
  once per level and passed in. Do it after correctness, and benchmark rather than assume.
- **A terminology sentence for `?calculate_efficiency_factor`.** It returns `(2/r_h) / apv`, the
  **average efficiency factor** — the harmonic mean of the canonical efficiency factors, i.e.
  **A-efficiency**, the measure paired with A-optimality. Verified against an independent eigenvalue
  computation on five equireplicate designs (exact to machine precision); nothing in `R/` on `main`
  calls `eigen()`, so it cannot be computing an E-efficiency (the *minimum* canonical efficiency
  factor). `summary()`'s "A-efficiency" label is correct.

  The confusion is a symbol collision: Williams & Piepho write the average efficiency factor as **`E`**
  (for **E**fficiency, often `E_A`) — see `Mario speed-eg3-jac12463.R`, *"The average efficiency factor
  is E = 0.411"*. That `E` is not E-optimality. Separately, when all canonical efficiency factors are
  equal (Latin squares, BIBDs) A-, D- and E-efficiency coincide exactly, so agreeing with another
  package on one design proves nothing about which criterion it used. Name the synonym in the docs so
  this doesn't recur.

---

## A5. Corrections that still matter

Corrections to superseded findings have been dropped along with the findings. These bear on open items.

| Earlier claim | Corrected |
|---|---|
| **Rank** the coordinates and you destroy real physical gaps, so validate instead | **Right, and it survived a detour.** Briefly overruled in favour of ranking; reinstated once it was clear that ranking is only the inverse of `add_buffers()`' displacement. Undo the displacement at its source and raw coordinates preserve genuine gaps at no cost. See D6. |
| D6 is a statistical question about whether buffers separate plots | **Not really.** It looked like one, but the only thing making a buffered design score differently was `add_buffers()` rewriting the real plots' coordinates. Verified: ranking the de-buffered coordinates restores the original `1..n` exactly, for all five buffer types. The statistical question is settled trivially — buffers must not change anything — and the rest was an implementation leak. |
| **G10** — `build_design_matrix()` must rank its coordinates to satisfy D6 | **Withdrawn, do not implement.** Ranking in `build_design_matrix()` would infer the undo in the wrong place and collapse real holes as collateral. `feature/buffers` records the displacement in `metadata$buffer` and inverts it in `.drop_buffer_rows()` instead; `build_design_matrix()` stays raw. G10's other three sub-items are also resolved: the `test-summary.R` comment is rewritten on that branch, the NEWS sentence has already been removed here, and `calculate_efficiency_factor()` needs no change (A1.1). |
| An efficiency factor `> 1` is a canary for the ordering bug (G7) | **Too narrow.** `> 1` signals rank deficiency however it arises. Measured: degenerate fixtures where treatment is confounded with row — which is what `initialise_design_df(rep(LETTERS[1:k], m), ...)` produces, see G9 — return values `> 1` in **row-major** order too, on `main`. It is a canary for "something is wrong", not for ordering specifically. |
| G8 is harmless for equireplicate designs | **Stands, re-verified.** Earlier doubt came from degenerate fixtures (G9), not from G8. |
| `KNOWN_ISSUES` #1b: `calculate_efficiency_factor()` cannot compute post-buffer (G6) | **No longer true** — resolved incidentally by the G7 coordinate fix. Measured in A1.1. |
