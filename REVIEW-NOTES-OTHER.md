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
> 🔴 **One live blocker: G10.** D6 has now been decided as **ranked** coordinates, and the code
> implements **raw**. A row-buffered design currently reports neighbour counts for a layout nobody
> will analyse. This must be settled before the branch merges, because it is a behaviour change either
> way and the NEWS entry currently describes the raw behaviour.
>
> ⬜ **Deferred, each its own PR: G8, G9.** Neither is an ordering bug.

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

---

## A2. Decisions

### 🔷 D6. Do buffers break adjacency? — **decided: ranked coordinates** (2026-08-06)

**Decision: rank the coordinates, ignoring buffers.** Plots either side of a buffer **are** neighbours.

**Rationale (Sam, 2026-08-06):** when a buffered trial is *analysed*, the buffer plots are excluded and
the model is fitted on the remaining plots, which treats them as a contiguous grid — adjacent even
where they are physically separated. The design metrics should describe the layout the analysis will
actually see, not the physical field. Matching the analysis is the point of the metric.

This also settles S3 in `REVIEW-NOTES-SUMMARY.md`: `main`'s `length(unique(...))` behaviour was the
right *behaviour*, and the only real defect there was that the choice had never been stated. It is now
stated here.

**Cost, accepted:** ranking cannot distinguish a buffer from a genuine hole, so a real physical gap — a
road, an irregular trial edge — is also collapsed, and two plots either side of it are counted as
neighbours. A design with a partial hole (one missing plot mid-row) still produces a sparse grid after
ranking, so G4's `NA` tolerance stays necessary.

Rejected alternative — **raw coordinates**, gaps preserved. Agronomically the more literal reading, and
what is implemented today, but it describes a layout no analysis uses. See G10.

---

## A3. Open findings

### G10 🔴 `build_design_matrix()` uses raw coordinates, contradicting D6

The branch implemented D6 in the **raw** direction before it was decided, and the decision went the
other way. Everything about reading coordinates instead of row order is correct and stays; only the
treatment of gaps is wrong.

`build_design_matrix()` ([R/design_utils.R](R/design_utils.R)) places plots at `max(rows)` × `max(cols)`
and comments that coordinates are *"deliberately not renumbered"*. `add_buffers()` never undoes its own
offset — it does `design$row <- design$row + 1` for `"edge"` and `design$row <- 2 * design$row` for
`"row"` ([R/buffers.R:24-47](R/buffers.R#L24-L47)) — so a de-buffered design arrives with
non-contiguous coordinates.

**Measured** on the 4×3 design, `add_buffers("row")` then buffer rows dropped (inner rows 2, 4, 6, 8):

| Coordinates | self-adj | pair min / max |
|---|---|---|
| raw (today) | 0 | **2 / 3** |
| ranked (D6) | 0 | **5 / 6** |
| the same design unbuffered | 0 | 5 / 6 |

Ranked reproduces the unbuffered design exactly, which is the correct answer under D6 — adding buffers
around a design does not change which of its plots neighbour each other in the analysis. Raw drops
every row-direction adjacency, because every pair of design rows is separated by an empty row.

`add_buffers("edge")` offsets without inserting gaps, so it is already correct under both conventions
(measured: self 0, min/max 5/6, matching unbuffered).

**Fix:** rank inside `build_design_matrix()` — `rows <- match(rows, sort(unique(rows)))`, likewise
`cols`, after the existing validation and before `max()`. One function, as A4.1 originally predicted.
Then:

- `calculate_efficiency_factor()` indexes `Z` by raw coordinate too. It happens to return the right
  value anyway (A1.1), because empty indicator columns contribute nothing, but it should rank for
  consistency and to keep `ZtZ` non-singular rather than leaning on the `kappa()` fallback.
- **`test-summary.R`'s buffer test comment is now wrong.** It currently asserts that `"edge"` counts
  match the unbuffered design *"(A `row` or `block` buffer does insert a gap, and there the counts are
  expected to differ: plots either side of a buffer are not neighbours.)"* Under D6 a `"row"` buffer
  must **not** change the counts either. Add that as a positive assertion — it is the test that pins
  D6.
- **The NEWS bullet must lose its second sentence.** *"Neighbour balance is now read from the plot
  coordinates, so plots separated by a buffer row or column are no longer counted as neighbours"*
  describes the rejected convention.
- Re-check the `?add_buffers` docs and the buffer vignette section for any claim that buffers separate
  plots for scoring purposes.

Whether to *also* renumber inside `add_buffers()` is now moot for metrics — ranking downstream makes it
unnecessary — but it would still make `print()`/`autoplot()` coordinates tidier. Separate question,
separate branch.

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
  once per level and passed in. Do it after correctness, and benchmark rather than assume. Note G10's
  ranking is also loop-invariant and belongs in the same hoist.
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
| **Rank** the coordinates and you destroy real physical gaps, so validate instead | **Half right, and the wrong half won.** Ranking does collapse genuine gaps — that cost stands and is recorded in D6 — but collapsing them is what the analysis does, so it is the correct behaviour for a design metric. D6 decided ranked; the raw implementation is now G10. |
| D6 recommendation: raw coordinates everywhere, plus renumbering inside `add_buffers()` | **Overturned by D6.** Rank downstream in `build_design_matrix()`; renumbering inside `add_buffers()` becomes optional tidying, not a fix. |
| An efficiency factor `> 1` is a canary for the ordering bug (G7) | **Too narrow.** `> 1` signals rank deficiency however it arises. Measured: degenerate fixtures where treatment is confounded with row — which is what `initialise_design_df(rep(LETTERS[1:k], m), ...)` produces, see G9 — return values `> 1` in **row-major** order too, on `main`. It is a canary for "something is wrong", not for ordering specifically. |
| G8 is harmless for equireplicate designs | **Stands, re-verified.** Earlier doubt came from degenerate fixtures (G9), not from G8. |
| `KNOWN_ISSUES` #1b: `calculate_efficiency_factor()` cannot compute post-buffer (G6) | **No longer true** — resolved incidentally by the G7 coordinate fix. Measured in A1.1. |
