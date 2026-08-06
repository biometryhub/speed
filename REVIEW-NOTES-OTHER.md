# Review notes: grid construction and core metrics

**Scope:** `R/design_utils.R` (`build_design_matrix()`), `R/calculate_adjacency_score.R`,
`R/metrics.R`. Branch **`bugfix/grid-orientation`** off `main`. Contains the most consequential
correctness work in any of these notes.

**Companion files** — one per workstream:

| File | Workstream |
|---|---|
| `REVIEW-NOTES.md` | `feature/incidence` (PR #97) — `R/incidence.R` |
| `REVIEW-NOTES-SUMMARY.md` | the merged `summary()` work — `R/summary.R` |
| `REVIEW-NOTES-PR91.md` | PR #91 `info-objective` |
| **this file** | grid construction / core metrics |

**Last verified:** 2026-08-06, R 4.6.1, `pkgload::load_all()`. `bugfix/grid-orientation` at `ef9bf17`,
`feature/incidence` at `1536991`. All numbers measured, not inferred.

> ✅ **Status: G1–G4 implemented on `bugfix/grid-orientation`.** `build_design_matrix()` is written
> fresh off `main` (validated coordinates, no renumbering, duplicate-coordinate guard), wired into
> `calculate_adjacency_score()` and `objective_function_piepho()` **including the G2 write-back fix**,
> with `.calculate_nb()` made NA-tolerant. Two new test files pin the behaviour. The findings below are
> kept as the rationale for the change and as review material.
>
> ⬜ **Still open: G5, G6, G7, G8, and the `.neighbour_balance()` call site (S1).** G7 and S1 are new
> to this revision and are the same bug class as G1 — two further grid-construction sites that were
> missed because neither `R/summary.R` nor `calculate_efficiency_factor()` was in the original diff.
> Both are in scope here; G6 and G8 are not.
>
> `feature/incidence` still carries its own earlier copy of this work, which must be stripped —
> in that state it is a **regression**, not a fix (G2).

---

## A1. Summary

- 🔴 **G2** — the coordinate-based grid refactor on `feature/incidence` makes
  `objective_function_piepho()` **worse**: it fixes two score components and corrupts the other two.
  Must not merge as-is.
- 🟠 **G1** — **four** functions on `main` assume a data ordering and none reads coordinates; two
  assume row-major and two column-major, so they disagree with each other.
  `calculate_adjacency_score()` returns 6 where the truth is 0 when handed
  `initialise_design_df()`'s own output. (Originally written as two functions — see G7 and S1.)
- 🟡 **G3** — `build_design_matrix()` doesn't validate its coordinates; a `row` value of 0 causes
  silent data loss.
- 🟡 **G4** — coordinate placement can produce **sparse** grids, and `.calculate_nb()` errors on them.
  This is the genuine fragility cost of the approach, and the default code path hits it.
- 🟡 **G5** — `calculate_adjacency_score(ring_dists = c(1, 2))` errors on `main`; the documented
  default is unusable.
- 🟡 **G6** — `calculate_efficiency_factor()` fails post-buffer (KNOWN_ISSUES #1b). This is only the
  **sparse-lattice** half of that function's grid problem; the ordering half is G7.
- 🟠 **G7** — `calculate_efficiency_factor()` builds its row/column indicator matrix positionally and
  ignores the coordinates. On a **dense** grid it does not error — it silently returns a *different
  number* for the same design in a different row order (measured 0.111 vs 0.625 on a 2×6). Same failure
  mode as G1, in a third function; G1's table was missing it.
- 🟠 **S1** — `.neighbour_balance()` ([R/summary.R:950](R/summary.R#L950)) has the identical bug and
  reports self-adjacencies that do not exist (measured 6 where the truth is 0). Fully documented as
  **S1 / S-D1** in `REVIEW-NOTES-SUMMARY.md`; recorded here because the fix now belongs in this branch.
- 🔵 **G8** — `calculate_efficiency_factor()`'s `Z` omits the intercept, so it projects onto a subspace
  one dimension smaller than the row + column model. Harmless for equireplicate designs, not for
  unequal replication. Statistical, not orientation — **out of scope here**.

## A2. Decisions

### 🔷 D6. Does a buffer break adjacency? — **answer this first; it determines G3**

*(Cross-cutting: also affects `summary()` — see S3 in `REVIEW-NOTES-SUMMARY.md`.)*

Coordinate-based construction forces this into the open, and there's no implementation-neutral answer.
`add_buffers()` shifts or scales coordinates: `type = "edge"` gives inner rows `2..n+1`;
`type = "row"` gives inner rows `2, 4, 6, 8`. Once buffer rows are dropped the inner design's
coordinates are non-contiguous. Two ways to rebuild, verified on a design with rows 1, 2, 4, 5 (a road
where row 3 would be):

```
raw coordinates (gap kept)     ranked coordinates (gap removed)
  A  B                           A  B
  C  C                           C  C
 NA NA                           C  C     <- now counted as adjacent
  C  C                           A  B
  A  B
adjacency = 2                  adjacency = 4
```

Ranking invents two C–C adjacencies across the road.

- **Raw coordinates** — plots either side of a buffer or gap are *not* neighbours. Agronomically the
  defensible reading, and my recommendation. Cost: grids can be **sparse**, which some code can't
  handle (G4).
- **Ranked coordinates** (`match(x, sort(unique(x)))`) — they *are* neighbours. Cost: silently changes
  the geometry and destroys real physical gaps.

⚠️ **`main` already made this choice implicitly, in the ranked direction** — `summary()`'s
`length(unique())` dimension fix rebuilds a row-buffered 4-row design from rows 2, 4, 6, 8 as a
contiguous 4×4 grid. So this isn't a greenfield decision; it's a question of whether to keep an
unstated one. Whichever way it goes, `summary()` and the objective functions must agree.

**Recommendation:** raw coordinates everywhere, plus renumbering *inside* `add_buffers()` if you want
buffered designs to stay contiguous — fix it where the offset is introduced, not in every consumer.

### 🔷 D1. Extract the grid work from `feature/incidence` into this branch? — **recommended: yes**

Of the four `R/` files `feature/incidence` touches, only `R/incidence.R` is the feature; the other
three (`R/design_utils.R`, `R/calculate_adjacency_score.R`, `R/metrics.R`) are this workstream. The
grid work is a correctness fix affecting anyone who used `objective_function_piepho()`, and it's
currently gated on the API review of two new functions.

Note this is a *larger* change than it first looks — it has to include G2 and G4, or piepho gets worse
rather than better.

- **Yes** → `bugfix/grid-orientation` off `main`; rebase `feature/incidence` on it.
- **No** → keep it in PR #97, but G2 and G4 are still mandatory before merge.

## A3. Findings

### G1 🟠 Four functions, opposing ordering assumptions

None reads coordinates; each hardcodes an assumption about data order, and they disagree:

| Function | Fill on `main` | Correct inside `speed()` (row-major)? | Correct on raw `initialise_design_df()` (column-major)? |
|---|---|---|---|
| `calculate_adjacency_score()` | `matrix(..., byrow = TRUE)` | ✅ | ❌ |
| `objective_function_piepho()` | `matrix(...)` column-major | ❌ | ✅ |
| `calculate_efficiency_factor()` | positional `plot_index` loop, row-major (G7) | ✅ | ❌ |
| `.neighbour_balance()` | `matrix(...)` column-major (S1) | ❌ | ✅ |

They split two-and-two, each pair wrong exactly where the other is right. `speed()` sorts row-major at
[R/speed.R:195](R/speed.R#L195); `initialise_design_df()` emits column-major via
`expand.grid(row = 1:nrows, col = 1:ncols)` ([R/design_utils.R:294](R/design_utils.R#L294)).

**Measured:** `calculate_adjacency_score()` on a 2×3 design straight from `initialise_design_df()`
returns **6** where the truth is **0**. The function is exported and its own examples use hand-written
row-major data, so they pass and the inconsistency is invisible. Exported functions in the same package
that silently disagree about layout.

**Revised 2026-08-06: it is four functions, not two.** `calculate_efficiency_factor()` (G7) and
`.neighbour_balance()` (S1) make the same class of assumption and were missed because neither
`R/summary.R` nor the efficiency code was in the original review diff. The count in the original
finding was an undercount, not a wrong call.

`build_design_matrix()` fixes `calculate_adjacency_score()` cleanly, with no side effects. Piepho is
not so simple — see G2.

### G2 🔴 The piepho refactor scrambles the treatment column

[R/metrics.R:244](R/metrics.R#L244), unchanged by the branch:

```r
design[[swap]] <- as.factor(design_matrix)     # write the flattened grid back
bal_score <- calculate_balance_score(design, swap, spatial_cols)
adj_score <- calculate_adjacency_score(design, swap, row_column, col_column)
```

Flattening a matrix in R is **column-major**. On `main` the grid was *also* filled column-major, so
this round-tripped exactly — verified `identical()`, i.e. line 244 was a **no-op**. With
coordinate-based filling the grid is the true layout, so flattening it column-major no longer matches a
row-major data frame, and the treatment column is silently permuted before `bal_score` and `adj_score`
are computed on it.

Measured on a 2×6 in **row-major** order — what `speed()` actually passes:

| | neighbour_balance | even_distribution | balance | adjacency | score |
|---|---|---|---|---|---|
| `main` | 1.3333 ❌ | 0.2357 ❌ | 2 ✅ | 0 ✅ | 3.569 |
| `feature/incidence` as-is | 0.3333 ✅ | 0.1975 ✅ | **8** ❌ | **6** ❌ | **14.53** |
| with the fix below | 0.3333 ✅ | 0.1975 ✅ | 2 ✅ | 0 ✅ | **2.531** |

Ground truth for that layout is `balance = 2`, `adjacency = 0`. The branch trades two wrong components
for two different wrong ones. On a 4×6 it's worse: the branch reports `balance 9.333, adjacency 0`
where the truth is `balance 36, adjacency 20`. Inside a real
`speed(obj_function = objective_function_piepho)` run the optimiser drives the **corrupted** objective
to near-zero adjacency, producing a design with 20 like-treatment adjacencies.

**Fix — delete the write-back.** Its only surviving effect was factor coercion:

```r
design[[swap]] <- as.factor(design[[swap]])    # was: as.factor(design_matrix)
```

Verified this restores every component to truth on 2×6, 4×6 and 3×3, and — the real prize — makes
piepho **order-invariant**: the same physical layout supplied row-major or column-major now scores
identically (2.530786 both ways). That invariance is the entire point of coordinate-based construction
and it does not hold until line 244 is fixed. It also removes one of the two sparse-grid failures in
G4 (the `replacement has 10 rows, data has 8` error came from this line).

### G3 🟡 `build_design_matrix()` doesn't validate its coordinates

It uses `row`/`col` directly as matrix indices, so it needs positive integers. Verified failure: a
`row` value of `0` gives `number of items to replace is not a multiple of replacement length` plus
silent data loss, because index 0 is dropped by matrix indexing. Negative values error less helpfully.

Fix by **validating, not transforming** — see D6 for why ranking is unsafe. Let sparse-but-valid
coordinates through as `NA` cells (G4).

**Not a new problem:** non-numeric coordinate labels (`"R1"`, `"C1"`) make `as_numeric_factor()` return
`NA`, and *both* the old `matrix()` approach and the new one warn identically, because the old code
already used it for dimensions. Shared pre-existing limitation, not a regression.

### G4 🟡 Sparse grids are a new input class, and some code can't take them

This is the real fragility cost of coordinate placement. The `byrow` fill **can never** produce `NA`;
coordinate placement can, whenever the lattice has a hole — a road, an irregular trial edge, or a
dropped buffer row under D6's raw-coordinate option.

Verified on a grid with rows 1, 2, 4, 5:

| Consumer | Sparse grid | Notes |
|---|---|---|
| `calculate_adjacency_score()` | ✅ returns 2 | `adjacency_score_vec()` is documented to treat NA pads as 0 |
| `calculate_nb(m, pair_mapping)` | ✅ | NA pairs fail the mapping lookup and `table()` drops them |
| `calculate_nb(m)` — **no** mapping | ❌ **errors** | `.calculate_nb()` does `if (node < bottom)`, which is `NA` |
| `calculate_ed()` | ⚠️ runs | Distances reflect the gap, arguably correct — but unpinned |
| `objective_function_piepho()` | ❌ **errors both ways** | via `.calculate_nb()`, and separately via G2's line 244 |

`pair_mapping` defaults to `NULL`, so the NA-intolerant path is the **default** one. Verified that a
real `add_buffers(d, "edge")` design with buffer rows dropped reaches it:

```r
objective_function_piepho(inner, "treatment", c("row", "col"))
#> ERROR: missing value where TRUE/FALSE needed
```

**Fix:** guard `.calculate_nb()` ([R/metrics.R:330](R/metrics.R#L330)) against `NA` neighbours — skip
the pair, matching what the `pair_mapping` path already does. Add a sparse-grid test for
`calculate_ed()`: it runs, but the behaviour is unpinned and it uses `NA` internally as a "not this
treatment" sentinel, so the interaction deserves an explicit check rather than an assumption.

Until G2 and G4 are both done, `build_design_matrix()` cannot safely be wired into piepho for anything
but a dense 1-based grid.

### G5 🟡 `ring_weights` doesn't recycle

Verified live on `main`:

```r
calculate_adjacency_score(d, "trt", ring_dists = c(1, 2))
#> ERROR: length(dists) == length(weights) is not TRUE
```

`adjacency_score_vec()` asserts equal lengths but `ring_weights` defaults to scalar `1`, so the
documented default is unusable with multi-ring `ring_dists`. Recycle `weights` to `length(dists)`.

### G6 🟡 `calculate_efficiency_factor()` fails post-buffer (KNOWN_ISSUES #1b)

Related root cause to G3. It derives `n_rows`/`n_cols` from `max()` then fills with
`for (i in 1:n_rows) for (j in 1:n_cols)` assuming `n_rows * n_cols == n_plots` — which a buffered or
sparse design violates. `summary()`'s `.efficiency_factor()` wrapper catches the error and degrades to
`available = FALSE`, so it fails safely rather than fabricating a number (see S4 in
`REVIEW-NOTES-SUMMARY.md`).

**Why a buffered design violates it:** `add_buffers()` (R/buffers.R) shifts or scales the real
design's coordinates — `type = "edge"` does `design$row <- design$row + 1` before appending the buffer
rows, `type = "row"` doubles them. Stripping the buffer rows back out does not undo the offset, so the
remaining plots no longer occupy a contiguous 1-indexed lattice and `n_rows * n_cols` exceeds
`n_plots`. Fixing it at the source, inside `add_buffers()`, is the direction recorded in D6.

Making it handle a sparse plot set is a bigger job than the other items here — the row/col indicator
matrices assume a complete lattice. **Keep it separable**; possibly its own small PR.

⚠️ **Scope correction (2026-08-06).** As written this finding covers only the *sparse* case, and its
"fails safely" conclusion is true only there. The same loop has a second failure mode on a **dense**
grid where it does not error at all — see **G7**, which is in scope for this branch. Fixing G7 does not
fix G6: coordinates make the fill order-independent, but the indicator matrices still assume a complete
lattice.

### G7 🟠 `calculate_efficiency_factor()` is order-dependent on dense grids

[R/metrics.R:694-707](R/metrics.R#L694-L707) walks a `plot_index` counter through nested
`for (i in 1:n_rows) for (j in 1:n_cols)` loops to build the row and column indicator matrices.
`row_column`/`col_column` are used **only** for `max()` to get the dimensions — the coordinate values
themselves are never read. So the function assumes a complete rectangular grid in row-major order, and
silently scores a different layout when it doesn't get one.

**Measured**, same physical design supplied two ways:

| Design | Column-major | Row-major | True A-efficiency |
|---|---|---|---|
| 2×6, 4 trt, r=3 | **0.111111** ❌ | 0.625000 ✅ | 0.625000 |
| 4×3, 3 trt, r=4 | **1.500000** ❌ | 0.937500 ✅ | 0.937500 |

The 4×3 case returns an efficiency factor **greater than 1**, which is not a possible value — a useful
canary, since it means the failure is not always silent.

For a square grid the two orderings are a clean transpose and `Z`'s column space is unchanged, so the
result is identical and correct; the bug only bites on non-square grids. Verified correct against an
independent eigenvalue computation for `speed()` output (3×8: 0.659612 both ways), because
[R/speed.R:195](R/speed.R#L195) sorts row-major — the same accident of ordering that hid G1.

**Where it bites today:** direct calls with `initialise_design_df()` output, which is column-major.
That includes the function's own documented example at
[R/metrics.R:656-662](R/metrics.R#L656-L662) — a 3×4 grid, scored as though it were laid out
differently.

**Fix:** build `Z` from `model.matrix(~ factor(row) + factor(col))` (or the coordinates directly)
instead of the positional loop. Cheap, and independent of G6.

### S1 🟠 `.neighbour_balance()` reports adjacencies that don't exist

Full write-up is **S1 / S-D1** in `REVIEW-NOTES-SUMMARY.md`; summarised here because the fix now
belongs in this branch rather than a separate one.

[R/summary.R:950](R/summary.R#L950) rebuilds the grid with `matrix(df[[swap]], nrow, ncol)` — a
column-major fill of a data frame `speed()` sorts row-major. **Measured** on a 3×8, 6-treatment design
optimised to a genuine zero:

```
summary() grid (matrix fill)      true field layout (build_design_matrix)
C B E E C A E F                   C D C B D A E F
D D F D E A D A                   F E D F C E B A
C A F F B C B B                   A C E D B F A B

self-adjacency reported by summary(): 6
self-adjacency in the actual field :  0
```

Every figure in the block — `min`, `max`, `pair_var`, `n_zero_pairs` — is computed on the scrambled
grid. The optimiser is doing its job and `summary()` misreports it.

**Why it lands here now:** S-D1 offered (A) adopt `build_design_matrix()` "once it exists on `main`" or
(B) a local fix in `R/summary.R`. Option A was the recommendation and its stated blocker is satisfied —
`build_design_matrix()` is on this branch. `REVIEW-NOTES-SUMMARY.md`'s own plan is now stale on this
point.

⚠️ The existing test rebuilds its expectation with the *same* `matrix()` call the implementation uses,
so it is self-fulfilling and passes against the bug. It must be rewritten, not just re-run — see
`REVIEW-NOTES-SUMMARY.md`.

### G8 🔵 `Z` omits the intercept — out of scope, recorded

[R/metrics.R:694-710](R/metrics.R#L694-L710) builds `Z` from row indicators `1..R-1` and column
indicators `1..C-1` with **no column of ones**. Its column space therefore has dimension `R+C-2` and
does not contain the intercept, where the row + column model space has dimension `R+C-1`. `A_RC` is
consequently not the mean-adjusted treatment information matrix.

**Measured:** for equireplicate designs this cancels exactly — the returned value matched the harmonic
mean of the canonical efficiency factors to machine precision on five non-square designs (3×8, 4×6,
6×4, 2×10, 5×6). Under **unequal replication** it does not: on the 25×12 p-rep example the function
returns **0.267052** where a properly adjusted `C` gives **0.268757**.

Statistical, not orientation. Fix it alongside the upper-bound work (A4.7), not here.

## A4. Plan: `bugfix/grid-orientation`

Branch created off `main`. Order matters: **D6 → G3 → G4 → G1/G2**. Wiring the call sites before the
consumers tolerate sparse grids reproduces G4's errors.

| Step | Status |
|---|---|
| A4.1 `build_design_matrix()` | ✅ done |
| A4.2 `.calculate_nb()` NA tolerance (G4) | ✅ done |
| A4.3a call sites — piepho + adjacency, incl. the G2 write-back fix | ✅ done |
| A4.3b call site — `.neighbour_balance()` (S1) | ⬜ open — unblocked, folded in here |
| A4.3c call site — `calculate_efficiency_factor()`'s `Z` (G7) | ⬜ open |
| A4.4 `ring_weights` recycling (G5) | ⬜ open |
| A4.5 tests | 🟡 partial — G1/G2/G4 pinned; G5, G7, S1 outstanding |
| A4.6 NEWS | 🟡 partial — covers A4.3a only |
| G6 `calculate_efficiency_factor()` sparse lattice | ⬜ open — own PR |
| G8 `Z` omits the intercept | ⬜ open — own PR, with the upper-bound work (A4.7) |

A4.3 was previously ticked ✅ while one of its three listed call sites was untouched; it is split into
a/b/c above so the tick is accurate. `test-grid-orientation.R` currently references neither
`calculate_efficiency_factor()` nor `.neighbour_balance()` — verified by grep, 2026-08-06.

**D6 was implemented as "raw coordinates"** — the recommended option. Coordinates are validated and
used as-is, never renumbered, so a gap in the coordinates stays a gap in the grid. If you decide the
other way, A4.1 is the only function that changes.

### A4.1 Add `build_design_matrix()` to `R/design_utils.R` ✅

Differences from the `feature/incidence` version: coordinates computed once into locals, explicit
validation (G3), and a duplicate-coordinate guard. Written for **D6 = raw coordinates**. The coercion
is wrapped in `suppressWarnings()` so a non-numeric coordinate column produces the explicit error
below rather than an `NAs introduced by coercion` warning first.

```r
build_design_matrix <- function(
  df,
  swap,
  row_column = "row",
  col_column = "col"
) {
  rows <- as_numeric_factor(df[[row_column]])
  cols <- as_numeric_factor(df[[col_column]])
  if (anyNA(rows) || anyNA(cols)) {
    stop(
      "Cannot place the design on a grid: `", row_column, "` and `",
      col_column, "` must be numeric, or coercible to numeric.",
      call. = FALSE
    )
  }
  # Used directly as matrix indices, so they must be positive whole numbers.
  # Deliberately not renumbered: a gap in the coordinates is a real gap in the
  # field, and collapsing it would make non-adjacent plots neighbours.
  if (any(rows < 1 | cols < 1) || any(rows != trunc(rows) | cols != trunc(cols))) {
    stop(
      "`", row_column, "` and `", col_column,
      "` must be positive whole numbers to index a grid.",
      call. = FALSE
    )
  }
  idx <- cbind(rows, cols)
  if (anyDuplicated(idx)) {
    stop(
      "Duplicate (", row_column, ", ", col_column, ") coordinates: the design ",
      "cannot be placed on a single grid. Split multi-site designs by site first.",
      call. = FALSE
    )
  }
  design_matrix <- matrix(NA_character_, nrow = max(rows), ncol = max(cols))
  design_matrix[idx] <- as.character(df[[swap]])
  return(design_matrix)
}
```

Two behaviour changes to watch: the duplicate guard errors where `main` silently truncated, and the
positive-integer guard errors where `main` produced a partially-filled matrix. **Check the MET examples
in `?speed` still run** — they reuse `row`/`col` across sites, and piepho now routes through here.

### A4.2 Make the consumers NA-tolerant (G4)

- `.calculate_nb()` — skip pairs where either cell is `NA`.
- `calculate_ed()` — add a sparse-grid test.

### A4.3 Point the call sites at it

- **`objective_function_piepho()`** ([R/metrics.R:234](R/metrics.R#L234)) — the grid build **and** the
  G2 write-back fix. These must land together; the grid change alone is a regression.
- `calculate_adjacency_score()` ([R/calculate_adjacency_score.R:255](R/calculate_adjacency_score.R#L255))
  — G1. Safe on its own.
- `.neighbour_balance()` ([R/summary.R:950](R/summary.R#L950)) — S1. **Decided: fold in here.** S-D1's
  option A was the recommendation, gated on `build_design_matrix()` existing; it now does (A4.1), so the
  gate is lifted and there is no reason to open `bugfix/summary-neighbour-balance` for it. Under D6's
  raw-coordinate option this changes buffered-design results. The self-fulfilling test noted in S1 must
  be rewritten at the same time.
- `calculate_efficiency_factor()` ([R/metrics.R:694-707](R/metrics.R#L694-L707)) — G7. Replace the
  positional `plot_index` loop with coordinate-driven indicators. Does **not** resolve G6 (sparse
  lattice) or G8 (missing intercept); both stay out of scope.

### A4.4 Fix `ring_weights` recycling (G5)

### A4.5 Tests

- New `tests/testthat/test-build_design_matrix.R`: row-major input, column-major input, non-square,
  factor coordinates with lexical level order, sparse coordinates, `NA` treatment cells, and the three
  new errors (non-numeric, non-positive-integer, duplicate coordinates).
- **Order-invariance for piepho** — the same physical layout as a row-major and a column-major frame
  must score identically. Verified this fails today and passes with G2's fix (2.530786 both ways).
  **This is the single highest-value test in the change**; it's the assertion that actually captures
  what coordinate-based construction buys.
- `objective_function_piepho()` on a non-square grid asserting **hand-derived** values for all four
  components, not just `expect_type()`. Use G2's table as the fixture. The existing piepho tests assert
  types only, which is why 1667 tests pass either side of the refactor.
- `objective_function_piepho()` on a sparse grid, with and without `pair_mapping` (G4).
- `calculate_adjacency_score(initialise_design_df(...), "treatment")` — the direct-call case that
  returns 6 instead of 0 today (G1).
- `calculate_adjacency_score(d, "trt", ring_dists = c(1, 2))` runs (G5).
- **Order-invariance for `calculate_efficiency_factor()`** (G7) — the same physical layout as a
  row-major and a column-major frame must return the same value. Fails today on any non-square grid
  (2×6: 0.111 vs 0.625). Pair it with a fixed-value assertion against an independently computed
  A-efficiency, and a regression test that the 4×3 case no longer returns a value `> 1`.
- **`.neighbour_balance()` on a non-square design** (S1) — a design optimised to zero self-adjacency
  must report zero. Build the expectation from the **coordinates**, never from the same `matrix()` call
  the implementation uses; the existing test does the latter and therefore passes against the bug.

### A4.6 NEWS

```markdown
## Bug Fixes

- `objective_function_piepho()` now builds the design grid from the `row`/`col` coordinates rather
  than assuming the data frame's row order, and no longer overwrites the treatment column with a
  flattened grid. All four score components are now computed on the actual layout, and the score no
  longer depends on the row ordering of the input. Designs generated with this objective should be
  regenerated.
- `calculate_adjacency_score()` is now robust to any row ordering of its input, including the
  column-major output of `initialise_design_df()`.
- `calculate_nb()` no longer errors on designs with missing plots when `pair_mapping` is not supplied.
- `calculate_adjacency_score()` now recycles `ring_weights` against `ring_dists`.
- `calculate_efficiency_factor()` now builds its row and column indicators from the `row`/`col`
  coordinates rather than assuming the data frame's row order, so it returns the same value for a
  design regardless of how its rows are ordered. Previously it could return an incorrect value, or one
  greater than 1, for a non-square design not supplied in row-major order.
- `summary()` no longer reports incorrect neighbour-balance counts (self-adjacency, pair minimum,
  maximum, variance and zero-count) for designs whose grid is not square.
```

The last two bullets are new (G7, S1); the first four were already added for A4.3a. The S1 wording is
shared with `REVIEW-NOTES-SUMMARY.md` — keep one copy, in whichever branch lands first.

### A4.7 Out of scope, recorded

- **Removing the sort at [R/speed.R:195](R/speed.R#L195).** Once grids are coordinate-based *and* G2 is
  fixed, the sort is no longer needed for correctness — the order-invariance test is what proves that.
  But `generate_neighbour`, `random_initialise`, `print.design` and `autoplot` may rely on row order.
  Leave it; note as a later simplification. Don't bundle it with a bug fix.
- **Hot-loop performance.** Measured on a 700-plot design (28×25), 2000 builds: `matrix()`
  **415 µs/build** vs `build_design_matrix()` **1180 µs/build** — **2.84×**, about 7.6 s extra per
  10,000 iterations per level. Real but not disqualifying, and avoidable: the row/col vectors never
  change during the SA loop, only `swap` does, so the validated `cbind(rows, cols)` index can be
  computed once per level and passed in. Do it after correctness, and benchmark rather than assume.
- **G6** — `calculate_efficiency_factor()` sparse lattice / post-buffer; own PR.
- **G8** — the missing intercept in `Z`; own PR, naturally paired with the upper-bound work below since
  both touch the same statistics rather than the grid.
- **A-efficiency upper bound in `summary()`.** 🔷 **Decided 2026-08-06: separate branch, not this one.**
  There is a closed-form upper bound on the average efficiency factor depending only on
  `(replication, nrow, ncol)` — no matrices, essentially free — that lets `summary()` report how close a
  design gets to the best achievable A-efficiency:

  `UB = (1/(t-1)) * sum_i [ 1 - minSumSq(r_i, nrow)/(ncol*r_i) - minSumSq(r_i, ncol)/(nrow*r_i) + r_i/n ]`

  where `minSumSq(r, K)` is the even-split minimum of `sum n_ik^2`. **Measured:** holds as a bound on
  every design tested, equals exactly 1.000 for 4×4 and 5×5 Latin squares (which are A-optimal), and
  tracks the optimiser — a 5×6 10-treatment design moves 0.000 → 0.620 → 0.774 against a bound of 0.815
  as iterations go 0 → 200 → 5000.

  Report it as **"% of upper bound"**, never "% of optimal": `A/UB = 1` proves A-optimality, but
  `A/UB < 1` does not prove sub-optimality, because the bound may be unattainable.

  **Explicitly declined:** reporting the raw A-value (average pairwise variance). It is already computed
  and discarded at [R/metrics.R:733-742](R/metrics.R#L733-L742), but it is in σ² units, only comparable
  across designs with identical replication, and actively misleading when the design is disconnected —
  measured, an unoptimised 5×6 reports an A-value of 0.503 against the optimised design's 0.862, which
  looks better and is a `ginv` artefact of the rank deficiency.
- **Disconnected designs get a healthy-looking efficiency.** On the 25×12 p-rep example the true average
  efficiency factor is 0 (`rank(C) = 251` against `t-1 = 252`), but `pseudo_inverse()`'s `1e-10`
  tolerance drops the null direction and `summary()` prints `0.2671` with no caveat — on the same
  screen as its own `DISCONNECTED - 1 treatment contract(s) not estimable` line. The two outputs
  contradict each other unless the reader joins them up. Suppress or annotate the efficiency value when
  `connectedness$connected` is `FALSE`. Belongs with the `summary()` presentation work, not here.

### A4.8 Terminology note — "A-efficiency" vs "E"

Recorded because it came up as a suspected mislabelling and is not one. `calculate_efficiency_factor()`
returns `(2/r_h) / apv`, the **average efficiency factor** — the harmonic mean of the canonical
efficiency factors, i.e. **A-efficiency**, the measure paired with A-optimality. Verified against an
independent eigenvalue computation on five equireplicate designs (exact to machine precision), and the
package contains no `eigen()` call, so it cannot be computing an E-efficiency (the *minimum* canonical
efficiency factor) at all. `summary()`'s "A-efficiency" label is correct.

The confusion is a symbol collision: Williams & Piepho write the average efficiency factor as **`E`**
(for **E**fficiency, often `E_A`) — see the comment in `Mario speed-eg3-jac12463.R`, *"The average
efficiency factor is E = 0.411"*. That `E` is not E-optimality. Separately, when all canonical
efficiency factors are equal (Latin squares, BIBDs) A-, D- and E-efficiency coincide exactly, so
agreement with another package on one design proves nothing about which criterion it used. Worth a
sentence in `?calculate_efficiency_factor` naming the synonym so this doesn't recur.

## A5. One pre-existing issue worth keeping visible

**Lexical factor levels defeat the row-major sort.** `to_factor()` runs at
[R/speed.R:185](R/speed.R#L185) *before* the sort, so a **character** row column with ≥10 rows gets
levels `1, 10, 11, 2, …` and `order()` follows them. Verified on an 11-row design: the grid `main`
reconstructs is `A J K E C D A F G H I` where the actual layout is `A E C D A F G H I J K`. Any
grid-based metric is then computed on a layout that isn't the design.

Coordinate-based construction fixes this; the sort alone never could. It's also the clearest argument
for why the order-invariance test is worth more than any number of fixed-input assertions.

---

## Corrections to my own earlier findings

| Earlier claim | Corrected |
|---|---|
| Piepho goes **3.569 → 2.531**; the branch fixes it | **Invalid comparison** — `main` measured row-major, branch column-major. On the same row-major input the branch gives **14.53**. Corrected table in G2. |
| The branch's piepho refactor is a clean bug fix | **No** — it fixes NB/ED and breaks balance/adjacency via the line-244 write-back (G2). Net regression until that line changes. |
| **Rank** the coordinates to handle buffer offsets | **Unsafe** — ranking destroys real physical gaps and silently changes which plots are neighbours (measured: adjacency 2 → 4). Validate instead; see G3 and D6. |
| `calculate_nb()` stringifies `NA` into a literal `"NA,A"` pair | **Wrong for the `pair_mapping` path** — NA pairs are dropped cleanly. The no-mapping path errors instead (G4). |
| Non-numeric coordinate labels are a new fragility | **No** — both old and new approaches warn identically; pre-existing shared limitation (G3). |
| Adjacency scoring is broken for all non-square `speed()` designs | **Was wrong**, and was already corrected before this consolidation. `speed()`'s row-major sort matches `byrow = TRUE`. The genuine defects are piepho, direct calls, and lexical factor levels (A5). |
| G1: **two** functions assume opposite orderings | **Undercount** — it is four. `calculate_efficiency_factor()` (G7) and `.neighbour_balance()` (S1) do the same thing and were outside the reviewed diff. Table in G1 corrected. |
| G6 covers `calculate_efficiency_factor()`'s grid problem, and it "fails safely" | **Only half.** G6 is the sparse case, where it errors and the wrapper degrades cleanly. On a dense non-square grid in the wrong order it returns a wrong number silently (G7) — including values `> 1`. "Fails safely" is true of G6, not of the function. |
| A4.3 (call sites) is ✅ done | **Two of three sites only.** `.neighbour_balance()` was listed as conditional and never done; `calculate_efficiency_factor()` was not listed at all. Split into A4.3a/b/c. |
| S-D1 option A is blocked on `build_design_matrix()` reaching `main` | **No longer** — it is on this branch (A4.1). The `bugfix/summary-neighbour-balance` plan in `REVIEW-NOTES-SUMMARY.md` is stale; S1 folds in here. |
