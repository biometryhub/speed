# Review notes: `feature/incidence`, PR #91, and the merged `summary()` work

Consolidates and supersedes `PR-incidence-review.md`, `PR-91-alignment-and-integration.md`,
`PR-91-info-objective-review.md` and `PLAN-adjacency-orientation-fix.md`, all four now deleted —
everything still relevant is carried forward here, and §7 records what changed so the reasoning
isn't lost.

`KNOWN_ISSUES.md` is **kept**, trimmed to the two items that are still open. It tracks package-level
issues that outlive any one branch; this file tracks the branch reviews and the plan.

**Last verified:** 2026-08-04, R 4.6.1, `pkgload::load_all()`. `feature/incidence` at `1536991`
(current `main` `a36d302` merged in). PR #91 at `d51d185` (14 commits). All numbers below are
measured, not inferred. Full test suite: **1667 pass, 0 fail**.

> **Revision note.** §3.2, §3.8 and §3.10 were rewritten after stress-testing coordinate-based grid
> construction. Three earlier conclusions were wrong: the piepho before/after comparison was made on
> differently-ordered inputs, the recommendation to *rank* coordinates is unsafe, and the branch
> introduces a **new** blocking bug in `objective_function_piepho()` (§3.3) that the original review
> missed. See §7.

---

## 1. State of play

**Already on `main`** (PR #98, `feature/summary`)

- `summary.design()` / `print.summary.design()` — connectedness, concurrence, block spread,
  replicate span, neighbour balance, opt-in A-efficiency.
- `design$metadata` now carries `row_column`, `col_column`, and per level `swap`, `spatial_cols`,
  `final_score`, `final_components`. **This is new and it changes the plan** — the incidence
  functions no longer need anything added to the `design` object (§3.4).
- `.drop_buffer_rows()` — buffer plots excluded from every summary computation.
- 🔴 **`summary()` reports wrong neighbour-balance numbers on non-square grids** (§3.1). Not caused
  by the summary work — it inherited the package's grid-reshaping assumption.
- 🟠 Pre-existing, unfixed: `calculate_adjacency_score(ring_dists = c(1, 2))` errors (§4.4);
  `calculate_efficiency_factor()` fails post-buffer (§4.5); lexical factor levels defeat the
  row-major sort (§7, closing note).

**On `feature/incidence`** (PR #97)

- `build_design_matrix()` — coordinate-based grid construction. Fixes the neighbour-balance and
  even-distribution halves of `objective_function_piepho()`, and makes `calculate_adjacency_score()`
  composable. Undeclared in `NEWS.md` and untested (§3.2).
- 🔴 **But the piepho refactor introduces a new bug**: line 244's write-back now scrambles the
  treatment column, so the *balance* and *adjacency* components are computed on a corrupted design.
  Net effect on piepho is a regression, not a fix, until that line is also changed (§3.3). **This is
  the single most important thing in this document.**
- `calculate_pair_incidence()` — adjacency pair counts. Maths is correct; matched brute force
  exactly on every case tested. This is the *correct* version of what `main` computes wrongly.
- `calculate_position_incidence()` — treatment × row and treatment × col counts. Correct, but it is
  a thin wrapper over two `table()` calls (§3.7 — **decision needed**).
- Fixable issues: ignores `metadata`, doesn't drop buffers, silently collapses MET designs, no
  validation of coordinates, no tests for `build_design_matrix()`, Air formatting, missing
  `return()` (§3).

**In PR #91** (`info-objective`, kaibagley) — **substantially changed since it was reviewed**

- Now exports **4** functions, not 8: `objective_function_info()`, `compute_L_projection()`,
  `cor_ar1()`, `cor_ar1_ar1()`.
- `calc_incidence_matrix()`, `calc_concurrence_matrix()`, `calc_info_matrix()` and
  `calculate_efficiency_factors()` are now `@keywords internal`. **The naming collision with
  `feature/incidence` is gone**, and so is the argument that `calculate_position_incidence()` is
  redundant against #91. Most of the old alignment document is obsolete (§7).
- `_pkgdown.yml` fixed by the author.
- Still behind `main` (merge base `72a3c94`) — has not merged the `summary()` work.
- 🔴 Still live, re-verified: `spatial_cols` is accepted and never used, so `L`/`Σ` are keyed
  positionally while `speed()` re-sorts rows (§5.1).

**The bottom line on duplication.** Your reading was that the branch duplicates metrics `main`
already has. It's the other way round: `calculate_pair_incidence()` duplicates a metric `main`
computes **incorrectly**, and it is the correct implementation. The fix is to converge them, not to
drop the branch's version.

---

## 2. Decisions you need to make

### 🔷 D1. Split the orientation bug fix into its own PR? — **recommended: yes**

`build_design_matrix()` plus its call-site refactors is a correctness fix affecting anyone who used
`objective_function_piepho()`. It's currently buried inside a feature PR, so it's gated on the API
review of two new functions. Splitting it means the fix ships now and can also fix `main`'s
`summary()` bug (§3.1) in the same change.

Note this is now a *larger* change than the original plan assumed — it has to include the §3.3
write-back fix and the §3.9 sparse-grid work, or piepho gets worse rather than better.

- **Yes** → `bugfix/grid-orientation` off `main`, then rebase `feature/incidence` on it. Plan in §4.
- **No** → keep it in PR #97, but §3.3, §3.9, the NEWS `## Bug Fixes` entry and the §4.5 tests are
  all still required. Merging the branch as it stands would ship a piepho regression.

### 🔷 D2. Keep `calculate_position_incidence()`? — **your call; I lean keep, but generalised**

You recalled correctly: it isn't replaced by anything, but its values are trivially derivable.
Concretely, the whole function is:

```r
table(df[[swap]], df[[row]])   # == result$row (transposed)
table(df[[swap]], df[[col]])   # == result$col (transposed)
```

`calculate_balance_score()` already builds exactly these tables internally and collapses them to a
variance sum. So the function adds **convenience, consistent numeric-aware treatment sorting, and a
documented return shape** — not new computation.

| Option | Trade-off |
|---|---|
| **A. Keep as-is** | Cheapest. Exports a `table()` wrapper; hardcodes row/col, so a user optimising `~ row + col + block` gets two of their three factors. |
| **B. Generalise to `spatial_factors`** (recommended) | `calculate_incidence(design, swap, spatial_factors = ~ row + col + block)` → one matrix per factor. Covers blocks and MET factors, mirrors `speed()`'s own interface, and subsumes PR #91's internal `calc_incidence_matrix()`. Moderate work. |
| **C. Drop it** | Smallest API. Users write `table()`. Loses the sorting fix and the discoverability. |

### 🔷 D3. Does `calculate_pair_incidence()` keep its name?

It is an *adjacency concurrence* matrix. In design theory "incidence matrix" conventionally means
treatment × block, and pairwise co-occurrence is the *concurrence* matrix — so a biometrician
reading `calculate_pair_incidence()` may expect `N Nᵀ` and get adjacency counts. This mattered more
when PR #91 was exporting a competing `calc_incidence_matrix()`; now that it isn't, the pressure is
lower and it's purely a clarity call.

- **Rename** to `calculate_adjacency_concurrence()` — precise, but long, and the branch is named for
  the old term.
- **Keep** and state the relationship to the standard terms in `@description` — cheaper, and fine
  now the collision is gone.

### 🔷 D4. What should the two new functions do about MET / multi-site designs?

Currently they answer confidently and wrongly (§3.6). Pick one: hard error on duplicate
coordinates; or add a `by = "site"` argument returning per-group results. An error is the safe
default and can ship now; `by =` can follow.

### 🔷 D5. Is `as_list = TRUE` worth keeping?

Its own docs concede it's "identical data, different container", and `M["A", ]` already works on a
matrix. It makes the return type depend on an argument value, which complicates any future `print`
method. Easier to add later than to remove. **Recommend dropping before release.**

### 🔷 D6. Does a buffer break adjacency? — **new; needed before §4.1 can be written**

This is a genuine statistical question that coordinate-based construction forces into the open, and
there is no implementation-neutral answer.

`add_buffers()` shifts or scales coordinates: `type = "edge"` gives inner rows `2..n+1`;
`type = "row"` gives inner rows `2, 4, 6, 8`. Once buffer rows are dropped, the inner design's
coordinates are non-contiguous. Two ways to rebuild the grid, verified on a design with rows
1, 2, 4, 5 (a road where row 3 would be):

```
raw coordinates (gap kept)     ranked coordinates (gap removed)
  A  B                           A  B
  C  C                           C  C
 NA NA                           C  C     <- now counted as adjacent
  C  C                           A  B
  A  B
adjacency = 2                  adjacency = 4
```

Ranking invents two C–C adjacencies across the road. So:

- **Raw coordinates** say plots either side of a buffer or gap are *not* neighbours. Agronomically
  this is the defensible reading, and it's what I'd recommend. Cost: the grid can be **sparse**, and
  some existing code can't handle that (§3.9).
- **Ranked coordinates** say they *are* neighbours. Cost: silently changes the geometry, and
  destroys real physical gaps.

⚠️ **`main` has already made this choice implicitly, in the ranked direction.** `summary()`'s
`length(unique())` dimension fix rebuilds a row-buffered 4-row design from rows 2, 4, 6, 8 as a
contiguous 4×4 grid — verified. So KNOWN_ISSUES #1a's "fix" is semantically the ranking option, not
a neutral bug fix. Whichever way you go, `summary()` and the objectives should agree.

**Recommendation:** raw coordinates everywhere, plus renumbering *inside* `add_buffers()` if you
want buffered designs to stay contiguous — fix it at the source rather than papering over it in
every consumer.

### ✅ Resolved: superseded planning docs removed

`git rm`'d: `PLAN-adjacency-orientation-fix.md`, `PR-91-alignment-and-integration.md`,
`PR-91-info-objective-review.md`, `PR-incidence-review.md`. `KNOWN_ISSUES.md` retained and trimmed
to its two still-open items. Top-level `.md` files still raise an `R CMD check` NOTE for
non-standard top-level files; to be tidied before merging to `main`.

---

## 3. `main` and `feature/incidence`: the live findings

### 3.1 🔴 `main`'s `summary()` reports wrong neighbour balance

`.neighbour_balance()` ([R/summary.R:950](R/summary.R#L950)) reshapes with
`matrix(df[[swap]], nrow, ncol)` — a **column-major** fill. But `speed()` sorts its input
**row-major** ([R/speed.R:195-197](R/speed.R#L195-L197)). On a non-square grid that isn't a
transpose, it's a garble. Square grids agree by transpose-invariance, which is why this went
unnoticed.

Measured on the 4×3 design in `summary.design`'s own `@examples`, run on `main`:

| | truth (brute force) | `main`'s `summary()` |
|---|---|---|
| self-adjacencies | **0** | **2** |
| pair min / max | **5 / 6** | **5 / 5** |

The printout contradicts itself in the same block — `adjacency 0` under Score, `Self-adj.: 2
like-treatment adjacencies` under Evaluation. On a 4×6 design: truth self=5 (summary says 4), truth
max=8 (summary says 9). `calculate_pair_incidence()` matched brute force exactly every time
(A-B=8, A-C=8, A-D=1, B-C=1, B-D=8, C-D=7).

**The guarding test cannot catch it.** [test-summary.R:730](tests/testthat/test-summary.R#L730)
cross-checks against `matrix(r$design_df$treatment, nrow = 4, ncol = 3)` — the same wrong reshape as
the implementation. It validates the code against a copy of its own mistake.

**Fix:** rewrite `.neighbour_balance()` as a reduction of `calculate_pair_incidence()` —
`self_adjacent = sum(diag(M))`, everything else from `M[upper.tri(M)]` — and rewrite that test to
assert hand-derived values on a non-square grid.

### 3.2 🟠 The orientation problem, measured honestly

Two functions assumed **opposite** data orderings and neither read coordinates:

| Function | Fill on `main` | Correct inside `speed()` (row-major)? | Correct on raw `initialise_design_df()` (column-major)? |
|---|---|---|---|
| `calculate_adjacency_score()` | `matrix(..., byrow = TRUE)` | ✅ | ❌ |
| `objective_function_piepho()` | `matrix(...)` column-major | ❌ | ✅ |

Each is wrong exactly where the other is right.

- **`calculate_adjacency_score()`**, 2×3 design straight from `initialise_design_df()`: returns
  **6** where the truth is **0**. Its own examples use hand-written row-major data, so they pass and
  the inconsistency is invisible. `build_design_matrix()` fixes this cleanly — no side effects.
- **`objective_function_piepho()`** — see §3.3. The grid fix is correct but insufficient.

> **Correction.** An earlier version of this document reported piepho going from **3.569 → 2.531**
> on a 2×6 design and called that the fix. That comparison was invalid: `main` was measured on a
> **row-major** frame and the branch on a **column-major** one. Measured on the same row-major input
> — which is what `speed()` actually passes — the branch gives **14.53**, not 2.531. The corrected
> figures are in §3.3.

Neither change is in `NEWS.md`, there's no `## Bug Fixes` heading, and **no test pins either**. All
1667 tests pass before and after the refactor, because the piepho tests only assert types, never
values.

### 3.3 🔴 The piepho refactor scrambles the treatment column — a new bug

[R/metrics.R:244](R/metrics.R#L244), unchanged by the branch:

```r
design[[swap]] <- as.factor(design_matrix)     # write the flattened grid back
bal_score <- calculate_balance_score(design, swap, spatial_cols)
adj_score <- calculate_adjacency_score(design, swap, row_column, col_column)
```

Flattening a matrix in R is **column-major**. On `main` the grid was *also* filled column-major, so
this round-tripped exactly — verified `identical()`, i.e. line 244 was a **no-op**. With
coordinate-based filling the grid is the true layout, so flattening it column-major no longer matches
a row-major data frame, and the treatment column is silently permuted before `bal_score` and
`adj_score` are computed on it.

Measured on a 2×6 in **row-major** order (the real `speed()` path):

| | neighbour_balance | even_distribution | balance | adjacency | score |
|---|---|---|---|---|---|
| `main` | 1.3333 ❌ | 0.2357 ❌ | 2 ✅ | 0 ✅ | 3.569 |
| branch as-is | 0.3333 ✅ | 0.1975 ✅ | **8** ❌ | **6** ❌ | **14.53** |
| with the fix below | 0.3333 ✅ | 0.1975 ✅ | 2 ✅ | 0 ✅ | **2.531** |

Ground truth for that layout is `balance = 2`, `adjacency = 0`. So the branch trades two wrong
components for two different wrong components. On a 4×6 it is worse: branch reports
`balance 9.333, adjacency 0` where the truth is `balance 36, adjacency 20`. Inside a real
`speed(obj_function = objective_function_piepho)` run the optimiser drives the *corrupted* objective
to near-zero adjacency, producing a design with 20 like-treatment adjacencies. **Merging the branch
as it stands would make piepho actively worse.**

**Fix — delete the write-back.** It only ever coerced to factor:

```r
design[[swap]] <- as.factor(design[[swap]])    # was: as.factor(design_matrix)
```

Verified this restores every component to truth on 2×6, 4×6 and 3×3, and — the real prize — makes
piepho **order-invariant**: the same physical layout supplied row-major or column-major now scores
identically (2.530786 both ways). That invariance is the whole point of coordinate-based
construction, and it does not hold until line 244 is fixed.

It also fixes one of the two sparse-grid failures for free (§3.9): the
`replacement has 10 rows, data has 8` error came from this line.

### 3.4 🟠 The `design`-object path fails on non-default column names

Both functions hardcode `"treatment"` / `"row"` / `"col"` and ignore `design$metadata`. Verified:

```r
r <- speed(d2, swap = "variety", grid_factors = list(dim1 = "range", dim2 = "col"), ...)
summary(r)                   # works
calculate_pair_incidence(r)  # Error: Column(s) not found in data: treatment, row
```

The diagnostic tool is less capable than the optimiser it diagnoses. **This is now a small fix** —
the old review proposed adding fields to the `design` object, but `main`'s summary work already did
it. `metadata$row_column`, `metadata$col_column` and `metadata$per_level[[1]]$swap` are all present;
the functions just need to read them when `inherits(design, "design")`, and fall back to
`infer_row_col(df, quiet = TRUE)` for plain data frames so `range` works consistently.

### 3.5 🟠 Buffers are not dropped

`summary()` calls `.drop_buffer_rows()`; these functions don't. On an edge-buffered 3×3, verified:

```
       A B buffer C
A      0 5      4 3
B      5 0      4 3
buffer 4 4     16 4
C      3 3      4 1
```

`"buffer"` appears as a treatment with 16 self-adjacencies, and it inflates the real pair counts.
This falsifies the docs' own reassurance that "a well-optimised design will have zeros on the
diagonal". `main` already has the helper — reuse it.

### 3.6 🟠 MET / multi-site designs silently collapse

`m[cbind(row, col)] <- ...` keeps only the **last** write when coordinates repeat. Verified: an
8-plot, two-site design became a 2×2 grid with site 1 discarded, no warning:

```
     [,1] [,2]
[1,] "A"  "A"
[2,] "B"  "B"
```

This is the MET pattern in `speed()`'s own documentation, where five sites each reuse `row` 1:28 and
`col` 1:5 — a user would get counts that are ~1/5 of the truth for the exact question they're
asking. `calculate_position_incidence()` fails *differently* on the same input: `table()` pools all
sites, so row "1" mixes five different physical rows. See **D4**.

### 3.7 🟡 `calculate_position_incidence()` docs overclaim

> This is the human-readable decomposition of what `calculate_balance_score()` collapses to a
> scalar.

Verified: `sum(rowVars(t(pi$row))) + sum(rowVars(t(pi$col)))` does reproduce the balance score
exactly — **for a row+col design**. But `calculate_balance_score()` sums over *all* `spatial_cols`,
so for `~ row + col + block` the block term is unreachable from this function's output. It also
returns the transpose of the table the score is built from, and the score is a sum of *variances*,
not counts. Either soften the claim or take **D2 option B**.

### 3.8 🟡 `build_design_matrix()` doesn't validate its coordinates

It uses `row`/`col` values directly as matrix indices, so it needs them to be positive integers.
Verified failure: a `row` value of `0` gives
`number of items to replace is not a multiple of replacement length` plus silent data loss, because
index 0 is dropped by matrix indexing. Negative values would error less helpfully.

> **Correction.** An earlier version of this document recommended *ranking* the coordinates
> (`match(x, sort(unique(x)))`) to sidestep this. That is unsafe — ranking destroys genuine physical
> gaps and silently changes which plots are neighbours. See **D6** for the measured example and the
> decision it forces.

**Fix:** validate rather than transform. Error clearly if the coordinates aren't positive integers,
and let sparse-but-valid coordinates through as `NA` cells (§3.9). If contiguity is wanted for
buffered designs, renumber in `add_buffers()` where the offset is introduced.

Worth noting that **non-numeric coordinate labels are not a new problem**: `as_numeric_factor()`
returns `NA` for `"R1"`/`"C1"`-style labels and *both* the old `matrix()` approach and the new one
warn identically, because the old code already used it to derive dimensions. Shared pre-existing
limitation, not a regression.

### 3.9 🟡 Sparse grids are a genuinely new input class — and some code can't take them

This is the real fragility cost of coordinate-based construction. The `byrow` fill **can never**
produce `NA`; coordinate placement can, whenever the coordinate lattice has a hole (a road, an
irregular trial edge, or a dropped buffer row under **D6**'s raw-coordinate option).

Verified on a grid with rows 1, 2, 4, 5:

| Consumer | Sparse grid | Notes |
|---|---|---|
| `calculate_adjacency_score()` | ✅ returns 2 | `adjacency_score_vec()` is documented to treat NA pads as 0 |
| `calculate_nb(m, pair_mapping)` | ✅ | NA pairs fail the mapping lookup and `table()` drops them |
| `calculate_nb(m)` — **no** mapping | ❌ **errors** | `.calculate_nb()` does `if (node < bottom)`, which is `NA` |
| `calculate_ed()` | ⚠️ runs | Distances reflect the gap, which is arguably correct — but untested |
| `objective_function_piepho()` | ❌ **errors both ways** | via `.calculate_nb()`, and separately via §3.3's line 244 |

`pair_mapping` defaults to `NULL`, so the NA-intolerant path is the **default** one. Verified that a
real `add_buffers(d, "edge")` design, buffer rows dropped, reaches it:

```r
objective_function_piepho(inner, "treatment", c("row", "col"))
#> ERROR: missing value where TRUE/FALSE needed
```

**Fix:** guard `.calculate_nb()` against `NA` neighbours (skip the pair, matching what the
`pair_mapping` path already does), and add a sparse-grid test for `calculate_ed()`. Fixing §3.3
removes the other half. Until both are done, `build_design_matrix()` cannot safely be wired into
piepho for anything but a dense 1-based grid.

### 3.10 🟡 Duplicates `calculate_nb()`'s edge enumeration

`calculate_nb()` ([R/metrics.R:287](R/metrics.R#L287)) already enumerates rook-adjacent pairs from a
design matrix. `calculate_pair_incidence()` is the same computation in a different container. Two
independent implementations that can drift is one too many — factor the edge enumeration into a
single internal helper, or express one in terms of the other.

> **Correction.** An earlier version claimed the two "disagree on `NA`", with `calculate_nb()`
> stringifying `NA` into a literal `"NA,A"` pair. That's wrong for the `pair_mapping` path — verified
> it drops NA pairs cleanly, the same as `calculate_pair_incidence()`. The no-mapping path doesn't
> miscount either; it **errors** (§3.9).

### 3.11 🟡 Housekeeping

- **No `print` method.** The stated objective was "calculate **and print**". Neither return value
  carries a class. A 100-treatment MET gives a 100×100 mostly-zero matrix dumped to the console,
  when the interesting output is three lines. Not a blocker for the maths, but the feature isn't
  complete without it.
- **Missing `return()`** on the final line of three functions — [incidence.R:113](R/incidence.R#L113),
  [incidence.R:185](R/incidence.R#L185), [design_utils.R:894](R/design_utils.R#L894). `CLAUDE.md`
  requires explicit returns.
- **`air format --check` fails** on `R/incidence.R`, `R/design_utils.R`,
  `R/calculate_adjacency_score.R`. All three are clean on `main`, so this is the branch's drift.
  Argument wrapping only; `air format` fixes it.
- **NEWS** — the two new functions sit under `## Major Changes` next to `summary()`;
  `## Minor Changes` fits better. The orientation work needs a `## Bug Fixes` entry (§4.6).
- **`.gitignore`** — `*\.RData` / `*\.rds` use regex escaping in a glob file. They work, but they're
  misleading, and `.RData` is already covered. Unrelated to the feature; drop them.
- **No cross-check test** asserting `sum(diag(M)) == calculate_adjacency_score(df, swap)`. Verified
  it holds at the defaults (0=0 and 1=1 on two 3×3 designs), so this is a free test that pins the
  documented claim. It holds *only* at the defaults — `ring_type = "chebyshev"` on the second design
  gives 5 against a diagonal sum of 1, so the docs should scope the claim to rook-distance-1.
- **Discoverability** — neither function is in `README.Rmd`, any vignette, or `?speed`'s `@seealso`.
  `_pkgdown.yml` picks them up automatically via `starts_with("calculate")`, so no config change is
  needed.

---

## 4. Implementation plan: `bugfix/grid-orientation` (D1)

Off `main`, no dependency on PR #91 or PR #97. **Resolve D6 first** — it determines whether §4.1
validates or transforms coordinates.

```sh
git checkout main && git pull
git checkout -b bugfix/grid-orientation
```

### 4.1 Add `build_design_matrix()` to `R/design_utils.R`

Differences from the branch version: coordinates computed once into locals, explicit validation
(§3.8), and a duplicate-coordinate guard (§3.6). Written for **D6 = raw coordinates**.

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
positive-integer guard errors where `main` produced a partially-filled matrix. Check the MET examples
in `?speed` still run.

### 4.2 Make the consumers NA-tolerant (§3.9) — **do this before §4.3**

- `.calculate_nb()` ([R/metrics.R:330](R/metrics.R#L330)) — skip pairs where either cell is `NA`,
  matching the `pair_mapping` path. Currently `if (node < bottom)` errors on `NA`.
- `calculate_ed()` — add a sparse-grid test; it already runs, but the behaviour is unpinned and it
  uses `NA` internally as a "not this treatment" sentinel, so the interaction deserves an explicit
  check rather than an assumption.

### 4.3 Point the call sites at it

- **`objective_function_piepho()`** ([R/metrics.R:234](R/metrics.R#L234)) — the grid build **and**
  the §3.3 write-back fix. These must land together; the grid change alone is a regression.
- `calculate_adjacency_score()` ([R/calculate_adjacency_score.R:255](R/calculate_adjacency_score.R#L255))
  — composability. Safe on its own.
- `.neighbour_balance()` ([R/summary.R:950](R/summary.R#L950)) — fixes §3.1. Under D6's raw-coordinate
  option this changes buffered-design results, because `main` currently collapses buffer gaps; that
  is the intended correction but it needs a NEWS line and a test update.

Better still, rewrite `.neighbour_balance()` as a reduction over the incidence matrix so there is one
adjacency enumeration in the package (§3.10). If `calculate_pair_incidence()` isn't on this branch,
inline the same `raw + t(raw)` fold and have PR #97 collapse them.

### 4.4 Fix `ring_weights` recycling

Verified live on `main`: `calculate_adjacency_score(d, "trt", ring_dists = c(1, 2))` errors with
`length(dists) == length(weights) is not TRUE`, because `ring_weights` defaults to scalar `1`. The
documented default is unusable with multi-ring `ring_dists`. Recycle `weights` to `length(dists)`.

### 4.5 Optional: `calculate_efficiency_factor()` post-buffer (KNOWN_ISSUES #1b)

Related root cause. It derives `n_rows`/`n_cols` from `max()` then fills with
`for (i in 1:n_rows) for (j in 1:n_cols)` assuming `n_rows * n_cols == n_plots` — which a sparse
design violates. `summary()`'s `.efficiency_factor()` wrapper catches the error and degrades to
`available = FALSE`, so it fails safely rather than fabricating a number. Making it handle a sparse
plot set is a bigger job than the other items here (the row/col indicator matrices assume a complete
lattice); keep it separable.

### 4.6 Tests

- New `tests/testthat/test-build_design_matrix.R`: row-major input, column-major input, non-square,
  factor coordinates with lexical level order, sparse coordinates, `NA` treatment cells, and the
  three new errors (non-numeric, non-positive-integer, duplicate coordinates).
- **Order-invariance for piepho** — the same physical layout as a row-major and a column-major frame
  must score identically. Verified this fails today and passes with §3.3's fix (2.530786 both ways).
  This is the single highest-value test in the change.
- `objective_function_piepho()` on a non-square grid asserting **hand-derived** values for all four
  components, not just `expect_type()`. Use the §3.3 table as the fixture.
- `objective_function_piepho()` on a sparse grid, with and without `pair_mapping` (§3.9).
- `calculate_adjacency_score(initialise_design_df(...), "treatment")` — the direct-call case that
  returns 6 instead of 0 today.
- Rewrite [test-summary.R:730](tests/testthat/test-summary.R#L730) to assert hand-derived values on
  a non-square grid instead of reusing `matrix()`.
- `sum(diag(calculate_pair_incidence(...))) == calculate_adjacency_score(...)` at the defaults, if
  the incidence function is available.

### 4.7 NEWS

```markdown
## Bug Fixes

- `objective_function_piepho()` now builds the design grid from the `row`/`col` coordinates rather
  than assuming the data frame's row order, and no longer overwrites the treatment column with a
  flattened grid. All four score components are now computed on the actual layout, and the score no
  longer depends on the row ordering of the input. Designs generated with this objective should be
  regenerated. (#97)
- `calculate_adjacency_score()` is now robust to any row ordering of its input, including the
  column-major output of `initialise_design_df()`. (#97)
- `summary()` no longer reports incorrect neighbour-balance counts for designs on non-square
  grids. (#97)
- `calculate_nb()` no longer errors on designs with missing plots when `pair_mapping` is not
  supplied.
- `calculate_adjacency_score()` now recycles `ring_weights` against `ring_dists`.
```

### 4.8 Out of scope, recorded

- **Removing the sort at [R/speed.R:195](R/speed.R#L195).** Once grids are coordinate-based *and*
  §3.3 is fixed, the sort is no longer needed for correctness — the order-invariance test in §4.6
  is what proves that. But `generate_neighbour`, `random_initialise`, `print.design` and `autoplot`
  may rely on row order. Leave it; note as a later simplification. Don't bundle it with a bug fix.
- **Hot-loop performance.** Measured on a 700-plot design (28×25), 2000 builds:
  `matrix()` **415 µs/build** vs `build_design_matrix()` **1180 µs/build** — **2.84×**, about 7.6 s
  extra per 10,000 iterations per level. Real but not disqualifying, and avoidable: the row/col
  vectors never change during the SA loop, only `swap` does, so the validated `cbind(rows, cols)`
  index can be computed once per level and passed in. Do it after correctness, and benchmark rather
  than assume.

---

## 5. PR #91 — what's still live

Re-verified against `d51d185`. Items about the four demoted helpers are dropped: they're
`@keywords internal` now, so they're no longer API surface.

### 5.1 🔴 Blocking: `L`/`Σ` ordering isn't tied to coordinates

`objective_function_info()` accepts `spatial_cols` and never uses it — `.compute_info(layout_df,
swap, L_matrix, block_column)` doesn't receive it, so `L_matrix` is consumed purely **positionally**
by the row order of `layout_df`. Meanwhile `speed()` re-sorts to row-major while
`initialise_design_df()` emits column-major, so the SA loop optimises against a **scrambled** Σ,
silently. The headline example and the 4×6 integration test both hit it; the test passes only
because it asserts `rank == 5` and `is.finite(A_value)`, both insensitive to a permutation.

This is the same class of defect as §3.2/§3.3 — an implicit ordering contract with nothing enforcing
it. The order-invariance test pattern from §4.6 applies here directly and would be the cleanest way
to prove the fix.

**Fix:** thread `spatial_cols` through so `L`/`Σ` are keyed to coordinates, or have `speed()` pass
the row permutation. At minimum validate and document the required ordering loudly.

### 5.2 🟠 Ignores the incremental-state contract

`CLAUDE.md` is explicit that objectives must accept `current_score_obj` / `swapped_items` and update
incrementally. `objective_function_info()` accepts neither (both fall into `...`) and although it
*returns* `info_matrix` and `eigenvalues`, it never consumes them. So every iteration rebuilds `X1`
in an R `for` loop, forms `t(X1) %*% L %*% X1` against a dense n×n `L`, and runs `eigen()` — 10,000
times by default. A swap touches two plots; the update is rank ≤ 4. Follow
`objective_function_piepho()`'s pattern, or document the O(n²v + v³) per-iteration cost loudly.

### 5.3 🟠 Behind `main`

Merge base is `72a3c94`; PR #91 has not merged the `summary()` work. Needs a merge before review can
conclude — in particular its `_pkgdown.yml` predates `main`'s `summary.design` /
`print.summary.design` entries, and pkgdown errors when a documented export is missing from an
explicit reference index.

### 5.4 🟡 Smaller items

- `spatial_cols` is dead in the signature (root cause of §5.1) — document or warn.
- Documented `L` formula drops the trailing `X₂ᵀΣ⁻¹` term; the code is right, the man page is wrong.
- `A_val = …` / `D_val = …` use `=` for assignment. Air won't fix this.
- `Sigma` is capitalised; every other argument is lower snake_case.
- `compute_L_projection()`'s validation `stop()`s omit `call. = FALSE`; consider moving them to
  `R/verify_utils.R` with the rest of the `.verify_*` family.
- Bare `setNames()`; `R/buffers.R` uses explicit `stats::setNames()`.
- Reimplements a pseudo-inverse via `eigen()` rather than reusing `pseudo_inverse()` in
  `R/utils.R` — defensible for a symmetric matrix, but say so in a comment.
- `R/objectives.R` vs `R/metrics.R` — every other `objective_function_*` lives in `metrics.R`.
  Splitting is fine but should be deliberate.
- Indicator matrices built with `for` loops; `X[cbind(i, j)] <- 1` is the idiom
  `build_design_matrix()` uses. The `X2` construction is duplicated verbatim between
  `.build_L_from_df()` and `compute_L_projection()`.
- `calculate_efficiency_factors` (plural) vs existing `calculate_efficiency_factor` (singular) —
  one character apart, different computations. Now internal, so lower priority, but rename to
  `calculate_canonical_efficiency_factors()` if it's ever exported.
- No `NEWS.md` entry. Unrelated `DESCRIPTION` whitespace churn. Dead `.Rbuildignore` entry
  (`^\.dir-locals\.el$`). Tests pass `block_col =` and survive only on partial matching.
- No test for `criterion = "D"` through `speed()`.

---

## 6. Suggested sequencing

1. **Resolve D6** (does a buffer break adjacency?). It determines §4.1 and whether `summary()`'s
   buffered-design numbers change.
2. **`bugfix/grid-orientation`** (§4) — off `main`. Coordinate-based grids, the §3.3 write-back fix,
   §3.9 NA tolerance, `summary()`'s neighbour balance, `ring_weights` recycling. No dependency on
   either feature PR. **Merge first.**
3. **PR #97 `feature/incidence`**, rebased on (2) and reduced to the new functions: metadata-aware
   defaults (§3.4), buffer dropping (§3.5), the D4 decision on MET, `.neighbour_balance()` collapsed
   onto `calculate_pair_incidence()` (§3.10), Air/`return()`/NEWS housekeeping (§3.11), and the
   D2/D3/D5 decisions applied.
4. **PR #91** — merge `main`, then §5.1 and §5.2.
5. **Optional follow-up** — a `print`/`autoplot` layer over the incidence matrices (§3.11),
   `calculate_incidence(spatial_factors = ...)` if you take D2 option B, the hot-loop hoist (§4.8),
   and `calculate_efficiency_factor()` post-buffer (§4.5).

---

## 7. What changed since the four original documents

Recorded so the reasoning isn't re-litigated.

### Corrections to my own earlier findings

| Earlier claim | Corrected |
|---|---|
| Piepho goes **3.569 → 2.531**; the branch fixes it | **Invalid comparison** — `main` measured row-major, branch column-major. On the same row-major input the branch gives **14.53**. Corrected table in §3.3. |
| The branch's piepho refactor is a clean bug fix | **No** — it fixes NB/ED and breaks balance/adjacency via the line-244 write-back (§3.3). Net regression until that line changes. |
| **Rank** the coordinates to handle buffer offsets | **Unsafe** — ranking destroys real physical gaps and silently changes which plots are neighbours (measured: adjacency 2 → 4). Validate instead; see §3.8 and **D6**. |
| `calculate_nb()` stringifies `NA` into a literal `"NA,A"` pair | **Wrong for the `pair_mapping` path** — NA pairs are dropped cleanly. The no-mapping path errors instead (§3.9, §3.10). |
| Non-numeric coordinate labels are a new fragility | **No** — both old and new approaches warn identically; pre-existing shared limitation (§3.8). |

### Supersessions from upstream changes

| Old claim | Status now |
|---|---|
| `PR-91-alignment` §1.1: #91's `calc_*` exports break the pkgdown build | **Superseded.** Author demoted them to internal and fixed `_pkgdown.yml`. |
| `PR-91-alignment` §1.2/§1.3/§1.5/§1.9 items about `calc_*` | **Mostly moot.** Those functions are internal now; the `treatment_column` and lexical-sort inconsistencies are internal-only. |
| `PR-91-alignment` §2.1: `calculate_position_incidence()` is redundant against #91 | **Superseded.** #91 no longer exports an incidence matrix. It is still a thin `table()` wrapper — see **D2**, a different argument for the same conclusion. |
| `PR-91-alignment` §2.3/§2.5, `PR-incidence` §2.5/§3.1: four-way naming collision | **Superseded.** No collision; #91 exports 4 functions, none named `*incidence*`. D3 is now a clarity call, not a conflict. |
| `PR-incidence` §2.2: add `swap` + grid factors to the `design` object | **Superseded — already done.** `main`'s summary work added `design$metadata`. The fix is now just reading it (§3.4). |
| `KNOWN_ISSUES` #1a: neighbour balance fixed on `feature/summary` | **Only half fixed.** The dimension half was fixed; the *fill-order* half was never addressed and is the more consequential one (§3.1). The dimension fix also silently chose the "buffers don't break adjacency" semantics — see **D6**. Retained in `KNOWN_ISSUES.md` #1a with both caveats. |
| `KNOWN_ISSUES` #1b: A-efficiency post-buffer | **Still open** (§4.5). Retained as `KNOWN_ISSUES.md` #1b. |
| `KNOWN_ISSUES` #2: neighbour auto-enable leaked across levels | **No longer applicable**; neighbour balance is computed for any grid design regardless of objective. Removed. |
| `KNOWN_ISSUES` #3: shared block factor across levels | **Fixed on `main`.** Removed. |
| `KNOWN_ISSUES` #4: internal functions inconsistently dot-prefixed | **Still open.** Cosmetic; the branch added another non-prefixed internal (`build_design_matrix`) so the inconsistency grew. Retained as `KNOWN_ISSUES.md` #2. If you act on it, update `CLAUDE.md`'s "Source layout" and "Conventions to preserve" in the same change. |
| `PR-incidence` §3.3: adjacency broken for all non-square `speed()` designs | **Was wrong, already corrected in that document.** `speed()`'s row-major sort matches `byrow = TRUE`. |

**One old finding worth keeping visible: lexical factor levels defeat the sort.** `to_factor()` runs
*before* the sort in `speed()`, so a **character** row column with ≥10 rows gets levels
`1, 10, 11, 2, …` and `order()` follows them. Verified on an 11-row design: the grid `main`
reconstructs is `A J K E C D A F G H I` where the actual layout is `A E C D A F G H I J K`. Any
grid-based metric is then computed on a layout that isn't the design. Coordinate-based construction
fixes it; the sort alone never could. This is also why the order-invariance test (§4.6) is worth more
than any number of fixed-input assertions.
