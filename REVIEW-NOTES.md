# Review notes: `feature/incidence` (PR #97)

**Scope:** the incidence feature itself — `R/incidence.R`
(`calculate_pair_incidence()`, `calculate_position_incidence()`), `tests/testthat/test-pair-incidence.R`,
`tests/testthat/test-pos-incidence.R`, and their `NEWS.md` / `man/` entries.

**Companion files** — one per workstream, so each can become its own small PR:

| File | Workstream |
|---|---|
| **this file** | `feature/incidence` (PR #97) — `R/incidence.R` |
| `REVIEW-NOTES-SUMMARY.md` | the merged `summary()` work — `R/summary.R` |
| `REVIEW-NOTES-OTHER.md` | grid construction / core metrics bugs, and PR #91 |

**Last verified:** 2026-08-04, R 4.6.1, `pkgload::load_all()`. Branch at `1536991` (current `main`
`a36d302` merged in). All numbers measured, not inferred. Full suite: 1667 pass, 0 fail.

> ⚠️ **This branch is currently ~75% not-this-feature.** It touches four `R/` files, and only
> `R/incidence.R` is the incidence feature. The other three — `R/design_utils.R`
> (`build_design_matrix()`), `R/calculate_adjacency_score.R`, `R/metrics.R` — are the grid-construction
> workstream, and **in their current state they make `objective_function_piepho()` worse, not better**.
> That work and its blocking bug are written up in `REVIEW-NOTES-OTHER.md` Part A. Extracting it (D1
> there) is the recommended first move, and it is a hard blocker on merging this branch as it stands.

---

## 1. Summary

**What this branch adds that is genuinely new and correct:**

- `calculate_pair_incidence()` — a symmetric treatment × treatment matrix of rook-adjacency pair
  counts, self-adjacency on the diagonal. The maths is right: `raw + t(raw)` with
  `diag(M) <- diag(raw)` is the correct way to fold ordered edge counts into a symmetric matrix while
  counting self-pairs once, it's fully vectorised, and the `nr >= 2` / `nc >= 2` guards handle
  single-row and single-column designs. **Matched brute-force ground truth on every case tested.**
  Notably this is the *correct* implementation of a quantity `main`'s `summary()` currently computes
  wrongly (see S1 in `REVIEW-NOTES-SUMMARY.md`) — the two should converge, on this version.
- `calculate_position_incidence()` — treatment × row and treatment × col count matrices. Correct, but
  a thin wrapper over two `table()` calls. **Decision needed: I-D1.**
- Deliberate, tested `NA` handling: dropping edges rather than counting `"NA"` as a treatment is the
  right call.
- Genuinely good roxygen — `@seealso` cross-links, runnable examples, `@inheritParams`, documented
  return shapes.

**Live findings, all fixable within this branch:**

- 🟠 **I1** — the `design`-object path errors on any design whose columns aren't literally
  `treatment`/`row`/`col`, even though `main` now records the real names in `design$metadata`.
- 🟠 **I2** — buffers aren't dropped; `"buffer"` shows up as a treatment with 16 self-adjacencies.
- 🟠 **I3** — MET / multi-site designs are silently wrong, and the two functions are wrong in
  *different* directions.
- 🟡 **I4** — `calculate_position_incidence()`'s docs overclaim the balance-score link.
- 🟡 **I5** — duplicates `calculate_nb()`'s edge enumeration.
- 🟡 **I6** — no `print` method (the stated objective said "calculate **and print**"), plus
  housekeeping.

**Not in scope here** (moved out, with cross-references kept):

| Was | Now |
|---|---|
| `build_design_matrix()`, the piepho + adjacency refactor, sparse-grid/NA work, `ring_weights` | `REVIEW-NOTES-OTHER.md` Part A (G1–G6, D6) |
| `.neighbour_balance()` being wrong, the tautological summary test | `REVIEW-NOTES-SUMMARY.md` (S1–S4) |
| PR #91 review | `REVIEW-NOTES-OTHER.md` Part B |

---

## 2. Decisions you need to make

*(Renumbered per file. Old numbering from the single consolidated document is noted for continuity.)*

### 🔷 I-D1. Keep `calculate_position_incidence()`? *(was D2)* — **your call; I lean keep, but generalised**

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
| **A. Keep as-is** | Cheapest. Exports a `table()` wrapper; hardcodes row/col, so a user optimising `~ row + col + block` gets a report on two of their three factors. |
| **B. Generalise to `spatial_factors`** (recommended) | `calculate_incidence(design, swap, spatial_factors = ~ row + col + block)` → one matrix per factor. Covers blocks and MET factors, mirrors `speed()`'s own interface, and fixes I4 in the same move. Moderate work. |
| **C. Drop it** | Smallest API. Users write `table()`. Loses the sorting fix and the discoverability. |

Note option B also subsumes PR #91's internal `calc_incidence_matrix()` — no longer a collision, since
#91 demoted it to internal, but still a duplicate implementation worth collapsing eventually.

### 🔷 I-D2. Does `calculate_pair_incidence()` keep its name? *(was D3)*

It is, precisely, an *adjacency concurrence* matrix. In design theory the **incidence matrix** is
conventionally treatment × block, and pairwise co-occurrence is the **concurrence** matrix `N Nᵀ` — so
a biometrician reading `calculate_pair_incidence()` may reasonably expect `N Nᵀ` and get adjacency
counts instead. `calculate_position_incidence()` *is* closer to the classical usage, which makes the
inconsistency between the two names worse rather than better.

This mattered more when PR #91 was exporting a competing `calc_incidence_matrix()`; it isn't any more,
so the pressure is lower and it's purely a clarity call.

- **Rename** to `calculate_adjacency_concurrence()` — precise, but long, and the branch is named for
  the old term.
- **Keep** and state the relationship to the standard terms in `@description` — cheaper, and fine now
  the collision is gone.

### 🔷 I-D3. What should these functions do about MET / multi-site designs? *(was D4)*

Currently they answer confidently and wrongly (I3). Options:

- **Hard error** when the design has duplicate `(row, col)` coordinates. Safe, ships now. If the grid
  work lands first (`REVIEW-NOTES-OTHER.md` A4.1), `build_design_matrix()` already raises this, so
  `calculate_pair_incidence()` inherits it free — but `calculate_position_incidence()` doesn't go
  through the grid at all, so it needs its own guard.
- **`by = "site"`** argument that splits the design and returns per-group results. The better answer,
  and it can follow.

Do the error now regardless; a wrong number is worse than no number.

### 🔷 I-D4. Is `as_list = TRUE` worth keeping? *(was D5)* — **recommend dropping**

Its own documentation concedes it's "the matrix split by row — identical data, different container",
and `M["A", ]` already works on a matrix. It adds a parameter to document, test and maintain forever
for no capability, it's asymmetric (only one of the two functions has it), and it makes the return type
depend on an argument value — which complicates any `print` method added for I6. Much easier to add
later than to remove.

### 🔷 Cross-cutting: D6 (does a buffer break adjacency?)

Lives in `REVIEW-NOTES-OTHER.md` A2 because that's where it's actioned, but it affects I2: if buffers
are dropped and the remaining coordinates are left raw, `calculate_pair_incidence()` will report plots
either side of a buffer as non-adjacent. That's the behaviour I'd argue for, but it should be a
conscious choice and it should match `summary()`.

---

## 3. Findings

### I1 🟠 The `design`-object path fails on non-default column names *(was §3.4)*

Both functions accept a `design` object — which reads as "this integrates with `speed()`" — then fall
back to `swap = "treatment"`, `row_column = "row"`, `col_column = "col"`. Verified:

```r
r <- speed(d2, swap = "variety", spatial_factors = ~ range + col,
           grid_factors = list(dim1 = "range", dim2 = "col"), ...)
summary(r)                   # works
calculate_pair_incidence(r)  # Error: Column(s) not found in data: treatment, row
```

The user did everything right, the object knows the answer, and the error doesn't even hint that
`swap =` is the argument to set. `speed()` is *actively more capable* here — `infer_row_col()` matches
`^(col(umn|)|range)(s|)$` case-insensitively, so a trial with a `range` column optimises fine and even
prints `row and range are used as row and column`. Then the diagnostic tool built to inspect it errors.

**This is now a small fix.** The earlier review proposed adding fields to the `design` object; `main`'s
summary work already did it. Verified present:

```r
r$metadata$row_column                  # "range"
r$metadata$col_column                  # "col"
r$metadata$per_level[[1]]$swap         # "variety"
```

So: default from `metadata` when `inherits(design, "design")`, and fall back to
`infer_row_col(df, quiet = TRUE)` for plain data frames so `range` is supported consistently.

**Why no test caught it:** both "design object accepted as input" tests
([test-pair-incidence.R:86](tests/testthat/test-pair-incidence.R#L86),
[test-pos-incidence.R:84](tests/testthat/test-pos-incidence.R#L84)) use a hand-built
`structure(list(design_df = df), class = "design")` **and** pass `swap = "trt"` explicitly. No test
ever calls either function against a real `speed()` result, and no test exercises the defaults.

### I2 🟠 Buffers are not dropped *(was §3.5)*

`summary()` calls `.drop_buffer_rows()`; these functions don't. Verified on an edge-buffered 3×3:

```
       A B buffer C
A      0 5      4 3
B      5 0      4 3
buffer 4 4     16 4
C      3 3      4 1
```

`"buffer"` is reported as a treatment with 16 self-adjacencies, and it inflates the real pair counts
(`A`–`buffer` = 4). This directly falsifies the function's own documented reassurance that "a
well-optimised design will have zeros (or near-zeros) on the diagonal".
`calculate_position_incidence()` has the same problem — verified `"buffer"` appears as a row.

It's also *inconsistent*: `add_buffers()` only fills a column named exactly `"treatment"` when
`treatment_cols` is `NULL`, leaving other columns `NA`. So a design with a `treatment` column gets a
polluted matrix while the same design with a `variety` column gets `NA` buffers that the function
correctly drops. Identical workflows, different answers, depending on a column name.

**Fix:** reuse `.drop_buffer_rows()` — `main` already has it, and it takes the design's `metadata`, so
it handles the multi-column case properly. Better long-term: have `add_buffers()` record buffer status
in a dedicated column so downstream code filters reliably instead of string-matching `"buffer"`.

### I3 🟠 MET / multi-site designs are silently wrong, in two different ways *(was §3.6)*

`calculate_pair_incidence()` goes through the grid, and `m[cbind(row, col)] <- ...` keeps only the
**last** write when coordinates repeat. Verified: an 8-plot, two-site design collapsed to a 2×2 grid
with site 1 discarded, no warning:

```
     [,1] [,2]
[1,] "A"  "A"
[2,] "B"  "B"
```

This is the MET pattern in `speed()`'s own documentation, where five sites each reuse `row` 1:28 and
`col` 1:5 and the design disambiguates via `site_row`/`site_col`. A user asking the obvious follow-up
question — "did any line end up next to itself?" — gets a confidently wrong, reassuringly low number at
roughly 1/5 of the truth.

`calculate_position_incidence()` fails **differently** on the same input: it uses
`table(trt_fac, df[[row_column]])` over the whole frame, so it *pools* all five sites and row `"1"`
mixes five different physical rows. One function silently drops 80% of the data, the other silently
aggregates over a factor it doesn't know exists. Neither warns.

**Fix:** per I-D3.

### I4 🟡 `calculate_position_incidence()`'s docs overclaim *(was §3.7)*

> This is the human-readable decomposition of what `calculate_balance_score()` collapses to a scalar.

Verified: `sum(rowVars(t(pi$row))) + sum(rowVars(t(pi$col)))` *does* reproduce the balance score
exactly — **for a row+col design**. Three ways the claim is still too strong:

1. `calculate_balance_score()` sums over *all* `spatial_cols`, so for `~ row + col + block` the block
   term is unreachable from this function's output.
2. The score is a sum of **variances** of counts, not the counts; a user can't recover it from these
   matrices without knowing that.
3. It tabulates `table(spatial, treatment)` — the **transpose** of what this function returns.

**Fix:** soften to "related to", and state the actual relationship. Or take **I-D1 option B** and
return the per-factor variances alongside the counts, which makes the claim literally true.

### I5 🟡 Duplicates `calculate_nb()`'s edge enumeration *(was §3.10)*

`calculate_nb()` ([R/metrics.R:287](R/metrics.R#L287)) already enumerates rook-adjacent pairs from a
design matrix. `calculate_pair_incidence()` is the same computation in a different container, so the
package would carry two independent implementations that can drift.

There are now **three** places that count adjacent pairs — `calculate_nb()`, this function, and
`.neighbour_balance()` in `R/summary.R` — and the third is measurably wrong (S1 in
`REVIEW-NOTES-SUMMARY.md`). That's the strongest argument for convergence: this branch has the
implementation the other two should be expressed in terms of.

**Fix:** factor the edge enumeration into one internal helper (`.adjacent_pairs(m)`) that all callers
use, and add a test asserting they agree.

> **Correction.** An earlier version of this review claimed the two "disagree on `NA`", with
> `calculate_nb()` stringifying `NA` into a literal `"NA,A"` pair. That's wrong for the `pair_mapping`
> path — verified it drops NA pairs cleanly, exactly as `calculate_pair_incidence()` does. The
> no-mapping path doesn't miscount either; it **errors**. See G4 in `REVIEW-NOTES-OTHER.md`.

### I6 🟡 The "print" half of the objective, and housekeeping *(was §3.11)*

The stated objective was to *calculate and print*. There is no `print`, `summary` or `autoplot` method,
and neither return value carries a class — `NAMESPACE` still has exactly two S3 methods
(`print.design`, `autoplot.design`).

That matters because the raw containers don't scale. The MET example has 100 treatments:
`calculate_pair_incidence()` returns a 100×100 integer matrix — 10,000 mostly-zero numbers wrapping
across dozens of screens — and to answer "which treatments share a row with themselves?" from
`calculate_position_incidence()` the user must eyeball 2,800 cells for values `> 1`. The interesting
output is tiny:

```
Adjacency incidence: 3 of 100 treatments are self-adjacent
  T14 (2 occurrences), T57 (1), T88 (1)
Row incidence: 4 treatments appear more than once in a row
Most frequent neighbour pairs: T2-T44 (3), T9-T17 (3), ...
```

**Fix:** give the return values classes wrapping the matrices, with `print` methods that surface the
violations and summarise the rest, keeping the matrix accessible underneath. An `autoplot` heatmap fits
the package's existing ggplot2 tooling and is a natural third step. Note I-D4 first — dropping
`as_list` makes this cleaner.

Remaining housekeeping:

- **Missing `return()`** on the final line — [incidence.R:113](R/incidence.R#L113) and
  [incidence.R:185](R/incidence.R#L185). `CLAUDE.md` requires explicit returns. (The third instance,
  `build_design_matrix()`, moves with the grid work.)
- **`air format --check` fails** on `R/incidence.R`; it's clean on `main`, so this is the branch's
  drift. Argument wrapping only — `air format R/incidence.R` fixes it. (The other two failing files go
  with the grid work.)
- **NEWS** — the two new functions sit under `## Major Changes` next to `summary()`;
  `## Minor Changes` fits better for two additive diagnostics that change no existing behaviour.
  Neither bullet references the PR, though this branch is the one that added the `(#999)` convention to
  `CLAUDE.md`.
- **`.gitignore`** — `*\.RData` / `*\.rds` use regex escaping in a glob file. They work, but they're
  misleading and `.RData` is already covered on line 3. Unrelated to the feature; drop them.
- **`stats::setNames()`** — used bare at [incidence.R:111](R/incidence.R#L111); resolves only because
  other files carry `@importFrom stats setNames`. `R/buffers.R` uses the explicit form; match it.
- **`CLAUDE.md`'s "Source layout (R/)"** section isn't updated for the new `R/incidence.R`.
- **Discoverability** — neither function is in `README.Rmd`, any vignette, or `?speed`'s `@seealso`.
  `_pkgdown.yml` picks them up automatically via `starts_with("calculate")`, so no config change is
  needed. For a feature whose whole purpose is *inspecting* a design, a few lines in the main vignette
  after the `autoplot()` example is where users will actually look.

### I7 🟡 Test gaps

The 17 existing tests are focused and correct — I verified the 3×3 Latin square case by hand and the
all-pairs-`= 4` expectation is right. The gaps map onto the findings above:

- No test uses a real `speed()` result, and none exercises the defaults (I1).
- No test for duplicate coordinates / multi-site (I3) or buffered designs (I2).
- The "missing column gives informative error" tests use `expect_error(..., "col")`, which matches the
  word "**Col**umn" in the message regardless of which column is missing — the test cannot fail for the
  reason it claims to check. Match on the actual column name.
- No test with a factor `swap` column carrying unused levels: `unique(as.character(...))` drops them, so
  a treatment with zero plots silently vanishes from the matrix, whereas `speed()`'s `table()`-based
  balance score would have counted it.
- **No test asserting `sum(diag(M)) == calculate_adjacency_score(df, swap)`.** Verified it holds at the
  defaults (0=0 and 1=1 on two 3×3 designs), so this is a free test that pins the documented claim and
  would catch drift against `calculate_nb()` (I5). It holds *only* at the defaults —
  `ring_type = "chebyshev"` on the second design gives 5 against a diagonal sum of 1 — so the docs
  should scope the claim to rook-distance-1 rather than implying general equivalence with
  `calculate_adjacency_score()`, which is parameterised by `ring_dists` / `ring_weights` / `ring_type` /
  `relationship` and forwards all four from `objective_function()`.

---

## 4. Plan

**Depends on:** `REVIEW-NOTES-OTHER.md` Part A landing first (D1 there), if you extract the grid work.
Answer D6 too — it determines what I2's buffer handling should report.

```sh
# after bugfix/grid-orientation merges to main
git checkout feature/incidence
git rebase main          # or start clean: git checkout -b feature/incidence-v2 main
# drop the R/design_utils.R, R/calculate_adjacency_score.R and R/metrics.R changes;
# they belong to the grid branch
```

1. **Apply I-D1 through I-D4** — they change the API surface, so settle them before polishing.
2. **I1** — metadata-aware defaults plus `infer_row_col()` fallback.
3. **I2** — reuse `.drop_buffer_rows()`.
4. **I3** — duplicate-coordinate guard in both functions (the grid helper covers
   `calculate_pair_incidence()` only).
5. **I4** — soften the docs, or generalise per I-D1 option B.
6. **I5** — collapse the edge enumeration to one helper; if the summary branch has already been fixed,
   express `.neighbour_balance()` in terms of `calculate_pair_incidence()`.
7. **I6/I7** — `print` method, housekeeping, and the test gaps. The
   `sum(diag(M)) == calculate_adjacency_score(...)` cross-check is the highest-value single test.
8. **NEWS** — move to `## Minor Changes`, add `(#97)`.

```markdown
## Minor Changes

- Added `calculate_pair_incidence()`, returning a symmetric treatment x treatment matrix of
  rook-adjacency neighbour counts with self-adjacency on the diagonal. (#97)
- Added `calculate_position_incidence()`, returning treatment x row and treatment x column count
  matrices. (#97)
```

---

## 5. Superseded findings from the original review

| Earlier claim | Status |
|---|---|
| Add `swap` + resolved grid factors to the `design` object | **Superseded — already done.** `main`'s summary work added `design$metadata`; the fix is now just reading it (I1). |
| Naming/semantic collision with PR #91's `calc_incidence_matrix()` / `calc_concurrence_matrix()` | **Superseded.** #91 demoted both to `@keywords internal`, so there is no competing public API. I-D2 is now a clarity call, not a conflict. |
| `calculate_position_incidence()` is redundant against PR #91 | **Superseded.** #91 no longer exports an incidence matrix. It's still a thin `table()` wrapper — see I-D1, a different argument for the same conclusion. |
| `calculate_nb()` and `calculate_pair_incidence()` disagree on `NA` | **Wrong for the `pair_mapping` path** — NA pairs drop cleanly in both. The no-mapping path errors instead (I5). |
| `build_design_matrix()` and the piepho/adjacency orientation fix | **Moved** to `REVIEW-NOTES-OTHER.md` Part A. Not this feature, and it carries a blocking regression. |
