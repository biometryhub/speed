# Review notes: PR #91 `info-objective`

**PR:** [#91](https://github.com/biometryhub/speed/pull/91) — author kaibagley, fork
`kaibagley/speed`, branch `info-objective` at `d51d185` (14 commits). Adds `R/objectives.R`.

**What it adds.** A fundamentally different objective: instead of the adjacency proxy, it scores a
design by the **Fisher information for treatment contrasts** under a user-supplied spatial covariance,
directly targeting the Cramér–Rao lower bound of the intended analysis model.

- `objective_function_info()` — the treatment information matrix `I = X₁ᵀ L X₁` after projecting out
  nuisance fixed effects, scored under **A**-optimality (`tr(I⁻) = Σ 1/λᵢ`) or contrast-space
  **D**-optimality (`−log|I| = −Σ log λᵢ`) over the `v−1` positive eigenvalues.
- `compute_L_projection()` — builds `L = Σ⁻¹ − Σ⁻¹X₂(X₂ᵀΣ⁻¹X₂)⁻¹X₂ᵀΣ⁻¹` from a user covariance `Σ`
  and block design `X₂`.
- `cor_ar1()` / `cor_ar1_ar1()` — correlation builders.
- Internal: `.compute_info()`, `.build_L_from_df()`, `.build_treatment_matrix()`,
  `calc_info_matrix()`, `calc_incidence_matrix()`, `calc_concurrence_matrix()`,
  `calculate_efficiency_factors()`.

**Companion files** — one per workstream:

| File | Workstream |
|---|---|
| `REVIEW-NOTES.md` | `feature/incidence` (PR #97) — `R/incidence.R` |
| `REVIEW-NOTES-SUMMARY.md` | the merged `summary()` work — `R/summary.R` |
| `REVIEW-NOTES-OTHER.md` | grid construction / core metrics (`bugfix/grid-orientation`) |
| **this file** | PR #91 `info-objective` |

**Last verified:** 2026-08-04, R 4.6.1, `pkgload::load_all()`, against `d51d185`.

> **This PR has changed substantially since it was first reviewed** (11 commits then, last touched
> 2026-07-16). It now exports **4** functions, not 8: `objective_function_info()`,
> `compute_L_projection()`, `cor_ar1()`, `cor_ar1_ar1()`. `calc_incidence_matrix()`,
> `calc_concurrence_matrix()`, `calc_info_matrix()` and `calculate_efficiency_factors()` are now
> `@keywords internal`, so they are no longer API surface, and the author fixed `_pkgdown.yml`.
> Findings about those four are dropped — see §5.

**Consequence for the other branches:** the naming collision with `feature/incidence` is gone, and so
is the old argument that `calculate_position_incidence()` is redundant against this PR.

---

## 1. Assessment

**Recommendation: request changes.** The concept and statistical core are sound and worth merging.
There is one blocking silent-correctness defect (§2.1), plus a performance/contract gap and a stale
base.

**Strengths, worth stating plainly:**

- **Sound statistical core.** Rank-deficiency is handled correctly: `I` is genuinely rank `v−1`
  (treatment columns sum to the intercept, which `L` removes), so filtering to positive eigenvalues
  and summing over them is the correct contrast-space formulation. The relative eigenvalue tolerance
  (`max_eig * 1e-10`) and the disconnected-design penalty are the right instincts.
- **Identity-Σ consistency** holds by construction and is tested:
  `compute_L_projection(df, diag(n))` reduces to `.build_L_from_df()`.
- **No new hard dependencies** — base `solve`/`eigen` only.
- **Valuable direction** — optimising against the actual analysis covariance is a real improvement
  over the adjacency proxy.
- Helper-level tests are thorough and readable.

---

## 2. Findings

### 2.1 🔴 Blocking: `L`/`Σ` ordering isn't tied to coordinates

Re-verified still live at `d51d185`. `objective_function_info()` accepts `spatial_cols` and never uses
it — `.compute_info(layout_df, swap, L_matrix, block_column)` doesn't receive it, so `L_matrix` is
consumed purely **positionally** by the row order of `layout_df`.

Meanwhile `speed()` re-sorts its input to row-major ([R/speed.R:195](R/speed.R#L195)) while
`initialise_design_df()` emits column-major ([R/design_utils.R:294](R/design_utils.R#L294)). For any
grid with `nrows > 1` and `ncols > 1` those orderings differ, so the SA loop optimises against a
**scrambled** Σ — silently, with no error.

Not hypothetical: the headline example and the 4×6 integration test both hit it. The test passes only
because it asserts `rank == 5` and `is.finite(A_value)`, both insensitive to a permutation.

This is the **same class of defect** as the grid-orientation bugs in `REVIEW-NOTES-OTHER.md` (G1/G2) —
an implicit ordering contract with nothing enforcing it. Those have now been fixed on
`bugfix/grid-orientation` by reading coordinates instead of assuming an order, and the
**order-invariance test pattern from that branch applies here directly**: assert that the same physical
layout, supplied row-major and column-major, scores identically. That single test would have caught
this and is the cleanest way to prove a fix.

**Fix:** thread `spatial_cols` through so `L`/`Σ` are keyed to coordinates, or have `speed()` pass the
row permutation to the objective. At minimum, validate and document the required ordering loudly.

### 2.2 🟠 `spatial_cols` is dead in the signature

`objective_function_info()` requires `spatial_cols` — it's how `speed()` calls every objective — but the
body ignores it entirely; block structure comes from `block_column` instead. This is the root cause of
§2.1 and is misleading on its own: the signature implies spatial awareness while the spatial
information actually lives in a separately hand-built `L`. A user who sets
`spatial_factors = ~ row + col + block` and switches to this objective gets silence.

**Fix:** use it (per §2.1), or warn when it is supplied and non-trivial.

### 2.3 🟠 Ignores the incremental-state contract

`CLAUDE.md` is explicit:

> The objective function … returns a `list(score = ..., ...)`. The returned list is fed back as
> `current_score_obj` on the next call so objective functions can incrementally update internal state
> from `swapped_items` instead of recomputing from scratch — **any custom objective function must
> honour this contract**.

`objective_function_info()` accepts neither `current_score_obj` nor `swapped_items` (both fall into
`...`), and although it *returns* `info_matrix` and `eigenvalues`, it never consumes them. So every
iteration rebuilds `X1` with an R `for` loop, forms `t(X1) %*% L %*% X1` against a dense n×n `L`, and
runs `eigen()` — 10,000 times by default. A swap touches two plots, so the information-matrix update is
rank ≤ 4 and should be a downdate/update.

Compare `objective_function_piepho()`, which threads `current_score_obj$ed` and `swapped_items` into
`calculate_ed()` exactly as the contract requires.

**Fix:** follow that pattern, or consciously document waiving it given the O(n²v + v³) per-iteration
cost.

### 2.4 🟠 Behind `main`

Merge base is `72a3c94`; this PR has not merged the `summary()` work (nor, now, the grid-orientation
fix). Needs a merge before review can conclude. In particular its `_pkgdown.yml` predates `main`'s
`summary.design` / `print.summary.design` entries, and pkgdown errors when a documented export is
missing from an explicit reference index.

### 2.5 🟡 Documented `L` formula is missing a term

The roxygen drops the trailing `X₂ᵀΣ⁻¹`. The PR description and the code are correct; only the man page
is wrong.

### 2.6 🟡 Tests don't validate spatial correctness

- Helper coverage is good (incidence, concurrence, eigenvalue properties, correlation builders, input
  validation).
- The two `speed()` integration tests **would pass even with the §2.1 misalignment**, since they only
  check rank and finiteness. A meaningful test should assert the optimised design beats a fixed or
  random arrangement *under the intended* Σ, and should exercise ordering explicitly.
- `block_col =` vs `block_column` — several tests pass the wrong argument name and survive only via R's
  partial-argument matching (`compute_L_projection()` has no `...`). Fragile: breaks under
  `options(warnPartialMatchArgs = TRUE)` or if another `block_*` argument is added.
- No test for `criterion = "D"` *through* `speed()`, only direct calls.

### 2.7 🟡 Style and conventions

- `A_val = …` / `D_val = …` use `=` for assignment. Air won't fix this.
- `Sigma` is capitalised; every other argument in the package is lower snake_case. Consider `sigma` or
  `covariance`. `L_matrix` is borderline but at least reads as a proper noun.
- `compute_L_projection()`'s validation `stop()`s omit `call. = FALSE`; `R/buffers.R` and
  `R/verify_utils.R` use it. Consider moving them into `R/verify_utils.R` as a `.verify_*` helper,
  where all other input validation lives.
- Bare `setNames()` — resolves only because other files carry `@importFrom stats setNames`.
  `R/buffers.R` uses the explicit `stats::setNames()`; match that.
- Reimplements a pseudo-inverse via `eigen()` rather than reusing `pseudo_inverse()` in `R/utils.R` —
  defensible for a symmetric matrix, but say so in a comment.
- `R/objectives.R` vs `R/metrics.R` — every other `objective_function_*` lives in `metrics.R`.
  Splitting is fine but should be deliberate.
- Indicator matrices are built with `for` loops. `X[cbind(i, j)] <- 1` is the idiom
  `build_design_matrix()` now uses on `bugfix/grid-orientation`:

  ```r
  X1[cbind(seq_len(n), trt_idx)] <- 1
  X2[cbind(seq_along(blocks), as.integer(blocks))] <- 1
  ```

  The `X2` construction is also duplicated verbatim between `.build_L_from_df()` and
  `compute_L_projection()` — factor into one internal helper.
- `calculate_efficiency_factors` (plural) vs the existing `calculate_efficiency_factor` (singular) —
  one character apart, computing different things (canonical efficiency factors from the information
  matrix vs Piepho's single efficiency value). Internal now, so lower priority, but rename to
  `calculate_canonical_efficiency_factors()` if it is ever exported.

  ⚠️ **Two deferred items now want this function** (added 2026-08-06): the A-efficiency upper bound for
  `summary()`, and **G8** (`calculate_efficiency_factor()`'s `Z` omits the intercept) — both in A4/A3 of
  `REVIEW-NOTES-OTHER.md`. It is the only canonical-eigenvalue machinery in the package, so coordinate
  with this branch rather than writing a third implementation. Worth checking whether its information
  matrix includes the intercept, since that is precisely G8's defect in the singular version.
- No `NEWS.md` entry. Unrelated `DESCRIPTION` whitespace churn (`CONTRIBUTING.md` says don't restyle
  unrelated lines; the author/affiliation additions themselves are fine). Dead `.Rbuildignore` entry
  `^\.dir-locals\.el$` — no such file is tracked.

---

## 3. Pre-merge checklist

- [ ] **(blocking)** Resolve `L`/`Σ` ordering vs `speed()`'s row reorder (§2.1); align via
      `spatial_cols`.
- [ ] Add an order-invariance test that would fail under a permutation (§2.1, §2.6).
- [ ] Add a test that actually proves spatial optimality under the intended Σ (§2.6).
- [ ] Honour the incremental-update contract, or document waiving it with the cost (§2.3).
- [ ] Merge current `main` — ideally after `bugfix/grid-orientation` lands, since it fixes the same
      class of defect and the pattern is reusable (§2.4).
- [ ] Fix the documented `L` formula (§2.5).
- [ ] Fix `block_col` → `block_column` in tests (§2.6).
- [ ] Add a `NEWS.md` bullet; revert the `DESCRIPTION` whitespace churn (§2.7).
- [ ] Confirm the intended scope of the four internal analysis helpers with the author — they aren't
      used by the objective.

---

## 4. Notes for whoever reviews this

`speed`'s objective functions plug into the SA loop (`speed_hierarchical()` in `R/speed.R`) and must
return `list(score = ..., ...)`; the returned list is fed back as `current_score_obj` on the next call.
The call contract itself is **fine** here — the loop calls
`obj_function(design, swap, spatial_cols, adj_weight=, bal_weight=, current_score_obj=, swapped_items=, ...)`,
user-supplied `criterion` / `L_matrix` / `block_column` arrive by name through `speed(...)`'s dots and
match correctly, and the unused arguments fall harmlessly into `...`. `random_initialise()` also
forwards `...`, and `rlang::check_dots_used()` is satisfied. The problem is not plumbing, it's that the
objective doesn't *use* what it's given (§2.2) and doesn't reuse state (§2.3).

---

## 5. Superseded findings

Recorded so they aren't re-raised. All follow from the author demoting four exports to
`@keywords internal` and fixing `_pkgdown.yml`.

| Earlier claim | Status |
|---|---|
| The `calc_*` exports break the pkgdown reference index / docs build | **Superseded.** Demoted to internal; `_pkgdown.yml` fixed by the author. |
| `calc_*` need `treatment_column` → `swap` to match the package | **Mostly moot.** Internal now, so it's an internal-consistency nit rather than an API problem. |
| `calc_*` order treatments with `levels(factor(x))` (lexical: `T1, T10, T2`) instead of `stri_sort(numeric = TRUE)` | **No longer user-visible.** Still worth a shared `.treatment_levels()` helper if these are ever exported, since `speed()` and `R/incidence.R` both use the numeric-aware sort. |
| `calc_info_matrix()` is exported without `@return`, which `R CMD check` flags | **Superseded.** No longer exported. |
| `class(N) <- "matrix"` leaves `names(dimnames(N))`, so the matrix prints with stray header rows | **Cosmetic only now.** Internal output. |
| Eight new exports in one PR is a large API surface to review | **Superseded.** Four exports, all coherent with the objective. |
| Naming collision with `feature/incidence`'s `calculate_pair_incidence()` / `calculate_position_incidence()` | **Superseded.** No competing public API; nothing exported here is named `*incidence*`. |
