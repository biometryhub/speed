# Code Review — PR #91 "Info objective"

- **Repository:** biometryhub/speed
- **PR:** [#91 "Info objective"](https://github.com/biometryhub/speed/pull/91)
- **Author:** kaibagley (fork `kaibagley/speed`, branch `info-objective`)
- **Base:** `main` · **Footprint:** 11 commits, 12 files, +1079/-10
- **Reviewed locally on branch:** `info-objective`
- **Reviewer / date:** Sam Rogers, 2026-06-12

---

## 1. Context

`speed` rearranges treatments via simulated annealing to minimise a weighted sum of an
**adjacency score** and a **balance score**. Objective functions plug into the SA loop
(`speed_hierarchical` in `R/speed.R`) and must return `list(score = ..., ...)`; the returned
list is fed back as `current_score_obj` on the next call so objectives can *incrementally*
update from `swapped_items` rather than recompute from scratch.

This PR adds a fundamentally different objective: instead of the adjacency proxy, it scores a
design by the **Fisher information for treatment contrasts** under a user-supplied spatial
covariance — directly targeting the Cramér–Rao lower bound of the intended analysis model.

### What it adds

New file [`R/objectives.R`](R/objectives.R) (434 lines):

- `objective_function_info()` — computes the treatment information matrix
  $I = X_1^\top L X_1$ after projecting out nuisance fixed effects, scored under
  **A**-optimality ($\mathrm{tr}(I^-) = \sum 1/\lambda_i$) or contrast-space
  **D**-optimality ($-\log|I| = -\sum \log\lambda_i$) over the $v-1$ positive eigenvalues.
- `compute_L_projection()` — builds the projection
  $L = \Sigma^{-1} - \Sigma^{-1}X_2(X_2^\top\Sigma^{-1}X_2)^{-1}X_2^\top\Sigma^{-1}$
  from a user covariance $\Sigma$ and block design $X_2$.
- Analysis helpers: `calc_info_matrix()`, `calc_incidence_matrix()`,
  `calc_concurrence_matrix()`, `calculate_efficiency_factors()`.
- Correlation builders: `cor_ar1()`, `cor_ar1_ar1()`.
- Internal helpers `.compute_info()`, `.build_L_from_df()`, `.build_treatment_matrix()`.

Plus [`tests/testthat/test-objective_function_info.R`](tests/testthat/test-objective_function_info.R)
(378 lines), 8 new `NAMESPACE` exports, `man/*` docs, and `DESCRIPTION` author additions
(Kai Bagley / Curtin University).

---

## 2. Overall assessment

**Recommendation: request changes.**

The concept and statistical core are sound and worth merging eventually. However, there is a
**silent-correctness defect** (§4.1) that currently affects the package's own example and
integration tests and must be resolved before merge, plus several secondary issues around
performance, conventions, and test rigour.

---

## 3. Strengths

- **Sound statistical core.** Rank-deficiency is handled correctly: $I$ is genuinely rank
  $v-1$ (treatment columns sum to the intercept, which $L$ removes), so filtering to positive
  eigenvalues and summing over them is the correct contrast-space formulation. Relative
  eigenvalue tolerance (`max_eig * 1e-10`) and the disconnected-design penalty are the right
  instincts.
- **Identity-Σ consistency** holds by construction and is tested:
  `compute_L_projection(df, diag(n))` reduces to `.build_L_from_df`.
- **No new hard dependencies** — base `solve`/`eigen` only.
- **Valuable direction** — optimising against the actual analysis covariance is a real
  improvement over the adjacency proxy.
- Helper-level tests are thorough and readable.

---

## 4. Correctness concerns

### 4.1 🔴 `L`/`Σ` ordering is not tied to design coordinates — and `speed()` reorders rows

**Severity: high (blocking).**

`compute_L_projection` and `.compute_info` use `L_matrix` purely **positionally** by the row
order of `layout_df` ([objectives.R:188](R/objectives.R#L188)); the `spatial_cols` argument is
accepted but never used to align anything. Meanwhile `speed()` re-sorts the data by
`(row, col)` before optimising:

```r
# R/speed.R:190 — runs whenever row/col are present
#   (infer_row_col returns inferred = TRUE, design_utils.R:167-173)
data <- data[do.call(order, data[c(row_column, col_column)]), ]
```

`initialise_design_df` emits **column-major** order (`expand.grid(row, col)` varies `row`
fastest), but `speed()` re-sorts to **row-major**. For any grid with `nrows > 1` and
`ncols > 1` these orderings differ, so the SA loop optimises against a **scrambled** $\Sigma$ —
silently, with no error.

This is not hypothetical: the headline example ([objectives.R:57-76](R/objectives.R#L57)) and
the 4×6 integration test ([test:324](tests/testthat/test-objective_function_info.R#L324)) both
hit it. The test passes only because it asserts `rank == 5` and `is.finite(A_value)`, both of
which are insensitive to the permutation.

**Fix direction:** thread `spatial_cols` through so `L`/`Σ` are keyed to coordinates (reorder
`L` to match the design, or sort `Σ`'s implied order), or have `speed()` pass the row
permutation to the objective. At minimum, validate and document the required ordering loudly.

### 4.2 🟠 `spatial_cols` is dead in the new objective

`objective_function_info` requires `spatial_cols` (it is how `speed()` calls every objective)
but the body ignores it ([objectives.R:85](R/objectives.R#L85)). This is the root cause of
§4.1 and is misleading on its own: the signature implies spatial awareness, but the spatial
information actually lives in a separately hand-built `L`.

### 4.3 🟡 Documented `L` formula is missing a term

The roxygen ([objectives.R:32-33](R/objectives.R#L32)) drops the trailing
$X_2^\top\Sigma^{-1}$ term. The PR description and the code
([objectives.R:246](R/objectives.R#L246)) are correct; only the man page is wrong.

---

## 5. Integration with `speed()`

- **Call contract: OK.** The loop calls
  `obj_function(design, swap, spatial_cols, adj_weight=, bal_weight=, current_score_obj=, swapped_items=, ...)`
  ([speed.R:289](R/speed.R#L289)). User-supplied `criterion` / `L_matrix` / `block_column`
  arrive by name through `speed(...)`'s dots and match correctly; `adj_weight` / `bal_weight` /
  `current_score_obj` / `swapped_items` fall harmlessly into `...`. `random_initialise` also
  forwards `...` ([design_utils.R:401](R/design_utils.R#L401)). `rlang::check_dots_used()` is
  satisfied.
- **🟠 Ignores the incremental-state contract → performance.** Unlike
  `objective_function_piepho`, this recomputes from scratch each iteration: rebuilds `X1` in an
  R `for`-loop, forms `t(X1) %*% L %*% X1` ($O(n^2v)$ with a **dense** $n \times n$ `L`), and
  runs `eigen` ($O(v^3)$) — ×10 000 iterations by default. A swap touches only two rows; the
  info-matrix update is rank-≤4. For field-trial sizes this will be far slower than the
  adjacency objective. Not incorrect, but a real scalability weakness.

---

## 6. API, packaging & conventions

- **8 new exports in one PR.** Several (`calc_incidence_matrix`, `calc_concurrence_matrix`,
  `calculate_efficiency_factors`) are not used by the objective — they are analysis
  conveniences. Worth confirming scope with the author; each is now a maintenance and
  `R CMD check` surface.
- **New `R/objectives.R` vs. existing `R/metrics.R`** — the other `objective_function_*` all
  live in `metrics.R`. Splitting is defensible but should be deliberate.
- **🟡 `NEWS.md` not updated** — CLAUDE.md requires a bullet for user-facing changes; 8 new
  exports qualify.
- **🟡 Unrelated whitespace churn in `DESCRIPTION`** (trailing spaces stripped on `Authors@R:`,
  `Imports:`, `Suggests:`, Description block). `CONTRIBUTING.md` says don't restyle unrelated
  lines. The author/affiliation additions themselves are fine.
- **Dangling `.Rbuildignore` entry** — adds `^\.dir-locals\.el$` though no such file is tracked
  (the Emacs config correctly did not land; the ignore line is now dead config).
- Reimplements a pseudo-inverse via `eigen` rather than reusing `pseudo_inverse` in
  `R/utils.R` — minor consistency point.

---

## 7. Tests

- Helper coverage is good (incidence, concurrence, eigenvalue properties, correlation builders,
  input validation).
- **🟠 The two `speed()` integration tests don't validate spatial correctness** — they would
  pass even with the §4.1 misalignment, since they only check rank/finiteness. A meaningful
  test should assert the optimised design beats a fixed/random arrangement *under the intended*
  $\Sigma$, and should exercise ordering explicitly.
- **🟡 `block_col =` vs `block_column`** — tests at
  [test:48](tests/testthat/test-objective_function_info.R#L48), 130, 143, 188 pass the wrong
  argument name and survive only via R partial-argument matching (`compute_L_projection` has no
  `...`). Fragile: breaks under `options(warnPartialMatchArgs = TRUE)` or if another `block_*`
  argument is added. Use the real name.
- No test for `criterion = "D"` *through* `speed()` (only direct calls).

---

## 8. Minor / style

- `A_val = …` / `D_val = …` use `=` for assignment inside braces
  ([objectives.R:288-289](R/objectives.R#L288)) — slipped past the "fix lintr" commit.
- `match.arg(criterion)` is good; consider validating length-1 when called directly.

---

## 9. Pre-merge checklist

- [ ] **(blocking)** Resolve `L`/`Σ` ordering vs. `speed()` row reorder (§4.1); align via
      `spatial_cols`.
- [ ] Add a test that actually proves spatial optimality (would catch §4.1).
- [ ] Honour the incremental-update contract, or consciously document waiving it given the
      performance cost (§5).
- [ ] Fix the documented `L` formula (§4.3).
- [ ] Add a `NEWS.md` bullet (§6).
- [ ] Revert unrelated `DESCRIPTION` whitespace changes (§6).
- [ ] Fix `block_col` → `block_column` in tests (§7).
- [ ] Confirm scope of the 8 new exports with the author (§6).
