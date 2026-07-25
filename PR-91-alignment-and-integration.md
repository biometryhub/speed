# PR #91 alignment + how `feature/incidence` fits on top

Two questions:

1. Beyond `calc_` → `calculate_`, what else should PR #91 change to match the rest of the codebase?
2. Assuming #91 merges first, how should `feature/incidence` fit in?

All claims verified at the console against R 4.6.1 (`pkgload::load_all()`), with #91's functions
sourced from the `info-objective` branch.

---

## Part 1 — Aligning PR #91 with the codebase

### 1.1 The rename is load-bearing, not cosmetic 🔴

`calculate_` isn't just a house style — `_pkgdown.yml` builds its reference index from prefix
patterns:

```yaml
  - subtitle: Calculation functions
  - contents:
      - starts_with("calculate")
  - subtitle: Objective functions
  - contents:
      - starts_with("objective_function")
```

Which of #91's 8 exports match a pattern:

| Export | Matches index? |
|---|---|
| `objective_function_info` | ✅ `objective_function*` |
| `calculate_efficiency_factors` | ✅ `calculate*` |
| `calc_info_matrix` | ❌ |
| `calc_incidence_matrix` | ❌ |
| `calc_concurrence_matrix` | ❌ |
| `compute_L_projection` | ❌ |
| `cor_ar1` | ❌ |
| `cor_ar1_ar1` | ❌ |

pkgdown **errors** when exported topics are absent from an explicit `reference:` index, so as it
stands #91 breaks the docs site build. The `calc_` → `calculate_` rename fixes three of the five
automatically. The remaining two groups still need attention:

- `compute_L_projection` — either rename to `calculate_L_projection` (picked up automatically), or
  add explicitly to the index. I'd rename: the package has no `compute_*` prefix anywhere, and this
  function is doing the same kind of job as the `calculate_*` family.
- `cor_ar1` / `cor_ar1_ar1` — these are genuinely a new category (covariance-structure
  constructors). Add a new `_pkgdown.yml` section rather than contorting the names:

  ```yaml
    - title: Covariance structures
      desc: Constructors for spatial correlation structures.
    - contents:
        - starts_with("cor_")
  ```

### 1.2 `treatment_column` → `swap` 🟠

#91 is inconsistent *with itself*: `objective_function_info(layout_df, swap, spatial_cols, ...)`
uses `swap`, but all five analysis helpers use `treatment_column`. The package uses `swap`
universally — `calculate_adjacency_score(layout_df, swap, ...)`,
`calculate_balance_score(layout_df, swap, spatial_cols)`, `objective_function(layout_df, swap, ...)`.

Rename `treatment_column` → `swap` throughout. Note the one genuine exception already in the
codebase: `calculate_efficiency_factor(design_df, item)` uses `item` *and* `substitute()` for
unquoted input — that's an outlier, not a precedent to follow.

While there: decide whether the treatment argument has a default. Currently there are three
conventions in play — `calculate_adjacency_score()` has none, #91 defaults to `"treatment"`, and
`feature/incidence` defaults to `"treatment"`. Pick one (I'd default to `"treatment"`, since it's
what `initialise_design_df()` produces) and apply it consistently.

### 1.3 `levels(factor(...))` → `stri_sort(numeric = TRUE)` 🟠

`.compute_info()` and `calc_incidence_matrix()` both order treatments with `levels(factor(x))`,
i.e. lexically. `initialise_design_df(items = 12)` produces `T1`…`T12`, so:

```
PR #91 rownames : T1 T10 T11 T12 T2 T3 T4 T5 T6 T7 T8 T9
this branch     : T1 T2 T3 T4 T5 T6 T7 T8 T9 T10 T11 T12
```

The package already imports `stringi` and uses `stri_sort(..., numeric = TRUE)` in `speed()` for
exactly this reason. This affects #91's *output* (incidence/concurrence row order, and the row/col
order of the information matrix), so it's user-visible, not internal. Worth a small shared internal
helper — `.treatment_levels(x)` — used by #91, this branch, and `speed()`, so it can never drift
again.

### 1.4 Roxygen: markdown, not Rd macros 🟠

`DESCRIPTION` sets `Roxygen: list(markdown = TRUE)` and `CLAUDE.md` calls markdown roxygen the
source of truth. #91 uses `\code{...}` throughout and refers to other functions as
`\code{compute_L_projection}` — which renders as code but is **not a link**. Package style is
backticks and `[fn()]` auto-links:

```r
#' Use [calculate_L_projection()] to generate it.   # links
#' Use \code{compute_L_projection} to generate it.   # doesn't
```

Keep `\eqn{}` / `\deqn{}` — there's no markdown equivalent and the maths is good. Just convert
`\code{}` → backticks and cross-references → `[fn()]`.

### 1.5 Missing `@return` on an exported function 🟠

`calc_info_matrix()` has `@param` tags and `@export` but **no `@return`**
([objectives.R:249-262](R/objectives.R)). `R CMD check` flags missing `\value` in Rd for exported
objects, and `check()` must pass before merge. It returns a 5-element list
(`info_matrix`, `eigenvalues`, `rank`, `A_value`, `D_value`) — worth an `\describe{}` block, as
`calculate_position_incidence()` does on the other branch.

Also `objective_function_info()` places `@return` *after* `@examples`; every other file puts it
before `@export`. Harmless but inconsistent.

### 1.6 The `swapped_items` / `current_score_obj` contract 🔴

This is the biggest convention gap, and `CLAUDE.md` is explicit about it:

> The objective function … returns a `list(score = ..., ...)`. The returned list is fed back as
> `current_score_obj` on the next call so objective functions can incrementally update internal
> state from `swapped_items` instead of recomputing from scratch — **any custom objective function
> must honour this contract**.

`objective_function_info()` accepts neither `current_score_obj` nor `swapped_items` (they vanish
into `...`), and although it *returns* `info_matrix` and `eigenvalues`, it never consumes them. So
every iteration rebuilds `X1` with an R `for` loop, forms `t(X1) %*% L %*% X1` against a dense
n×n `L`, and runs `eigen()` — 10,000 times by default. A swap touches two plots, so the information
matrix update is rank ≤ 4 and the whole thing should be a downdate/update.

Compare `objective_function_piepho()`, which threads `current_score_obj$ed` and `swapped_items`
into `calculate_ed()` precisely as the contract requires. #91 should follow that pattern, or at
minimum document loudly that it is O(n²v + v³) per iteration.

### 1.7 Vectorise the indicator-matrix loops 🟡

Three places build indicator matrices with `for` loops:

```r
# .build_treatment_matrix
for (i in seq_len(n)) X1[i, trt_idx[i]] <- 1

# .build_L_from_df and compute_L_projection (duplicated)
for (j in seq_len(b)) X2[blocks == levels(blocks)[j], j] <- 1
```

Both are one-liners with the matrix-index idiom that `feature/incidence` just introduced in
`build_design_matrix()`:

```r
X1[cbind(seq_len(n), trt_idx)] <- 1
X2[cbind(seq_along(blocks), as.integer(blocks))] <- 1
```

The `X2` construction is also duplicated verbatim between `.build_L_from_df()` and
`compute_L_projection()` — factor into one internal helper. (`calculate_efficiency_factor()` on
`main` has the same loop-built `X`; not #91's job to fix, but worth knowing the idiom is already
inconsistent.)

### 1.8 Reuse `pseudo_inverse()` 🟡

`R/utils.R` already provides `pseudo_inverse()` (SVD-based Moore-Penrose with a rank guard).
`calc_info_matrix()` and `objective_function_info()` reimplement the same rank-deficiency handling
via `eigen()` with a `1e-10` relative tolerance. Reuse the helper, or explain in a comment why
`eigen` is preferred here (it is symmetric, so it's defensible — but then say so).

### 1.9 Smaller items 🟡

- **`=` instead of `<-`** at `A_val = sum(1 / pos_eig)` / `D_val = -sum(log(pos_eig))`
  ([objectives.R:288-289](R/objectives.R)). Package uses `<-` throughout; Air won't fix this.
- **`Sigma` is capitalised.** Every other argument in the package is lower snake_case. Consider
  `sigma` or `covariance`; `L_matrix` is borderline but at least reads as a proper noun.
- **`stop()` without `call. = FALSE`.** `compute_L_projection()`'s six validation `stop()`s omit
  it; `R/buffers.R` and `R/verify_utils.R` use `call. = FALSE`. Also consider moving them into
  `R/verify_utils.R` as a `.verify_*` helper, which is where all other input validation lives.
- **Bare `setNames()`** at [objectives.R:398](R/objectives.R) — resolves only because other files
  carry `@importFrom stats setNames`. `R/buffers.R` uses explicit `stats::setNames()`; match that.
- **`class(N) <- "matrix"`** leaves `names(dimnames(N))` as `c("treatments", "blocks")`, so the
  matrix prints with stray header rows:

  ```
            blocks
  treatments 1 2 3 4
         T1  1 0 0 0
  ```

  Add `names(dimnames(N)) <- NULL`, or build with `matrix(as.integer(tbl), ...)` as
  `feature/incidence` does. Pick one idiom for both branches.
- **`spatial_cols` is accepted and never used.** `objective_function_info()` takes it to satisfy the
  signature but ignores it entirely — block structure comes from `block_column` instead. A user who
  sets `spatial_factors = ~ row + col + block` and switches to this objective gets silence. Document
  it, or warn when `spatial_cols` is supplied and non-trivial.
- **`calculate_efficiency_factors` vs existing `calculate_efficiency_factor`** — singular/plural
  differing by one character, computing different things (canonical efficiency factors from the
  information matrix vs. Piepho's single efficiency value). Rename to
  `calculate_canonical_efficiency_factors()`.
- **File placement** — every other `objective_function_*` lives in `R/metrics.R`. See §2.3 for a
  suggested layout that resolves this alongside `feature/incidence`.
- **`NEWS.md`** — 8 new exports and no entry.
- **Unrelated `DESCRIPTION` whitespace churn** and the dead `.Rbuildignore` entry (both already
  noted in `PR-91-info-objective-review.md`).

---

## Part 2 — How `feature/incidence` fits on top of #91

### 2.1 The key finding: `calculate_position_incidence()` becomes redundant 🔴

These are the **same computation**. Verified on a 12-treatment, 4×3 blocked design:

```r
calc_incidence_matrix(df, block_column = "row")   # PR #91
calculate_position_incidence(df, swap)$row        # feature/incidence
#> identical values; differ only in row ordering (§1.3)
```

Both reduce to `table(treatment, spatial_factor)`. #91's version takes one factor; this branch's
takes exactly two (hardcoded `row` and `col`) and returns a list. Neither can do what users
actually need, which is *arbitrary* factors — blocks, `site_col`, `site_block`.

**So don't port `calculate_position_incidence()` on top of #91.** Instead generalise #91's function
to accept multiple factors, mirroring `speed()`'s own interface:

```r
calculate_incidence(design, swap = "treatment", spatial_factors = ~ row + col)
# -> named list of treatment x level matrices, one per factor
```

That single change:

- removes the duplicate implementation,
- fixes the hardcoded-row/col gap in this branch (review §2.4),
- fixes the block gap in this branch (review §2.4),
- and makes the argument name match `speed()`, so users write the same formula in both places.

`calculate_position_incidence()` then disappears from the diff entirely. Nothing is lost — it was
`calculate_incidence(design, spatial_factors = ~ row + col)`.

### 2.2 What `feature/incidence` uniquely contributes

Stripping out the redundancy, three things in this branch are genuinely additive and should survive:

1. **`build_design_matrix()` and the adjacency orientation fix.** The most valuable thing in the
   branch — `main` scores a 12×5 design as 0 adjacencies when it has 48 (see
   `PR-incidence-review.md` §3.3). Independent of #91. **Split this into its own PR and merge it
   first, ahead of both features.**
2. **Adjacency-based pair counting.** #91's concurrence is block-based; nothing in #91 counts
   *spatially adjacent* pairs. This is the real new capability.
3. **The print/summary layer.** Neither branch has one, and both produce v×v or v×b matrices that
   are unreadable at realistic sizes. Best built once, over a shared representation.

### 2.3 Proposed combined surface

Assuming #91 merges first with §1's renames applied:

| Function | Origin | What it does |
|---|---|---|
| `calculate_incidence(design, swap, spatial_factors)` | #91, generalised (§2.1) | treatment × factor-level counts, one matrix per factor |
| `calculate_concurrence(design, swap, spatial_factors)` | #91, generalised | pairs co-occurring in the same factor level (`N Nᵀ`) |
| `calculate_adjacency_concurrence(design, swap, ...)` | this branch, renamed | pairs that are spatially adjacent |
| `calculate_info_matrix()`, `calculate_L_projection()`, `calculate_canonical_efficiency_factors()`, `cor_ar1*()` | #91 | information-theoretic tooling |

This resolves the terminology collision in `PR-incidence-review.md` §3.1: "incidence" keeps its
established design-theory meaning (treatment × block), "concurrence" means co-occurrence, and the
adjacency variant is explicitly labelled as such. `calculate_pair_incidence` — which is neither an
incidence matrix nor block concurrence — goes away.

Suggested file layout, resolving §1.9's placement point:

- `R/metrics.R` — `objective_function_info()` joins the other `objective_function_*`.
- `R/information.R` — #91's information-matrix machinery and covariance constructors.
- `R/incidence.R` — `calculate_incidence()`, `calculate_concurrence()`,
  `calculate_adjacency_concurrence()`, and their print methods.
- `R/design_utils.R` — `build_design_matrix()` (already there).

### 2.4 One thing the shared docs must explain

The two diagonals mean completely different things, and users will compare them:

- `diag(calculate_concurrence(...))` is `sum_j n_ij²` — for a binary design, the **replication
  number** of treatment *i*.
- `diag(calculate_adjacency_concurrence(...))` is the **self-adjacency count** — the thing
  `calculate_adjacency_score()` penalises, and ideally 0.

One is "how many plots does this treatment have", the other is "how many times did it end up next to
itself". Same shape, same-looking output, opposite interpretation — a shared `@details` section or a
short vignette comparing them is worth more than any amount of per-function documentation.

### 2.5 Suggested merge order

1. **`build_design_matrix()` + adjacency orientation fix + NEWS "Bug Fixes"** — split out of
   `feature/incidence`, no dependency on #91, fixes a live correctness bug. Merge first.
2. **PR #91** with §1 applied — the renames (§1.1, §1.2), sorting helper (§1.3), roxygen (§1.4),
   `@return` (§1.5), and a decision on the incremental-update contract (§1.6).
3. **`calculate_incidence()` / `calculate_concurrence()` generalised to `spatial_factors`** (§2.1) —
   small follow-up to #91.
4. **`feature/incidence` reduced** to `calculate_adjacency_concurrence()` plus the print layer, with
   `calculate_position_incidence()` dropped and the blockers in `PR-incidence-review.md` §2.1–2.3
   addressed.

Step 3 is where the two branches actually meet, and it's small — which is the argument for
sequencing it this way rather than merging both features and reconciling afterwards.
