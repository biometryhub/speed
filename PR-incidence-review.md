# Review: `feature/incidence`

**Reviewer perspective:** user experience — is this something a biometrician can pick up and use
correctly without reading the source?

**Stated objective:** functions to calculate and print (a) the incidence of treatments appearing
next to, or in the same row/column as, another replicate of the same treatment, and (b) the
frequency of pairs of treatments appearing together.

**Scope reviewed:** `main...feature/incidence` — 14 files, +615/−11. New `R/incidence.R`
(186 lines), new internal `build_design_matrix()` in `R/design_utils.R`, refactor of
`calculate_adjacency_score()` and `objective_function_piepho()` onto it, 2 new test files
(217 lines), 3 new `man/` pages, `NEWS.md`, `DESCRIPTION` 0.0.8 → 0.0.9.

**Method:** findings were derived by reading the code, then **verified at the console** against
R 4.6.1 with `pkgload::load_all()`. Numbers quoted below are measured, not inferred.

**Recommendation: request changes.** The maths in `calculate_pair_incidence()` is correct and the
edge enumeration is clean and vectorised. But as a *user-facing feature* it is not there yet: the
happy path fails for any design whose columns aren't literally named `treatment`/`row`/`col`, it
returns silently wrong answers for multi-site designs, the "print" half of the objective is
unimplemented, and the "pairs of treatments appearing together" half is answered by a *different
open PR* using a different naming convention.

---

## 1. Summary of findings

| # | Severity | Finding |
|---|----------|---------|
| 2.1 | 🔴 | Silently wrong on multi-site (MET) designs; the two functions disagree with each other |
| 2.2 | 🔴 | Passing a `design` object only works if columns are named `treatment`/`row`/`col` |
| 2.3 | 🔴 | "Print" is not implemented — output is unusable at realistic trial sizes |
| 2.4 | 🟠 | Objective only half met: no pairwise co-occurrence, no block support |
| 2.5 | 🟠 | Naming/semantic collision with PR #91 (`calc_incidence_matrix`, `calc_concurrence_matrix`) |
| 3.1 | 🟠 | "Incidence" conflicts with its established meaning in design theory |
| 3.2 | 🟠 | Cannot match `calculate_adjacency_score()`'s neighbourhood; docs overclaim the link |
| 3.3 | 🟠 | Undocumented bug fix: `objective_function_piepho()` was scoring a scrambled grid inside `speed()`; adjacency becomes robust for direct calls |
| 3.4 | 🟡 | `add_buffers()` output pollutes the incidence matrix, inconsistently |
| 3.5 | 🟡 | Hot-loop performance regression in `build_design_matrix()` |
| 3.6 | 🟡 | Duplicates `calculate_nb()`'s neighbour enumeration, and disagrees with it on NA |
| 3.7 | 🟡 | `calculate_position_incidence()` docs overclaim the balance-score link |
| 4.x | 🟡/⚪ | `as_list`, error messages, column ordering, test gaps, `.gitignore`, NEWS |

---

## 2. Blocking issues

### 2.1 🔴 Silently wrong on multi-site designs — and the two functions disagree

`build_design_matrix()` ([R/design_utils.R:648](R/design_utils.R#L648)) places values by
coordinate:

```r
m[cbind(as_numeric_factor(df[[row_column]]),
        as_numeric_factor(df[[col_column]]))] <- as.character(df[[swap]])
```

If two data-frame rows share a `(row, col)` coordinate, **the later one silently overwrites the
earlier one**. No warning, no error.

This is not hypothetical — it is the MET example in `speed()`'s own documentation
([R/speed.R:118-122](R/speed.R#L118-L122)):

```r
df_site   <- initialise_design_df(1, 28, 5, 14, 5)      # row 1:28, col 1:5
df_initial <- rbind(df_site, df_site, df_site, df_site, df_site)
df_initial$site <- rep(c("a", "b", "c", "d", "e"), each = 140)
```

Five sites, each reusing `row` 1:28 and `col` 1:5. The design disambiguates spatially via
`site_row`/`site_col`. A user who runs that documented example and then asks the obvious
follow-up question —

```r
calculate_pair_incidence(result, swap = "lines")
```

— gets a 28 × 5 grid containing **site "e" only** (the last 140 rows to be assigned), reported as
if it were the whole 700-plot trial. Every count is ~1/5 of the truth. For the exact question the
user wants answered — "did any line end up next to itself?" — this returns a confidently wrong,
reassuringly low number.

Worse, the sibling function does something *different*: `calculate_position_incidence()` uses
`table(trt_fac, df[[row_column]])` over the full data frame
([R/incidence.R:171](R/incidence.R#L171)), so it **pools across all five sites** — row "1" mixes
five different physical rows. So on the same input, one function silently drops 80% of the data and
the other silently aggregates over a factor it doesn't know exists. Neither warns.

**Suggested fix.** Two parts:

1. `build_design_matrix()` should refuse to guess. `if (anyDuplicated(cbind(r, c))) stop(...)` with
   a message naming the offending coordinates — a hard error here is much kinder than a wrong
   matrix.
2. Give both functions a `by` / `within` argument (e.g. `by = "site"`) that splits the design and
   returns per-group results, or at minimum have them detect that `nrow(df) > max(row) * max(col)`
   and error with a pointer to that argument.

### 2.2 🔴 Passing a `design` object only works by coincidence

Both functions accept a `design` object, which reads as "this integrates with `speed()`":

```r
df <- if (inherits(design, "design")) design$design_df else design
```

But they then fall back to `swap = "treatment"`, `row_column = "row"`, `col_column = "col"`. The
`design` object records **neither** the swap column name nor the grid factors — its documented
fields are `design_df`, `score`, `scores`, `temperatures`, `iterations_run`, `stopped_early`,
`treatments` (the treatment *values*, not the column name) and `seed`
([R/speed.R:46-58](R/speed.R#L46-L58)). So:

```r
result <- speed(df, swap = "variety", seed = 42)
calculate_pair_incidence(result)
#> Error: Column(s) not found in data: treatment
```

The user did everything right. The object knows the answer. They still get an error, and the error
doesn't tell them that `swap =` is the argument to set.

It's worse for the grid columns, because `speed()` is *actively more capable* here. `infer_row_col()`
([R/design_utils.R](R/design_utils.R)) matches `^(col(umn|)|range)(s|)$` case-insensitively — so a
field trial with a `range` column optimises fine, and even prints `row and range are used as row and
column, respectively`. Then `calculate_pair_incidence(result)` errors on the missing `col`. The
diagnostic tool is less capable than the optimiser it diagnoses.

**Suggested fix.**

- Add `swap` (the column name) and the resolved `grid_factors` to the `design` object in `speed()`,
  and have both functions default to them when `inherits(design, "design")`. Then
  `calculate_pair_incidence(result)` just works — which is surely the intended headline call.
- Fall back to `infer_row_col(df, quiet = TRUE)` for plain data frames so `range` is supported
  consistently.
- Note this is invisible to the current tests because both "design object accepted as input" tests
  use a hand-built `structure(list(design_df = df), class = "design")` *and* pass `swap = "trt"`
  explicitly ([test-pair-incidence.R:86-97](tests/testthat/test-pair-incidence.R#L86-L97)). No test
  ever calls either function against a real `speed()` result, and no test ever exercises the
  defaults.

### 2.3 🔴 The "print" half of the objective is not implemented

The objective says *calculate and print*. There is no `print` method, no `summary` method, no
`autoplot` method, and neither return value carries a class — `NAMESPACE` still has exactly two S3
methods (`print.design`, `autoplot.design`).

That matters because the raw containers don't scale. The MET example has 100 treatments:
`calculate_pair_incidence()` returns a 100 × 100 integer matrix — 10,000 numbers, overwhelmingly
zero — dumped to the console, wrapping across dozens of screens. `calculate_position_incidence()`
returns a 100 × 28 and a 100 × 5 matrix, and to answer "which treatments share a row with
themselves?" the user must visually scan 2,800 cells for values `> 1`.

The functions produce the raw ingredients but leave the user to do the actual analysis. For the
stated use case the interesting output is tiny:

```
Adjacency incidence: 3 of 100 treatments are self-adjacent
  T14 (2 occurrences), T57 (1), T88 (1)
Row incidence: 4 treatments appear more than once in a row
  T3 (row 7, ×2), ...
Most frequent neighbour pairs: T2–T44 (3), T9–T17 (3), ...
```

**Suggested fix.** Give the return values classes (`speed_pair_incidence`,
`speed_position_incidence`) wrapping the matrices, with `print` methods that surface the
violations and summarise the rest, plus a `summary`/`as.data.frame` for programmatic use. Keep the
matrix accessible underneath so nothing is lost. An `autoplot` (heatmap for the pair matrix) would
be a natural third step and fits the package's existing ggplot2 tooling.

### 2.4 🟠 The objective is only half met

Mapping the objective onto what shipped:

| Stated goal | Delivered? |
|---|---|
| Treatments next to another rep of themselves | ✅ `diag(calculate_pair_incidence(...))` |
| Treatments in the same **row/column** as another rep of themselves | ⚠️ Only indirectly — marginal counts, user must spot `> 1` |
| Frequency of **pairs of treatments appearing together** | ❌ Only *adjacent* pairs |

Two real gaps:

- **No pairwise co-occurrence.** "Appearing together" in design language means co-occurring in the
  same row / column / block — the concurrence matrix. `calculate_pair_incidence()` only counts
  pairs that are *physically adjacent*; `calculate_position_incidence()` gives per-treatment
  marginals, not pairs. So *nothing in this branch answers "how often do treatments i and j share a
  row?"* — and that appears to be one of the three things asked for.
- **No block support.** `calculate_position_incidence()` hardcodes exactly two dimensions, `row`
  and `col`, while `speed()` optimises over arbitrary `spatial_factors` (the MET example uses
  `~ site_col + site_block`). A user optimising `~ row + col + block` can get a report on two of
  their three spatial factors. Blocks are the most common structure in the package's target
  audience.

**Suggested fix.** Replace the `row_column`/`col_column` pair with a `spatial_factors` argument
mirroring `speed()` (accepting the same `~ row + col` formula), returning one matrix per factor.
That fixes the block gap and the naming asymmetry in one move, and makes the function's output
actually align with what was optimised.

### 2.5 🟠 Head-on collision with PR #91

`info-objective` (reviewed in `PR-91-info-objective-review.md`) exports
`calc_incidence_matrix()` and `calc_concurrence_matrix()`
([`R/objectives.R:316`, `:344` on that branch](R/objectives.R)):

```r
calc_incidence_matrix(layout_df, treatment_column = "treatment", block_column = "block")  # N
calc_concurrence_matrix(layout_df, treatment_column = "treatment", block_column = "block") # N Nᵀ
```

If both branches merge, users get **four** overlapping functions across **two** naming conventions
and **two** meanings of "incidence":

| Function | Branch | Prefix | Treatment arg | Meaning of "incidence" |
|---|---|---|---|---|
| `calc_incidence_matrix` | #91 | `calc_` | `treatment_column` | treatment × block (classical `N`) |
| `calc_concurrence_matrix` | #91 | `calc_` | `treatment_column` | pairs sharing a block |
| `calculate_pair_incidence` | this | `calculate_` | `swap` | pairs that are adjacent |
| `calculate_position_incidence` | this | `calculate_` | `swap` | treatment × row/col |

Note that **`calc_concurrence_matrix()` already delivers §2.4's missing piece** — "pairs of
treatments appearing together" — for blocks. And the two branches even sort treatments
differently: #91 uses `factor(...)` (lexical: `T1, T10, T100, T2`) while this branch uses
`stringi::stri_sort(..., numeric = TRUE)` (`T1, T2, ..., T10`). The same treatment set will print
in different row orders depending on which function you call. That is a genuinely confusing user
experience, and it's the kind of thing that is very hard to fix after release.

**Suggested fix.** Reconcile the two PRs before either merges. Concretely: one prefix
(`calculate_`, matching the existing `calculate_*` family), one treatment-column argument name
(`swap`, matching the rest of the package), one sort helper, and an explicit split of
responsibilities — e.g. `calculate_incidence()` for treatment × factor, `calculate_concurrence()`
for pairs sharing a factor level, and `calculate_adjacency_concurrence()` for pairs that are
spatially adjacent. This branch's `calculate_pair_incidence()` is the odd one out on naming and
should probably be the one to move.

---

## 3. Significant issues

### 3.1 🟠 "Incidence" means something else to this audience

In experimental design, the **incidence matrix** is conventionally the treatment × block matrix
`N`, and pairwise co-occurrence is the **concurrence matrix** `N Nᵀ`. For a GRDC-funded package out
of a biometry group, a user seeing `calculate_pair_incidence()` will reasonably expect the
classical concurrence matrix. They get adjacency counts instead. `calculate_position_incidence()`
is closer to correct usage (treatment × row and treatment × column *are* incidence matrices) —
which makes the inconsistency between the two names worse, not better.

`calculate_pair_incidence()` is, precisely, an *adjacency concurrence* matrix. Naming it so — or at
minimum stating the relationship to the standard terms in `@description` — would prevent a
predictable misreading. See also §2.5.

### 3.2 🟠 Can't reproduce the neighbourhood that was actually optimised

`calculate_adjacency_score()` is parameterised by `ring_dists`, `ring_weights`, `ring_type`
(`"manhattan"`/`"chebyshev"`) and `relationship`
([R/calculate_adjacency_score.R:242-254](R/calculate_adjacency_score.R#L242-L254)), and
`objective_function()` forwards all four through `...`
([R/metrics.R:58-69](R/metrics.R#L58-L69)). `calculate_pair_incidence()` supports **none** of them —
it hardcodes rook adjacency at distance 1.

So a user who optimises with `ring_dists = c(1, 2)`, or `ring_type = "chebyshev"`, or a
`relationship` matrix, gets an incidence report that describes a **different neighbourhood than the
one the design was optimised for** — with nothing to indicate the mismatch. Same for
`adj_weight = 0` (as in the MET example): the function will happily report adjacency counts for a
design where adjacency was never part of the objective.

This makes the doc claim overreach ([R/incidence.R:9-11](R/incidence.R#L9-L11)):

> The diagonal holds self-adjacency counts — the quantity that `calculate_adjacency_score()`
> penalises.

True only at the defaults. Measured on one 3×3 design:

| Quantity | Value |
|---|---|
| `sum(diag(M))` | 3 |
| `calculate_adjacency_score()` (defaults) | 3 ✅ |
| `calculate_adjacency_score(ring_type = "chebyshev")` | 5 |
| `calculate_adjacency_score(ring_dists = c(1, 2), ring_weights = c(1, 1))` | 6 |

So the relationship does hold exactly at the defaults — and the incidence report understates the
optimised objective by up to 2× as soon as the user changes the neighbourhood, with nothing to
signal it.

The relationship is also **never tested**. There is no assertion anywhere that
`sum(diag(M)) == calculate_adjacency_score(df, swap)`. That is the single highest-value test this
branch could add: I confirmed it passes today, it pins the documented claim, and it would catch
future drift between the two neighbour enumerations (§3.6).

**Suggested fix.** Accept and forward `ring_dists`/`ring_weights`/`ring_type`, or scope the docs
honestly to rook-distance-1 and say so in the description rather than implying equivalence.

Related, pre-existing on `main` and worth fixing if these args get forwarded:
`calculate_adjacency_score(d, "trt", ring_dists = c(1, 2))` **errors** —
`adjacency_score_vec()` asserts `length(dists) == length(weights)` but `ring_weights` defaults to
scalar `1`, so the documented default is unusable with multi-ring `ring_dists`. It should recycle.

### 3.3 🟠 This branch silently fixes a real bug — in `objective_function_piepho()`, not adjacency

> **Correction.** An earlier revision of this review claimed adjacency scoring was broken for all
> non-square designs produced by `speed()`. That was wrong. `speed()` sorts its input at
> [R/speed.R:188-192](R/speed.R#L188-L192) —
> `data[do.call(order, data[c(row_column, col_column)]), ]`, with the comment *"Sort the data frame
> to start with to ensure consistency in calculating the adjacency later"* — which produces
> **row-major** order, exactly what `byrow = TRUE` expects. Verified: `matrix(byrow = TRUE)` on
> sorted data is `identical()` to coordinate placement for 3×3, 4×3, 3×4, 6×4, 4×4 and 12×5. Designs
> produced by `speed()` with numeric row/col columns were correct.

The real story is more interesting. Two functions assumed **opposite** data orderings, and neither
read coordinates:

| Function | Fill | Assumes | Correct inside `speed()` (row-major)? | Correct on raw `initialise_design_df()` (column-major)? |
|---|---|---|---|---|
| `calculate_adjacency_score()` | `matrix(..., byrow = TRUE)` | row-major | ✅ | ❌ |
| `objective_function_piepho()` | `matrix(...)` | column-major | ❌ | ✅ |

Each is wrong precisely where the other is right. So the genuine bug this branch fixes is in
**piepho**, which receives `speed()`'s row-major data and fills it column-major. Verified wrong for
**every** shape tested — including square grids, so the transpose-invariance let-off doesn't apply.
On a 4×3 design:

```
piepho old grid          correct grid
  T1  T2  T3               T1  T1  T1
  T1  T2  T4               T2  T2  T2
  T1  T3  T4               T3  T3  T3
  T2  T3  T4               T4  T4  T4
```

The multiset of treatments is preserved (so replication counts look right), but the spatial
arrangement is scrambled — which is the entire input to `calculate_ed()`. Anyone using
`obj_function = objective_function_piepho` has designs optimised against a layout that isn't theirs.

Two narrower issues the fix also resolves:

- **`calculate_adjacency_score()` is exported and doesn't compose with `initialise_design_df()`.**
  The latter produces column-major data ([R/design_utils.R:304](R/design_utils.R#L304)), so a direct
  call `calculate_adjacency_score(initialise_design_df(...), "treatment")` is wrong on `main` — a 4×3
  design scores 0 when the answer is 8. The function's own examples happen to use hand-written
  row-major data, so they pass. Two exported functions in the same package that silently disagree.
- **The sort is applied to factors, so lexical level order can defeat it.** `to_factor()` runs at
  [R/speed.R:185](R/speed.R#L185) *before* the sort, so a **character** row column with ≥10 rows gets
  levels `1, 10, 11, 2, …` and `order()` follows them. Demonstrated: an 11×1 design with a genuine
  like-pair at rows 1-2 scores **0** on `main` and **1** correctly. Integer row columns are
  unaffected (`as.factor(1:12)` sorts numerically).

Remaining process points, unchanged:

- `NEWS.md` says nothing about any of this — two "Major Changes" bullets for the new diagnostics, no
  "Bug Fixes" heading.
- `build_design_matrix()` has **zero tests**, despite now underpinning `calculate_adjacency_score()`,
  `objective_function_piepho()` and `calculate_pair_incidence()`.

Also confirmed harmless: `matrix()` on a factor already returns a **character** matrix in R 4.6.1,
so `calculate_ed()`'s type-sensitive
`design_matrix[!(design_matrix %in% swapped_items)] <- NA`
([R/metrics.R:388](R/metrics.R#L388)) is unaffected by the change.

**Suggested fix.** See `PLAN-adjacency-orientation-fix.md` for the full plan. In short: a `NEWS.md`
"Bug Fixes" entry naming `objective_function_piepho()` as the affected code path (piepho users need
to regenerate designs; default-objective users do not), plus `test-build_design_matrix.R` and
direct-call regression tests for `calculate_adjacency_score()`.

### 3.4 🟡 Buffers pollute the incidence matrix, and inconsistently

`create_buffers()` fills treatment columns with the literal value `"buffer"`
([R/buffers.R:112](R/buffers.R#L112)). So on a buffered design,
`calculate_pair_incidence()` reports `"buffer"` as a treatment, and `M["buffer", "buffer"]` is a
large number — buffer plots are mostly adjacent to each other. This immediately falsifies the
documented reassurance that "a well-optimised design will have zeros (or near-zeros) on the
diagonal", and there is no argument to exclude anything.

It's inconsistent, too. `add_buffers()` only fills a column named exactly `"treatment"` when
`treatment_cols` is `NULL` ([R/buffers.R:99-107](R/buffers.R#L99-L107)); other columns are left
`NA`. So a design with a `treatment` column gets a polluted incidence matrix, while the same design
with a `variety` column gets `NA` buffers that the function correctly drops. Identical workflows,
different answers, depending on a column name.

**Suggested fix.** An `exclude` argument (defaulting to `"buffer"`, since that value is
package-generated) is the cheap fix. Better: have `add_buffers()` record which plots are buffers in
a dedicated column so downstream functions can filter reliably rather than string-matching.

### 3.5 🟡 Hot-loop performance regression

`build_design_matrix()` now runs once per SA iteration (default 10,000, per level, times
`random_initialisation` restarts). Compare the coercion work:

```r
# before: 2 passes
max(as_numeric_factor(df[[row_column]]))   # 1
max(as_numeric_factor(df[[col_column]]))   # 2

# after: 4 passes + an extra as.character + cbind alloc + scattered subassignment
nr <- max(as_numeric_factor(df[[row_column]]))   # 1
nc <- max(as_numeric_factor(df[[col_column]]))   # 2
m[cbind(as_numeric_factor(df[[row_column]]),     # 3
        as_numeric_factor(df[[col_column]]))] <- # 4
  as.character(df[[swap]])
```

`as_numeric_factor` is `as.numeric(as.character(x))` ([R/utils.R:301](R/utils.R#L301)) — two full
allocating passes each. That's roughly double the per-iteration coercion cost, plus a scatter-assign
instead of a contiguous fill, in the innermost loop of a package called `speed`. And it is entirely
avoidable: **the row and column vectors never change across iterations** — only the `swap` column
does.

**Suggested fix.** Hoist the invariant part. Compute `nr`, `nc` and the `cbind(r, c)` index matrix
once per level (outside the SA loop) and pass them in, or memoise on the row/col columns. A
low-level `build_design_matrix(df, swap, index = precomputed)` variant keeps the public behaviour
while removing the per-iteration cost. Worth benchmarking a realistic case (700 plots × 10,000
iterations) before and after — the fix is cheap enough that it's worth doing regardless.

### 3.6 🟡 Duplicates `calculate_nb()`, and disagrees with it

`calculate_nb()` already enumerates rook-adjacent pairs from a design matrix
([R/metrics.R:274-285](R/metrics.R#L274-L285)):

```r
lefts <- design_matrix[, -ncol(design_matrix)]; rights <- design_matrix[, -1]
tops  <- design_matrix[-nrow(design_matrix), ]; bottoms <- design_matrix[-1, ]
lr_pairs <- paste(lefts, rights, sep = ","); tb_pairs <- paste(tops, bottoms, sep = ",")
nb <- table(sorted_pairs)
```

That is the same computation as `calculate_pair_incidence()`, in a different container. The package
now has two independent implementations of "enumerate adjacent treatment pairs" that can drift, and
they **already disagree on NA**: `calculate_nb()` uses `paste()`, which stringifies `NA` into the
literal pair `"NA,A"`, while `calculate_pair_incidence()` explicitly drops NA edges
([R/incidence.R:96-97](R/incidence.R#L96-L97)). So `calculate_nb()$nb` and
`calculate_pair_incidence()` give different answers for the same design with missing plots — and
missing plots are exactly what buffered and irregular trials have.

**Suggested fix.** Express one in terms of the other, or factor the edge enumeration into a single
internal helper (`.adjacent_pairs(m)`) that both call, and settle the NA question once. Add a test
asserting the two agree.

### 3.7 🟡 `calculate_position_incidence()` docs overclaim

> This is the human-readable decomposition of what `calculate_balance_score()` collapses to a
> scalar.

Three ways that's not quite right. `calculate_balance_score()`
([R/metrics.R:164-175](R/metrics.R#L164-L175)) is `sum(rowVars(table(...)))` — a sum of **variances**
of counts, not the counts; a user can't recover the score from these matrices without knowing that.
It operates over arbitrary `spatial_cols`, not just row and col (§2.4). And it tabulates
`table(layout_df[[el]], layout_df[[swap]])` — spatial as rows, treatment as columns — the
**transpose** of what this function returns.

**Suggested fix.** Either soften to "related to" and state the actual relationship, or return the
per-factor variances alongside the counts so the claim becomes literally true.

---

## 4. Polish

### 4.1 🟡 `as_list` is redundant API surface

The parameter's own documentation concedes it
([R/incidence.R:22-24](R/incidence.R#L22-L24)): "This is the matrix split by row — identical data,
different container." A `t × t` matrix already supports `M["A", ]`, so `as_list = TRUE` adds a
parameter to document, test and maintain forever for no capability. It's also asymmetric — only
`calculate_pair_incidence()` has it, and its return type now depends on an argument value, which
complicates every downstream `print`/`summary` method added for §2.3. Recommend dropping it before
release; it's much easier to add later than remove.

### 4.2 🟡 Error messages are thin and duplicated

```r
stop("Column(s) not found in data: ", paste(missing_cols, collapse = ", "), call. = FALSE)
```

`call. = FALSE` removes the only clue about *where* the error came from, so the user sees a bare
`Error: Column(s) not found in data: treatment` with no function name, no indication that `swap =`
is the argument to set, and no list of what *is* available. The package already imports `rlang`,
which makes `cli`-style messages with a bullet list of available columns cheap. The 8-line block is
also copy-pasted verbatim into both functions — worth a `.verify_incidence_cols()` helper alongside
the existing `.verify_*` family in `R/verify_utils.R`.

### 4.3 🟡 Position columns aren't sorted with the same care as treatments

`calculate_pair_incidence()` carefully uses `stri_sort(..., numeric = TRUE)` for treatment levels,
but `calculate_position_incidence()` passes the position column straight to `table()`
([R/incidence.R:171-172](R/incidence.R#L171-L172)). If `row` is character (or a factor with default
lexical levels), the columns come out `1, 10, 11, 12, 2, 3, ...` — so `res$row[, 2]` is row 10, not
row 2. Given the branch commit "Working on using factors for spatial columns" this seems in scope.
Apply `stri_sort(numeric = TRUE)` to the position levels too.

### 4.4 🟡 Test gaps

The 17 tests that exist are focused and correct — I verified the 3×3 Latin square case by hand and
the expected all-pairs-`= 4` result is right. But the coverage has holes that map onto every
blocking issue above:

- No test asserts `sum(diag(M)) == calculate_adjacency_score(df, swap)` — the documented
  relationship (§3.2). I verified it holds (both `3` on a 3×3 test case), so this is a free test.
- No test uses a real `speed()` result; both "design object" tests use a hand-built fake **and**
  pass `swap` explicitly, so the defaults and the `design`-object integration are untested (§2.2).
- No test for duplicate coordinates / multi-site (§2.1), buffered designs (§3.4), or
  `build_design_matrix()` at all (§3.3).
- The "missing column gives informative error" tests use `expect_error(..., "col")` — which matches
  the word "Column" in the message regardless of whether the missing column is named. The test
  cannot fail for the reason it claims to check. Match on the actual column name.
- No test with a factor `swap` column carrying unused levels: `unique(as.character(...))` drops
  them, so a treatment with zero plots silently vanishes from the matrix, while `speed()`'s
  `table()`-based balance score would have counted it.

### 4.5 ⚪ `.gitignore` scope creep and risk

```gitignore
*\.RData
*\.rds
*\.txt
*\.xlsx
*\.csv
```

Two problems. The backslashes are regex escaping, not gitignore glob syntax — they happen to work
(`\.` collapses to a literal `.`) but they're misleading and will get copied. More substantively,
blanket-ignoring `*.csv`, `*.txt` and `*.rds` across an R package is risky: it will silently
swallow `inst/extdata/` fixtures, test data, and `data-raw/` inputs, and the failure mode is a
contributor whose file "didn't get committed" with no error. Scope these to the directories where
scratch files actually accumulate, and drop them from this branch — they're unrelated to the
feature.

### 4.6 ⚪ NEWS, versioning, and docs integration

- The two new functions are filed under **Major Changes**. Two additive diagnostic helpers that
  change no existing behaviour read as **Minor Changes**; the genuinely major item in this branch is
  the undocumented adjacency fix (§3.3), which isn't listed at all.
- This branch is the one that added to `CLAUDE.md`: *"Link to an issue number or PR such as (#999)
  where relevant."* Neither new bullet has a link.
- Neither function is mentioned in `README.Rmd` or any vignette, and `?speed` doesn't `@seealso`
  them. They're discoverable only via the pkgdown reference index (where `starts_with("calculate")`
  does pick them up correctly — no `_pkgdown.yml` change needed). For a feature whose whole purpose
  is *inspecting* a design, a few lines in the main vignette right after the `autoplot()` example is
  where users will actually find it.
- `CLAUDE.md`'s "Source layout (R/)" section isn't updated for the new `R/incidence.R`.
- `setNames()` is used bare at [R/incidence.R:111](R/incidence.R#L111); it resolves only because
  `R/buffers.R` and `R/metrics.R` happen to carry `@importFrom stats setNames`. `R/buffers.R` uses
  the explicit `stats::setNames()`. Match that, or add the tag to this file.

---

## 5. What's good

Worth saying plainly, because the critique above is long:

- **The pair-counting logic is correct and well constructed.** `raw + t(raw)` with
  `diag(M) <- diag(raw)` is the right way to fold ordered edge counts into a symmetric matrix while
  keeping self-adjacency counted once, and it's fully vectorised rather than looped. The `nc >= 2` /
  `nr >= 2` guards correctly handle single-row and single-column designs, and both are tested.
- **`build_design_matrix()` is the right abstraction** and fixes a real orientation bug (§3.3). It
  just needs tests, a duplicate-coordinate guard, and the invariant work hoisted out of the loop.
- **NA handling in `calculate_pair_incidence()` is deliberate and tested** — dropping edges rather
  than counting `"NA"` as a treatment is the correct choice (and the one `calculate_nb()` gets
  wrong).
- **Roxygen is genuinely good** — `@seealso` cross-links, runnable examples, `@inheritParams` to
  avoid duplication, and the return values are documented in enough detail to use. The overclaims
  in §3.2/§3.7 are the flip side of docs that bother to explain *why* the function exists, which is
  more than most packages manage.

---

## 6. Suggested path to merge

Minimum to unblock:

1. **Document and test the adjacency orientation fix (§3.3).** This is the most consequential change
   in the branch and currently the least documented. Arguably it should be split into its own PR and
   merged first, so the fix isn't gated on the review of a new feature.
2. Guard duplicate coordinates in `build_design_matrix()` and decide the multi-site story (§2.1).
3. Store `swap` + resolved grid factors on the `design` object; default to them; fall back to
   `infer_row_col()` (§2.2).
4. Add `print` methods so the functions satisfy "print" and are usable at 100 treatments (§2.3).
5. Reconcile naming and scope with PR #91 before either merges (§2.5, §3.1) — see
   `PR-91-alignment-and-integration.md`.
6. Add the `sum(diag(M)) == calculate_adjacency_score(...)` cross-check test (§3.2).

Then, ideally in the same PR: `spatial_factors` instead of hardcoded row/col (§2.4), hoist the
hot-loop coercions (§3.5), drop `as_list` (§4.1), and revert the `.gitignore` churn (§4.5).
