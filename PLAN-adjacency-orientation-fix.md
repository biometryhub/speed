# Plan: make grid construction coordinate-based (fixes `objective_function_piepho()`)

**Suggested branch:** `bugfix/grid-orientation` (off `main`) — matches the existing
`bugfix/adjacency-sorting`, `bugfix/ed` naming.

**Status of evidence:** every claim was measured at the console against R 4.6.1 with
`pkgload::load_all()`. Test constants in §6 are hand-derivable, so the tests assert *correct* values
rather than pinning current behaviour.

> **This plan was revised after review feedback.** The first version claimed
> `calculate_adjacency_score()` was broken for all non-square designs produced by `speed()`. That
> was **wrong** — `speed()` sorts its input to row-major, which is what `byrow = TRUE` expects. The
> corrected analysis is in §1; the genuinely broken function is `objective_function_piepho()`, and
> it is broken for *all* grid shapes including square ones.

---

## 1. The bug

### 1.1 What `speed()` already does

`speed()` sorts its input before optimising ([R/speed.R:188-192](R/speed.R#L188-L192)):

```r
if (inferred$inferred) {
  # Sort the data frame to start with to ensure consistency in calculating the adjacency later
  data <- data[do.call(order, data[c(row_column, col_column)]), ]
  rownames(data) <- seq_len(nrow(data))
}
```

`order(row, col)` produces **row-major** order — row varies slowest, col fastest. `byrow = TRUE`
fills row-major. So the sort and `calculate_adjacency_score()` are correctly paired, and the comment
says so explicitly. Verified: on sorted data, `matrix(..., byrow = TRUE)` is `identical()` to
coordinate placement for 3×3, 4×3, 3×4, 6×4, 4×4 and 12×5.

**Designs produced by `speed()` with the default objective and numeric row/col columns were
correct.** No regeneration needed.

### 1.2 The actual bug: two functions, opposite assumptions

Neither function reads coordinates; each hardcodes an ordering assumption, and they disagree:

| Function | Fill | Assumes | Inside `speed()` (row-major) | Raw `initialise_design_df()` (column-major) |
|---|---|---|---|---|
| `calculate_adjacency_score()` | `matrix(..., byrow = TRUE)` | row-major | ✅ correct | ❌ wrong |
| `objective_function_piepho()` | `matrix(...)` (column-major) | column-major | ❌ **wrong** | ✅ correct |

Each is wrong exactly where the other is right. `initialise_design_df()` produces
`expand.grid(row = 1:nrows, col = 1:ncols)` ([R/design_utils.R:304](R/design_utils.R#L304)), which is
column-major — the opposite of what `speed()` feeds the objective.

### 1.3 Impact 1 — `objective_function_piepho()` (all shapes) 🔴

Piepho receives `speed()`'s row-major data and fills column-major. Verified **not identical** to
coordinate placement for 3×3, 4×3, 6×4, 4×4 and 12×5 — square grids included, so the
transpose-invariance that saved adjacency does not apply here. On a 4×3 design:

```
piepho old grid          correct grid
  T1  T2  T3               T1  T1  T1
  T1  T2  T4               T2  T2  T2
  T1  T3  T4               T3  T3  T3
  T2  T3  T4               T4  T4  T4
```

The multiset of treatments is preserved, so replication counts look plausible, but the spatial
arrangement — the entire input to `calculate_ed()` — is scrambled. **Anyone who used
`obj_function = objective_function_piepho` has designs optimised against a layout that isn't
theirs.** These need regenerating.

### 1.4 Impact 2 — `calculate_adjacency_score()` doesn't compose 🟠

It is exported, and wrong when handed anything that isn't already row-major — including the output of
the package's own `initialise_design_df()`:

```r
df <- initialise_design_df(items = rep(paste0("T", 1:4), length.out = 12),
                           nrows = 4, ncols = 3)
calculate_adjacency_score(df, "treatment")   # main: 0    correct: 8
```

Its documented examples use hand-written row-major data, so they pass and the inconsistency is
invisible. Two exported functions in the same package that silently disagree about layout.

### 1.5 Impact 3 — the sort is applied to factors 🟠

`to_factor()` runs at [R/speed.R:185](R/speed.R#L185), *before* the sort, converting every column
including `row`. `order()` on a factor follows **level order**. For an integer row column
`as.factor(1:12)` sorts numerically and all is well — but a **character** row column with ≥10 rows
gets levels `1, 10, 11, 12, 2, …`, and the sort follows that, so `byrow = TRUE` fills a permuted
grid. Demonstrated on an 11×1 design with a genuine like-pair at true rows 1-2:

```
lexical row order : 1 10 11 2 3 4 5 6 7 8 9
old grid    : A J K A C D E F G H I     score 0
correct grid: A A C D E F G H I J K     score 1
```

So adjacency *inside* `speed()` is also wrong for this input shape — the sort silently does not
deliver the invariant that `byrow = TRUE` depends on.

---

## 2. Root cause and fix

`byrow` can only encode an assumption about data ordering; it cannot read coordinates. The sort at
[R/speed.R:190](R/speed.R#L190) exists to manufacture that assumption, but it (a) only helps the one
function whose assumption it matches, (b) is defeated by lexical factor levels, and (c) does nothing
for direct calls to exported functions.

Placing values by their actual `(row, col)` coordinates makes all call paths correct and makes the
sort unnecessary for correctness.

---

## 3. Scope

**In scope**

1. Add internal `build_design_matrix()` to `R/design_utils.R`.
2. Point `objective_function_piepho()` at it — **the bug fix**.
3. Point `calculate_adjacency_score()` at it — robustness + composability.
4. New `tests/testthat/test-build_design_matrix.R`.
5. Regression tests: piepho grid orientation; adjacency direct-call and character-label cases.
6. `NEWS.md` **Bug Fixes** entry; `DESCRIPTION` version bump.

**Out of scope** (recorded so they aren't lost)

- **Removing the sort at [R/speed.R:190](R/speed.R#L190).** Once grids are coordinate-based the sort
  is no longer needed *for correctness*, but other code may rely on row order (`generate_neighbour`,
  `random_initialise`, `print.design`, `autoplot`). Leave it; note it as a possible later
  simplification. Do **not** bundle its removal with a bug fix.
- **Duplicate `(row, col)` coordinates** — multi-site/MET designs reuse coordinates and
  `build_design_matrix()` keeps only the last written. Separate pre-existing bug; see §8.
- Performance work beyond keeping this cost-neutral (§5.1).
- The new incidence functions — they stay on `feature/incidence`.

---

## 4. Branch setup

`build_design_matrix()` was introduced on `feature/incidence` in commit `a4eff49`, mixed in with the
incidence functions, so there is no clean cherry-pick. Start from `main`; the function is ~20 lines.

```sh
git checkout main
git pull
git checkout -b bugfix/grid-orientation
```

---

## 5. Implementation

### 5.1 Add `build_design_matrix()` to `R/design_utils.R`

Append after `initialize_design_df`. Two differences from the `feature/incidence` version: the
coordinate vectors are computed **once** into locals, and there is a clear error for non-coercible
coordinates.

```r
#' Build a Spatial Design Matrix from a Data Frame
#'
#' @description
#' Places each treatment value at the grid position given by its `row_column`
#' and `col_column` coordinates, returning a character matrix of dimensions
#' `max(row)` by `max(col)`. Cells with no corresponding row in `df` are `NA`.
#' Unlike filling via `matrix(..., byrow)`, this is robust to any row ordering
#' of `df`, and to factor columns whose level order is not numeric.
#'
#' @param df A data frame with columns named by `swap`, `row_column`,
#'   `col_column`.
#' @param swap Column name of the treatment variable.
#' @param row_column Column name of the row position variable (default `"row"`).
#' @param col_column Column name of the column position variable
#'   (default `"col"`).
#'
#' @return A character matrix of dimensions `max(row)` by `max(col)`.
#'
#' @keywords internal
build_design_matrix <- function(df,
                                swap,
                                row_column = "row",
                                col_column = "col") {
  rows <- as_numeric_factor(df[[row_column]])
  cols <- as_numeric_factor(df[[col_column]])

  if (all(is.na(rows)) || all(is.na(cols))) {
    stop(
      "Cannot place the design on a grid: `", row_column, "` and `",
      col_column, "` must be numeric, or coercible to numeric.",
      call. = FALSE
    )
  }

  design_matrix <- matrix(
    NA_character_,
    nrow = max(rows, na.rm = TRUE),
    ncol = max(cols, na.rm = TRUE)
  )
  design_matrix[cbind(rows, cols)] <- as.character(df[[swap]])
  design_matrix
}
```

**On the two locals** — this matters. The obvious implementation calls `as_numeric_factor()` four
times (twice for `max()`, twice for the index). It is `as.numeric(as.character(x))`
([R/utils.R:301](R/utils.R#L301)) — two allocating passes each — and this runs once per SA iteration
(10,000 by default, per level). Hoisting keeps it at two passes, matching `main`. A bug fix should
not be a performance regression.

**On the error branch** — non-numeric coordinate labels currently give `NAs introduced by coercion`
then `invalid 'nrow' value (too large or NA)`. Both pre-existing; a clear message is cheap here.

### 5.2 `R/metrics.R` — `objective_function_piepho()` (the fix)

```r
  design_matrix <- build_design_matrix(
    design, swap,
    row_column = row_column, col_column = col_column
  )
```

Confirmed safe downstream: `matrix()` on a factor already returns a **character** matrix in R 4.6.1,
so `calculate_ed()`'s type-sensitive
`design_matrix[!(design_matrix %in% swapped_items)] <- NA`
([R/metrics.R:388](R/metrics.R#L388)) is unchanged, and `get_vertices()` calls
`as.character(design_matrix)` anyway ([R/metrics.R:470](R/metrics.R#L470)).

### 5.3 `R/calculate_adjacency_score.R`

Same replacement. This is **not** a behaviour change for `speed()` with numeric row/col columns
(verified identical on sorted data across six grid shapes). It fixes direct calls (§1.4) and the
character-label case (§1.5).

### 5.4 Formatting

```sh
air format R/
```

Note the single-line signature used on `feature/incidence`
(`build_design_matrix <- function(df, swap, row_column = "row", col_column = "col") {`) is 83
characters and violates `air.toml`'s 80-column setting — hence the wrapped signature above.

---

## 6. Tests

### 6.1 New file: `tests/testthat/test-build_design_matrix.R`

```r
test_that("treatments are placed by coordinate, not by data-frame order", {
  # Column-major input, as initialise_design_df() produces:
  #   A A A
  #   B B B
  #   C C C
  #   D D D
  df <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    swap = rep(c("A", "B", "C", "D"), times = 3)
  )
  expected <- matrix(
    rep(c("A", "B", "C", "D"), times = 3),
    nrow = 4, ncol = 3
  )
  expect_equal(build_design_matrix(df, "swap"), expected)
})

test_that("result is invariant to row ordering of the data frame", {
  df <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    swap = rep(c("A", "B", "C", "D"), times = 3)
  )
  withr::with_seed(9, shuffled <- df[sample(nrow(df)), ])
  expect_equal(
    build_design_matrix(shuffled, "swap"),
    build_design_matrix(df, "swap")
  )
})

test_that("factor columns with non-numeric level order are placed correctly", {
  # as.factor() on character rows gives levels 1, 10, 11, 2, ... Coordinates
  # must be read numerically, not by level order.
  df <- data.frame(
    row = factor(as.character(1:11)),
    col = 1,
    swap = c("A", "A", "C", "D", "E", "F", "G", "H", "I", "J", "K")
  )
  m <- build_design_matrix(df, "swap")
  expect_equal(dim(m), c(11L, 1L))
  expect_equal(as.vector(m), c("A", "A", "C", "D", "E", "F", "G", "H", "I", "J", "K"))
})

test_that("missing grid positions are NA", {
  # rows 1, 2, 4, 5 present; row 3 absent
  df <- data.frame(
    row = c(1, 2, 4, 5),
    col = c(1, 1, 1, 1),
    swap = c("A", "A", "B", "B")
  )
  m <- build_design_matrix(df, "swap")
  expect_equal(dim(m), c(5L, 1L))
  expect_true(is.na(m[3, 1]))
  expect_equal(as.vector(m), c("A", "A", NA, "B", "B"))
})

test_that("non-numeric coordinates give an informative error", {
  df <- data.frame(row = c("R1", "R2"), col = c(1, 1), swap = c("A", "B"))
  expect_error(
    suppressWarnings(build_design_matrix(df, "swap")),
    "must be numeric"
  )
})
```

### 6.2 `tests/testthat/test-objective_functions.R` — the piepho regression

This is the test that would have caught the real bug. Requires `igraph` (a `Suggests`).

```r
test_that("objective_function_piepho scores the design's actual layout", {
  skip_if_not_installed("igraph")

  # Row-major data, exactly as speed() supplies after its internal sort.
  #   A A A
  #   B B B
  #   C C C
  # Piepho's pre-fix column-major fill produced a scrambled grid for this input.
  df <- data.frame(
    row = rep(1:3, each = 3),
    col = rep(1:3, times = 3),
    treatment = rep(c("A", "B", "C"), each = 3)
  )
  expect_equal(
    build_design_matrix(df, "treatment"),
    matrix(rep(c("A", "B", "C"), each = 3), nrow = 3, byrow = TRUE)
  )

  # Score must not depend on the row ordering of the input.
  withr::with_seed(4, shuffled <- df[sample(nrow(df)), ])
  expect_equal(
    objective_function_piepho(shuffled, "treatment", c("row", "col"))$score,
    objective_function_piepho(df, "treatment", c("row", "col"))$score
  )
})
```

### 6.3 `tests/testthat/test-calculate_adjacency_score.R` — composability

```r
test_that("adjacency score is correct for column-major input", {
  # initialise_design_df() is column-major; the pre-fix byrow = TRUE fill
  # scored this 0 instead of 8.
  #   A A A     horizontal like-pairs: 2 per row x 4 rows = 8
  #   B B B     vertical like-pairs:   0
  #   C C C
  #   D D D
  design <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    swap = rep(c("A", "B", "C", "D"), times = 3)
  )
  expect_equal(calculate_adjacency_score(design, "swap"), 8)
})

test_that("adjacency score composes with initialise_design_df()", {
  # 12 x 5, 4 treatments cycling down each column of 12: because 12 is a
  # multiple of 4 every column is identical, so every row is constant.
  # horizontal like-pairs: 4 per row x 12 rows = 48; vertical: 0.
  df <- initialise_design_df(
    items = rep(paste0("T", 1:4), length.out = 60),
    nrows = 12, ncols = 5
  )
  expect_equal(calculate_adjacency_score(df, "treatment"), 48)
})

test_that("adjacency score does not depend on data-frame row order", {
  design <- data.frame(
    row = rep(1:4, times = 3),
    col = rep(1:3, each = 4),
    swap = rep(c("A", "B", "C", "D"), times = 3)
  )
  withr::with_seed(9, shuffled <- design[sample(nrow(design)), ])
  expect_equal(
    calculate_adjacency_score(shuffled, "swap"),
    calculate_adjacency_score(design, "swap")
  )
})

test_that("adjacency score reads factor coordinates numerically", {
  # Lexical levels 1, 10, 11, 2, ... must not permute the grid. True rows 1-2
  # hold the only like-pair, so the score is 1; the pre-fix fill gave 0.
  design <- data.frame(
    row = factor(as.character(1:11)),
    col = 1,
    swap = c("A", "A", "C", "D", "E", "F", "G", "H", "I", "J", "K")
  )
  expect_equal(calculate_adjacency_score(design, "swap"), 1)
})
```

### 6.4 Existing tests need no changes

Confirmed: the four expectations in
[test-calculate_adjacency_score.R:1-28](tests/testthat/test-calculate_adjacency_score.R#L1-L28)
(6, 2, 7, 0) still hold — they use 3×3 designs with row-major data. The equivalence assertion at
[test-objective_functions.R:68](tests/testthat/test-objective_functions.R#L68) also still holds (both
sides give 6). **No test churn**, which keeps the diff honest and easy to review.

Worth noting for the PR description: the suite's blind spot was that every adjacency case used
row-major 3×3 data — the one shape and ordering for which both old implementations agreed with the
truth.

---

## 7. NEWS and version

`main` is at `0.0.8`. Bump `DESCRIPTION` to `0.0.9` and add to the top of `NEWS.md`:

```markdown
# speed 0.0.9

## Bug Fixes

- `objective_function_piepho()` built its design grid by filling column-wise from the data frame,
  but `speed()` supplies rows in row-major order. The grid was therefore scrambled for all design
  shapes, so designs optimised with `obj_function = objective_function_piepho` were scored against
  a layout other than their own. **Designs produced with this objective should be regenerated.**
  Designs using the default `objective_function()` were not affected. (#NNN)
- `calculate_adjacency_score()` assumed its input was already sorted in row-major order, so calling
  it directly on a data frame in any other order — including the column-major output of
  `initialise_design_df()` — returned an incorrect score. It now places treatments by their `row`
  and `col` coordinates and is independent of row ordering. (#NNN)
- Grid construction now reads `row` and `col` numerically rather than by factor level order, fixing
  designs with character row or column labels and ten or more rows, where lexical level ordering
  (`1, 10, 11, 2, ...`) permuted the grid. (#NNN)
```

Replace `#NNN` with the issue/PR number per `CLAUDE.md`.

**Version conflict:** `feature/incidence` already bumped to `0.0.9`. Whichever merges second needs
`0.0.10` — expect a trivial `DESCRIPTION`/`NEWS.md` conflict.

---

## 8. Follow-up: duplicate coordinates (separate issue)

Worth filing before this branch closes.

`build_design_matrix()` silently keeps only the last value written to a repeated `(row, col)`.
Multi-site designs reuse coordinates — the MET example in `?speed`
([R/speed.R:118-122](R/speed.R#L118-L122)) `rbind`s five copies of a 28×5 site grid. Verified on a
two-site reduction: 12 data rows collapse into 6 cells and only the last site survives. `main` is
also wrong here, just differently (it truncates to the first `nrow × ncol` values).

That example sets `adj_weight = 0`, and `objective_function()`'s `ifelse` leaves the score
unevaluated, so the documented example doesn't hit it. Any multi-site design with adjacency enabled
does.

**Where the check belongs:** *not* in `build_design_matrix()` — anything that warns or errors there
fires once per SA iteration. It belongs in `speed()`'s up-front validation as a `.verify_*` helper in
`R/verify_utils.R`, where it runs once and can name the offending column and suggest `grid_factors`
or a site-specific coordinate column.

---

## 9. Acceptance criteria

```sh
devcontainer-exec-here R -e "devtools::document()"
devcontainer-exec-here R -e "devtools::test()"
devcontainer-exec-here R -e "devtools::check()"
```

- [ ] `build_design_matrix()` added to `R/design_utils.R`; `man/` regenerated with
      `devtools::document()` (never hand-edit `man/*.Rd`).
- [ ] `objective_function_piepho()` and `calculate_adjacency_score()` both use it.
- [ ] New `test-build_design_matrix.R` passes; new piepho and adjacency cases pass.
- [ ] All pre-existing tests pass **unmodified**.
- [ ] `devtools::check()` clean.
- [ ] `air format R/` produces no further changes.
- [ ] `NEWS.md` Bug Fixes entries with issue/PR links; `DESCRIPTION` bumped.
- [ ] Confirm a `speed()` run with the **default** objective, numeric row/col, gives an unchanged
      result for a fixed seed — this fix should be a no-op on that path.
- [ ] Benchmark a representative `speed()` run against `main` to confirm no slowdown (§5.1).
- [ ] Follow-up issue filed for duplicate coordinates (§8).

`igraph` is a `Suggests` dependency needed by `calculate_ed()` for 4+ replicates; install it before
running the piepho tests or they will error (or they will skip, per `skip_if_not_installed`).

---

## 10. Reviewer summary

Suggested PR description:

> `speed()` sorts its input to row-major order before optimising, and `calculate_adjacency_score()`
> fills its grid with `byrow = TRUE` to match. But `objective_function_piepho()` fills column-major,
> so it scored a scrambled grid for every design shape — including square ones. Designs optimised
> with that objective were scored against a layout other than their own.
>
> The underlying problem is that both functions encode an assumption about data ordering instead of
> reading coordinates. This replaces both with coordinate-based placement, which also fixes direct
> calls to the exported `calculate_adjacency_score()` on column-major data (such as
> `initialise_design_df()` output, previously scoring a 4×3 design 0 instead of 8), and designs whose
> row labels are characters with ten or more rows, where lexical factor levels defeated the sort.
>
> No existing test changed — the suite's adjacency cases were all row-major 3×3, the one shape and
> ordering where both old implementations happened to be right. Adds a test file for the new helper
> plus regression cases for each affected path.
>
> The default `objective_function()` path is unaffected; piepho users should regenerate designs.
