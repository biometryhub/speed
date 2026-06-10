# Implementation Plan: `summary.design` method for speed objects

## Goal

Add a `summary()` method for objects of class `"design"` that provides richer,
statistically meaningful design-evaluation output, while keeping `print()` as a
compact identity card. The combined-score number that `print()` currently shows
is not enough to *evaluate* a design; `summary()` should let a user (and the
workshop audience) interrogate and defend a design.

## Decisions taken (from review)

1. **Persist metadata** in the returned design object — agreed. Prerequisite for
   everything else (Phase 1).
2. **Efficiency factor is opt-in** — `summary()` does *not* compute it by default
   (it is the heaviest metric and assumes a row–column model). Enabled via an
   argument.
3. **`print.design` keeps the full treatment list** — do not truncate. Print is
   slimmed only by adding structure/convergence lines, not by removing treatments.
4. **Connectedness** — two complementary base-R paths, no igraph/lme4 dependency
   (revised after reviewing a colleague's `sommario.ed.connectedness`):
   - **Block-graph** (when a block factor exists): treatment × block incidence
     via `xtabs()`, adjacency `A <- (M %*% t(M)) > 0`, zero diagonal, single
     component via base-R BFS. Answers "are treatments linked through shared
     blocks?".
   - **Model estimability** (when row/col exist): fit `lm(dummy ~ row + col +
     treatment)` and count aliased (`NA`) coefficients — `0` ⇒ connected. Plain
     base `lm`; the response is a dummy because estimability is a property of the
     design-matrix rank, not the data. Answers "is `treatment` estimable after
     adjusting for row + col?" — the question that matters for how these designs
     are actually analysed.

   Pick the path from what factors are present (prefer the model path when row/col
   exist; fall back to / additionally report the block-graph when a block exists).
   This removes the earlier "no block factor → `NA`" gap.
5. **Concurrence vs. spatial incidence** — `calculate_pair_incidence()` and
   `calculate_position_incidence()` (from `feature/incidence`) count spatial grid
   neighbours; concurrence uses a block-membership incidence matrix
   (`xtabs(~ treatment + block)`). Different things — `summary()` computes block
   incidence independently. No changes to the incidence functions.
6. **Neighbour balance auto-detected from metadata** — `neighbour = NULL` (default)
   auto-enables NB/ED output when Phase-1 metadata records the objective as
   `objective_function_piepho`. Users can override with `TRUE`/`FALSE`.
7. **Replicate spatial span** — new metric adopted from the colleague's
   `sommario.duplicates.span.doe`: per treatment, the minimum Manhattan
   separation between its replicate plots (`min(dist(rows, "manhattan")) + 1`,
   likewise for cols), plus the worst-case (minimum) span across treatments. A
   small span flags replicates clustered together. Pure base R (`tapply` +
   `dist`). Reported in the evaluation section.
8. **Replication breakdown, not just equal/unequal** — `print()`/`summary()`
   report the replication *distribution* (e.g. counts of treatments at each rep
   level), framed for p-rep-style designs (single-rep entries vs replicated
   entries) rather than a bare "unequal reps" flag.

---

## Current state (reference)

Design object is built in `R/speed.R` (`speed_hierarchical`, ~lines 371–397) and
contains:

`design_df`, `score`, `scores`, `temperatures`, `iterations_run`,
`stopped_early`, `treatments`, `seed`.

It does **not** store the per-level `swap` column, `spatial_factors`, or
`adj_weight`/`bal_weight`. `summary()` needs these to recompute per-level metrics,
which is why Phase 1 is a prerequisite.

`print.design` lives at `R/speed.R:411`.

Reusable exported metrics already in `R/metrics.R` /
`R/calculate_adjacency_score.R`:

- `calculate_adjacency_score(layout_df, swap, row_column, col_column, ...)`
- `calculate_balance_score(layout_df, swap, spatial_cols)`
- `calculate_efficiency_factor(design_df, item)` — Piepho A-efficiency (row–col model)
- `calculate_nb(...)`, `calculate_ed(...)` — neighbour balance / even distribution

Branch on `is.list(x$treatments)` to distinguish simple vs hierarchical designs
(same pattern `print.design` already uses).

---

## Phase 1 — Make the design object self-describing (prerequisite)

**File:** `R/speed.R`, in `speed_hierarchical` where `output` is assembled
(~lines 371–397).

Add a compact `metadata` field capturing, per level:

- `swap` — the swap column name
- `spatial_factors` / resolved `spatial_cols`
- `adj_weight`, `bal_weight`
- `iterations` requested, `start_temp`, `cooling_rate`

Plus a top-level record:

- `call` — `match.call()` from `speed()` (thread through if not already available)
- `levels` — ordered level names

Notes:

- Purely **additive** to the list; do not change existing field names.
- For the simple (single-level) case still store a one-element `metadata` list so
  `summary()` has a single code path.
- **Risk:** any snapshot/structure tests that assert on the full object shape
  (e.g. `tests/testthat/_snaps/speed.md`, `str()` output in vignettes) may need
  updating. Check `str(crd_result)` usages in `vignettes/speed.qmd`.

**Acceptance:** existing tests pass (after snapshot updates); `result$metadata`
exposes per-level swap/spatial/weight info.

---

## Phase 2 — Slim/augment `print.design`

**File:** `R/speed.R:411`.

Keep the full treatment list (per decision 3). Add:

- **Layout line**: `<nrows> rows x <ncols> cols (<n> plots)` derived from `design_df`.
- **Treatment count + replication summary** (decision 8): equal reps →
  `8 (3 reps each)`; unequal → a compact distribution rather than a bare flag,
  e.g. `20 (12 x 1 rep, 8 x 3 reps)`. Full per-rep breakdown lives in
  `summary()`.
- **Iterations line**: compact `run / total`, with a `(stopped early)` note only
  when it stopped early (e.g. `2,525 / 5,000 (stopped early)` vs `5,000 / 5,000`).
- **Hint footer**: `Use summary() for design evaluation metrics.`
- **Uniform layout**: every field is `label  value` with a fixed-width label
  column; continuation / per-level lines align under the value column.

Target output (simple):

```
Optimised Experimental Design
-----------------------------
Layout:       12 rows x 4 cols (48 plots)
Treatments:   8 (3 reps each)
              V1, V2, V3, V4, V5, V6, V7, V8
Score:        1.43
Iterations:   2,525 / 5,000 (stopped early)
Seed:         42

Use summary() for design evaluation metrics.
```

Hierarchical: per-level lines under Treatments and Iterations, with the first
level on the label line and the rest indented. Treatments show `<level>: <n>
(<list>)`; per-level replication is left to `summary()` (at plot level it is
ambiguous for nested factors):

```
Layout:       6 rows x 4 cols (24 plots)
Treatments:   wp: 3 (A, B, C)
              sp: 4 (a, b, c, d)
Score:        47
Iterations:   wp: 50 / 50
              sp: 50 / 50
Seed:         42
```

**Acceptance:** print snapshot updated; full treatment list still shown.

---

## Phase 3 — `summary.design` + `print.summary.design`

**New file:** `R/summary.R`.

### Signature

```r
summary.design <- function(object,
                           efficiency    = FALSE,  # opt-in (decision 2)
                           connectedness = TRUE,   # graph-based, base-R BFS
                           concurrence   = NULL,   # NULL = auto when a block exists
                           neighbour     = NULL,   # NULL = auto from metadata (decision 6)
                           ...)
```

Returns a list of class `"summary.design"` (computed metrics), **invisibly
returnable** so `s <- summary(d); s$efficiency` works. `print.summary.design`
does formatting only.

### Sections computed

**A. Structure**
- grid dims, n plots, spatial factors present + their levels
- replication table (reps per treatment; min/mean/max; equal-rep flag)
- per-level structure (hierarchical)

**B. Optimisation**
- final score **decomposed into adjacency vs balance** components, recomputed via
  `calculate_adjacency_score` × `adj_weight` and `calculate_balance_score` ×
  `bal_weight` using the Phase-1 metadata
- per-level scores (hierarchical)
- initial → final score (`scores[1]` vs final), iterations, stopped-early, seed,
  start temp / cooling rate

**C. Evaluation metrics**
- adjacency diagnostics: count of remaining same-treatment neighbours; which
  treatments still self-touch
- replicate spatial span (decision 7): per-treatment min Manhattan separation of
  replicates (row + col), plus worst-case span across treatments; flag clustered
  replicates (Phase 4)
- A-efficiency factor — **only if `efficiency = TRUE`** (`calculate_efficiency_factor`)
- connectedness — block-graph and/or model-estimability, by available factors
  (Phase 4)
- concurrence λ — when a block factor exists (Phase 4)
- neighbour balance / even distribution — when `neighbour = TRUE`, or `NULL` and
  Phase-1 metadata records a NB-based objective (`calculate_nb` / `calculate_ed`)

**D. Flags** (rendered prominently at the top of the printed summary)
- did not converge (hit iteration cap)
- disconnected design (loud)
- unequal replication (note; expected for p-rep)

### `print.summary.design`

- Flags block first, only if relevant, otherwise omit.
- Then Structure → Provenance → Evaluation, each a titled block.
- Skipped/opt-out metrics shown as a one-line note (e.g.
  `Efficiency factor:  not computed (efficiency = TRUE to enable)`).

### Internal helpers (same file)

- `.replication_table(design_df, swap)` — rep distribution (decision 8)
- `.score_components(design_df, meta)` — returns adj/bal split per level
- `.design_connectedness(...)` — Phase 4 (block-graph + model-estimability)
- `.design_concurrence(...)` — Phase 4
- `.replicate_spans(design_df, swap, row, col)` — Phase 4 (decision 7)

### Hierarchical handling

Branch on `is.list(object$treatments)`. Per-level metrics from `metadata`;
whole-design metrics (efficiency, connectedness) on `design_df`.

---

## Phase 4 — New evaluation helpers

**File:** `R/summary.R` (or `R/design_metrics.R`).

### Connectedness (base R, no new deps)

Two complementary paths (decision 4); choose by available factors, report
whichever applies (both, when both row/col and a block exist):

**Block-graph path** (needs a block factor):
- Build the treatment × block incidence matrix with `xtabs(~ treatment + block)`.
- Two treatments are adjacent if they share a block: `A <- (M %*% t(M)) > 0`;
  zero the diagonal.
- Design is connected iff the adjacency graph is a single component (reachability
  / BFS from node 1, check all visited).

**Model-estimability path** (needs row + col):
- Fit `lm(dummy ~ row + col + treatment)` with a constant/dummy response and
  count aliased coefficients: `sum(is.na(coef(fit)))`. Zero ⇒ `treatment` is
  estimable ⇒ connected. (Adapted from the colleague's
  `sommario.ed.connectedness`; the response is irrelevant — estimability is a
  rank property of the design matrix.)

- Both paths return `list(connected = TRUE/FALSE, n_components = k, method = ...,
  message = ...)`.
- **No `lme4`/`lmer` dependency** — base `lm` suffices. (The sandbox
  `R/connectedness.R` uses lme4; we do not adopt that as a hard dependency.)
- When neither a block nor row/col is available, return `NA` with a reason.

### Replicate spatial span (base R, decision 7)

Adapted from the colleague's `sommario.duplicates.span.doe`:
- For each treatment, `min(dist(rows, method = "manhattan")) + 1` (the `+ 1`
  counts plots inclusively), and likewise for cols, via `tapply(swap, ...)`.
- Report per-treatment row/col spans plus the worst-case (minimum) span across
  treatments; a small worst-case span flags replicates clustered together.
- Guard: treatments with a single rep have no pairwise distance — return `NA` for
  those and exclude them from the minima. Needs row/col columns; degrade to `NA`
  + reason when absent.

### Concurrence

- From the same incidence matrix `M`: `C <- M %*% t(M)`; off-diagonals are the
  pairwise concurrences λ_ij, diagonal is replication.
- Summarise: range of λ, whether constant (BIBD-like), count of zero-concurrence
  pairs.
- Only when a block factor exists (`concurrence = NULL` → auto-detect).

### Efficiency (opt-in wrapper)

- Thin wrapper over `calculate_efficiency_factor`.
- **Guards:** requires `row` and `col` present and ≥ 3 treatments; on failure
  return `NA` with a reason string rather than erroring.
- Note in output that this is a **row–column** efficiency factor.

---

## Phase 5 — Docs, tests, NEWS

- **roxygen** for `summary.design` and `print.summary.design`
  (`@export`, `@method summary design`, `@method print summary.design`),
  `@return` describing the `summary.design` list, runnable `@examples`.
- `devtools::document()` to regenerate `man/*.Rd` and `NAMESPACE`.
- **Tests** (`tests/testthat/test-summary.R`):
  - simple design — all sections present, types correct
  - hierarchical design — per-level metrics
  - deliberately **disconnected** design — `connected = FALSE` flagged
  - `efficiency = TRUE` path computes a finite number; guard path returns `NA`
    cleanly when row/col missing
  - `summary()` return value is programmatically accessible (`s$...`)
  - `print.summary.design` snapshot
- **NEWS.md** — bullet at the top: new `summary()` method for design objects.
- Consider a short **vignette** section / workshop tie-in on evaluating a design
  with `summary()`.

---

## Edge cases / guards

- 2-treatment or `adj_weight = 0` designs: adjacency is degenerate — note it
  rather than reporting a misleading score split.
- Efficiency on large designs: O(t²) loops + matrix inverse; opt-in mitigates this,
  but still document the cost.
- Connectedness/concurrence need a block factor; degrade to `NA` + reason when
  absent.
- Hierarchical designs: ensure metadata exists for every level; guard against
  levels with no `spatial_factors`.

---

## Open questions / still to decide

1. ~~**Connectedness scope**~~ — **resolved** (revised): two base-R paths —
   block-graph (needs a block) and `lm` model-estimability (needs row/col). No
   igraph/lme4. Only degrades to `NA` when neither block nor row/col exists.
2. ~~**Default for `concurrence`**~~ — **resolved**: auto-on only for an
   *incomplete* block factor (some treatment absent from some block, i.e. a zero
   in the incidence matrix). Complete blocks (RCBD, split-plot, ...) make every
   concurrence equal the replication, so they are skipped with a note unless
   `concurrence = TRUE` forces them.
3. ~~**Neighbour balance / ED**~~ — **resolved**: `neighbour = NULL` auto-detects
   from Phase-1 metadata whether the NB objective was used.

---

## Suggested order of work

1. Phase 1 (metadata) — unblocks everything; update affected snapshots.
2. Phase 2 (print) — small, independent.
3. Phase 3 skeleton (`summary.design` + printer) with Structure + Provenance only.
4. Phase 4 helpers (connectedness, concurrence, efficiency wrapper), wired into Phase 3.
5. Phase 5 (docs, tests, NEWS).
