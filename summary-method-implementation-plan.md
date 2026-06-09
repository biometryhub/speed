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
4. **Connectedness** — include the cheap, dependency-free graph-based check as the
   core. The optional model-based estimability path (`model = ~ ...`) is deferred /
   flagged as an open decision (see Open Questions).

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
- **Treatment count + replication summary**: e.g. `8 (3 reps each)` or
  `8 (unequal reps - see summary())`.
- **Convergence line**: collapse "Iterations Run / Stopped Early" into one human
  line — `stopped early at 2,525 / 5,000` vs `ran full 5,000 iterations`.
- **Hint footer**: `Use summary() for design evaluation metrics.`

Target output:

```
Optimised Experimental Design
------------------------------
Layout:        12 rows x 4 cols (48 plots)
Treatments:    8 (3 reps each)
               V1, V2, V3, V4, V5, V6, V7, V8
Score:         1.43
Convergence:   stopped early at 2,525 / 5,000 iterations
Seed:          42

Use summary() for design evaluation metrics.
```

Hierarchical: keep the existing per-level treatment block; add layout +
convergence lines.

**Acceptance:** print snapshot updated; full treatment list still shown.

---

## Phase 3 — `summary.design` + `print.summary.design`

**New file:** `R/summary.R`.

### Signature

```r
summary.design <- function(object,
                           efficiency   = FALSE,   # opt-in (decision 2)
                           connectedness = TRUE,    # graph-based, cheap
                           concurrence  = NULL,     # NULL = auto when a block exists
                           neighbour    = FALSE,    # NB / ED
                           model        = NULL,     # optional formula, see Open Qs
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

**B. Optimisation provenance**
- final score **decomposed into adjacency vs balance** components, recomputed via
  `calculate_adjacency_score` × `adj_weight` and `calculate_balance_score` ×
  `bal_weight` using the Phase-1 metadata
- per-level scores (hierarchical)
- initial → final score (`scores[1]` vs final), iterations, stopped-early, seed,
  start temp / cooling rate

**C. Evaluation metrics**
- adjacency diagnostics: count of remaining same-treatment neighbours; which
  treatments still self-touch
- A-efficiency factor — **only if `efficiency = TRUE`** (`calculate_efficiency_factor`)
- connectedness — graph-based (Phase 4)
- concurrence λ — when a block factor exists (Phase 4)
- neighbour balance / even distribution — only if `neighbour = TRUE`
  (`calculate_nb` / `calculate_ed`)

**D. Flags** (rendered prominently at the top of the printed summary)
- did not converge (hit iteration cap)
- disconnected design (loud)
- unequal replication (note; expected for p-rep)

### `print.summary.design`

- Flags block first.
- Then Structure → Provenance → Evaluation, each a titled block.
- Skipped/opt-out metrics shown as a one-line note (e.g.
  `Efficiency factor:  not computed (efficiency = TRUE to enable)`).

### Internal helpers (same file)

- `.replication_table(design_df, swap)`
- `.score_components(design_df, meta)` — returns adj/bal split per level
- `.design_connectedness(...)` — Phase 4
- `.design_concurrence(...)` — Phase 4

### Hierarchical handling

Branch on `is.list(object$treatments)`. Per-level metrics from `metadata`;
whole-design metrics (efficiency, connectedness) on `design_df`.

---

## Phase 4 — New evaluation helpers

**File:** `R/summary.R` (or `R/design_metrics.R`).

### Connectedness (base R, no new deps)

- Build the treatment × block incidence matrix with `xtabs(~ treatment + block)`.
- Two treatments are adjacent if they share a block: `A <- (M %*% t(M)) > 0`;
  zero the diagonal.
- Design is connected iff the adjacency graph is a single component (reachability
  / DFS from node 1, check all visited).
- Returns `list(connected = TRUE/FALSE, n_components = k, message = ...)`.
- **No `lme4`/`lmer` dependency.** (The sandbox `R/connectedness.R` uses lme4; we
  do not adopt that as a hard dependency.)
- Requires a block-type factor. When absent, return `NA` with a reason (use the
  optional model path instead — see Open Questions).

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

1. **Connectedness scope** — ship graph-based only (current plan), or also add the
   optional `model = ~ row + col + block` estimability path (base-R `lm`,
   NA/aliased-coefficient check, no lme4)? The model path is the only way to assess
   connectedness for pure row–column designs with no block factor. Leaning: add the
   `model =` hook in a follow-up once the graph-based core lands.
2. **Default for `concurrence`** — auto-on when a block exists (current plan) vs
   always opt-in.
3. **Neighbour balance / ED** — surface in summary at all, or leave as standalone
   exported functions? Currently behind `neighbour = FALSE`.

---

## Suggested order of work

1. Phase 1 (metadata) — unblocks everything; update affected snapshots.
2. Phase 2 (print) — small, independent.
3. Phase 3 skeleton (`summary.design` + printer) with Structure + Provenance only.
4. Phase 4 helpers (connectedness, concurrence, efficiency wrapper), wired into Phase 3.
5. Phase 5 (docs, tests, NEWS).
