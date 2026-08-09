# Incremental scoring in the objective functions

Split out from `test-performance-findings.md` §7. That document is about making the *test suite*
cheaper; this one is a package-level change that makes every SA iteration cheaper, so it speeds up
tests, examples **and real user runs**. It carries genuine correctness risk and warrants its own
branch.

**Status:** proposed, not started. Findings below are inherited from the measurement work on
`feature/keep-with` (commit `a4fdcd0`, R 4.6.1 / Windows 11 / 16 cores); the source-level analysis in
§2-§3 was re-verified against `chore/add-tests` (`3d4702a`).

---

## 1. Findings

### 1.1 The incremental contract already exists and is fully plumbed

`speed_hierarchical()` passes both halves of the contract into every objective call —
[R/speed.R:373-375](R/speed.R#L373-L375):

```r
new_score_obj <- opt$obj_function(new_design$design, opt$swap, spatial_cols,
                                  adj_weight = adj_weight,
                                  bal_weight = bal_weight,
                                  current_score_obj = current_score_obj,
                                  swapped_items = new_design$swapped_items,
                                  grid_index = grid_idx, ...)
```

The returned list is fed back as `current_score_obj` on the next call, and — importantly — it is
only updated when a move is **accepted** ([R/speed.R:379-382](R/speed.R#L379-L382)). So
`current_score_obj` always describes `current_design`, and `swapped_items` always describes the
single hop from `current_design` to the design being scored. The plumbing is correct for
incremental use; nothing in the loop needs to change.

### 1.2 The two default objectives ignore it; `objective_function_piepho` does not

`objective_function` and `objective_function_factorial` let both arguments fall into `...` and
recompute the entire design every iteration:

- **`objective_function`** ([R/metrics.R:41-90](R/metrics.R#L41-L90)) — full
  `calculate_adjacency_score` + `calculate_balance_score` over all plots.
- **`objective_function_factorial`** ([R/metrics.R:109-159](R/metrics.R#L109-L159)) — re-splits the
  whole treatment column with `stringi`, rebuilds `treatment_1..n` temp columns, then calls
  `objective_function` once per factor plus once for the interaction.

`objective_function_piepho` ([R/metrics.R:326-427](R/metrics.R#L326-L427)) **already accepts both**
as named formals and threads them into `calculate_ed()`, which uses `swapped_items` to mask the
design matrix ([R/metrics.R:580-581](R/metrics.R#L580-L581)):

```r
design_matrix[!(design_matrix %in% swapped_items)] <- NA
```

This is the in-repo precedent to follow — and note *how* it uses `swapped_items`: as a value-based
mask, not as positions. That distinction is the central obstacle (§2.1).

### 1.3 Measured cost

| Function | Cost per call (135-plot 3-way design) |
| --- | ---: |
| `objective_function` | 1.67 ms |
| `objective_function_factorial` | 3.23 ms |

At 14 080 iterations that is ~45 s of pure objective evaluation in a single test. Cost is
essentially linear in iterations run — fixed per-call overhead in `speed()` (validation,
`create_speed_input()`, result assembly) is negligible, so **every saving comes from running fewer
iterations or making an iteration cheaper**. This document is the second lever.

A single-swap move changes only 2 plots, so both the adjacency and balance deltas are **O(1)**
against **O(n_plots)** today.

### 1.4 Folded-in smaller optimisation

`calculate_balance_score` ([R/metrics.R:181-192](R/metrics.R#L181-L192)) uses `table()` +
`matrixStats::rowVars`. A `tabulate()`-based rewrite gives **identical results** (verified with
`all.equal`) at **1.7x** on that function, but only ~20% end-to-end since balance is roughly a third
of an objective call. Not worth a separate PR — fold it into this work, where the incremental path
needs a `tabulate()`-shaped count table anyway.

---

## 2. Obstacles

These are the reasons this is a high-risk change rather than a mechanical one. Each was confirmed
by reading the current source.

### 2.1 `swapped_items` carries treatment *values*, not plot positions

Both neighbour generators fill `swapped_items` with treatment labels, never indices —
[R/design_utils.R:81](R/design_utils.R#L81) and
[R/design_utils.R:164-165](R/design_utils.R#L164-L165):

```r
swapped_items[swapped_idx:(swapped_idx + 1)] <- to_be_swapped   # single-swap
swapped_items[swapped_idx]     <- swap_pair[1]                  # multi-swap
swapped_items[swapped_idx + 1] <- swap_pair[2]
```

The objective receives the **already-swapped** design plus the pre-swap score object. To compute a
delta it must know which *positions* changed and what they held before. With replicated treatments
the labels alone do not identify positions — treatment `"A"` appears at many plots and only one of
them moved. `calculate_ed` sidesteps this because a value-based mask is sufficient for its purpose;
adjacency and balance deltas are not so lucky.

**Consequence:** the contract needs extending to carry indices (§3.4). This is the single most
important design decision in the change, and it is additive — a new element in the list returned by
`generate_neighbour()`, ignored by any objective that does not ask for it.

### 2.2 `swap_all` moves far more than two plots

`generate_multi_swap_neighbour` swaps **every instance of two treatments** within a group, across
all groups when `swap_all_blocks` is set
([R/design_utils.R:157-162](R/design_utils.R#L157-L162)):

```r
plots_1 <- which(group_filter & new_design[[swap]] == swap_pair[1])
plots_2 <- which(group_filter & new_design[[swap]] == swap_pair[2])
new_design[[swap]][plots_1] <- swap_pair[2]
new_design[[swap]][plots_2] <- swap_pair[1]
```

So "a swap changes 2 plots" holds for the single-swap path only. Under `swap_all`, or with
`swap_count > 1`, or with `adaptive_swaps` early in the run
([R/speed.R:360-366](R/speed.R#L360-L366)), the changed set can be a large fraction of the design.
The delta path must stay correct there and should **fall back to full recomputation** when the
changed set is large enough that incremental costs more than it saves.

### 2.3 Floating-point drift changes the search trajectory

`objective_function` rounds to 10 dp on return ([R/metrics.R:84](R/metrics.R#L84)). Accumulating
deltas over thousands of iterations drifts away from the recomputed value in a way a single rounded
return does not. This is not merely cosmetic:

- acceptance is a strict comparison, `new_score < current_score`
  ([R/speed.R:379](R/speed.R#L379));
- early stopping tests `new_score < .Machine$double.eps`
  ([R/speed.R:405](R/speed.R#L405)).

A drift of one ULP can flip an acceptance, and from there the entire seeded trajectory diverges —
which shows up as mass snapshot and exact-score test failures that look far scarier than the actual
cause. Mitigation in §3.5.

### 2.4 `.optimal_score` identity-checks the objective function

[R/metrics.R:268](R/metrics.R#L268) gates the lower-bound calculation on:

```r
identical(obj_function, objective_function)
```

If the incremental version is introduced by wrapping, decorating, or otherwise replacing the
exported `objective_function`, this check silently returns `NA_real_` and the `optimal_score`
feature quietly stops working. Whatever shape the change takes, `objective_function` must remain
the same object the user passes and this identity must keep holding.

### 2.5 The contract is public API

`objective_function_signature` and the custom-objective vignette document this contract for
user-written objectives. Any extension must be **additive and optional**: existing custom
objectives that ignore the new argument must keep working unchanged, and objectives that accept
`...` must not break on a new named argument.

---

## 3. Implementation plan

Sequenced so that each phase is independently verifiable and independently revertable. Phases 1-2
are low risk and deliver real gains; phase 3 is where the risk concentrates.

### Phase 0 — Equivalence harness (do this first, it is the safety net)

Nothing else in this plan is safe without it.

1. Add an internal helper that, for a given design + swap + spatial cols, computes the score both
   ways (full recompute vs incremental) and asserts exact equality.
2. Property-style test: generate random designs (varying grid size, replication, missing plots,
   multiple grids via `by`/`grid_index`, 2-treatment designs, factorial separators), apply a random
   legal move, assert `full(new) == incremental(old, move)` — not `all.equal`, exact equality, for
   the reasons in §2.3.
3. Seeded end-to-end test: a handful of `speed()` calls with fixed seeds whose **entire result**
   (design, score, `iterations_run`, `scores` vector) must be byte-identical before and after.
   Capture these as snapshots on `main` *before* touching the objectives.
4. Benchmark script recording per-call cost for both objectives at several design sizes, so each
   phase's gain is measured rather than assumed.

### Phase 1 — Cheap wins, no contract change

Pure speedups, no incremental state, no API change. Should be a mergeable PR on its own.

- **`tabulate()` rewrite of `calculate_balance_score`** (§1.4) — 1.7x on that function, verified
  identical output.
- **Hoist the 2-treatment check.** [R/metrics.R:50](R/metrics.R#L50) runs
  `length(unique(layout_df[[swap]]))` on every call to decide whether to zero `adj_weight`. The
  treatment set is invariant across an SA run; compute once. Careful: the warning must still fire
  exactly once with the same text, so it cannot simply move behind a cache.
- **Cache the factorial split.** `objective_function_factorial` re-runs the `stringi` split of the
  whole treatment column every call ([R/metrics.R:120-136](R/metrics.R#L120-L136)). The mapping
  from treatment label to its factor components is fixed for the whole run — build it once and
  index into it.

Expected: meaningful reduction in the factorial path (the most expensive objective) with essentially
zero correctness risk.

### Phase 2 — Incremental balance score

Balance is the easier half and needs no positional information, so it can land before the contract
change.

`calculate_balance_score` is `sum` over spatial factors of `sum(rowVars(table(level, treatment)))`.
A move that takes treatment `a` from level `L1` to `L2` (per spatial factor) changes exactly two
cells of that factor's count table, in at most two rows. Row variance has a closed-form update from
the row's sum and sum-of-squares, so:

1. Carry the per-factor count table (and per-row sums / sums-of-squares) in the returned list.
2. On each call, apply the count deltas for the changed plots and recompute variance only for
   touched rows.
3. Return the updated tables alongside `score` and `components`.

Note the returned list grows — check that `summary.design()` and `print.design()`, which read
`components`, are unaffected by extra elements.

### Phase 3 — Extend the neighbour contract with positions, then incremental adjacency

This is the risky phase; do it last and alone.

1. **Extend `generate_neighbour()`** to return changed plot indices — e.g. a `swapped_index`
   element alongside the existing `swapped_items` — in both
   `generate_single_swap_neighbour` and `generate_multi_swap_neighbour`. Additive only:
   `swapped_items` keeps its current meaning and position so `calculate_ed` and any user code
   continue to work.
2. **Pass it through** the objective call at [R/speed.R:373-375](R/speed.R#L373-L375) as a new named
   argument. Objectives that do not name it absorb it via `...`.
3. **Document it** on `objective_function_signature` and in the custom-objective vignette as an
   optional input, explicitly noting that honouring it is opt-in.
4. **Incremental adjacency.** `calculate_adjacency_score`
   ([R/calculate_adjacency_score.R:258-300](R/calculate_adjacency_score.R#L258-L300)) builds a
   design matrix per grid and sums `adjacency_score_vec()` per cell, halved. Only cells within
   `max(ring_dists)` of a changed plot can change contribution, so the delta touches a bounded
   neighbourhood. Constraints to respect: per-grid decomposition (no edge crosses a grid boundary,
   so grids stay independent), `ring_dists`/`ring_weights`/`ring_type`, and the `relationship`
   matrix path.
5. **Size-based fallback** (§2.2): when the changed set exceeds a threshold fraction of the grid,
   recompute in full. Cheaper *and* it caps the blast radius of any delta bug.

### Phase 4 — Factorial

With phases 2-3 in place, `objective_function_factorial` mostly gets its speedup for free, since it
delegates to `objective_function` per factor. Remaining work is threading `current_score_obj`
through per-factor state — the returned list must carry one sub-state per factor plus the
interaction, keyed so the next call can find them.

### Phase 5 — Documentation and NEWS

- Update the custom-objective vignette to describe the extended contract and show the
  incremental pattern.
- `NEWS.md` bullet under **Minor Changes** (per `CLAUDE.md`, sections ordered Major / Minor / Bug
  Fixes): concise, 1-2 sentences.
- `devtools::document()` for the roxygen changes — never hand-edit `man/*.Rd`.

---

## 4. Verification

A change that alters scores by one ULP is indistinguishable from a change that alters them by a
lot, once the trajectory diverges. So verify at both levels:

| Level | Check | Gate |
| --- | --- | --- |
| Unit | Property tests from phase 0, per phase | Exact equality, not `all.equal` |
| Integration | Seeded `speed()` runs, full result compared | Byte-identical to pre-change snapshots |
| Suite | `devtools::test()` | Same pass count, no new snapshot diffs |
| Package | `devtools::check()` | Clean, on all CI platforms |
| Performance | Phase 0 benchmark script | Measured gain per phase, recorded |

Watch specifically for:

- tests asserting exact `iterations_run` paired with `stopped_early` (~15 in `test-speed.R`) —
  these are the first casualties of drift;
- `vdiffr` snapshots in `test-speed.R` / `test-plotting.R` / `test-buffers.R`;
- `optimal_score` behaviour, per §2.4;
- the 2-treatment warning still firing exactly once.

If the seeded end-to-end results *do* change, treat it as a bug until proven otherwise. Only accept
a trajectory change with an explicit, understood reason — and then regenerate snapshots deliberately
rather than in bulk.

---

## 5. Risks

| Risk | Likelihood | Mitigation |
| --- | --- | --- |
| Float drift flips acceptance, mass snapshot failures | High | §2.3; periodic re-anchor to a full recompute; exact-equality property tests |
| Delta wrong on `swap_all` / multi-swap paths | Medium | Size-based fallback (§3.5); property tests covering those generators |
| Delta wrong at grid boundaries or with `relationship` | Medium | Per-grid property tests; `by`/`grid_index` cases in the generator |
| Breaks user-written objectives | Low | Additive optional argument only (§2.5) |
| Silently disables `optimal_score` | Low | §2.4; assert the `identical()` check in a test |
| Returned-list growth breaks `summary`/`print` | Low | Covered by existing tests; check explicitly in phase 2 |

**Rollback:** each phase is a separate commit with its own verification, so any phase can be
reverted without unwinding the others. Phase 3 in particular should be a distinct commit from
phases 1-2, which are safe to keep.

---

## 6. Acceptance criteria

1. Seeded `speed()` runs produce identical designs and scores to pre-change `main`.
2. `devtools::check()` clean.
3. Per-call objective cost materially reduced at every benchmarked design size, with numbers
   recorded against the §1.3 baseline.
4. Public contract remains backwards compatible; custom objectives ignoring the new argument work
   unchanged.
5. `objective_function` identity preserved, `optimal_score` still derives bounds.
6. Vignette and `NEWS.md` updated.

---

## 7. Notes

- `speed` is a **pure R package** — there is no `src/`, and nothing here requires adding one. Keep
  it that way unless a phase-3 benchmark makes a compelling case, since compiled code changes the
  build and CI story on all 5 platforms.
- `test-performance-findings.md` §10 lists this as item 8, "very large, helps users too,
  **high risk — own branch**". Items 1-5 there are independent of this work and should land first.
- Neither this file nor `test-performance-findings.md` is covered by `.Rbuildignore`. If either is
  committed to the repo, add an entry — otherwise `R CMD check` flags a non-standard top-level file.
