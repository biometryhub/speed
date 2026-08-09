# Split-plot convergence: why subplot allocations come out sub-optimal

**Status:** investigation complete, no package changes made. Prototype fixes were verified in a
scratch copy of `R/` only; the working tree is untouched.

Measured on branch `chore/add-tests` (`8cc860e`), R 4.6.1 / Windows 11, running against a copy of
`R/` outside the repo so `tests/testthat/_snaps/` was never touched.

The reported symptom was that split-plot designs converge quickly and cannot be nudged out of the
result, and that the sub-optimality is felt at the **subplot** level rather than the whole-plot
level. Early convergence at the whole-plot level is expected — a whole plot is a large design unit
with correspondingly few arrangements.

---

## 1. Designs used

All built with `initialise_split_design_df()`. "G" is the number of `swap_within` groups at the
subplot level, i.e. the number of whole plots.

| ID | subplot trts | whole-plot unit | block | rep_dim | plots | blocks | G |
|----|--------------|-----------------|-------|---------|-------|--------|---|
| D1 | 8 (`a`-`h`)  | 1x8, 3 trts     | 3x8   | 2x2     | 96    | 4      | 12 |
| D2 | 4 (`a`-`d`)  | 1x4, 6 trts     | 6x4   | 2x2     | 96    | 4      | 24 |
| D3 | 4 (`a`-`d`)  | 2x2, 6 trts     | 6x4   | 2x2     | 96    | 4      | 24 |
| D4 | 6 (`a`-`f`)  | 2x3, 4 trts     | 4x6   | 2x2     | 96    | 4      | 16 |

D2 scaled to G = 12/24/48/96 by varying `rep_dim` and block size, holding 4 subplot treatments per
whole plot.

---

## 2. Main finding — the exploratory budget per whole plot is fixed at roughly `400 / G`

### 2.1 Mechanism

Two facts combine badly:

1. `generate_neighbour()` perturbs **one randomly chosen `swap_within` group per iteration** —
   [R/design_utils.R:41-47](R/design_utils.R#L41-L47) for the single-swap path,
   [R/design_utils.R:98-106](R/design_utils.R#L98-L106) for `swap_all`. With
   `swap_all_blocks = FALSE` (the default) `blocks_to_swap <- sample(blocks, 1)`.
2. The temperature decays **once per iteration for the whole design** —
   [R/speed.R:392](R/speed.R#L392), `temp <- temp * optimise_params$cooling_rate`.

So the annealing schedule is a *global* resource, spent one group at a time. With the defaults
(`start_temp = 100`, `cooling_rate = 0.99`, [R/optim_params.R:39-40](R/optim_params.R#L39-L40)) the
temperature falls below a typical uphill move at around iteration 400, regardless of the requested
budget. Each group therefore receives roughly `400 / G` exploratory moves, while the space *inside*
each group grows factorially with subplots per whole plot.

Measured, subplot level:

| design | G | typical uphill delta | temp < delta at iter | moves per whole plot | arrangements each |
|--------|---|----------------------|----------------------|----------------------|-------------------|
| D2 (1x4) | 24 | 1.667 | 408 | 17.0 | 4! = 24 |
| D3 (2x2) | 24 | 1.667 | 408 | 17.0 | 4! = 24 |
| D4 (2x3) | 16 | 1.400 | 425 | 26.6 | 6! = **720** |

D4 is the clearest statement of the problem: about 27 exploratory moves to search 720 arrangements
per whole plot.

Random group selection adds a second-order penalty. Over 361 exploratory iterations across 24 whole
plots (2000 simulated runs), the median unluckiest whole plot receives **8** moves and the luckiest
**23** — a 3x spread, so some whole plots are barely touched before the temperature dies.

### 2.2 The gap grows with G

Subplot level only, 4 subplot treatments per whole plot, package defaults, 10 seeds. `bound` is
`.optimal_score()`.

| G | plots | score | bound | gap | % at optimum | iterations run |
|----|-------|-------|-------|-------|--------------|----------------|
| 12 | 48    | 2.667 | 2.667 | **0.000** | **100%** | 624 |
| 24 | 96    | 3.000 | 0.000 | 3.000 | 0% | 2 984 |
| 48 | 192   | 4.500 | 0.000 | 4.500 | 0% | 3 345 |
| 96 | 384   | 9.500 | 0.000 | 9.500 | 0% | 4 094 |

Small split-plots are fine, which is why this does not show up in the package's own examples. The
gap then grows monotonically as the fixed ~400 exploratory iterations are divided among more groups.

The same designs under a stretched schedule (`start_temp = 5`, `cooling_rate = 0.9995`, budget
10 000) — tuning cuts the gap roughly threefold at every size, but does not close it at large G,
because 10 000 is still far short of the `800 * G` that §2.4 shows is needed:

| G | gap, defaults | gap, stretched schedule | % at optimum |
|----|---------------|-------------------------|--------------|
| 12 | 0.000 | 0.000 | 100% |
| 24 | 3.000 | 0.500 | 50% |
| 48 | 4.500 | 1.000 | 20% |
| 96 | 9.500 | 3.000 | 0% |

This is the evidence for §5.2: the schedule and the budget both have to scale with G, not just the
schedule. The bound is confirmed attainable at G = 12 and G = 24; at G = 48 and G = 96 it was not
reached within any budget tested, so attainability there is unverified.

### 2.3 Extra iterations do not help

Whole-plot level, D2, identical seeds. `last_chg` is the last iteration at which the score moved
at all:

| budget | score | last_chg |
|--------|-------|----------|
| `iterations = 2 000`, `early_stop = 2 000` | 123.2 | **368** |
| `iterations = 10 000`, `early_stop = 10 000` | 123.2 | **368** |
| `iterations = 50 000`, `early_stop = 50 000` | 123.2 | **368** |

Once the temperature is below the smallest uphill delta the accept-worse branch at
[R/speed.R:379](R/speed.R#L379) can never fire again, so the run is deterministic hill-climbing in a
proven local minimum. Twenty-five times the budget changes nothing. This is the direct explanation
for "I can't seem to kick them out of that".

### 2.4 What actually closes the gap: moves per group

D2 (G = 24), varying **only** the length of the schedule, expressed as exploratory moves per whole
plot. `cooling_rate` set to `(0.01 / start_temp)^(1 / iterations)` so the schedule spans the budget:

| moves/whole plot | iterations | median | best | % at optimum |
|------------------|------------|--------|------|--------------|
| 5   | 120    | 15.500 | 7.000 | 0% |
| 15  | 360    | 3.500  | 2.667 | 0% |
| 50  | 1 200  | 2.000  | 1.000 | 0% |
| 200 | 4 800  | 1.667  | 0.000 | 20% |
| 800 | 19 200 | **0.000** | 0.000 | **80%** |
| **package default** | **2 984** | **3.000** | 2.000 | **0%** |

The default spent 2 984 iterations to deliver the quality of a 360-iteration run, because its
temperature is dead by ~400 either way. Reaching the optimum on this design needs about 800 moves
per whole plot — roughly `800 * G` iterations — **with a schedule stretched to match**. The default
cannot get there at any budget.

### 2.5 Corroboration: the subplot problem is nearly separable

An exhaustive per-whole-plot sweep (5 passes, all 24 permutations of each whole plot, 2 880
objective calls — under a quarter of the default budget) took D2 from a random start of 26.333 to
**1.000**, bound 0.000. The search is not hard; the effort is allocated badly.

---

## 3. Approaches tested and rejected

### 3.1 Per-group temperature — no gain

Each whole plot carrying its own temperature, decayed only on iterations where that whole plot is
perturbed. D2, equal total iterations, 10 seeds, all starting from `start_temp = 5` so only the
schedule *shape* differs (median score / % at optimum):

| moves/whole plot | iterations | A: fixed `cool = 0.99` | B: global, budget-spanning | C: per-group temp |
|------------------|------------|------------------------|----------------------------|-------------------|
| 15  | 360    | 6.000 / 0% | 4.167 / 0% | 4.167 / 0% |
| 50  | 1 200  | 2.000 / 0% | 2.333 / 0% | 2.000 / 10% |
| 200 | 4 800  | 2.000 / 0% | **1.000 / 30%** | 1.167 / 20% |
| 800 | 19 200 | 2.000 / 0% | **0.000 / 70%** | **0.000 / 70%** |

B and C track each other at every budget and are identical at the largest. The intuition that a
per-group temperature is needed to guarantee each whole plot a monotonic hot-to-cold traverse does
not survive measurement: a budget-spanning *global* schedule already gives each whole plot its ~k
moves sampled uniformly across the full temperature curve. Annealing needs enough moves at each
temperature *scale*, not moves in monotonic order.

This matters because per-group temperature is the expensive option — `generate_neighbour()` would
have to report its chosen group, a temperature vector would have to be threaded through the loop,
and new per-group state would appear in the result object. Column B gets the same result from a
one-line change.

Column A is the control: at fixed `cooling_rate = 0.99` the score is pinned at 2.000 / 0% from
k = 50 to k = 800. Sixteen times the budget, no improvement.

### 3.2 `swap_all_blocks = TRUE` — actively harmful at the subplot level

The obvious lever for "touch every group each iteration". D2, subplot level, 10 seeds: median
**21.500** against **2.000** for the default. Every proposal becomes a simultaneous change across
all 24 whole plots, so the acceptance test is all-or-nothing on a very large delta.

### 3.3 Changing `cooling_rate` alone — harmful

D2 whole-plot level, default `early_stop_iterations = 2000`, 10 seeds:

| variant | median | min | max |
|---------|--------|-----|-----|
| default (`T = 100`, `cool = 0.99`) | **123.2** | 123.2 | 123.2 |
| `cooling_rate = 0.999`  | 130.6 | 123.2 | 152.8 |
| `cooling_rate = 0.9995` | 132.6 | 123.2 | 145.4 |

At `cooling_rate = 0.999` the temperature is still 13.5 at iteration 2 000, so early stopping cuts
the run off while it is still wandering and it never gets to exploit. The schedule and the budget
have to move together — which is the argument for deriving one from the other rather than exposing
them as independent knobs.

### 3.4 Not a general issue: row balance at the subplot level

Worth recording because it is easy to over-generalise. When whole plots are **1xk strips**,
permuting subplot treatments within a whole plot cannot move a treatment to a different row, so the
`row` term of the default `spatial_factors = ~ row + col` is invariant. This is **specific to
single-row whole plots** — with rectangular whole plots both terms are live. Range over 200 random
subplot allocations:

| whole-plot geometry | row balance | col balance | adjacency |
|---------------------|-------------|-------------|-----------|
| D2, 1x4 strip | **0.000 (fixed)** | 38.000 | 25.000 |
| D3, 2x2 block | 24.000 | 28.000 | 20.000 |
| D4, 2x3 block | 15.200 | 14.800 | 16.000 |

---

## 4. Separate defects found along the way

### 4.1 The best layout of a level is discarded at the level boundary

`speed_hierarchical()` initialises `current_design` and `best_design` together
([R/speed.R:289-290](R/speed.R#L289-L290)). Within a level, `current_design` follows every
*accepted* move and `best_design` only *improving* ones
([R/speed.R:380-388](R/speed.R#L380-L388)). At the end of a level
([R/speed.R:421-424](R/speed.R#L421-L424)) `current_design` is never reset, so the next level
resumes from the previous level's **last accepted state** and immediately overwrites `best_design`
from there. A level's best arrangement survives only if that level happened to finish on it.

D1, 40 seeds, whole-plot optimum 212 (established by exhaustive enumeration of all 1 296 states):

| | reaches optimum |
|---|---|
| best score **seen** during the whole-plot level | **40/40** |
| whole-plot score of the **returned** design | **29/40** |

- Best layout discarded in **11/40 runs (28%)**, giving up 9.5 points on average.
- The returned whole-plot score equals the level's last `current` score in **40/40** runs,
  confirming the output tracks `current_design`.

Fix, verified to take the same 40 seeds to **40/40** with zero discards:

```r
    # hand the next level this level's best layout, not the last state it accepted
    current_design <- best_design
    current_score_obj <- best_score_obj

    all_scores[[level]] <- scores
```

It also removes a latent inconsistency: `best_score` is recomputed per level from `current_design`
while `best_design` still holds the previous level's result, so for part of each level the two
disagree.

**This will change hierarchical designs generally**, so `tests/testthat/_snaps/speed-hierarchical/*.svg`
will need regenerating. On a split-split-plot it changed nothing in 20/20 seeds (no regression).

### 4.2 `random_initialisation` destroys hierarchical structure

`shuffle_items()` permutes the swap column plot-by-plot within `swap_within` and ignores `swap_all`
([R/design_utils.R:588-600](R/design_utils.R#L588-L600)). On D2, `optim_params(random_initialisation = 20)`
produced a design where **24 of 24 whole plots held more than one whole-plot treatment** — no longer
a split plot. The damage is permanent: `swap_all` moves preserve whatever pattern they find.

It is also silently *rewarded*. With `random_initialisation = 50` the whole-plot level reports
**30.0** against **123.2** for every valid layout, because the corrupted design can reach scores no
legitimate split plot can. Anyone comparing scores would choose it.

This is the natural "try more random starts" escape hatch, so it is worth fixing before advising
anyone to use it.

### 4.3 `plot_progress()` fails on any hierarchical design

`result$scores` is a per-level list for multi-level designs, but the function indexes it as a flat
vector ([R/plotting.R:555-560](R/plotting.R#L555-L560)):

```
Error: Problem while computing aesthetics ... object 'score' not found
```

This is why the freeze was invisible — there is no supported way to plot the convergence trace of a
split plot.

### 4.4 `.optimal_score()` is unreachable at a whole-plot level

The bound assumes a zero adjacency component ([R/metrics.R:278-280](R/metrics.R#L278-L280)), but a
whole plot is a contiguous run of identical labels, so a large part of the adjacency score is fixed
by the unit geometry and no arrangement can remove it (72 of 85 points on D2).

| design | `.optimal_score()` | attainable minimum | gap as % of score |
|--------|--------------------|--------------------|-------------------|
| D2 | 3.200 | 123.200 | 97% |
| D1 | 2.000 | 212.000 | 99% |
| 4 wp trts, 1x3 strips | 0.000 | 32.000 | 100% |

Consequences: `stop_at_optimal` can never fire at a whole-plot level, so the level always burns its
full `early_stop_iterations` in a frozen state; and `summary()` reports a target no design can hit.

### 4.5 Zero temperature crashes the loop

`start_temp = 0` passes validation (`verify_non_negative_whole`,
[R/verify_utils.R:113](R/verify_utils.R#L113)), and with the default cooling `temp` underflows to
exactly `0` at iteration 74 141. Either way, the next proposal with an *equal* score computes
`exp(0 / 0) = NaN` at [R/speed.R:379](R/speed.R#L379), `runif(1) < NaN` is `NA`, and the run dies
with `missing value where TRUE/FALSE needed`. Reachable precisely when someone raises `iterations`
to escape a local optimum.

Note also that `verify_non_negative_whole()` rejects a fractional `start_temp`, which any
auto-calibration scheme would need.

---

## 5. Recommended changes

Ordered by ratio of benefit to risk.

### 5.1 Derive `cooling_rate` from the level's iteration budget

The single change that moves subplot quality. Instead of a fixed `0.99`:

```r
cooling_rate = (T_end / start_temp)^(1 / iterations)
```

**Why it helps:** it makes the temperature reach its floor exactly at the end of the budget rather
than at iteration ~1 500 regardless. Every iteration the user pays for then does annealing work.
§2.4 shows this is the difference between 3.000 and 0.000 on D2, and §3.1 column A shows that
without it, extra budget is inert.

**Risk:** changes every design's trajectory. Needs a deprecation/versioning decision, since
`cooling_rate` is public API and users may have tuned it. Consider accepting
`cooling_rate = "auto"` as the new default while leaving a numeric value as an explicit override.

### 5.2 Scale a hierarchical level's default `iterations` by its number of `swap_within` groups

A level with 2 groups and a level with 96 groups currently both get `iterations = 10000`. Sizing as
*(moves per group) x (number of groups)* makes the default independent of design size.

**Why it helps:** §2.2 shows the gap is a pure function of G under a fixed budget. §2.4 shows the
requirement is expressed naturally in moves per group (~800 on D2), not in absolute iterations.

**Risk:** substantially longer default runtimes on large designs. Worth pairing with the
`incremental-scoring-plan.md` work, which reduces per-iteration cost. Note the two are orthogonal —
cheaper iterations of a frozen search are still frozen.

### 5.3 Fix `shuffle_items()` to respect `swap_all` (§4.2)

Permute at the unit level rather than the plot level when `swap_all` is set.

**Why it helps:** restores `random_initialisation` as a usable escape from local optima on
hierarchical designs, and stops the optimiser reporting scores from invalid designs. Self-contained
bug fix with no effect on non-hierarchical calls.

### 5.4 Carry `best_design` across the level boundary (§4.1)

Two lines. **Why it helps:** recovers a result the optimiser has already found and then throws away
in 28% of runs. Requires regenerating hierarchical snapshots.

### 5.5 Fix `plot_progress()` for hierarchical designs (§4.3)

**Why it helps:** it is the only diagnostic that would have made any of this visible. Facet by level,
or accept a `level` argument.

### 5.6 Make `.optimal_score()` account for the structural adjacency floor (§4.4)

Either add the count of like-treatment edges forced by the unit geometry, or return `NA_real_` when
the swap variable is constant within a larger unit.

**Why it helps:** re-enables `stop_at_optimal` for whole-plot levels — which by §2.3 is currently
wasting ~95% of that level's budget — and stops `summary()` reporting an unreachable target.

### 5.7 Guard the acceptance test against zero temperature (§4.5)

Treat `temp <= 0` as greedy (accept only strict improvements) instead of evaluating `exp(x / 0)`.

---

## 6. Interim guidance for users

Until 5.1/5.2 land, the schedule can be set per level — this already works, verified with the
whole-plot level at `start_temp = 250` and the subplot level at `5` in the same call:

```r
G <- length(unique(df$wholeplot))   # number of subplot-level swap_within groups
k <- 800                            # exploratory moves wanted per whole plot

speed(df, optimise = list(
  wp = list(swap = "wholeplot_treatment", swap_within = "block", swap_all = TRUE,
            iterations = 2000, early_stop_iterations = 2000),
  sp = list(swap = "subplot_treatment", swap_within = "wholeplot", swap_all = TRUE,
            iterations = k * G, early_stop_iterations = k * G,
            optimise_params = optim_params(start_temp = 5,
                                           cooling_rate = (0.01 / 5)^(1 / (k * G))))
), seed = 1)
```

Also: do not use `random_initialisation` on hierarchical designs (§4.2), and do not set
`swap_all_blocks = TRUE` on a subplot level (§3.2).

---

## 7. Notes

- Nothing in the working tree was modified. The §4.1 fix was applied and verified only in a scratch
  copy of `R/`.
- Neither this file nor the other top-level findings documents are covered by `.Rbuildignore`. If
  this is committed, add an entry — otherwise `R CMD check` flags a non-standard top-level file.
- The comparison in §3.1 used 10 seeds per cell; differences under about 0.5 in median score should
  be read as noise. The §2.2, §2.4 and §4.1 results have larger separations or 40 seeds and are
  solid.
- `.optimal_score()` identity-checks the objective function, so any change to `objective_function`
  must keep that identity intact — see `incremental-scoring-plan.md` §2.4.
