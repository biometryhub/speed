# Add `summary()` for `design` objects (and a leaner `print()`)

## What

Adds a `summary.design()` method (with a `print.summary.design()`) that turns a
design object into a statistically meaningful evaluation, and reworks
`print.design()` into a compact output. `print()` tells you *what* a
design is; `summary()` lets you *evaluate and defend* it. Targets
`feature/summary`; bumps to 0.0.9.

## Why

The single combined score that `print()` showed isn't enough to judge a design.
Biometricians (and workshop audiences) need to see replication, how the score
breaks down, and design-quality diagnostics like connectedness and concurrence —
without dropping into the object internals by hand.

## Changes

**`print.design`** — now a uniform, compact card: layout
(`rows × cols (n plots)`), treatments with a replication summary (a distribution
like `20 (12 x 1 rep, 8 x 3 reps)` when unequal, framed for p-rep designs),
score, a compact `Iterations: run / total (stopped early)` line, seed, and a
pointer to `summary()`. The full treatment list is kept; hierarchical designs
report per level.

**`summary.design`** — returns a queryable `"summary.design"` list (printing is
formatting-only) in three sections:

- **Structure** — layout, per-level treatments/replication, spatial factors and
  their levels.
- **Optimisation** — objective used, the score **decomposed into its additive
  components** (adjacency/balance for the default objective;
  neighbour-balance/even-distribution/balance/adjacency for Piepho;
  main/interaction for factorial), initial → final, iterations and temperature
  schedule.
- **Evaluation** — connectedness, within-block concurrence, replicate spatial
  span, opt-in A-efficiency, and neighbour balance. Plus prominent **Flags**
  (didn't converge, disconnected, unequal replication) when relevant.

**Self-describing design objects** — `speed()` results now carry a `metadata`
field recording each level's swap variable, spatial factors, weights,
optimisation settings, achieved score and **score components**. This is the
prerequisite that lets `summary()` report faithfully rather than re-derive.

## Key design decisions

- **Score components come from the optimisation run, not a recompute.** Each
  built-in objective now returns a `components` vector (the additive pieces that
  sum to its score), captured into metadata with the exact arguments used. So the
  decomposition is always faithful — including non-additive objectives
  (Piepho/factorial) and ring-weighted adjacency — and custom objectives that
  don't expose components simply show the score with no decomposition.
- **Connectedness is a model-based estimability check**
  (`lm(~ spatial factors + block + treatment)`, counting aliased treatment
  contrasts), base R, no `lme4`. It adjusts for whatever the design is stratified
  by, so incomplete-block designs are assessed the way they're actually analysed.
  It is **size-guarded**: skipped for very large designs (where the dense fit is
  expensive) unless forced.
- **Concurrence is reported only for incomplete blocks.** For complete designs
  (RCBD, split-plot) every concurrence equals the replication and carries no
  information, so it is auto-skipped (overridable).
- **A-efficiency is opt-in** (`efficiency = TRUE`) — it is the heaviest metric
  and assumes a row–column model.
- **Neighbour balance auto-enables** when the design was optimised with the
  Piepho objective.
- **Buffers are excluded.** Plots added by `add_buffers()` are a field-layout
  convenience, not part of the statistical design, so `print()` and `summary()`
  silently ignore them.
- **No new dependencies** — everything is base R.

## Tests / docs

New `test-summary.R` plus additions across the objective/efficiency tests;
roxygen and runnable examples for the new methods; a new "Evaluating a Design"
vignette section; a `NEWS.md` entry. Full test suite green.
