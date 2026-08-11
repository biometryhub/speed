# Piepho ED/NB alignment - status and open work

Work on `objective_function_piepho()` and its components, checked against:

- **Piepho, Michel & Williams (2018)**, *Neighbor balance and evenness of distribution of treatment
  replications in row-column designs*, Biometrical Journal 60(6), 1172-1189.
  <https://doi.org/10.1002/bimj.201800013>
- **Piepho, Williams & Michel (2021)**, *Generating row-column field experimental designs with good
  neighbour balance and even distribution of treatment replications*, J Agro Crop Sci 207, 745-753.
  <https://doi.org/10.1111/jac.12463>

Both PDFs are in the repo root. Every page citation below was checked against them on 2026-08-10.

---

## Read this first

**State:** branch `bugfix/ed`, merged with `main` at `43af2a5`. Suite green at **2086 passing, 0
failures**. `devtools::document()` run. The metric definitions match the papers, verified against the
published statistics on 2018 p. 1174.

**What changed since the last revision of this document:** the `main` merge brought `summary()`,
per-grid (MET) scoring, and `build_design_matrix()`/`grid_index()`. That resolved one of the two
judgement calls, partly resolved the other, and re-opened the performance question. Verification also
turned up **four defects that were not previously recorded**, one of which is more serious than
anything that was.

**What is left:** four defects, four decisions, and a short list of optional additions. One of the
decisions is new: the objective's global optimum turns out to be the design class the 2021 paper
explicitly says it does not want.

**Added 2026-08-10, second pass.** Review comments on the *Feat: revise piepho objective function*
issue - "`pair_mapping` could be created internally" and "pair table might not be required, using
matrix calculation instead" - were checked against the code, measured, and cross-referenced against
every open branch. Both hold. The second is the more important: a treatment x treatment count matrix
is a verified drop-in for `calculate_nb()`, is 3.1-4.8x faster, and closes defects 2 and 4
structurally rather than by patch. The branch survey also found that **the same computation exists in
up to seven places** across `main` and four branches, which is the subject of the new
new [METRICS_CONSOLIDATION.md](METRICS_CONSOLIDATION.md). See Performance for the numbers.

**Running things** - paths are per-machine, no devcontainer on either:

```sh
# machine A
RS="C:/Users/a1193984/AppData/Local/Programs/R/R-4.6.1/bin/x64/Rscript.exe"
"$RS" -e 'devtools::test("c:/Workspace/speed", reporter = "silent")'

# machine B
RS="C:/Program Files/R/R-4.6.1/bin/Rscript.exe"
"$RS" -e 'devtools::test("d:/Workspaces/speed-sam", reporter = "silent")'
```

Every `Rscript`-based `devtools::test()` run **deletes the vdiffr SVG snapshots** - all 84 of them,
across `_snaps/buffers/`, `_snaps/initialise_design_df/`, `_snaps/plotting/` and `_snaps/speed/`, not
just the one directory this note used to name. `setwd()` does not prevent it. Restore immediately
after:

```sh
git checkout -- tests/testthat/_snaps/
```

To check one file without the hazard, `devtools::load_all()` then `testthat::test_file()`.

---

## The framing that matters: criteria vs mechanism

This distinction is what the remaining decisions turn on, and getting it wrong sent the first few rounds
of this work off course.

**Criteria - what you measure.** ED, NB, the direction rule, the efficiency factor. These are published,
peer-reviewed definitions, worth matching exactly: users citing `objective_function_piepho` expect the
numbers to mean what the papers say, they are verifiable against 2018 Figure 1, and getting them wrong
produced real pathologies (see the cyclic Latin square below).

**Mechanism - how you search.** Priority-ordered interchange, hard binarity rejection, restart-and-gate
on efficiency. These should *not* be copied, because `speed` is a fundamentally different algorithm:

- **Lexicographic priority ordering is incompatible with Metropolis acceptance**, which needs a scalar
  whose magnitudes carry information. Faking it with widely separated weights is numerically poor and
  lexicographic in effect anyway.
- **Hard-reject constraints shrink the connectivity of the search space.** Escaping local optima is
  simulated annealing's advantage over their deterministic interchange; rejecting moves outright discards
  it.
- **Some of their choices reflect 2018 tooling.** MSTs came from a SAS macro (Moser, 1992), so using
  spans during the search and `MST_i` only for evaluation was plausibly a cost decision.

The practical upshot: **match their criteria, not their algorithm.**

---

## Settled - do not revert

Compressed to the rationale that stops someone undoing it. The work itself is in the git history.

| | |
|---|---|
| `MST_i` is the arithmetic **mean** edge length, not the total | 2018 §3.2(b), p. 1176 |
| NB tabulates **every** distinct pair, zeros included | 2018 §3.2(c), p. 1176 |
| NB excludes self-pairs, reported separately as `self_adjacencies` | 2018 p. 1178, Figure 3 caption |
| `directions = "auto"` follows the layout shape | 2021 §2, p. 746 - but see the caveat below |
| `.mst_mean_prim()` below the igraph threshold | performance, plus igraph mishandles zero-length edges |
| Published-value oracle | `tests/testthat/test-piepho-paper.R` |

**Why the structural zeros matter** - the one piece of evidence worth keeping in full, because the
metric looks fine without them. Enumerating all 576 order-4 Latin squares (row adjacencies fixed at
`k(s-1) = 12`, so the pair pool is constant):

| NB statistic | correlation with the paper's S2 |
|---|---|
| variance over **all** `v(v-1)/2` pairs | **exactly 1** - the affine map is `var = 0.4 * S2 - 2.4`, bit-exact |
| variance over **observed** pairs only (the old behaviour) | 0.53 |

The affine identity is algebraic, not empirical. Writing `P` for the pool size and `S = sum(nb)`:

```
S2 = ((P - 1) / 2) * var + (S^2 / P - S) / 2
```

The slope is positive, so **wherever `P` and `S` are both fixed, `var` and `S2` rank designs
identically**. That covers the whole of the source papers' domain - one full rectangular layout, one
treatment set, binary - so counting the zeros makes the variance provably equivalent to the published
criterion there. It does **not** hold unconditionally; see the caveats under decision 1.

Dropping them does not merely coarsen the metric - **it inverts the ranking.** Across the 576 squares,
the 144 *worst* designs (`S2 = 14`) score 1.33 on the old metric, strictly *better* than the 144
`S2 = 12` designs, which score 2.4. And 288 of 576 tie at `var = 0` with true `S2` spanning 6 to 12.

The worked failure is the cyclic Latin square - fully binary, so admissible under the papers' own
constraints - where only 6 of 15 pairs are ever adjacent. Counting both directions, which is what
`"auto"` resolves to for a square layout:

```
a b c d e f      old calculate_nb() -> all six observed pairs at 10
b c d e f a      var = 0, the best score the metric could return
c d e f a b
d e f a b c      9 of 15 pairs never adjacent, and therefore invisible
e f a b c d      Piepho S2 = 270, worse than any of 400 random 6x6 Latin squares (max 189)
f a b c d e      now scores var = 25.71
```

(Row-only, the same design gives `S2 = 60` and `var = 6.43`. Earlier revisions of this document mixed
the two - the `S2 = 270` and the `var = 6.43` came from different direction settings.)

The **global optimum of the old metric was a near-worst design for neighbour balance**, and not by
accident: equal counts among observed pairs is exactly what dropping zeros rewards.

**Caveat on the direction rule.** 2021 p. 746 covers non-resolvable designs with columns > rows and with
rows > columns (strict inequalities), and resolvable designs latinized by column or by both. It says
**nothing** about a square non-resolvable layout, nor about one latinized by rows only. `"auto"`
returning both directions for a square grid is therefore a `speed` choice, not a published rule, and
`calculate_nb()`'s docs should not imply otherwise.

---

## Open defects

### 1. `swapped_items` carries factor codes, so the incremental ED path is dead

**This is the highest-priority item in this document.** It predates the `main` merge - the code is
byte-identical at `267cf08` - and it is invisible to the whole test suite.

The neighbour generators preallocate `swapped_items <- character(...)`
([R/design_utils.R:50](R/design_utils.R#L50), [:111](R/design_utils.R#L111)) and then assign a **factor**
into it ([:81](R/design_utils.R#L81), [:166-167](R/design_utils.R#L166-L167)). R stores the integer
codes, not the labels. `speed()` factors the design before the loop, so:

```
raw character column      -> swapped_items = "D" "A" "A" "B"
factored frame (speed's)  -> swapped_items = "4" "1" "1" "2"
```

`calculate_ed()` then does `intersect(names(trt_groups), as.character(swapped_items))`
([R/metrics.R:735](R/metrics.R#L735)), which is **empty**, so `msts` stays frozen at its initial values
for the entire run. Measured on a 4x6, 6 treatments x 4 reps, seed 5, 300 iterations:

| labels | reported best `min(scores)` | actual score of the returned design |
|---|---|---|
| `A`..`F` | 2.641979 | 3.236952 |
| `T1`..`T6` | 2.641979 | 3.236952 |
| `11`..`16` | 2.641979 | 3.236952 |
| `1`..`6` | 3.057267 | 3.057267 |

Three symptoms, in descending order of how much they should worry us:

1. **`speed()` reports a best score no design in the run achieved** - 2.64 against a returned design
   that actually scores 3.24. `plot_progress()` and `summary()`'s "initial -> final" both read that
   trajectory.
2. **The search optimises a corrupted objective** and lands on a worse design: 3.237 against the 3.057
   the same seed reaches when the incremental path works. The ED term is frozen - only 2 distinct
   `inv_total_mst` values across 62 calls - so the run is effectively optimising NB and self-adjacency
   alone.
3. **Relabelling changes the result**, which it must not: ED and NB depend only on the partition. The
   three non-integer label sets give byte-identical results because the codes are always `1..6`.

It is invisible exactly when the labels are the integers `1..v` in level order - which every example in
both papers uses, so the Figure 1 oracle passes. `tests/testthat/test-design_utils.R:30` asserts
`all(single$swapped_items %in% letters[1:3])` and passes too, because it calls the generator on a bare
character column and never on the frame `speed()` actually builds.

**Fix:** record `as.character(to_be_swapped)` in the generators. **Add a regression test that goes
through `speed()`** (or at least `to_factor()`) with non-integer labels and asserts
`min(scores)` equals the from-scratch score of the returned design - a generator-level test cannot see
this.

### 2. In a MET, each grid builds its own pair universe, losing the structural zeros

`objective_function_piepho()` passes `pair_mapping` (default `NULL`) down to `calculate_nb()` **per
grid** ([R/metrics.R:397-401](R/metrics.R#L397-L401)), so each grid derives its pair universe from its
own treatments and `totals` is the union of those. Cross-site pairs that appear in no single grid are
therefore dropped - the exact failure mode that counting structural zeros was meant to fix.

Measured on two 3x4 sites holding treatments {1,2,3} and {4,5,6}:

| `pair_mapping` | pool | `var` | score |
|---|---|---|---|
| `NULL` (the default `speed()` path) | 6 pairs | 0 | 3.554155 |
| `create_pair_mapping(met$treatment)` | 15 pairs (9 structural zeros) | 2.314286 | 5.868441 |

`speed()` never builds a global mapping, so the default MET path is the broken one.
[`.neighbour_balance()`](R/summary.R#L1049) already gets this right - "One pair mapping for the whole
design, so a pair absent from one site counts as zero" - so the two code paths now disagree with each
other.

The same defect produces an opaque error: because the mapping is resolved per grid, a MET where one
grid holds a single treatment fails with `combn`'s bare `n < m`, with no mention of treatments, grids or
`speed`. That also fires for a genuinely single-treatment design, where the default objective runs fine.

**Fix:** build one `pair_mapping` from the whole design, and guard the fewer-than-two-items case with a
message naming the design or grid.

**Better fix, and the one to take** (added 2026-08-10). The universe should not be a *pair table* at
all. Under the count-matrix form ([METRICS_CONSOLIDATION.md](METRICS_CONSOLIDATION.md), stage 1) the
universe is the **level set** - `v`
labels rather than `v(v-1)/2` pair strings - and a treatment missing from a grid is a zero row and
column instead of a dropped pair, so the structural zeros survive by construction. Three consequences:

- **`speed()` already has it for free.** [R/speed.R:206](R/speed.R#L206) factors the data before the
  loop, so `levels(factored[[swap]])` is the universe. Nothing needs building.
- **The cost of getting this wrong today is larger than the fix.** `create_pair_mapping()` measures
  **92 ms** on the 2021 Ex4 layout (v = 450) against **~9.5 ms** for the whole NB call, and 0.65 ms
  for `sort(unique())`. The `pair_mapping = NULL` default therefore pays roughly 10x the cost of the
  work it enables, every iteration.
- **The opaque `combn` error disappears** rather than needing a guard: there is no `combn` call. The
  fewer-than-two-items case is `v < 2`, where the count matrix is 1x1 and `var` is already defined as 0.

The review comment "`pair_mapping` could be created internally" points at this defect, but "internally"
cannot mean inside `calculate_nb()` - that is precisely the scope that cannot see the whole design.
It has to come from `speed()`.

### 3. `msts` keeps phantom entries when a treatment leaves a grid

Introduced by the merge, because ED is now per grid: `msts` is seeded from `current_ed$msts`
([R/metrics.R:725](R/metrics.R#L725)), so a treatment that has moved out of a grid is in `swapped_items`
but no longer in `trt_groups`, and is never recomputed or dropped. With `by=` plus `swap_all = TRUE`,
exchanging all of treatment 2 with all of treatment 4 across two sites gave incremental **26.0** against
a fresh **24.0**. With single swaps the phantom is 0-valued and the score is unaffected, but 117 of 400
iterations returned `ed[[g]]$msts` containing treatments absent from that grid, which is what any
per-treatment reporting would read. This could not happen before the merge - a single pooled grid has an
invariant treatment set.

**Fix:** one line - `msts <- msts[names(trt_groups)]` after seeding.

### 4. `self_adjacencies` inherits the `directions` restriction

`calculate_nb(directions = "auto")` resolves to `"row"` for a layout with more columns than rows, and
`self_adjacencies` is then counted along rows only. The clearest symptom is a **self-contradiction
inside one `summary()` printout**: the Optimisation block prints `self_adjacency  0` while the
Evaluation block, eight lines later, prints `Self-adj.: 4 like-treatment adjacencies` - because
[`.neighbour_balance()`](R/summary.R#L1063) forces both directions while the objective passes
`nb_directions = "auto"`.

On a 3x10 with three treatments (25 seeds, 5000 and 20000 iterations - identical, so early stop binds):
under the default, row self-adjacencies reach 0 in 25/25 seeds while the true both-direction count only
reaches 1-12 (median 5). With `nb_directions = c("row", "col")` all 25 seeds reach 0. Zero is genuinely
attainable - `letters[((row + col) %% 3) + 1]` achieves it - so this is a search-signal defect, not
infeasibility. (An earlier revision quoted "16 vertical ones"; that figure is not reproducible and its
worked illustration accounted for only two.)

This is wrong on the merits, independent of the papers. The NB direction rule is about **where neighbour
effects propagate** - 2018 p. 1173, "plots are longish down the columns, and there is space left for
driveways between rows of plots, meaning that neighbor effects are expected in the row direction only".
Avoiding a treatment clustering with itself is an **ED** concern that applies in both directions
regardless, and the papers' own configurations are two-dimensional - 2018 Figure 3, p. 1178: *"The
figure does not highlight self-adjacencies in the same row or column because the design is assumed to be
binary."*

**Fix:** count `self_adjacencies` in both directions always, independently of `directions`. Implement it
as an `==` comparison on the two shifted matrices rather than a second `paste`/`pair_mapping` pass -
cheaper (0.55 -> 0.65 ms on 300 plots, an upper bound) and it does not depend on `paste()`-built labels,
which `create_pair_mapping()` explicitly allows to contain commas.

**The count-matrix form makes this free** (added 2026-08-10). In a v x v count matrix the **diagonal
is the self-adjacency count** and the off-diagonal is `nb`, so "diagonal counted in both directions,
off-diagonal following `directions`" is a natural split rather than a bolt-on second pass. The
measured 3.1-4.8x NB speedup in Performance is *with* both-direction self-adjacency already included -
this defect is fixed inside the speedup, not traded against it. Take this fix as part of stage 1 in
[METRICS_CONSOLIDATION.md](METRICS_CONSOLIDATION.md) rather than on its own; the blast radius below is
unchanged either way.

Contained to `calculate_nb()`'s implementation, but **not** behaviour-contained: it changes
`objective_function_piepho()`'s score for every non-square grid, hence the search trajectory and the
returned design. `.neighbour_balance()` is bit-for-bit unaffected (it already forces both directions).

**Blast radius, measured against a local patch - exactly two assertions, both in
`tests/testthat/test-calculate_nb.R`:**

1. line 61, `expect_equal(by_col$self_adjacencies, 0L)`: 0 becomes 6.
2. lines 66-72, the additive identity across directions: only valid while `self_adjacencies` is
   direction-scoped, so it must be **rewritten**, not re-pinned.

No absolute Piepho score is pinned anywhere in the suite (every assertion is relative, a component-sum
identity, or a rounding check), and no snapshot mentions the piepho components. The Figure 1 oracle is
unaffected: `self_adjacencies` is 0 in every direction because the design is row- and column-binary.

> **Do not "fix" the oracle to 2.** Figure 1 *does* have two diagonal self-adjacencies (treatments 6 and
> 18, p. 1174), so its **S1** is 2 - but S1 is Table 2's SA1 *diagonal* count, and `self_adjacencies` is
> an **orthogonal** count. They are different statistics.

---

## Decisions for you

### 1. `self_adj_weight`: keep at 1, drop it, or wire in `calculate_adjacency_score()`?

`objective_function_piepho(self_adj_weight = 1)` is **an invention of this work** - the papers exclude
self-adjacent designs by requiring binarity instead. It was added because removing self-pairs from the
NB pool (correctly) left nothing in the objective responding to a treatment sitting near itself.

**New evidence, and it makes the term load-bearing rather than cosmetic.** Both NB metrics have a
**shared degenerate global optimum that only this term separates.** On a 4x8 with 4 treatments at r = 8,
counting rows only (the 2018 convention, and what `"auto"` gives any layout wider than tall), four solid
rows - each treatment filling one row - gives `var = 0` **and** `S2 = 0`, the global minimum of both,
with 28 self-adjacencies. The maximally spread design gives `var = 15.47`, `S2 = 90`, self 0. So without
`self_adj_weight` the objective strictly prefers the worst possible design. The old justification here
("it is 0 for binary designs anyway, so it changes nothing") had it backwards: the term is not a harmless
extra, it is what stops a documented degeneracy.

Where `var` and `S2` differ is **local, near balance**, not global. Converting one distinct-pair
adjacency into a self-adjacency changes `S2` by exactly `-(n_p - 1)`, where `n_p` is the sacrificed
pair's count - so `S2` can never rise, and strictly falls once that pair had two or more adjacencies.
Restricting to starts in the lowest quartile of `var` on a 4x4 (v = 4): such a swap lowers `S2` in 75%
of cases (mean -1.1) while *raising* `var` in 77% (mean +0.61). Away from balance the contrast collapses
and can invert - on a 4x6 with v = 12, r = 2, `var` falls in 80% of self-adjacency-creating swaps against
`S2`'s 21%, i.e. **`var` is the more perverse of the two there.** So `var` is the better choice near the
optimum, but it is not immune, and the correlations are a property of the design family rather than of
the metric (`cor(self_adjacencies, S2)` ranges from -0.24 to -0.99 across families; on 500 random 4x4
v = 4 designs counting both directions it is -0.83, against -0.13 for `var`).

**Two caveats on the `var`/`S2` equivalence**, both of which bite in `speed`'s domain and not the
papers'. The identity needs `P` (pool size) and `S = sum(nb)` both fixed:

- **Empty/`NA` plots in the swap pool** make `S` vary, because adjacencies touching an `NA` are dropped.
  On a 4x6 with two movable empty plots, `S` ranged 30-34 and 6.9% of design pairs were ranked oppositely
  by the two criteria - which pick different optima.
- **`pair_mapping = NULL` with `by=`** makes `P` vary, because each grid derives its own universe: `P`
  took values 6, 11, 14 and 15 in a two-site example, with 116 strict rank inversions among 192 binary
  designs. Supplying a full mapping restores exact agreement. This is defect 2 above, and this is a
  second reason to fix it.

Also note `calculate_nb()` hard-codes `var = 0` when there is only one pair, so the NB term is
identically zero for any two-treatment design while `S2` is not.

Still to weigh:

- **It overlaps with `adj_weight`.** `calculate_adjacency_score()` already penalises like-treatment
  neighbours, with richer machinery (Manhattan or Chebyshev rings, configurable radii, per-ring weights).
- **`objective_function_piepho()` still does not call `calculate_adjacency_score()` at all**, and ring
  arguments passed to it are **silently swallowed by `...`** with no warning: a 4x6 design scores
  identically with and without `ring_dists = c(1,2), ring_weights = c(1,0.5), adj_weight = 5`. That is a
  trap regardless of which way this decision goes.
- **Composition is not free.** A user writing their own wrapper must re-expose the piepho result's `ed`
  element, because `speed()` feeds the returned list back as `current_score_obj` and
  [R/metrics.R:387](R/metrics.R#L387) reads `current_score_obj$ed[[nm]]`. Drop it and every iteration
  silently becomes a full recompute. That contract is the real argument for wiring rings in rather than
  leaving it to users.

Note defect 4 interacts: at `self_adj_weight = 1` the term goes from usually-0 (rows only) to
usually-non-zero once the direction bug is fixed, so the default starts doing real work and should be
re-argued at that point.

### 2. Is `.replicate_spans()` meant to be Piepho's span, or a clumping detector?

`main` added [`.replicate_spans()`](R/summary.R#L730), wired into `summary()` unconditionally and printed
as `Repl. span: worst-case 1 (row), 1 (col) ...`. **It is not the paper's span.**

2018 p. 1177: *"The span is defined as the maximum number of columns or rows two replicates of a
treatment are apart. For example, if a treatment appears in columns 3, 5, 6, and 8, the span is six
columns."* That is `max - min + 1`. `.replicate_spans()` computes `min(dist(x, "manhattan")) + 1` - the
*closest approach*. On the paper's own example: paper **6**, `.replicate_spans()` **2**.

The two agree only at `r = 2` (100% of the time; `r = 3`: 0.8%; `r >= 4`: never), which is why the p-rep
Examples 3-5 would not have caught it.

**There is now a second published oracle.** 2021 Figure 2 (p. 749, v = 20 dates, r = 3, 6x10) publishes
its spans, and only one definition reproduces them:

| | 2a published | 2b published | `max-min+1` | `.replicate_spans()` |
|---|---|---|---|---|
| rows | 4 | 4 | 4, 4 ✓ | 2, 2 ✗ |
| columns | 1 | 5 | 1, 5 ✓ | 1, 2 ✗ |

Its roxygen is honest ("the closest its replicates come to each other") and it is a defensible clumping
detector - arguably better than the paper's span, which ignores interior clustering. But the label
"Repl. span" collides with the paper's term. **Decide: rename it, or add the paper's span alongside it.**

Related, and probably just a bug: `.replicate_spans()` **refuses multi-grid designs** ("design spans 3
grids"), but 2021 reports spans per location for exactly that case - Example 4, p. 749: *"All locations
have a minimum span of 15 for columns and 9 for rows"*; likewise Example 5. A span within one location
is perfectly well defined, and the sibling [`.efficiency_factor()`](R/summary.R#L964) already returns a
`per_grid` list, so the plumbing exists.

### 3. In a MET, pool NB counts then take the variance, or sum per-grid variances?

The merge pools counts across grids and takes one variance
([R/metrics.R:412-427](R/metrics.R#L412-L427)), while ED is scored per grid and summed. Measured on a
two-site design: pooled-then-variance **1.686**, sum of per-grid variances **2.019**. Pooling lets one
site's imbalance offset another's; summing optimises each site independently, consistent with how ED and
the paper's spans are handled.

**The papers do not settle this.** 2021's multi-location Examples 4 and 5 report spans and efficiency
per location and never discuss cross-location NB. Note the direction rule is also resolved *per grid*
now, so sites of different shape contribute adjacencies measured on different axes to one pooled
variance.

### 4. Does the ED term need bounding or staging?

The objective's global optimum on a 5x5 is Tedin's knight's-move Latin square - exactly what 2021
p. 751 says the method should *not* produce. `speed` currently avoids it only because its single-swap
neighbourhood gets trapped, not because the objective discourages it. See the first entry under
*Findings not previously recorded* for the enumeration. Options: leave it and rely on the search (fragile
- a stronger search walks into it), cap the per-treatment ED contribution, or make ED satisficing above
some spread threshold. This is the one open question where the papers state a goal that the current score
actively contradicts.

### 5. Optional additions - which, if any?

- **Piepho's span** as a scored component (see decision 2). If added, add it for the *directional*
  information it carries, not as a replacement for `MST_i`: 2018 §3.2(b) defines `MST_i` as *the* ED
  measure, it uses all replicates where a span sees only the extremes, and it is now cheap.
- **Report `MST_i`.** 2018 §3.2(b): *"We propose to report the smallest `MST_i` for each number of
  treatment replications occurring for a design."* Nothing surfaces it - `summary()` has no MST
  statistic at all, and the design object retains no MST values. It is derivable
  (`tapply(ed$msts, reps, min)`, verified to give 3.1796 / 2.2709 on Figure 1) but not offered. This
  belongs in `summary()`'s evaluation block, **not** in `calculate_ed()`'s return value, which is on the
  hot path. (`main`'s pre-merge `calculate_ed()` had the right *shape* for this - keyed by replication
  class with `min_mst` - but the statistic inside was the total, so it never reproduced 3.17 / 2.27.)
- **`f_A^RC` in `print.design()`.** Already in `summary(efficiency = TRUE)`; `print.design()` has no
  efficiency reporting.

---

## Opportunity: a lower bound for the Piepho objective

`main` added `stop_at_optimal` and `.optimal_score()` for the default objective. The 2018 paper supplies
the equivalent closed forms for NB, in Appendix A.1 (p. 1188): a neighbour-balanced design exists only if

```
m = 2k(s - 1) / (v(v - 1))     must be a positive integer   (adjacencies per pair)
r = ks / v                     must be a positive integer   (replications per treatment)
```

So for a given layout it is decidable up front whether **complete** NB is attainable, and when it is,
`var = 0` and `S2 = m(m-1)/2 * v(v-1)/2` are exact lower bounds. When it is not, the partial-NB floor
follows from distributing `k(s-1)` adjacencies as evenly as possible over `v(v-1)/2` pairs - the same
"remainder" argument `.balance_score_min()` already uses for the balance score. That would let
`stop_at_optimal` work for `objective_function_piepho`, and would tell a user whether the score they got
is as good as their layout allows. This did not exist when the machinery was last considered.

---

## Performance

The picture **inverted** after the MST work, and the previous table in this document is stale by up to a
factor of 21. Re-measured 2026-08-10 (microbenchmark minima, 150 reps, randomised order, one fresh
R 4.6.1 process per design, igraph 2.3.3; `pair_mapping` and `grid_index` precomputed, as the SA loop
does). Minima are reproducible to ~10%; medians run 20-60% higher and are load-sensitive.

```
                                            per iteration (ms)
design                        items  pairs     NB   NB pool   ED (incr.)   objective   NB share
2018 Ex1   99 plots,  9x11       25     300   0.23     0.06    0.81 (0.56)      1.24     19% (24%)
2021 Ex2   60 plots,  6x10       20     190   0.15     0.04    0.72 (0.52)      1.04     15% (19%)
2021 Ex3  300 plots, 25x12      253  31878   2.76     5.35    5.00 (4.72)     13.70     20% (59%)
2021 Ex4  540 plots, 18x30      450 101025   8.58    19.36    8.45 (8.06)     39.00     22% (72%)

  NB          calculate_nb(), full recompute
  NB pool     the cross-grid pooling block in objective_function_piepho()
  ED (incr.)  calculate_ed() with current_ed and two swapped items; in brackets, the
              data.frame + split() setup
  objective   one objective_function_piepho() call as the SA loop makes it
  NB share    NB / objective, and in brackets (NB + NB pool) / objective
```

**`calculate_nb()` is no longer negligible.** It is 15-22% of an iteration on its own - never below 14%,
and remarkably flat across sizes - and 19-72% once the pooling it feeds is counted. `calculate_ed()`
dominates only the two small designs (65-69%). The old claim that NB was "1-5% of an iteration" so
"there is nothing to win" no longer holds, and correction #3 below is withdrawn accordingly. (A
second measurement using `Sys.time()` loops and *without* a precomputed `pair_mapping` put NB at 1.4 ms
on Figure 1 rather than 0.23 - which is itself worth knowing: calling
`objective_function_piepho()` without `pair_mapping` rebuilds it every iteration.)

Two concrete optimisations follow. Neither changes results on its own - the count-matrix rewrite below
is bit-identical to what `calculate_nb()` returns today; only bundling defect 4's fix with it moves any
score:

- ~~**A single-grid fast path for the pooling block.**~~ **Superseded 2026-08-10 - do the count-matrix
  rewrite instead.** For `length(grid_index) == 1` lines 412-427 recompute exactly what
  `calculate_nb()` already returned, materialising and de-duplicating all 101,025 pair names on Ex4,
  which is 19 of the 39 ms. A fast path would fix the single-grid case only. The count matrix makes
  pooling `Reduce("+", mats)`, which is **5.5-18.5x faster in every case including MET** and needs no
  special-casing. See the measured table below.
- **`calculate_ed()`'s setup now dominates its own call.** The `data.frame` + `split()` is 0.56 ms on
  Figure 1 but 8.06 ms at 450 items, which is 69-95% of an *incremental* call - `split()` is ~96% of
  that. Once only two treatments need recomputing, rebuilding every treatment's coordinate groups from
  scratch is the whole cost. Hoisting or caching the grouping is the obvious win. (Note this is
  currently masked by defect 1, which means nothing is recomputed at all.)

**The count-matrix rewrite, measured** (2026-08-10, host R 4.6.1, microbenchmark minima, 400 reps below
v = 100 and 50 above, names precomputed in *both* variants as the SA loop does). Implemented
standalone: factor codes via `match()`, one `tabulate()` into a `v x v` matrix, symmetrise.

```
design                     current   matrix   speedup
2018 Ex1   9x11,  v=25       0.271    0.057      4.8x
2021 Ex2   6x10,  v=20       0.159    0.052      3.1x
2021 Ex3  25x12, v=253       2.828    0.810      3.5x
2021 Ex4  18x30, v=450       9.492    2.436      3.9x     ms per calculate_nb() call

MET pooling block (3 grids), name union vs matrix addition
2018 Ex1                     0.079    0.011      6.9x
2021 Ex2                     0.053    0.010      5.5x
2021 Ex3                     7.126    0.523     13.6x
2021 Ex4                    32.523    1.759     18.5x     ms
```

`var` and `s2` need no triangle materialised. With `S` and `SS` the sums of counts and squared counts
over the upper triangle - both obtainable from `sum(M)`, `sum(M*M)` and the diagonal:

```
var = (SS - S^2 / P) / (P - 1)          s2 = (SS - S) / 2
```

**Equivalence checked, not assumed:** 0 mismatches over 300 random designs spanning v = 2-12, layouts
2x2 to 8x8 and all four `directions` settings, comparing `nb` **including its names and their order**,
plus `var`, `s2`, `max_nb`, `max_pairs` and `self_adjacencies`. `NA`/empty-plot handling matches too.

Three caveats found while measuring, all of which change how it should be written:

1. **The named `nb` vector is the cost centre, not the counting.** Rebuilding the `v(v-1)/2` pair names
   per call made the matrix version *2.3x slower* than current at v = 450 (24.5 ms against 10.6). Build
   them once, exactly as `pair_mapping` is today. The clean split is a lean internal returning counts
   plus scalars, with `calculate_nb()` a thin public wrapper that attaches names - which is what the SA
   loop wants anyway, since it reads only `var` and `self_adjacencies`.
2. **Level order.** `to_factor()` leaves an existing factor's level order alone
   ([R/utils.R:85-87](R/utils.R#L85-L87)), so `levels()` is not always `sort(unique())`. Values are
   unaffected, but `nb`'s *name order* would shift for pre-factored input. Sort the level set to keep
   it byte-identical.
3. **`create_pair_mapping()` is exported** and listed in `_pkgdown.yml`, so it needs a deprecation
   cycle rather than deletion. `calculate_nb(pair_mapping = )` can stay and derive the level set from
   the mapping it is handed.

**Incremental NB becomes possible for the first time.** A single swap touches at most 8 adjacencies, so
the count matrix updates in place: a further **2.7x** over a full matrix recompute, flat across all four
sizes. That is a floor, not a ceiling - the prototype still recomputes the variance in `O(v^2)`, and
`S`/`SS` admit `O(1)` updates too. `origin/feature/incremental-scoring` is plan-only (328 lines, no
code); this is its precondition, so sequence the two together.

**The MST kernels.** `.mst_mean_prim()` versus `.mst_mean_igraph()`, mean edge length for one
treatment's replications:

```
  points    2     3     5    10    15    20    24    25    30    40
  Prim  .0028  .015  .041  .203  .587  1.23  1.24  1.39  2.15  4.66   ms
  igraph .782  1.87  1.97  2.15  2.33  2.19  1.52  1.40  1.39  1.58   ms
  ratio   279x  126x   48x   11x  4.0x  1.8x  1.2x  1.0x  0.6x  0.3x
```

igraph's cost is **flat in point-set size** (~1 ms, measured flat from 2 to 40 points) because graph
construction dominates, and a treatment's replications are a small point set. The crossover is at
**n = 25**; the code's threshold is `n > 20` ([R/metrics.R:751](R/metrics.R#L751)), so Prim's is still
1.2-1.6x faster over n = 21-24 - a small, safe win by moving the threshold to 24.

**igraph reads a weight of exactly 0 as an absent edge.** Confirmed: on `{(1,1),(1,1),(1,4)}` it builds
2 edges instead of 3 and returns `MST_i = 3` against the true 1.5; over 955 random distance matrices
containing an exact zero it disagreed with a brute-force Kruskal MST 955/955 times, up to 3x inflation.
For an *isolated* coincident pair it does not inflate but **errors** (`REAL() can only be applied to a
'numeric'`). Prim's is correct in every one of those cases.

That said, the reason this used to be given - that buffered designs could manufacture coincident plots
via a `matrix()` recycling issue - **is wrong, and the source it cited does not say it.** Coincident
plots cannot reach `calculate_ed()` at all now: `grid_index()` rejects duplicate coordinates
([R/design_utils.R:990](R/design_utils.R#L990)) and `calculate_ed()` takes positions from
`row()`/`col()` of the matrix, so distinct cells necessarily have distinct coordinates. Verified across
3000 random designs and all five `add_buffers()` types: no within-treatment distance of 0. Keep Prim's
because it is faster and because a zero-length edge is handled correctly if it ever does arise - not
because the input is expected.

---

## Related: metrics consolidation

Defects 2 and 4 above are both closed by a single refactor - replacing the pasted-pair-string container
in `calculate_nb()` with a treatment x treatment count matrix. That work grew past the scope of this
document, because it also touches `calculate_balance_score()`, `.design_concurrence()`,
`.block_spread()`, `summary()` and `calculate_efficiency_factor()`, and it spans four other branches.

It now lives in **[METRICS_CONSOLIDATION.md](METRICS_CONSOLIDATION.md)**, including the target
architecture, the verification results, the staged path and its open decisions. Branch grouping and
merge order are in **[BRANCH_TRIAGE.md](BRANCH_TRIAGE.md)**.

The one thing worth carrying here: **stage 1 of that document is the route to defects 2 and 4 together**,
and it subsumes what those entries originally proposed - a global `pair_mapping`, a separate `==` pass
for self-adjacencies, and the single-grid pooling fast path all become unnecessary rather than done.

---

## Deliberately not doing

Recorded so these are not re-litigated.

- **Binarity as a constraint or a score.** The papers require `v >= s >= k` and reject any interchange
  making a design non-binary (2018 p. 1177). `speed` supports designs where binarity is *impossible* -
  p-rep, augmented, anything with fewer treatments than columns. On a 3x10 with three treatments the
  layout fails `v >= s >= k` outright (`3 >= 10` is false), so the papers would never generate it; by
  pigeonhole every row carries at least 7 repeats, and the row component of a binarity score measured
  **exactly 21 across all 75 optimised designs** - literally zero gradient. Signal survives only in the
  column component, which self-adjacency already drives to zero. Self-adjacency is the better
  generalisation for `speed`'s design space.
- **The S3-S9 spatial scores.** 2018 p. 1177: the algorithm stops tracking S3-S9 once it moves to the
  spans, and *"the main emphasis ... is on S1, S2, row minimum span, column minimum span, and
  `f_A^RC`"*. Lowest value of anything here. There is also an **impossibility proof** that
  `calculate_adjacency_score()`'s rings cannot express them: both ring shapes are functions of
  `(|dx|, |dy|)` symmetric in their arguments, so every ring is closed under transposition - and SA2
  `(±1,±2)` / SA5 `(±2,±1)` are exact transposes, as are SA3 `(±1,±3)` / SA6 `(±3,±1)`. No ring at any
  radius can contain one without the other, so S3 can never be separated from S7, nor S5 from S8.
  Separately, S4 and S9 are column-separation counts unbounded in row separation, so not ring-shaped at
  all. (The older framing - "every ring mixes SA configurations with non-SA cells" - is weaker and not
  quite true: under binarity the Manhattan rings' non-SA cells contribute 0, and Manhattan `d = 2` equals
  SA1 exactly.)
- **Lexicographic priority staging.** See the mechanism argument above.

---

## Findings not previously recorded

Beyond the four defects, verification turned up these:

- **The objective's global optimum is exactly the design class 2021 p. 751 warns against.** This is the
  most consequential finding here and it needs a decision. Enumerating **all 161,280 order-5 Latin
  squares** and scoring each with `objective_function_piepho`: minimum score **2.236068**, attained by
  exactly the 240 **knight's-move** squares and nothing else (median 5.098, max 20.52). Tedin's step-2
  cyclic square scores the exact global minimum; the step-1 diagonal square scores 20.52. A full-space
  anneal over any arrangement of 5x5 independently returned the same design. So the paper's warning -
  *"Our objective is to not achieve extreme examples of ED, such as the knights move Latin squares that
  Tedin (1931) studied, as this could result in increased error variance bias"* - applies directly to the
  objective as written.

  There is no ED/NB tension to soften it: on a 5x5 the knight's move is simultaneously the ED optimum and
  the only `nb$var = 0` Latin square. And **at convergence ED is the only live term** - `nb$var = 0` and
  `self_adjacencies = 0` in 17 of 17 5x5 runs, so ED is 100% of the final score (95-100% on 6x6).

  `speed()` does not currently get there - 0 of 30 seeds on a 5x5, best 2.392 against the optimal
  2.23607, and none of the converged designs is even a Latin square. But that is **search weakness, not a
  property of the objective**: at convergence 38 of 250 single swaps improve ED while 0 improve the total,
  because any ED-improving swap lifts `nb$var` off its zero floor by at least 0.100 against a typical ED
  gain of 0.085. A plain 12-restart anneal on the identical objective finds Tedin's square without
  difficulty. **Treat the current behaviour as luck, not a safeguard** - improving the search (multi-swap,
  slower cooling, more restarts) would walk into the pattern the paper rejects. Bounding or staging the ED
  term is therefore a real design question, not a hypothetical one.

  Corrections to what this document previously said, both now disproved: `1/MST` is **not** unbounded -
  on an integer grid `MST_i >= 1`, so `1/MST_i <= 1` and `inv_total_mst <= v`. And the derivative
  argument (`-1/MST^2`, so pressure decays with spread) is **not** load-bearing: on a 5x5 `MST_i` is
  confined to `[1, 3]` (max mean MST edge over all 53,130 five-cell subsets is exactly 3.0), the
  derivative varies only ~2.4x over the band real designs occupy, and acceptance is decided by finite
  jumps (median `|dED|` 0.085 per swap) rather than an infinitesimal gradient. The converse worry - one
  clumped treatment hijacking the score - is also unfounded: its contribution is capped at 1.0 and at
  ~2.1x an even share.
- **The *mean* MST edge is why ED alone is not a complete clumping guard.** A few long edges offset a
  surviving clump. The argmax of mean MST edge over all five-cell subsets of a 5x5 itself contains an
  adjacent pair at distance 1; and at the 6x6 optimum two of six treatments each retain two
  diagonally-adjacent (1.414) pairs, paid for by 3.16-3.61 edges elsewhere. This is an independent
  argument for the directional span components floated in decision 5 - unrelated to boundedness.
- **Piepho's unordered-pair convention matches `speed`'s canonicalisation.** 2018 p. 1189: Azais et al.
  distinguish left from right neighbours and require both to match, which *"obviously requires twice the
  number of plots ... compared to when direction does not matter as we have assumed throughout the
  paper"*. So collapsing `a,b` and `b,a` is correct, and the directional variant is explicitly not
  Piepho's.
- **`create_pair_mapping()` produces an `"NA,NA"` self-pair entry** when items contain `NA`, but it
  cannot leak: the NB universe comes from `attr(., "pairs")`, built via `combn(sort(items), 2)`, and
  `sort()` drops `NA`; adjacencies touching an `NA` cell are filtered before pasting. Verified clean.
  Cosmetic only.
- **`get_vertices()` / `get_edges()` are still exported but no longer used by `calculate_ed()`**, with
  two near-duplicate test files. Decide whether they stay.
- **The count-matrix form of `calculate_nb()` is a verified drop-in, not a rewrite** (2026-08-10). 0
  mismatches over 300 random designs on names, name order, `var`, `s2`, `max_nb`, `max_pairs`,
  `self_adjacencies` and `NA` handling; 3.1-4.8x faster; 5.5-18.5x on the MET pooling block. See
  Performance for the numbers and the three implementation caveats, and
  [METRICS_CONSOLIDATION.md](METRICS_CONSOLIDATION.md) for the wider
  duplication it exposes. `feature/incidence` already contains this computation
  (`calculate_pair_incidence()`, with a 114-line test file), so stage 1 is partly written.
- **`.block_spread()` and `.design_concurrence()` build the same incidence table twice**, and
  `.block_spread()`'s roxygen [says so explicitly](R/summary.R#L933-L934) without sharing it. Same for
  `calculate_balance_score()` and `.balance_score_min()`. Both are free wins under
  [METRICS_CONSOLIDATION.md](METRICS_CONSOLIDATION.md) stage 3.
- **`main` already has a concurrence matrix.** [`.design_concurrence()`](R/summary.R#L900) computes
  `M %*% t(M)` internally, so `info-objective`'s exported `calc_concurrence_matrix()` is a second
  implementation of something `main` already has - worth knowing before that branch is revisited.
- **The score formula in the docs is stale.** [R/metrics.R:290](R/metrics.R#L290) and
  `man/objective_function_piepho.Rd` both say the score is
  `nb$var + ed$inv_total_mst + self_adj_weight * nb$self_adjacencies`, but `ed` is a per-grid list now,
  so `ed$inv_total_mst` does not exist - it is a sum over grids.
- **`random_initialise()` breaks MET designs.** It calls the objective without `grid_index`, and
  `speed_hierarchical()` passes `grid_by` rather than `by`, so the objective rebuilds a single pooled
  index. `speed(..., grid_factors = list(dim1 = "row", dim2 = "col", by = "site"))` works with
  `random_initialisation = 0` and fails with `= 2`: *"Duplicate (row, col) coordinates ..."*. Not
  piepho-specific (it also hits the default objective at `adj_weight > 0`), and no test combines the
  two - but it blocks the MET + piepho combination this work cares about.

---

## Corrections to earlier reasoning

Claims in earlier revisions that turned out to be wrong, so stale notes are not trusted:

1. **"The papers optimise E subject to ED and NB, and `speed` inverts that hierarchy."** Wrong. `speed`
   implements **strategy 2** of the 2018 paper, which *"directly optimizes ED and NB, while
   simultaneously seeking to minimize the loss in row-column efficiency relative to a fully randomized
   row-column design"* (§3.2, p. 1176 - the comparator matters). The error came from reading 2021
   p. 751's "sole optimality criterion is the average efficiency factor E" literally; "optimality
   criterion" there is the design-theory term of art for the *statistical* criterion, and 2021 p. 746
   describes the spatial properties as *"a set of constraints on permissible spatial configurations"*.
2. **"The papers never add their criteria together."** Wrong. 2021 p. 746: *"A spatial objective function
   is constructed to incorporate all of the NB and ED considerations. The aim is to minimize this
   function."* E is applied as a separate stage on top of it.
3. ~~**"NB is the dominant per-iteration cost."**~~ **Withdrawn.** This was corrected to "it is 1-5%",
   and that correction is now itself wrong: after the MST work NB plus its pooling is 19-72% of an
   iteration and the largest single term on p-rep designs. See Performance.
4. **"`self_adjacencies` is a weak proxy for binarity that misses non-adjacent repeats."** True as
   stated, but the implied conclusion - replace it with a true binarity score - was wrong. See
   Deliberately not doing. (Concretely: a column reading `a,b,a` has no vertical self-adjacency but is
   non-binary.)
5. **"The `2/r_h` harmonic-mean baseline in `calculate_efficiency_factor()` is what 2021 p. 746
   defines."** Not quite. Both 2021 series normalise by `r̄_a`, the *arithmetic* mean of the replication
   numbers; the harmonic-mean baseline follows 2018 §3.2(a)'s verbal definition, and the two coincide
   only under equal replication. No code change is implied - the oracle is a 2018 figure and reproduces
   0.864 - but do not cite 2021 p. 746 as agreeing.
6. **"All five published statistics from Figure 1 reproduce exactly."** Four are in the Figure 1 caption;
   the `(1, 19)` pair comes from the prose on the same page. And "exactly" means *to the precision
   printed* - 3.1796 against 3.17, 2.2709 against 2.27; the paper truncates rather than rounds.

Two things about the papers themselves, worth knowing on a re-read:

- 2018 p. 1177 says *"The score S1 addresses NB, whereas the other eight scores target the ED of a
  design"*, but Table 2 defines S1 as SA1 (diagonal) self-adjacencies and **S2** as the row-neighbour
  `n(n-1)/2` score, which 2021 p. 746 restates as "the NB score". The sentence reads as S1 where S2 is
  meant. `nb$s2` implements Table 2's S2.
- The algorithm's step numbering on 2018 pp. 1178-1179 is broken: the main list runs 1-4, a second list
  restarts at 1, and the prose twice refers to a "step 5" that no numbered list contains.

---

## Reference: the published oracles

**2018 Figure 1 (p. 1174)** - a complete design published *with* its statistics.
`(v, k, s) = (25, 9, 11)`; treatment 2 has three replications, all others four. In
`tests/testthat/test-piepho-paper.R`.

| Published statistic | Value | Reproduced by |
|---|---|---|
| `f_A^RC` | 0.864 | `calculate_efficiency_factor()` -> 0.8637 |
| `MST_2` (3 reps) | 3.17 | `calculate_ed()` -> 3.1796, only with the *mean* |
| `MST_6` (smallest among 4-rep) | 2.27 | `calculate_ed()` -> 2.2709 |
| `(n_0, n_1, n_2, n_3)` | (222, 67, 10, 1) | `calculate_nb(directions = "row")` |
| unique pair with `h = 3` (from the p. 1174 prose) | (1, 19) | same |

`n_0 + ... + n_3 = 300 = v(v-1)/2` establishes that the paper's `n_h` spans **all** treatment pairs and
**excludes** self-pairs - and the paper does that arithmetic itself on p. 1179 (*"k(s − 1) = 90
adjacencies overall. With v(v − 1)/2 = 300 treatment pairs"*). The transcription is cross-checked
against three prose statements: treatment 2 is the only one replicated three times, the sole `h = 3`
pair is (1, 19), and there is one diagonal self-adjacency each for treatments 6 and 18.

**2021 Figure 2 (p. 749)** - a second oracle, for spans. `v = 20`, `r = 3`, 6x10. Published: 2a
minimum span 1 in columns, 4 in rows; 2b minimum span 5 in columns, 4 in rows. Reproduced only by
`max - min + 1` (see decision 2). **Not yet in the test suite** - worth adding alongside the span
decision.

---

## Reference: where each criterion is defined

Every row verified against the PDFs on 2026-08-10.

| Criterion | Location |
|---|---|
| The two design strategies | 2018 §3.2, closing paragraph, p. 1176 |
| Average efficiency factor `f_A^RC` (overbar in the paper) | 2018 §3.2(a), p. 1176; 2021 §2, p. 746 (*g* and *e* series, arithmetic-mean normalised) |
| ED via `MST_i`, and the reporting convention | 2018 §3.2(b), p. 1176 |
| NB via `n_h` | 2018 §3.2(c), p. 1176 |
| NB counted along rows, and why | 2018 p. 1173; two-dimensional extension not pursued, p. 1185 |
| NB direction rule by design shape (**silent on square non-resolvable**) | 2021 §2, p. 746 |
| NB score `n(n-1)/2` | 2018 Table 2 (S2), p. 1178; restated 2021 p. 746 |
| Strategy 1, model-based | 2018 §3.3, pp. 1176-1177 |
| Strategy 2, direct / model-free | 2018 §3.4, pp. 1177-1179; 2021 §2, p. 746 |
| SA1-SA6 self-adjacency types | 2018 Table 1 and Figure 3, p. 1178 |
| Nine spatial scores S1-S9 | 2018 Table 2, p. 1178 |
| Binarity requirement, `v >= s >= k` | 2018 p. 1177 |
| Row and column spans (inclusive count) | 2018 p. 1177; 2021 p. 746 |
| Priority order and acceptance gates | 2018 p. 1178 step 2, p. 1179 |
| "Main emphasis" is S1, S2, spans, `f_A^RC` | 2018 p. 1177 |
| Complete-NB existence conditions | 2018 Appendix A.1, p. 1188 |
| Unordered pairs; Azais et al.'s directional variant | 2018 Appendix A, p. 1189 |
| Warning against extreme ED | 2021 p. 751 |
| Multi-location p-rep, spans and E per location | 2021 pp. 749-751, Examples 4 and 5 |
| The oracles | 2018 Figure 1, p. 1174; 2021 Figure 2, p. 749 |

---

## If picking this up cold

1. Confirm the suite is green and the snapshots intact (`git status tests/testthat/_snaps/`).
2. **Fix defect 1** (`swapped_items` factor codes). It is one line in each generator plus a regression
   test that goes through `speed()`, and until it is done every non-integer-labelled run is optimising a
   frozen ED term and reporting a score no design achieved. Re-measure anything performance-related
   afterwards - defect 1 currently means the incremental path does no work.
3. **Fix defect 3** (one line).
4. **Do [METRICS_CONSOLIDATION.md](METRICS_CONSOLIDATION.md) stage 1** - the count-matrix form of `calculate_nb()`. This is now the route to
   **defects 2 and 4 together**, and it subsumes what those entries originally proposed (a global
   `pair_mapping`, a separate `==` pass for self-adjacencies, and the single-grid pooling fast path).
   It is behaviour-changing, so it lands as its own commit with NEWS entries under *Bug Fixes*. Blast
   radius is two assertions in `test-calculate_nb.R` plus one error message.
5. Then re-argue the `self_adj_weight` default (decision 1), which is coupled to defect 4: the term goes
   from usually-0 to usually-non-zero once both directions are counted.
6. **[METRICS_CONSOLIDATION.md](METRICS_CONSOLIDATION.md) stages 2-3** - collapse `.neighbour_balance()` and the piepho pooling block onto the
   same primitive, and share the incidence tables. Pure refactors; pin `summary()`'s output first. Add
   the cross-path invariant test, without which this all re-diverges.
7. Decide the span question (decision 2) - there is a published oracle for it now - and the MET NB
   pooling question (decision 3).
8. The `calculate_ed()` `split()` hoist is a free performance win once defect 1 is fixed.
9. **Before making the search stronger** (multi-swap, slower cooling, more restarts), settle decision 4.
   Fixing defect 1 alone makes the ED term live again, which moves in that direction; the current
   distance from the knight's-move optimum is search weakness, not a guard.
10. [METRICS_CONSOLIDATION.md](METRICS_CONSOLIDATION.md) stages 4-6 (the `X` primitive and the branch
    rebases) are the long tail. Both objective
    branches are additive rather than reverting (verified - see *Ordering hazards*), so the work there is
    reconciling duplicated functionality, not untangling a merge.
