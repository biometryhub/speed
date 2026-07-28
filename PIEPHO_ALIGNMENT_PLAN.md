# Piepho ED/NB alignment - status and remaining work

Work on `objective_function_piepho()` and its components, checked against:

- **Piepho, Michel & Williams (2018)**, *Neighbor balance and evenness of distribution of treatment
  replications in row-column designs*, Biometrical Journal 60(6), 1172-1189.
  <https://doi.org/10.1002/bimj.201800013>
- **Piepho, Williams & Michel (2021)**, *Generating row-column field experimental designs with good
  neighbour balance and even distribution of treatment replications*, J Agro Crop Sci 207, 745-753.
  <https://doi.org/10.1111/jac.12463>

Both PDFs are in the repo root.

---

## Read this first

**State:** branch `bugfix/ed`. Test suite green at **1721 passing, 0 failures**. Air clean.
`devtools::document()` run. All five published statistics from Figure 1 of the 2018 paper reproduce
exactly.

**What is left:** one small bug, two judgement calls, and a list of things deliberately *not* done. The
bulk of the work - making the ED and NB metrics match their published definitions - is finished.

**Running things on this machine** (no devcontainer here):

```sh
RS="C:/Users/a1193984/AppData/Local/Programs/R/R-4.6.1/bin/x64/Rscript.exe"
"$RS" -e 'setwd("c:/Workspace/speed"); devtools::test(reporter = "silent")'
```

Every `Rscript`-based `devtools::test()` run **deletes the vdiffr SVG snapshots** under
`tests/testthat/_snaps/initialise_design_df/`. `setwd()` does not prevent it. Restore immediately after:

```sh
git checkout -- tests/testthat/_snaps/
```

---

## The framing that matters: criteria vs mechanism

This distinction is what the remaining decisions turn on, and getting it wrong sent the first few rounds
of this work off course.

**Criteria - what you measure.** ED, NB, the direction rule, the efficiency factor. These are published,
peer-reviewed definitions. They are worth matching exactly: users citing `objective_function_piepho`
expect the numbers to mean what the papers say, they are verifiable against Figure 1, and getting them
wrong produced real pathologies (see the cyclic Latin square below). **All of the completed work is in
this category.**

**Mechanism - how you search.** Priority-ordered interchange, hard binarity rejection, restart-and-gate
on efficiency. These should *not* be copied, because `speed` is a fundamentally different algorithm:

- **Lexicographic priority ordering is incompatible with Metropolis acceptance**, which needs a scalar
  whose magnitudes carry information. Faking it with widely separated weights is numerically poor and
  lexicographic in effect anyway.
- **Hard-reject constraints shrink the connectivity of the search space.** Escaping local optima is
  simulated annealing's advantage over their deterministic interchange; rejecting moves outright discards
  it.
- **Some of their choices reflect 2018 tooling.** MSTs came from a SAS macro (Moser, 1992), so using
  spans during the search and `MST_i` only for evaluation was plausibly a cost decision. After the
  performance work below an MST costs about 1 ms, so that constraint does not apply here.

The practical upshot: **match their criteria, not their algorithm.**

---

## Done

| | |
|---|---|
| `calculate_ed()` regression | Fixed - `trt_groups` construction had been lost in merge `52f8ee4` |
| `MST_i` as the mean edge length | Fixed - was the total |
| NB counts structural zeros | Fixed - never-adjacent pairs were dropped entirely |
| NB excludes self-pairs | Fixed - reported separately as `self_adjacencies` |
| NB direction rule | `directions = "auto"`, per 2021 Section 2 |
| Published-value test oracle | `tests/testthat/test-piepho-paper.R` |
| Per-iteration performance | ~18x faster on the paper's own Example 1 |
| Housekeeping | Stale tests, docs, imports, dead code, `swapped_items` trimming |

### The two substantive metric fixes

**`MST_i` is the arithmetic mean of the tree's edge lengths, not the total** (2018 Section 3.2(b),
p. 1176). A tree over `r` replications has `r - 1` edges, so using the total made low-replication
treatments look tightly clustered - and unequal replication is the motivating case of both papers.
Verified against Figure 1: treatment 2 gives `(sqrt(5) + sqrt(17)) / 2 = 3.1796` against the published
3.17, treatment 6 gives `(sqrt(2) + sqrt(5) + sqrt(10)) / 3 = 2.2709` against 2.27. The paper truncates
rather than rounds.

**NB must tabulate every distinct pair, including pairs that never adjoin** (2018 Section 3.2(c)). The old
code used `table()`, which only creates levels for observed pairs, so the size of the variance pool
changed with the design and the optimiser could improve its score by making pairs *disappear*.

Evidence, from enumerating all 576 order-4 Latin squares (row adjacencies fixed at `k(s-1) = 12`):

| NB statistic | correlation with Piepho's published S2 score |
|---|---|
| variance over **all** `v(v-1)/2` pairs | **1.000000000000** - exactly affine |
| variance over **observed** pairs only (old behaviour) | 0.53 |

So counting the zeros makes the variance provably equivalent to the published criterion. Truncating them
left **288 of 576 designs tied at `var = 0`**, with true S2 ranging from the optimum of 6 up to 12.

The worked failure is the cyclic Latin square - fully binary, so admissible under the papers' own
constraints - in which only a handful of pairs are ever adjacent:

```
a b c d e f      old calculate_nb() -> a,b:10 a,f:10 b,c:10 c,d:10 d,e:10 e,f:10
b c d e f a      var = 0, the best score the metric could return
c d e f a b
d e f a b c      9 of 15 pairs never adjacent, and therefore invisible
e f a b c d      Piepho S2 = 270, the worst of any design tested
f a b c d e      now scores var = 6.43
```

The **global optimum of the old metric was a near-worst design for neighbour balance**, and not by
accident: equal counts among observed pairs is exactly what dropping zeros rewards.

End to end on a 6x6 with six treatments, `speed()` went from never improving on its starting score
(1.47 -> 1.47, because the broken metric already scored the degenerate start as near-optimal) to
44.57 -> 2.88, reaching a design with zero self-adjacencies and all 15 treatment pairs adjacent.

### Performance

The plan used to assert that NB was "the dominant per-iteration cost". Measurement disproved it:

```
                                   per iteration
design                        NB        ED (incremental)   NB share
2018 Example 1  (99 plots)    0.20 ms         16.95 ms          1%
2021 Example 2  (60 plots)    0.90 ms         17.90 ms          5%
2021 Example 3 (300 plots)   11.40 ms          6.85 ms         32%
2021 Example 4 (540 plots)   40.55 ms         60.90 ms         43%
```

The cost was in `calculate_ed()`, and not in its setup either (the `data.frame` plus `split()` is
0.65 ms) but in **`igraph` call overhead**: constructing a graph object costs 1-5.5 ms regardless of size,
and a treatment's replications form a 2-6 point set where that dwarfs the tree itself.

`calculate_ed()` now uses `.mst_mean_prim()` at 20 points or fewer and `.mst_mean_igraph()` above.
Measured 10-30x faster per MST for 2-10 points, crossing over around 25. One iteration on Figure 1 went
from 18.30 ms to 1.00 ms; on 2021 Example 3, from 35.25 to 10.95.

A real bug surfaced here: **`igraph::graph_from_adjacency_matrix()` reads a weight of exactly 0 as an
absent edge**, so two plots of the same item at the same position were dropped from the tree, inflating
`MST_i`. A valid single-site design cannot have coincident plots, but the `matrix()` recycling issue
recorded in `KNOWN_ISSUES.md` for buffered designs can manufacture them. Prim's is correct here, so the
small-design path is now also the safer one.

**NB was left as a full recompute.** At 1-5% of an iteration there is nothing to win, and an incremental
version would need the swapped *positions*, whereas `generate_neighbour()` returns item *labels* - so it
would mean changing that contract or diffing the design matrix each iteration. Revisit only for large-`v`
p-rep designs, and ideally alongside spans, which share the plumbing.

---

## Open: one bug

### `self_adjacencies` inherits the `directions` restriction, and should not

`calculate_nb(directions = "auto")` resolves to `"row"` for a layout with more columns than rows, and
`self_adjacencies` is then counted along rows only. Demonstrated on an optimised 3x10 design with three
treatments: `self_adjacencies` reports **0** while `calculate_adjacency_score()` finds **16** vertical
ones (column 1 is `b,a,a`; column 9 is `a,a,b`).

This is wrong on the merits, independent of the papers. The NB direction rule is about **where neighbour
effects propagate** - an agronomic fact about long thin plots, per 2018 p. 1173. Avoiding a treatment
clustering with itself is an **ED** concern that applies in both directions regardless, and the papers'
own SA1-SA6 configurations are two-dimensional (Figure 3, p. 1178).

**Fix:** count `self_adjacencies` in both directions always, independently of `directions`. Contained to
`calculate_nb()`. Note the fix changes scores for any non-square design, so the Figure 1 oracle and any
absolute-value expectations need rechecking - Figure 1 itself is binary, so its `self_adjacencies` stays
0 and that test should be unaffected.

---

## Open: two judgement calls

### 1. The `self_adj_weight` default, and its overlap with `adj_weight`

`objective_function_piepho()` gained `self_adj_weight = 1`, applied to `nb$self_adjacencies`. This term is
**an invention of this work**, not something the papers have - they exclude self-adjacent designs by
requiring binarity instead. It was added because removing self-pairs from the NB pool (correctly) left
nothing in the objective responding to a treatment sitting near itself.

Two things to weigh:

- **It overlaps with `speed`'s existing `adj_weight`.** `calculate_adjacency_score()` already penalises
  like-treatment neighbours, and has richer machinery than this term does (Manhattan or Chebyshev rings at
  configurable radii with per-ring weights). Having two mechanisms for one concern is confusing. The
  alternative is to drop `self_adj_weight`, document that adjacency is `adj_weight`'s job, and let users
  compose - though `objective_function_piepho()` does not currently call `calculate_adjacency_score()` at
  all, so composition would need wiring.
- **Is 1 the right default?** It is exactly 0 for any binary design, so it changes nothing for the designs
  the papers care about and only penalises ones they would reject. That is a defensible default, but it
  was chosen by reasoning rather than by evidence.

### 2. Spans, and reporting the efficiency factor

Both are *additions* rather than corrections, and neither is required for the metrics to be right.

**Minimum row and column spans.** The papers' search uses these for ED - "the maximum number of columns or
rows two replicates of a treatment are apart" (2018 p. 1177) - and gates acceptance on them (p. 1179).
`MST_i` is by contrast an *evaluation* statistic from Section 3.2(b).

An earlier draft of this document recommended replacing `inv_total_mst` with spans. **That now looks
wrong.** `MST_i` is what Section 3.2(b) defines as *the* ED measure; it uses all replicates where a span
looks only at the two extremes; and it is now cheap. Their preference for spans in-search was plausibly
an artefact of MST cost. If spans are added, add them for the *directional* information they carry, as an
additional component - not as a replacement.

Relevant when weighing this: 2021 p. 751 warns against pushing ED hard - *"Our objective is to not achieve
extreme examples of ED, such as the knights move Latin squares that Tedin (1931) studied, as this could
result in increased error variance bias ... we mainly aim to provide a reasonable guard against clumping,
without wanting to impose a very specific pattern."* An unbounded `1/MST` pressure runs against that,
which is an argument for a bounded or satisficing ED term however it is measured.

**Reporting the efficiency factor.** This is the one concern that survives regardless of mechanism:
optimising ED and NB can cost row-column efficiency, and nothing currently measures that. 2018 Section
3.2, closing paragraph, describes strategy 2 as directly optimising ED and NB *"while simultaneously
seeking to minimize the loss in row-column efficiency"*, and `speed` does only the first half.

`calculate_efficiency_factor()` exists and reproduces the published 0.864 for Figure 1, so surfacing it in
`print.design()` or `summary()` is cheap and would close the gap without adopting their staging. Note that
computing it requires a Moore-Penrose pseudo-inverse, so it belongs at the end of a run or once per
restart, **not** in the per-iteration objective - that would dominate the ~1 ms iteration entirely.

---

## Deliberately not doing

Recorded so these are not re-litigated.

- **Binarity as a constraint or a score.** The papers require `v >= s >= k` and reject any interchange
  making a design non-binary (2018 p. 1177). `speed` supports designs where binarity is *impossible* -
  p-rep, augmented, and anything with fewer treatments than columns. Tested on a 3x10 with three
  treatments: binarity cannot be achieved, so scoring it adds an irreducible floor with no search signal,
  whereas self-adjacency was optimisable from 27 down to 0. Self-adjacency is the better generalisation
  for `speed`'s design space.
- **The S3-S9 spatial scores** (SA2-SA6 knight moves and double diagonals, plus the column-proximity
  counts). 2018 p. 1177 says the algorithm stops tracking S3-S9 once it moves on to the spans, and that
  *"the main emphasis of design optimization by the direct method is on S1, S2, row minimum span, column
  minimum span, and f_A^RC"*. Lowest value of anything considered here. Note also that
  `calculate_adjacency_score()`'s rings cannot express them exactly - every ring mixes SA configurations
  with non-SA cells and bundles configurations the paper treats at different priorities.
- **Lexicographic priority staging.** See the mechanism argument above.

---

## Corrections to earlier reasoning

Earlier drafts of this document contained claims that turned out to be wrong. Noted so stale notes are not
trusted:

1. **"The papers optimise E subject to ED and NB, and `speed` inverts that hierarchy."** Wrong. `speed`
   implements **strategy 2** of the 2018 paper, which *"directly optimizes ED and NB"* (Section 3.2,
   p. 1176). Directly optimising ED and NB is correct. The error came from reading 2021 p. 751's "sole
   optimality criterion is the average efficiency factor E" literally; "optimality criterion" there is the
   design-theory term of art for the *statistical* criterion, and 2021 p. 746 describes the spatial
   properties as *"a set of constraints on permissible spatial configurations"*.
2. **"The papers never add their criteria together."** Wrong. 2021 p. 746: *"A spatial objective function
   is constructed to incorporate all of the NB and ED considerations. The aim is to minimize this
   function."* A combined spatial score is not foreign to the method; E is applied as a separate stage on
   top of it.
3. **"NB is the dominant per-iteration cost."** Wrong - it is 1-5% on typical designs. See Performance.
4. **"`self_adjacencies` is a weak proxy for binarity that misses non-adjacent repeats."** True as stated,
   but the implied conclusion - that it should be replaced by a true binarity score - was wrong. See
   Deliberately not doing.
5. **"Replace `inv_total_mst` with spans."** Reconsidered; see judgement call 2.
6. **The 2021 replication-weighted *e*-series was flagged as a possible correction to
   `calculate_efficiency_factor()`.** It is not - the function reproduces the published `f_A^RC` of 0.864
   exactly, and its `2/r_h` harmonic-mean baseline is right, because averaging `sigma^2 (1/r_i + 1/r_j)`
   over all pairs gives exactly `2/r_h`. No change needed.

One apparent slip in the paper itself, worth knowing on a re-read: 2018 p. 1177 says *"The score S1
addresses NB, whereas the other eight scores target the ED of a design"*, but Table 2 defines S1 as SA1
self-adjacencies and **S2** as the row-neighbour `n(n-1)/2` score, which 2021 p. 746 restates as "the NB
score". The sentence reads as S1 where S2 is meant. `nb$s2` implements Table 2's S2.

---

## Reference: the published oracle

Figure 1 of the 2018 paper (p. 1174) is a complete design published *with* its statistics, which makes it
an exact oracle. It lives in `tests/testthat/test-piepho-paper.R`. `(v, k, s) = (25, 9, 11)`; treatment 2
has three replications, all others four.

| Published statistic | Value | Reproduced by |
|---|---|---|
| `f_A^RC` | 0.864 | `calculate_efficiency_factor()` -> 0.8637 |
| `MST_2` (3 reps) | 3.17 | `calculate_ed()` -> 3.1796, only with the *mean* |
| `MST_6` (smallest among 4-rep) | 2.27 | `calculate_ed()` -> 2.2709 |
| `(n_0, n_1, n_2, n_3)` | (222, 67, 10, 1) | `calculate_nb(directions = "row")` |
| unique pair with `h = 3` | (1, 19) | same |

`n_0 + ... + n_3 = 300 = v(v-1)/2`, which is what establishes that the paper's `n_h` spans **all**
treatment pairs and **excludes** self-pairs. The transcription is cross-checked against three statements
in the paper's prose: treatment 2 is the only one replicated three times, the sole `h = 3` pair is
(1, 19), and the two diagonal self-adjacencies belong to treatments 6 and 18.

---

## Reference: where each criterion is defined

| Criterion | Location |
|---|---|
| The two design strategies | 2018 Section 3.2, closing paragraph, p. 1176 |
| Average efficiency factor `f_A^RC` | 2018 Section 3.2(a), p. 1176; 2021 Section 2, p. 746 (*g* and *e* series) |
| ED via `MST_i` | 2018 Section 3.2(b), p. 1176 |
| NB via `n_h` | 2018 Section 3.2(c), p. 1176 |
| NB counted along rows, and why | 2018 p. 1173; two-dimensional extension not pursued, p. 1185 |
| NB direction rule by design shape | 2021 Section 2, p. 746 |
| NB score `n(n-1)/2` | 2018 Table 2 (S2), p. 1178; restated 2021 p. 746 |
| Strategy 1, model-based | 2018 Section 3.3, pp. 1176-1177 |
| Strategy 2, direct / model-free | 2018 Section 3.4, pp. 1177-1179; 2021 Section 2, p. 746 |
| SA1-SA6 self-adjacency types | 2018 Table 1 and Figure 3, p. 1178 |
| Nine spatial scores S1-S9 | 2018 Table 2, p. 1178 |
| Binarity requirement | 2018 p. 1177 |
| Row and column spans | 2018 p. 1177; 2021 p. 746 |
| Priority order and acceptance gates | 2018 p. 1178 step 2, p. 1179 |
| "Main emphasis" is S1, S2, spans, `f_A^RC` | 2018 p. 1177 |
| Warning against extreme ED | 2021 p. 751 |
| The oracle design | 2018 Figure 1, p. 1174 |

---

## If picking this up cold

1. Confirm the suite is still green and the snapshots intact (`git status`).
2. Fix the `self_adjacencies` direction bug - the only outstanding defect.
3. Decide judgement call 1 (`self_adj_weight`), which is the smaller of the two and affects the public
   signature.
4. Reporting `f_A^RC` in `print.design()` / `summary()` is the highest-value optional addition, and is
   independent of everything else.
5. Spans are worth doing only if the directional information is actually wanted; they are no longer
   considered a correction.
