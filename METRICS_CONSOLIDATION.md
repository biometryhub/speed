# Metrics consolidation - one implementation per primitive

Split out of [PIEPHO_ALIGNMENT_PLAN.md](PIEPHO_ALIGNMENT_PLAN.md) on 2026-08-11, where it had outgrown
its host: this is a package-wide refactor touching `calculate_balance_score()`, `.design_concurrence()`,
`.block_spread()`, `summary()` and `calculate_efficiency_factor()`, none of which are Piepho ED/NB.

**Why this exists.** The trigger was a review comment on the *Feat: revise piepho objective function*
issue - "pair table might not be required, using matrix calculation instead" - but the finding is
broader: **counting how often two treatments co-occur is implemented up to seven times across `main`
and four branches**, in incompatible containers, and nothing in the suite asserts that any two of them
agree.

**Relationship to the other documents.** Defects 2 and 4 in
[PIEPHO_ALIGNMENT_PLAN.md](PIEPHO_ALIGNMENT_PLAN.md) are closed by stage 1 below, and its *Performance*
section holds the timing baseline. Branch grouping, merge order and what can be dropped live in
[BRANCH_TRIAGE.md](BRANCH_TRIAGE.md) - not repeated here.

---

## What is duplicated

**Family A - treatment x treatment co-occurrence over some plot relation.**

| Where | Branch | Relation | Container |
|---|---|---|---|
| [`calculate_nb()`](R/metrics.R#L520) | main | rook adjacency | named vector over `v(v-1)/2` pasted pair strings |
| [piepho pooling block](R/metrics.R#L412-L427) | main | same, pooled over grids | re-derives `max`/`var`/`s2` by name union |
| [`.neighbour_balance()`](R/summary.R#L1042) | main | same, forces both directions | own per-grid loop and own pooling |
| [`.design_concurrence()`](R/summary.R#L900) | main | block co-membership | `table()` then `M %*% t(M)` |
| [`adjacency_score_vec()`](R/calculate_adjacency_score.R#L132) | main | arbitrary ring offsets, graded - the general case, **stays** | per-cell score matrix |
| `calculate_pair_incidence()` | `origin/feature/incidence` | rook adjacency | **already the `v x v` matrix form** |
| `calc_concurrence_matrix()` | `info-objective` | block co-membership | `N %*% t(N)` - duplicate of `.design_concurrence()` |

**Family B - treatment x spatial-level incidence.** [`calculate_balance_score()`](R/metrics.R#L185) and
[`.balance_score_min()`](R/metrics.R#L215) build the same `table()` separately;
[`.design_concurrence()`](R/summary.R#L904) and [`.block_spread()`](R/summary.R#L943) build the same `M`
separately - and `.block_spread()`'s own roxygen [says so](R/summary.R#L933-L934) ("It reads the same
treatment-by-block incidence `M`"). Add `calculate_position_incidence()` on `feature/incidence` and
`calc_incidence_matrix()` on `info-objective`.

**Family C - plot x treatment indicator `X`.** Built independently in
[`calculate_efficiency_factor()`](R/metrics.R#L981-L982) on `main`, in `.build_treatment_matrix()` on
`info-objective`, and again in `objective_function_a_optimality` on `origin/feature/a-optimality`.

---

## The identity that unifies them

With `X` the `n x v` plot-treatment indicator, every entry in family A is **`t(X) %*% A %*% X`** for a
different plot-by-plot relation `A`:

| `A` | Result | Consumers |
|---|---|---|
| rook adjacency | adjacency concurrence | `calculate_nb`, `self_adjacencies`, the `adjacency_score_vec` diagonal |
| block co-membership `Z Z'` | classical concurrence `N N'` | `.design_concurrence`, efficiency factors |
| projection `L` | treatment information matrix | `objective_function_info`, A/D-optimality |

Family B is the same construction one level down (`t(X) %*% Z`), which is what
`calculate_balance_score()` takes row variances of.

`t(X) %*% A %*% X` is the **identity that explains the shared shape, not the implementation.** Dense, it
is `O(n^2 v)` - about 131M flops at Ex4's size (`n = 540`, `v = 450`) - so it is fine for `summary()`
and hopeless per iteration. The implementation is a sparse edge-list `tabulate()`, and **one worker
covers every relation in the table above** (verified below). An earlier revision said the target had to
be "one primitive per family with two backends, not one function"; the prototype disproved that.

---

## The target architecture

Four layers. Only the first does any work.

```r
# Layer 1 - the single worker. Everything below feeds it.
# `item1`, `item2`: equal-length integer item codes, one entry per related plot
# pair. NA (empty plots) drop out. Returns the symmetric v x v count matrix.
count_pairs <- function(item1, item2, n_items) {
  keep <- !is.na(item1) & !is.na(item2)
  item1 <- item1[keep]
  item2 <- item2[keep]
  raw <- matrix(
    tabulate((item2 - 1L) * n_items + item1, n_items * n_items),
    n_items,
    n_items
  )
  counts <- raw + t(raw)
  diag(counts) <- diag(raw) # self-pairs counted once, not twice
  return(counts)
}

# Layer 2 - relation adapters. Small, and the only part that knows the design.
rook_pairs(codes, directions)   # grid neighbours  -> NB, self-adjacency
group_pairs(codes, group)       # block co-members -> concurrence

# Layer 3 - statistics, shared. All from S, SS and the diagonal; no triangle
# is ever materialised, so this is O(v^2) arithmetic with no allocation.
pair_stats(counts)  # n_pairs, self, total, max, var, s2, n_zero

# Layer 4 - public faces, all thin.
calculate_nb()             # attaches pair names; same shape as today
calculate_cooccurrence()   # the v x v matrix, `relation = c("adjacency", "block")`
calculate_incidence()      # treatment x level (family B)
```

`count_pairs()` matches the layer-2 adapters to the container `speed()` already has: the SA loop holds
integer codes (`speed()` factors at [R/speed.R:206](R/speed.R#L206)), so no adapter allocates strings
and the objective never touches layer 4.

The two remaining internals, for families B and C:

```r
incidence_table(df, swap, factor_col)  # treatment x level counts (family B)
treatment_indicator(df, swap)          # the n x v X (family C)
```

### Verified 2026-08-10 against the current implementation

| Check | Result |
|---|---|
| Layers 1-3 vs `calculate_nb()`, 300 random designs | **0 mismatches** on `var`, `s2`, `self`, `max`, `n_pairs`, `total`, `n_zero` |
| Defect 4 as a splice: `diag(counts) <- diag(counts_both)` | self 1 -> 3, off-diagonals provably **unchanged** |
| `group_pairs()` vs `table()` then `M %*% t(M)` | off-diagonal concurrences **identical** |
| MET pooling as `counts1 + counts2` | structural zeros retained across sites |
| Cost, 2018 Ex1 / 2021 Ex4 | **7.4x / 3.5x faster** than current `calculate_nb()` |

The layering is *faster* than the monolithic matrix version measured in the plan's *Performance* section
(0.033 ms against 0.057 on Ex1) because `pair_stats()` never builds the named vector - only layer 4
does, and only when a caller asks for it.

### The one caveat that survives

**The diagonal means different things per relation, and this must be documented.** Under `rook_pairs()`
it is the **self-adjacency count**; under `group_pairs()` it is **within-block self-pairs**, which is
*not* `N N'`'s diagonal (replication). Measured on 5 treatments in 4 complete blocks: worker diagonal
`0,0,0,0,0`, `N N'` diagonal `4,4,4,4,4`. Off-diagonals agree exactly. So
`calculate_cooccurrence(relation = "block")` must either document its diagonal or fill it with
replication to match the design-theory convention - a deliberate choice, not an accident.

---

## What deliberately does not fold in

- **[`adjacency_score_vec()`](R/calculate_adjacency_score.R#L132) stays as it is.** Arbitrary ring
  offsets x graded `relationship` lookups x per-cell output is a genuinely different function, and
  collapsing it into a `v x v` container would lose the per-cell result that
  `calculate_adjacency_score()` needs. But **at `ring_dists = 1, ring_type = "manhattan"` the ring is
  exactly rook adjacency**, so `sum(diag(counts))` must equal `calculate_adjacency_score()` at the
  defaults. `feature/incidence`'s review notes verified that identity holds at the defaults and **fails**
  under `ring_type = "chebyshev"` (5 against 1), so pin it scoped to rook distance 1.
- **[`.efficiency_factor()`](R/summary.R#L964)** is a legitimate guarded wrapper over
  `calculate_efficiency_factor()`, not a duplicate.
- **[`get_vertices()`](R/metrics.R#L784) / [`get_edges()`](R/metrics.R#L820)** - exported, no longer
  used by `calculate_ed()`, two near-duplicate test files. A separate keep-or-deprecate decision,
  recorded under *Findings not previously recorded* in
  [PIEPHO_ALIGNMENT_PLAN.md](PIEPHO_ALIGNMENT_PLAN.md).

---

## The staged path

Each stage is independently shippable and has a different blast radius. **Do not bundle stage 1 with
stages 2-4:** stage 1 changes returned designs, the others change nothing, and mixing them makes any
regression unattributable.

**Stage 0 - prerequisite, not part of this work.** Defects 1 and 3 in the plan. Defect 1 freezes the
incremental ED path, so *no performance claim here can be validated until it lands*.

**Stage 1 - layers 1-3, plus `calculate_nb()` as the first layer-4 face.** Add `count_pairs()`,
`rook_pairs()` and `pair_stats()` as sketched above; `calculate_nb()` becomes a thin wrapper that
attaches pair names. **Closes defects 2 and 4.** Its return keeps the same shape and names, but
`self_adjacencies` **values change** for every non-square grid - that is defect 4's fix, not an
accident. Behaviour-changing, so: own commit, NEWS under *Bug Fixes*. Blast radius already measured -
two assertions in `test-calculate_nb.R`, plus a reworded error message at lines 105-109. Do **not** add
`calculate_cooccurrence()` / `calculate_incidence()` here; new public API in a behaviour-changing commit
makes a revert expensive.

**Stage 2 - collapse the other two adjacency copies onto it.** The piepho pooling block becomes
`Reduce("+", counts)` then `pair_stats()`; `.neighbour_balance()` calls the same primitive. Pure
refactor - pin `summary()`'s output first, then assert it is unchanged. This ends the
objective-versus-summary contradiction in defect 4 *by construction* rather than by keeping two
implementations in step.

**Stage 3 - `incidence_table()`.** Merges the table built twice in
`calculate_balance_score`/`.balance_score_min` and the `M` built twice in
`.design_concurrence`/`.block_spread`. Pure refactor, no API change, cheapest stage here - it can be
done at any point, including before stage 1.

**Stage 4 - `treatment_indicator()`.** Extract the `n x v` `X` from `calculate_efficiency_factor()`. No
behaviour change on `main`; it is the prerequisite for stages 5-6.

**Stage 5 - rebase `origin/feature/incidence` and add the remaining layer-4 faces.**
`calculate_cooccurrence(relation = c("adjacency", "block"))` and `calculate_incidence()` land here, on
top of the already-shipped layers 1-3. This settles that branch's own **I5** ("duplicates
`calculate_nb()`'s edge enumeration") and answers its **I-D2** naming question as a consequence rather
than a style call: with one implementation serving two relations, the adjacency-versus-block distinction
belongs in a `relation =` argument, not in two function names - it makes the shared implementation
visible in the API and stops the two faces drifting again. Document the diagonal convention per
relation. **But settle open decision 1 below first** - whether these belong in the public API at all.

**Stage 6 - rebase `info-objective` and `origin/feature/a-optimality`** onto `treatment_indicator()`.
`calc_concurrence_matrix()` collapses into `incidence_table()` plus the product, which is
`.design_concurrence()`'s body. See [BRANCH_TRIAGE.md](BRANCH_TRIAGE.md) §3a for the merge mechanics -
both branches are additive rather than reverting, verified.

---

## The test that makes it stick

There are three copies of adjacency counting on `main` alone because **nothing asserts they agree** -
which is also why `.neighbour_balance()` could be right about both directions while the objective was
wrong (defect 4) without a single failure. One cross-path invariant test is the durable fix: on the same
design, `calculate_nb()`, `.neighbour_balance()` and `calculate_pair_incidence()` must return identical
counts, and `sum(diag(counts))` must equal `calculate_adjacency_score()` at rook distance 1. Without it
a future branch adds a fourth copy and nothing fails.

---

## Open decisions

**1. Should `calculate_cooccurrence()` and `calculate_incidence()` be exported at all?** Stage 5 assumes
yes, but the prior question is unanswered. `summary()` already reports neighbour balance,
self-adjacency, concurrence (`lambda_min`/`lambda_max`/`n_zero_pairs`) and block spread. If that covers
the reporting need, this adds two exported functions, two `.Rd` files and `_pkgdown.yml` entries for
little gain - while `create_pair_mapping()` is an existing exported function whose reason to exist is
about to evaporate. `origin/feature/incidence` exists because someone wanted these, but no document
records who or for what. Settle this before stage 5; stages 1-4 do not depend on it.

**2. Where does the new code live?** Unresolved, and it needs to be before stage 1. `R/metrics.R` is
already 1071 lines. Repo precedent supports a dedicated file -
[R/calculate_adjacency_score.R](R/calculate_adjacency_score.R) is one file for one function family, as
are `R/buffers.R` and `R/summary.R`. Note `origin/feature/incidence` has already claimed `R/incidence.R`
for `calculate_pair_incidence()` / `calculate_position_incidence()`, so stage 5 either adopts that name
or renames on rebase. Recommendation: `R/cooccurrence.R` for layers 1-3, leaving `R/incidence.R` free
for family B if that branch's names survive.

**3. Dot prefix on the new internals - open.** Shown undotted above, matching
`adjacency_score_vec()`, `build_design_matrix()`, `grid_index()` and `random_initialise()`, and matching
the maintainer's stated preference. The counter-case is that the closest siblings by *subject* are
dotted (`.neighbour_balance()`, `.design_concurrence()`, `.block_spread()`, `.balance_score_min()`), so
undotted names put related functions on opposite sides of a convention. The repo is genuinely mixed and
[KNOWN_ISSUES.md](KNOWN_ISSUES.md) item 5 already tracks this as a package-wide inconsistency; this
refactor adds six names to whichever side it picks, so it is a reasonable moment to decide. Either way
it is a rename, not a redesign.

**4. `calculate_cooccurrence(relation = "block")`'s diagonal** - document as within-block self-pairs,
or fill with replication to match `N N'`. See *The one caveat that survives*.
