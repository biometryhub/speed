# Implementation Plan: Relax `.verify_functionally_dependent()` for `linked_cols` on `swap_all` Levels

## Background

`linked_cols` (added on `feature/keep-with`) lets a companion column travel with the
`swap` column it belongs to. Internally, `speed()` stamps a hidden per-plot provenance
index (`.origin_<n>_<timestamp>`), moves that index in lockstep with the treatment
through every swap, and uses it at the end to reorder the linked columns:

```r
design$design_df[[col]] <- linked_values[[col]][design$design_df[[origin_col]]]
```

On a level with `swap_all = TRUE`, `.verify_linked_cols()` currently calls
`.verify_functionally_dependent()`, which requires a linked column to have **exactly one
value per treatment** at that level. A per-plot column such as `plot_id` is rejected:

```
`linked_cols` column 'plot_id' is not uniquely determined by 'wp_trt' at level 'wp':
treatment 'A' has 3 different values ('P01', 'P02', 'P03'). A level using
`swap_all = TRUE` moves whole treatment groups at once, so its linked columns must have
exactly one value per treatment.
```

That rule was written when `generate_multi_swap_neighbour()` exchanged provenance
indices with `rep_len()` recycling, because the two treatment groups could differ in
size and there was no unit-level bijection to preserve. **Both of those premises have
since changed**, so the restriction is now stricter than the implementation requires.

## Why the restriction is no longer necessary

Two changes removed the reason for it:

1. **PR #110 (`bugfix/swap-all-replication`)** added `.verify_swap_all_replication()`
   ([R/verify_utils.R:278](R/verify_utils.R#L278), called from
   [R/speed.R:260](R/speed.R#L260)), which rejects unequal replication within a swap
   group up front. A `swap_all` exchange therefore always moves two plot sets *of the
   same size*.
2. On the back of that, the `rep_len()` recycling in
   `generate_multi_swap_neighbour()` was replaced with a straight one-for-one exchange
   ([R/design_utils.R:146-154](R/design_utils.R#L146-L154)).

With equal-sized sets and a straight exchange, the operation is a **bijection**: every
provenance index is used exactly once, so `linked_values[[col]][origin]` is a valid
permutation of the input column rather than a lookup that happens to be well defined
only for functionally dependent values.

Verified empirically — 200 successive multi-swaps on an 8-plot, 2-treatment group:

```r
d <- data.frame(block = factor(rep(1, 8)), wp_trt = factor(rep(c("A", "B"), each = 4)))
d$.origin <- seq_len(8)
for (i in 1:200) {
  d <- generate_multi_swap_neighbour(d, "wp_trt", "block", 1, FALSE, ".origin")$design
}
identical(sort(d$.origin), seq_len(8))   # TRUE  - still a permutation
# and every plot_id still sits with the treatment it started with
```

The same already holds on the other two paths that move the index:
`generate_single_swap_neighbour()` (an exact 2-element exchange) and `shuffle_items()`
(one permutation applied to both columns).

## Blocker: fix this first

**Status: resolved** on `bugfix/swap-all-replication-mid-search` (Option A, commits `6ac6256`
and `96631bc`). Merge that to `main`, then merge `main` into `feature/keep-with` before
starting the "Proposed change" section below. The merge of the two has been tested and is
clean.

**Do not relax the guard before resolving this.** `.verify_swap_all_replication()`
checks the *input* data once, before the SA loop. In a hierarchical design where two
levels have **cross-cutting `swap_within` groups**, a swap at level 1 can break the
equal-replication invariant that level 2 relies on, at runtime, after the check has
already passed.

Reproducer — `block` and `site` cut across each other, and replication is equal within
every block *and* every site on input, so verification passes:

```r
df <- data.frame(
  row   = rep(1:6, times = 2),
  col   = rep(1:2, each = 6),
  block = c(1, 1, 1, 1, 1, 1,  2, 2, 2, 2, 2, 2),
  site  = c("a", "a", "a", "b", "b", "b",  "a", "a", "a", "b", "b", "b"),
  lines = c("X", "X", "Z", "Y", "Y", "Z",  "Y", "Y", "Z", "X", "X", "Z")
)
df$line_name <- paste0("L_", df$lines)

res <- speed(df, swap = "lines",
  optimise = list(
    lvl1 = list(swap_within = "block", swap_all = TRUE),
    lvl2 = list(swap_within = "site",  swap_all = TRUE)
  ),
  linked_cols = "line_name", iterations = 30, seed = 3, quiet = TRUE)

table(res$design_df$site, res$design_df$lines)
#     X Y Z
#   a 2 3 1     <- unequal within site, though it was 2/2/2 on input
#   b 2 1 3
```

Level 1 relabels treatments inside `block`, which permutes the per-site counts
non-uniformly; level 2 then draws two treatment sets of *different* sizes and the
one-for-one exchange silently recycles:

```r
# groups of size 3 and 1
generate_multi_swap_neighbour(d, "t", "block", 1, FALSE, ".o")
# Warning: number of items to replace is not a multiple of replacement length
# origins: 4,4,4,1   -> no longer a permutation
```

Today this is invisible: the functional-dependence guard means the only columns allowed
here are ones where a duplicated index still yields the right value. **Relaxing the
guard is what would make it a real corruption**, so the invariant has to be enforced
where it is actually needed. Note the underlying replication change is a pre-existing
gap in #110's guard and affects designs with no `linked_cols` at all — it is worth
fixing on its own merits.

### How reachable is this?

Narrower than the reproducer suggests. All five of the following are required:

1. Two or more levels with `swap_all = TRUE`;
2. both optimising the **same** treatment column — in a normal split-plot, level 1 swaps
   `wp_trt` and level 2 `sp_trt`, so level 1 cannot disturb level 2;
3. `swap_within` factors that are **crossed, not nested**;
4. input balanced within every group of every level, or
   `.verify_swap_all_replication()` errors before the search starts; **and**
5. treatments unbalanced within the **intersection cells** of the two factors.

Condition 5 is the binding one and is easy to miss. If each block × site cell is
internally balanced, a level 1 swap permutes labels symmetrically inside every cell and
the level 2 margins do not move. Measured against `main` before the fix:

| Design | Outcome |
| --- | --- |
| MET, blocks nested within sites | preserved — nesting makes it structurally impossible |
| 4×4 Latin square, row × col | never searches (scores 0, early stop) |
| Row × col, 2 treatments | never searches (adjacency weight forced to 0) |
| 24 plots, 4 blocks × 2 sites crossed, **cells balanced** | 0 of 10 seeds affected |
| 12 plots, 2 blocks × 2 sites crossed, **cells unbalanced** | affected (the reproducer above) |

So this is a corner, not a common case. It is still worth fixing: the failure is silent
and returns a design with the wrong replication, the fix is local and provably inert
outside the precondition, and relaxing `linked_cols` widens the exposure rather than
narrowing it.

Options, roughly in order of preference:

- **A. Re-check per swap group at generation time.** In
  `generate_multi_swap_neighbour()`, draw `swap_pair` only from treatments with equal
  counts in the group, or skip the swap when the drawn pair is unequal. Keeps the
  invariant local to where it is used and needs no cross-level reasoning.
- **B. Extend `.verify_swap_all_replication()` to cross-cutting levels.** Detect when a
  later level's `swap_within` is not nested within an earlier level's, and reject the
  combination. Cheap, but rejects designs that would have been fine.
- **C. Re-verify between levels** in `speed_hierarchical()`, erroring if a completed
  level has broken a later level's replication. Honest but late — the user gets an error
  after part of the search has run.

Option A is the one that lets `linked_cols` be relaxed unconditionally, and is what was
implemented: `generate_multi_swap_neighbour()` tabulates plots per treatment per group and
draws `swap_pair` only from treatments sharing a replication count, weighted by
`choose(n, 2)` so the pair is still uniform over every exchangeable pair. If no two
treatments in a group share a count, the swap is skipped.

## On merging `main` into `feature/keep-with`

The provenance-exchange block in `generate_multi_swap_neighbour()` carries a comment
justifying the one-for-one exchange by the input check:

> `.verify_swap_all_replication()` rejects unequal replication within a swap
> group, so the two plot sets are the same size and their provenance indices
> exchange one for one

That is the exact reasoning this plan identifies as insufficient — the input check does not
hold mid-search. It is accurate on `feature/keep-with` as it stands today, and becomes wrong
the moment `main` is merged in, so it must be corrected as part of that merge rather than
before it. Replacement:

```r
# The swap pair is drawn from treatments with equal replication, so the two plot
# sets are the same size and their provenance indices exchange one for one
```

## Proposed change

Once the blocker is resolved:

1. **Delete `.verify_functionally_dependent()`** from
   [R/verify_utils.R](R/verify_utils.R) and its call site in `.verify_linked_cols()`
   (the `if (isTRUE(opt$swap_all))` branch). Nothing else calls it.
2. `.verify_linked_cols()` then applies one uniform set of rules regardless of
   `swap_all`: the column must exist, must not be a `swap` / `swap_within` / spatial
   factor column, and must not be linked to two different swap columns.
3. **Simplify the roxygen** for `linked_cols` in [R/speed.R](R/speed.R) — drop the final
   sentence, which is the only place the restriction is documented:

   > On a level with `swap_all = TRUE` whole treatment groups move at once, so a linked
   > column must have exactly one value per treatment at that level

4. `devtools::document()` to regenerate `man/speed.Rd` and `man/verify.Rd`.

## Tests

In [tests/testthat/test-linked-cols.R](tests/testthat/test-linked-cols.R):

- **Remove** `"linked_cols rejects a per-plot column on a swap_all level"` — that
  behaviour is the thing being removed.
- **Keep** `"linked_cols allows a functionally dependent column on a swap_all level"`;
  it should still pass unchanged.
- **Add** the inverse of the removed test: a per-plot `plot_id` linked on a `swap_all`
  level, asserting `expect_setequal(result$design_df$plot_id, df$plot_id)` (no value
  duplicated or lost) and `expect_pairing_preserved(df, result$design_df, "plot_id",
  "treatment")`. This mirrors the existing single-swap test at
  `"linked_cols carries a per-plot column on a single-swap level"`.
- **Add** a regression test for the blocker — the cross-cutting `block`/`site` design
  above with a per-plot linked column, asserting no warning and that the linked column
  is still a permutation of the input.

In [tests/testthat/test-verify_utils.R](tests/testthat/test-verify_utils.R): there are
no direct `.verify_functionally_dependent()` tests to remove — its only coverage is the
two `speed()`-level tests above. Add coverage there for whichever blocker option is
chosen, alongside the existing `.verify_swap_all_replication()` tests at line 537.

## Also update

- **NEWS.md** — the `linked_cols` bullet under `# speed 0.0.10` currently promises the
  restriction. If `feature/keep-with` has already been released by then, add a separate
  bullet; if not, edit that bullet in place.
- Check the custom-objective and design vignettes for any mention of the rule.

## Open decision: frozen swap groups

Option A skips rather than partially exchanges, so a group in which **no two treatments
share a replication count** can no longer swap at all. The SA loop still runs its full
iteration count over that group and never moves anything, with no indication to the user.

Scope is narrow. `.verify_swap_all_replication()` ([R/verify_utils.R:142](R/verify_utils.R#L142))
errors on *any* unequal replication within a `swap_all` group in the input, so a frozen
group is unreachable from the data as supplied. It arises only mid-search, when an earlier
level with cross-cutting `swap_within` groups unbalances a later level — the same path as
the blocker itself. A partial freeze is the more likely shape: some groups keep swapping
while others stop.

This needs a decision before the `linked_cols` relaxation lands, because relaxing the guard
points `swap_all` at designs with richer per-plot structure and makes the case easier to
hit. Options:

- **Warn.** Emit one warning per run naming the affected groups and level. Honest about a
  search that cannot progress; costs a check in the loop or a flag set by the generator.
- **Leave silent.** The design is returned unchanged at that level, which is a valid if
  useless result. Cheapest, but a full search that provably could not move anything looks
  indistinguishable from one that simply found no improvement.
- **Partial exchange.** Exchange only as many plots as the smaller treatment holds. Preserves
  replication *and* keeps searching, but changes what `swap_all` means — a treatment would end
  up split across its old and new positions rather than moving as a whole group. Effectively a
  different operator, and a larger change than this plan.

**Resolved: warn.** `generate_multi_swap_neighbour()` returns the groups it could not swap
in as `frozen`, and `speed_hierarchical()` collects them and emits one warning per level
after the search. Partial exchange is worth considering on its own merits, but not as part
of this plan.

## Out of scope

- The pairing produced by a multi-swap is **positional** — it follows the order
  `which()` returns, i.e. plot order within the group. It is a valid bijection but an
  arbitrary one, so a per-plot column linked on a `swap_all` level will be correctly
  *permuted* without being meaningfully *paired* plot-for-plot. That is inherent to
  moving whole treatment groups and is not something this change addresses; it may be
  worth a sentence in the `linked_cols` documentation.
- Relaxing `.verify_swap_all_replication()` itself. This plan assumes equal replication
  within a swap group stays a hard requirement.
