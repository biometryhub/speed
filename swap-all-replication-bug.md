# Bug: `swap_all = TRUE` silently changes treatment replication

**Status:** fixed on `bugfix/swap-all-replication` by rejecting the input up front - see
[Resolution](#resolution). Present in 0.0.10 and every earlier version that has
`generate_multi_swap_neighbour`.

Note on line numbers: the code references below are as they stand on this branch (0.0.9,
no `linked_cols`). The `linked_cols` section at the end describes the 0.0.10 branch, where
this code path also carries a `rep_len()` recycling step.

**Severity:** high — wrong results, no error, no warning. The returned object is not a
rearrangement of the input design; it is a different design with different replication.

**Found:** while implementing `linked_cols` (0.0.10), building a test for a design with
unequal treatment group sizes. It is unrelated to `linked_cols` and reproduces with the
feature entirely unused.

## Summary

When `swap_all = TRUE`, `speed()` proposes moves by exchanging two treatment *labels*
across every plot holding them. If the two treatments do not occupy the same number of
plots within the swap group, the exchange **swaps their replication counts** rather than
their positions. Repeated over an optimisation run, treatment totals drift away from the
input.

Designs with equal replication inside every swap group — which includes the balanced
split-plot in `?speed` — are unaffected, which is why this has gone unnoticed.

## Reproducing

Two blocks, each holding treatment A three times, B twice and C once:

```r
df <- data.frame(
  row       = rep(1:6, times = 2),
  col       = rep(1:2, each = 6),
  block     = rep(1:2, each = 6),
  treatment = rep(c("A", "A", "A", "B", "B", "C"), 2)
)

table(df$treatment)
#> A B C
#> 6 4 2
```

With `swap_all = TRUE` the totals change, differently for each seed:

```r
for (s in c(1, 2, 3, 42)) {
  r <- speed(df, swap = "treatment", swap_within = "block",
             swap_all = TRUE, seed = s, quiet = TRUE)
  print(table(r$design_df$treatment))
}
#> A B C        A B C        A B C        A B C
#> 4 5 3        4 3 5        3 5 4        5 3 4
```

The same design with `swap_all = FALSE` is preserved exactly, for every seed:

```r
#> A B C
#> 6 4 2
```

### The mechanism in isolation

Calling the neighbour generator directly shows the exchange:

```r
d <- data.frame(
  row       = rep(1:2, times = 3),
  col       = rep(1:3, each = 2),
  treatment = factor(c("A", "A", "A", "A", "B", "B")),
  grp       = factor(rep(1, 6))
)

table(d$treatment)
#> A B
#> 4 2

set.seed(1)
n <- generate_multi_swap_neighbour(d, "treatment", "grp", 1, TRUE)
table(n$design$treatment)
#> A B
#> 2 4
```

A had four plots and B had two; after one proposal A has two and B has four. The counts
have been exchanged along with the labels.

## Root cause

[design_utils.R:131-136](R/design_utils.R#L131-L136):

```r
plots_1 <- which(group_filter & new_design[[swap]] == swap_pair[1])
plots_2 <- which(group_filter & new_design[[swap]] == swap_pair[2])

new_design[[swap]][plots_1] <- swap_pair[2]
new_design[[swap]][plots_2] <- swap_pair[1]
```

`plots_1` and `plots_2` are index vectors of *every* plot holding each treatment in the
group. Nothing constrains them to the same length. When they differ, the assignment
relabels a set of size `n1` to treatment 2 and a set of size `n2` to treatment 1, so
after the move:

```
count(t1) == n2      count(t2) == n1
```

That is a valid rearrangement only when `n1 == n2`. Otherwise the design's replication
structure has been altered, and because the objective functions do not score replication,
nothing detects it. Whichever such design happens to score best is then returned.

## Why it has stayed hidden

Two things mask it:

1. **Balanced designs are immune.** In the split-plot in `?speed`, every whole-plot
   treatment occupies exactly one whole plot per block, so `n1 == n2` always and the
   exchange is a genuine relocation. All existing tests are of this shape.

2. **Single-group designs never accept the move.** With no `swap_within`, the whole design
   is one group, so a multi-swap is a *global* relabel. Under the default objective
   function that is score-invariant — measured directly, score 7 before and 7 after — so
   the proposal never improves on the incumbent and `best_design` is never replaced. The
   corruption is generated but discarded.

   Worth noting separately: this means **`swap_all = TRUE` with no `swap_within` cannot
   improve anything under the default objective function.** Every proposal it can make is
   score-neutral. That is its own issue. (It would not hold for an objective function that
   is sensitive to treatment identity, such as one supplied with a `relationship` matrix.)

So the bug needs *both* unequal within-group replication *and* more than one swap group
before it becomes visible — but that combination is an ordinary unbalanced design, not an
exotic one.

## Who is affected

Any call with `swap_all = TRUE` where some treatment appears a different number of times
from another **within the same `swap_within` group**. In practice:

- unbalanced designs, or designs with a control replicated more heavily than the entries;
- split-plots where a whole-plot treatment occupies more whole plots than another in the
  same block;
- MET or multi-site designs where a line appears more often at one site than another,
  when the site is the swap group.

Not affected: `swap_all = FALSE` (the single-swap generator exchanges exactly two plots
and is replication-preserving by construction), and any design with equal within-group
replication.

## Possible fixes

**A. Reject unequal-size proposals.** In `generate_multi_swap_neighbour`, skip the swap
when `length(plots_1) != length(plots_2)`. Smallest change, and correct — it narrows the
neighbourhood to moves that preserve the design. The risk is a treatment whose group size
matches nothing else becomes unswappable and the search silently stalls, so it should be
paired with a check that each group has at least one legal pair.

**B. Swap grouping units instead of label sets.** For the split-plot case, the intended
move is "give whole plot *i* the treatment currently on whole plot *j*". Exchanging units
rather than "every row with this label" makes unequal sizes impossible to express and
matches what `swap_all` is for. Larger change: the generator would need to know the unit
column, not just the swap group.

**C. Validate up front.** Error, or warn, when `swap_all = TRUE` and within-group
replication is unequal. Does not fix anything, but converts a silent wrong answer into a
visible one, and is cheap.

**Rejected: exchange a size-matched random subset.** Trimming the larger set to match the
smaller one preserves replication but breaks the point of `swap_all` — a whole plot would
end up holding two different whole-plot treatments. Reconsidered and rejected again during
the fix: it is replication-correct, but because the subset is chosen at *plot* level it
fragments the grouping unit, turning a visible replication error into an invisible
structural one — `table(treatment)` then looks right on a design that is no longer a split
plot.

## Resolution

**C, as an error.** `swap_all` exists for hierarchical designs, where a swap group holding
one treatment more often than another is a malformed design rather than a case to optimise
around. The move is undefined there, so `speed()` refuses it instead of choosing a
semantics for it.

`.verify_swap_all_replication()` ([verify_utils.R](R/verify_utils.R)) is called from
`speed()` ([speed.R:217](R/speed.R#L217)) on the resolved `optimise` list, immediately
after `swap_within` is normalised and before any optimisation runs. Because it works from
the resolved list it covers all three input shapes — simple, legacy hierarchical, and
`optimise = ` — and levels that set `swap_all` individually. The generator in
`design_utils.R` is untouched; the bad state can no longer be reached.

For each level with `swap_all = TRUE`, the treatments *present* in each group must have
equal counts. Deliberately permitted:

- **incomplete groups** — a treatment absent from a group is never sampled for a swap
  there, so it cannot cause an unequal exchange;
- **single-treatment groups** — the generator already skips them;
- **`NA`** in either column, excluded to match `generate_multi_swap_neighbour`.

The error names the level (for multi-level designs), the column, the first offending group
with its counts, and how many other groups are affected.

Applies to `swap_all = TRUE` with no `swap_within` too, where the group is the whole
design. That case could not produce a wrong answer under the default objective function
(see below), but the move is equally undefined, and an objective function sensitive to
treatment identity would accept it.

Not addressed, still open: **`swap_all = TRUE` with no `swap_within` cannot improve
anything under the default objective function**, since every proposal it can make is
score-neutral. Now that unequal replication errors, the remaining case is a balanced design
where the run is simply a no-op. Worth a warning of its own.

Found while fixing: the fixture in `test_that("autoplot palettes work with hierarchical
designs")` was itself a malformed split plot — whole plots straddled the block boundary,
giving `A 8/0, B 4/4, C 0/8` across two blocks — and was corrected to nest whole plots
inside blocks. Its two `vdiffr` snapshots were regenerated.

Still worth doing: **B** would allow per-unit swaps on unequally replicated designs rather
than rejecting them, and the unit column is often already available — in the split-plot in
`?speed`, level 2's `swap_within` (`wholeplot`) is exactly level 1's unit column.

## Interaction with `linked_cols`

`linked_cols` (0.0.10) tracks provenance through swaps, and had to take a position on this
code path. It currently recycles with `rep_len()` when the two sets differ in size
([design_utils.R:147-156](R/design_utils.R#L147-L156)), and `.verify_linked_cols` requires
linked columns on a `swap_all` level to be functionally dependent on the treatment, which
makes the recycling immaterial. Linked columns therefore stay correct even on an affected
design — they faithfully track whatever the treatment column does, including the drift.

Verified: with the feature on and off, the resulting treatment vector is identical, so
`linked_cols` neither causes nor worsens this.

If fix **A** or **B** lands, the two sets are always equal in size, and:

- the `rep_len()` recycling can become a plain positional exchange;
- the functional-dependence restriction on `swap_all` levels could be relaxed, allowing
  per-plot linked columns everywhere.
