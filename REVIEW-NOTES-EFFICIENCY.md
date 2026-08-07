# Review notes: efficiency-factor statistics

**Scope:** the statistics inside `calculate_efficiency_factor()` (R/metrics.R) and what `summary()`
reports from it. **Needs its own branch off `main`** once `bugfix/grid-orientation` has landed - none of
it is a grid or ordering problem, and none of it should be bundled with a bug fix.

**Companion files** — one per workstream:

| File | Workstream |
|---|---|
| `REVIEW-NOTES.md` | `feature/incidence` (PR #97) — `R/incidence.R` |
| `REVIEW-NOTES-SUMMARY.md` | the merged `summary()` work — `R/summary.R` |
| `REVIEW-NOTES-OTHER.md` | grid construction / core metrics — `bugfix/grid-orientation` |
| `REVIEW-NOTES-PR91.md` | PR #91 `info-objective` |
| **this file** | efficiency-factor statistics |

**Last verified:** 2026-08-07, R 4.6.1. All numbers measured, not inferred.

> ✅ **E2 (`Z` omits the intercept) is closed** — fixed on `bugfix/grid-orientation`, because the rank
> gate G14 needed it: without an intercept the treatment mean sits inside the treatment term, so no rank
> test on `A_RC` distinguishes "all contrasts estimable" from "some are not". Verified on a p-rep design
> (8 treatments × 4 reps plus 16 unreplicated): the function now returns **0.4572765**, matching pairwise
> contrast variances from the full model's Moore-Penrose inverse exactly, where the missing intercept
> previously biased unequally replicated designs. Equireplicate values are unchanged.
>
> ⬜ **E1 is the only item left here.**

---

## E1. Report the A-efficiency upper bound in `summary()`

🔷 **Decided 2026-08-06 (Sam): worth doing, its own branch.**

A design's A-efficiency alone doesn't tell you whether it is *good* - 0.53 could be excellent or poor
depending on what the parameters allow. There is a closed-form upper bound on the average efficiency
factor depending only on `(replication, nrow, ncol)` - no matrices, essentially free - so `summary()`
can report how close a design gets to the best achievable:

```
UB = (1/(t-1)) * sum_i [ 1 - minSumSq(r_i, nrow)/(ncol*r_i)
                           - minSumSq(r_i, ncol)/(nrow*r_i)
                           + r_i/n ]
```

where `minSumSq(r, K)` is the even-split minimum of `sum n_ik^2` - for `r` replicates over `K`
rows/columns, `q = r %/% K`, `s = r %% K`, giving `(K - s) * q^2 + s * (q + 1)^2`. It follows from the
canonical efficiency factors summing to a value fixed by how evenly each treatment spreads across rows
and columns, and the harmonic mean being maximised when they are all equal.

**Measured.** Holds as a bound on every design tested; exactly `1.000` for 4×4 and 5×5 Latin squares,
which are A-optimal; and it tracks the optimiser - a 5×6, 10-treatment design moves

| iterations | A-efficiency | bound | % of bound |
|---|---|---|---|
| 0 | 0.000 | 0.815 | 0% |
| 200 | 0.620 | 0.815 | 76% |
| 5000 | 0.774 | 0.815 | **95%** |

That last line is the sentence worth printing: *this design achieves 95% of the best A-efficiency any
arrangement of these treatments in this grid could achieve.*

**Report it as "% of upper bound", never "% of optimal".** `A/UB = 1` proves A-optimality; `A/UB < 1`
does **not** prove sub-optimality, because no arrangement may reach the bound.

**Explicitly declined (Sam):** reporting the raw A-value (average pairwise variance). It is already
computed and discarded inside `calculate_efficiency_factor()`, but it is in σ² units, only comparable
across designs with identical replication, and actively misleading when the design is disconnected -
measured, an unoptimised 5×6 reports 0.503 against the optimised design's 0.862, which looks *better*
and is a `ginv` artefact of the rank deficiency.

**Sketch of the work** (~60-80 lines including docs and tests):

1. New internal `.a_efficiency_bound(r, n_rows, n_cols)` - pure arithmetic.
2. `.efficiency_factor()` (R/summary.R) gains `bound` and `relative` fields; it needs `n_rows`/`n_cols`,
   which are already in the caller's `layout`.
3. Print block, e.g. `Efficiency: 0.7737 (A-efficiency, row-column model)` / `95.0% of the 0.8148 upper bound`.
4. Keep it behind the existing `efficiency = TRUE` flag - the bound is free but only interpretable next
   to the value.
5. Tests: bound `= 1` for a Latin square, bound `>= A-efficiency` across a range of designs.

---

## Reuse: do not write a third eigenvalue implementation

PR #91 (`REVIEW-NOTES-PR91.md`) already computes canonical efficiency factors from the information
matrix via `eigen()`, in a function confusingly named `calculate_efficiency_factors` (plural, one
character from the existing singular). **E1 wants that machinery.** Coordinate with that branch before
starting, and check whether its information matrix includes the intercept - the singular version now
does, and the two must not disagree.

## Related, tracked elsewhere

- The `KNOWN_ISSUES.md` #3 fill-order wart, which produces degenerate fixtures that look like statistics
  problems and are not.
