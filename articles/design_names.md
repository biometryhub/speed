# Naming Designs in speed

## Why “spatially balanced”

Classical design names such as *completely randomised design* and
*randomised complete block design* describe two things at once: a
structure (no blocks, or complete blocks) and the way treatments are
allocated to that structure, which is at random. The randomisation is
part of the design. It is what justifies the standard analysis of that
structure, because every permissible allocation was equally likely to be
used.

`speed` keeps the structures but allocates treatments differently. It
searches for the allocation that minimises an objective function. By
default, that objective penalises like treatments sitting next to each
other, and treatments being unevenly spread across rows, columns and any
other spatial factors. The allocation that comes out is chosen for its
spatial properties rather than drawn at random, so a name containing
“randomised” would describe it wrongly. “Spatially balanced” describes
how the allocation was actually chosen.

> **Note**
>
> The search itself uses random numbers: simulated annealing proposes
> random swaps, and `optim_params(random_initialisation = )` shuffles
> the starting layout. That is randomness in *how the optimum is found*,
> not in *which allocation is used*. Different seeds can return
> different designs, but each one is returned because it scored well.

## The names

| Structure       | Name in `speed`                 | Abbreviation |
|-----------------|---------------------------------|--------------|
| No blocking     | Spatially Balanced Design       | SBD          |
| Complete blocks | Spatially Balanced Block Design | SBBD         |

Names that describe only a structure are unchanged, because the
structure holds however the allocation was chosen. A Latin square still
has each treatment once in every row and column; a split-plot still has
sub-plots nested in whole plots. The same applies to strip-plot,
incomplete block, row-column and p-rep designs.

## What this means for analysis

The allocation was optimised over the spatial factors given in
`spatial_factors` (rows and columns by default), so those factors are
part of the design, and they belong in the analysis model. For example,
an SBBD laid out in rows and columns should be analysed with terms for
the rows and columns it was balanced over, alongside its blocks, rather
than with blocks alone. The rows and columns can enter as fixed or
random effects, or through a spatial model of the residuals (for
example, AR1 × AR1), whichever suits the trial.

[`summary()`](https://rdrr.io/r/base/summary.html) reports the structure
each design was optimised over; see the [Evaluating a
Design](https://biometryhub.github.io/speed/articles/speed.html#evaluating-a-design)
section of the main vignette.
