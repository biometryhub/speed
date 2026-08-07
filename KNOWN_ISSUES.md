# Known issues

Package-level issues that outlive any one branch. Anything with an owning branch lives in that
branch's review notes instead:

| File | Workstream |
|---|---|
| `REVIEW-NOTES.md` | `feature/incidence` (PR #97) — `R/incidence.R` |
| `REVIEW-NOTES-SUMMARY.md` | the merged `summary()` work — `R/summary.R` |
| `REVIEW-NOTES-OTHER.md` | grid construction / core metrics — `bugfix/grid-orientation` |
| `REVIEW-NOTES-EFFICIENCY.md` | efficiency-factor statistics — branch `feature/a-optimality` exists |
| `REVIEW-NOTES-PR91.md` | PR #91 `info-objective` |

The buffer handover spec - what `biometryassist` needs so buffers can move there - lives in the
**biometryassist** repo as `BUFFERS-HANDOFF.md`, since that is where the work happens.

Items that have been fully resolved are removed; see git history for what they were.

## 1. Buffer coordinate convention: buffers must not change any metric

**Decided 2026-08-06. Package-level, so it lives here rather than in a branch's notes.**

**The convention: a buffered design must score exactly as the same design unbuffered.** Plots either
side of a buffer **are** neighbours.

**Why:** a buffered trial is *analysed* with the buffer plots excluded, and the model then treats the
remaining plots as a contiguous grid - adjacent even where they are physically separated. A design
metric should describe the layout the analysis will see, not the physical field.

**Why it breaks.** `add_buffers()` displaces the real plots' `row`/`col` coordinates to make room
(`row + 1` for `"edge"`, `row * 2` for `"row"`, `3 * row - 1` for `"double row"`, ...) and never undoes
it ([R/buffers.R:24-47](R/buffers.R#L24-L47)). Grid construction reads coordinates **raw**, which is
correct - it preserves genuine physical gaps (a road, an irregular edge) that an inferred undo would
collapse - so an unrestored displacement shows up directly in the metrics.

**Not yet true on `bugfix/grid-orientation`. Measured 2026-08-06** on a 4×3 design clumped by column
(adjacency 9 unbuffered), scoring the de-buffered frame:

| buffer type | adjacency |
|---|---|
| none | 9 |
| `"edge"` | 9 - matches (it offsets, it inserts no gap) |
| `"row"`, `"double row"` | **0** - convention violated |
| `"col"`, `"double col"` | 9, but only because this design's adjacencies all run *within* columns; a gap on the other axis would break it the same way |

**The fix lives on `feature/buffers`**, which records the displacement in `metadata$buffer` and inverts
it in `.drop_buffer_rows()` - undone where it is created, not compensated for in each consumer. None of
that machinery (`metadata$buffer`, `.restore_buffer_coords()`, `.warn_if_buffers()`) exists on
`bugfix/grid-orientation` yet, so **this section describes the intended end state, not current
behaviour.** See A2 of `REVIEW-NOTES-OTHER.md` for what arrives on merge.

Ranking coordinates downstream was considered and rejected: it is only the inverse of the displacement,
applied by guesswork, and cannot tell a buffer from a real hole. Verified for all five buffer types -
ranking the de-buffered coordinates restores the original `1..n` exactly, which is what shows it to be an
undo rather than a statistical position on buffers.

Two rules follow for any grid-shaped metric: read the plot coordinates, never the data frame's row
order; and take the design from `.drop_buffer_rows()` rather than reaching into `design_df` directly.

- ✅ **#1a - neighbour balance.** `.neighbour_balance()` reads coordinates instead of reshaping the
  treatment column. The gap question is settled above, but the buffer *restoration* it depends on is
  still inbound, so the test named after this anchor
  ([test-summary.R:298](tests/testthat/test-summary.R#L298)) currently asserts only the `"edge"` case -
  and its comment states the **opposite** rule ("plots either side of a buffer are not neighbours").
  `feature/buffers` rewrites both. Anyone reading that comment before then will take the wrong rule from
  it.

**This section is time-limited.** `add_buffers()` is deprecated as of 0.0.10 and moving to
`biometryassist` (see `BUFFERS-HANDOFF.md` in that repo). Once it goes, speed never creates a displacement, the
restoration goes with it, and only the first of the two rules above still needs stating.

The `#1a` anchor is retained because `test-summary.R` names it in a test description; it can go when
that test is renamed.

## 2. Both packages register S3 methods on class `"design"`

speed and `biometryassist` both use `class(x) == c("design", "list")`, and speed registers methods on a
class name it does not own. **Measured** with both loaded, calling on a **biometryassist** design:

| Call | Result |
|---|---|
| `summary(des)` | speed's `summary.design` runs → `"This design has no metadata; ... Re-run speed()"` |
| `print(des)` | speed's `print.design` runs → prints `"Optimised Experimental Design"`; wrong, and silent |
| `autoplot(des)` | last package loaded wins (`Registered S3 method overwritten by 'biometryassist'`) |

`biometryassist` defines neither `print.design` nor `summary.design`, so speed's methods capture its
objects unopposed. `print()` is the worst of the three: no error, just plausible wrong output. speed is
the newer package and the one that adopted an already-taken class name, so speed is the side that should
move.

**Fix:** one line at [R/speed.R:491](R/speed.R#L491) -
`class(output) <- c("speed_design", "design", class(output))` - then rename speed's four registrations to
`autoplot.speed_design`, `print.speed_design`, `summary.speed_design`, `print.summary.speed_design`.
Keep `"design"` in the vector so `inherits(x, "design")` keeps working; consider dropping it later so
cross-package calls fail cleanly rather than half-succeeding.

This also gives the `biometryassist` adapter its discriminator (`inherits(x, "speed_design")`),
replacing a `[["design_df"]]` sniff - see change 5 of `BUFFERS-HANDOFF.md` in the biometryassist repo.

**Own branch.** Small, but it touches `R/plotting.R`, `R/summary.R`, `R/speed.R`, `NAMESPACE`, four
`man/` pages and any test asserting `class(x)`.

## 3. `initialise_design_df()` fills `items` down columns, and nothing says so

`initialise_design_df()` builds its grid with `expand.grid(row = 1:nrows, col = 1:ncols)`
([R/design_utils.R:294](R/design_utils.R#L294)), which varies `row` fastest, then assigns
`df$treatment <- items` positionally. So `items` is read **down columns**. Nothing in
`?initialise_design_df` says so, and the natural way to transcribe a design - one grid row per source
line - silently produces a *different design* from the one on the page.

The package's own test suite fell into it. `test-calculate_efficiency_factor.R` writes four published
designs visually, 4 rows of 9, and asserted the paper's values. **Measured:**

| Design | as written | grid actually matching the paper | paper's value |
|---|---|---|---|
| 1 | 0.644 | **0.834** | 0.834 |
| 2 | 0.683 | **0.783** | 0.783 |
| 3 | 0.540 | **0.827** | 0.827 |

It passed on `main` only because two conventions cancelled: `initialise_design_df()` stored the design
column-major and `calculate_efficiency_factor()` read it back positionally row-major, recovering the
design the author wrote. Fixing the latter on `bugfix/grid-orientation` broke the cancellation and
exposed both. Supplying the items column-major reproduces every published value exactly, which is the
confirmation that the grid fix is right and the storage was wrong.

**Worked around in the tests** on `bugfix/grid-orientation` via a local `by_row()` helper, so the
literals stay readable against the paper. **Not fixed in the package** - it is a user-facing API
question:

- At minimum, document the fill order in `?initialise_design_df` with a worked example.
- Better, add `byrow = FALSE`, mirroring `matrix()`. Anyone transcribing a published design wants
  `byrow = TRUE` and silently gets a different design today.

Check the other tests and the vignettes for the same latent transposition before that lands.

## 4. The row-major sort in `speed()` is no longer load-bearing

[R/speed.R:227](R/speed.R#L227) sorts the design row-major before the annealing loop. That existed so
grid metrics could reshape the treatment column positionally. Now that every grid is built from
coordinates, it is no longer needed for correctness - the order-invariance tests on
`bugfix/grid-orientation` are what prove that.

One detail worth knowing before anyone removes it: it is **conditional on `inferred$inferred`**, so it
runs only when the row/column columns were auto-detected and not when they were named explicitly -
meaning the two paths already produce differently ordered frames today, and any code that does depend on
row order is only reliably fed by one of them.

Leave the sort for now: `generate_neighbour()`, `random_initialise()`, `print.design()` and `autoplot()`
may rely on row order. Removing it is a simplification to make deliberately, on its own branch, not
bundled with a bug fix.

## 5. Internal functions are inconsistently dot-prefixed (low severity, cosmetic)

The package uses two naming conventions for non-exported functions, with no rule distinguishing
them:

- **Dot-prefixed:** `.verify_*` (R/verify_utils.R), `.mst_mean_prim` / `.mst_mean_igraph`
  (R/metrics.R), `.grid_stop` (R/design_utils.R), and all of `summary.R`'s internals
  (`.neighbour_balance`, `.replicate_spans`, `.efficiency_factor`, ...).
- **Not dot-prefixed:** `ring_offsets`, `shift_pad` (R/calculate_adjacency_score.R),
  `env_add_one`, `to_factor`, `to_types`, `pseudo_inverse`, `as_numeric_factor` (R/utils.R), and
  `build_design_matrix` / `grid_index` / `grid_indices` (R/design_utils.R).

`bugfix/grid-orientation` shows how easily it drifts: it added `grid_index()` and `grid_indices()`
un-prefixed and `.grid_stop()` prefixed, in the same workstream.

All are `@keywords internal` and none are in `NAMESPACE`, so the distinction carries no meaning. The
practical consequence is in `man/`: a non-prefixed internal generates e.g. `man/ring_offsets.Rd`,
which sits alongside genuine public API and reads as exported to anyone browsing the directory,
whereas a dot-prefixed one generates `man/dot-*.Rd` and is obviously internal.

**Suggested fix direction:** pick one convention and apply it. Dot-prefixing everything internal is
the more informative option, since `man/dot-*.Rd` is self-labelling. Note this is a rename-only
change for the functions listed above - none are exported, so there is no deprecation cycle - but it
touches roxygen cross-references (`@inheritParams`, `[fn]` links) and any tests that call the
helpers directly.

**`CLAUDE.md` needs updating in the same change.** Its "Source layout (R/)" section names several of
these helpers (`env_add_one`, `to_factor` / `to_types`, `pseudo_inverse`, and `.verify_*`), and its
"Conventions to preserve" section has no entry for internal naming - whichever convention is chosen
should be recorded there so it is not re-litigated.

**Do this on its own branch.** It is a wide, purely mechanical rename that would swamp the diff of
any functional change it was bundled with.
