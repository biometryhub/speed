# `initialise_split_design_df()` — ordering & wording rework

**Status:** in progress — code/doc/test/vignette edits applied, **not yet regenerated or tested**
**Date:** 2026-07-03
**Branch:** `feature/update-sp`

## What this is about

We set out to clean up the unclear wording in `initialise_split_design_df()`
(`R/design_utils.R`). While doing so we found a bigger issue than wording: the
function's `splits` list was ordered **innermost → outermost** (subplot,
wholeplot, block), which was confusing and internally inconsistent (the two
roxygen examples originally disagreed with each other on order).

Decision taken: **flip the public argument order to outermost → innermost**
(block, wholeplot, subplot) while keeping the internal build logic unchanged
(it still works inside-out, generalising cleanly to any depth). The list is
simply reversed once at the function boundary.

## Why outermost → innermost (the reasoning)

1. **Matches statistical convention.** Split-plots are described and randomised
   top-down (blocks → main/whole plots → sub-plots). This is the mental model
   of the target audience.
2. **Self-validating with cell-based dimensions.** Each level's `nrows`/`ncols`
   is in base grid cells and must tile evenly into its parent. Read top-down
   (`3×4 → 1×4 → 1×1`) each line divides the one above it, checkable as you
   read. Bottom-up forward-references a parent not yet written — that mismatch
   (building from the smallest unit but specifying absolute footprints) was the
   real source of the confusion.
3. **Consistent with the rest of the package.** `initialise_design_df()` takes
   the whole-field `nrows`/`ncols` first, then `block_nrows`/`block_ncols` that
   subdivide it (outermost first), and the deprecated `splits` arg is documented
   outermost-first too.

Because this is new, unreleased (0.0.9) API, flipping the order is **not** a
breaking change for any released version.

## Alternative considered (deferred, not chosen)

A genuinely bottom-up "smallest first" API would only make sense if dimensions
were **relative counts** ("this level holds N of the previous level") rather
than absolute cell footprints, e.g.

```r
list(
  subplot   = list(items = letters[1:4]),
  wholeplot = list(items = LETTERS[1:3], n = 4),  # 4 subplots each
  block     = list(n = 3)                          # 3 wholeplots each
)
```

This is coherent but a larger change: new dimension semantics, loss of direct
control over each plot's cell shape, implicit field geometry, and further from
the rest of the package. **Not pursued** unless we decide the compositional
model is worth it. (Reversing the list while keeping absolute cell dims — the
current plan — is the sensible middle; bottom-up *with* absolute dims is the
combination to avoid.)

## Changes applied so far

- [x] `R/design_utils.R` — rewrote `@description`, `@param splits`,
      `@param rep_dim` for the outermost-first order; also fixed wording issues:
      - "innermost is always 1×1" → "defaults to a single cell (1×1)" (multi-cell
        innermost units are still supported by the code, just uncommon)
      - clarified dims are "base grid cells (not child units)"
      - stated the tiling-into-parent constraint (previously only enforced, never
        documented)
      - tightened the `items` recycling rule (length must divide units-per-parent)
- [x] `R/design_utils.R` — both `@examples` flipped to outermost-first
- [x] `R/design_utils.R` — `initialise_split_design_df()` body now reverses the
      list at the boundary (`splits <- rev(add_names(splits))`); build logic
      otherwise unchanged
- [x] `R/design_utils.R` — `.verify_initialise_split_design_df()` now reads
      outermost-first natively (innermost = last entry, walk `names(splits)`),
      so its friendly type-checks still run first
- [x] `tests/testthat/test-initialise_split_design_df.R` — every input `splits`
      list flipped to outermost-first (built designs are identical, so all output
      assertions are unchanged); validation helper and 3-level error case flipped
- [x] `vignettes/speed.qmd` — `splits` list flipped + prose notes the ordering
- [x] `vignettes/complex_designs.qmd` — `splits` list flipped
- [x] `NEWS.md` — added a bullet for the new function noting the ordering

## Remaining work

- [] **Regenerate docs** — `devtools::document()`. `man/initialise_split_design_df.Rd`
      is currently **stale** (still shows innermost-first); it must be regenerated
      from the updated roxygen. (Run via `setwd("/workspaces/speed")` first, not by
      passing a path to `document()`/`test()`, to avoid clobbering vdiffr snapshots.)
- [] **Run tests** — `devtools::test()` (or at least the split-design file). Not
      yet verified after the edits.
- [ ] **Re-render vignettes** — `quarto::quarto_render("vignettes/speed.qmd")` and
      `complex_designs.qmd`, to confirm the flipped lists still produce the intended
      figures.
- [ ] **`devtools::check()`** — full check before any PR (also catches the stale
      `.Rd` / roxygen mismatch).
- [ ] **Air formatting** — confirm edits conform (`air.toml`; don't restyle
      unrelated code).

## Open questions for sign-off

1. **Confirm the direction:** stick with outermost → innermost + cell-based
   dimensions (current plan), or explore the relative-counts bottom-up API?
2. **Multi-cell innermost units:** confirmed we keep the *option* (code already
   supports it; doc now says "defaults to 1×1"). No further action unless you
   want to forbid or emphasise it.
