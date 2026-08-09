# Plan: p-rep colour palette for `autoplot.design()`

Feature request: a palette for p-rep designs that colours only the replicated plots and leaves
everything else white. Typical p-rep structure is one or two check lines replicated ~10-15 times,
some lines replicated 2-4 times, and the bulk replicated once.

## Core idea

Colour by a **derived replication band**, not by treatment identity.

`autoplot.design()` currently maps `fill = treatment` and hands `scale_fill_manual()` one colour per
treatment level ([R/plotting.R:205-208](R/plotting.R#L205-L208)). Giving every treatment the colour of
its rep band would work visually, but the legend would then list all N lines - useless for a p-rep with
hundreds of entries.

Instead: compute a `rep_band` factor column, map `fill` to that, and keep `label = treatment`. The
legend becomes 3-4 entries. This is a contained change because the only thing blocking it is that both
plot builders hardcode `fill = .data[[trt_expr]]` ([R/plotting.R:344](R/plotting.R#L344),
[R/plotting.R:387](R/plotting.R#L387)) - adding a `fill_expr` parameter that defaults to `trt_expr`
leaves all existing behaviour untouched.

## Settled decisions

| Decision | Outcome |
|---|---|
| Buffer colour | Grey, not white - buffers must not collide with unreplicated plots |
| Rep counts source | Either derived from occurrence counts, or read from a user-named column / named vector |
| Two checks at different rep counts | Handled by `prep_breaks`, e.g. `c(2, 5, 13)` splits a 12-rep check from a 15-rep check. See caveat below |
| Colour structure | Ordinal one-hue ramp (monotone lightness), not unrelated hues |

**Caveat on separating checks.** `prep_breaks` separates checks only when their rep counts straddle a
break. Two checks *both* at 12 reps cannot be separated this way, because that is an identity question,
not a magnitude one - it would need a different encoding (e.g. treating named checks as their own
category). Worth documenting so users aren't surprised.

## API

`prep_breaks` is the mode switch; `palette` keeps its existing meaning ("the colours").

```r
autoplot(d, palette = "prep")                              # default breaks c(2, 5) -> 1 / 2-4 / 5+
autoplot(d, palette = "prep", prep_breaks = c(2, 5, 13))   # 1 / 2-4 / 5-12 / 13+
autoplot(d, prep_breaks = c(2, 5), palette = c("white", "#86b6ef", "#256abf"))

autoplot(d, palette = "prep", reps = "n_reps")             # counts from a column
autoplot(d, palette = "prep", reps = c(chk1 = 15, chk2 = 12))  # counts supplied directly
```

New arguments:

- **`prep_breaks`** - numeric vector of the lower bounds of bands 2..n. Must be strictly increasing,
  finite, and all `> 1`. Number of colours required is `length(prep_breaks) + 1`.
- **`reps`** - `NULL` (default) derives counts by tabulating the treatment column; a string names a
  column in the design holding per-row rep counts; a named numeric vector supplies counts per treatment.

`palette = "prep"` alone enables the mode with default breaks. `"prep"` must be added to the
valid-options message in [R/plotting.R:306-321](R/plotting.R#L306-L321).

**Do not auto-detect a rep column.** In field trials `rep` conventionally names the *replicate block
factor*, not the number of replicates. Silently treating it as a count would be actively wrong. Hence
the explicit `reps` argument.

## Colours

Rep count is **ordinal**, so use one hue with monotone lightness steps - "more replicated" then reads as
"darker" without consulting the legend, and it survives greyscale printing and the common forms of
colour blindness because hue isn't carrying the signal.

| Band | Colour | Notes |
|---|---|---|
| 1 rep | `#ffffff` | white |
| 2-4 | `#86b6ef` | pale blue |
| 5+ | `#256abf` | strong blue |
| buffer | `#c3c2b7` | grey; use `#e1e0d9` if it reads too heavy against the plots |

For more than three bands, interpolate along the same blue ramp
(`grDevices::colorRampPalette(c("#ffffff", "#256abf"))`) rather than introducing a second hue, so the
ordinal reading is preserved. Note the deliberate departure from the usual rule that the lightest step
should stay distinct from the page background - white is the *point* here ("nothing to look at"), and
every tile already carries a black outline, so it stays legible as a cell.

If a user wants checks to *pop* rather than sit at the top of a ramp, that's identity encoding rather
than magnitude, and they should pass their own vector, e.g. `c("white", "#86b6ef", "#e34948")`.

## Implementation steps

### 1. `.rep_bands()` - new internal in [R/plotting.R](R/plotting.R)

```r
.rep_bands <- function(df, trt_expr, breaks = c(2, 5), reps = NULL) {
  trt <- as.character(df[[trt_expr]])

  n <- if (is.null(reps)) {
    counts <- table(trt[trt != "buffer"])   # buffers must not count as a treatment
    unname(counts[trt])                     # NA on buffer rows
  } else if (is.character(reps) && length(reps) == 1) {
    df[[reps]]
  } else {
    unname(reps[trt])
  }
  n[trt == "buffer"] <- NA

  cuts <- c(1, breaks, Inf)
  labs <- .band_labels(cuts)
  out  <- cut(n, breaks = cuts, right = FALSE, labels = labs)
  out  <- factor(out, levels = c(labs, "buffer"))
  out[is.na(n)] <- "buffer"
  return(out)
}
```

`right = FALSE` gives half-open bands `[1,2)`, `[2,5)`, `[5,Inf)` = 1, 2-4, 5+.

Drop the `"buffer"` level entirely when the design has no buffers, so the legend doesn't carry a dead
entry.

### 2. `.band_labels()` - new internal

Given `cuts`, for each band `i` with `lower = cuts[i]`, `upper = cuts[i + 1]`:

- `upper == Inf` -> `paste0(lower, "+")`
- `upper - lower == 1` -> `as.character(lower)`
- otherwise -> `paste0(lower, "-", upper - 1)`

So `c(1, 2, 5, Inf)` -> `"1"`, `"2-4"`, `"5+"`; `c(1, 2, 5, 13, Inf)` -> `"1"`, `"2-4"`, `"5-12"`,
`"13+"`.

### 3. `.setup_prep_palette()` - new internal

Returns the band colours plus the buffer grey when a buffer level is present. Accepts either the
`"prep"` sentinel (use the built-in ramp) or a user vector, validating
`length(palette) == length(prep_breaks) + 1` with an error message matching the style of the existing
one in [R/plotting.R:224-230](R/plotting.R#L224-L230).

### 4. Wire into `autoplot.design()`

After the treatment factor setup ([R/plotting.R:137-161](R/plotting.R#L137-L161)) and replacing the
palette block at [R/plotting.R:163-169](R/plotting.R#L163-L169):

```r
if (prep_mode) {
  object$rep_band <- .rep_bands(object, trt_expr, prep_breaks, reps)
  fill_expr       <- "rep_band"
  colour_palette  <- .setup_prep_palette(palette, prep_breaks, has_buffer)
  legend_name     <- "Reps"
} else {
  fill_expr      <- trt_expr
  colour_palette <- .setup_colour_palette(palette, ntrt)
  if (has_buffer) colour_palette <- c(colour_palette, "white")
  legend_name    <- tools::toTitleCase(trt_expr)
}
```

Guard the existing buffer-append at [R/plotting.R:167-169](R/plotting.R#L167-L169) so it does not run in
prep mode - the buffer colour comes from the band palette there.

### 5. Text colour

The block at [R/plotting.R:171-177](R/plotting.R#L171-L177) needs its merge key changed to `fill_expr`
(`colnames(colours)[1] <- fill_expr`). `.is_light_colour()` ([R/plotting.R:507-517](R/plotting.R#L507-L517))
then works unchanged: black text on white and pale blue, white text on the dark blue. No new logic.

### 6. Thread `fill_expr` through the plot builders

Add `fill_expr = trt_expr` to `create_basic_plot()` ([R/plotting.R:325](R/plotting.R#L325)) and
`create_blocked_plot()` ([R/plotting.R:364](R/plotting.R#L364)); use it in the `geom_tile()` `fill`
aesthetic only. `geom_text()` keeps `label = .data[[trt_expr]]`, and the buffer/treatment row split
keeps using `trt_expr`.

### 7. Legend

Pass `name = legend_name` to `scale_fill_manual()` ([R/plotting.R:205-208](R/plotting.R#L205-L208)).

**Open decision:** `legend` defaults to `FALSE` ([R/plotting.R:76](R/plotting.R#L76)). In prep mode the
legend is small and genuinely useful, so defaulting it to `TRUE` when `prep_mode` is on is worth
considering - at the cost of a default that varies with another argument. Recommend defaulting to `TRUE`
in prep mode; an explicit `legend = FALSE` still wins.

### 8. Docs

- Update the `palette` roxygen ([R/plotting.R:7](R/plotting.R#L7)) to mention `"prep"`.
- Add `@param prep_breaks` and `@param reps`.
- Add a `@examples` block building a small p-rep (a check at 6 reps, a few lines at 2-3, the rest at 1).
- `devtools::document()`.

### 9. Validation

Add `.verify_*` style checks in [R/verify_utils.R](R/verify_utils.R) or inline, consistent with the
surrounding style:

- `prep_breaks` numeric, strictly increasing, finite, all `> 1`.
- `reps` is `NULL`, a length-1 character naming an existing column, or a named numeric vector covering
  every non-buffer treatment.
- Colour vector length matches band count.

## Testing

Add to [tests/testthat/test-plotting.R](tests/testthat/test-plotting.R):

- `.rep_bands()` assigns the right band for boundary counts (1, 2, 4, 5) under default breaks.
- `.band_labels()` produces `"1"`, `"2-4"`, `"5+"` and the 4-band variant.
- Buffers land in the `"buffer"` band and are excluded from the counts.
- `reps` as a column name and as a named vector give the same result as derived counts when consistent.
- Invalid `prep_breaks` and mismatched colour-vector length error.
- Non-prep calls are unchanged (existing snapshots must still pass).
- One vdiffr snapshot of a small p-rep design.

Run per [CLAUDE.md](CLAUDE.md):

```sh
devcontainer-exec-here R -e "devtools::test()"
```

**vdiffr caveat:** use `devtools::test()`, not `pkgload::load_all()` + `testthat::test_dir()` - that
combination makes vdiffr treat `_snaps/*.svg` as orphaned and delete them.

## NEWS entry

Under **Minor Changes** in the current `speed 0.0.11` section:

```
- `autoplot()` gains a p-rep palette via `palette = "prep"`, colouring plots by replication band
  (unreplicated plots white, buffers grey) rather than by treatment. Bands are controlled with
  `prep_breaks`, and rep counts can be supplied with `reps`.
```

## Known limitations to note in the docs

- A real p-rep has enough entries that `geom_text()` labels are illegible at the default `size = 4`.
  Pre-existing, but this feature makes it obvious immediately - there is currently no way to turn labels
  off. A `labels = FALSE` argument would be a natural companion change, but is out of scope here.
- Rep counts derived by tabulation are wrong for MET designs where a line appears once per site. The
  `reps` argument is the escape hatch; consider counting within `grid_factors$by` in a later pass.
