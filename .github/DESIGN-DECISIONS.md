# Settled design decisions

Points that have been raised, discussed, and **deliberately closed without a
change**. They are not bugs and not backlog - they are questions with an answer,
recorded so the same argument is not re-made every time someone new (or some
review tool) reads the code and notices the same thing.

Work that *is* going to happen belongs in
[GitHub issues](https://github.com/biometryhub/speed/issues), not here.

**Format:** one entry per decision. Say what was proposed, what was decided, and
why - the *why* is the part that stops it coming back. Date it, and link the
commit or discussion if there is one. Add new entries at the top.

---

## `linked_cols` is not recorded in the design output

*2026-08-18*

`metadata$per_level` records every other per-level argument - `swap`,
`spatial_factors`, the weights, `iterations`, `start_temp`, `cooling_rate`,
`obj_function` - so `linked_cols` being absent was raised as convention drift by
omission, along with the related question of whether `summary()` should report
which columns travelled.

**Decided: record neither.** It is irrelevant to how the design was optimised -
linked columns take no part in scoring - and the user knows which columns they
linked, because they named them. Do not add `linked_cols` to `per_level`, and do
not add a line for it to `summary()`.

## Carrying a nested treatment with `linked_cols` is a happy accident

*2026-08-18*

`linked_cols` covers two things that read differently: passive companion columns
that never get optimised, and linking a *later* level's `swap` column at an
earlier level so a child treatment rides with its parent. Raised as a question of
whether one argument should mean both, since the second makes the user responsible
for level order or they get an error.

**Decided: leave as one argument.** The intention is moving columns along with the
swap column; that this also happens to move a column nested under another is a
welcome side effect rather than a second feature needing its own vocabulary. The
point about the two styles is understood and accepted.

## A frozen level reports one iteration run, having proposed none

*2026-08-18*

`iterations_run` derives from `length(scores)`, and the frozen branch seeds
`scores[1]` so the level's final score is recorded. `summary()` therefore prints
`Iterations: 1 / 10000 (no swaps possible)` for a level that proposed no swaps,
and `total_iterations` gains one per frozen level.

**Decided: leave it.** Separating the score series from an iteration count is not
worth it for this case; the off-by-one is accepted.

## `stop_reason` is public API from 0.0.11

*2026-08-17*

The four literals `"optimal"`, `"no_improvement"`, `"frozen"` and `"iterations"`
are documented in `?summary.design` and announced in `NEWS.md`, so they are a
published vocabulary rather than an implementation detail. Raised as a chance to
pull them back to internal before the release.

**Decided: leave as is, public.** Renaming any of the four now needs a
deprecation cycle. `"frozen"` in particular is jargon that appears nowhere else
in the package - it prints as "(no swaps possible)" - and stays anyway.

## `exchange_linked()` swaps column by column, and the comment saying why stays

*2026-08-17*

`exchange_linked()` loops over `linked_cols` rather than using a single data
frame `[<-`, and carries a comment saying the data frame form benchmarks slower.
The comment was flagged as defending a decision nobody would question.

**Decided: keep it.** The objection *would* be raised - the vectorised
`design[plots_1, cols] <- design[plots_2, cols]` is the obvious idiom, and a
reader who does not know it was measured will reach for it as a tidy-up. A
comment recording that the obvious form was tried and lost is exactly the kind
worth keeping; "nobody would question this" was wrong.

## `*/vignette-cache` in `.gitignore` is single-level on purpose

*2026-08-17*

Flagged as inconsistent with the neighbouring `**/*.quarto_ipynb`.

**Decided: leave it.** The pattern was chosen to match where the cache actually
lands on another development machine. Do not widen it to `**/`.

## `.claude/` stays fully ignored

*2026-08-17*

`CLAUDE.md` is tracked, so ignoring all of `.claude/` also excludes any shared
`settings.json`, skills or agents. Raised as a question of whether the narrower
`.claude/settings.local.json` was meant.

**Decided: keep the whole directory ignored** for now. Revisit only if there is
something in there worth sharing with the other maintainers.

## `linked_cols` sits between `swap_all` and `optimise` in `speed()`

*2026-08-17*

Inserting a new argument mid-signature shifts positional matching for any caller
passing arguments positionally past `swap_all`.

**Decided: leave it where it is.** It reads better next to `swap_all` than
appended to the end, and the package is pre-1.0 with no known positional callers
that deep.

## `.level_value()` does not guard against level names colliding with argument fields

*2026-08-17*

`create_speed_input()` treats a named list as per-level when
`all(names(value) %in% levels)`. Raised because `grid_factors` and
`optimise_params` are themselves named lists of their own fields, so levels
named `dim1` and `dim2` would take one element of `grid_factors` each and get a
silently wrong design. An `own_fields` guard was written and then **reverted**.

**Decided: no guard.** Mis-splitting `optimise_params` needs nine levels named
`swap_count`, `swap_all_blocks`, `adaptive_swaps`, `start_temp`, `cooling_rate`,
`random_initialisation`, `adj_weight`, `bal_weight` and `stop_at_optimal`, since
`optim_params()` always returns all nine. Mis-splitting `grid_factors` needs
levels named exactly `dim1` and `dim2`. Both had to be manufactured to
demonstrate, which is the tell.

The plausible mistake in this area is the opposite one: a typo in a level name.
`iterations = list(wp = 5, spp = 7)` against levels `wp`/`sp` fails
`all(names %in% levels)`, so the whole list is assigned as `iterations` to every
level and dies with `invalid 'length' argument`. That is a validation gap
tracked in [#126](https://github.com/biometryhub/speed/issues/126), not an
argument for guarding the collision.

## `linked_cols` is absent, not `NULL`, in a resolved level that does not set it

*2026-08-17*

`.DEFAULT` has no `linked_cols` entry, so `%||% .DEFAULT[[arg]]` assigns `NULL`
and drops the element. A resolved `optimise` level therefore carries nine fields
rather than ten, and `str()` on one will not show `linked_cols`.

**Decided: deliberate.** `opt$linked_cols` reads `NULL` either way, which is what
every consumer wants. Defaulting it to `character(0)` to keep the field present
buys nothing but a visible empty slot.

## The neighbour generators keep guards that `speed()` can no longer trigger

*2026-08-17*

`speed_hierarchical()` passes `swappable_groups()$swappable` into
`generate_neighbour()`, so every group it proposes a swap in holds at least two
distinct treatments. That makes several guards unreachable *from `speed()`*:
`length(block_indices) >= 2` and the `to_be_swapped <- NULL` skip path in
`generate_single_swap_neighbour()`; `nrow(group_data) >= 2`,
`length(group_treatments) < 2` and `length(replications) == 0` in
`generate_multi_swap_neighbour()`.

**Decided: keep all of them.** `swappable` defaults to `NULL`, meaning every
group, and that default is part of the generators' contract rather than an
accident of the current caller. Removing the guards would leave the functions
correct for `speed()` and quietly wrong for anything calling them directly.

Note the test evidence is uneven. `generate_multi_swap_neighbour()` is called
directly on the `NULL` default four times in
`tests/testthat/test-design_utils.R`, so its guards are exercised.
`generate_single_swap_neighbour()` and `generate_neighbour()` have no direct
caller, so the guards named first above - `length(block_indices) >= 2` and the
`to_be_swapped <- NULL` skip path - rest on the contract alone. Closing
[#116](https://github.com/biometryhub/speed/issues/116) is what would even this
up; until then, do not read the guards' survival as evidence they are covered.

## The frozen check is hoisted above the search loop, and must stay there

*2026-08-17*

Whether a level can move anything is settled before the search: a level's swaps
permute treatments within a group, so they change neither the number of distinct
treatments nor their replication counts. The check therefore sits above the loop
rather than inside it.

**Decided: keep it hoisted**, and note that it is now load-bearing rather than
merely tidier. The generators sample from `swappable`, which is `character(0)`
for a frozen level, and `sample(character(0), 1)` errors. Moving the check back
inside the loop turns a frozen level from a clean stop into an error.

For the same reason the loop bound is `seq_len()`, not `1:`: skipping the loop
needs a zero-length sequence, and `1:0` is `c(1, 0)`, which would run the body
once. The frozen tests in `tests/testthat/test-design_utils.R` catch a
regression here, but by erroring rather than by saying what broke.

## `summary()` handles designs saved before a metadata field existed

*2026-08-17*

`print.summary.design()` falls back when `stop_reason` is absent, and
`summary.design()` uses `pm$stop_reason %||% NA_character_`. Raised as possible
over-engineering, since `stop_reason` is new in 0.0.11 and the fallback branch is
unreachable from any design this version produces.

**Decided: keep.** `summary()` shipped in 0.0.10, so design objects without the
field exist in users' saved work. The same file already does
`pm$optimal_score %||% NA_real_` and `pm$final_score %||% NA_real_` - this is the
established pattern for reading `per_level` metadata, not a one-off.

## `linked_cols` is the right name for the argument

*2026-08-17*

Considered and settled during `feature/keep-with`; renamed from "carried" to
"linked" at c2b3ab4. The `_cols` suffix is not an abbreviation slip either -
`add_buffers(treatment_cols = )` is the existing precedent for a
character-vector-of-column-names argument.

**Decided: do not re-propose alternatives** (`keep_with`, `carry`, `linked_columns`).

## Feature-scoped test files are a convention, not drift

*2026-08-17*

`tests/testthat/test-linked_cols.R` does not correspond to a file in `R/`.

**Decided: fine.** `test-grid-orientation.R` and `test-optimal_score.R` are the
same shape. A test file may be scoped to a feature or to a source file; both are
in use deliberately.

## Buffers must not separate plots for scoring

*2026-08-10*

A buffered design scores exactly as the same design unbuffered - plots either
side of a buffer **are** neighbours, because a buffered trial is analysed with
the buffer plots excluded and the model then treats the rest as contiguous. A
design metric should describe the layout the analysis sees, not the field.

Implemented by recording `add_buffers()`' coordinate displacement in
`metadata$buffer` and inverting it in `.drop_buffer_rows()`.

**Rejected: ranking coordinates** downstream (`match(x, sort(unique(x)))`). It is
only the inverse of the displacement by guesswork, and cannot tell a buffer from
a genuine hole - a road, an irregular edge - which grid construction must
preserve. Reading coordinates raw in grid construction is therefore correct and
is **not** a bug.

Known hazard: the buffer test comment in `tests/testthat/test-summary.R` states
the opposite rule.

## `summary()` will not report the raw A-value

*2026-08-06*

Reporting the average pairwise variance alongside the A-efficiency was proposed
and **declined** - not worth the added output.

The related upper bound on the average efficiency factor *is* wanted, as its own
piece of work. When it lands, report it as "% of upper bound", never "% of
optimal": `A/UB = 1` proves A-optimality, but `A/UB < 1` does not prove
sub-optimality, since the bound may be unattainable.
