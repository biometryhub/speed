# Term-based `swap`: unify simple and factorial designs through a shared terms normaliser

Status: **draft plan — not yet implemented.**

## What

Generalise the `swap` argument of `speed()` so that, in addition to a single
column name, it accepts:

- a **one-sided formula** — `~ N`, `~ N + P`, `~ N * P`, `~ N / P`, `~ N * P:K`,
  `~ (N + P + K)^2`, …; and
- a **character vector of term labels** — `"N"`, `c("N", "P")`,
  `c("N", "P", "N:P")`.

All three shapes collapse into one internal representation — an ordered set of
**terms** — and every built-in objective function becomes a single fold over
that set. A simple design is just the one-term case; a factorial is the
many-term case; there is no separate code path.

This is driven by a new **shared design utility**, `normalise_terms()`, built on
`stats::terms()`, so that any formula-valued argument (starting with `swap`, and
later `spatial_factors`) parses formulas the same way.

## Why

Factorial designs today require the user to hand-paste factor levels into a
single string column (e.g. `"A1-B2"`) and pass `obj_function =
objective_function_factorial`. That objective re-splits every treatment string
with `stringi` on **every** SA iteration (`R/metrics.R`, `objective_function_factorial`),
rebuilds temporary columns, and recomputes all balance tables and adjacency
matrices from scratch — even though the factor decomposition never changes
during optimisation and a swap only touches two plots. It also ignores the
incremental `current_score_obj` / `swapped_items` contract that
`objective_function_piepho` honours, and it fires the two-treatment warning in
`objective_function` (`R/metrics.R`) on every call for any 2-level factor (i.e.
every 2^k design).

More fundamentally, the thing that makes a factorial design distinct — optimising
the **main effects and the interaction(s) simultaneously off a single, linked
permutation** — is currently bolted on rather than modelled. The permutation
must move every factor together (permuting the interaction moves the main
effects), but the *score* must see each effect separately. Modelling `swap` as a
set of terms captures exactly this: one swap unit, many scored terms.

## The model

Two orthogonal roles, wired together at normalisation time:

- **Swap unit** — the single column the SA loop permutes. It is the full crossing
  of all base factors appearing anywhere in the terms (`all.vars` of the
  formula/labels), built once as an internal combined column. This enforces the
  linkage constraint structurally: `N`, `P`, and every interaction move together
  because they are all projections of this one column. `generate_neighbour()`,
  `shuffle_items()`, `random_initialise()` and the SA loop stay untouched — they
  still swap exactly one column.
- **Scored terms** — whatever the formula/vector lists, expanded via
  `stats::terms()`. Each term is a column to be scored (a single-variable term is
  that factor column; an interaction term is the crossing of its variables). The
  score is a weighted sum over terms.

A character vector **is** the term-label list, using the same vocabulary
`terms()` emits; a formula is sugar that expands to it. So `~ N * P` and
`c("N", "P", "N:P")` are two spellings of the same set, and a scalar string is
the one-term case.

### Normalisation table

| `swap` input | scored terms | base factors (`all.vars`) | swap unit (permutes) |
| --- | --- | --- | --- |
| `"N"` | `N` | N | N |
| `~ N` | `N` | N | N |
| `~ N + P` | `N, P` | N, P | N×P |
| `c("N", "P")` | `N, P` | N, P | N×P |
| `~ N * P` | `N, P, N:P` | N, P | N×P |
| `c("N", "P", "N:P")` | `N, P, N:P` | N, P | N×P |
| `~ N : P` | `N:P` | N, P | N×P |
| `~ N / P` | `N, N:P` | N, P | N×P |
| `~ N * P:K` | `N, P:K, N:P:K` | N, P, K | N×P×K |

Note that `~ N + P`, `~ N * P` and `~ N : P` share the **same swap unit** (N×P)
and differ only in the scored-term set. The formula operators therefore have a
precise, R-idiomatic meaning here:

- `+` — add main effects only.
- `*` — main effects **and** their interaction (the factorial crux).
- `:` — the interaction term only (today's trivial "optimise the interaction" case).
- `/` — nesting: `N / P` scores `N` and `N:P` but not `P` marginally (correct when
  P's levels are only meaningful within a level of N).
- `^`, `-`, etc. — all fall out of `terms()` with no operator-specific code.

## Changes

### 1. New shared utility — `normalise_terms()` and `build_term_columns()`

New file `R/terms.R` (internal, exported later if useful):

- `normalise_terms(x)` — pure parse, no data required. Accepts a formula, a
  character vector, or a scalar string and returns a small structure:
  - `terms` — canonicalised term labels (variables within each interaction sorted
    to a stable order so `"P:N"` and `"N:P"` agree, matching `terms()` output);
  - `vars` — base factor names (`all.vars`);
  - `orders` — interaction order per term (from `attr(terms(f), "order")`), so
    by-order weighting is available if wanted later.
  This is the general design utility. It is deliberately data-agnostic so it can
  also back `spatial_factors` and any future formula argument.
- `build_term_columns(data, spec)` — given `data` and a `normalise_terms()`
  result, builds:
  - the **combined swap column** = `interaction(data[spec$vars], drop = TRUE)`
    with an internal, generated name (`.speed_terms_<id>`), following the existing
    `dummy_<timestamp>` pattern in `speed()` (`R/speed.R`);
  - a **projection lookup** per term — a named vector mapping each combined-column
    level to that term's level — so the objective can materialise any term's
    per-plot values by a vectorised gather, with **no per-iteration string ops**.

### 2. `speed()` dispatch (`R/speed.R`)

Extend the top-of-function shape detection. Because a formula and a length-`>1`
character vector both currently **error** in validation (`verify_column_exists`
runs `if (!(col %in% names(data)))`, which fails on a length-`>1` condition or a
formula), adding them is strictly additive — no existing valid call is shadowed.
The dispatch becomes:

```r
if (inherits(swap, "formula"))                    # factorial / general (formula)
else if (is.character(swap) && length(swap) > 1)  # factorial / general (term vector)
else if (is.list(swap) && !is.null(names(swap)))  # hierarchical (unchanged)
else                                              # simple (unchanged)
```

`spatial_factors` is also a one-sided formula but is a separate named argument, so
there is no formula-vs-formula confusion.

### 3. `create_speed_input()` (`R/utils.R`)

For a formula/vector `swap`, run `normalise_terms()` and `build_term_columns()`,
then store on the per-level `optimise` entry:

- `swap` = the internal combined-column name (the swap unit);
- `terms` = canonical term labels;
- `projections` = per-term lookups;
- `term_weights` = resolved weights (see **Weights**).

For a scalar `swap` the entry is unchanged except `terms` defaults to the swap
column itself and `projections` is `NULL` — i.e. the current simple design,
byte-for-byte.

### 4. Objective functions — one fold over terms (`R/metrics.R`)

`objective_function()` gains optional `terms` / `projections` / `term_weights`
arguments (all defaulting to the single-column behaviour) and becomes:

```r
objective_function <- function(layout_df, swap, spatial_cols, ...,
                               terms = swap, projections = NULL,
                               term_weights = NULL) {
  swap_vals <- as.character(layout_df[[swap]])
  per_term <- vapply(terms, function(t) {
    term_col <- if (is.null(projections)) swap_vals else projections[[t]][swap_vals]
    score_one_column(term_col, layout_df, spatial_cols, ...)  # existing adj + bal helpers
  }, numeric(1))
  w <- term_weights %||% rep(1, length(terms))
  list(score = sum(w * per_term),
       components = stats::setNames(w * per_term, terms))
}
```

- `length(terms) == 1`, `projections = NULL` → identical to the current simple
  objective.
- The per-term work reuses `calculate_adjacency_score()`
  (`R/calculate_adjacency_score.R`) and `calculate_balance_score()`
  (`R/metrics.R`) unchanged; the `vapply` is the vectorisation over terms.
- `components` now has **one entry per term**, giving `summary.design()` a genuine
  per-term decomposition rather than the coarse `main` / `interaction` split.
- The two-treatment warning is hoisted out of the per-iteration path into a
  one-time validation, so it stops firing every call.

`objective_function_piepho()` (`R/metrics.R`) gets the same `terms` fold so
neighbour-balance / even-distribution can be scored per term too.

### 5. SA loop wiring (`R/speed.R`, `speed_hierarchical`)

The loop already threads arbitrary named arguments to `obj_function`. Add
`terms = opt$terms`, `projections = opt$projections`,
`term_weights = opt$term_weights` to the objective calls (initial score, per
iteration, and the final per-level metadata call). No change to
`generate_neighbour()` — it still permutes the single combined column, and
`swapped_items` remain combined-column labels, which the objective projects per
term for incremental updates.

### 6. Return value / write-back

Before returning, project the optimised combined column back onto the **base
factor columns** so the returned `design_df` carries correctly permuted `N`, `P`,
… ready to plot per factor directly (removing the manual `strsplit` the factorial
vignette currently requires). The internal `.speed_terms_<id>` column is removed
before returning, mirroring how the `dummy_group` column is stripped in `speed()`.

### 7. `objective_function_factorial()` — deprecate to a thin wrapper

Reimplement it as a wrapper that string-splits a pasted column into base-factor
columns and delegates to the unified `objective_function` with the derived
`terms`. Keep it for back-compat (users who only have a pasted string column) and
document the formula/vector path as preferred. `main_weight` / `interaction_weight`
map onto `term_weights` (mains vs order-`>=2` terms).

### 8. Hierarchical composition (`R/verify_utils.R`)

Allow a formula/vector element **inside** a named-list `swap`, e.g.
`swap = list(wp = "wp_trt", sp = ~ N * P)` (a factorial subplot treatment). The
outer detection stays `is.list && names`; each element is then run through the
same per-element normalisation. `.verify_hierarchical_inputs()` currently does
`swap[[level]] %in% names(data)` and must learn the same per-element dispatch.

## Key design decisions

- **Swap unit vs scored terms are separate concerns.** The permutation is always
  over the full crossing (linkage); scoring is over the listed terms. This is the
  single idea that makes simple and factorial designs the same code.
- **Everything reduces to a term-label vector.** Formula → `terms()` → labels;
  character vector → labels directly; scalar → one label. No implicit interaction
  expansion — `c("N", "P")` is mains only; ask for `c("N", "P", "N:P")` or
  `~ N * P` to include the interaction.
- **The objective is a fold, not a special case.** One implementation, driven by a
  term vector of length `>= 1`; `objective_function_factorial` is retired to a
  compatibility wrapper.
- **Projections precomputed once.** No `stringi` in the loop; incremental scoring
  via `swapped_items` is preserved and, for factorials, newly gained.
- **Shared normaliser.** `normalise_terms()` is written data-agnostic and general
  so `spatial_factors` (and future formula args) can adopt it; `swap` is the first
  consumer.
- **Reserved grammar.** `:` denotes interaction in a term label; base factors must
  be real columns in `data` (validated via `all.vars`, as `spatial_factors`
  already is), while interaction terms are derived, never required as columns.

## Weights

- **Default: equal.** Every term gets weight `1`.
- **Override: a named per-term map**, e.g.
  `term_weights = c("N" = 2, "N:P" = 0.5)`, merged over the equal-weight default
  so unlisted terms stay `1`. Keys use the canonical term labels from
  `normalise_terms()`.
- Passed through `speed(..., term_weights = ...)` into the objective (the same
  channel `interaction_weight` uses today). `adj_weight` / `bal_weight` remain the
  per-level SA weights in `optim_params()` and apply within each term's
  `objective_function` scoring.

## Edge cases

- **Single factor** (`~ N`, `"N"`): one-term fold, swap unit = `N`, identity
  projection — exactly the current simple design.
- **Factor level text containing the old separator**: no longer a problem — factors
  are real columns and the combined column is built by `interaction()`, not by
  string parsing.
- **Interaction on unique combinations**: adjacency of a single-rep interaction
  term is ~0 and only bites when combinations replicate — correct, and automatic.
- **`relationship` matrix + terms**: initially the relationship applies to the
  combined/base column as today; per-term relationships are out of scope for the
  first pass (documented).
- **Canonicalisation**: `c("P:N")` normalises to `"N:P"` so weight-map keys and
  `components` names are predictable.

## Backward compatibility

- Scalar `swap` and named-list `swap` behave exactly as now.
- All existing objective-function callers are unaffected (`terms` defaults to the
  swap column, `projections = NULL`).
- `objective_function_factorial` continues to work via the wrapper.
- Formula / length-`>1`-vector `swap` were errors before, so no valid call changes
  meaning.

## Tests

- `test-terms.R` (new): `normalise_terms()` over formulas, vectors, scalars;
  operator expansion (`+`, `*`, `:`, `/`, `^`, `-`); canonicalisation; error
  messages for unknown base factors.
- `test-objective_functions.R`: term-fold equals the sum of per-term
  `objective_function` scores; `length(terms) == 1` matches the pre-change score;
  `term_weights` applied; `components` names/values; no two-treatment warning in
  the loop.
- `test-speed.R`: `speed(swap = ~ N * P)` equals `speed(swap = c("N","P","N:P"))`;
  base factor columns correctly permuted on return; combined column stripped;
  seeded reproducibility; factorial nested at a hierarchy level.
- Parity test: unified path vs current `objective_function_factorial` on the
  vignette design (same score, faster).

## Docs

- Rewrite `vignettes/factorial.qmd` around the formula/vector `swap`; drop the
  manual `strsplit` round-trip; explain `+` vs `*` vs `:` vs `/`.
- Roxygen for `swap` (new shapes), `normalise_terms()`, `term_weights`, and the
  `objective_function` term arguments; `devtools::document()`.
- `NEWS.md` bullet.
- Note in `CLAUDE.md` architecture section: `swap` shapes now include
  formula/term-vector; simple and factorial share one objective fold.

## Out of scope (for now)

- Independently randomised factors (e.g. strip-plot, where N and P permute on
  separate spatial supports) — that is the existing hierarchical/named-list path,
  not this linked-factorial feature.
- Per-term `relationship` matrices.
- Migrating `spatial_factors` onto `normalise_terms()` (the utility is written to
  allow it; the migration itself is a separate, optional follow-up).

## Phasing

Each phase is independently testable and backward-compatible:

1. `normalise_terms()` + `build_term_columns()` and their tests (pure; no
   behaviour change).
2. Term fold in `objective_function` with defaults preserving current behaviour +
   tests.
3. `speed()` / `create_speed_input()` dispatch and wiring for formula/vector
   `swap`; combined-column build, projections, `term_weights`, base-factor
   write-back.
4. Generalise `objective_function_piepho`; retire `objective_function_factorial`
   to a wrapper.
5. Hierarchical composition (formula element within named-list `swap`) +
   `.verify_hierarchical_inputs` update.
6. Docs: factorial vignette rewrite, roxygen, `NEWS.md`; optional
   `spatial_factors` migration.
