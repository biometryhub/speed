# Review notes: the `summary()` work (merged on `main`)

**Scope:** `R/summary.R` and `tests/testthat/test-summary.R`, as merged to `main` in PR #98
(`feature/summary`, commit `a36d302`).

**Companion files** — one per workstream, so each can become its own small PR:

| File | Workstream |
|---|---|
| `REVIEW-NOTES.md` | `feature/incidence` (PR #97) — `R/incidence.R`, the two new exports |
| **this file** | the merged `summary()` work — `R/summary.R` |
| `REVIEW-NOTES-OTHER.md` | grid construction / core metrics — `bugfix/grid-orientation` |
| `REVIEW-NOTES-EFFICIENCY.md` | efficiency-factor statistics — branch `feature/a-optimality` |
| `REVIEW-NOTES-PR91.md` | PR #91 `info-objective` |

Package-level issues that outlive any branch are in `KNOWN_ISSUES.md`.

**Last verified:** 2026-08-07, R 4.6.1, `pkgload::load_all()`, `bugfix/grid-orientation` at `95c48fb`.
All numbers measured against brute-force ground truth, not inferred. Resolved findings are deleted
rather than annotated — see git history and the `NEWS.md` entries.

> ✅ **S1, S2, S3 and S4 are all resolved on `bugfix/grid-orientation`**, not on a branch of their own.
> `.neighbour_balance()` reads coordinates via `build_design_matrix()`, its self-fulfilling test was
> rewritten against hand-derived values, and S4's underlying failure (G6) turned out to be fixed as a
> side effect. **Do not create `bugfix/summary-neighbour-balance`** — there is nothing left for it.
>
> ✅ **G10 is withdrawn — nothing to read before merging.** It said `build_design_matrix()` must *rank*
> its coordinates; that was reversed. Ranking is only the inverse of the displacement `add_buffers()`
> applies, so the undo moved to where the displacement is created (`feature/buffers`), and grid
> construction keeps coordinates **raw**. The convention itself is unchanged and is now stated in
> `KNOWN_ISSUES.md` #1: a buffered design must score exactly as the same design unbuffered.
>
> ✅ **S5 is closed, and so is half of S6** — both were faces of the missing rank check, which landed as
> G14 on `bugfix/grid-orientation`. `calculate_efficiency_factor()` now refuses a design whose treatment
> contrasts are not estimable, so a disconnected design reports a reason instead of a plausible number.
> **Verified 2026-08-07** on a design disconnected by construction: `Connected: FALSE` and
> `Efficiency: unavailable - treatment contrasts not estimable given row + col`, where it previously
> printed both a DISCONNECTED warning and a healthy-looking value.
>
> ⬜ **Open here: S6(1) only** — `summary()` still errors outright on a single-row or single-column
> design, from `.design_connectedness()`, which nothing on that branch touched.

---

## 1. Summary

- 🟠 **S6(1)** — `summary()` **errors outright** on a single-row or single-column design.

**What the summary work got right, and which other workstreams depend on:**

- `design$metadata` — carries `row_column`, `col_column`, `grid_by`, and per level `swap`,
  `spatial_cols`, `final_score`, `final_components`. This is what lets the incidence functions resolve
  column names instead of hardcoding them (see I1 in `REVIEW-NOTES.md`). It removed a whole finding from
  that branch's review.
- `.drop_buffer_rows()` — the buffer-exclusion helper the incidence functions should reuse (I2).

Both are good and should not change; noted here so the dependency is visible if `R/summary.R` is
refactored.

---

## 2. Findings

### S6(1) 🟠 `summary()` errors on a single-row or single-column design

Found while probing G14 on `bugfix/grid-orientation`. Not grid-construction work — that branch does not
touch `.design_connectedness()` — so it belongs with the rest of `summary()`.

**Measured 2026-08-07**, 3 treatments in each shape, after the G14 rank gate landed:

| Design | `connectedness = TRUE` (the default) | `connectedness = FALSE` |
|---|---|---|
| 1×6 | **Error:** `contrasts can be applied only to factors with 2 or more levels` | efficiency withheld, with a reason |
| 6×1 | **Error:** same | efficiency withheld, with a reason |
| 2×3 | efficiency 0.7500 | efficiency 0.7500 |

Thrown from `.design_connectedness()` via `model.matrix()`: a spatial factor with one level has no
contrasts. A one-row trial is a legitimate design, so this wants the same treatment G12 gave the grid
metrics — report `available = FALSE` with a reason rather than propagating the error and taking the whole
of `summary()` down with it.

The second half of this finding — an impossible efficiency of `1.5` once the error was suppressed — is
**closed**: the G14 rank gate refuses it and `summary()` reports the reason. Fixing the error is now safe
on its own, where before it would have converted a loud failure into a quiet wrong number.

---

## 3. Plan

One action, `R/summary.R` only, no branch of its own: report a reason instead of erroring when a spatial
factor has a single level.

### Out of scope

- Grid construction, MET, and the efficiency rank gate — `REVIEW-NOTES-OTHER.md`.
- The A-efficiency upper bound — `REVIEW-NOTES-EFFICIENCY.md` (E1).
- The buffered-design neighbour counts — `feature/buffers` rewrites that test; convention in
  `KNOWN_ISSUES.md` #1.
- Everything about `R/incidence.R` — `REVIEW-NOTES.md`.

---

## 4. Corrections that still matter

| Earlier claim | Corrected |
|---|---|
| **Suggested branch: `bugfix/summary-neighbour-balance` off `main`** | **Never created, and shouldn't be.** S1/S2/S3 were folded into `bugfix/grid-orientation` once `build_design_matrix()` existed there; S4 resolved itself. |
| S3: the `KNOWN_ISSUES` #1a fix silently chose that *plots either side of a buffer are neighbours*, and that choice is questionable | **The behaviour was right; only the silence was wrong.** The convention is now stated in `KNOWN_ISSUES.md` #1 — a buffered trial is analysed with the buffers dropped, so the analysis treats those plots as adjacent. `main`'s behaviour matched the analysis; it just wasn't stated. |
| The mechanism would be *ranked* coordinates in `build_design_matrix()` (D6, then G10) | **Reversed; grid construction keeps coordinates raw.** Ranking is exactly the inverse of the displacement `add_buffers()` applies, so it was an undo applied by inference in the wrong place — and one that collapses genuine physical gaps as collateral. `feature/buffers` records the displacement and inverts it at its source instead. The convention above is unaffected; only the mechanism changed. |
| S5 needs `.efficiency_factor()` to consult `connectedness` before printing a value | **Not needed in the end.** The rank gate (G14) refuses the value at source, so there is nothing to suppress at print time and no ordering dependency inside `summary.design()` to respect. A presentation-layer fix would have papered over a metric that should not have produced a number. |
| `KNOWN_ISSUES` #1a: neighbour balance was only half fixed on `feature/summary` | **Both halves resolved, one still inbound.** The fill-order half is fixed (S1); the gap-collapsing half is a stated convention (`KNOWN_ISSUES.md` #1) whose restoration arrives with `feature/buffers`. Until then a `"row"` buffer still changes the counts — measured 9 → 0 on a clumped 4×3 — and the comment at [test-summary.R:299-305](tests/testthat/test-summary.R#L299-L305) states the *opposite* rule. |
