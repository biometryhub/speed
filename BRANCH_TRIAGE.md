# Branch triage

Survey of every branch as at 2026-08-11, from the review recorded in
[PIEPHO_ALIGNMENT_PLAN.md](PIEPHO_ALIGNMENT_PLAN.md) (see its *Consolidation* section).

**17 branches besides `main` and `gh-pages`. Nine contain no package code at all.** Acting on the
groups below takes the count to 8, collapsing into roughly five real lines of work.

Ahead/behind counts are `git rev-list --left-right --count main...<branch>`; "code" means files under
`R/` or `tests/`.

---

## 1. Drop now

| Branch | Evidence | Reasoning |
|---|---|---|
| `origin/feature/aco` | 0 ahead, 580 behind; listed by `git branch --merged main` | Fully merged. Deleting loses nothing. |
| local `feature/incidence` | 102 behind / 11 ahead | `origin/feature/incidence` (49 behind / 16 ahead) strictly supersedes it. Delete local, track origin. |

> ⚠️ **`info-objective` is local-only** - no `origin/` counterpart, 146 behind. It is the sole copy of
> `R/objectives.R` (434 lines) and its 378-line test file. **Push it before anything else.**

---

## 2. Nine branches that are documents, not branches

**Plan-only** - one markdown file, one commit, zero code:
`feature/prep-palette`, `bugfix/split-plot-convergence`, `feature/incremental-scoring`,
`feature/update-factorial`.

**Script/doc-only** - nothing under `R/` or `tests/`:

- `bugfixe/summary` - 4 review notes + `summary-feature-demo.R`, 42 behind (note the typo in the name)
- `feature/benchmarking` - `benchmarking.R`, 9 commits, 79 behind
- `testing-randomisations` - `testing_randomisations.R`, 5 commits, 210 behind
- `feature/interface` - a `.qmd` + `.pdf`, 6 commits, 308 behind

Plus `feature/aco` from group 1.

**Reasoning.** These are a backlog and a scratch drawer stored as branches, which is why they are
invisible: a plan on a branch is read by nobody and drifts silently behind `main`. Merge the documents
to `main` under `plans/`, move the scripts to an `.Rbuildignore`d `dev/`, or convert them to issues.
Zero code risk either way.

`bugfixe/summary` needs triage before deletion, but is at least partly superseded - its KNOWN_ISSUES
item 3 ("`initialise_design_df()` fills `items` down columns, and nothing says so") is now documented
on `main` at [R/metrics.R:920-921](R/metrics.R#L920-L921).

---

## 3. Merge candidates - the same issue in more than one place

### 3a. Design-metrics consolidation → one branch

`origin/feature/incidence` + `info-objective` + `origin/feature/a-optimality`.

All three add treatment-matrix / concurrence machinery already covered by the plan's *Consolidation*
section. Rebase them onto the stage-1 count-matrix primitive rather than landing them separately.

**The trap, and the reason this group matters most:** `feature/incidence` touches **only**
`R/incidence.R` and two test files - zero textual overlap with `R/metrics.R`. It merges with no
conflict at all while duplicating `calculate_nb()`'s edge enumeration semantically. Git will never
warn you. That is precisely how the package reached seven copies of one computation.

**Verified, because the opposite is the natural assumption:** these branches carry a pre-structural-
zeros working copy of `metrics.R`, but merging them would *not* revert `calculate_nb()`.
`git diff --numstat main...origin/feature/a-optimality` gives `401 0` on `R/metrics.R` - pure append -
and the diff contains no line matching `calculate_nb`, `sorted_pairs` or `pair_mapping`.
`info-objective` adds `R/objectives.R` as a new file (`434 0`). The real conflict surfaces are small:
`a-optimality`'s `R/speed.R` (`6 5`, against 145 commits of drift) and `info-objective`'s `DESCRIPTION`
(`18 10`). The work is reconciling duplicated functionality, not untangling a merge.

### 3b. `feature/incremental-scoring` → fold into `bugfix/ed`

Its 328-line plan is *about* the count-matrix work `bugfix/ed` now carries. One document, no code,
same subject - it should not be a separate branch.

### 3c. `bugfix/ed` and `chore/add-tests` are colliding siblings

Both 0 behind `main`, sharing four files: `R/design_utils.R`, `R/metrics.R`,
`tests/testthat/test-objective_functions.R`, `tests/testthat/test-speed.R`.

**Land `chore/add-tests` first**, then rebase `bugfix/ed` on it. `bugfix/ed` changes behaviour
(Consolidation stage 1 moves scores for every non-square grid), so it wants the wider test net
underneath it, not developed alongside it.

### 3d. `feature/buffers` - sequence, do not parallelise

Touches `R/metrics.R`, `R/summary.R` and `R/calculate_adjacency_score.R` - exactly the consolidation
surface - and is 40 behind. Land it either before stage 1 or rebased after, never concurrently.

---

## 4. Leave alone

- **`feature/keep-with`** (18 ahead, 0 behind) - touches `constants.R`, `design_utils.R`, `speed.R`,
  `utils.R`, `verify_utils.R` and two tests. No metrics or summary code, so it is genuinely disjoint
  from the consolidation. Ship whenever.
- **`feature/update-sp`** (86 behind) - its own issue, but overlaps `R/design_utils.R` with
  `feature/keep-with` and `chore/add-tests`, so it needs rebasing before it lands.

---

## Suggested order

1. **Push `info-objective`** - it is unbacked up.
2. Delete `origin/feature/aco` and local `feature/incidence` (group 1). Reversible only from a
   reflog, so do these deliberately.
3. Move the nine document branches into `plans/` / `dev/` / issues (group 2). No code risk, biggest
   drop in branch count.
4. Land `chore/add-tests`, then rebase `bugfix/ed` (3c) and do Consolidation stage 1.
5. Rebase the design-metrics group (3a) onto the stage-1 primitive, then `feature/buffers` (3d).
6. `feature/keep-with` and `feature/update-sp` whenever convenient.
