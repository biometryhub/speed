# =============================================================================
# Demonstration: print.design / summary.design updates (speed 0.0.9)
# -----------------------------------------------------------------------------
# A walk-through of the new design-evaluation output across realistic designs.
# Run top to bottom. Each block prints something worth looking at.
#
#   - print(design)   : compact "identity card"
#   - summary(design) : structure + optimisation + evaluation metrics + flags
#
# If you are iterating on the package source, swap library(speed) for
# pkgload::load_all(".").
# =============================================================================

library(speed)

# Helper to title each section in the console output.
banner <- function(x) {
  cat("\n", strrep("=", 70), "\n", x, "\n", strrep("=", 70), "\n", sep = "")
}


# =============================================================================
# 1. Completely randomised design (CRD)
#    8 varieties, 4 reps, on an 8 x 4 grid. The simplest case.
# =============================================================================
banner("1. CRD - print() vs summary()")

crd <- initialise_design_df(items = 8, nrows = 8, ncols = 4)
crd_res <- speed(crd, swap = "treatment", seed = 42, quiet = TRUE)

# print(): the compact identity card - layout, replication, score, iterations.
print(crd_res)

# summary(): the full evaluation. Note the score is decomposed into its
# adjacency and balance components (raw x weight = contribution), and the
# Evaluation section reports connectedness and replicate spread.
summary(crd_res)


# =============================================================================
# 2. The design object is now self-describing (metadata field)
#    summary() relies on this; you can inspect it directly.
# =============================================================================
banner("2. metadata: how the design was produced")

str(crd_res$metadata, max.level = 2)


# =============================================================================
# 3. Partially-replicated (p-rep) design with check varieties
#    8 test lines at 1 rep + 2 checks at 4 reps -> unequal replication.
#    Demonstrates the replication distribution in print() and the
#    "unequal replication" flag in summary().
# =============================================================================
banner("3. p-rep design - unequal replication")

prep <- data.frame(
  row = rep(1:4, each = 4),
  col = rep(1:4, times = 4),
  treatment = c(paste0("line", 1:8), rep(c("CHK1", "CHK2"), 4))
)
prep_res <- speed(
  prep,
  swap = "treatment",
  spatial_factors = ~ row + col,
  iterations = 2000,
  seed = 11,
  quiet = TRUE
)

# print() summarises the replication distribution rather than just "unequal".
print(prep_res)

# summary() reports min/mean/max replication and raises the flag.
summary(prep_res)


# =============================================================================
# 4. Randomised complete block design (RCBD)
#    6 varieties in 4 complete blocks. Every variety appears in every block,
#    so concurrence carries no information and is auto-skipped.
# =============================================================================
banner("4. RCBD - concurrence auto-skipped (complete blocks)")

rcbd <- data.frame(
  row = rep(1:4, each = 6),
  col = rep(1:6, times = 4),
  block = rep(1:4, each = 6),
  treatment = rep(LETTERS[1:6], 4)
)
rcbd_res <- speed(
  rcbd,
  swap = "treatment",
  swap_within = "block",
  spatial_factors = ~ row + col + block,
  iterations = 2000,
  seed = 5,
  quiet = TRUE
)

summary(rcbd_res)

# You can still force concurrence on if you want to see it:
cat("\n--- same design, concurrence = TRUE (forced) ---\n")
summary(rcbd_res, concurrence = TRUE)


# =============================================================================
# 5. Incomplete block design (a balanced incomplete block design, BIBD)
#    7 varieties in 7 blocks of 3 (the Fano plane): every pair of varieties
#    shares exactly one block, so concurrence is constant (lambda = 1).
#    This is where concurrence is genuinely informative.
# =============================================================================
banner("5. BIBD - informative concurrence (constant lambda)")

bibd <- data.frame(
  row = rep(1:7, each = 3),
  col = rep(1:3, times = 7),
  block = rep(1:7, each = 3),
  treatment = c(
    "A",
    "B",
    "C",
    "A",
    "D",
    "E",
    "A",
    "F",
    "G",
    "B",
    "D",
    "F",
    "B",
    "E",
    "G",
    "C",
    "D",
    "G",
    "C",
    "E",
    "F"
  )
)
bibd_res <- speed(
  bibd,
  swap = "treatment",
  swap_within = "block",
  spatial_factors = ~ row + col + block,
  iterations = 2000,
  seed = 3,
  quiet = TRUE
)

summary(bibd_res)


# =============================================================================
# 6. Split-plot design (hierarchical)
#    3 whole-plot treatments x 4 sub-plot treatments in 2 blocks.
#    print() and summary() report per-level structure and optimisation.
# =============================================================================
banner("6. Split-plot - per-level reporting")

split <- data.frame(
  row = rep(1:6, each = 4),
  col = rep(1:4, times = 6),
  block = rep(1:2, each = 12),
  wholeplot_treatment = rep(LETTERS[1:3], each = 8),
  subplot_treatment = rep(letters[1:4], 6)
)
split_res <- speed(
  split,
  swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
  swap_within = list(wp = "block", sp = "wholeplot_treatment"),
  spatial_factors = ~ row + col,
  iterations = list(wp = 1500, sp = 1500),
  seed = 42,
  quiet = TRUE
)

print(split_res)
summary(split_res)


# =============================================================================
# 7. A-efficiency factor (opt-in)
#    The heaviest metric (assumes a row-column model); off by default.
# =============================================================================
banner("7. Opt-in A-efficiency factor")

summary(crd_res, efficiency = TRUE)


# =============================================================================
# 8. Neighbour-balance design (Piepho objective)
#    When the design is optimised with objective_function_piepho, summary()
#    auto-detects it and reports neighbour-balance diagnostics.
# =============================================================================
banner("8. Neighbour balance - auto-detected from the objective used")

nb <- data.frame(
  row = rep(1:5, each = 5),
  col = rep(1:5, times = 5),
  treatment = rep(LETTERS[1:5], 5)
)
nb_res <- speed(
  nb,
  swap = "treatment",
  spatial_factors = ~ row + col,
  obj_function = objective_function_piepho,
  iterations = 2000,
  seed = 1,
  quiet = TRUE
)

summary(nb_res)


# =============================================================================
# 9. Programmatic access
#    summary() returns a list of class "summary.design"; everything printed is
#    also queryable for scripting / reporting.
# =============================================================================
banner("9. Pulling metrics out of a summary object")

s <- summary(rcbd_res)
cat("Final score          :", s$score, "\n")
cat("Treatments           :", s$per_level[[1]]$n_treatments, "\n")
cat(
  "Replication (min/max):",
  s$per_level[[1]]$replication$min,
  "/",
  s$per_level[[1]]$replication$max,
  "\n"
)
cat("Score components     : (sum to the final score)\n")
print(s$per_level[[1]]$score$components)
cat(
  "Connected            :",
  s$per_level[[1]]$evaluation$connectedness$connected,
  "\n"
)
cat("Hit iteration cap    :", length(s$flags$hit_iteration_cap) > 0, "\n")


# =============================================================================
# 10. Connectedness diagnostics (under the hood)
#     speed designs are normally connected, so the disconnected case is shown
#     directly via the internal helper on hand-built layouts.
# =============================================================================
banner("10. Connectedness - connected vs disconnected")

# Connectedness adjusts for the design's stratifying factors (spatial + block)
# and checks whether all treatment contrasts remain estimable.

# (a) treatment fully confounded with row -> not estimable given row + col
confounded <- data.frame(
  row = rep(1:4, each = 3),
  col = rep(1:3, times = 4),
  treatment = rep(LETTERS[1:4], each = 3)
)
cat("Confounded (treatment = row):\n  ")
print(
  speed:::.design_connectedness(
    confounded,
    "treatment",
    NULL,
    c("row", "col")
  )$message
)

# (b) two variety groups that never share a block -> disconnected given block
disjoint <- data.frame(
  block = c(1, 1, 2, 2, 3, 3, 4, 4),
  treatment = c("A", "B", "A", "B", "C", "D", "C", "D")
)
cat("Disjoint blocks:\n  ")
print(
  speed:::.design_connectedness(
    disjoint,
    "treatment",
    "block",
    character(0)
  )$message
)

# (c) a properly connected block structure
linked <- data.frame(
  block = c(1, 1, 2, 2, 3, 3),
  treatment = c("A", "B", "B", "C", "C", "A")
)
cat("Linked blocks:\n  ")
print(
  speed:::.design_connectedness(
    linked,
    "treatment",
    "block",
    character(0)
  )$message
)

banner("End of demonstration")
