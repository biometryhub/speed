# Set up parameters
sites <- c(rep("S1", 18), rep("S2", 20), rep("S3", 24))
plot_ids <- paste0(sites, "_", c(1:18, 1:20, 1:24))
treatments <- paste0("V", 1:15)

# Create initial data frame with random treatment allocation
set.seed(42) # For reproducibility
initial_design <- data.frame(
    site = sites,
    plot = plot_ids,
    row = c(rep(1:6, each = 3), rep(1:5, each = 4), rep(1:6, each = 4)),
    col = c(rep(1:3, times = 6), rep(1:4, times = 5), rep(1:4, times = 6)),
    treatment = sample(treatments, 62, replace = TRUE)
)

# Check counts
table(initial_design$site)
# S1 S2 S3
# 18 20 24

# Check initial treatment distribution
table(initial_design$treatment)
# Treatment counts should be approximately 4-5 each (62 plots / 15 treatments ≈ 4.13)

# Check initial site distribution per treatment
site_treatment_table <- table(initial_design$site, initial_design$treatment)
site_treatment_table
# This shows how many times each treatment appears in each site

# Check how many sites each treatment appears in
treatment_site_counts <- apply(site_treatment_table > 0, 2, sum)
treatment_site_counts
# Ideally, we want most treatments to appear in all three sites


initial_design <- initial_design[order(initial_design$treatment),]

result <- speed_df(
    data = initial_design,
    swap = "treatment",
    swap_within = "none",  # Allow treatments to be swapped across sites
    spatial_factors = ~ site,
    obj_function = objective_function_multisite(connectivity_weight = 1, replication_weight = 2),
    quiet = FALSE
)

site_treatment_incidence <- table(result$design_df$site, result$design_df$treatment)
site_treatment_incidence
apply(site_treatment_incidence > 0, 2, sum)

treatment_site_incidence <- table(result$design_df$treatment, result$design_df$site)
treatment_site_incidence



# Example 1: Basic multisite design with balanced replication
df_balanced <- data.frame(
  site = rep(1:3, each = 6),
  row = rep(1:2, times = 9),
  col = rep(rep(1:3, each = 2), times = 3),
  treatment = rep(LETTERS[1:3], each = 6)
)

# Example 2: Unbalanced multisite design
df_unbalanced <- data.frame(
  site = rep(1:3, each = 6),
  row = rep(1:2, times = 9),
  col = rep(rep(1:3, each = 2), times = 3),
  treatment = c(rep("A", 8), rep("B", 6), rep("C", 4))
)

# Test connectivity score
con_score_balanced <- calculate_connectivity_score(df_balanced, "treatment", "site")
print(paste("Connectivity score (balanced):", con_score_balanced))
# Should be high as treatments are evenly distributed across sites

con_score_unbalanced <- calculate_connectivity_score(df_unbalanced, "treatment", "site")
print(paste("Connectivity score (unbalanced):", con_score_unbalanced))
# Should be lower due to uneven distribution

# Test replication score
rep_score_balanced <- calculate_replication_score(df_balanced, "treatment")
print(paste("Replication score (balanced):", rep_score_balanced))
# Should be low (better) as treatments are evenly replicated

rep_score_unbalanced <- calculate_replication_score(df_unbalanced, "treatment")
print(paste("Replication score (unbalanced):", rep_score_unbalanced))
# Should be higher (worse) due to uneven replication

# Test complete objective function
obj_fn <- objective_function_multisite(connectivity_weight = 1, replication_weight = 1)

score_balanced <- obj_fn(df_balanced, "treatment", "site")
print(paste("Overall score (balanced):", score_balanced))

score_unbalanced <- obj_fn(df_unbalanced, "treatment", "site")
print(paste("Overall score (unbalanced):", score_unbalanced))
# Unbalanced design should have a worse (higher) score
