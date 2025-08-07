# Test examples for the enhanced initialise_design_df function

# Load the package (assuming the functions are available)
# library(speed)

# Example 1: Simple CRD (existing functionality)
crd_design <- initialise_design_df(
  items = 4,  # 4 treatments
  nrows = 4,
  ncols = 4,
  design_type = "crd",
  seed = 123
)
print("CRD Design:")
print(head(crd_design))

# Example 2: RCBD (enhanced existing functionality)
rcbd_design <- initialise_design_df(
  items = c("A", "B", "C", "D"),
  nrows = 8,
  ncols = 4,
  block_nrows = 2,
  block_ncols = 2,
  design_type = "rcbd",
  seed = 123
)
print("\nRCBD Design:")
print(head(rcbd_design))

# Example 3: Split-plot design
split_plot_design <- initialise_design_df(
  items = list(
    whole_plot = c("Irrigation_High", "Irrigation_Low"),
    sub_plot = c("Variety_A", "Variety_B", "Variety_C")
  ),
  nrows = 6,
  ncols = 4,
  design_type = "split_plot",
  wp_nrows = 2,
  wp_ncols = 2,
  seed = 123
)
print("\nSplit-plot Design:")
print(head(split_plot_design, 12))

# Example 4: Strip-plot design
strip_plot_design <- initialise_design_df(
  items = list(
    row_factor = c("Tillage_Conv", "Tillage_NoTill"),
    col_factor = c("Nitrogen_0", "Nitrogen_50", "Nitrogen_100")
  ),
  nrows = 6,
  ncols = 6,
  design_type = "strip_plot",
  wp_nrows = 3,
  wp_ncols = 2,
  seed = 123
)
print("\nStrip-plot Design:")
print(head(strip_plot_design, 12))

# Example 5: Split-plot with blocking
split_plot_blocked <- initialise_design_df(
  items = list(
    whole_plot = c("Water_Stress", "Water_Normal"),
    sub_plot = c("Fertilizer_Low", "Fertilizer_Med", "Fertilizer_High")
  ),
  nrows = 12,
  ncols = 4,
  block_nrows = 6,
  block_ncols = 2,
  design_type = "split_plot",
  wp_nrows = 2,
  wp_ncols = 2,
  seed = 123
)
print("\nSplit-plot with Blocking:")
print(head(split_plot_blocked, 12))

# Example 6: Large split-plot design
large_split_plot <- initialise_design_df(
  items = list(
    whole_plot = c("Treatment_A", "Treatment_B", "Treatment_C", "Treatment_D"),
    sub_plot = c("Method_1", "Method_2", "Method_3", "Method_4", "Method_5")
  ),
  nrows = 20,
  ncols = 16,
  design_type = "split_plot",
  wp_nrows = 4,
  wp_ncols = 4,
  seed = 123
)
print("\nLarge Split-plot Design:")
print(paste("Design dimensions:", nrow(large_split_plot), "plots"))
print(paste("Number of whole plots:", length(unique(large_split_plot$whole_plot))))
print("First few rows:")
print(head(large_split_plot))
