# Calculate Adjacency Score for Design

Calculates the adjacency score for a given experimental design. The
adjacency score represents the number of adjacent plots with the same
treatment. Lower scores indicate better separation of treatments.

## Usage

``` r
calculate_adjacency_score(
  layout_df,
  swap,
  row_column = "row",
  col_column = "col"
)
```

## Arguments

- layout_df:

  A data frame representing the current design

- swap:

  A column name of the items to be swapped

- row_column:

  Name of column representing the row of the design (default: "row")

- col_column:

  Name of column representing the column of the design (default: "col")

## Value

Numeric score for treatment adjacencies (lower is better)

## Examples

``` r
# Example 1: Design with no adjacencies
design_no_adj <- data.frame(
  row = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  col = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  treatment = c("A", "B", "A", "B", "A", "B", "A", "B", "A")
)

# Gives 0
calculate_adjacency_score(design_no_adj, "treatment")
#> [1] 0

# Example 2: Design with adjacencies
design_with_adj <- data.frame(
  row = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  col = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
  treatment = c("A", "A", "A", "B", "B", "B", "A", "A", "A")
)

# Gives value 6
calculate_adjacency_score(design_with_adj, "treatment")
#> [1] 6
```
