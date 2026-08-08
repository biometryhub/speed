# Calculate Efficiency Factor according Piepho

Calculates an efficiency factor of a design according to Piepho 2015.

## Usage

``` r
calculate_efficiency_factor(
  design_df,
  item,
  row_column = "row",
  col_column = "col"
)
```

## Arguments

- design_df:

  A data frame containing the experimental design with spatial
  coordinates

- item:

  A column name of the items in the design (e.g., `treatment`,
  `variety`, `genotype`, etc)

- row_column:

  Name of the column giving the row of the design (default: "row")

- col_column:

  Name of the column giving the column of the design (default: "col")

## Value

A numeric value representing the efficiency factor of the design,
between 0 and 1. Higher values indicate more efficient designs.

Errors with a `speed_efficiency_rank` condition if the design cannot
support the estimate - that is, if some treatment contrast is not
estimable once row and column effects are eliminated, whether because
too few residual degrees of freedom remain or because a treatment is
confounded with a row or column. Such a design has no efficiency factor;
before this check the formula returned a plausible-looking value,
usually above 1.

## References

Piepho, H. P., Williams, E., & Michel, V. (2015). Nonresolvable
Row-Column Designs with an Even Distribution of Treatment Replications.
Journal of Agricultural, Biological, and Environmental Statistics, 21,
227-242 (2016). <https://doi.org/10.1007/s13253-015-0241-2>

## Examples

``` r
# `initialise_design_df()` fills `items` down columns, so the literal below is
# column-major; the grid it produces is
#   a b d c
#   e a f b
#   c f e d
df_design <- initialise_design_df(c(
  "a", "e", "c",
  "b", "a", "f",
  "d", "f", "e",
  "c", "b", "d"
), 3, 4)

calculate_efficiency_factor(df_design, "treatment")
#> [1] 0.6268657

# Not every design can support the estimate. Here each treatment fills one
# grid row, so the treatment differences cannot be separated from the row
# effects and there is no efficiency factor to report:
#   a a a a
#   b b b b
#   c c c c
confounded <- initialise_design_df(rep(c("a", "b", "c"), 4), 3, 4)
try(calculate_efficiency_factor(confounded, "treatment"))
#> Error : Not all treatment contrasts are estimable after eliminating `row` and `col` effects, so this design cannot support an efficiency factor.
```
