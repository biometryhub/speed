# Calculate Efficiency Factor according Piepho

Calculates an efficiency factor of a design according to Piepho 2015.

## Usage

``` r
calculate_efficiency_factor(design_df, item)
```

## Arguments

- design_df:

  A data frame containing the experimental design with spatial
  coordinates

- item:

  A column name of the items in the design (e.g., `treatment`,
  `variety`, `genotype`, etc)

## Value

A numeric value representing the efficiency factor of the design. Higher
values indicate more efficient designs.

## References

Piepho, H. P., Williams, E., & Michel, V. (2015). Nonresolvable
Row-Column Designs with an Even Distribution of Treatment Replications.
Journal of Agricultural, Biological, and Environmental Statistics, 21,
227-242 (2016). <https://doi.org/10.1007/s13253-015-0241-2>

## Examples

``` r
df_design <- initialise_design_df(c(
  "a", "b", "d", "c",
  "e", "a", "f", "b",
  "c", "f", "e", "d"
), 3, 4)

calculate_efficiency_factor(df_design, "treatment")
#> [1] 0.6268657
```
