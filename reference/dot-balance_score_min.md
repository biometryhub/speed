# Smallest Achievable Balance Score

Lower bound on
[`calculate_balance_score()`](https://biometryhub.github.io/speed/reference/calculate_balance_score.md)
for any arrangement of `swap`. Each level of a spatial factor holds a
fixed number of plots, and the variance of the treatment counts within
it is smallest when those plots are split as evenly as possible across
the `t` treatments. For a level of `n` plots, the remainder, `rem` is
`n %% t`, that minimum has the closed form
`rem * (t - rem) / (t * (t - 1))`.

## Usage

``` r
.balance_score_min(layout_df, swap, spatial_cols)
```

## Arguments

- layout_df:

  A data frame representing the current design

- swap:

  A column name of the items to be swapped

- spatial_cols:

  Column name(s) of the spatial factors

## Value

A single non-negative numeric value.

## See also

[`calculate_balance_score()`](https://biometryhub.github.io/speed/reference/calculate_balance_score.md)
