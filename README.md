
<!-- README.md is generated from README.Rmd. Please edit that file -->

# speed - Spatially Efficient Experimental Designs

<!-- badges: start -->

[![R-CMD-check](https://github.com/biometryhub/speed-sam/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/biometryhub/speed-sam/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

The `speed` package optimizes spatial experimental designs by
rearranging treatments to improve statistical efficiency while
maintaining statistical validity. It uses simulated annealing to:

- Minimize treatment adjacency (reducing neighbor effects)
- Maintain spatial balance across rows and columns
- Respect blocking constraints if specified
- Provide visualization tools for design evaluation

## Installation

You can install the development version of speed from
[GitHub](https://github.com/biometryhub/speed) with:

``` r
# install.packages("pak")
pak::pak("biometryhub/speed")
```

## Features

- Flexible optimization of experimental designs
- Support for blocked designs
- Customizable optimization parameters
- Built-in visualization functions
- Progress tracking during optimization
- Early stopping when convergence is reached

See the package documentation for more detailed examples and options.

## Basic Example

A simple example optimizing a 4×3 completely randomised design with 4
treatments:

``` r
library(speed)

# Create a simple design with 3 replicates of 4 treatments
df <- data.frame(
  row = rep(1:4, each = 3),
  col = rep(1:3, times = 4),
  Treatment = rep(LETTERS[1:4], 3)
)

# Set seed for reproducibility
set.seed(42)

# Optimize the design
result <- speed(df, permute = ~Treatment)
#> Iteration: 1000 Score: 1 Best: 1 Since Improvement: 359 
#> Iteration: 2000 Score: 1 Best: 1 Since Improvement: 1359 
#> Early stopping at iteration 2641

# Plot the optimized design
plot_design(result)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r

# View optimization progress
plot_progress(result)
```

<img src="man/figures/README-example-2.png" width="100%" /><img src="man/figures/README-example-3.png" width="100%" />

## Blocked design

You can also optimize designs within blocks:

``` r
# Create a design with blocks
df <- data.frame(
  row = rep(1:6, each = 4),
  col = rep(1:4, times = 6),
  Treatment = rep(LETTERS[1:8], 3),
  Block = rep(1:3, each = 8)
)

# Set seed for reproducibility
set.seed(42)

# Optimize while respecting blocks
result <- speed(df, 
                permute = ~Treatment,
                swap_within = ~Block,
                iterations = 2000)
#> Iteration: 1000 Score: 2.571429 Best: 2.571429 Since Improvement: 315 
#> Iteration: 2000 Score: 2.571429 Best: 2.571429 Since Improvement: 1315

# Plot the design with block boundaries
plot_design(result, block_var = "Block")
```

<img src="man/figures/README-blocks-1.png" width="100%" />
