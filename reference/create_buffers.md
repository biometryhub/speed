# Create buffers for design plots

Create buffers for design plots

## Usage

``` r
create_buffers(design, type, blocks = FALSE, treatment_cols = NULL)
```

## Arguments

- design:

  The data frame of the design.

- type:

  The type of buffer. One of edge, row, column, double row, double
  column, or block (coming soon).

- blocks:

  Does the design data frame contain blocks?

- treatment_cols:

  Character vector of treatment column names to fill with "buffer". If
  NULL (default), uses "treatment" column if it exists.

## Value

The original data frame, updated to include buffers
