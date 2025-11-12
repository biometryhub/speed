# Initialise Multiple Design Data Frames

Initialise Multiple Design Data Frames

## Usage

``` r
initialise_multiple_designs_df(items, designs, design_col)
```

## Arguments

- items:

  Items to be placed in the design. Either a single numeric value (the
  number of equally replicated items), or a vector of items. (default:
  `NULL`)

- designs:

  A list of named arguments describing design specifications, required
  if `nrows` and `ncols` are absent. (default: `NULL`)

- design_col:

  A column name to distinguish different designs (default: `"site"`)
