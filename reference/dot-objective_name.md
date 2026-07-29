# Identify a known objective function by name

Compares `fn` by identity against the package's exported objective
functions so the summary can report a readable name (and detect the
neighbour-balance objective). Returns `"custom"` for anything
unrecognised.

## Usage

``` r
.objective_name(fn)
```

## Arguments

- fn:

  A function.

## Value

A length-one character string.
