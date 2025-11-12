# Random Initialize

Randomly shuffle items with
[shuffle_items](https://biometryhub.github.io/speed/reference/shuffle_items.md)
n times and return the best design.

## Usage

``` r
random_initialize(design, optimise, seed = NULL, ...)
```

## Arguments

- optimise:

  A list of named arguments describing optimising parameters; see more
  in example.

- seed:

  A numeric value for random seed. If provided, it ensures
  reproducibility of results (default: `NULL`).

- ...:

  Other arguments passed through to objective functions.

## Value

A data frame with the items shuffled
