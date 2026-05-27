# Reject `...` arguments that must travel through [`optim_params()`](https://biometryhub.github.io/speed/reference/optim_params.md).

Stops with a message pointing the user at
`optimise_params = optim_params(...)` when any of the listed names is
found in `dots`.

## Usage

``` r
.reject_optim_params_in_dots(dots)
```

## Arguments

- dots:

  A named list captured from `...`.

## Value

`NULL`, invisibly. Called for its side effect.
