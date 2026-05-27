# Validate and Flatten a Relationship Matrix for Lookup

Validates `relationship` and packs it into a flat numeric vector plus
row/column level vectors that
[`adjacency_score_vec()`](https://biometryhub.github.io/speed/reference/adjacency_score_vec.md)
can index into in constant time per cell.
[`speed()`](https://biometryhub.github.io/speed/reference/speed.md)
calls this once before the SA loop so the per-iteration cost stays in
the lookup itself. Direct callers of
[`calculate_adjacency_score()`](https://biometryhub.github.io/speed/reference/calculate_adjacency_score.md)
must wrap their matrix with this function before passing - the score
functions consume the prepped form only.

## Usage

``` r
prep_relationship(relationship, treatments = NULL)
```

## Arguments

- relationship:

  A numeric matrix with rownames and colnames covering every treatment
  value to be scored.

- treatments:

  Optional character vector of treatment values that `relationship` must
  cover. Skipped when `NULL`.

## Value

A list with `flat` (column-major flattened matrix), `row_levels`,
`col_levels`, and `n_row`.
