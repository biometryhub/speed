# Signal a Coordinate Problem with a Classed Condition

Carries two phrasings of the same problem: `message`, for someone
calling a metric directly, and `reason`, a fragment
[`summary()`](https://rdrr.io/r/base/summary.html) reports in place of a
metric it cannot compute. Both are defined at the throw site so they
cannot drift, and the class lets callers dispatch without matching on
message text.

## Usage

``` r
.grid_stop(class, reason, ...)
```

## Arguments

- class:

  Condition subclass naming the specific problem.

- reason:

  Short phrase for a [`summary()`](https://rdrr.io/r/base/summary.html)
  field.

- ...:

  Pasted together to form the message.
