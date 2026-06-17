# Initialise a Split-Plot Design Data Frame

Build a split plot design from the ground up, given the nested unit
structure and how many times the largest unit is replicated. The field
dimensions are derived from the nested structure, and any number of
split levels is supported (split-plot, split-split-plot, and so on).

## Usage

``` r
initialise_split_design_df(splits, rep_dim = c(1, 1))
```

## Arguments

- splits:

  A named list of nested-unit specifications, ordered from the
  *innermost* (smallest) level to the *outermost* (the replicated unit,
  e.g. the block). Each entry is itself a list with:

  - `nrows`, `ncols` - the dimensions of one unit at that level, in
    cells. The innermost level is always 1x1, so its `nrows`/`ncols` may
    be omitted.

  - `items` - optional treatments to allocate across the units at that
    level, one item per unit. A single number is expanded to `T1`, `T2`,
    ...; a shorter vector is reused to fill each parent unit. The
    outermost entry defines the replicated unit; `rep_dim` tiles it
    across the field. For each level a `<name>` ID column is added, plus
    a `<name>_treatment` column wherever `items` is supplied.

- rep_dim:

  Length-2 integer vector `c(row_reps, col_reps)` giving the replicate
  dimension of the whole structure (default: `c(1, 1)`).

## Value

A data frame with `row` and `col` columns plus one ID (and optional
treatment) column per split level.

## Examples

``` r
# split-plot: 4 blocks (2x2) of 3x4 cells; each block holds 3 wholeplots
# (1x4, treatments A-C), each wholeplot holds 4 subplots (1x1, treatments a-d)
initialise_split_design_df(
  splits = list(
    subplot   = list(items = letters[1:4]),
    wholeplot = list(items = LETTERS[1:3], nrows = 1, ncols = 4),
    block     = list(nrows = 3, ncols = 4)
  ),
  rep_dim = c(2, 2)
)
#>    row col block wholeplot wholeplot_treatment subplot subplot_treatment
#> 1    1   1     1         1                   A       1                 a
#> 2    2   1     1         2                   B       5                 a
#> 3    3   1     1         3                   C       9                 a
#> 4    4   1     2         4                   A      13                 a
#> 5    5   1     2         5                   B      17                 a
#> 6    6   1     2         6                   C      21                 a
#> 7    1   2     1         1                   A       2                 b
#> 8    2   2     1         2                   B       6                 b
#> 9    3   2     1         3                   C      10                 b
#> 10   4   2     2         4                   A      14                 b
#> 11   5   2     2         5                   B      18                 b
#> 12   6   2     2         6                   C      22                 b
#> 13   1   3     1         1                   A       3                 c
#> 14   2   3     1         2                   B       7                 c
#> 15   3   3     1         3                   C      11                 c
#> 16   4   3     2         4                   A      15                 c
#> 17   5   3     2         5                   B      19                 c
#> 18   6   3     2         6                   C      23                 c
#> 19   1   4     1         1                   A       4                 d
#> 20   2   4     1         2                   B       8                 d
#> 21   3   4     1         3                   C      12                 d
#> 22   4   4     2         4                   A      16                 d
#> 23   5   4     2         5                   B      20                 d
#> 24   6   4     2         6                   C      24                 d
#> 25   1   5     3         7                   A      25                 a
#> 26   2   5     3         8                   B      29                 a
#> 27   3   5     3         9                   C      33                 a
#> 28   4   5     4        10                   A      37                 a
#> 29   5   5     4        11                   B      41                 a
#> 30   6   5     4        12                   C      45                 a
#> 31   1   6     3         7                   A      26                 b
#> 32   2   6     3         8                   B      30                 b
#> 33   3   6     3         9                   C      34                 b
#> 34   4   6     4        10                   A      38                 b
#> 35   5   6     4        11                   B      42                 b
#> 36   6   6     4        12                   C      46                 b
#> 37   1   7     3         7                   A      27                 c
#> 38   2   7     3         8                   B      31                 c
#> 39   3   7     3         9                   C      35                 c
#> 40   4   7     4        10                   A      39                 c
#> 41   5   7     4        11                   B      43                 c
#> 42   6   7     4        12                   C      47                 c
#> 43   1   8     3         7                   A      28                 d
#> 44   2   8     3         8                   B      32                 d
#> 45   3   8     3         9                   C      36                 d
#> 46   4   8     4        10                   A      40                 d
#> 47   5   8     4        11                   B      44                 d
#> 48   6   8     4        12                   C      48                 d

# split-split-plot: an extra level nested inside the subplot
initialise_split_design_df(
  splits = list(
    subsubplot = list(items = 1:2),
    subplot    = list(items = letters[1:4], nrows = 1, ncols = 2),
    wholeplot  = list(items = LETTERS[1:3], nrows = 1, ncols = 8),
    block      = list(nrows = 3, ncols = 8)
  ),
  rep_dim = c(2, 1)
)
#>    row col block wholeplot wholeplot_treatment subplot subplot_treatment
#> 1    1   1     1         1                   A       1                 a
#> 2    2   1     1         2                   B       5                 a
#> 3    3   1     1         3                   C       9                 a
#> 4    4   1     2         4                   A      13                 a
#> 5    5   1     2         5                   B      17                 a
#> 6    6   1     2         6                   C      21                 a
#> 7    1   2     1         1                   A       1                 a
#> 8    2   2     1         2                   B       5                 a
#> 9    3   2     1         3                   C       9                 a
#> 10   4   2     2         4                   A      13                 a
#> 11   5   2     2         5                   B      17                 a
#> 12   6   2     2         6                   C      21                 a
#> 13   1   3     1         1                   A       2                 b
#> 14   2   3     1         2                   B       6                 b
#> 15   3   3     1         3                   C      10                 b
#> 16   4   3     2         4                   A      14                 b
#> 17   5   3     2         5                   B      18                 b
#> 18   6   3     2         6                   C      22                 b
#> 19   1   4     1         1                   A       2                 b
#> 20   2   4     1         2                   B       6                 b
#> 21   3   4     1         3                   C      10                 b
#> 22   4   4     2         4                   A      14                 b
#> 23   5   4     2         5                   B      18                 b
#> 24   6   4     2         6                   C      22                 b
#> 25   1   5     1         1                   A       3                 c
#> 26   2   5     1         2                   B       7                 c
#> 27   3   5     1         3                   C      11                 c
#> 28   4   5     2         4                   A      15                 c
#> 29   5   5     2         5                   B      19                 c
#> 30   6   5     2         6                   C      23                 c
#> 31   1   6     1         1                   A       3                 c
#> 32   2   6     1         2                   B       7                 c
#> 33   3   6     1         3                   C      11                 c
#> 34   4   6     2         4                   A      15                 c
#> 35   5   6     2         5                   B      19                 c
#> 36   6   6     2         6                   C      23                 c
#> 37   1   7     1         1                   A       4                 d
#> 38   2   7     1         2                   B       8                 d
#> 39   3   7     1         3                   C      12                 d
#> 40   4   7     2         4                   A      16                 d
#> 41   5   7     2         5                   B      20                 d
#> 42   6   7     2         6                   C      24                 d
#> 43   1   8     1         1                   A       4                 d
#> 44   2   8     1         2                   B       8                 d
#> 45   3   8     1         3                   C      12                 d
#> 46   4   8     2         4                   A      16                 d
#> 47   5   8     2         5                   B      20                 d
#> 48   6   8     2         6                   C      24                 d
#>    subsubplot subsubplot_treatment
#> 1           1                    1
#> 2           9                    1
#> 3          17                    1
#> 4          25                    1
#> 5          33                    1
#> 6          41                    1
#> 7           2                    2
#> 8          10                    2
#> 9          18                    2
#> 10         26                    2
#> 11         34                    2
#> 12         42                    2
#> 13          3                    1
#> 14         11                    1
#> 15         19                    1
#> 16         27                    1
#> 17         35                    1
#> 18         43                    1
#> 19          4                    2
#> 20         12                    2
#> 21         20                    2
#> 22         28                    2
#> 23         36                    2
#> 24         44                    2
#> 25          5                    1
#> 26         13                    1
#> 27         21                    1
#> 28         29                    1
#> 29         37                    1
#> 30         45                    1
#> 31          6                    2
#> 32         14                    2
#> 33         22                    2
#> 34         30                    2
#> 35         38                    2
#> 36         46                    2
#> 37          7                    1
#> 38         15                    1
#> 39         23                    1
#> 40         31                    1
#> 41         39                    1
#> 42         47                    1
#> 43          8                    2
#> 44         16                    2
#> 45         24                    2
#> 46         32                    2
#> 47         40                    2
#> 48         48                    2
```
