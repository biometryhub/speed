# Initialise Design Data Frame

Initialise a design data frame with or without blocking.

## Usage

``` r
initialise_design_df(
  items = NULL,
  nrows = NULL,
  ncols = NULL,
  block_nrows = NULL,
  block_ncols = NULL,
  splits = NULL,
  designs = NULL,
  design_col = "site"
)

initialize_design_df(
  items = NULL,
  nrows = NULL,
  ncols = NULL,
  block_nrows = NULL,
  block_ncols = NULL,
  splits = NULL,
  designs = NULL,
  design_col = "site"
)
```

## Arguments

- items:

  Items to be placed in the design. Either a single numeric value (the
  number of equally replicated items), or a vector of items. (default:
  `NULL`)

- nrows:

  Number of rows in the design (default: `NULL`)

- ncols:

  Number of columns in the design (default: `NULL`)

- block_nrows:

  Number of rows in each block (default: `NULL`)

- block_ncols:

  Number of columns in each block (default: `NULL`)

- splits:

  A named list of nested-unit specifications, ordered from the outermost
  level to the innermost. Each entry is itself a list with `nrows` and
  `ncols` (the dimensions of one unit at that level, in cells) and an
  optional `items` (treatments to allocate across the units at that
  level, one item per unit, ordered by parent then within-parent ID).
  For each level, `<name>` and `<name>_treatment` columns are added (the
  latter only if `items` is provided). Used to build hierarchical
  layouts such as split-plot, split-split-plot, and strip-plot designs.
  (default: `NULL`)

- designs:

  A list of named arguments describing design specifications, required
  if `nrows` and `ncols` are absent. (default: `NULL`)

- design_col:

  A column name to distinguish different designs (default: `"site"`)

## Value

A data frame containing the design

## Examples

``` r
initialise_design_df(
  items = c(1, 2, 2, 1, 3, 3, 1, 3, 3),
  nrows = 3,
  ncols = 3
)
#>   row col treatment
#> 1   1   1         1
#> 2   2   1         2
#> 3   3   1         2
#> 4   1   2         1
#> 5   2   2         3
#> 6   3   2         3
#> 7   1   3         1
#> 8   2   3         3
#> 9   3   3         3

# blocking
initialise_design_df(rep(1:8, 4), 8, 4, 2, 2)
#>    row col treatment row_block col_block block
#> 1    1   1         1         1         1     1
#> 2    2   1         2         1         1     1
#> 3    3   1         5         2         1     2
#> 4    4   1         6         2         1     2
#> 5    5   1         1         3         1     3
#> 6    6   1         2         3         1     3
#> 7    7   1         5         4         1     4
#> 8    8   1         6         4         1     4
#> 9    1   2         3         1         1     1
#> 10   2   2         4         1         1     1
#> 11   3   2         7         2         1     2
#> 12   4   2         8         2         1     2
#> 13   5   2         3         3         1     3
#> 14   6   2         4         3         1     3
#> 15   7   2         7         4         1     4
#> 16   8   2         8         4         1     4
#> 17   1   3         1         1         2     5
#> 18   2   3         2         1         2     5
#> 19   3   3         5         2         2     6
#> 20   4   3         6         2         2     6
#> 21   5   3         1         3         2     7
#> 22   6   3         2         3         2     7
#> 23   7   3         5         4         2     8
#> 24   8   3         6         4         2     8
#> 25   1   4         3         1         2     5
#> 26   2   4         4         1         2     5
#> 27   3   4         7         2         2     6
#> 28   4   4         8         2         2     6
#> 29   5   4         3         3         2     7
#> 30   6   4         4         3         2     7
#> 31   7   4         7         4         2     8
#> 32   8   4         8         4         2     8

# another blocking example
initialise_design_df(
  items = paste0("T", 1:6),
  nrows = 4,
  ncols = 6,
  block_nrows = 2,
  block_ncols = 3
)
#>    row col treatment row_block col_block block
#> 1    1   1        T1         1         1     1
#> 2    2   1        T2         1         1     1
#> 3    3   1        T1         2         1     2
#> 4    4   1        T2         2         1     2
#> 5    1   2        T3         1         1     1
#> 6    2   2        T4         1         1     1
#> 7    3   2        T3         2         1     2
#> 8    4   2        T4         2         1     2
#> 9    1   3        T5         1         1     1
#> 10   2   3        T6         1         1     1
#> 11   3   3        T5         2         1     2
#> 12   4   3        T6         2         1     2
#> 13   1   4        T1         1         2     3
#> 14   2   4        T2         1         2     3
#> 15   3   4        T1         2         2     4
#> 16   4   4        T2         2         2     4
#> 17   1   5        T3         1         2     3
#> 18   2   5        T4         1         2     3
#> 19   3   5        T3         2         2     4
#> 20   4   5        T4         2         2     4
#> 21   1   6        T5         1         2     3
#> 22   2   6        T6         1         2     3
#> 23   3   6        T5         2         2     4
#> 24   4   6        T6         2         2     4

# MET
initialise_design_df(
  items = c(rep(1:10, 6), rep(11:20, 8)),
  designs = list(
    a = list(nrows = 10, ncols = 3),
    b = list(nrows = 10, ncols = 5),
    c = list(nrows = 10, ncols = 6)
  )
)
#>     row col treatment site
#> 1     1   1         1    a
#> 2     2   1         2    a
#> 3     3   1         3    a
#> 4     4   1         4    a
#> 5     5   1         5    a
#> 6     6   1         6    a
#> 7     7   1         7    a
#> 8     8   1         8    a
#> 9     9   1         9    a
#> 10   10   1        10    a
#> 11    1   2         1    a
#> 12    2   2         2    a
#> 13    3   2         3    a
#> 14    4   2         4    a
#> 15    5   2         5    a
#> 16    6   2         6    a
#> 17    7   2         7    a
#> 18    8   2         8    a
#> 19    9   2         9    a
#> 20   10   2        10    a
#> 21    1   3         1    a
#> 22    2   3         2    a
#> 23    3   3         3    a
#> 24    4   3         4    a
#> 25    5   3         5    a
#> 26    6   3         6    a
#> 27    7   3         7    a
#> 28    8   3         8    a
#> 29    9   3         9    a
#> 30   10   3        10    a
#> 31    1   1         1    b
#> 32    2   1         2    b
#> 33    3   1         3    b
#> 34    4   1         4    b
#> 35    5   1         5    b
#> 36    6   1         6    b
#> 37    7   1         7    b
#> 38    8   1         8    b
#> 39    9   1         9    b
#> 40   10   1        10    b
#> 41    1   2         1    b
#> 42    2   2         2    b
#> 43    3   2         3    b
#> 44    4   2         4    b
#> 45    5   2         5    b
#> 46    6   2         6    b
#> 47    7   2         7    b
#> 48    8   2         8    b
#> 49    9   2         9    b
#> 50   10   2        10    b
#> 51    1   3         1    b
#> 52    2   3         2    b
#> 53    3   3         3    b
#> 54    4   3         4    b
#> 55    5   3         5    b
#> 56    6   3         6    b
#> 57    7   3         7    b
#> 58    8   3         8    b
#> 59    9   3         9    b
#> 60   10   3        10    b
#> 61    1   4        11    b
#> 62    2   4        12    b
#> 63    3   4        13    b
#> 64    4   4        14    b
#> 65    5   4        15    b
#> 66    6   4        16    b
#> 67    7   4        17    b
#> 68    8   4        18    b
#> 69    9   4        19    b
#> 70   10   4        20    b
#> 71    1   5        11    b
#> 72    2   5        12    b
#> 73    3   5        13    b
#> 74    4   5        14    b
#> 75    5   5        15    b
#> 76    6   5        16    b
#> 77    7   5        17    b
#> 78    8   5        18    b
#> 79    9   5        19    b
#> 80   10   5        20    b
#> 81    1   1        11    c
#> 82    2   1        12    c
#> 83    3   1        13    c
#> 84    4   1        14    c
#> 85    5   1        15    c
#> 86    6   1        16    c
#> 87    7   1        17    c
#> 88    8   1        18    c
#> 89    9   1        19    c
#> 90   10   1        20    c
#> 91    1   2        11    c
#> 92    2   2        12    c
#> 93    3   2        13    c
#> 94    4   2        14    c
#> 95    5   2        15    c
#> 96    6   2        16    c
#> 97    7   2        17    c
#> 98    8   2        18    c
#> 99    9   2        19    c
#> 100  10   2        20    c
#> 101   1   3        11    c
#> 102   2   3        12    c
#> 103   3   3        13    c
#> 104   4   3        14    c
#> 105   5   3        15    c
#> 106   6   3        16    c
#> 107   7   3        17    c
#> 108   8   3        18    c
#> 109   9   3        19    c
#> 110  10   3        20    c
#> 111   1   4        11    c
#> 112   2   4        12    c
#> 113   3   4        13    c
#> 114   4   4        14    c
#> 115   5   4        15    c
#> 116   6   4        16    c
#> 117   7   4        17    c
#> 118   8   4        18    c
#> 119   9   4        19    c
#> 120  10   4        20    c
#> 121   1   5        11    c
#> 122   2   5        12    c
#> 123   3   5        13    c
#> 124   4   5        14    c
#> 125   5   5        15    c
#> 126   6   5        16    c
#> 127   7   5        17    c
#> 128   8   5        18    c
#> 129   9   5        19    c
#> 130  10   5        20    c
#> 131   1   6        11    c
#> 132   2   6        12    c
#> 133   3   6        13    c
#> 134   4   6        14    c
#> 135   5   6        15    c
#> 136   6   6        16    c
#> 137   7   6        17    c
#> 138   8   6        18    c
#> 139   9   6        19    c
#> 140  10   6        20    c

# MET with different items for each site
initialise_design_df(
  designs = list(
    a = list(items = 1:30, nrows = 10, ncols = 6),
    b = list(items = 1:25, nrows = 10, ncols = 5),
    c = list(items = 16:30, nrows = 10, ncols = 3)
  )
)
#>     row col treatment site
#> 1     1   1         1    a
#> 2     2   1         2    a
#> 3     3   1         3    a
#> 4     4   1         4    a
#> 5     5   1         5    a
#> 6     6   1         6    a
#> 7     7   1         7    a
#> 8     8   1         8    a
#> 9     9   1         9    a
#> 10   10   1        10    a
#> 11    1   2        11    a
#> 12    2   2        12    a
#> 13    3   2        13    a
#> 14    4   2        14    a
#> 15    5   2        15    a
#> 16    6   2        16    a
#> 17    7   2        17    a
#> 18    8   2        18    a
#> 19    9   2        19    a
#> 20   10   2        20    a
#> 21    1   3        21    a
#> 22    2   3        22    a
#> 23    3   3        23    a
#> 24    4   3        24    a
#> 25    5   3        25    a
#> 26    6   3        26    a
#> 27    7   3        27    a
#> 28    8   3        28    a
#> 29    9   3        29    a
#> 30   10   3        30    a
#> 31    1   4         1    a
#> 32    2   4         2    a
#> 33    3   4         3    a
#> 34    4   4         4    a
#> 35    5   4         5    a
#> 36    6   4         6    a
#> 37    7   4         7    a
#> 38    8   4         8    a
#> 39    9   4         9    a
#> 40   10   4        10    a
#> 41    1   5        11    a
#> 42    2   5        12    a
#> 43    3   5        13    a
#> 44    4   5        14    a
#> 45    5   5        15    a
#> 46    6   5        16    a
#> 47    7   5        17    a
#> 48    8   5        18    a
#> 49    9   5        19    a
#> 50   10   5        20    a
#> 51    1   6        21    a
#> 52    2   6        22    a
#> 53    3   6        23    a
#> 54    4   6        24    a
#> 55    5   6        25    a
#> 56    6   6        26    a
#> 57    7   6        27    a
#> 58    8   6        28    a
#> 59    9   6        29    a
#> 60   10   6        30    a
#> 61    1   1         1    b
#> 62    2   1         2    b
#> 63    3   1         3    b
#> 64    4   1         4    b
#> 65    5   1         5    b
#> 66    6   1         6    b
#> 67    7   1         7    b
#> 68    8   1         8    b
#> 69    9   1         9    b
#> 70   10   1        10    b
#> 71    1   2        11    b
#> 72    2   2        12    b
#> 73    3   2        13    b
#> 74    4   2        14    b
#> 75    5   2        15    b
#> 76    6   2        16    b
#> 77    7   2        17    b
#> 78    8   2        18    b
#> 79    9   2        19    b
#> 80   10   2        20    b
#> 81    1   3        21    b
#> 82    2   3        22    b
#> 83    3   3        23    b
#> 84    4   3        24    b
#> 85    5   3        25    b
#> 86    6   3         1    b
#> 87    7   3         2    b
#> 88    8   3         3    b
#> 89    9   3         4    b
#> 90   10   3         5    b
#> 91    1   4         6    b
#> 92    2   4         7    b
#> 93    3   4         8    b
#> 94    4   4         9    b
#> 95    5   4        10    b
#> 96    6   4        11    b
#> 97    7   4        12    b
#> 98    8   4        13    b
#> 99    9   4        14    b
#> 100  10   4        15    b
#> 101   1   5        16    b
#> 102   2   5        17    b
#> 103   3   5        18    b
#> 104   4   5        19    b
#> 105   5   5        20    b
#> 106   6   5        21    b
#> 107   7   5        22    b
#> 108   8   5        23    b
#> 109   9   5        24    b
#> 110  10   5        25    b
#> 111   1   1        16    c
#> 112   2   1        17    c
#> 113   3   1        18    c
#> 114   4   1        19    c
#> 115   5   1        20    c
#> 116   6   1        21    c
#> 117   7   1        22    c
#> 118   8   1        23    c
#> 119   9   1        24    c
#> 120  10   1        25    c
#> 121   1   2        26    c
#> 122   2   2        27    c
#> 123   3   2        28    c
#> 124   4   2        29    c
#> 125   5   2        30    c
#> 126   6   2        16    c
#> 127   7   2        17    c
#> 128   8   2        18    c
#> 129   9   2        19    c
#> 130  10   2        20    c
#> 131   1   3        21    c
#> 132   2   3        22    c
#> 133   3   3        23    c
#> 134   4   3        24    c
#> 135   5   3        25    c
#> 136   6   3        26    c
#> 137   7   3        27    c
#> 138   8   3        28    c
#> 139   9   3        29    c
#> 140  10   3        30    c

# split-plot: 4 replicate blocks of 12x1, each block holds 3 wholeplots of 4x1,
# each wholeplot holds 4 subplots
initialise_design_df(
  nrows = 12, ncols = 4,
  block_nrows = 12, block_ncols = 1,
  splits = list(
    wholeplot = list(nrows = 4, ncols = 1, items = LETTERS[1:3]),
    subplot = list(nrows = 1, ncols = 1, items = letters[1:4])
  )
)
#>    row col row_block col_block block wholeplot wholeplot_treatment subplot
#> 1    1   1         1         1     1         1                   A       1
#> 2    2   1         1         1     1         1                   A       2
#> 3    3   1         1         1     1         1                   A       3
#> 4    4   1         1         1     1         1                   A       4
#> 5    5   1         1         1     1         2                   B       5
#> 6    6   1         1         1     1         2                   B       6
#> 7    7   1         1         1     1         2                   B       7
#> 8    8   1         1         1     1         2                   B       8
#> 9    9   1         1         1     1         3                   C       9
#> 10  10   1         1         1     1         3                   C      10
#> 11  11   1         1         1     1         3                   C      11
#> 12  12   1         1         1     1         3                   C      12
#> 13   1   2         1         2     2         4                   A      13
#> 14   2   2         1         2     2         4                   A      14
#> 15   3   2         1         2     2         4                   A      15
#> 16   4   2         1         2     2         4                   A      16
#> 17   5   2         1         2     2         5                   B      17
#> 18   6   2         1         2     2         5                   B      18
#> 19   7   2         1         2     2         5                   B      19
#> 20   8   2         1         2     2         5                   B      20
#> 21   9   2         1         2     2         6                   C      21
#> 22  10   2         1         2     2         6                   C      22
#> 23  11   2         1         2     2         6                   C      23
#> 24  12   2         1         2     2         6                   C      24
#> 25   1   3         1         3     3         7                   A      25
#> 26   2   3         1         3     3         7                   A      26
#> 27   3   3         1         3     3         7                   A      27
#> 28   4   3         1         3     3         7                   A      28
#> 29   5   3         1         3     3         8                   B      29
#> 30   6   3         1         3     3         8                   B      30
#> 31   7   3         1         3     3         8                   B      31
#> 32   8   3         1         3     3         8                   B      32
#> 33   9   3         1         3     3         9                   C      33
#> 34  10   3         1         3     3         9                   C      34
#> 35  11   3         1         3     3         9                   C      35
#> 36  12   3         1         3     3         9                   C      36
#> 37   1   4         1         4     4        10                   A      37
#> 38   2   4         1         4     4        10                   A      38
#> 39   3   4         1         4     4        10                   A      39
#> 40   4   4         1         4     4        10                   A      40
#> 41   5   4         1         4     4        11                   B      41
#> 42   6   4         1         4     4        11                   B      42
#> 43   7   4         1         4     4        11                   B      43
#> 44   8   4         1         4     4        11                   B      44
#> 45   9   4         1         4     4        12                   C      45
#> 46  10   4         1         4     4        12                   C      46
#> 47  11   4         1         4     4        12                   C      47
#> 48  12   4         1         4     4        12                   C      48
#>    subplot_treatment
#> 1                  a
#> 2                  b
#> 3                  c
#> 4                  d
#> 5                  a
#> 6                  b
#> 7                  c
#> 8                  d
#> 9                  a
#> 10                 b
#> 11                 c
#> 12                 d
#> 13                 a
#> 14                 b
#> 15                 c
#> 16                 d
#> 17                 a
#> 18                 b
#> 19                 c
#> 20                 d
#> 21                 a
#> 22                 b
#> 23                 c
#> 24                 d
#> 25                 a
#> 26                 b
#> 27                 c
#> 28                 d
#> 29                 a
#> 30                 b
#> 31                 c
#> 32                 d
#> 33                 a
#> 34                 b
#> 35                 c
#> 36                 d
#> 37                 a
#> 38                 b
#> 39                 c
#> 40                 d
#> 41                 a
#> 42                 b
#> 43                 c
#> 44                 d
#> 45                 a
#> 46                 b
#> 47                 c
#> 48                 d
```
