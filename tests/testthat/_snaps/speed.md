# speed handles large layouts with blocking

    Code
      result$design_df
    Output
          row col block treatment
      1     1   1     1         L
      2     1   2     1         N
      3     1   3     1         P
      4     1   4     1         B
      5     1   5     1         C
      6     1   6     1         G
      7     1   7     1         E
      8     1   8     1         M
      9     1   9     1         O
      10    1  10     1         I
      11    1  11     1         H
      12    1  12     1         K
      13    1  13     1         F
      14    1  14     1         D
      15    1  15     1         A
      16    1  16     1         J
      17    2   1     1         O
      18    2   2     1         C
      19    2   3     1         F
      20    2   4     1         L
      21    2   5     1         B
      22    2   6     1         P
      23    2   7     1         D
      24    2   8     1         J
      25    2   9     1         K
      26    2  10     1         A
      27    2  11     1         M
      28    2  12     1         N
      29    2  13     1         H
      30    2  14     1         G
      31    2  15     1         E
      32    2  16     1         O
      33    3   1     1         E
      34    3   2     1         B
      35    3   3     1         J
      36    3   4     1         G
      37    3   5     1         D
      38    3   6     1         H
      39    3   7     1         K
      40    3   8     1         A
      41    3   9     1         I
      42    3  10     1         O
      43    3  11     1         L
      44    3  12     1         F
      45    3  13     1         P
      46    3  14     1         N
      47    3  15     1         C
      48    3  16     1         I
      49    4   1     1         M
      50    4   2     1         K
      51    4   3     1         M
      52    4   4     1         H
      53    4   5     1         A
      54    4   6     1         J
      55    4   7     1         I
      56    4   8     1         L
      57    4   9     1         P
      58    4  10     1         D
      59    4  11     1         B
      60    4  12     1         C
      61    4  13     1         N
      62    4  14     1         E
      63    4  15     1         F
      64    4  16     1         G
      65    5   1     2         A
      66    5   2     2         B
      67    5   3     2         H
      68    5   4     2         F
      69    5   5     2         N
      70    5   6     2         M
      71    5   7     2         C
      72    5   8     2         K
      73    5   9     2         J
      74    5  10     2         E
      75    5  11     2         I
      76    5  12     2         P
      77    5  13     2         G
      78    5  14     2         O
      79    5  15     2         L
      80    5  16     2         D
      81    6   1     2         O
      82    6   2     2         I
      83    6   3     2         G
      84    6   4     2         K
      85    6   5     2         D
      86    6   6     2         E
      87    6   7     2         F
      88    6   8     2         D
      89    6   9     2         A
      90    6  10     2         M
      91    6  11     2         P
      92    6  12     2         B
      93    6  13     2         H
      94    6  14     2         C
      95    6  15     2         J
      96    6  16     2         N
      97    7   1     2         D
      98    7   2     2         J
      99    7   3     2         I
      100   7   4     2         P
      101   7   5     2         G
      102   7   6     2         B
      103   7   7     2         A
      104   7   8     2         N
      105   7   9     2         E
      106   7  10     2         H
      107   7  11     2         F
      108   7  12     2         O
      109   7  13     2         L
      110   7  14     2         K
      111   7  15     2         M
      112   7  16     2         C
      113   8   1     2         H
      114   8   2     2         L
      115   8   3     2         O
      116   8   4     2         C
      117   8   5     2         E
      118   8   6     2         A
      119   8   7     2         F
      120   8   8     2         B
      121   8   9     2         M
      122   8  10     2         L
      123   8  11     2         N
      124   8  12     2         J
      125   8  13     2         K
      126   8  14     2         I
      127   8  15     2         G
      128   8  16     2         P
      129   9   1     3         I
      130   9   2     3         D
      131   9   3     3         N
      132   9   4     3         E
      133   9   5     3         I
      134   9   6     3         K
      135   9   7     3         J
      136   9   8     3         O
      137   9   9     3         F
      138   9  10     3         G
      139   9  11     3         C
      140   9  12     3         M
      141   9  13     3         E
      142   9  14     3         P
      143   9  15     3         L
      144   9  16     3         H
      145  10   1     3         J
      146  10   2     3         F
      147  10   3     3         H
      148  10   4     3         O
      149  10   5     3         P
      150  10   6     3         F
      151  10   7     3         C
      152  10   8     3         E
      153  10   9     3         N
      154  10  10     3         L
      155  10  11     3         K
      156  10  12     3         G
      157  10  13     3         B
      158  10  14     3         M
      159  10  15     3         D
      160  10  16     3         I
      161  11   1     3         K
      162  11   2     3         A
      163  11   3     3         C
      164  11   4     3         N
      165  11   5     3         J
      166  11   6     3         M
      167  11   7     3         O
      168  11   8     3         H
      169  11   9     3         B
      170  11  10     3         F
      171  11  11     3         G
      172  11  12     3         I
      173  11  13     3         D
      174  11  14     3         L
      175  11  15     3         P
      176  11  16     3         A
      177  12   1     3         B
      178  12   2     3         E
      179  12   3     3         B
      180  12   4     3         D
      181  12   5     3         M
      182  12   6     3         K
      183  12   7     3         P
      184  12   8     3         G
      185  12   9     3         L
      186  12  10     3         C
      187  12  11     3         A
      188  12  12     3         H
      189  12  13     3         J
      190  12  14     3         A
      191  12  15     3         O
      192  12  16     3         N
      193  13   1     4         C
      194  13   2     4         G
      195  13   3     4         E
      196  13   4     4         I
      197  13   5     4         P
      198  13   6     4         N
      199  13   7     4         M
      200  13   8     4         F
      201  13   9     4         H
      202  13  10     4         J
      203  13  11     4         O
      204  13  12     4         D
      205  13  13     4         A
      206  13  14     4         B
      207  13  15     4         K
      208  13  16     4         L
      209  14   1     4         P
      210  14   2     4         M
      211  14   3     4         B
      212  14   4     4         G
      213  14   5     4         L
      214  14   6     4         I
      215  14   7     4         N
      216  14   8     4         C
      217  14   9     4         D
      218  14  10     4         K
      219  14  11     4         J
      220  14  12     4         A
      221  14  13     4         O
      222  14  14     4         H
      223  14  15     4         I
      224  14  16     4         F
      225  15   1     4         K
      226  15   2     4         P
      227  15   3     4         L
      228  15   4     4         M
      229  15   5     4         H
      230  15   6     4         O
      231  15   7     4         A
      232  15   8     4         I
      233  15   9     4         C
      234  15  10     4         B
      235  15  11     4         D
      236  15  12     4         E
      237  15  13     4         G
      238  15  14     4         F
      239  15  15     4         N
      240  15  16     4         B
      241  16   1     4         F
      242  16   2     4         G
      243  16   3     4         D
      244  16   4     4         A
      245  16   5     4         O
      246  16   6     4         L
      247  16   7     4         M
      248  16   8     4         P
      249  16   9     4         K
      250  16  10     4         N
      251  16  11     4         E
      252  16  12     4         J
      253  16  13     4         C
      254  16  14     4         J
      255  16  15     4         H
      256  16  16     4         E

# speed prints progress output when quiet=FALSE for simple designs

    Code
      result <- speed(data = test_data, swap = "treatment", swap_within = "1",
        spatial_factors = ~ row + col, iterations = 2000, seed = 42, quiet = FALSE)
    Message
      row and col are used as row and column, respectively.
    Output
      Iteration: 1000 Score: 1 Best: 1 Since Improvement: 1000 
      Iteration: 2000 Score: 1 Best: 1 Since Improvement: 2000 
      Early stopping at iteration 2000 

# speed prints progress output when quiet=FALSE for hierarchical designs

    Code
      result <- speed(df_split, swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
      swap_within = list(wp = "block", sp = "wholeplot_treatment"), spatial_factors = ~
       row + col, iterations = list(wp = 1500, sp = 1500), seed = 42, quiet = FALSE)
    Message
      row and col are used as row and column, respectively.
    Output
      Optimising level: wp 
      Level: wp Iteration: 1000 Score: 32 Best: 32 Since Improvement: 1000 
      Optimising level: sp 
      Level: sp Iteration: 1000 Score: 4 Best: 4 Since Improvement: 981 

