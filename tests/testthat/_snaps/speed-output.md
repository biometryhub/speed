# speed prints progress output when quiet=FALSE for simple designs

    Code
      result <- speed(data = test_data, swap = "treatment", swap_within = "1",
        spatial_factors = ~ row + col, iterations = 2000, optimise_params = optim_params(
          stop_at_optimal = FALSE), seed = 42, quiet = FALSE)
    Message
      row and col are used as row and column, respectively.
    Output
      Optimising level: single treatment within whole design 
      Level: single treatment within whole design Iteration: 1000 Score: 1 Best: 1 Since Improvement: 1000 
      Level: single treatment within whole design Iteration: 2000 Score: 1 Best: 1 Since Improvement: 2000 
      Early stopping at iteration 2000 for level single treatment within whole design 

# speed prints progress output when quiet=FALSE for hierarchical designs

    Code
      result <- speed(df_split, swap = list(wp = "wholeplot_treatment", sp = "subplot_treatment"),
      swap_within = list(wp = "block", sp = "wholeplot_treatment"), spatial_factors = ~
       row + col, iterations = list(wp = 1500, sp = 1500), optimise_params = optim_params(
        stop_at_optimal = FALSE), seed = 42, quiet = FALSE)
    Message
      row and col are used as row and column, respectively.
    Output
      Optimising level: wp 
      Level: wp Iteration: 1000 Score: 22 Best: 22 Since Improvement: 480 
      Optimising level: sp 
      Level: sp Iteration: 1000 Score: 1.333333 Best: 1.333333 Since Improvement: 14 

