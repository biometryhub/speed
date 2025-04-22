# shiny
options(shiny.port = 4200, shiny.host = "0.0.0.0", shiny.launch.browser = FALSE)

nrows <- 5
ncols <- 8
treatments <- paste0("T", 1:5) # 5 treatments
blocks <- create_block_structure(nrows, ncols, 5, 1) # 8 blocks of 5x1

generate_design <- function() {
  set.seed(123) # For reproducibility
  return(optimize_design(nrows, ncols, treatments, blocks,
    iterations = 10000,
    start_temp = 100,
    cooling_rate = 0.99,
    adj_weight = 1,
    bal_weight = 1,
    early_stop_iterations = 2000, quiet = TRUE
  ))
}

prof_generate_design <- lineprof::lineprof(generate_design())

initial_design <- create_initial_design(nrows, ncols, treatments, blocks)
