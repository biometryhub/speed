# Testing how well an ACO algorithm would work for a spatially
# optimal design process.
#
# Code author: Russell A. Edson, AAGI-AU
# Date last modified: 17/04/2025

# Come up with an example design to test the idea
# (For ease of eyeball checking, just make it a sudoku for this play)
library(biometryassist)

treatments <- paste0("T", 1:9)
reps <- 9
nrows <- 9
ncols <- 9
brows <- 3
bcols <- 3
des_obj <- biometryassist::design(
  type = "rcbd",
  treatments = treatments,
  reps = reps,
  nrows = nrows,
  ncols = ncols,
  brows = brows,
  bcols = bcols
)

# WLOG these could be the factor numbers for treatments or something
des <- matrix(NA, nrows, ncols)
for (row in 1:nrow(des_obj$design)) {
  des[des_obj$design$row[row], des_obj$design$col[row]] <- des_obj$design$treatments[row]
}
original_des <- des

# We use this as a check:
check_design <- function(mat) {
  grid <- expand.grid(row = 1:nrows, col = 1:ncols)
  x <- grid
  for (i in 1:nrow(x)) { x$trt[i] <- mat[x$row[i], x$col[i]]}
  print(
    ggplot(x %>% mutate(across(c("row", "col", "trt"), as.factor))) + 
      geom_tile(aes(x = col, y = row, fill = trt)) + 
      geom_text(aes(x = col, y = row, label = trt)) +
      scale_y_discrete(limits = rev) +
      theme_bw()
  )

  rowwise <- t(table(mat, grid$row))
  colnames(rowwise) <- paste0("T", colnames(rowwise))
  names(dimnames(rowwise))[1] <- "row"
  names(dimnames(rowwise))[2] <- "trt"
  colwise <- t(table(mat, grid$col))
  colnames(colwise) <- paste0("T", colnames(colwise))
  names(dimnames(colwise))[1] <- "col"
  names(dimnames(colwise))[2] <- "trt"
  print(rowwise)
  print(colwise)
}

## Fitness function ############################################################
# Define some shift functions (that also work for negative indices)
vec_shift <- function(vec, shift = 1) {
  shift <- shift %% length(vec)
  if (shift == 0) {
    vec
  } else {
    c(vec[-(0:shift)], vec[0:shift])
  }
}
row_shift <- function(mat, shift = 1) {
  mat[vec_shift(1:nrow(mat), shift), ]
}
col_shift <- function(mat, shift = 1) {
  mat[ , vec_shift(1:ncol(mat), shift)]
}

# A basic adjacency score for nearest-neighbours
neighbour_score <- function(mat) {
  # Just checking nearest neighbours for now
  colwise <- colSums((mat == row_shift(mat, 1)))# + (mat == row_shift(mat, -1)))
  rowwise <- rowSums((mat == col_shift(mat, 1)))# + (mat == col_shift(mat, -1)))
  sum(colwise) + sum(rowwise)
}

# A basic 'spread' score to penalise treatments occurring in the same row/col
# (Sam/Jules reckon the variance of these is probably more useful)
spread_score <- function(mat) {
  grid <- expand.grid(row = 1:nrow(mat), col = 1:ncol(mat))
  row_counts <- table(mat, grid$row)
  col_counts <- table(mat, grid$col)
  #sum(2^row_counts + 2^col_counts)
  sum(apply(row_counts, 2, var)) + sum(apply(col_counts, 2, var))
}

# So our simple fitness function looks like
fitness <- function(mat) {
  neighbour_score(mat) + 8*spread_score(mat)
}


## Pheromone matrices ##########################################################

# Init: set up the pheromone matrices for each treatment
treatments_fac <- unique(as.numeric(des_obj$design$treatments))
pheromone_matrices <- lapply(
  treatments_fac,
  function(trt) {
    matrix(1 / length(treatments), nrow = nrows, ncol = ncols)
  }
)
names(pheromone_matrices) <- treatments

evaporation_rate <- 0.99
update_pheromones <- function(best_design) {
  for (k in 1:length(pheromone_matrices)) {
    # Add pheromone based on where the best design placed treatments
    pheromone_matrices[[k]] <<- pheromone_matrices[[k]] * evaporation_rate
    pheromone_matrices[[k]] <<- pheromone_matrices[[k]] + 
      (best_design == k) * 0.7  #TODO: Need to have a play with this number 
  }
  
  # Normalise across treatment probabilities
  #TODO: Check, I'm not even sure this is necessary with how we've set it up
  #total <- Reduce(`+`, pheromone_matrices)
  #pheromone_matrices <<- lapply(
  #  pheromone_matrices, 
  #  function(mat) { mat / total }
  #)
}

# For a quick graphic visualisation
check_pheromones <- function(k) {
  grid <- expand.grid(row = 1:nrows, col = 1:ncols)
  x <- grid
  for (i in 1:nrow(x)) { 
    x$pheromone[i] <- pheromone_matrices[[k]][x$row[i], x$col[i]]
  }
  print(
    ggplot(x %>% mutate(across(c("row", "col"), as.factor))) + 
      geom_tile(aes(x = col, y = row, fill = pheromone)) +
      scale_y_discrete(limits = rev) +
      theme_bw() +
      labs(title = paste0("Pheromone for Treatment ", k))
  )
}


## Ant Colony Optimisation #####################################################

# Deduce the block structure (this could be provided by user, e.g.)
block_imax <- ncol(des) / bcols
block_jmax <- nrow(des) / brows
block_rows <- list()
block_cols <- list()
for (i in 1:block_imax) {
  for (j in 1:block_jmax) {
    block_cols[[length(block_cols) + 1]] <- (i - 1)*bcols + 1:bcols
    block_rows[[length(block_rows) + 1]] <- (j - 1)*brows + 1:brows
  }
}
num_blocks <- length(block_cols)

ants <- 50
iterations <- 1000
stagnate_count <- 80  # If we haven't improved for some time, stop early
best <- des  # Init: the given design is the 'best' candidate
best_fitness <- fitness(des)

# Each ant wanders around the grid (taking into account blocking structure,
# placing treatments according to the pheromone matrices)
ant_designs <- list()  # For fun, keep track of things the ants make
stagnate <- 0
for (iter in 1:iterations) {
  iter_designs <- list()
  
  # Update the pheromone matrices based on best solution found so far
  update_pheromones(best)
  
  # These can work in parallel, for instance
  for (ant in 1:ants) {
    ant_des <- matrix(NA, nrow = nrows, ncol = ncols)
    for (block in 1:num_blocks) {
      # Reset tabu list
      tabu <- treatments_fac
      
      for (i in block_cols[[block]]) {
        for (j in block_rows[[block]]) {
          # Get treatment probabilities for (i,j) from pheromones
          phero <- sapply(tabu, function(k) pheromone_matrices[[k]][i, j])
          phero <- phero / sum(phero)
          if (length(tabu) > 1) {
            chosen <- sample(tabu, 1, prob = phero)  # length 1 prob doesn't work??
          } else {
            chosen <- tabu
          }
          ant_des[i, j] <- chosen
          tabu <- setdiff(tabu, chosen)
        }
      }
    }
    
    # Ant has a design: check fitness (for debugging, keep track of ant designs)
    ant_designs[[length(ant_designs) + 1]] <- ant_des
    iter_designs[[length(iter_designs) + 1]] <- ant_des
  }
  
  # Check each ant's design and pick the best one
  iter_fitnesses <- sapply(iter_designs, fitness)
  min_index <- which.min(iter_fitnesses)
  
  if (iter_fitnesses[min_index] < best_fitness) {
    stagnate <- 0
    best_fitness <- iter_fitnesses[min_index]
    best <- iter_designs[[min_index]]
    print(paste0("Iter ", iter, ": Found new best fitness = ", best_fitness))
  } else {
    # Stagnating: keep best as previous design, update stag count
    #print("Stagnated iteration")
    stagnate <- stagnate + 1
    if (stagnate > stagnate_count) {
      break
    }
  }
}

check_design(best)
check_pheromones(1)
check_pheromones(2)
check_pheromones(4)



# 
# 
# # Simulated annealing bizo below (may or may not be useful)
# 
# # For our block design example, a legal permutation could be something
# # like: pick a block, pick two indices, and swap their treatments
# permute_design <- function(mat, brows, bcols) {
#   # Deduce the block structure
#   block_imax <- ncol(mat) / bcols
#   block_jmax <- nrow(mat) / brows
#   
#   # Randomly pick a block
#   block_i <- sample(1:block_imax, 1)
#   block_j <- sample(1:block_jmax, 1)
#   block_cols <- (block_i - 1)*bcols + 1:bcols
#   block_rows <- (block_j - 1)*brows + 1:brows
#   
#   # Randomly swap two elements
#   swapi <- sample(block_rows, replace = TRUE, 2)
#   swapj <- sample(block_cols, replace = TRUE, 2)
#   temp <- mat[swapj[1], swapi[1]]
#   mat[swapj[1], swapi[1]] <- mat[swapj[2], swapi[2]]
#   mat[swapj[2], swapi[2]] <- temp
#   mat
# }
# 
# 
# # Test loop to see that this basic idea works...
# mat <- des
# maxiter <- 50000
# accept_prob <- 0.0001  # 0.01% chance to accept a worse design to avoid local extrema
# fitness_mat <- fitness(mat)
# for (i in 1:maxiter) {
#   candidate <- permute_design(mat, 3, 3)
#   fitness_candidate <- fitness(candidate)
#   if (fitness_candidate < fitness_mat) {
#     mat <- candidate
#     fitness_mat <- fitness_candidate
#     print(paste0("Accepted better design with fitness=", round(fitness_mat, digits = 4)))
#   } else {
#     if (runif(1) < accept_prob) {
#       mat <- candidate
#       fitness_mat <- fitness_candidate
#       print(paste0("Accepted worse design with fitness=", round(fitness_mat, digits = 4)))
#     }
#   }
# }
# 
# # Check at end
# check_design(mat)


