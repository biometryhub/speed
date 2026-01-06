# Create and Verify Optimization Parameters

Create and verify optimization parameters. These parameters are used to
control the behaviour of simulated annealing algorithm.

## Usage

``` r
optim_params(
  swap_count = 1,
  swap_all_blocks = FALSE,
  adaptive_swaps = FALSE,
  start_temp = 100,
  cooling_rate = 0.99,
  random_initialisation = 0,
  adj_weight = 1,
  bal_weight = 1
)
```

## Arguments

- swap_count:

  Number of treatment swaps per iteration (default: 1).

- swap_all_blocks:

  Logical; if `TRUE`, performs swaps in all blocks at each iteration
  (default: `FALSE`).

- adaptive_swaps:

  Logical; if `TRUE`, adjusts swap parameters based on temperature
  (default: `FALSE`).

- start_temp:

  Starting temperature for simulated annealing (default: 100). A higher
  start temperature allows the algorithm to accept worse solutions early
  on, encouraging exploration of the solution space and helping to avoid
  local optima. Lower values make the algorithm greedier from the start,
  which can speed up convergence but increases the risk of getting stuck
  in a poor solution. A good starting temperature allows moderately
  worse solutions to be accepted with a probability of 70–90% at the
  beginning of the optimisation.

- cooling_rate:

  Rate at which temperature decreases for simulated annealing (default:
  0.99). This controls how quickly the algorithm shifts from exploration
  to exploitation. The temperature is updated at each iteration by
  multiplying it by this rate: `T_i = start_temp * cooling_rate^i`. A
  higher cooling rate (e.g. 0.995–0.999) results in slower cooling and a
  longer exploration phase, which is generally better for complex or
  noisy optimisation landscapes. Lower values (e.g. 0.95–0.98) cool
  quickly, leading to faster convergence but greater risk of premature
  convergence to a suboptimal design.

- random_initialisation:

  Number of times to randomly shuffle items within `swap_within`; the
  design with the best score is used as an initial design (default: 0).

- adj_weight:

  Weight for adjacency score (default: 1).

- bal_weight:

  Weight for balance score (default: 1).

## Value

A named list of optimization parameters.

## See also

[`speed()`](https://biometryhub.github.io/speed/reference/speed.md) for
examples
