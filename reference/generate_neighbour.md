# Generate a Neighbour Design by Swapping Treatments

Generate a Neighbour Design by Swapping Treatments

## Usage

``` r
generate_neighbour(
  design,
  swap,
  swap_within,
  swap_count = getOption("speed.swap_count", 1),
  swap_all_blocks = getOption("speed.swap_all_blocks", FALSE),
  swap_all = FALSE
)
```

## Arguments

- design:

  Data frame containing the current design

- swap:

  Column name of the treatment to swap, or named list for hierarchical
  designs

- swap_within:

  Column name defining groups within which to swap treatments, or named
  list for hierarchical designs

- swap_count:

  Number of swaps to perform

- swap_all_blocks:

  Whether to perform swaps in all blocks or just one

- swap_all:

  Whether to swap all matching items or a single item at a time
  (default: FALSE)

## Value

A list with the updated design after swapping and information about
swapped items
