# Benchmarking speed and odw

library(bench)
library(dplyr)
library(odw)
library(speed)

row <- rep(1:20, each = 20)
col <- rep(1:20, 20)
block <- rep(1:10, each = 40)
treats <- rep(paste("V", 1:40, sep = ""), 10)
dat <- data.frame(row = row, col = col, treat = treats, rowBlock = block)
dat <- dat[order(dat$col, dat$row),]
dat$colBlock <- rep(1:10, each = 40)

## swap in rowBlocks and also balance across colBlocks
options(speed.swap_count = 5, speed.swap_all_blocks = TRUE, speed.adaptive_swaps = TRUE, speed.cooling_rate = 0.99)
des <- speed(dat, swap = "treat", swap_within = "rowBlock",
             spatial_factors = ~ colBlock, iterations = 150000,
             early_stop_iterations = 50000, seed = 123)

lapply(split(desd, desd$rowBlock), function(el, trs) trs %in% as.character(unique(el$treat)), trs)
lapply(split(desd, desd$colBlock), function(el, trs) trs %in% as.character(unique(el$treat)), trs)

## already optimal across rows and cols

desd <- des$design_df
table(desd$treat, desd$row)
table(desd$treat, desd$col)

calculate_efficiency_factor(des$design_df, treat)


str(dat)
dat <- dat |> mutate(across(1:5, factor))
dat$dummy <- factor(rep(1, nrow(dat)))

twod_od <- function() {
  twod.odi <- odw(random = ~treat + rowBlock + colBlock,
                  data = dat,
                  permute = ~treat,
                  swap = ~dummy,
                  search = "tabu+rw", maxit = 10, start.values = TRUE)

  pars.init <- twod.odi$vparameters.table
  pars.init[2:3,2] <- 100

  twod.od <- odw(random = ~treat + rowBlock + colBlock,
                 data = dat,
                 permute = ~treat,
                 swap = ~dummy,
                 search = "tabu+rw",
                 G.param = pars.init, R.param = pars.init,
                 maxit = 10)
}

twod.od <- twod_od()
lapply(split(twod.od$design, twod.od$design$rowBlock), function(el, trs) trs %in% as.character(unique(el$treat)), trs)
lapply(split(twod.od$design, twod.od$design$colBlock), function(el, trs) trs %in% as.character(unique(el$treat)), trs)

## already optimal across rows and cols

desd <- twod.od$design
table(desd$treat, desd$row)
table(desd$treat, desd$col)

calculate_efficiency_factor(twod.od$design, treat)

bench::mark(check = FALSE, iterations = 10,
            speed = speed(dat, swap = "treat", swap_within = "rowBlock",
                          spatial_factors = ~ colBlock, iterations = 150000,
                          early_stop_iterations = 50000, seed = 123, quiet = TRUE),
            odw = twod_od()
)





### Easy case

df <- initialise_design_df(10, 10, 6, 10, 1)
# Optimise the design
result <- speed(df, swap = "treatment", seed = 42)
autoplot(result)


df_od <- df |> mutate(across(1:6, factor))
rcb_od <- function() {
  rcb.odi <- odw(random = ~treatment + block + row,
                 data = df_od,
                 permute = ~treatment,
                 swap = ~block,
                 search = "tabu+rw", maxit = 10, start.values = TRUE, trace = FALSE)

  pars.init <- rcb.odi$vparameters.table
  pars.init[3,2] <- 100

  rcb.od <- odw(random = ~treatment + block + row,
                data = df_od,
                permute = ~treatment,
                swap = ~block,
                search = "tabu+rw",
                G.param = pars.init, R.param = pars.init,
                maxit = 10, trace = FALSE)
}

des <- rcb.od$design
des$row <- as.numeric(des$row)
des$col <- as.numeric(des$col)
class(des) <- c(class(des), "design")
autoplot(des)


res <- bench::mark(check = FALSE, iterations = 10,
            speed = speed(df, swap = "treatment", seed = 42, quiet = T),
            odw = rcb_od())
autoplot(res, type = "boxplot")
