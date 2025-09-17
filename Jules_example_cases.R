######### tough 2D blocking

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

## check design

desd <- des$design_df
trs <- paste("V", 1:40, sep = "")

## check unique entries in rowBlock and colBlock.

lapply(split(desd, desd$rowBlock), function(el, trs) trs %in% as.character(unique(el$treat)), trs)
lapply(split(desd, desd$colBlock), function(el, trs) trs %in% as.character(unique(el$treat)), trs)

## already optimal across rows and cols

desd <- des$design_df
table(desd$treat, desd$row)
table(desd$treat, desd$col)

autoplot(des, treatments = "treat", block = "rowBlock")
autoplot(des, treatments = "treat", block = "colBlock")

## nailed it

############ RCBD with multiple treatment reps appearing in rows and columns

row <- rep(1:25, each = 20)
col <- rep(1:20, 25)
block <- rep(1:50, each = 10)
treats <- rep(paste("V", 1:10, sep = ""), 50)
datb <- data.frame(row, col, treat, block)

## swap in Blocks and balance across rows and cols
op <- options()
options(speed.swap_count = 3, speed.adaptive_swaps = TRUE)
des <- speed(datb, swap = treat, swap_within = "block", iterations = 50000, early_stop_iterations = 2000)

desd <- des$design_df
table(desd$treat, desd$row)
autoplot(des, treatments = "treat")


## nailed it

############ partial rep, balance across rows and columns

## 150 Treatments, 50 replicated, 200 plots

row <- rep(1:20, each = 10)
col <- rep(1:10, 20)
block <- rep(1:2, each = 100)

treats <- sample(paste("V", 1:150, sep = ""), 150, replace = FALSE)
trep <- sample(treats, 50, replace = FALSE)
tunrep <- treats[!(treats %in% trep)]
treat <- unlist(lapply(split(tunrep, rep(1:2, each = 50)), function(el, trep) c(el, trep), trep))
datp <- data.frame(row, col, treat, block)

## swap in Blocks and balance across rows and cols

des <- speed(datp, swap = treat, swap_within = "block", spatial_factors = ~ row + col, iterations = 30000, early_stop_iterations = 5000)

desd <- des$design_df
table(desd$treat, desd$row)
table(desd$treat, desd$col)

autoplot(des, treatments = "treat")

## nailed it

########### large RCBD with 500 treatments/varieties

row <- rep(1:50, times = 40)
col <- rep(1:40, each = 50)
block <- rep(1:4, each = 500)
treat <- rep(paste("V", 1:500, sep = ""), 4)
dath <- data.frame(row, col, treat, block)

## swap in Blocks and balance across rows and cols
options(speed.swap_count = 10)
set.seed(42)
des <- speed(dath, swap = treat, swap_within = "block", spatial_factors = ~ row + col,
             iterations = 20000, early_stop_iterations = 4000)

desd <- des$design_df
unique(c(table(desd$treat, desd$row)))
unique(c(table(desd$treat, desd$col)))

autoplot(des, treatments = "treat") + theme(legend.position="none")

options(op)

## nailed it
