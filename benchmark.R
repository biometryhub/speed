library(DiGGer)

source("./R/design_utils.R")
source("./R/utils.R")
source("./R/speed.R")
source("./R/metrics.R")
source("./R/verify_utils.R")


# digger
digger_design <- ibDiGGer(
  numberOfTreatments = 10,
  rowsInDesign = 20,
  columnsInDesign = 20,
  rowsInBlock = 1,
  columnsInBlock = 10,
  maxInterchanges = 1000000,
  rngSeeds = c(112, 112)
)
