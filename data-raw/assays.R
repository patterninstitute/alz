library(readODS)
library(readr)

assays <- readODS::read_ods(path = "data-raw/assays.ods")
readr::write_rds(x = assays, file = "inst/alz-datasets/assays.rds", compress = "xz")
