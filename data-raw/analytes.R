library(readODS)
library(readr)

analytes <- readODS::read_ods(path = "data-raw/analytes.ods")
readr::write_rds(x = analytes, file = "inst/alz-datasets/analytes.rds", compress = "xz")
