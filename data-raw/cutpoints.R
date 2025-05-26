library(readODS)
library(readr)

cutpoints <- readODS::read_ods(path = "data-raw/cutpoints.ods")
readr::write_rds(x = cutpoints, file = "inst/alz-datasets/cutpoints.rds", compress = "xz")
